*          DATA SET DMWRKRM    AT LEVEL 028 AS OF 10/07/15                      
*PHASE WKMAINTA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'WKMAINT - WKFILE FILE MAINTENANCE'                              
         PRINT NOGEN                                                            
WKMAINT  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,WKMAINT,RA,R9,R8,WORK=A(WKWORK)                                
*&&DO                                                                           
WKMA1    L     R1,20               SAVE DOS SUPV INFO                           
         SVC   33                                                               
         ST    R1,ACOMRG                                                        
         MVC   UPSIVAL,23(R1)      SAVE UPSI                                    
         MVC   UPSIINP,UPSIVAL                                                  
         L     R1,36(R1)                                                        
         LA    R1,8(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
WKMA1X   ST    R1,ALOADPT          SAVE EXTERNAL LOAD POINT ADDR                
*&&                                                                             
*&&OS                                                                           
WKMA1    ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    WKMA1C                                                           
         CHI   R2,8                                                             
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
*&&                                                                             
         GOTO1 =V(DATCON),DMCB,(5,DUB),(10,DATEVAL)                             
         GOTO1 (RF),DMCB,(4,DATEVAL),(0,DATEYMD)                                
         EJECT                                                                  
         L     RC,=V(CPRINT)       RC IS PRINTER CONTROL REGISTER               
         USING DPRINT,RC                                                        
         MVC   TITLE+19(23),=C'WORKER FILE MAINTENANCE'                         
*                                                                               
         L     R1,=A(CDREC)                                                     
         ST    R1,ACDREC                                                        
         L     R1,=A(CIREC)                                                     
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
*                                                                               
         XC    CIDATA,CIDATA                                                    
         LA    RE,L'WKINDEX                                                     
         STH   RE,CINDXLN                                                       
         EJECT                                                                  
***********************************************************************         
* READ A SET OF INPUT PARAMETER CARDS                                           
***********************************************************************         
GETPARM  CLI   FRSTTIME,C'X'       WAS LAST SET TERMINATED WITH /* CARD         
         BE    EOJ                 YES EOJ                                      
         BH    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         MVC   P(15),=C'PARAMETER CARDS'                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=16C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(VALPARM)                                                   
         BASR  RE,RF               GOTO PARM VALIDATION CSECT                   
         CLI   ERRNUM,0                                                         
         BNE   GPERR               ERROR FOUND IN CARD                          
*                                                                               
GP1      CLC   FILEID,SPACES       SET WORKER FILE INFO                         
         BNE   GP1A                                                             
         MVC   FILEID,FIDWKFI      DEFAULT IS C'WKFILE'                         
*                                                                               
GP1A     CLC   FILEID(4),=C'WRKF'  THESE FILES USE WRKFZ/M                      
         BE    GPBAD                                                            
*                                                                               
         LA    RF,VWKFILE                                                       
         CLC   FILEID(3),=C'FACWRK'                                             
         BNE   *+8                                                              
         LA    RF,VFACWRK                                                       
*&&US*&& CLC   FILEID(3),=C'EASIWK'                                             
*&&US*&& BNE   *+8                                                              
*&&US*&& LA    RF,VEASIWK                                                       
         MVC   DTFWORK(24),0(RF)   SET A(DTF)/FILEID/DMGRID/ENQID               
*                                                                               
         LA    R4,PARMTBL          CHECK FOR REQUIRED & OPTIONAL PARMS          
GP2      TM    2(R4),X'80'                                                      
         BZ    GP4                                                              
         CLI   0(R4),0             WAS REQUIRED PARM INPUT                      
         BNE   GP6                 YES                                          
         LA    R1,4(R4)            NO- ERROR                                    
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GP4      CLI   0(R4),0             WAS OPTIONAL PARM INPUT                      
         BNE   GP6                 YES                                          
         MVC   0(1,R4),1(R4)       NO- SET DEFAULT VALUE                        
GP6      LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   GP2                                                              
*                                                                               
GP8      CLI   OUTPUT,0            OUTPUT ONLY FOR MODE=COPY                    
         BE    GPB                                                              
         CLI   MODE,4                                                           
         BE    GPB                                                              
GPA      LA    R1,OUTPUT+4         INVALID OUTPUT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPB      TM    INPUT,DISK          CANT HAVE INPUT=OUTPUT=DISK                  
         BZ    *+12                                                             
         TM    OUTPUT,DISK                                                      
         BO    GPA                                                              
*                                                                               
GPM1     CLI   MODE,1              INIT - NO PARAMS REQUIRED                    
         BE    GETPARMX                                                         
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
GPM3     CLI   MODE,3              REPT - INPUT MUST BE DISK                    
         BNE   GPM4                                                             
         TM    INPUT,TAPE                                                       
         BZ    GPM3B                                                            
GPM3A    LA    R1,INPUT+4          INVALID INPUT=                               
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM3B    CLI   USER,0              DEFAULT USER TO ALL                          
         BNE   GPM3C                                                            
         MVI   USER,X'80'                                                       
         LA    RF,FILTAB                                                        
         ST    RF,AFILALL                                                       
         LA    RF,L'FILTAB(RF)                                                  
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
GPERR    MVC   WORK(60),SPACES                                                  
         L     R1,ERRNUM                                                        
*                                                                               
         CLI   ERRNUM,1                                                         
         BNE   *+20                                                             
         MVC   WORK(20),=C'MISSING PARAMETER - '                                
         MVC   WORK+20(8),0(R1)                                                 
         B     GPERR2                                                           
*                                                                               
         CLI   ERRNUM,2                                                         
         BNE   *+14                                                             
         MVC   WORK(29),=C'INVALID PARAMETER CARD SYNTAX'                       
         B     GPERR2                                                           
*                                                                               
         CLI   ERRNUM,3                                                         
         BNE   *+20                                                             
         MVC   WORK(20),=C'INVALID PARAMETER - '                                
         MVC   WORK+20(8),0(R1)                                                 
         B     GPERR2                                                           
*                                                                               
         CLI   ERRNUM,4                                                         
         BNE   *+20                                                             
         MVC   WORK(30),=C'INVALID VALUE FOR PARAMETER - '                      
         MVC   WORK+30(8),0(R1)                                                 
         B     GPERR2                                                           
*                                                                               
         DC    H'0'                                                             
*                                                                               
GPERR2   GOTO1 =V(LOGIO),DMCB,1,(80,P)                                          
         GOTO1 (RF),(R1),,(38,WORK)                                             
*                                                                               
GPERR4   MVC   P+82(38),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
         CLI   FRSTTIME,C'X'                                                    
         BE    EOJ                                                              
GPERR6   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    EOJ                                                              
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         B     GPERR6                                                           
*                                                                               
GPBAD    WTO   TEXT=BADMSGH                                                     
         ABEND 911                                                              
*                                                                               
EOJ      XBASE RC=RTERR,RL=1                                                    
*                                                                               
BADMSGH  DC    AL2(50)                                                          
         DC    CL50'***ERROR** YOU MUST USE WRKFM/Z FOR WRKF FILES'             
*                                                                               
GETPARMX EQU   *                                                                
         EJECT                                                                  
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
         MVI   4(R1),0                                                          
         TM    INPUT,TAPE                                                       
         BZ    *+8                                                              
         OI    4(R1),X'80'         SET TAPE INPUT FLAG                          
         TM    OUTPUT,TAPE                                                      
         BZ    *+8                                                              
         OI    4(R1),X'40'         SET TAPE OUTPUT FLAG                         
         BASR  RE,RF                                                            
*                                                                               
GPXX     DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE AND OPEN FILE                                                      
***********************************************************************         
INIT     L     R2,DTFWORK                                                       
*                                                                               
         CLI   WRITE,NO            WRITE=NO                                     
         BE    INIT10                                                           
         CLI   WRSRV,NO            WRSRV=NO                                     
         BNE   INIT20                                                           
INIT10   MVI   WRITE,NO                                                         
*                                                                               
         USING DTFPHD,R2                                                        
         OI    DTFOPEN,DTF_RO      READ-ONLY                                    
         USING SSBD,RE                                                          
         L     RE,=A(SSB)                                                       
         OI    SSOMTIND,SSOWSRN    FORCE SERVICE TO WRITE=NO                    
         DROP  RE                                                               
*                                                                               
INIT20   MVI   DUB,C'N'                                                         
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1                                                        
         MVC   P2,ACXREC                                                        
         MVC   P4,DTFWORK                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00010100'                                                  
*                                                                               
         TM    DTFOPEN,X'20'       TEST IF ALREADY OPEN                         
         BZ    INIT40              NO                                           
         CLC   DTFDD,FILEID        TEST IF SAME FILE ID                         
         BE    INIT50              YES                                          
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 =V(DADDS),P1        CLOSE FILE                                   
*                                                                               
INIT40   MVC   DTFDD,FILEID                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN',=C'SER',DUB                             
INIT50   MVC   DNEXT,=X'00010000'                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 =V(DADDS),P1                                                     
         DROP  R2                                                               
*                                                                               
         MVC   TITLE+14(23),=C'WORKER FILE MAINTENANCE'                         
         MVI   TITLE+37,C' '                                                    
         MVC   TITLE+38(7),FILEID                                               
*                                                                               
         GOTO1 =V(PRINTER)         PRINT ACTION MESSAGE HEADER                  
         MVC   P(15),=C'ACTION MESSAGES'                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=16C'-'                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   MODE,1              READ FIRST INDEX RECORD                      
         BE    INITWK                                                           
         L     R2,ACXREC                                                        
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CXADDR,(R2)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,DTFWORK                                                       
         SH    RE,=H'40'                                                        
         MVC   0(20,RE),0(R2)      EXTRACT PART 1 INDEX DATA                    
         MVC   20(20,RE),PART2IND                                               
         TM    0(RE),X'80'                                                      
         BZ    *+14                                                             
         MVC   20(20,RE),20(R2)    EXTRACT PART 2 INDEX DATA                    
         NI    0(RE),X'7F'                                                      
         MVC   CIDATA,0(RE)                                                     
         L     RE,DTFWORK          SET F/L REC LEN IN WKFILE DTF                
         LA    RE,52(RE)                                                        
         MVC   0(2,RE),CIBLKLN                                                  
         OI    0(RE),X'80'                                                      
*                                                                               
         XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
         TM    INPUT,TAPE          OPEN INPUT TAPE IF SPECIFIED                 
         BZ    INIT70                                                           
         BAS   RE,OPNIN                                                         
*                                                                               
INIT70   TM    OUTPUT,TAPE         OPEN OUTPUT TAPE IF SPECIFIED                
         BZ    INIT80                                                           
         BAS   RE,OPNOUT                                                        
*                                                                               
INIT80   CLI   MODE,2                                                           
         BE    PRNTWK                                                           
         CLI   MODE,3                                                           
         BE    REPTWK                                                           
         CLI   MODE,4                                                           
         BE    COPYWK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE WKFILE                                                             
***********************************************************************         
INITWK   CLI   ENTRYS,0                                                         
         BNE   *+10                                                             
         MVC   CICITOT,=H'0180'    DEFAULT NUM OF ENTRYS                        
         CLI   CISIZE,0                                                         
         BNE   *+10                                                             
         MVC   CITRKS,=H'1'        DEFAULT TRACKS PER CI                        
         CLI   BLKSIZE,0                                                        
         BNE   *+10                                                             
         MVC   CIBLKLN,=H'3660'    DEFAULT BLOCK SIZE                           
         CLI   OENTRYS,0                                                        
         BNE   *+10                                                             
         MVC   CJCITOT,=H'0180'    DEFAULT OVERFLOW NUM OF ENTRYS               
         CLI   OCISIZE,0                                                        
         BNE   *+10                                                             
         MVC   CJTRKS,=H'2'        DEFAULT OVERFLOW TRACKS PER CI               
*                                                                               
         BAS   RE,WKLOCK           LOCK PRINT QUEUE                             
*                                                                               
         SR    RE,RE               SET BLOCK SIZE MOD ENTRY SIZE                
         LH    RF,CIBLKLN                                                       
         LA    R0,L'WKINDEX                                                     
         DR    RE,R0                                                            
         MR    RE,R0                                                            
         STH   RF,CIBLKLN                                                       
*                                                                               
INITWK2  XC    P1(24),P1           CALC BLOCKS PER TRACK                        
         MVC   P1,=A(DARPT)                                                     
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,DTFWORK                                                       
         GOTO1 =V(DADDS),P1                                                     
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
         LA    R0,L'WKINDEX                                                     
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
         STH   RF,CICITOT                                                       
         SR    RE,RE               OPTIMISE PART 2 NUMBER OF CI'S               
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         OC    CJCITOT,CJCITOT                                                  
         BZ    INITWK5                                                          
         LA    RF,1(RF)                                                         
         AH    RF,CJCITOT                                                       
         DR    RE,R0                                                            
         CH    RE,=H'10'                                                        
         BNL   INITWK3                                                          
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT                                                     
         SR    RF,RE                                                            
         STH   RF,CJCITOT                                                       
*                                                                               
INITWK3  SR    RE,RE               CALC PAGE/ENTRY OF PART 2 INDEX              
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
INITWK5  LH    R0,CIENTRYS         CALC NUM OF INDEX PAGES                      
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
*                                                                               
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P1,=A(WTCKD)                                                     
         MVC   P2,ACXREC                                                        
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,DTFWORK                                                       
         LA    R0,P6                                                            
         ST    R0,P5                                                            
*                                                                               
         SR    R3,R3               R3=NUM OF ACTIVE INDEX ENTRYS                
         ICM   R3,3,CICITOT                                                     
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
         BZ    INITWK6                                                          
         MVC   DUB(2),CJPAGE                                                    
         LH    RF,CJENTRY                                                       
         SH    RF,=H'1'                                                         
         STH   RF,DUB+2                                                         
         BNM   INITWK6                                                          
         LH    RF,CJPAGE                                                        
         BCTR  RF,0                                                             
         STH   RF,DUB                                                           
         LH    RF,CIENTRYS                                                      
         BCTR  RF,0                                                             
         STH   RF,DUB+2                                                         
*                                                                               
INITWK6  L     R5,ACXREC           WRITE INDEX RECORDS LOOP                     
         LH    R6,CIENTRYS         R6=NUM OF 00 ENTRYS                          
         SR    R7,R7               R7=NUM OF FF ENTRYS                          
         CR    R3,R6                                                            
         BL    *+10                                                             
         SR    R3,R6               FULL INDEX PAGE                              
         B     INITWK6A                                                         
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         SR    R6,R6               EMPTY INDEX PAGE                             
         LH    R7,CIENTRYS                                                      
         B     INITWK6A                                                         
         LR    R6,R3               PARTIAL INDEX PAGE                           
         LH    R7,CIENTRYS                                                      
         SR    R7,R3                                                            
         SR    R3,R3                                                            
*                                                                               
INITWK6A LTR   R6,R6                                                            
         BZ    INITWK6B                                                         
         XC    0(L'WKINDEX,R5),0(R5)                                            
         LA    R5,L'WKINDEX(R5)                                                 
         BCT   R6,*-10                                                          
INITWK6B LTR   R7,R7                                                            
         BZ    INITWK6C                                                         
         MVC   0(L'WKINDEX,R5),FFS                                              
         LA    R5,L'WKINDEX(R5)                                                 
         BCT   R7,*-10                                                          
*                                                                               
INITWK6C OC    FULL(2),FULL        SET FIRST PAGE DATA                          
         BNZ   *+28                                                             
         L     R5,ACXREC                                                        
         MVC   0(L'CIDATA,R5),CIDATA                                            
         OC    CJCITOT,CJCITOT     SET PART 2 INDEX PRESENT                     
         BZ    INITWK6D                                                         
         OI    0(R5),X'80'                                                      
         CLC   FULL(2),DUB         SET END OF PART 1 INDEX                      
         BNE   INITWK6D                                                         
         LH    R5,DUB+2                                                         
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         MVC   0(L'WKINDEX,R5),FFS                                              
*                                                                               
INITWK6D GOTO1 =V(DADDS),P1                                                     
         OC    P3(2),P3                                                         
         BNZ   INITWKW                                                          
         LH    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STH   RE,FULL                                                          
         BCT   R4,INITWK6                                                       
*                                                                               
INITWK8  L     R5,ACIREC           POINT TO CI DATA RECORD                      
         USING WKRECD,R5                                                        
         ST    R5,P2                                                            
         LR    RE,R5                                                            
         LH    RF,CIBLKLN                                                       
         XCEF                                                                   
         SR    R0,R0               CALC NUM OF TRACKS OF DATA CI'S              
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LH    R4,CJCITOT                                                       
         MH    R4,CJTRKS                                                        
         AR    R4,R0               R4=NUM OF TRACKS IN PARTS 1 AND 2            
*                                                                               
INITWKA  LH    R0,CIHIREC          WRITE DATA CI RECORDS LOOP                   
         GOTO1 =V(DADDS),P1                                                     
         OC    P3(2),P3                                                         
         BNZ   INITWKW                                                          
         BCT   R0,INITWKA+4                                                     
         BCT   R4,INITWKA                                                       
         MVI   FLAG1,0                                                          
*                                                                               
INITWKB  MVC   P1,=A(WTERASE)      ERASE TO END OF EXTENT                       
         XC    P2(8),P2                                                         
         GOTO1 =V(DADDS),P1                                                     
         B     INITWKX                                                          
*                                                                               
INITWKW  MVC   P(21),=C'*ERROR* - END OF FILE'                                  
         TM    P3+1,X'04'                                                       
         BO    *+10                                                             
         MVC   P+10(11),=C'DISK WRITE '                                         
         GOTO1 =V(LOGIO),DMCB,1,(21,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVI   FLAG1,1                                                          
         B     INITWKY                                                          
*                                                                               
INITWKX  MVC   P(19),=C'XXXXXXX INITIALISED'                                    
         MVC   P(7),FILEID                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
INITWKY  BAS   RE,WKUNLK           UNLOCK PRINT QUEUE                           
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,WKOUT                                                         
         CLI   FLAG1,0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF FAIL TO INITIALISE                    
         B     GETPARM                                                          
         EJECT                                                                  
***********************************************************************         
* COPY WORK FILES FROM DISK/TAPE TO DISK/TAPE                                   
***********************************************************************         
COPYWK   L     R3,ACDREC           R3=A(WORK FILE DATA RECORD)                  
         XC    FULL,FULL                                                        
         XC    INDEX,INDEX         SET TO READ ALL DISK FILES                   
         TM    INPUT,DISK                                                       
         BZ    COPYWK0                                                          
         BAS   RE,WKLOCK           LOCK WKFILE DURING DISK COPY                 
*                                                                               
COPYWK0  BAS   RE,GETFILE          GET NEXT WORK FILE HEADER                    
         OC    0(2,R3),0(R3)                                                    
         BZ    COPYWKS             END OF INPUT FILE                            
         LH    RF,FULL+2                                                        
         LA    RF,1(RF)            BUMP NUM OF FILES READ                       
         STH   RF,FULL+2                                                        
         LA    R5,28(R3)           R5=A(WORK FILE ACTUAL INDEX)                 
         USING WKRECD,R5                                                        
*                                                                               
COPYWK2  MVC   FID,WKUSRID         SAVE FILE ID                                 
         MVI   DUB,0                                                            
COPYWK2A BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   COPYWK0                                                          
         BAS   RE,TSTFILE          FILTER ON FILE ID                            
         BNE   COPYWK2B                                                         
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   COPYWK2B                                                         
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   COPYWK2B                                                         
         B     COPYWK4                                                          
COPYWK2B CLC   AFILALL,AFILTAB     SET TO CONTINUE IF SPECIFIC USER             
         BE    COPYWK0                                                          
         MVI   DUB,X'FF'                                                        
         B     COPYWK2A                                                         
*                                                                               
COPYWK4  MVC   SAVE1(36),12(R3)    WORK FILE FOUND FOR COPYING                  
         TM    INPUT,TAPE                                                       
         BO    COPYWK6                                                          
         L     R0,ACIREC           COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,CIBLKLN                                                       
         LA    R1,256(R1)          PLUS SAVE AREA                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,ACIREC           GET FULL EXTENDED FILE HEADER                
         MVI   INDEX+13,X'C0'                                                   
         GOTO1 VDATAMGR,DMCB,=C'SEQ',NAMWORK,INDEX,(R3),(R0)                    
         CLC   8(3,R3),=C'ERR'     TEST FOR ERROR ON FIRST DISK READ            
         BE    COPYWKQ                                                          
*                                                                               
COPYWK6  TM    LOAD,YES            PASS FILE HEADER TO EXTERNAL                 
         BZ    COPYWK8                                                          
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),1             SET FILE SOF                                 
         BASR  RE,RF                                                            
         CLI   0(R1),X'FF'                                                      
         BE    COPYWK0             EXTERNAL DELETED THIS FILE                   
*                                                                               
COPYWK8  TM    OUTPUT,TAPE         WRITE WORK FILE HEADER TO TAPE/DISK          
         BZ    *+12                                                             
         BAS   RE,WOTAPE                                                        
         B     COPYWKA                                                          
         MVI   INDEX+13,X'E0'      USE SOF REC FOR KEY/STATUS/DATE              
         CLC   0(2,R3),=H'92'                                                   
         BL    *+8                                                              
         OI    INDEX+13,X'18'      USE SOF REC FOR RETN/COMMENT                 
         LA    RE,28(R3)                                                        
         TM    WKFTYPE-WKINDEX(RE),X'02'                                        
         BZ    *+8                                                              
         OI    INDEX+13,X'02'      SET LIBRARY FILE                             
         TM    WKFTYPE-WKINDEX(RE),X'01'                                        
         BZ    *+8                                                              
         OI    INDEX+13,X'01'      SET DUPLICATES ALLOWED                       
         L     R0,ACIREC                                                        
         GOTO1 VDATAMGR,DMCB,=C'OPEN',NAMWORK,INDEX,(R3),(R0)                   
         CLI   8(R1),0                                                          
         BNE   COPYWKQ             ERROR ON OPEN                                
*                                                                               
COPYWKA  TM    INPUT,TAPE          GET NEXT INPUT DATA RECORD                   
         BZ    COPYWKA4                                                         
         BAS   RE,RITAPE                                                        
         OC    0(2,R3),0(R3)                                                    
         BNZ   COPYWKC                                                          
COPYWKA2 MVC   0(2,R3),=H'12'      SEQ ERROR ON INPUT                           
         MVC   4(8,R3),EOFLAB                                                   
         MVC   8(3,R3),=C'ERR'                                                  
         MVC   12(12,R3),0(R3)                                                  
         B     COPYWKC                                                          
COPYWKA4 GOTO1 VDATAMGR,DMCB,=C'SEQ'                                            
         CLI   8(R1),0                                                          
         BNE   COPYWKA2                                                         
         CLC   4(4,R3),SOFLAB                                                   
         BE    COPYWKA2                                                         
*                                                                               
COPYWKC  SR    R7,R7               SET R7 NONZERO IF EOF LABEL RECORD           
         CLC   0(2,R3),=H'12'                                                   
         BNE   COPYWKE                                                          
         CLC   4(4,R3),EOFLAB                                                   
         BNE   COPYWKE                                                          
         LA    R7,=C'CLOSE'        SET NORMAL EOF                               
         CLC   8(3,R3),5(R3)                                                    
         BE    *+8                                                              
         LA    R7,=C'CLO/PUR'      SET ERROR EOF                                
*                                                                               
COPYWKE  TM    LOAD,YES            PASS FILE RECORD TO EXTERNAL                 
         BZ    COPYWKG                                                          
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),2             SET FILE DATA                                
         LTR   R7,R7                                                            
         BZ    *+8                                                              
         MVI   0(R1),3             SET FILE EOF                                 
         BASR  RE,RF                                                            
         LTR   R7,R7                                                            
         BNZ   COPYWKG                                                          
         CLI   0(R1),X'FF'         EXTERNAL DELETED THIS RECORD                 
         BE    COPYWKA                                                          
*                                                                               
COPYWKG  TM    OUTPUT,TAPE         PUT NEXT OUTPUT RECORD                       
         BZ    COPYWKG2                                                         
         BAS   RE,WOTAPE                                                        
         LTR   R7,R7                                                            
         BNZ   COPYWKI                                                          
         B     COPYWKA                                                          
COPYWKG2 LTR   R7,R7                                                            
         BNZ   COPYWKG4                                                         
         GOTO1 VDATAMGR,DMCB,=C'ADD'                                            
         CLI   8(R1),0                                                          
         BNE   COPYWKQ                                                          
         B     COPYWKA                                                          
COPYWKG4 GOTO1 VDATAMGR,DMCB,(R7)                                               
         CLI   8(R1),0                                                          
         BNE   COPYWKQ                                                          
*                                                                               
COPYWKI  LA    RF,=C'CLOSE'        END OF FILE RECORD                           
         CR    R7,RF                                                            
         BNE   COPYWKQ                                                          
         LH    RF,FULL             BUMP NUMBER OF FILES COPIED                  
         LA    RF,1(RF)                                                         
         STH   RF,FULL                                                          
         B     COPYWK0                                                          
*                                                                               
COPYWKQ  LA    R6,P                ERROR IN WORK FILE COPY                      
         MVC   0(20,R6),=C'ERROR IN COPY INDEX='                                
         LA    R6,20(R6)                                                        
         GOTO1 =V(HEXOUT),P1,SAVE1,(R6),16,=C'TOG'                              
         LA    R6,32(R6)                                                        
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
*                                                                               
         MVI   RTERR,4             SET RC=4 SOME ERRORS FOUND                   
*                                                                               
         CLC   12(12,R3),0(R3)     TEST EOF SEQUENCE INPUT TAPE                 
         BNE   COPYWK0             BACK FOR NEXT FILE                           
*                                                                               
         MVI   RTERR,8             SET RC=8 FATAL ERROR FOUND                   
*                                                                               
COPYWKS  TM    LOAD,YES            PASS CONTROL TO EXTERNAL AT EOF              
         BZ    COPYWKS2                                                         
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),X'FF'                                                      
         BASR  RE,RF                                                            
COPYWKS2 TM    OUTPUT,TAPE         CLOSE OUTPUT TAPE FILE                       
         BZ    *+8                                                              
         BAS   RE,RITAPEX4                                                      
         OC    FULL(2),FULL                                                     
         BNZ   COPYWKU                                                          
         MVC   P(15),=C'NO FILES COPIED'                                        
         GOTO1 =V(PRINTER)                                                      
         B     COPYWKX                                                          
*                                                                               
COPYWKU  MVC   P(35),=C'NNNN FILES COPIED FROM TAPE TO TAPE'                    
         TM    INPUT,TAPE                                                       
         BO    *+10                                                             
         MVC   P+23(4),=C'DISK'                                                 
         TM    OUTPUT,TAPE                                                      
         BO    *+10                                                             
         MVC   P+31(4),=C'DISK'                                                 
         LH    R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  P(4),DUB                                                         
         OI    P+3,X'F0'                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
COPYWKX  TM    INPUT,DISK          UNLOCK WKFILE AT END OF DISK COPY            
         BZ    GETPARM                                                          
         BAS   RE,WKUNLK                                                        
         B     GETPARM                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT WORK FILE RECORDS                                                       
***********************************************************************         
PRNTWK   L     R3,ACDREC           R3=A(WORK FILE DATA RECORD)                  
         XC    FULL,FULL                                                        
         XC    INDEX,INDEX         SET TO READ ALL DISK FILES                   
*                                                                               
PRNTWK0  BAS   RE,GETFILE          GET NEXT WORK FILE HEADER                    
         OC    0(2,R3),0(R3)                                                    
         BZ    PRNTWKS             END OF INPUT FILE                            
         LH    RF,FULL+2                                                        
         LA    RF,1(RF)            BUMP NUM OF FILES READ                       
         STH   RF,FULL+2                                                        
         LA    R5,28(R3)           R5=A(WORK FILE ACTUAL INDEX)                 
         USING WKRECD,R5                                                        
*                                                                               
PRNTWK2  MVC   FID,WKUSRID         SAVE FILE ID                                 
         MVI   DUB,0                                                            
PRNTWK2A BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   PRNTWK0                                                          
         BAS   RE,TSTFILE          FILTER ON FILE ID                            
         BNE   PRNTWK2B                                                         
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   PRNTWK2B                                                         
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   PRNTWK2B                                                         
         B     PRNTWK4                                                          
PRNTWK2B CLC   AFILALL,AFILTAB     SET TO CONTINUE IF SPECIFIC USER             
         BE    PRNTWK0                                                          
         MVI   DUB,X'FF'                                                        
         B     PRNTWK2A                                                         
*                                                                               
PRNTWK4  MVC   SAVE1(36),12(R3)    WORK FILE FOUND FOR PRINTING                 
         XC    RECNUM,RECNUM                                                    
         SR    R7,R7                                                            
         TM    INPUT,TAPE                                                       
         BO    PRNTWK6                                                          
         L     R0,ACIREC           COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,CIBLKLN                                                       
         LA    R1,256(R1)          PLUS SAVE AREA                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,ACIREC                                                        
         MVI   INDEX+13,X'C0'                                                   
         GOTO1 VDATAMGR,DMCB,=C'SEQ',NAMWORK,INDEX,(R3),(R0)                    
         CLC   8(3,R3),=C'ERR'     TEST FOR ERROR ON FIRST DISK READ            
         BNE   PRNTWK6                                                          
         LA    R7,=C'CLO/PUR'      SET R7 NONZERO IF BAD SOF RECORD             
         B     PRNTWK8                                                          
*                                                                               
PRNTWK6  TM    LOAD,YES            PASS FILE HEADER TO EXTERNAL                 
         BZ    PRNTWK8                                                          
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),1             SET FILE SOF                                 
         BASR  RE,RF                                                            
         CLI   0(R1),X'FF'                                                      
         BE    PRNTWK0             EXTERNAL DELETED THIS FILE                   
*                                                                               
PRNTWK8  OC    FULL(2),FULL        SET ACTION MESSAGE IF FIRST TIME             
         BNZ   PRNTWK8A                                                         
         MVC   P(33),=C'FILE DESCRIPTION AND DATA FOLLOWS'                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRNTWK8A ZAP   LINE,=P'99'         NEW PAGE FOR NEW FILE                        
         MVC   SAVE(14),FILUSER                                                 
         XC    FILUSER(14),FILUSER SET USER ID NAME                             
         MVC   FILUSER,WKUSRID                                                  
         BAS   RE,IDOUT                                                         
         MVC   P(7),=C'USER ID'                                                 
         MVC   P+10(10),FILIDA                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   FILUSER(12),WKUSRID SET FILE KEY = SPPX,DD,C,SEQN                
         XC    FILUSER,FILUSER                                                  
         BAS   RE,IDOUT                                                         
         MVC   FILUSER(14),SAVE                                                 
         MVC   P(9),=C'FILE KEY'                                                
         MVC   P+11(14),FILIDA                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   CISTAT,WKSTAT       SET STATUS                                   
         BAS   RE,STATOUT                                                       
         MVC   P(6),=C'STATUS'                                                  
         MVC   P+11(9),FILSTATA                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LTR   R7,R7               PRINT ERROR IF BAD SOF RECORD                
         BNZ   PRNTWKQ                                                          
*                                                                               
         MVC   P(7),=C'CREATED'    SET DATE/TIME CREATED                        
         GOTO1 =V(DATCON),P1,(1,WKDATEC),(8,P+10)                               
         OI    P+10,X'F0'                                                       
         MVI   P+17,C','                                                        
         UNPK  DUB+1(5),WKTIMEC(3)                                              
         MVC   DUB(2),DUB+1                                                     
         MVI   DUB+2,C'.'                                                       
         MVC   P+18(5),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(9),=C'RETN DAYS'                                               
         EDIT  (B2,WKRETN),(4,P+10),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(9),=C'FILE TYPE'                                               
         MVC   P+10(4),=C'DATA'                                                 
         TM    WKFTYPE,X'02'                                                    
         BZ    *+10                                                             
         MVC   P+10(7),=C'LIBRARY'                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(7),=C'COMMENT'                                                 
         MVC   P+10(16),WKCOMNT                                                 
         OC    P+10(16),SPACES                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(8),=C'NUM RECS'   SET NUMBER OF RECORDS                        
         EDIT  (B4,WKRECS),(6,P+10),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(8),=C'AVG RECL'   SET AVERAGE RECORD LENGTH                    
         EDIT  (B2,WKRECL),(4,P+10),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(7),=C'NUM CIS'    SET NUMBER OF CONTROL INTERVALS              
         EDIT  (B1,WKAGES),(3,P+10),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRNTWKA  TM    INPUT,TAPE          GET NEXT INPUT RECORD                        
         BZ    PRNTWKA4                                                         
         BAS   RE,RITAPE                                                        
         OC    0(2,R3),0(R3)                                                    
         BNZ   PRNTWKC                                                          
PRNTWKA2 MVC   0(2,R3),=H'12'      SEQ ERROR ON INPUT                           
         MVC   4(8,R3),EOFLAB                                                   
         MVC   8(3,R3),=C'ERR'                                                  
         MVC   12(12,R3),0(R3)                                                  
         B     PRNTWKC                                                          
PRNTWKA4 GOTO1 VDATAMGR,DMCB,=C'SEQ'                                            
         CLI   8(R1),0                                                          
         BNE   PRNTWKA2                                                         
         CLC   4(4,R3),SOFLAB                                                   
         BE    PRNTWKA2                                                         
*                                                                               
PRNTWKC  SR    R7,R7               SET R7 NONZERO IF EOF LABEL RECORD           
         CLC   0(2,R3),=H'12'                                                   
         BNE   PRNTWKC2                                                         
         CLC   4(4,R3),EOFLAB                                                   
         BNE   PRNTWKC2                                                         
         LA    R7,=C'CLOSE'        SET NORMAL EOF                               
         CLC   8(3,R3),5(R3)                                                    
         BE    *+8                                                              
         LA    R7,=C'CLO/PUR'      SET ERROR EOF                                
         B     PRNTWKE                                                          
PRNTWKC2 LH    RF,RECNUM           BUMP RECORD NUMBER                           
         LA    RF,1(RF)                                                         
         STH   RF,RECNUM                                                        
         OC    FILRECS,FILRECS                                                  
         BZ    PRNTWKE                                                          
         CH    RF,FILRECS                                                       
         BNH   PRNTWKE                                                          
         LA    R7,=C'CLOSE'        SET MAX REC COUNT EXCEEDED                   
*                                                                               
PRNTWKE  TM    LOAD,YES            PASS FILE RECORD TO EXTERNAL                 
         BZ    PRNTWKG                                                          
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),2             SET FILE DATA                                
         LTR   R7,R7                                                            
         BZ    *+8                                                              
         MVI   0(R1),3             SET FILE EOF                                 
         BASR  RE,RF                                                            
         LTR   R7,R7                                                            
         BNZ   PRNTWKG                                                          
         CLI   0(R1),X'FF'         EXTERNAL DELETED THIS RECORD                 
         BE    PRNTWKA                                                          
*                                                                               
PRNTWKG  LTR   R7,R7               PRINT RECORD                                 
         BNZ   PRNTWKI                                                          
         GOTO1 =V(PRINTER)                                                      
         LH    R0,RECNUM           SET RECORD NUMBER                            
         CVD   R0,DUB                                                           
         UNPK  P(4),DUB                                                         
         OI    P+3,X'F0'                                                        
         LH    R6,0(R3)            R6=RESIDUAL RECORD LENGTH                    
         LR    R4,R3               R4=A(NEXT RECORD BYTE)                       
*                                                                               
PRNTWKG0 CP    LINE,=P'58'         ADJUST IF TOO NEAR END OF PAGE               
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         LR    R0,R4               SET RECORD BYTE NUMBER                       
         SR    R0,R3                                                            
         AH    R0,=H'1'                                                         
         CVD   R0,DUB                                                           
         UNPK  P+5(4),DUB                                                       
         OI    P+8,X'F0'                                                        
*                                                                               
         LA    R0,100              SET R0=NUM OF CHRS IN LINE                   
         CR    R0,R6                                                            
         BL    *+6                                                              
         LR    R0,R6                                                            
         GOTO1 =V(HEXOUT),P1,(R4),WORK,(R0),=C'SEP'                             
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),WORK                                                     
         GOTO1 =V(PRINTER)                                                      
         LA    RE,WORK+1(R1)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),0(RE)                                                    
         GOTO1 =V(PRINTER)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+10(3),0(R4)                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AR    R4,R0               BUMP TO NEXT RECORD BYTE                     
         SR    R6,R0                                                            
         BP    PRNTWKG0                                                         
         B     PRNTWKA             BACK FOR NEXT RECORD                         
*                                                                               
PRNTWKI  LA    RF,=C'CLOSE'        END OF FILE RECORD                           
         CR    R7,RF                                                            
         BNE   PRNTWKQ                                                          
         LH    RF,FULL             BUMP NUMBER OF FILES PRINTED                 
         LA    RF,1(RF)                                                         
         STH   RF,FULL                                                          
         B     PRNTWK0             BACK FOR NEXT FILE                           
*                                                                               
PRNTWKQ  LA    R6,P                ERROR IN FILE ENCOUNTERED                    
         GOTO1 =V(PRINTER)                                                      
         MVC   0(9,R6),=C'**ERROR**'                                            
         GOTO1 =V(HEXOUT),P1,SAVE1,P+10,16,=C'TOG'                              
         LA    R6,42(R6)                                                        
         GOTO1 =V(PRINTER)                                                      
         CLC   12(12,R3),0(R3)     TEST EOF SEQUENCE ON INPUT                   
         BE    PRNTWKS                                                          
         B     PRNTWK0             BACK FOR NEXT FILE                           
*                                                                               
PRNTWKS  TM    LOAD,YES            PASS CONTROL TO EXTERNAL AT EOF              
         BZ    PRNTWKS2                                                         
         L     RF,ALOADPT                                                       
         LA    R1,PLXTRN                                                        
         MVI   0(R1),X'FF'                                                      
         BASR  RE,RF                                                            
PRNTWKS2 OC    FULL(2),FULL                                                     
         BNZ   PRNTWKX                                                          
         MVC   P(14),=C'NO FILES FOUND'                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRNTWKX  B     GETPARM                                                          
         EJECT                                                                  
***********************************************************************         
* REPORT ON WKFILE STATUS FOR ALL OR A SET OF USER IDS                          
***********************************************************************         
REPTWK   CLI   KDAYS,0             DEFAULT KEEP DAYS TO 1                       
         BNE   *+10                                                             
         MVC   FILKDAYS,=F'-1'                                                  
         CLI   LDAYS,0             DEFAULT LIVE DAYS TO 3                       
         BNE   *+10                                                             
         MVC   FILLDAYS,=F'-3'                                                  
         CLI   DDAYS,0             DEFAULT DEAD DAYS TO 1                       
         BNE   *+10                                                             
         MVC   FILDDAYS,=F'-1'                                                  
         CLI   HDAYS,0             DEFAULT HOLD DAYS TO 3                       
         BNE   *+10                                                             
         MVC   FILHDAYS,=F'-1'                                                  
*                                                                               
         MVC   DUB1,DATEVAL                                                     
         MVC   TODAY4,DUB1                                                      
         GOTO1 =V(DATCON),DMCB,(4,DUB1),(0,DUB)                                 
         MVC   TODAY0,DUB                                                       
         L     R5,FILKDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(1,FILKDATE)                            
         L     R5,FILLDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(1,FILLDATE)                            
         L     R5,FILDDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(1,FILDDATE)                            
         L     R5,FILHDAYS                                                      
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R5)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(1,FILHDATE)                            
*                                                                               
         ZAP   CICOUNT,=P'0'                                                    
         LA    R5,SORTC1A                                                       
         CLI   SORT,1                                                           
         BE    *+8                                                              
         LA    R5,SORTC1N                                                       
         GOTO1 =V(SORTER),DMCB,(R5),SORTC2,0                                    
         B     REPTWK2                                                          
*                                                                               
SORTC1A  DC    CL80'SORT FIELDS=(21,10,A,01,12,A,14,01,A),FORMAT=BI,WORX        
               K=1'                                                             
SORTC1N  DC    CL80'SORT FIELDS=(21,10,A,01,02,A,11,02,A,03,08,A,14,01,X        
               A),FORMAT=BI,WORK=1'                                             
SORTC2   DC    CL80'RECORD TYPE=F,LENGTH=30'                                    
*                                                                               
REPTWK2  L     R5,ACIREC           R5=A(CI REC)                                 
         USING WKRECD,R5                                                        
         LA    R6,SRTREC           R6=A(SORT REC)                               
         USING SRTRECD,R6                                                       
         SR    R7,R7               R7=NUMBER OF DATA CI'S                       
         ICM   R7,3,CICITOT                                                     
         AH    R7,CJCITOT                                                       
         LH    R1,CICINDX                                                       
         SR    R7,R1                                                            
         MH    R1,CITRKS                                                        
         LA    R1,1(R1)                                                         
         SLL   R1,16                                                            
         ST    R1,CIADDR           SET DISK ADDR OF FIRST DATA CI               
         MVI   CIADDR+2,1                                                       
*                                                                               
         CLI   REORG,YES                                                        
         BNE   REPTWK4                                                          
         CLI   WRITE,YES                                                        
         BNE   REPTWK4                                                          
         BAS   RE,WKLOCK           LOCK PRINT QUEUE                             
*                                                                               
REPTWK4  XC    SRTREC,SRTREC       INIT SORT REC AND READ CI REC                
         MVC   SRTADDR,CIADDR                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CIADDR,(R5)                         
         CLI   8(R1),0                                                          
         BE    REPTWK6                                                          
         MVC   SRTUSER,FFS         ERRORS HAVE HIGH KEY                         
         MVI   SRTSYSPG,1                                                       
         B     REPTWKA             ALL DISK ERRORS ARE LISTED                   
*                                                                               
REPTWK6  CLI   WKSTAT,WKSTPU       PURGED HAVE HIGH KEY                         
         BNE   *+14                                                             
         MVC   SRTUSER,FFS                                                      
         B     REPTWK8                                                          
         TM    WKSTAT,WKSTTE       TEMP CI'S HAVE HIGH KEY                      
         BZ    *+18                                                             
         MVC   SRTUSER,FFS                                                      
         MVI   SRTSYSPG,3                                                       
         B     REPTWK8                                                          
*                                                                               
         OC    WKUSRID,WKUSRID     ALL CI'S MUST HAVE VALID KEY                 
         BZ    REPTWK6A                                                         
         OC    WKSYSPRG,WKSYSPRG                                                
         BZ    REPTWK6A                                                         
         CLC   WKSYSPRG,SPACES                                                  
         BE    REPTWK6A                                                         
         OC    WKFILNO,WKFILNO                                                  
         BZ    REPTWK6A                                                         
         CLC   WKFILNO,=H'9999'                                                 
         BH    REPTWK6A                                                         
         CLI   WKSEQ,1                                                          
         BH    REPTWK7                                                          
*                                                                               
         OC    WKRECS,WKRECS       FRST CI MUST HAVE VALID DATA                 
         BNZ   *+12                                                             
         TM    WKFTYPE,X'02'       LIBRARY FILES CAN HAVE NO RECS               
         BZ    REPTWK6A                                                         
         OC    WKRECL,WKRECL                                                    
         BNZ   *+12                                                             
         TM    WKFTYPE,X'02'       LIBRARY FILES CAN HAVE ZERO REC LEN          
         BZ    REPTWK6A                                                         
         GOTO1 DATECHK,WKDATEC                                                  
         BNE   REPTWK6A                                                         
         GOTO1 TIMECHK,WKTIMEC                                                  
         BH    REPTWK6A                                                         
         B     REPTWK7                                                          
REPTWK6A MVC   SRTUSER,FFS         INVALID CI'S HAVE HIGH KEY                   
         MVI   SRTSYSPG,2                                                       
         B     REPTWK8                                                          
*                                                                               
REPTWK7  MVC   FID,WKUSRID         SAVE FILE ID FOR GOOD CI'S                   
         MVI   DUB,0                                                            
REPTWK7A BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   REPTWKB                                                          
         BAS   RE,TSTFILE          FILTER ON FILE                               
         BNE   REPTWK7B                                                         
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   REPTWK7B                                                         
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   REPTWK7B                                                         
         MVC   SRTUSER(14),WKINDEX BUILD REST OF SORT REC                       
         MVC   SRTNEXT,WKCINEXT                                                 
         B     REPTWKA                                                          
REPTWK7B CLC   AFILALL,AFILTAB     SET TO CONTINUE IF SPECIFIC STATUS           
         BE    REPTWKB                                                          
         MVI   DUB,X'FF'                                                        
         B     REPTWK7A                                                         
*                                                                               
REPTWK8  MVC   FID,WKUSRID         SAVE FILE ID FOR BAD CI'S                    
         MVI   DUB,0                                                            
         BAS   RE,TSTUSR           FILTER ON USER                               
         BNE   REPTWKB                                                          
         MVC   SRTNEXT,WKUSRID     SAVE DATA OF BAD/TEMP/PRGD CI'S              
         MVC   SRTNEXT+2(2),WKFILNO                                             
*                                                                               
REPTWKA  MVC   SRTKEY(2),SRTUSER   GET ALPHA USER ID AND PUT TO SORT            
         CLC   SRTKEY(2),FFS                                                    
         BE    REPTWKA2                                                         
         MVC   USERN,SRTUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   SRTKEY,USERA                                                     
REPTWKA2 GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         AP    CICOUNT,=P'1'                                                    
*                                                                               
REPTWKB  SR    R1,R1               BUMP TO NEXT CI                              
         ICM   R1,3,CIADDR                                                      
         CLM   R1,3,CJSTTRK                                                     
         BL    *+12                                                             
         AH    R1,CJTRKS           PART2  CI                                    
         B     *+8                                                              
         AH    R1,CITRKS           PART1  CI                                    
         STH   R1,CIADDR                                                        
         BCT   R7,REPTWK4                                                       
         CP    CICOUNT,=P'0'                                                    
         BNE   REPTWKC                                                          
         MVC   P(14),=C'NO FILES FOUND'                                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         GOTO1 (RF),(R1),=C'GET'                                                
         GOTO1 (RF),(R1),=C'GET'   FORCE SORTER COMPLETION                      
         B     GETPARM                                                          
*                                                                               
REPTWKC  MVC   P(21),=C'WKFILE REPORT FOLLOWS'                                  
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'99'         SET UP FOR PRINTING FILE                     
         MVC   MID1(120),HLINE1                                                 
         MVC   MID2(120),HLINE2                                                 
         MVC   MID3(120),HLINE3                                                 
         XC    SRTREC,SRTREC                                                    
         ZAP   CIINUSE,=P'0'                                                    
         ZAP   CIKEEP,=P'0'                                                     
         ZAP   CIERROR,=P'0'                                                    
         ZAP   CJINUSE,=P'0'                                                    
         ZAP   CJKEEP,=P'0'                                                     
         ZAP   CJERROR,=P'0'                                                    
         LA    R7,P                                                             
         USING REPTLD,R7           R7=A(REPORT LINE)                            
*                                                                               
REPTWKD  LA    R6,SRTREC                                                        
         MVC   SRTRECL,SRTREC                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+14                                                             
         MVC   SRTREC,0(RF)                                                     
         B     *+14                                                             
         MVC   SRTUSER,FFS         SET END OF FILE                              
         MVI   SRTSYSPG,X'FF'                                                   
         CLC   SRTUSER,FFS                                                      
         BE    REPTWKF                                                          
         CLC   SRTREC(12),SRTRECL  TEST FOR NEW FILE                            
         BNE   REPTWKF                                                          
*                                                                               
REPTWKE  L     RF,AQBNEXT          SAME FILE- SAVE SRTREC IN QBUFF              
         MVC   0(L'SRTREC,RF),SRTREC                                            
         LA    RF,L'SRTREC(RF)                                                  
         XC    0(L'SRTREC,RF),0(RF)                                             
         ST    RF,AQBNEXT          BUMP TO NEXT SLOT                            
         AP    CICOUNT,=P'1'       BUMP NUM OF CI'S IN FILE                     
         CP    CICOUNT,=P'255'                                                  
         BNH   REPTWKD             BACK FOR NEXT CI                             
*                                                                               
REPTWKF  OC    SRTRECL(2),SRTRECL  TEST FIRST TIME                              
         BZ    REPTWKN                                                          
         L     R6,=A(QBUFF)        TEST LAST TIME                               
         CLC   0(2,R6),FFS                                                      
         BE    REPTWKP                                                          
         MVC   CIADDR(2),SRTADDR   READ FIRST CI REC                            
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CIADDR,(R5)                         
         CLI   8(R1),0                                                          
         BE    REPTWKF1                                                         
         DC    H'0'                                                             
REPTWKF1 L     R1,ACXREC           SAVE FIRST CI DATA IN CXREC                  
         MVC   0(WKDATA1-WKRECD,R1),0(R5)                                       
         MVC   CISTAT,WKSTAT       SAVE OLD STATUS                              
         MVC   CIFILNO,WKFILNO     SAVE OLD SEQUENCE NUMBER                     
         CLI   WKSEQ,1             TEST FIRST CI SEQ NUM                        
         BNH   REPTWKG             SET DEFAULT ERROR VALUES                     
         MVC   WKBATTR-WKRECD(L'WKBATTR,R1),ERRBATTB                            
         MVC   WKFATTR-WKRECD(L'WKFATTR,R1),ERRFATTB                            
*                                                                               
REPTWKG  CLI   REORG,YES           REORGANISE MODE                              
         BNE   REPTWKM                                                          
REPTWKG1 TM    WKSTAT,WKSTTE       PURGE IF FIRST CI IN CREATE STATUS           
         BO    REPTWKGX                                                         
REPTWKG2 ZAP   DUB,CICOUNT                                                      
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         CLI   WKSEQ,0             PURGE IF MORE THAN ONE CI IF SINGLE          
         BNE   REPTWKG3                                                         
         CLI   DUB,1                                                            
         BE    REPTWKG4                                                         
         B     REPTWKGX                                                         
REPTWKG3 CLI   WKSEQ,1             PURGE IF FIRST CI SEQ NUM NOT 1              
         BNE   REPTWKGX                                                         
         L     R6,AQBNEXT          PURGE IF LAST CI SEQ NUM NOT TOTAL           
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         CLC   SRTSEQ,DUB                                                       
         BNE   REPTWKGX                                                         
REPTWKG4 L     R6,AQBNEXT          PURGE IF LAST CI LINK ADDR NOT ZERO          
         LA    R0,L'SRTREC                                                      
         SR    R6,R0                                                            
         OC    SRTNEXT,SRTNEXT                                                  
         BZ    REPTWKH                                                          
REPTWKGX MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,FFS         SET PURGED DUE TO ERROR                      
         B     REPTWKL                                                          
*                                                                               
REPTWKH  OC    WKRETN,WKRETN       PROCESS FILES WITH RETENTION DATE            
         BZ    REPTWKH1                                                         
         CLI   RETAIN,NO           WAS RETAIN = IGNORE ENTERED                  
         BE    REPTWKH1                                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(1,WKDATEC),(0,DUB)                              
         MVC   DUB1(2),WKRETN                                                   
         LH    R0,DUB1                                                          
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R0)                                     
         CLC   DUB1(6),TODAY0                                                   
         BNL   REPTWKK                                                          
         MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,=X'FFFE'    SET PURGED DUE TO DEAD DATE                  
         B     REPTWKL                                                          
*                                                                               
REPTWKH1 TM    WKSTAT,WKSTKE       PROCESS KEEP FILES                           
         BZ    REPTWKI                                                          
         CLC   WKDATEC,FILKDATE                                                 
         BNL   REPTWKK                                                          
         NI    CISTAT,255-WKSTKE   SET UNKEEP STATUS                            
*                                                                               
         TM    KPURGE,YES          KPURGE=Y PURGE KEEPS                         
         BZ    REPTWKI                                                          
         MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,=X'FFFE'    SET PURGED DUE TO KEEP DATE                  
         B     REPTWKL                                                          
*                                                                               
REPTWKI  TM    WKSTAT,WKSTAC                                                    
         BZ    REPTWKI1                                                         
         CLC   WKDATEC,FILLDATE                                                 
         BNL   REPTWKK                                                          
         MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,=X'FFFE'    SET PURGED DUE TO LIVE DATE                  
         B     REPTWKL                                                          
*                                                                               
REPTWKI1 TM    WKSTAT,WKSTHO                                                    
         BZ    REPTWKJ                                                          
         CLC   WKDATEC,FILHDATE                                                 
         BNL   REPTWKK                                                          
         MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,=X'FFFE'    SET PURGED DUE TO HOLD DATE                  
         B     REPTWKL                                                          
*                                                                               
REPTWKJ  TM    WKSTAT,WKSTUS                                                    
         BZ    REPTWKK                                                          
         CLC   WKDATEC,FILDDATE                                                 
         BNL   REPTWKK                                                          
         MVI   CISTAT,WKSTPU       SET NEW STATUS                               
         MVC   CIFILNO,=X'FFFD'    SET PURGED DUE TO DEAD DATE                  
         B     REPTWKL                                                          
*                                                                               
REPTWKK  LH    R1,USERSEQ          SET NEW SEQUENCE NUMBER                      
         LA    R1,1(R1)                                                         
         STH   R1,USERSEQ                                                       
         CLI   RESEQ,YES                                                        
         BNE   *+8                                                              
         STH   R1,CIFILNO                                                       
*                                                                               
REPTWKL  BAS   RE,FILEUPDT         UPDATE DATA CONTROL INTERVALS                
*                                                                               
REPTWKM  L     R1,ACXREC           RESTORE OLD FIRST CI DATA                    
         MVC   0(64,R5),0(R1)                                                   
         MVC   SAVE(14),FILUSER                                                 
         MVC   RLUSERA(8),FILUSERA SET USER ID NAME                             
         MVI   RLTYPE,C'.'         SET FILE TYPE                                
         TM    WKFTYPE,X'02'                                                    
         BZ    *+8                                                              
         MVI   RLTYPE,C'L'                                                      
         XC    FILUSER(14),FILUSER                                              
         MVC   FILSYSPG(6),WKSYSPRG                                             
         MVC   FILEXTRA,WKEXTRA                                                 
         BAS   RE,IDOUT            SET SPPX,DD,C,X                              
         MVC   RLID,FILIDA                                                      
         CLI   CIFILNO,X'FF'       SET FILE SEQ NUMBER                          
         BNE   *+14                                                             
         MVC   RLSEQ,DOTS                                                       
         B     REPTWKM1                                                         
         EDIT  (B2,CIFILNO),(4,RLSEQ)                                           
         OC    RLSEQ,ZEROS                                                      
*                                                                               
REPTWKM1 BAS   RE,STATOUT          SET STATUS                                   
         MVC   RLSTAT,FILSTATA                                                  
*                                                                               
REPTWKM2 GOTO1 =V(DATCON),DMCB,(1,WKDATEC),(8,RLCRTD)                           
         UNPK  DUB(5),WKTIMEC(3)                                                
         MVC   RLCRTD+9(4),DUB                                                  
         OC    WKRETN,WKRETN                                                    
         BNZ   *+14                                                             
*&&UK*&& MVC   RLRETN(7),DOTS                                                   
*&&US*&& MVC   RLRETN(8),DOTS                                                   
         B     REPTWKM3                                                         
         GOTO1 =V(DATCON),DMCB,(1,WKDATEC),(0,DUB)                              
         MVC   DUB1(2),WKRETN                                                   
         LH    R0,DUB1                                                          
         GOTO1 =V(ADDAY),DMCB,DUB,DUB1,(R0)                                     
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(8,RLRETN)                              
*                                                                               
REPTWKM3 EDIT  (B4,WKRECS),(5,RLRECS-1)                                         
         EDIT  (B2,WKRECL),(4,RLRECL)                                           
         EDIT  (P3,CICOUNT),(3,RLNCI)                                           
*                                                                               
REPTWKM4 ZAP   DUB,CICOUNT                                                      
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         MH    RF,CJTRKS                                                        
         AH    RF,CITRKS                                                        
         MH    RF,CIHIREC                                                       
         TM    WKFTYPE,X'02'                                                    
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         LH    RE,CIBLKLN                                                       
         SH    RE,=H'34'                                                        
         MR    RE,RE               RE&RF=MAX CAPACITY                           
         LH    R1,WKRECL                                                        
         L     R0,WKRECS                                                        
         MH    R0,=H'100'                                                       
         MR    R0,R0               R0&R1=ACT CAPACITY * 100                     
         DR    R0,RF                                                            
         SLDL  R0,32                                                            
         CH    R0,=H'100'          ADJUST FOR APPROX                            
         BNH   *+8                                                              
         LH    R0,=H'100'                                                       
         EDIT  (R0),(3,RLPCT),ZERO=NOBLANK                                      
*                                                                               
         MVC   RLCOMNT,WKCOMNT                                                  
         OC    RLCOMNT,SPACES                                                   
*                                                                               
REPTWKM5 CLI   REORG,YES                                                        
         BNE   REPTWKM6                                                         
         MVC   FLAG,CISTAT                                                      
         MVC   CISTAT,WKSTAT                                                    
         BAS   RE,STATOUT                                                       
         CLC   CIFILNO,FFS                                                      
         BNE   *+10                                                             
         MVC   FILSTATA(6),=C'ERROR '                                           
         MVC   RLOSTAT,FILSTATA                                                 
         EDIT  (B2,WKFILNO),(4,RLOSEQ)                                          
         OC    RLOSEQ,ZEROS                                                     
         MVC   CISTAT,FLAG                                                      
*                                                                               
REPTWKM6 MVC   FILUSER(14),SAVE                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
REPTWKM8 SP    CICOUNT,=P'1'                                                    
         TM    CISTAT,WKSTKE                                                    
         BZ    *+20                                                             
         AP    CIKEEP,=P'1'                                                     
         AP    CJKEEP,CICOUNT                                                   
         B     *+24                                                             
         TM    CISTAT,WKSTAC+WKSTHO                                             
         BZ    *+16                                                             
         AP    CIINUSE,=P'1'                                                    
         AP    CJINUSE,CICOUNT                                                  
         ZAP   CICOUNT,=P'0'                                                    
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         CLC   SRTREC(2),FFS                                                    
         BE    REPTWKMX                                                         
         CLC   SRTREC(2),SRTRECL                                                
         BE    REPTWKE                                                          
         B     REPTWKN                                                          
*                                                                               
REPTWKMX MVC   0(2,RF),FFS                                                      
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         B     REPTWKP                                                          
*                                                                               
REPTWKN  MVC   SAVE(14),FILUSER    GET NEW USER ID NAME                         
         XC    FILUSER(14),FILUSER                                              
         MVC   FILUSER,SRTREC                                                   
         BAS   RE,IDOUT                                                         
         MVC   FILUSER(14),SAVE                                                 
         MVC   FILUSERA,FILIDA     SAVE USER ID NAME                            
         ZAP   CICOUNT,=P'0'                                                    
         L     RF,=A(QBUFF)                                                     
         ST    RF,AQBNEXT                                                       
         XC    USERSEQ,USERSEQ                                                  
*                                                                               
         CLI   COMPACT,YES         TEST FOR COMPACT FORMAT                      
         BNE   REPTWKN4                                                         
         CP    LINE,MAXLINE        CHECK IF NEAR END OF PAGE                    
         BNL   REPTWKN2                                                         
         CP    LINE,=P'57'                                                      
         BNL   REPTWKN2                                                         
         GOTO1 =V(PRINTER)                                                      
         B     *+10                                                             
REPTWKN2 ZAP   LINE,=P'99'                                                      
         B     REPTWKNX                                                         
*                                                                               
REPTWKN4 ZAP   LINE,=P'99'         NEW USER = NEW PAGE FORMAT                   
         MVC   MID1(10),FILIDA                                                  
*                                                                               
REPTWKNX B     REPTWKE                                                          
*                                                                               
REPTWKP  LA    R6,SRTREC                                                        
         CLI   SRTSYSPG,X'FF'      TEST EOF                                     
         BE    REPTWKQ                                                          
         L     RF,=A(QBUFF)                                                     
         MVC   0(L'SRTREC,RF),SRTREC                                            
         XC    L'SRTREC(L'SRTREC,RF),L'SRTREC(RF)                               
         MVI   CISTAT,WKSTPU                                                    
         MVC   CIFILNO,SRTNEXT+2                                                
         MVC   FULL(2),SRTADDR                                                  
         MVC   FULL+2(2),=X'0100'                                               
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB1,4,=C'MIX'                              
*                                                                               
REPTWKP0 CLI   SRTSYSPG,0          TEST IF PURGED                               
         BNE   REPTWKP1                                                         
         OC    CIFILNO,CIFILNO     IGNORE ZERO NUMBERED CI'S                    
         BZ    REPTWKD                                                          
         BAS   RE,FILEUPDT                                                      
         B     REPTWKD                                                          
*                                                                               
REPTWKP1 CLI   SRTSYSPG,1          TEST DISK ERROR                              
         BNE   REPTWKP2                                                         
         MVC   P(15),=C'DISK READ ERROR'                                        
         MVC   P+17(8),DUB1                                                     
         B     REPTWKPA                                                         
*                                                                               
REPTWKP2 CLI   SRTSYSPG,2          TEST INVALID CI DATA                         
         BNE   REPTWKP3                                                         
         MVC   P(15),=C'INVALID CI DATA'                                        
         MVC   P+17(8),DUB1                                                     
         MVC   CIFILNO,FFS                                                      
         BAS   RE,FILEUPDT                                                      
         GOTO1 =V(HEXOUT),DMCB,SRTNEXT,P+27,8,=C'MIX'                           
         B     REPTWKPA                                                         
*                                                                               
REPTWKP3 CLI   SRTSYSPG,3          TEST TEMP CI                                 
         BNE   REPTWKP4                                                         
         BAS   RE,FILEUPDT                                                      
         B     REPTWKD                                                          
*                                                                               
REPTWKP4 DC    H'0'                                                             
*                                                                               
REPTWKPA GOTO1 =V(PRINTER)                                                      
         B     REPTWKD                                                          
*                                                                               
REPTWKQ  CLI   REORG,YES           REBUILD INDEX                                
         BNE   REPTWKR                                                          
         CLI   WRITE,YES                                                        
         BNE   REPTWKR                                                          
         BAS   RE,FILENDX                                                       
         BAS   RE,WKUNLK           UNLOCK PRINT QUEUE                           
*                                                                               
REPTWKR  ZAP   DUB,CIERROR                                                      
         BZ    *+14                                                             
         MVC   P(13),=C'FILES INVALID'                                          
         BAS   R7,REPTEDIT                                                      
         ZAP   DUB,CIKEEP                                                       
         BZ    *+14                                                             
         MVC   P(10),=C'FILES KEEP'                                             
         BAS   R7,REPTEDIT                                                      
         ZAP   DUB,CIINUSE                                                      
         BZ    *+14                                                             
         MVC   P(10),=C'FILES LIVE'                                             
         BAS   R7,REPTEDIT                                                      
*                                                                               
REPTWKS  CLI   USER,X'80'          DISPLAY AVAIL DATA IF USER=ALL               
         BE    REPTWKS1                                                         
         ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
REPTWKS1 SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         CVD   R0,DUB                                                           
         SP    DUB,CIERROR                                                      
         SP    DUB,CIKEEP                                                       
         SP    DUB,CIINUSE                                                      
         MVC   P(11),=C'FILES AVAIL'                                            
         BAS   R7,REPTEDIT                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ZAP   DUB,CJERROR                                                      
         BZ    *+14                                                             
         MVC   P(13),=C'PART2 CIS INV'                                          
         BAS   R7,REPTEDIT                                                      
         ZAP   DUB,CJKEEP                                                       
         BZ    *+14                                                             
         MVC   P(14),=C'PART2 CIS KEEP'                                         
         BAS   R7,REPTEDIT                                                      
         ZAP   DUB,CJINUSE                                                      
         BZ    *+14                                                             
         MVC   P(14),=C'PART2 CIS LIVE'                                         
         BAS   R7,REPTEDIT                                                      
         LH    R0,CJCITOT                                                       
         CVD   R0,DUB                                                           
         SP    DUB,CJERROR                                                      
         SP    DUB,CJKEEP                                                       
         SP    DUB,CJINUSE                                                      
         MVC   P(15),=C'PART2 CIS AVAIL'                                        
         BAS   R7,REPTEDIT                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,WKOUT                                                         
         ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         SPACE 2                                                                
REPTEDIT CVB   R0,DUB                                                           
         EDIT  (R0),(5,P+17),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR VALID DATES                                                         
***********************************************************************         
DATECHK  OC    0(3,R1),0(R1)       VALIDATE DATE FIELD AT R1                    
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO DATE                            
         B     DCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID DATE                      
*NOP*    CLI   0(R1),X'79'         DATES >=2000 ARE NOT VALID PACKED            
*NOP*    BL    DCHKX                                                            
*NOP*    CLI   0(R1),X'99'                                                      
*NOP*    BH    DCHKX                                                            
         CLI   1(R1),X'01'                                                      
         BL    DCHKX                                                            
         CLI   1(R1),X'12'                                                      
         BH    DCHKX                                                            
         CLI   2(R1),X'01'                                                      
         BL    DCHKX                                                            
         CLI   2(R1),X'31'                                                      
         BH    DCHKX                                                            
         SR    RF,RF               CC=EQUAL IF DATE OK                          
DCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
TIMECHK  OC    0(2,R1),0(R1)       VALIDATE TIME FIELD AT R1                    
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO TIME                            
         B     TCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID TIME                      
         CLI   0(R1),X'23'                                                      
         BH    TCHKX                                                            
         CLI   1(R1),X'59'                                                      
         BH    TCHKX                                                            
         SR    RF,RF               CC=EQUAL IF OK                               
TCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
WKLOCK   LA    R0,C'E'             LOCK/UNLOCK WKFILE FILE                      
         B     *+8                                                              
WKUNLK   LA    R0,C'D'                                                          
         TM    LOCK,YES            TEST IF LOCK REQUIRED                        
         BZR   RE                                                               
         ST    RE,CISAVRE                                                       
         L     RF,CIENQDEQ                                                      
         GOTO1 (RF),CIP1,((R0),ENQWORK)                                         
         L     RE,CISAVRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
WKOUT    NTR1                      PRINT CI DATA                                
*                                                                               
         MVC   P(13),=C'RECORD LENGTH'                                          
         LH    R0,CIBLKLN                                                       
         BAS   R5,WKOUT2                                                        
         MVC   P(14),=C'RECS PER TRACK'                                         
         LH    R0,CIHIREC                                                       
         BAS   R5,WKOUT2                                                        
         MVC   P(14),=C'TRKS FOR INDEX'                                         
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2                                                        
         MVC   P(14),=C'TRKS FOR PART1'                                         
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2                                                        
         MVC   P(14),=C'TRKS FOR PART2'                                         
         LH    R0,CJCITOT                                                       
         MH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(13),=C'TRKS/INDEX CI'                                          
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2                                                        
         MVC   P(13),=C'TRKS/PART1 CI'                                          
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2                                                        
         MVC   P(13),=C'TRKS/PART2 CI'                                          
         LH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(16),=C'NUM OF INDEX CIS'                                       
         LH    R0,CICINDX                                                       
         BAS   R5,WKOUT2                                                        
         MVC   P(16),=C'NUM OF PART1 CIS'                                       
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         BAS   R5,WKOUT2                                                        
         MVC   P(16),=C'NUM OF PART2 CIS'                                       
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         BAS   R5,WKOUT2                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(16),=C'INDEX ENTRYS/REC'                                       
         LH    R0,CIENTRYS                                                      
         BAS   R5,WKOUT2                                                        
         MVC   P(16),=C'INDEX TOTAL RECS'                                       
         LH    R0,CIPAGES                                                       
         BAS   R5,WKOUT2                                                        
         MVC   P(16),=C'INDEX PART1 RECS'                                       
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         BAS   R5,WKOUT2                                                        
         MVC   P(16),=C'INDEX PART2 RECS'                                       
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         BAS   R5,WKOUT2                                                        
         B     WKOUTX                                                           
*                                                                               
WKOUT2   EDIT  (R0),(6,P+17),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R5                                                               
*                                                                               
WKOUTX   XIT1                                                                   
         EJECT                                                                  
TSTUSR   ST    RE,DUB+4            TEST IF USERID AT FIDUSER IS WANTED          
         LA    RE,FILTAB                                                        
         CLI   DUB,0               START AT BEGINNING OF FILTAB                 
         BE    *+12                                                             
         L     RE,AFILTAB          CONTINUE SEARCH                              
         LA    RE,L'FILTAB(RE)                                                  
*                                                                               
TSTUSR2  CLC   0(2,RE),FFS         SEARCH FILTAB FOR USER                       
         BE    TSTUSR4                                                          
         MVC   DUB+2(2),0(RE)                                                   
         NI    DUB+2,X'7F'                                                      
         CLC   FIDUSER,DUB+2                                                    
         BE    TSTUSR6                                                          
         LA    RE,L'FILTAB(RE)                                                  
         B     TSTUSR2                                                          
*                                                                               
TSTUSR4  TM    USER,X'80'          USERID NOT IN TABLE                          
         BZ    TSTUSRN                                                          
         CLI   DUB,0                                                            
         BNE   TSTUSRN                                                          
         L     RE,AFILALL          USE ALL VALUE IF USER=ALL INPUT              
         B     TSTUSRY                                                          
*                                                                               
TSTUSR6  TM    0(RE),X'80'         USERID IS IN TABLE                           
         BZ    TSTUSRY                                                          
*                                                                               
TSTUSRN  LA    RE,1                CC=NEQ IF USERID NOT WANTED                  
         B     TSTUSRX                                                          
TSTUSRY  MVC   FILUSER(14),0(RE)   CC=EQL IF USERID IS WANTED                   
         OC    FILSEQL,FILSEQL                                                  
         BNZ   *+10                                                             
         MVC   FILSEQL,=H'1'                                                    
         OC    FILSEQH,FILSEQH                                                  
         BNZ   *+10                                                             
         MVC   FILSEQH,=H'9999'                                                 
TSTUSRY1 MVC   CLASSL(10),14(RE)                                                
         MVI   CLASSL+10,0                                                      
         MVC   FILSTAT,24(RE)                                                   
         MVC   FILRECS,25(RE)                                                   
         ST    RE,AFILTAB                                                       
         SR    RE,RE                                                            
*                                                                               
TSTUSRX  LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE                                                                 
***********************************************************************         
TSTFILE  ST    RE,DUB+4            TEST IF FILEID AT FID IS WANTED              
         OC    FILSYSPG(9),FILSYSPG                                             
         BZ    TSTFIY                                                           
*                                                                               
TSTFI2   CLI   FILSYSPG,0          FILTER ON SPPX                               
         BE    *+14                                                             
         CLC   FILSYSPG(1),FIDSYSPG                                             
         BNE   TSTFIN                                                           
         OC    FILSYSPG+1(2),FILSYSPG+1                                         
         BZ    *+14                                                             
         CLC   FILSYSPG+1(2),FIDSYSPG+1                                         
         BNE   TSTFIN                                                           
         CLI   FILSUBPG,0                                                       
         BE    *+14                                                             
         CLC   FILSUBPG(1),FIDSUBPG                                             
         BNE   TSTFIN                                                           
*                                                                               
TSTFI4   CLI   FILDAY,0            FILTER ON DAY/CLASS                          
         BE    TSTFI4A                                                          
         CLC   FILDAY(1),FIDDAY                                                 
         BNE   TSTFIN                                                           
*                                                                               
TSTFI4A  CLI   FILCLASS,0                                                       
         BE    TSTFI4B                                                          
         CLC   FILCLASS,FIDCLASS                                                
         BNE   TSTFIN                                                           
*                                                                               
TSTFI4B  CLI   FILEXTRA,0                                                       
         BE    TSTFI6                                                           
         CLC   FILEXTRA,FIDEXTRA                                                
         BNE   TSTFIN                                                           
*                                                                               
TSTFI6   CLC   FILSEQL,FIDSEQN     FILTER ON SEQUENCE NUMBER                    
         BH    TSTFIN                                                           
         CLC   FILSEQH,FIDSEQN                                                  
         BL    TSTFIN                                                           
         B     TSTFIY                                                           
*                                                                               
TSTFIN   LA    RE,1                CC=NEQ IF FILE NOT WANTED                    
         B     *+6                                                              
TSTFIY   SR    RE,RE               CC=EQL IF FILE IS WANTED                     
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLASS                                                                
***********************************************************************         
TSTCLASS ST    RE,DUB+4            TEST IF CLASS AT FIDCLASS IS WANTED          
         CLI   CLASSL,0                                                         
         BE    TSTCLY              NO CLASS SPECIFIED                           
         LA    RE,CLASSL+1                                                      
*                                                                               
TSTCL2   CLI   0(RE),0             SEARCH CLASS LIST                            
         BE    TSTCL4                                                           
         CLC   FIDCLASS,0(RE)                                                   
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
***********************************************************************         
* VALIDATE STATUS                                                               
***********************************************************************         
TSTSTAT  ST    RE,DUB+4            TEST IF STATUS AT FIDSTAT IS WANTED          
         MVC   DUB(1),FIDSTAT                                                   
         TM    DUB,WKSTTE                                                       
         BO    TSTSTN              IGNORE TEMPORARY FILES                       
         CLI   FILSTAT,0                                                        
         BE    TSTSTY                                                           
*                                                                               
TSTST2   MVC   DUB+1(1),FILSTAT                                                 
         TM    DUB+1,X'04'         TEST SPECIAL NOT KEEP STAT (UNKEEP)          
         BZ    *+12                                                             
         TM    DUB,WKSTKE                                                       
         BO    TSTSTN                                                           
         TM    DUB+1,WKSTKE                                                     
         BZ    *+12                                                             
         TM    DUB,WKSTKE                                                       
         BZ    TSTSTN                                                           
         NI    DUB+1,255-WKSTKE-X'04'                                           
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
***********************************************************************         
* UPDATE WKFILE DATA CI'S                                                       
***********************************************************************         
FILEUPDT NTR1  ,                   UPDATE WKFILE DATA CI'S                      
         CLI   REORG,YES                                                        
         BNE   FILEUPX                                                          
         CLI   WRITE,YES                                                        
         BNE   FILEUPX                                                          
         L     R6,=A(QBUFF)                                                     
         USING SRTRECD,R6                                                       
         L     R5,ACIREC                                                        
         USING WKRECD,R5                                                        
         LA    R0,L'SRTREC                                                      
*                                                                               
FILEUP2  OC    SRTUSER,SRTUSER                                                  
         BZ    FILEUPX             END OF CI LIST                               
         MVC   CIADDR(2),SRTADDR                                                
         MVC   CIADDR+2(2),=X'0100'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CIADDR,(R5)                         
         CLI   8(R1),0                                                          
         BNE   FILEUPE                                                          
         CLI   CISTAT,0            PURGE CI                                     
         BNE   *+14                                                             
         XC    WKINDEX,WKINDEX                                                  
         B     FILEUP4                                                          
         MVC   WKFILNO,CIFILNO     RESEQUENCE CI                                
         MVC   WKSTAT,CISTAT                                                    
         AR    R6,R0                                                            
         MVC   WKCINEXT,SRTADDR                                                 
         SR    R6,R0                                                            
*                                                                               
FILEUP4  GOTO1 (RF),(R1),DMWRT                                                  
         CLI   8(R1),0                                                          
         BNE   FILEUPE                                                          
         AR    R6,R0                                                            
         B     FILEUP2                                                          
*                                                                               
FILEUPE  MVC   P+RLSUNDRY-REPTLD(5),=C'ERROR '                                  
*                                                                               
FILEUPX  XIT1                                                                   
         EJECT                                                                  
FILENDX  NTR1                      UPDATE WKFILE INDEX FROM DATA CI'S           
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
FILENDX2 GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CXADDR,(R6)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ INDEX REC                   
*                                                                               
FILENDX4 CLC   0(L'WKINDEX,R7),FFS                                              
         BNE   *+6                                                              
         DC    H'0'                DIE IF INDEX CLOBBERED                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,NAMWORK,CIADDR,(R5)                         
         CLI   8(R1),0                                                          
         BNE   *+10                                                             
         MVC   0(L'WKINDEX,R7),0(R5)    COPY DATA TO INDEX IF NO ERRORS         
*                                                                               
FILENDX6 AH    R7,CINDXLN          BUMP TO NEXT INDEX ENTRY                     
         LH    RF,CXENTRY                                                       
         LA    RF,1(RF)                                                         
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   FILENDXA            END OF INDEX PAGE                            
         CLC   0(L'WKINDEX,R7),FFS                                              
         BNE   FILENDX8                                                         
         OC    CJCITOT,CJCITOT                                                  
         BZ    FILENDXA                                                         
         CLI   FLAG1,1             END OF PART 1 INDEX                          
         BNE   FILENDXA            NO EXIT                                      
         MVI   FLAG1,2             YES SET PART 2 INDEX                         
         B     FILENDX6            BYPASS END OF PART 1 INDEX ENTRY             
*                                                                               
FILENDX8 LA    RF,FILENDX4         BUMP TO NEXT DATA CI                         
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
FILENDXA GOTO1 VDATAMGR,DMCB,DMWRT,NAMWORK,CXADDR,(R6)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT WRITE INDEX REC                  
         CLC   CXENTRY,CIENTRYS                                                 
         BL    FILENDXX                                                         
         LR    R7,R6               RESET TO START OF NEW INDEX REC              
         XC    CXENTRY,CXENTRY                                                  
         SR    RE,RE               BUMP TO NEXT INDEX REC                       
         IC    RE,CXADDR+2                                                      
         LA    RE,1(RE)                                                         
         STC   RE,CXADDR+2                                                      
         CH    RE,CIHIREC                                                       
         BNH   FILENDXC                                                         
         LH    RE,CXADDR           BUMP TO NEXT INDEX TRACK                     
         LA    RE,1(RE)                                                         
         STH   RE,CXADDR                                                        
         MVI   CXADDR+2,1                                                       
*                                                                               
FILENDXC LA    RF,FILENDX2         BUMP TO NEXT DATA CI                         
         B     FILENDX8+4                                                       
*                                                                               
FILENDXX XIT1                                                                   
         EJECT                                                                  
STATOUT  NTR1                      EXPAND FILE STATUS TO FILSTATA               
         MVC   FILSTATA,SPACES                                                  
         LA    RE,FILSTATA                                                      
         LA    RF,3                RF=L'OUTPUT-1                                
         CLI   CISTAT,0                                                         
         BNE   *+14                                                             
         MVC   0(6,RE),=C'PURGED'                                               
         B     STATOUTX                                                         
*                                                                               
         TM    CISTAT,WKSTAC       MAIN STATUS = ACTV/HOLD/DLTD                 
         BZ    *+10                                                             
         MVC   DUB(4),=C'ACTV'                                                  
         TM    CISTAT,WKSTHO                                                    
         BZ    *+10                                                             
         MVC   DUB(4),=C'HOLD'                                                  
         TM    CISTAT,WKSTUS                                                    
         BZ    *+10                                                             
         MVC   DUB(4),=C'DLTD'                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DUB                                                      
         LA    RE,1(RE,RF)                                                      
*                                                                               
         TM    CISTAT,WKSTKE       SUB STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         LA    RF,0                                                             
         MVI   0(RE),C','                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),=C'KEEP'                                                 
         LA    RE,2(RE,RF)                                                      
*                                                                               
STATOUTX XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* EXPAND KEY TO READABLE FORMAT                                                 
***********************************************************************         
IDOUT    NTR1  ,                   EXPAND FILE ID TO FILIDA                     
         LA    R4,FILIDA                                                        
         MVC   FILIDA,SPACES                                                    
*                                                                               
IDOUT2   OC    FILUSER,FILUSER     FILE USER ID                                 
         BZ    IDOUT6                                                           
         MVC   USERN,FILUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   0(10,R4),USERA                                                   
         LA    R4,11(R4)                                                        
         OC    FILSYSPG(10),FILSYSPG                                            
         BZ    IDOUTX                                                           
*                                                                               
IDOUT6   MVC   0(4,R4),FILSYSPG    SYSTEM/PROGRAM/SUBPROG                       
         OC    0(4,R4),SPACES                                                   
         LA    R0,4                                                             
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                                                         
         BCT   R0,*-16                                                          
         LA    R4,1(R4)                                                         
*                                                                               
IDOUT8   MVC   0(2,R4),ZEROS       DAY NUMBER                                   
         CLI   FILDAY,0                                                         
         BE    *+16                                                             
         UNPK  DUB(3),FILDAY(2)                                                 
         MVC   0(2,R4),DUB                                                      
         LA    R4,3(R4)                                                         
*                                                                               
IDOUTA   MVC   0(1,R4),FILCLASS    CLASS                                        
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R4),C'*'                                                       
         LA    R4,2(R4)                                                         
*                                                                               
IDOUTB   MVC   0(1,R4),FILEXTRA    EXTRA                                        
         OI    0(R4),C' '                                                       
         LA    R4,2(R4)                                                         
*                                                                               
IDOUTC   OC    FILSEQL,FILSEQL     FILE NUMBER                                  
         BZ    IDOUTX                                                           
         EDIT  (B2,FILSEQL),(4,(R4))                                            
         LA    R4,5(R4)                                                         
*                                                                               
IDOUTX   OC    FILIDA,SPACES                                                    
         LA    R0,L'FILIDA                                                      
         GOTO1 =V(SQUASHER),P1,FILIDA,(C',',(R0))                               
         XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* EXPAND KEY TO READABLE FORMAT                                                 
***********************************************************************         
GETUSER  NTR1  ,                   GET USER ALPHA FROM USER NUMBER              
         MVI   DMCB1+8,0                                                        
         MVC   USERA,SPACES                                                     
         L     R5,=A(CTBUF)                                                     
*                                                                               
GUSER1   OC    0(2,R5),0(R5)       SEARCH USER ID TABLE                         
         BZ    GUSER2                                                           
         CLC   0(2,R5),FFS                                                      
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
***********************************************************************         
* GET NEXT WORK FILE FROM DISK OR TAPE                                          
***********************************************************************         
GETFILE  NTR1                                                                   
         L     R3,ACDREC                                                        
         TM    INPUT,TAPE                                                       
         BZ    GETF4                                                            
*                                                                               
GETF2    BAS   RE,RITAPE           READ TAPE UNTIL FILE HEADER FOUND            
         OC    0(2,R3),0(R3)                                                    
         BZ    GETFX               EXIT IF EOF                                  
         CLC   4(8,R3),SOFLAB                                                   
         BE    GETFX                                                            
         B     GETF2                                                            
*                                                                               
GETF4    L     R0,ACXREC           GET NEXT INDEX ENTRY USING CXREC             
         MVI   INDEX+13,0                                                       
         GOTO1 VDATAMGR,DMCB,=C'SEQ',NAMWORK,INDEX,(R3),(R0)                    
         CLI   8(R1),0                                                          
         BE    GETFX                                                            
         TM    8(R1),X'80'         EXIT IF EOF                                  
         BZ    *+14                                                             
         XC    0(4,R3),0(R3)                                                    
         B     GETFX                                                            
         DC    H'0'                ERROR ON INDEX READ                          
*                                                                               
GETFX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ INPUT TAPE                                                               
***********************************************************************         
RITAPE   ST    RE,RWSAVRE                                                       
         L     R0,=A(CDRECH)                                                    
         L     R1,=A(TAPEIN)                                                    
         GET   (1),(0)                                                          
         B     RITAPEXX                                                         
*                                                                               
RITAPEX  TM    MSG,YES            TEST IF MESSAGES SUPPRESSED                   
         BZ    RITAPEX2                                                         
         GOTO1 =V(LOGIO),DMCB,1,=C'ANY MORE INPUT TAPES'                        
         MVC   WORK,SPACES                                                      
         GOTO1 =V(LOGIO),DMCB,0,(3,WORK)                                        
         CLI   WORK,C'N'                                                        
         BE    RITAPEX2                                                         
         CLI   WORK,C'Y'                                                        
         BNE   RITAPEX                                                          
         BAS   RE,CLSIN            END OF INPUT VOLUME                          
         BAS   RE,OPNIN                                                         
         B     RITAPE+4                                                         
*                                                                               
RITAPEX2 BAS   RE,CLSIN            END OF INPUT FILE                            
         L     R1,=A(CDRECH)                                                    
         LA    R0,8                                                             
         SLL   R0,16                                                            
         ST    R0,0(R1)                                                         
         L     R1,ACDREC                                                        
         XC    0(4,R1),0(R1)       SET EOF IN RECORD HEADER                     
         B     RITAPEXX                                                         
*                                                                               
RITAPEX4 ST    RE,RWSAVRE          CLOSE OUTPUT TAPE IF SPECIFIED               
         TM    OUTPUT,TAPE                                                      
         BZ    RITAPEXX                                                         
         BAS   RE,CLSOUT                                                        
*                                                                               
RITAPEXX L     RE,RWSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE OUTPUT TAPE                                                             
***********************************************************************         
WOTAPE   ST    RE,RWSAVRE          BUILD V/L TAPE RECORD HEADER                 
         L     R0,=A(CDRECH)                                                    
         LR    R1,R0                                                            
         LH    RE,4(R1)                                                         
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,0(R1)                                                         
         L     R1,=A(TAPEOUT)                                                   
         PUT   (1),(0)                                                          
         L     RE,RWSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
*&&DO                              DOS TAPE OPEN/CLOSE ROUTINES                 
OPNIN    ST    RE,OCSAVRE                                                       
         L     R1,=A(TAPEIN)                                                    
         OPEN  (1)                                                              
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
CLSIN    ST    RE,OCSAVRE                                                       
         L     R1,=A(TAPEIN)                                                    
         CLOSE (1)                                                              
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
OPNOUT   ST    RE,OCSAVRE                                                       
         L     R1,=A(TAPEOUT)                                                   
         OPEN  (1)                                                              
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
CLSOUT   ST    RE,OCSAVRE                                                       
         L     R1,=A(TAPEOUT)                                                   
         CLOSE (1)                                                              
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*&&                                                                             
*&&OS                              MVS TAPE OPEN/CLOSE ROUTINES                 
OPNIN    ST    RE,OCSAVRE                                                       
         L     R2,=A(TAPEIN)                                                    
         OPEN  ((2),INPUT)                                                      
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
CLSIN    ST    RE,OCSAVRE                                                       
         L     R2,=A(TAPEIN)                                                    
         CLOSE ((2))                                                            
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
OPNOUT   ST    RE,OCSAVRE                                                       
         L     R2,=A(TAPEOUT)                                                   
         OPEN  ((2),OUTPUT)                                                     
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*                                                                               
CLSOUT   ST    RE,OCSAVRE                                                       
         L     R2,=A(TAPEOUT)                                                   
         CLOSE ((2))                                                            
         L     RE,OCSAVRE                                                       
         BR    RE                                                               
*&&                                                                             
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
***********************************************************************         
         DS    0F                                                               
PARMTBL  DS    0CL16                                                            
MODE     DC    X'0000C004',C'MODE    ',A(MODEL)                                 
INPUT    DC    X'00004001',C'INPUT   ',A(INPUTL)                                
OUTPUT   DC    X'00004001',C'OUTPUT  ',A(OUTPUTL)                               
MSG      DC    X'00024001',C'MSG     ',A(MSGL)                                  
WARN     DC    X'00024001',C'WARNINGS',A(WARNL)                                 
LOAD     DC    X'00000004',C'LOAD    ',A(LOADNAME)                              
PARAM    DC    X'00002102',C'PARAM   ',A(VALP)                                  
USER     DC    X'00002003',C'USER    ',A(VUSER)                                 
FILE     DC    X'00002002',C'FILE    ',A(VFILE)                                 
CLASS    DC    X'00002001',C'CLASS   ',A(VCLASS)                                
STATUS   DC    X'00002001',C'STATUS  ',A(VSTAT)                                 
KDAYS    DC    X'00002001',C'KDAYS   ',A(VKDAY)                                 
LDAYS    DC    X'00002001',C'LDAYS   ',A(VLDAY)                                 
DDAYS    DC    X'00002001',C'DDAYS   ',A(VDDAY)                                 
HDAYS    DC    X'00002001',C'HDAYS   ',A(VHDAY)                                 
REORG    DC    X'00014001',C'REORG   ',A(REORGL)                                
RESEQ    DC    X'00024001',C'RESEQ   ',A(RESEQL)                                
RETAIN   DC    X'00024001',C'RETAIN  ',A(RETAINL)                               
WRITE    DC    X'00024001',C'WRITE   ',A(WRITEL)                                
WRSRV    DC    X'00024001',C'WRSRV   ',A(WRSRVL)                                
COMPACT  DC    X'00024001',C'COMPACT ',A(COMPACTL)                              
SORT     DC    X'00024001',C'SORT    ',A(SORTL)                                 
RECS     DC    X'00002001',C'RECS    ',A(VRECS)                                 
LOCK     DC    X'00024001',C'LOCK    ',A(LOCKL)                                 
KPURGE   DC    X'00024001',C'KPURGE  ',A(KPURG)                                 
ENTRYS   DC    X'00002001',C'ENTRYS  ',A(VNCIS)                                 
CISIZE   DC    X'00002001',C'CISIZE  ',A(VTRKS)                                 
BLKSIZE  DC    X'00002003',C'BLKSIZE ',A(VBLKS)                                 
OENTRYS  DC    X'00002001',C'OENTRYS ',A(VNCIS)                                 
OCISIZE  DC    X'00002001',C'OCISIZE ',A(VTRKS)                                 
WFILE    DC    X'00000004',C'WKFILE  ',A(FILEID)                                
         DC    X'00000004',C'DAFILE  ',A(FILEID)                                
PARMTBLX DC    X'FFFF'                                                          
*                                                                               
LOADNAME DC    CL8' '                                                           
FILEID   DC    CL8' '              INPUT WORKER FILE NAME                       
CLASSL   DC    XL12'00'            MAX OF 10 CLASSES                            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
ERRNUM   DS    F                                                                
ACOMRG   DS    A                                                                
ALOADPT  DS    A                                                                
AQBNEXT  DS    A                                                                
RWSAVRE  DS    A                                                                
OCSAVRE  DS    A                                                                
FIRST    DS    F                                                                
LAST     DS    F                                                                
FRSTTIME DC    C'Y'                                                             
UPSIVAL  DC    X'00'                                                            
UPSIINP  DC    X'00'                                                            
FLAG     DC    X'00'                                                            
FLAG1    DC    X'00'                                                            
         DC    XL3'00'                                                          
DATEVAL  DC    CL8' '              C'DD/MM/YY'                                  
DATEYMD  DC    CL8' '              C'YYMMDD'                                    
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PARAM1   DS    F                                                                
PARAM2   DS    F                                                                
PARAM3   DS    F                                                                
PARAM4   DS    F                                                                
PARAM5   DS    F                                                                
PARAM6   DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
USERN    DS    H                                                                
USERA    DS    CL10                                                             
INDEX    DS    CL16                                                             
SAVE     DS    CL80                                                             
SAVE1    DS    CL80                                                             
WORK     DS    CL256                                                            
C        DS    CL80                                                             
*                                                                               
RDID     EQU   01                                                               
WTCKD    EQU   05                                                               
WTERASE  EQU   08                                                               
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
VWKFILE  DC    A(0)                                                             
FIDWKFI  DC    CL8' '                                                           
NAMWKFI  DC    CL8'WKFILE'                                                      
ENQWKFI  DC    CL4'WRKR'                                                        
*                                                                               
VFACWRK  DC    A(0)                                                             
FIDFACW  DC    CL8' '                                                           
NAMFACW  DC    CL8'FACWRK'                                                      
ENQFACW  DC    CL4'FACW'                                                        
*&&US                                                                           
VEASIWK  DC    A(0)                                                             
FIDEASI  DC    CL8' '                                                           
NAMEASI  DC    CL8'EASIWK'                                                      
ENQEASI  DC    CL4'EASI'                                                        
*&&                                                                             
DTFWORK  DC    A(0)                WORK FILE DTF ADDR                           
FIDWORK  DC    CL8' '              WORK FILE DTF FILE ID                        
NAMWORK  DC    CL8' '              WORK FILE NAME FOR DATAMGR                   
ENQWORK  DC    CL4' '              WORK FILE ENQUEUE ID FOR ENQDEQ              
*                                                                               
RTERR    DC    XL1'00'                                                          
*                                                                               
DMREAD   DC    C'DMREAD  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
SOFLAB   DC    C'*SOFSOF*'                                                      
EOFLAB   DC    C'*EOFEOF*'                                                      
PART2IND DC    XL4'00',XL10'7FFF7FFF000000010100',XL6'00'                       
ACDREC   DS    A                                                                
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
AFILTAB  DS    A                                                                
AFILALL  DS    A                                                                
CINDXMIN DC    H'2'                                                             
RECNUM   DS    H                                                                
*                                                                               
*DMWRKRW                                                                        
       ++INCLUDE DMWRKRW                                                        
         DS    2F                  EXTRA CIP4/CIP5 FOR LOCKER                   
*                                                                               
CISEQ    DS    XL1                                                              
CISTAT   DS    XL1                                                              
CIFILNO  DS    XL2                                                              
CICOUNT  DS    PL3                                                              
CIERROR  DS    PL3                                                              
CIINUSE  DS    PL3                                                              
CIKEEP   DS    PL3                                                              
CJCOUNT  DS    PL3                                                              
CJERROR  DS    PL3                                                              
CJINUSE  DS    PL3                                                              
CJKEEP   DS    PL3                                                              
USERSEQ  DS    H                                                                
         DS    H                                                                
*                                                                               
SRTREC   DS    CL30                                                             
SRTRECL  DS    CL30                                                             
*                                                                               
FID      DS    0CL14                                                            
FIDUSER  DS    XL2                                                              
FIDSYSPG DS    CL3                                                              
FIDSUBPG DS    CL1                                                              
FIDDAY   DS    PL1                                                              
FIDCLASS DS    CL1                                                              
FIDEXTRA DS    CL1                                                              
         DS    XL1                                                              
FIDSEQN  DS    XL2                                                              
FIDSTAT  DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
FILDEFN  DS    0CL14                                                            
FILUSER  DS    XL2                                                              
FILSYSPG DS    CL3                                                              
FILSUBPG DS    CL1                                                              
FILDAY   DS    PL1                                                              
FILCLASS DS    CL1                                                              
FILEXTRA DS    CL1                                                              
         DS    XL1                                                              
FILSEQL  DS    XL2                                                              
FILSEQH  DS    XL2                                                              
*                                                                               
FILRECS  DS    XL2                                                              
FILSTAT  DS    XL1                                                              
FILSTATA DS    CL11                                                             
FILUSERA DS    CL10                                                             
FILIDA   DS    CL32                                                             
FILKDATE DS    XL3                                                              
FILLDATE DS    XL3                                                              
FILDDATE DS    XL3                                                              
FILHDATE DS    XL3                                                              
         DS    XL1                                                              
FILKDAYS DS    F                                                                
FILLDAYS DS    F                                                                
FILDDAYS DS    F                                                                
FILHDAYS DS    F                                                                
TODAY0   DS    CL8                                                              
TODAY4   DS    CL8                                                              
*                                                                               
ERRBATTB DC    AL4(1),AL4(1),AL2(1),AL2(0)                                      
ERRFATTB DC    X'800101',X'1200',X'0001',X'00',XL8'00'                          
         DC    CL16'**ERROR**ERROR**'                                           
*                                                                               
HLINE1   DC    CL120'USER-ID T FILE-KEY    SEQN STATUS CREATED  TIME REX        
               TAINED RECS RECL NCI PCT COMMENT          OLDSTA OLDS'           
HLINE2   DC    CL120'------- - ----------- ---- ------ ------------- --X        
               ------ ---- ---- --- --- ---------------- ------ ----'           
HLINE3   DC    CL120' '                                                         
ZEROS    DC    20C'0'                                                           
DOTS     DC    20C'.'                                                           
FFS      DC    20X'FF'                                                          
MAXBLKQ  EQU   6140                                                             
*                                                                               
PLXTRN   DC    A(CDREC),A(TAPEOUT),A(PARMCARD),A(0)                             
         DC    V(PRINTER),V(CPRINT)                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    8X'FF',C'*FILTAB*',8X'FF'                                        
FILTAB   DS    50XL32                                                           
         DC    8X'FF'                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'***CD***'                                                      
CDRECH   DS    F                                                                
CDREC    DS    (MAXBLKQ)C          BLOCK SIZE MAX                               
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'***CI***'                                                      
CIREC    DS    8192C                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'***CX***'                                                      
CXREC    DS    8192C                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'***CB***'                                                      
QBUFF    DS    8192C                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'***WK***'                                                      
WKWORK   DS    8192D                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**UIDS**'                                                      
CTBUF    DC    0500XL12'00'                                                     
         DC    X'FFFF'                                                          
         SPACE 2                                                                
CTREC    DS    2048C                                                            
PARMCARD DS    CL80                                                             
         EJECT                                                                  
***********************************************************************         
* LISTS OF PARAMETER VALUES AND EQUATES                                         
***********************************************************************         
YES      EQU   X'02'                                                            
NO       EQU   X'01'                                                            
DISK     EQU   X'01'                                                            
TAPE     EQU   X'02'                                                            
*                                                                               
MODEL    DC    X'01',CL7'INIT'                                                  
         DC    X'02',CL7'PRINT'                                                 
         DC    X'03',CL7'REPORT'                                                
         DC    X'04',CL7'COPY'                                                  
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
RETAINL  DC    X'01',CL7'IGNORE'                                                
         DC    X'02',CL7'YES'                                                   
RETAINLX DC    X'FF'                                                            
*                                                                               
RESEQL   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
RESEQLX  DC    X'FF'                                                            
*                                                                               
WRITEL   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
WRITELX  DC    X'FF'                                                            
*                                                                               
WRSRVL   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
WRSRVLX  DC    X'FF'                                                            
*                                                                               
COMPACTL DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
COMPACTX DC    X'FF'                                                            
*                                                                               
SORTL    DC    X'01',CL7'ALPHA'                                                 
         DC    X'02',CL7'NUMERIC'                                               
SORTLX   DC    X'FF'                                                            
*                                                                               
LOCKL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
LOCKLX   DC    X'FF'                                                            
*                                                                               
KPURG    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
KPURGX   DC    X'FF'                                                            
         EJECT                                                                  
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=RITAPEX,        X        
               RECFM=VB,BLKSIZE=0,LRECL=6144,BUFNO=2                            
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,BLKSIZE=8200,LRECL=6144,BUFNO=2                         
**********************************************************************          
* SSB AND UTL                                                                   
**********************************************************************          
         DS    0D                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    XL255'00'                                                        
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'01'               SYSTEM 1  (SERVICE)                          
         ORG                                                                    
                                                                                
         DS    0D                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSRSQF)       SET RECOVERY OFF                             
         ORG                                                                    
         EJECT                                                                  
         DROP  R8,R9,RA            DROP WKMAINT CSECT'S EXTRA BASE REGS         
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                                  
* R0=A(WKMAINT CSECT PRINTER) AND R1=A(WKMAINT CSECT STORAGE)                   
***********************************************************************         
VALPARM  NMOD1 0,**VALP**                                                       
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
*                                                                               
VPARM1A1 MVC   DATEVAL,C+5         OVERRIDE SYSTEM IPL DATE                     
         GOTO1 =V(DATCON),DMCB,(4,DATEVAL),(0,DATEYMD)                          
         L     RF,=A(SSB)                                                       
         MVI   4(RF),X'80'         SET OFFLINE PASS OF DATE IN V(SSB)           
         MVC   8(6,RF),DATEYMD     PASS DATE AS C'YYMMDD'                       
         B     VPARM1P                                                          
*                                                                               
VPARM1B  CLC   C(6),=CL8'DDSIO='   DDSIO=XXXXXXXX TO SET WHICH DMDMGR           
         BNE   VPARM1C                                                          
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),C+6                                                      
         B     VPARM1P                                                          
*                                                                               
VPARM1C  CLC   C(7),=CL8'DSPACE='  DSPACE=                                      
         BNE   VPARM1X                                                          
         USING SSBD,RF                                                          
         L     RF,=V(SSB)                                                       
         MVC   SSODSPAC,C+7                                                     
         B     VPARM1P                                                          
         DROP  RF                                                               
*                                                                               
VPARM1P  GOTO1 =V(PRINTER)                                                      
         B     VPARM1                                                           
*                                                                               
VPARM1X  GOTO1 VDATAMGR,DMCB,=C'DMOPEN',=C'SER',=C'NCTFILE X'                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',=C'WKFILE'                              
         MVC   VWKFILE,DMCB+12                                                  
         USING DTFPHD,RE                                                        
         L     RE,VWKFILE                                                       
         MVC   FIDWKFI,DTFDD       EXTRACT ORIGINAL WKFILE ID                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',=C'FACWRK'                              
         MVC   VFACWRK,DMCB+12                                                  
         L     RE,VFACWRK                                                       
         MVC   FIDFACW,DTFDD       EXTRACT ORIGINAL FACWRK ID                   
*&&US                                                                           
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',=C'EASIWK'                              
         MVC   VEASIWK,DMCB+12                                                  
         L     RE,VEASIWK                                                       
         MVC   FIDEASI,DTFDD       EXTRACT ORIGINAL EASIWK ID                   
*&&                                                                             
         L     RE,=V(DMENQDEQ)     SAVE A(DDS ENQ/DEQ ROUTINE)                  
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
VPARM4   MVC   FILEID,SPACES                                                    
         LA    R1,FILTAB                                                        
         CLC   0(4,R1),FFS                                                      
         BE    *+18                                                             
         XC    0(L'FILTAB,R1),0(R1)                                             
         LA    R1,L'FILTAB(R1)                                                  
         B     *-20                                                             
         LA    R1,FILTAB                                                        
         LA    R0,L'FILTAB                                                      
         SR    R1,R0                                                            
         ST    R1,AFILTAB                                                       
         L     RE,=A(PARMCARD)                                                  
         MVC   0(80,RE),SPACES                                                  
         B     VPARM8                                                           
*                                                                               
VPARM6   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'*'                                                           
         BE    VPARMW              PRINT AND IGNORE COMMENT CARDS               
         CLC   C(2),=C'/*'                                                      
         BNE   *+12                                                             
         MVI   FRSTTIME,C'X'                                                    
         B     VPARMX                                                           
         CLC   C(2),MODE+4                                                      
         BE    VPARMX                                                           
*                                                                               
VPARM8   MVI   FLAG1,0             BUILD SINGLE EXTENDED SCANNER BLOCK          
         MVC   P(80),C                                                          
         MVC   C+72(8),SPACES                                                   
         L     R2,ACIREC                                                        
         XC    0(12,R2),0(R2)                                                   
         MVC   12(90,R2),SPACES                                                 
         LA    RE,C                FIND FIRST PARAM                             
         LA    RF,9                                                             
         CLI   0(RE),C'='                                                       
         BE    VPARM8B                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
VPARM8A  MVI   ERRNUM,2                                                         
         B     VPARMX                                                           
VPARM8B  LR    RF,RE                                                            
         LA    R1,C                                                             
         SR    RF,R1                                                            
         STC   RF,0(R2)                                                         
         SH    RF,=H'1'                                                         
         BNP   VPARM8A                                                          
         EX    RF,*+8              MOVE LEFT SIDE                               
         B     *+10                                                             
         MVC   12(0,R2),C                                                       
         LA    R1,69                                                            
         SR    R1,RF                                                            
         EX    R1,*+8              MOVE RIGHT SIZE                              
         B     *+10                                                             
         MVC   22(0,R2),1(RE)                                                   
         CLC   22(80,R2),SPACES                                                 
         BE    *+8                                                              
         MVI   1(R2),4                                                          
         LA    R0,1                                                             
         B     VPARMA                                                           
*                                                                               
VPARM9   MVI   FLAG1,1                                                          
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
         BE    VPARMC1                                                          
         LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BE    VPARMB                                                           
         B     VPARMC                                                           
VPARMC1  TM    2(R4),X'01'         IS PARAM A SINGLE CARD                       
         BO    VPARMC2             YES                                          
         CLI   FLAG1,0             NO RESCAN TO FETCH OTHERS                    
         BE    VPARM9                                                           
         B     VPARMD                                                           
VPARMC2  CLI   FLAG1,0             SINGLE MUST BE FIRST                         
         BNE   VPARMB                                                           
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
         MVC   0(8,RF),22(R2)      KEYWORD VALUE BY VALUE                       
         MVI   0(R4),YES                                                        
         B     VPARMV                                                           
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
         B     VPARMV                                                           
*                                                                               
VPARMG   BASR  RE,RF               GO TO ROUTINE WITH R2=A(SCAN ENTRY)          
         CLI   ERRNUM,0                                                         
         BNE   VPARMX                                                           
         B     VPARMV                                                           
*                                                                               
VPARMV   LA    R2,32(R2)           BUMP TO NEXT PARM                            
         BCT   R0,VPARMA                                                        
*                                                                               
VPARMW   GOTO1 =V(PRINTER)         PRINT CARD                                   
         B     VPARM6              GO GET NEXT PARM CARD                        
*                                                                               
VPARMX   XMOD1 1                                                                
         EJECT                                                                  
* PARAMETER VALUE ROUTINES                                                      
*                                                                               
VNCIS    MVI   0(R4),1             NUMBER OF CONTROL INTERVALS                  
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VNCISERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'65000'        MAXIMUM VALUE                                
         BH    VNCISERR                                                         
         C     RF,=F'01'           MINIMUM VALUE                                
         BL    VNCISERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STH   RF,CICITOT                                                       
         B     *+8                                                              
         STH   RF,CJCITOT                                                       
         B     *+8                                                              
VNCISERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
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
* VERIFY BLOCK SIZE                                                             
***********************************************************************         
VBLKS    MVI   BLKSIZE,1           BLOCK SIZE IN BYTES                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VBLKSERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=AL4(MAXBLKQ+4)  MAXIMUM BLOCK SIZE                           
         BH    VBLKSERR                                                         
         C     RF,=F'3660'         MINIMUM BLOCK SIZE                           
         BL    VBLKSERR                                                         
         STH   RF,CIBLKLN                                                       
         B     *+8                                                              
VBLKSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
VRECS    MVI   RECS,1              NUMBER OF RECORDS                            
         CLI   MODE,2                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VRECSERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VRECSERR                                                         
         STH   RF,FILRECS                                                       
         L     RF,AFILTAB          SAVE IN FILTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VRECSERR                                                         
         MVC   25(2,RF),FILRECS                                                 
         B     *+8                                                              
VRECSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
VKDAY    MVI   KDAYS,1             KEEP NUMBER OF DAYS                          
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
         ST    RF,FILKDAYS                                                      
         B     *+8                                                              
VKDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
VLDAY    MVI   LDAYS,1             LIVE NUMBER OF DAYS                          
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
         ST    RF,FILLDAYS                                                      
         B     *+8                                                              
VLDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
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
         ST    RF,FILDDAYS                                                      
         B     *+8                                                              
VDDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
VHDAY    MVI   HDAYS,1             DEAD NUMBER OF DAYS                          
         CLI   MODE,3                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VHDAYERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BNP   VHDAYERR                                                         
         CH    RF,=H'1000'                                                      
         BH    VHDAYERR                                                         
         LNR   RF,RF                                                            
         ST    RF,FILHDAYS                                                      
         B     *+8                                                              
VHDAYERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
         SPACE 2                                                                
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
         L     RF,AFILTAB          SAVE IN FILTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VCLASSE                                                          
         MVC   14(10,RF),CLASSL                                                 
         B     *+8                                                              
*                                                                               
VCLASSE  MVI   ERRNUM,4                                                         
VCLASSX  XIT1                                                                   
         SPACE 2                                                                
VSTAT    NTR1                      FILE STATUS                                  
         MVI   STATUS,1                                                         
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VSTATX                                                           
         MVI   FILSTAT,0                                                        
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
         OI    FILSTAT,WKSTAC                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'H'                                                       
         BNE   *+12                                                             
         OI    FILSTAT,WKSTHO                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'L'                                                       
         BNE   *+12                                                             
         OI    FILSTAT,WKSTAC                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'K'                                                       
         BNE   *+12                                                             
         OI    FILSTAT,WKSTKE                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'D'                                                       
         BNE   *+12                                                             
         OI    FILSTAT,WKSTUS                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'U'          SPECIAL UNKEEP STATUS                        
         BNE   *+12                                                             
         OI    FILSTAT,X'04'                                                    
         B     VSTAT6                                                           
         B     VSTATERR                                                         
*                                                                               
VSTAT6   LA    R5,1(R5)                                                         
         BCT   R0,VSTAT4                                                        
         L     RF,AFILTAB          SAVE IN FILTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VSTATERR                                                         
         MVC   24(1,RF),FILSTAT                                                 
         B     *+8                                                              
VSTATERR MVI   ERRNUM,4                                                         
VSTATX   XIT1                                                                   
         SPACE 2                                                                
VUSER    NTR1                      USERID = ALL OR XXX...                       
         CLI   MODE,2                                                           
         BNL   *+12                                                             
VUSER0   MVI   ERRNUM,3                                                         
         B     VUSERX                                                           
         MVC   FILUSERA,SPACES                                                  
         CLI   22(R2),C'-'                                                      
         BNE   *+14                                                             
         MVC   FILUSERA(9),23(R2)                                               
         B     *+10                                                             
         MVC   FILUSERA,22(R2)                                                  
         XC    DUB(2),DUB                                                       
         CLC   FILUSERA(4),=C'ALL '                                             
         BE    VUSER3                                                           
         L     R5,=A(CTREC)                                                     
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'I'                                                       
         MVC   15(10,R5),FILUSERA                                               
         GOTO1 VDATAMGR,DMCB1,DMREAD,=C'CTFILE',(R5),(R5)                       
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
         MVC   DUB(2),2(R5)                                                     
*                                                                               
VUSER3   OC    DUB(2),DUB          ONLY ONE USER=ALL ALLOWED                    
         BNZ   VUSER4                                                           
         TM    USER,X'80'                                                       
         BO    VUSERERR                                                         
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
VUSER6   L     RF,AFILTAB          BUMP TO NEXT FILTAB ENTRY                    
         LA    RF,L'FILTAB(RF)                                                  
         CLC   0(4,RF),FFS                                                      
         BE    VUSERERR            FILTAB IS FULL                               
         ST    RF,AFILTAB                                                       
         MVC   0(2,RF),DUB                                                      
         OC    DUB(2),DUB                                                       
         BNZ   *+8                                                              
         ST    RF,AFILALL          SAVE A(ALL ENTRY)                            
         LA    RF,L'FILTAB(RF)     SET END OF TABLE                             
         MVC   0(2,RF),FFS                                                      
         B     *+8                                                              
VUSERERR MVI   ERRNUM,4                                                         
VUSERX   XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* VALIDATE FILE ID                                                              
***********************************************************************         
VFILE    NTR1  ,                   FILE ID = ALL OR SPPXDDCX,LLLL,HHHH          
         ST    R2,FULL                                                          
         CLI   MODE,2                                                           
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VUSERX                                                           
*                                                                               
         MVI   FILE,1                                                           
         XC    FILSYSPG(12),FILSYSPG                                            
         ZIC   RF,1(R2)            MUST BE 1 THRU 8 CHRS                        
         CLI   1(R2),1                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),8                                                          
         BH    VFILEERR                                                         
         LA    RE,22(R2)                                                        
         CLI   0(RE),C'*'          REPLACE *'S BY NULLS                         
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
VFILE0   CLI   1(R2),3             FILE=ALL IS OK                               
         BNE   *+14                                                             
         CLC   22(3,R2),=C'ALL'                                                 
         BE    VFILE0X                                                          
         MVC   FILSYSPG(1),22(R2)                                               
         CLI   1(R2),2                                                          
         BL    VFILE0X             FILE=S IS OK                                 
         BE    VFILEERR                                                         
         MVC   FILSYSPG+1(2),23(R2)                                             
         OC    FILSYSPG+1(2),FILSYSPG+1                                         
         BZ    VFILE0A                                                          
         CLI   FILSYSPG+1,0                                                     
         BE    VFILEERR                                                         
         CLI   FILSYSPG+2,0                                                     
         BE    VFILEERR                                                         
VFILE0A  CLI   1(R2),4             FILE=SPP IS OK (SO IS S**)                   
         BL    VFILE0X                                                          
         MVC   FILSUBPG(1),25(R2)                                               
         BE    VFILE0X             FILE=SPPX IS OK                              
VFILE0B  CLI   1(R2),6             DAY MUST BE NN                               
         BL    VFILEERR                                                         
         OC    26(2,R2),26(R2)                                                  
         BZ    VFILE0C                                                          
         MVC   DUB1(2),ZEROS                                                    
         MVZ   DUB1(2),26(R2)                                                   
         CLC   DUB1(2),ZEROS                                                    
         BNE   VFILEERR                                                         
         PACK  DUB1(2),26(3,R2)                                                 
         MVC   FILDAY,DUB1                                                      
*                                                                               
VFILE0C  CLI   1(R2),7             FILE=SPPXDD IS OK                            
         BL    VFILE0X                                                          
         MVC   FILCLASS,28(R2)                                                  
         CLI   1(R2),8             FILE=SPPXDDC IS OK                           
         BL    VFILE0X                                                          
         MVC   FILEXTRA,29(R2)                                                  
*                                                                               
VFILE0X  LA    R4,FILSEQL                                                       
*                                                                               
VFILE2   LA    R2,32(R2)           BUMP SCANNER BLOCK                           
         BCT   R0,*+8                                                           
         B     VFILE6                                                           
*                                                                               
         CLI   1(R2),0             EXIT IF END OR KEYWORD                       
         BNE   VFILE6                                                           
         TM    2(R2),X'80'         CHECK SEQUENCE NUMBER                        
         BZ    VFILE4                                                           
         CLC   4(4,R2),=F'1'                                                    
         BL    VFILEERR                                                         
         CLC   4(4,R2),=F'9999'                                                 
         BH    VFILEERR                                                         
         MVC   0(2,R4),6(R2)                                                    
         LA    R5,FILSEQH                                                       
         CR    R4,R5               SECOND SEQUENCE NUM                          
         BE    VFILE8              YES EXIT                                     
         LR    R4,R5               NO POINT TO SECOND                           
         B     VFILE2                                                           
VFILE4   LA    R5,FILSEQH          HIGH SEQUENCE CAN BE 'END'                   
         CR    R4,R5                                                            
         BNE   VFILEERR                                                         
         CLC   12(3,R2),=C'END'                                                 
         BNE   VFILEERR                                                         
         MVC   0(2,R4),FFS                                                      
*                                                                               
VFILE6   SH    R2,=H'32'           DECR SCANNER BLOCK                           
         AH    R0,=H'1'                                                         
VFILE8   OC    FILSEQL,FILSEQL                                                  
         BNZ   *+16                                                             
         MVC   FILSEQL,=H'1'                                                    
         MVC   FILSEQH,=H'9999'                                                 
         OC    FILSEQH,FILSEQH                                                  
         BNZ   *+10                                                             
         MVC   FILSEQH,FILSEQL                                                  
         CLC   FILSEQH,FILSEQL                                                  
         BL    VFILEERR                                                         
         L     RF,AFILTAB          SAVE IN FILTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VFILEERR                                                         
         MVC   2(12,RF),FILSYSPG                                                
         B     VFILEX                                                           
VFILEERR L     R2,FULL                                                          
         MVI   ERRNUM,4                                                         
VFILEX   XIT1  REGS=(R0,R2)                                                     
         EJECT ,                                                                
***********************************************************************         
* PARAMETER CARD FOR EXTERNAL                                                   
***********************************************************************         
VALP     L     RF,=A(PARMCARD)     PARAMETER CARD FOR EXTERNAL                  
         MVI   0(R4),YES                                                        
         MVC   0(80,RF),22(R2)                                                  
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* CONSTANTS AND DSECTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
REPTLD   DSECT ,                   REPORT LINE                                  
RLUSERA  DS    CL7                                                              
         DS    CL1                                                              
RLTYPE   DS    CL1                                                              
         DS    CL1                                                              
RLID     DS    CL11                                                             
         DS    CL1                                                              
RLSEQ    DS    CL4                                                              
         DS    CL1                                                              
RLSTAT   DS    CL6                                                              
         DS    CL1                                                              
RLCRTD   DS    CL13                                                             
         DS    CL1                                                              
RLRETN   DS    CL8                                                              
         DS    CL1                                                              
RLRECS   DS    CL4                                                              
         DS    CL1                                                              
RLRECL   DS    CL4                                                              
         DS    CL1                                                              
RLNCI    DS    CL3                                                              
         DS    CL1                                                              
RLPCT    DS    CL3                                                              
         DS    CL1                                                              
RLCOMNT  DS    CL16                                                             
         DS    CL1                                                              
RLOSTAT  DS    CL6                                                              
         DS    CL1                                                              
RLOSEQ   DS    CL4                                                              
         DS    CL1                                                              
RLSUNDRY DS    CL8                                                              
*                                                                               
SRTRECD  DSECT ,                   SORT RECORD                                  
SRTUSER  DS    XL2                                                              
SRTSYSPG DS    CL3                                                              
SRTSUBPG DS    CL1                                                              
SRTDAY   DS    PL1                                                              
SRTCLASS DS    CL1                                                              
SRTEXTRA DS    CL1                                                              
         DS    CL1                                                              
SRTFILNO DS    XL2                                                              
SRTSTAT  DS    XL1                                                              
SRTSEQ   DS    XL1                                                              
SRTADDR  DS    XL2                                                              
SRTNEXT  DS    XL2                                                              
         DS    XL2                                                              
SRTKEY   DS    CL10                                                             
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
*FASSBOFF                                                                       
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DMWRKRM   10/07/15'                                      
         END                                                                    
