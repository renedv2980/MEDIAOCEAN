*          DATA SET SPREPR302  AT LEVEL 021 AS OF 09/12/12                      
*PHASE SPR302A,*                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SPREPR302 (SPR302) - REP -> SPOT BUY REP FILE UPDATE'           
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR302 (SPR302) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          REP FILE UPDATE FROM SPOT FILE ADDS       *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
* QOPT1/2 (COL 62/63)   =   REP SYSTEM NUMBER (IN HEX)               *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
* WHILE NOT EOF ON REPIN FILE                                        *          
*     -WRITE RECORD TO THE REP FILE                                  *          
* END WHILE                                                          *          
*                                                                    *          
* PRODUCE CONTROL REPORT                                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JUL15/91 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*  FEB03/93 (BU ) --- >MAKE SYSTEM # ASSIGNMENT SOFT                 *          
*                                                                    *          
*  FEB04/00 (BU ) --- >FORCE DUMP ON TERMINATION ERROR               *          
*                                                                    *          
*  NOV29/06 (BU ) --- >DMOPEN FOR UPDATE                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
SPR302   CSECT                                                                  
         NMOD1 0,SPR302,R9,R8,RR=R2                                             
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,RA                                                    
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAINLINE                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE EQU   *                                                                
         MVC   P(24),=C'* * * START OF RUN * * *'                               
         GOTO1 REPORT                                                           
         BAS   RE,INIT                                                          
         BNZ   MAINERR                                                          
         BAS   RE,READIN                                                        
         BNZ   MAINERR                                                          
         BAS   RE,DOREPORT                                                      
         BNZ   MAINERR                                                          
*                                                                               
*        END MAINLINE / FINISH RUN                                              
*                                                                               
MAINGOOD EQU   *                                                                
         MVC   P(21),=C'*** END OF REPORT ***'                                  
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
MAINERR  EQU   *                                                                
         MVC   P(40),=C'* * * ERROR ENCOUNTERED DURING RUN * * *'               
         GOTO1 REPORT                                                           
         MVC   P(60),RUNERROR                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=C'* * * RUN TERMINATED * * *'                             
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
MAINEXIT EQU   *                                                                
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        INIT --- SET INITIAL VALUES FOR THIS RUN                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         L     RE,UTL                                                           
         MVC   SPOTSYS,4(RE)                                                    
*                                                                               
         LA    RE,IOAREA1                                                       
         ST    RE,AREC                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'              FIND CONTROL FILE ID RECORD               
         MVC   WORK+23(02),RCORIGID   LOAD AGENCY CODE                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                NO  - SHOULD HAVE BEEN THERE...              
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND THE SYS AUTHORIZATION ELEMENT           
INIT0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   INIT0020            FOUND                                        
         CLI   2(R1),X'08'         IS IT 'REP SYSTEM'?                          
         BE    INIT0030                                                         
INIT0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   INIT0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
INIT0030 EQU   *                                                                
         MVC   REPSYS,3(R1)        LOAD REP SYSTEM NUMBER                       
         L     RE,UTL                                                           
         MVC   4(1,RE),REPSYS                                                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AREC,DMWORK             
*                                                                               
         OPEN  (REPIN,(INPUT))                                                  
         LTR   RF,RF                                                            
         BZ    INIT20                                                           
         DC    H'0'                                                             
INIT20   EQU   *                                                                
*                                                                               
         GET   REPIN,REPDATA              GET AND PROCESS THE HEADER            
         OC    REPDATA(2),REPDATA                                               
         BNZ   INIT105                                                          
         CLC   REPDATA+2(6),=C'HEADER'                                          
         BE    INIT110                                                          
INIT105  EQU   *                                                                
         MVC   RUNERROR(L'NOHEADER),NOHEADER                                    
         B     INITBAD                                                          
INIT110  EQU   *                                                                
         GOTO1 =V(DATVAL),DMCB,(0,REPDATA+50),WORK+0,RR=RELO                    
         OC    DM1,DM1                                                          
         BZ    INIT115             NO DATE IN FILE = NO FILE                    
         L     R2,VMASTC                                                        
         USING MASTD,R2                                                         
         GOTO1 =V(DATVAL),DMCB,(0,MCDATE),WORK+6,RR=RELO                        
         DROP  R2                                                               
         OC    DM1,DM1                                                          
         BZ    INIT115                                                          
         CLC   WORK+0(6),WORK+6                                                 
*                                                                               
*   SKIP DATE CHECK - UNLIKELY TO BE OUT OF SYNCH                               
*                                                                               
         B     INIT120             UNCONDITIONAL BRANCH                         
         BE    INIT120             ORIGINAL BRANCH                              
INIT115  EQU   *                                                                
         MVC   RUNERROR(L'BADDATE),BADDATE                                      
         B     INITBAD                                                          
INIT120  EQU   *                                                                
*                                                                               
*        INIT EXIT                                                              
*                                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
INITBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        READIN --- READ AND PROCESS THE REP INPUT FILE                         
*                                                                               
READIN   NTR1                                                                   
*                                                                               
RRIN10   EQU   *                                                                
         LA    RE,REPDATA                 CLEAR THE INPUT AREA                  
         LA    RF,REPDATAX-REPDATA                                              
         XCEFL                                                                  
         GET   REPIN,REPDATA                                                    
         L     RF,INRECS                                                        
         A     RF,=F'1'                                                         
         ST    RF,INRECS                                                        
         CLI   REPDATA,X'FF'              LOGICAL EOF?                          
         BE    RRINGOOD                                                         
         XC    REPKEY,REPKEY                                                    
         MVI   REPKEY,X'0B'                                                     
         MVC   REPKEY+16(11),REPDATA                                            
         BAS   RE,REPHIGH                                                       
         CLC   REPKEY(27),REPKSAVE                                              
         BE    RRIN50                                                           
         MVC   RUNERROR(L'BUYGONE),BUYGONE                                      
         GOTO1 REPORT                                                           
         B     RRIN99                                                           
RRIN50   EQU   *                                                                
         CLI   REPDATA+11,0                                                     
         BNE   RRIN60                                                           
*                                                                               
*  TEST                                                                         
         MVC   P+1(11),=C'KEY SOUGHT:'                                          
         MVC   P+13(27),REPKSAVE                                                
         GOTO1 REPORT                                                           
*  TEST END                                                                     
*                                                                               
         MVC   RUNERROR(L'LINEMISS),LINEMISS                                    
         MVC   P+1(18),=C'SPOT LINE MISSING:'                                   
         L     RF,AREC                                                          
         MVC   P+22(27),0(RF)      INSERT KEY                                   
         GOTO1 REPORT                                                           
         B     RRIN10              GO BACK FOR NEXT RECORD                      
***      B     RRINBAD                                                          
RRIN60   EQU   *                                                                
         BAS   RE,REPGET                                                        
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE '),(X'08',AREC),(0,0),  X        
               (0,0),RR=RELO                                                    
         CLI   DM4,0                                                            
         BE    RRIN70                                                           
         MVC   RUNERROR(L'SPOTMISS),SPOTMISS                                    
         MVC   P+1(18),=C'SPOT DATA MISSING:'                                   
         L     RF,AREC                                                          
         MVC   P+22(27),0(RF)      INSERT KEY                                   
         GOTO1 REPORT                                                           
         B     RRIN10              GO BACK FOR NEXT RECORD                      
***      B     RRINBAD                                                          
RRIN70   EQU   *                                                                
         L     R4,DM4                                                           
         USING RBUYSPEL,R4                                                      
         MVC   RBUYSPL#,REPDATA+11                                              
         MVC   RBUYSPDT,REPDATA+12                                              
         MVC   RBUYSPTM,REPDATA+15                                              
         CLI   QOPT2,C'T'          TEST SHOT?                                   
         BE    RRIN80              YES - DON'T REWRITE                          
         BAS   RE,REPPUT                                                        
RRIN80   EQU   *                                                                
         CLI   QOPT2,C'T'          TEST SHOT?                                   
         BNE   RRIN90              NO  - DON'T PRINT OUTPUT                     
         MVC   P+1(30),=C'NEW SPOTPAK INTERFACE ELEMENT='                       
         MVC   P+31(48),0(R4)                                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
RRIN90   EQU   *                                                                
         L     RF,OUTRECS                                                       
         A     RF,=F'1'                                                         
         ST    RF,OUTRECS                                                       
RRIN99   EQU   *                                                                
         B     RRIN10                  GET NEXT RECORD                          
*                                                                               
RRINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
RRINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
RRINERR  EQU   *                                                                
         MVC   RUNERROR(L'BADEOF),BADEOF                                        
         B     RRINBAD                                                          
         EJECT                                                                  
*                                                                               
*        DOREPORT --- PRODUCE EXTRACT CONTROL REPORT                            
*                                                                               
DOREPORT NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(36),=CL36'* * * RUN TOTALS * * *'                              
         GOTO1 REPORT                                                           
         MVC   P(31),=CL31'NUMBER OF UPDATE RECORDS READ ='                     
         EDIT  (B4,INRECS),(6,P+32),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         MVC   P(27),=CL27'NUMBER OF RECORDS WRITTEN ='                         
         EDIT  (B4,OUTRECS),(6,P+32),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
*                                                                               
*        DOREPORT EXIT                                                          
*                                                                               
DREPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
DREPBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (DIRECTORY)                     
*                                                                               
         SPACE 3                                                                
REPREAD  MVC   COMMAND,DMREAD                                                   
         B     REPDIR                                                           
         SPACE 2                                                                
REPSEQ   MVC   COMMAND,DMRSEQ                                                   
         B     REPDIR                                                           
         SPACE 2                                                                
REPHIGH  MVC   COMMAND,DMRDHI                                                   
         MVC   REPKSAVE,REPKEY                                                  
         B     REPDIR                                                           
         SPACE 2                                                                
REPDIR   NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   REPKSAVE,REPKEY                                                  
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR  ',REPKSAVE,      X        
               REPKEY                                                           
         B     DMCHECK                                                          
         SPACE 3                                                                
*                                                                               
*        SPOTPAK COMMUNICATION WITH DATA MANAGER (FILE)                         
*                                                                               
         SPACE 3                                                                
REPGET   MVC   COMMAND,GETREC                                                   
         OI    DMINBTS,X'80'                                                    
         B     REPFILE                                                          
REPPUT   EQU   *                                                                
         MVC   COMMAND,PUTREC                                                   
*                                                                               
REPFILE  EQU   *                                                                
         NTR1                                                                   
         LA    R2,REPKEY+28                                                     
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFIL  ',(R2),AREC,     X        
               DMWORK,0                                                         
         NI    DMINBTS,X'7F'                                                    
         SPACE 3                                                                
DMCHECK  EQU   *                                                                
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
DMERRS   EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*        SPOT GETEL                                                             
*                                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*        ERROR TEXTS                                                            
*                                                                               
NOHEADER DC    C'THE REP UPDATE FILE IS MISSING THE HEADER RECORD.'             
BADDATE  DC    C'THE REP UPDATE FILE HAS THE WRONG DATE.'                       
BADEOF   DC    C'BAD EOF MARKER FOUND ON THE REP UPDATE DATA FILE.'             
BUYGONE  DC    C'THIS BUY RECORD IS MISSING FROM THE REP FILE.'                 
SPOTMISS DC    C'SPOTPAK DATA IS MISSING FROM THIS BUYLINE.'                    
LINEMISS DC    C'THE SPOTPAK BUYLINE NUMBER IS MISSING FROM THIS LINE.'         
         EJECT                                                                  
*                                                                               
* DCB'S                                                                         
*                                                                               
REPIN    DCB   DDNAME=REPIN,DSORG=PS,RECFM=FB,LRECL=1000,              X        
               BLKSIZE=1000,MACRF=GM,EODAD=RRINERR                              
         EJECT                                                                  
*                                                                               
*        LITERAL POOL                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        REP FILE LIST                                                          
*                                                                               
         DS    0D                                                               
REPFLIST DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR '                                                    
         DC    CL8'UREPRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
*        IO AREA                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IOAREA*'                                                    
IOAREA1  DS    2008C                                                            
IOAREA1X EQU   *                                                                
IOAREA1L EQU   IOAREA1X-IOAREA1                                                 
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*LOCAL**'                                                    
INRECS   DS    F            NUMBER OF INPUT RECORDS READ                        
OUTRECS  DS    F            NUMBER OF RECORDS WRITTEN TO THE REP FILE           
RUNERROR DS    CL60                                                             
ELCODE   DS    X                                                                
REPKEY   DS    CL34                                                             
REPKSAVE DS    CL34                                                             
SPOTSYS  DS    X            SPOT SYS NUMBER (TO AND FROM UTL)                   
REPSYS   DS    X            REP SYS NUMBER                                      
*                                                                               
*        INPUT FILE AREA                                                        
*                                                                               
REPDATA  EQU   *                                                                
         DS    1000C        WILL HOLD ONLY A REP SYSTEM RECORD                  
REPDATAX EQU   *                                                                
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPR302 09/12/12'                                      
         END                                                                    
