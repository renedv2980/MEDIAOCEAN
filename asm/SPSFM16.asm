*          DATA SET SPSFM16    AT LEVEL 142 AS OF 12/13/04                      
*PHASE T21716A                                                                  
***********************************************************************         
*                                                                               
* B E W A R E !  THIS PROGRAM CONTAINS HARD-CODED DISPLACEMENTS!                
*                                                                               
* IF YOU ARE CHANGING SAVED SAVE AREA, DON'T FORGET TO CHANGE                   
* DISPLACEMENTS IN CODE, OR ELSE IT WILL CAUSE YOU A LOT OF GRIEF ...           
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE        T21716 - NEW SID RECORD COMPETITION MAINTENANCE       *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY                                      *         
*                                                                     *         
*  INPUTS       SCREEN T217B6 (MAINTENANCE)                           *         
*                                                                     *         
*  OUTPUTS      UPDATED SID COMPETITION RECORDS                       *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- FIRST SYSD BASE                                 *         
*               R6 -- THIRD PROGRAM BASE                              *         
*               R7 -- SECOND PROGRAM BASE                             *         
*               R8 -- WORK                                            *         
*               R9 -- WORK                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST PROGRAM BASE                              *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - COMPETITION RECORD                              *         
*               IO2 - SID DETAIL RECORD                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21716 - NEW SID RECORD COMPETITION MAINTENANCE'                
T21716   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21716,RR=R3                                                   
*                                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING T21716,RB,R7,R6                                                  
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R5,ASYSD                                                         
         USING SYSD,R5                                                          
         ST    RD,SAVERD                                                        
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'N'          I/O FOR GENCON                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BE    BSE050                                                           
         MVI   IOOPT,C'Y'          NO I/O FOR GENCON                            
*                                                                               
BSE050   CLI   MODE,VALKEY         VALIDATE DETAIL KEY                          
         BNE   LOOP50                                                           
         BRAS  RE,VK                                                            
         B     STEXIT                                                           
*                                                                               
LOOP50   CLI   MODE,DISPREC        VALIDATE DETAIL RECORD                       
         BE    LOOP70                                                           
         CLI   MODE,VALREC         VALIDATE DETAIL RECORD                       
         BNE   STEXIT                                                           
*                                                                               
LOOP70   DS    0H                                                               
*                                                                               
         BAS   RE,FSTRPT                                                        
         BAS   RE,DISREC                                                        
*                                                                               
STEXIT   XC    SRCMSG,SRCMSG                                                    
         CLI   NDETSW,X'FF'                                                     
         BNE   STEX20                                                           
         MVC   SRCMSG(45),=C'** WARNING - THIS SCREEN WILL NOT BE SAVEDX        
                **'                                                             
STEX20   OI    SRCMSGH+6,X'80'                                                  
         OI    SRCMSGH+1,X'08'                                                  
         OI    SRCBK1H+6,X'40'                                                  
         CLC   CONACT(3),=CL3'SEL'                                              
         JNE   XIT                                                              
         GOTO1 SAVEUWKA            SAVE SYSD                                    
XIT      XIT1                                                                   
* FIRST TIME FOR REPORT                                                         
*                                                                               
FSTRPT   NTR1                                                                   
         CLI   TWASTA,X'FF'                                                     
         BNE   FRX                                                              
*                                                                               
         MVC   SRCSRC(1),DEMOSRC   GET BKVALSRC FIELD                           
         MVI   SRCSRCH+5,X'01'                                                  
         LA    R2,SRCSRCH                                                       
         GOTO1 VALISRC                                                          
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
         MVC   SVSELMED,DBSELMED                                                
         MVC   SVSELSRC,DBSELSRC                                                
*                                                                               
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   FR05                NO                                           
*****    CLI   SRCSRC,C'A'         TEST ARBITRON                                
*****    BNE   *+14                NO                                           
*****    MVC   SRCSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
*****    B     FR05                                                             
*****    MVC   SRCSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
*                                                                               
FR05     XC    SVSTANO,SVSTANO                                                  
*                                                                               
         L     R3,AIO2                                                          
         SR    R0,R0                                                            
         LA    R4,SRCSCHH          TURN PREV VALIDATED BITS ON                  
*                                                                               
         L     RE,AIO1                                                          
         USING COMPETE,RE                                                       
         DROP  RE                                                               
*                                                                               
FR10     TM    FVATRB-FVIHDR(R4),FVAPROT                                        
         BO    *+8                                                              
         OI    FVIIND-FVIHDR(R4),FVIVAL                                         
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FR10                                                             
*                                                                               
         XC    SVUPDAY,SVUPDAY     CLEAR SAVED VALUES                           
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVBKS,SVBKS                                                      
         XC    SVSTA,SVSTA                                                      
         MVI   SVENDSTA,X'FF'                                                   
         XC    SVDEM,SVDEM                                                      
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         L     R4,AIO                                                           
         MVI   ELCODE,EUPCODEQ     DETAIL DEFAULT UPGRADE ELEMENT               
         BRAS  RE,GETEL                                                         
         BNE   FR15                                                             
*                                                                               
         USING EUPELEM,R4                                                       
         MVC   SVUPFILE(1),EUPUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EUPUPGRD                                              
         MVC   SVUPFRBK(2),EUPUPFBK                                             
         MVC   SVUPINP(16),EUPUPINP                                             
         DROP  R4                                                               
*                                                                               
FR15     L     R4,AIO                                                           
         MVI   ELCODE,EOVCODEQ     DETAIL DEFAULT DAY/TIME ELEMENT              
         BRAS  RE,GETEL                                                         
         BNE   FR25                                                             
*                                                                               
         USING EOVELEM,R4                                                       
         MVC   SVUPDAY(1),EOVUPDAY   STORE UPGRADE EXPRESSION                   
         MVC   SVUPTIM(4),EOVUPTIM                                              
         DROP  R4                                                               
*                                                                               
FR25     BAS   RE,DEMO             INSPECT DEMO FIELD                           
*                                                                               
         BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         OC    SVBKS,SVBKS                                                      
         BNZ   FR60                BOOKS INPUT                                  
*                                                                               
         OC    PERBOOKS,PERBOOKS   NONE - TEST FOR PERIOD DEFINED BOOKS         
         BZ    *+14                                                             
         MVC   SVBKS,PERBOOKS             YES - USE THOSE                       
         B     FR48                                                             
*                                                                               
*                                  NO BATCH DEFINED BOOKS -                     
         XC    DBLOCK,DBLOCK       GET LATEST BOOK FROM DEMAND                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIO3                                                      
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
*****    MVC   DBSELMED,QMED                                                    
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   DBSELMED,C'C'       CHANGE MEDIA TO CANADIEN                     
*****    MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELSTA,QSTA                                                    
         L     RE,ATWA                                                          
         MVC   DBSELAGY,14(RE)                                                  
         MVI   DBFUNCT,DBGETTLB                                                 
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                                                             
         MVC   0(2,R4),DBACTBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    FR46                NO - THEN INCLUDE LATEST BOOK                
*                                                                               
FR34     MVC   0(2,R4),DBACTBK     DETERMINE ALL FOUR BOOKS                     
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    *+14                                                             
         CLC   0(2,R4),SVUPFRBK    YES - COMPARE TO SHARE BOOK MONTH            
         BE    FR46                                                             
         CLC   0(2,R4),SVUPGRD+2   COMPARE TO PUT MONTH                         
         BE    FR46                                                             
         LA    R0,4                                                             
         LA    RE,MAJBKS                                                        
*                                                                               
FR36     CLC   1(1,R4),0(RE)                                                    
         BE    FR46                                                             
         BL    FR40                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,FR36                                                          
*                                                                               
FR40     CLC   0(1,RE),MAJBKS                                                   
         BNE   FR44                                                             
         MVC   1(1,R4),MAJBKS+3                                                 
         ZIC   RF,DBACTBK                                                       
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
         STC   RF,0(R4)                                                         
         B     FR46                                                             
*                                                                               
FR44     BCTR  RE,0                                                             
         SR    RF,RF                                                            
FR45     IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,1(R4)                                                         
         CLC   1(1,R4),0(RE)                                                    
         BNH   FR46                                                             
         CLC   0(2,R4),SVUPFRBK                                                 
         BE    FR46                                                             
         CLC   0(2,R4),SVUPGRD+2                                                
         BE    FR46                                                             
         B     FR45                                                             
*                                                                               
FR46     ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         STC   RF,DBACTBK+1                                                     
         B     FR47                                                             
         MVI   DBACTBK+1,12                                                     
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
*                                                                               
FR47     BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         BCT   R1,FR34                                                          
*                                                                               
FR48     CLI   SVUSEDDS,C'N'       IF EST/COMP USER THEN 4 BOOKS                
         BE    FR48A                                                            
         BRAS  RE,BOOKSET                                                       
FR48A    LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R8,SRCBK1H                                                       
         MVI   WORK+2,1                                                         
*                                                                               
FR49     CLI   0(R4),X'00'                                                      
         BE    FR50                                                             
         MVC   WORK(2),0(R4)                                                    
         GOTO1 DATCON,DMCB,(3,WORK),(6,8(R8))                                   
         OI    6(R8),FVOXMT                                                     
FR50     LA    R4,2(R4)                                                         
         LA    R8,SRCBK2H-SRCBK1H(R8)                                           
         BCT   R0,FR49                                                          
*                                                                               
*--READ COMPETITION STATION LIST RECORD SEE IF IT EXISTS                        
*                                                                               
FR60     XC    SVSTA(100),SVSTA                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D47'                                                
         MVC   KEY+2(1),SAVEKEY+1  A/M CODE                                     
         MVC   KEY+3(2),BMKT       MARKET                                       
         MVC   KEY+5(2),SAVEKEY+2  SCHEME                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BE    FR63                                                             
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+5(2),KEY+5                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BNE   FR70                                                             
*                                                                               
FR63     MVC   AIO,AIO3             RESTORE AIO                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R8,AIO3                                                          
         USING CLSKEY,R8                                                        
*                                                                               
         SR    R9,R9                                                            
         LA    R1,CLSSTA           LOOK FOR STATION ELEMENT                     
         LA    R3,SVSTA                                                         
         LA    R4,CLSEL                                                         
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LA    RE,5                                                             
FR65     MVC   0(5,R3),0(R1)                                                    
         LA    R9,1(R9)                                                         
         LA    R3,5(R3)                                                         
         BXLE  R1,RE,FR65                                                       
         STC   R9,SVSTANO                                                       
         DROP  R8                                                               
*                                                                               
*--READ COMPETITION RECORD SEE IF IT EXISTS                                     
*                                                                               
FR70     SR    RE,RE               SET UP YEAR MONTH FIELD                      
         ICM   RE,1,SCRYEAR                                                     
         OC    SCRYEAR,SCRYEAR     GET YEAR FROM SCREEN                         
         BNZ   FR71                                                             
         ICM   RE,1,CURYEAR        GET YEAR FROM SCHEME                         
FR71     SRDA  RE,32(0)                                                         
         D     RE,=F'10'                                                        
         SLA   RE,4(0)                                                          
         SR    RF,RF                                                            
         ICM   RF,1,PERNUM                                                      
         OR    RE,RF                                                            
         STC   RE,COMPER                                                        
*                                                                               
         L     R4,AIO1                                                          
         USING COMPETE,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0C01'                                                
         MVC   KEY+2(1),SAVEKEY+1  A/M CODE                                     
         MVC   KEY+3(2),SAVEKEY+2  SCHEME CODE                                  
         MVC   KEY+5(1),COMPER     Y/M                                          
         MVC   KEY+6(2),BMKT       MARKET                                       
         MVC   KEY+8(5),DAYTIME    DAY TIME                                     
         MVC   COMPKEY(13),KEY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST COMPETITION EXISTS                     
         BNE   FR80                                                             
*                                                                               
         MVC   AIO,AIO1             RESTORE AIO                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,CHKSRC                                                        
*                                                                               
         XC    SVSTANO,SVSTANO                                                  
         CLI   SVSTA,X'00'                                                      
         BNE   FR75                                                             
         MVC   SVSTA(100),CMSSTA                                                
*                                                                               
FR75     BAS   RE,FILLREC          CHECK TO SEE IF RECORD IN SYNC               
         BAS   RE,GETPUTS          GET THE PUTS AND FORMAT THEM                 
         BAS   RE,GETDEMS          GET THE RATING/SHARE VALUES                  
         BAS   RE,DEMUP            DO THE UPGRADES                              
         J     XIT                                                              
*-FIND RATING MARKING AND ANY OVERRIDE MARKET SOURCES                           
FR80     LA    R9,4                SET BOOK CHECK LOOP                          
         LA    R8,SVBKS                                                         
*                                                                               
FR80A    XC    DBLOCK,DBLOCK       FIND RMKT AND ACTSRC                         
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO3                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
******   MVC   DBSELMED,QMED                                                    
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   *+8                 NO                                           
******   MVI   DBSELMED,C'C'       CHANGE MEDIA TO CANADIEN                     
******   MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELUMK,BMKT                                                    
         MVC   DBSELDAY(5),DAYTIME                                              
         MVC   DBSELSTA,QSTA                                                    
         L     RE,ATWA                                                          
         MVC   DBSELAGY,14(RE)                                                  
         GOTO1 CLUNPK,DMCB,SCHCLI,DBSELCLI                                      
         MVC   DBSELBK,0(R8)                                                    
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         OC    DBACTRMK,DBACTRMK   SEE IF MARKET FOUND                          
         BNZ   FR85                                                             
         LA    R8,2(R8)                                                         
         OC    0(2,R8),0(R8)                                                    
         BZ    NODATA                                                           
         BCT   R9,FR80A                                                         
         B     NODATA                                                           
*                                                                               
FR85     MVC   HLDBOOK,0(R8)                                                    
         MVC   MKTRS,DBACTRMK                                                   
         MVC   SRCSRC(1),DBACTSRC                                               
         MVC   DEMOSRC,SRCSRC                                                   
         MVI   SRCSRCH+5,X'01'                                                  
         DROP  R2                                                               
         LA    R2,SRCSRCH                                                       
         GOTO1 VALISRC                                                          
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
*                                                                               
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         CLI   SVSELMED,C'C'                                                    
         BNE   FR90                NO                                           
         CLI   SRCSRC,C'A'         TEST ARBITRON                                
         BNE   *+18                NO                                           
         MVC   SRCSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
         MVI   DEMOSRC,C'B'                                                     
         B     FR90                                                             
         MVC   SRCSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
         MVI   DEMOSRC,C'C'                                                     
*                                                                               
FR90     L     RE,AIO1             CLEAR THE WORK AREA                          
         LA    RF,2000                                                          
         XCEF                                                                   
         MVC   CMPKEY(13),KEYSAVE                                               
         MVC   CMSCODE(2),=X'0166'                                              
         CLI   SVSTA,X'00'         ARE STATIONS ALREADY SET UP                  
         BE    FR95                NO CREATE STATION LIST                       
         MVC   CMSSTA(100),SVSTA                                                
         B     FR100                                                            
*                                                                               
*                                                                               
*        XC    DBLOCK,DBLOCK       FIND STATIONS IN THE MARKET                  
*        MVI   DBFUNCT,DBVLST                                                   
*        MVC   DBFILE,=C'TP '                                                   
*        MVC   DBSELSRC,BKVALSRC                                                
*        MVC   DBSELBK,SVBKS                                                    
*        MVC   DBSELMED,QMED                                                    
*        MVC   DBSELSTA,QSTA                                                    
*        MVC   DBAREC,AIO3                                                      
*        MVC   DBCOMFCS,ACOMFACS                                                
*        GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
*        MVC   MKTRS,DBACTRMK                                                   
*                                                                               
FR95     XC    DBLOCK,DBLOCK       FIND STATIONS IN THE MARKET                  
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,SVSELMED                                                
         MVC   DBSELSRC,SVSELSRC                                                
******   MVC   DBSELMED,QMED                                                    
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   *+8                 NO                                           
******   MVI   DBSELMED,C'C'       CHANGE MEDIA TO CANADIEN                     
******   MVC   DBSELSRC,BKVALSRC                                                
         MVC   DBSELRMK,MKTRS                                                   
         MVC   DBSELBK,HLDBOOK                                                  
         MVC   SVSTA(5),QSTA                                                    
         MVI   SVSTANO,1                                                        
         MVI   STASW,X'00'                                                      
         GOTO1 VDEMAND,DMCB,DBLOCK,STAHOOK,0                                    
         MVC   CMSSTA(100),SVSTA                                                
*                                                                               
         CLI   STASW,X'FF'                                                      
         BNE   NODATA                                                           
*                                                                               
FR100    BAS   RE,GETPUTS          GET THE PUTS AND FORMAT THEM                 
         BAS   RE,GETDEMS          GET THE RATING/SHARE VALUES                  
         BAS   RE,GETPROGS         GET DEFAULT PROGRAM NAMES                    
         BAS   RE,DEMUP            DO THE UPGRADES                              
*                                                                               
FRX      J     XIT                                                              
         EJECT                                                                  
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
*                                                                               
STAHOOK  L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   SHX                 YES - IGNORE                                 
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    SHX                 YES - IGNORE                                 
         CLC   QSTA,MLSTAT         TEST STATION = REQUEST STATION               
         BNE   STH050                                                           
         MVI   STASW,X'FF'         SET FOUND SWITCH                             
         B     SHX                 YES - IGNORE (ALREADY FIRST IN LIST)         
STH050   CLI   SVSTANO,NMAXSTA                                                  
         BL    STH100                                                           
*        DC    H'0'                BLOW IF TOO MANY STATIONS                    
         OI    SVFLAG,SVF2STA      TOO MANY STATIONS TO LIST (>20)              
         B     SHX                                                              
*                                                                               
STH100   ZIC   R1,SVSTANO                                                       
         LR    RF,R1                                                            
         MH    RF,=H'5'                                                         
         LA    RF,SVSTA(RF)                                                     
         MVC   0(5,RF),MLSTAT                                                   
         LA    R1,1(R1)                                                         
         STC   R1,SVSTANO                                                       
SHX      BR    RE                                                               
         EJECT                                                                  
         DROP  R2                                                               
* DISPLAY RECORDS                                                               
*                                                                               
DISREC   NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
*                                                                               
         CLI   TWASTA,X'FF'                                                     
         BE    DR340                                                            
*                                                                               
         XC    STCHG,STCHG                                                      
*                                                                               
*--WRITE COMPETITION RECORD TO TEMP STORAGE                                     
         XC    DMCB,DMCB           SAVE TWA                                     
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE NUMBER                                  
         L     R9,AIO1                                                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'07D0'     LENGTH OF IO AREA                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',,(R9)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(13,R9),COMPKEY    SEE IF COMPETE RECORD LOADED                 
         BE    *+6                                                              
         DS    H'0'                                                             
*                                                                               
DR050    L     R3,AIO1                                                          
         USING COMPETE,R3                                                       
*                                                                               
DR180    TM    SRCDEMH+FVIIND-FVIHDR,FVIVAL    TEST DEMO FIELD CHANGED          
         BO    DR200                                                            
         OI    SRCDEMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,DEMO                         INSPECT DEMO FIELD               
*                                                                               
DR200    TM    SRCBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DR210                                                            
         TM    SRCBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DR210                                                            
         TM    SRCBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DR210                                                            
         TM    SRCBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BO    DR220                                                            
*                                                                               
DR210    OI    SRCBK1H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCBK2H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCBK3H+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
*                                                                               
*--LOOP FOR OUTPUT CHECK                                                        
*                                                                               
DR220    CLC   0(5,RE),=5X'00'                                                  
         BNE   DR225                                                            
         J     XIT                                                              
*                                                                               
DR225    BAS   RE,GETELADD                                                      
         MVC   DMCB(4),STAADDR                                                  
         GOTO1 GTDEMO,DMCB                                                      
         GOTO1 GTPROG,DMCB                                                      
         LA    R2,SRCPR1H                                                       
         XC    LOOPCT,LOOPCT                                                    
*                                                                               
DR230    TM    4(R2),FVIVAL                  INSPECT PROGRAM NAME FLD           
         BO    DR250                                                            
         OI    4(R2),FVIVAL                                                     
*                                                                               
         ICM   R4,15,PRGADDR       ADDRESS OF PROGRAM ELEMENT                   
         USING CMPEL,R4                                                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   CMPPROG(12),8(R2)              TEST PROGRAM CHANGE               
         BE    DR250                                                            
         OI    CMPSEQ,X'80'                                                     
         MVC   CMPPROG(17),=17X'40'                                             
         MVC   CMPPROG(12),8(R2)                  YES -                         
         L     RE,PJPADDR                                                       
         MVC   0(12,RE),8(R2)                                                   
         OI    LCHG,LPRG                                                        
         DROP  R4                                                               
*                                                                               
*--BUMP R2 TO RATING/SHARE HEADLINE FIELD                                       
DR250    SR    R0,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
*                                                                               
         TM    4(R2),FVIVAL        INSPECT RTG/SHR FIELD                        
         BO    DR330                                                            
         OI    4(R2),FVIVAL                                                     
         CLI   5(R2),0             VALIDATE RTG/SHR FIELD                       
         BE    DRX                                                              
         MVC   FVILEN,5(R2)        VALIDATE RTG/SHR FIELD                       
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(10),8(R2)                                                 
         TM    LCHG,LDEMO          TEST DEMO CHANGE                             
         BO    DR330               YES - IGNORE RTG/SHR CHANGE                  
         XC    ELEM,ELEM                                                        
         LA    RF,C'/'                                                          
         STC   RF,ELEM(RF)                                                      
         LR    RE,R2               STORE REGISTER 2                             
         TRT   FVIFLD(10),ELEM      SEARCH FOR / CHARACTER                      
         BZ    INVINPT                                                          
         LR    R2,RE               RELOAD REGISTER 2                            
         LR    R9,R1               R1 = A(C'/')                                 
         SR    RE,RE               LOCATE RATING FIELD                          
         BCTR  RE,0                                                             
         LA    RF,FVIFLD                                                        
         BCTR  RF,0                                                             
         XC    FULL,FULL                                                        
*                                                                               
DR260    BXH   R9,RE,*+18                                                       
         OC    FULL,FULL                                                        
         BZ    INVINPT                                                          
         B     DR280                                                            
         CLI   0(R9),C' '                                                       
         BNH   DR270                                                            
         CLI   0(R9),C'*'          REMOVE * IF ANY                              
         BNE   *+8                                                              
         MVI   0(R9),C' '                                                       
         OC    FULL,FULL                                                        
         BNZ   DR260                                                            
         ST    R9,FULL                                                          
         B     DR260                                                            
DR270    OC    FULL,FULL                                                        
         BZ    DR260                                                            
*                                                                               
DR280    L     RF,FULL                                                          
         SR    RF,R9                                                            
         ST    RF,DMCB+4                                                        
         LA    R9,1(R9)                                                         
         ST    R1,FULL             FULL = A(C'/')                               
         GOTO1 CASHVAL,DMCB,(1,(R9))        VALIDATE RATING FIELD               
         CLI   DMCB,FF                                                          
         BE    INVINPT                                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    INVINPT                                                          
         L     RE,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         CLC   1(3,RE),DMCB+5      TEST RATING CHANGE                           
         BE    DR290                                                            
         OI    LCHG,LRTG             YES -                                      
         MVC   0(4,RE),DMCB+4      CHANGE SAVED RATING                          
         OI    0(RE),X'80'          INDICATE OVERRIDE                           
*                                                                               
DR290    L     R9,FULL             VALIDATE SHARE FIELD                         
         LA    R9,1(R9)                                                         
         ZIC   RE,FVILEN                                                        
         LA    RE,FVIFLD(RE)                                                    
         SR    RE,R9                                                            
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,(R9))                                            
         CLI   DMCB,FF                                                          
         BE    INVINPT                                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    INVINPT                                                          
         L     RE,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         CLC   4(4,RE),DMCB+4     TEST SHARE CHANGE                             
         BE    DR300                                                            
         OI    LCHG,LSHR              YES -                                     
         MVC   LOLDSHR,4(RE)          (SAVE OLD SHARE)                          
         MVC   4(4,RE),DMCB+4     CHANGE SAVED SHARE                            
*                                                                               
DR300    TM    LCHG,LSHR           TEST FOR SHARE CHANGE                        
         BZ    DR310                                                            
         L     R0,SVPJPUT          YES - CALCULATE NEW RATING                   
         SRDA  R0,32                                                            
         L     RE,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         M     R0,4(RE)                                                         
         LA    R1,500(R1)                                                       
         D     R0,=F'1000'                                                      
         ST    R1,0(RE)                  SAVE NEW RATING                        
         OI    0(RE),X'80'              RATING OVERRIDE BIT                     
         OI    LCHG,LRTG                                                        
         B     DR320                                                            
*                                                                               
DR310    TM    LCHG,LRTG           TEST RATING CHANGE (BUT NO SHR CHG)          
         BZ    DR330                                                            
         L     RE,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         L     R0,0(RE)            YES - CALCULATE NEW SHARE                    
         SLL   R0,1                      ELIMINATE OVERRIDE BIT                 
         SRDL  R0,33                                                            
         M     R0,=F'1000'                                                      
         D     R0,SVPJPUT                                                       
         MVC   LOLDSHR,4(RE)             (SAVE OLD SHARE)                       
         ST    R1,4(RE)                                                         
         OI    LCHG,LSHR                                                        
*                                                                               
DR320    ICM   R4,15,DEMADDR       CHANGE RATING IN COMP RECORD                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         OI    0(R4),X'80'         SET OVERRIDE BIT                             
         MVC   1(2,R4),2(RE)       MOVE RATING TO DEMO ELEMENT                  
*                                                                               
DR330    TM    LCHG,LSHR           TEST SHARE CHANGE                            
         BZ    DR335                                                            
         L     RF,PJRADDR          ADDRESS IN RATING/SHARE TABLE                
         L     RE,4(RF)            YES - CALCULATE NEW PROJ CUM SHARE           
         S     RE,LOLDSHR                                                       
         A     RE,SVPJCUM                                                       
         ST    RE,SVPJCUM                                                       
         BAS   RE,FMTPJCUM         FORMAT PROJ CUM SHARE                        
*                                                                               
DR335    OC    STCHG,LCHG                                                       
         XC    LCHG,LCHG                                                        
*                                                                               
         L     RE,STAADDR                                                       
         LA    RE,5(RE)                                                         
         CLC   0(5,RE),=5X'00'                                                  
         BE    DR340                                                            
         CLI   0(RE),X'FF'                                                      
         BE    DR340                                                            
         ST    RE,STAADDR                                                       
*                                                                               
         SR    R0,R0               BUMP R2 TO PROGRAM FIELD                     
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         AR    R2,R0                                                            
*                                                                               
         ICM   RE,1,LOOPCT         CHECK FOR END OF SCREEN                      
         LA    RE,1(RE)                                                         
         STC   RE,LOOPCT                                                        
         CLI   LOOPCT,7                                                         
         BNL   DR340                                                            
*                                                                               
         MVC   DMCB(4),STAADDR                                                  
         GOTO1 GTDEMO,DMCB                                                      
         GOTO1 GTPROG,DMCB                                                      
*                                                                               
         ICM   RE,15,PJPADDR                                                    
         LA    RE,12(RE)                                                        
         ST    RE,PJPADDR                                                       
*                                                                               
         ICM   RE,15,PJRADDR                                                    
         LA    RE,8(RE)                                                         
         ST    RE,PJRADDR                                                       
         B     DR230                                                            
*                                                                               
         DROP  R8                                                               
         DROP  R3                                                               
*                                                                               
DR340    MVC   LCHG,STCHG                                                       
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         CLI   TWASTA,FF           SKIP TO DISPLAY FOR VERY FIRST SCRN          
         BE    DR350                                                            
         TM    LCHG,LDEMO+LBK      TEST DEMO OR BOOK CHANGES                    
         BZ    DR350                                                            
         BAS   RE,GETPUTS          YES - GET PUTS                               
         BAS   RE,GETDEMS                GET RATINGS AND SHARES                 
         TM    LCHG,LDEMO          TEST DEMO CHANGE                             
         BZ    DR350                                                            
         BAS   RE,DEMUP            YES - DO THE UPGRADES                        
*                                                                               
DR350    CLI   TWASTA,FF           TEST FOR VERY FIRST SCREEN                   
         BNE   *+12                                                             
         MVI   TWASTA,0                                                         
         B     DR360                                                            
         TM    LCHG,LDEMO+LBK+LRTG+LPRG  TEST DEMO, BK RTP PRG CHANGES          
         BNZ   DR360                     YES- RE-DISPLAY CURRENT SCREEN         
         ZIC   RE,TWASTA                 NO   - DISPLAY NEXT SCREEN             
         LA    RF,NSTASCR                                                       
         AR    RE,RF                                                            
         STC   RE,TWASTA                                                        
         CLI   MORESW,X'FF'        TEST ALL SCREENS DISPLAYED                   
         BE    DR360                                                            
         MVI   TWASTA,0            YES - DISPLAY FROM FIRST SCREEN              
*                                                                               
DR360    XC    LCHG,LCHG                                                        
         MVI   MORESW,X'00'                                                     
*                                                                               
         SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,SRCST1H                                                       
         LA    R8,SRCCUSH                                                       
DR365    LA    R9,6                                                             
DR370    IC    RE,0(R4)                                                         
         TM    1(R4),X'02'                                                      
         BZ    DR373                                                            
         SH    RE,=H'8'            EXTENDED HEADER                              
DR373    SH    RE,=H'9'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,DROC                                                          
         BZ    DR375                                                            
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
DR375    TM    1(R4),X'02'                                                      
         BZ    DR378                                                            
         LA    R4,17(RE,R4)                                                     
         B     DR380                                                            
DR378    LA    R4,9(RE,R4)                                                      
DR380    BCT   R9,DR370                                                         
         CR    R4,R8                                                            
         BL    DR365                                                            
*                                                                               
         LA    R2,SRCPR1H                                                       
         LA    R0,7                                                             
DR385    OI    1(R2),FVAHIGH    SHOW HIGH INTENSITY                             
         LA    R2,SRCLN5H-SRCLN3H(R2)                                           
         BCT   R0,DR385                                                         
*                                                                               
         LA    R2,SRCRS2H                                                       
         LA    R0,7                                                             
DR388    OI    1(R2),FVAHIGH    SHOW HIGH INTENSITY                             
         LA    R2,SRCLN5H-SRCLN3H(R2)                                           
         BCT   R0,DR388                                                         
*                                                                               
         LA    R0,NSTASCR          FORMAT STATIONS                              
         ZIC   RE,TWASTA                                                        
         ZIC   RF,SVSTANO                                                       
         SR    RF,RE                                                            
         CR    R0,RF               TEST FOR A FULL SCREEN                       
         BNH   *+6                                                              
         LR    R0,RF               NO                                           
         LA    R0,7                NUMBER OF STATIONS ON THIS SCREEN            
         MH    RE,=H'5'                                                         
         LA    RE,SVSTA(RE)                                                     
         LA    R4,SRCST1H                                                       
         LA    R8,SRCST1                                                        
         LA    R9,SRCLN2H                                                       
*                                                                               
DR390    OC    0(5,RE),0(RE)                                                    
         BZ    DR420                                                            
         CLI   0(RE),X'FF'                                                      
         BE    DR420                                                            
         MVC   0(4,R8),0(RE)                                                    
         MVC   4(3,R8),=C'-TV'                                                  
         CLI   4(RE),C'T'                                                       
         BE    *+14                                                             
         MVC   5(1,R8),4(RE)                                                    
         MVI   6(R8),C'M'                                                       
         CLI   3(RE),C' '                                                       
         BNE   *+14                                                             
         MVC   3(3,R8),4(R8)                                                    
         MVI   6(R8),C' '                                                       
         OI    6(R4),FVOXMT        TRANSMIT BOTH STATION LINES                  
         OI    6(R9),FVOXMT                                                     
         CLM   R0,1,LNSTASCR                                                    
         BNE   DR400                                                            
         LA    R4,SRCST3H                                                       
         LA    R8,SRCST3                                                        
         LA    R9,SRCLN4H                                                       
         B     DR410                                                            
DR400    LA    R4,SRCST5H-SRCST3H(R4)                                           
         LA    R8,SRCST5H-SRCST3H(R8)                                           
         LA    R9,SRCST5H-SRCST3H(R9)                                           
DR410    LA    RE,5(RE)                                                         
         BCT   R0,DR390                                                         
DR420    CLI   0(RE),X'FF'                                                      
         BE    DR425                                                            
         CLC   0(5,RE),=5X'00'                                                  
         BE    DR425                                                            
         MVI   MORESW,X'FF'        MORE STATIONS TO DISPLAY                     
*                                                                               
DR425    ZIC   R4,TWASTA           FORMAT PROGRAM NAMES                         
         MH    R4,=Y(4*L'L1PROG1)                                               
         LA    R4,SVPROGS(R4)                                                   
         ZIC   R8,TWASTA                                                        
         MH    R8,=Y(L'L1PJPROG)                                                
         LA    R2,SRCLN1H                                                       
         LA    R8,SVPJPRGS(R8)                                                  
         LA    R9,SRCLN1                                                        
         USING LINE1D,R9                                                        
         LA    RF,7                                                             
*                                                                               
DR430    LA    RE,L1PROG1                                                       
         LA    R0,4                                                             
         CLI   SVUSEDDS,C'N'       IF EST/COMP USER THEN 4 BOOKS                
         BE    *+8                                                              
         LA    R0,2                                                             
*                                                                               
DR440    MVC   0(L'L1PROG1,RE),0(R4)                                            
         LA    R4,L'L1PROG1(R4)                                                 
         LA    RE,L1PROG2-L1PROG1(RE)                                           
         BCT   R0,DR440            DO FOR ALL BOOKS                             
         OI    6(R2),FVOXMT                                                     
         CLI   SVUSEDDS,C'N'       IF EST/COMP USER THEN 4 BOOKS                
         BE    *+12                                                             
         LA    R4,L'L1PROG1(R4)                                                 
         LA    R4,L'L1PROG1(R4)                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         MVC   8(L'L1PJPROG,R2),0(R8)                                           
         OI    6(R2),FVOXMT                                                     
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         LA    R9,8(R2)                                                         
         LA    R8,L'L1PJPROG(R8)                                                
         BCT   RF,DR430            DO FOR ALL STATIONS                          
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT RATINGS AND SHARES                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         ZIC   R4,TWASTA                                                        
         SLL   R4,5                                                             
         LA    R4,SVDEMVAL(R4)     R4 = A(RTG/SHR)                              
         ZIC   R9,TWASTA                                                        
         SLL   R9,3                                                             
         LA    R9,SVPROJ(R9)       R9 = A(PROJ RTG/SHR)                         
         LA    RF,7                                                             
*                                                                               
DR460    LA    R2,SRCLN2H                                                       
         LA    R1,8(R2)                                                         
         USING LINE2D,R1                                                        
*                                                                               
DR470    LA    R8,L2RS1                                                         
         LA    R0,4                                                             
         CLI   SVUSEDDS,C'N'       IF EST/COMP USER THEN 4 BOOKS                
         BE    *+8                                                              
         LA    R0,2                                                             
*                                                                               
DR480    XC    0(L'L2RS1,R8),0(R8)                                              
         LA    R8,2(R8)                                                         
         BAS   RE,FMTRS            FORMAT RATING/SHARE ROUTINE                  
         LA    R8,L2RS2-L2RS1-2(R8)                                             
         LA    R4,8(R4)                                                         
         BCT   R0,DR480            DO FOR ALL BOOKS                             
         CLI   SVUSEDDS,C'N'       IF EST/COMP USER THEN 4 BOOKS                
         BE    *+8                                                              
         LA    R4,16(R4)                                                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         XC    8(12,R2),8(R2)                                                   
         OI    6(R2),FVOXMT                                                     
         OI    4(R2),FVIVAL                                                     
         LA    R8,8(R2)                                                         
*                                                                               
         ST    R4,FULL                                                          
         LR    R4,R9                                                            
         BAS   RE,FMTRS            FORMAT PROJ RTG/SHR                          
         OC    0(12,R8),=12X'40'                                                
         L     R4,FULL                                                          
         LA    R9,8(R9)                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         CLC   8(4,R2),=4X'40'     ANY MORE STATIONS                            
         BNH   DR490                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         OI    6(R2),FVOXMT                                                     
         LA    R1,8(R2)                                                         
         BCT   RF,DR470            DO FOR ALL STATIONS                          
*                                                                               
*--WRITE COMPETITION RECORD TO TEMP STORAGE                                     
DR490    XC    DMCB,DMCB           SAVE TWA                                     
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE NUMBER                                  
         L     R9,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',,(R9)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NDETSW,X'FF'                                                     
         BE    DRX                                                              
         MVC   AIO,AIO1                                                         
         CLI   SVSTANO,X'00'                                                    
         BE    DR500                                                            
         GOTO1 ADDREC                       YES - ADD THE RECORD                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DRX                                                              
*                                                                               
DR500    MVC   KEY(13),COMPKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST COMPETITION EXISTS                     
         BE    *+6                                                              
         DS    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                       YES - PUT THE RECORD                
*-- MAKE SURE THE RECORDS IS IN SYNC                                            
         L     RE,AIO1                                                          
         L     RF,AIO3                                                          
         CLC   0(13,RE),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                       YES - PUT THE RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DRX      XC    SVSTANO,SVSTANO                                                  
*                                                                               
         LA    R2,SRCSTAH                                                       
         TM    SVFLAG,SVF2STA      DO WE SHOW THE SPECIAL MESSAGE?              
         BNZ   FIRST20                                                          
         J     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
* ROUTINE TO FORMAT RATING/SHARE                                                
* INPUT  : R4 = A(RTG/SHR)                                                      
*          R8 = A(FORMAT AREA)                                                  
*                                                                               
FMTRS    NTR1  ,                                                                
         MVC   DUB(4),0(R4)        DUB(4) = RATING                              
         NI    DUB,FF-X'80'                                                     
         LA    RE,5                RE = LENGTH OF FORMATTED RATING FLD          
         MVI   BYTE,4              BYTE = LENGTH OF FORMATTED SHR FLD           
         CLC   4(4,R4),=F'100'                                                  
         BNL   *+12                                                             
         MVI   BYTE,3                                                           
         LA    R8,1(R8)                                                         
         CLC   DUB(4),=F'100000'                                                
         BNL   FS4                                                              
         CLC   DUB(4),=F'10000'                                                 
         BNL   FS2                                                              
         CLC   DUB(4),=F'1000'                                                  
         BNL   FS4                                                              
FS2      LA    R8,1(R8)                                                         
         BCTR  RE,0                                                             
         CLC   DUB(4),=F'100'                                                   
         BNL   FS4                                                              
         LA    R8,1(R8)                                                         
         BCTR  RE,0                                                             
FS4      STC   RE,EBLOUT           FORMAT THE RATING                            
         OC    DUB(4),DUB                                                       
         BNZ   *+14                                                             
         MVC   0(3,R8),=C'0.0'                                                  
         B     FS6                                                              
         MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'         TEST OVERRIDE                                
         BZ    *+14                                                             
         MVI   EBFLOAT,C'*'                                                     
         LA    RE,1(RE)                                                         
         BCTR  R8,0                                                             
         ST    R8,EBAOUT                                                        
         STC   RE,EBLOUT                                                        
         LA    R1,DUB                                                           
         ST    R1,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLC   DUB(4),=F'10000'                                                 
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
FS6      ZIC   RE,EBLOUT           FORMAT THE SHARE                             
         AR    R8,RE                                                            
         MVI   0(R8),C'/'                                                       
         LA    R8,1(R8)                                                         
         LA    R4,4(R4)                                                         
         ST    R4,EBAIN                                                         
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R8),=C'0.0'                                                  
         B     FSX                                                              
         ST    R8,EBAOUT                                                        
         MVC   EBLOUT,BYTE                                                      
         MVI   EBFLOAT,0                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
FSX      J     XIT                                                              
         EJECT                                                                  
* INSPECT THE DEMO FIELD                                                        
* OUTPUT : LCHG = LDEMO IF THE DEMO CHANGES                                     
*          CC EQ OK                                                             
*             NE ERROR                                                          
*                                                                               
DEMO     NTR1                                                                   
         LA    R2,SAVED                                                         
         USING SAVED,R2                                                         
         CLI   SRCDEMH+5,0                                                      
         BNE   DE10                                                             
         MVI   SRCDEMH+5,7                                                      
         MVI   SRCDEMH+7,X'80'                                                  
*                                                                               
*--READ DEMO MENU RECORD                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D26'                                                
         MVC   KEY+2(1),SAVEKEY+1                                               
         MVC   KEY+3(4),DEMMENU                                                 
*                                                                               
         MVC   AIO,AIO3            RESTORE AIO                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST DEMO MENU EXISTS                        
         BNE   INVDMN                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,X'05'        LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   INVDMN                                                           
*                                                                               
         MVC   SRCDEM(7),5(R4)                                                  
         OI    SRCDEMH+FVIIND-FVIHDR,FVIVAL                                     
*                                                                               
DE10     XC    DMCB(24),DMCB                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+8                 NO                                           
         MVI   DBSELMED,C'C'       CHANGE MEDIA TO CANADIEN                     
         PRINT GEN                                                              
         GOTO1 VDEMOVAL,DMCB,(1,SRCDEMH),(1,DUB),(C'S',DBLOCK)                  
         PRINT NOGEN                                                            
         CLI   0(R1),0                                                          
         BE    DE15                                                             
         B     INVDMO                                                           
DE15     CLC   SVDEM,DUB           TEST DEMO HAS CHANGED                        
         BE    DEX                                                              
         MVC   SVDEM,DUB           YES                                          
         OI    LCHG,LDEMO                                                       
*                                                                               
DE20     MVC   SVPUT,SVDEM         DETERMINE PUT AND SHARE DEMO CODES           
         MVC   SVRTGSHR(3),SVDEM                                                
         MVC   SVRTGSHR+3(4),SVDEM                                              
         MVI   SVPUT+1,C'P'                                                     
         MVI   SVRTGSHR+4,C'S'                                                  
         CLI   SVDEM+1,C'I'                                                     
         BNE   DEX                                                              
         MVI   SVPUT+1,C'Q'                                                     
         MVI   SVRTGSHR+4,C'X'                                                  
*                                                                               
DEX      J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* INSPECT BOOK FIELDS                                                           
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                                     
*          CC EQ  - OK                                                          
*             NE  - ERROR                                                       
*                                                                               
BOOKS    NTR1                                                                   
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         LA    R4,SRCBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R0,4                                                             
*                                                                               
BK10     LR    R1,R4               VALIDATE BOOK FIELD                          
         CLI   5(R4),0                                                          
         BNE   BK15                                                             
         OC    0(2,R8),0(R8)       MISSING                                      
         BZ    BK20                                                             
         XC    0(2,R8),0(R8)                                                    
         OI    LCHG,LBK                                                         
         B     BK20                                                             
*                                                                               
BK15     LA    R9,8(R4)                                                         
         GOTO1 DATVAL,DMCB,(2,(R9)),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BK90                INVALID DATE                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         CLC   0(2,R8),WORK+6      COMPARE TO OLD VALUE                         
         BE    BK20                                                             
         MVC   0(2,R8),WORK+6      SAVE BOOK YR/MN                              
         OI    LCHG,LBK                                                         
*                                                                               
BK20     LA    R4,SRCBK2H-SRCBK1H(R4)   NEXT BOOK                               
         LA    R8,2(R8)                                                         
         BCT   R0,BK10             DO FOR ALL BOOKS                             
         B     BKX                                                              
*                                                                               
BK90     LR    R2,R4                                                            
         B     INVBOOK             INVALID BOOK                                 
*                                                                               
BKX      J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* GET THE PUT VALUES AND FORMAT TO HEADLINE                                     
*                                                                               
GETPUTS  NTR1                                                                   
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         XC    EBLOCK,EBLOCK       PREPARE EDITOR BLOCK                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'SRCPT1                                                  
         XC    SPDEMLK,SPDEMLK     LOOK UP PUT VALUES FOR EACH BOOK             
         LA    RE,DEMLKSV                                                       
         ST    RE,SPLKXTND                                                      
         L     RE,AIO3                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAGY,AGENCY                                                   
*****    MVC   SPLKMED,QMED                                                     
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   SPLKMED,C'C'        CHANGE MEDIA TO CANADIEN                     
*****    MVC   SPLKSRC,BKVALSRC                                                 
         LA    RE,DEMLKSV                                                       
         ST    RE,SPLKXTND                                                      
         MVC   SPLKMED,SVSELMED                                                 
         MVC   SPLKSRC,SVSELSRC                                                 
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSTA,QSTA                                                     
         MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKDAY(5),DAYTIME                                               
         MVI   SPLKSVI,X'FF'                                                    
         LA    R1,SVPUT                                                         
         ST    R1,SPLKALST                                                      
         LA    R1,WORK                                                          
         ST    R1,SPLKAVAL                                                      
         LA    R0,4                                                             
         LA    R2,SRCPT1H                                                       
         LA    R4,SVPUTVAL                                                      
         LA    R8,SVBKS                                                         
         XC    SVPUTVAL,SVPUTVAL                                                
*                                                                               
GP10     XC    8(L'SRCPT1,R2),8(R2)                                             
         OI    6(R2),FVOXMT                                                     
         OC    0(2,R8),0(R8)                                                    
         BZ    GP20                                                             
         MVC   SPLKDBK,0(R8)                                                    
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)     CALL SPGETDEM                  
         MVC   0(4,R4),WORK        PUT VALUE RETURNED                           
         ST    R4,EBAIN            FORMAT IT                                    
         LA    R1,8(R2)                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLC   0(4,R4),=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
*                                                                               
GP20     LA    R2,SRCPT2H-SRCPT1H(R2)   NEXT BOOK                               
         LA    R4,4(R4)                                                         
         LA    R8,2(R8)                                                         
         BCT   R0,GP10                                                          
*                                                                               
         MVC   SPLKDBK,SVUPGRD+2              CALL SPGETDEM FOR                 
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)       PROJECTED PUT                
         MVC   SVPJPUT,WORK                                                     
         LA    R1,SVPJPUT          FORMAT PROJECTED PUT                         
         ST    R1,EBAIN                                                         
         LA    R1,SRCPPT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLC   SVPJPUT,=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         OI    SRCPPTH+6,FVOXMT                                                 
*                                                                               
GPX      J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO GET THE RATING SOURCE                                              
         EJECT                                                                  
* ROUTINE TO GET THE RATING AND SHARE VALUES                                    
*            AND FORMAT THE CUM SHARES                                          
*                                                                               
GETDEMS  NTR1                                                                   
         XC    SPDEMLK,SPDEMLK                                                  
         L     RE,AIO3                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAGY,AGENCY                                                   
******   MVC   SPLKMED,QMED                                                     
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   *+8                 NO                                           
******   MVI   SPLKMED,C'C'        CHANGE MEDIA TO CANADIEN                     
******   MVC   SPLKSRC,BKVALSRC                                                 
         LA    R1,SAVED                                                         
         USING SAVED,R1                                                         
         MVC   SPLKMED,SVSELMED                                                 
         MVC   SPLKSRC,SVSELSRC                                                 
         DROP  R1                                                               
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKDAY(5),DAYTIME                                               
         MVI   SPLKSVI,X'FF'                                                    
         LA    R1,SAVED                                                         
         LA    R1,1058(R1)         SVRTGSHR                                     
         ST    R1,SPLKALST                                                      
         LA    R1,WORK                                                          
         ST    R1,SPLKAVAL                                                      
         LA    R4,SAVED            R4 = A(STATION LIST)                         
         LA    R4,949(R4)          SVSTA                                        
         LA    R2,SAVED            R2 = A(PROGRAM NAME AREA)                    
         LA    R2,1308(R2)         SVPROGS                                      
         LA    R9,SAVED            R9 = A(DEMO VALUE AREA)                      
         LA    R9,40(R9)           SVDEMVAL                                     
         LR    RE,R2               CLEAR PROGRAM NAME AREA                      
         LA    RF,960              SVPROGSL                                     
         XCEF                                                                   
         LR    RE,R9               CLEAR DEMO VALUE AREA                        
         LA    RF,640              SVDEMVLL                                     
         XCEF                                                                   
         XC    SAVED(16),SAVED     CLEAR CUM SHARE VALUE AREA                   
*                                                                               
GD10     MVC   SPLKSTA,0(R4)                                                    
         LA    R0,4                                                             
         LA    R3,SAVED            R3 = A(CUM SHARE VALUE AREA)                 
         LA    R8,SAVED            R8 = A(BOOK LIST)                            
         LA    R8,941(R8)          SVBKS                                        
*                                                                               
GD20     OC    0(2,R8),0(R8)                                                    
         BZ    GD30                                                             
         MVC   SPLKDBK,0(R8)                                                    
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)     CALL SPGETDEM                  
*                                                                               
         MVC   0(L'L1PROG1,R2),SPLKPRG    PROGRAM NAME                          
         MVC   0(4,R9),WORK        RATING                                       
         MVC   4(4,R9),WORK+8      SHARE                                        
         L     R1,0(R3)            ACCUMULATE TOTAL BOOK SHARE                  
         A     R1,WORK+8                                                        
         ST    R1,0(R3)                                                         
*                                                                               
GD30     LA    R3,4(R3)            NEXT BOOK                                    
         LA    R2,L'L1PROG1(R2)                                                 
         LA    R8,2(R8)                                                         
         LA    R9,8(R9)                                                         
         BCT   R0,GD20                                                          
*                                                                               
         LA    R4,5(R4)            NEXT STATION                                 
         CLC   0(5,R4),=5X'00'                                                  
         BE    GD35                                                             
         CLI   0(R4),X'FF'                                                      
         BE    GD35                                                             
         B     GD10                                                             
*                                                                               
GD35     XC    EBLOCK,EBLOCK       FORMAT CUM SHARES                            
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUM1                                                    
         MVI   EBDECS,1                                                         
         LA    R0,4                                                             
         LA    R3,SAVED            SVCUMS                                       
         LA    R4,SRCCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R8,CUM1                                                          
*                                                                               
GD40     ST    R3,EBAIN                                                         
         ST    R8,EBAOUT                                                        
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         LA    R3,4(R3)                                                         
         LA    R8,CUM2-CUM1(R8)                                                 
         BCT   R0,GD40                                                          
         OI    SRCCUMH+6,FVOXMT                                                 
*                                                                               
GDX      J     XIT                                                              
         EJECT                                                                  
* DEMO UPGRADE ROUTINE                                                          
*                                                                               
DEMUP    NTR1                                                                   
         L     R3,AIO1                                                          
         USING COMPETE,R3                                                       
*                                                                               
         LA    R8,SAVED            R8 = A(STATION LIST)                         
         LA    R8,949(R8)          SVSTA                                        
         LA    R2,SAVED            R2 = A(PROGRAM NAMES)                        
         LA    R2,1068(R2)         SVPJPRGS                                     
         LA    R9,SAVED            R9 = A(DEMO VALUE AREA)                      
         LA    R9,680(R9)          SVPROJ                                       
         LR    RE,R9               CLEAR DEMO VALUE AREA                        
         LA    RF,SVPROJL                                                       
         XCEF                                                                   
         LA    RE,SAVED                                                         
         XC    16(4,RE),16(RE)     CLEAR CUM SHARES SVPJCUM                     
*                                                                               
         XC    DEMADDR,DEMADDR                                                  
         XC    PRGADDR,PRGADDR                                                  
         XC    LDEMOVR,LDEMOVR     BUILD DEMO OVERRIDE ELEMENT                  
         LA    RE,SAVED                                                         
         CLI   1065(RE),0      IS THERE A COMPETITION RECORD SVSTANO            
         BNE   DM80                NO                                           
*                                                                               
         XC    NODEMSW,NODEMSW                                                  
         ST    R8,DMCB                                                          
         GOTO1 GTPROG,DMCB      GET PROGRAM ADDRESS                             
*                                                                               
DM20     L     R4,AIO1                                                          
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BE    DM40                                                             
         MVI   NODEMSW,1                                                        
         B     DM80                                                             
*                                                                               
         USING CMDEL,R4                                                         
DM40     LA    RE,SAVED                                                         
         CLC   CMDTYP(2),1051(RE)   FIND DEMO IN DEMO ELEMENT SVDEM+1           
         BE    DM50                                                             
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DM40                                                             
         MVI   NODEMSW,1                                                        
         B     DM80                                                             
*                                                                               
DM50     ST    R4,DEMELEM                                                       
         ST    R8,DMCB                                                          
         GOTO1 GTDEMO,DMCB                                                      
         CLC   DEMADDR,=4X'00'        YES - TEST FOR OVERRIDE                   
         BE    DM80                                                             
*                                                                               
         MVC   DEMOTYP,CMDTYP                                                   
*                                                                               
DM60     XC    LDEMOVR,LDEMOVR                                                  
         ICM   RE,15,DEMADDR                                                    
         TM    0(RE),X'80'         IS THIS AN OVERRIDDEN DEMO                   
         BZ    DM80                                                             
         CLC   1(2,RE),=XL2'FFFF'  ELEMENT HAVE DEFAULT VALUE                   
         BE    DM80                                                             
*                                                                               
         MVI   LDEMOVR,OVERELEM                                                 
         MVI   LDEMOVR+1,6                                                      
         MVC   LDEMOVR+2(2),DEMOTYP                                             
         MVC   LDEMOVR+4(2),1(RE)                                               
         B     DM80                                                             
         DROP  R4                                                               
*                                                                               
DM80     LA    RE,SAVED                                                         
         MVI   1058(RE),0    SVRTGSHR    REMOVE OVERRIDE INDICATORS             
         MVI   1061(RE),0    SVRTGSHR+3                                         
*                                                                               
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,AIO3                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
*****    MVC   SPUPMED,QMED                                                     
*****    CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
*****    BNE   *+8                 NO                                           
*****    MVI   SPUPMED,C'C'        CHANGE MEDIA TO CANADIEN                     
******   MVC   SPUPSRC,BKVALSRC                                                 
         LA    R1,SAVED                                                         
         USING SAVED,R1                                                         
         MVC   SPUPMED,SVSELMED                                                 
         MVC   SPUPSRC,SVSELSRC                                                 
         DROP  R1                                                               
         MVC   SPUPDAY(5),DAYTIME                                               
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPFBK,SVUPFRBK                                                 
         LA    R1,LDEMOVR                                                       
         ST    R1,SPUPAOVR                                                      
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
*                                                                               
DM100    CLC   0(5,R8),=5X'00'     SEE IF ANY MORE STATIONS LEFT                
         BE    DM200                                                            
         CLI   0(R8),X'FF'                                                      
         BE    DM200                                                            
*                                                                               
         MVC   SPUPSTA,0(R8)                                                    
         LA    RE,SAVED                                                         
         MVC   DUB(7),1058(RE)                                                  
         GOTO1 VSPDEMUP,DMCB,LDMUPBLK,DUB,(R9)    SVRTGSHR                      
         LA    RE,SAVED                                                         
         MVC   1058(7,RE),DUB                                                   
*                                                                               
         CLC   0(L'L1PJPROG,R2),=12X'00'   PROGRAM NAME                         
         BNE   DM103                                                            
         MVC   0(L'L1PJPROG,R2),SPUPPRG    PROGRAM NAME                         
DM103    CLC   PRGADDR,=4X'00'     TEST IF PROGRAM ELEMENT EXISTS               
         BZ    DM110                                                            
*                                                                               
         ICM   RE,15,PRGADDR                                                    
         CLC   3(12,RE),=12X'FF'                                                
         BNE   DM105                                                            
         MVC   3(16,RE),SPUPPRG                                                 
DM105    MVC   0(L'L1PJPROG,R2),3(RE)    PROGRAM NAME FROM CMP RECORD           
         LA    R4,LDMUPBLK         RESET R4                                     
*                                                                               
DM110    LA    RE,SAVED                                                         
         CLI   1058(RE),OVERELEM    TEST FOR OVERRIDE RATING SVRTGSHR           
         BNE   DM120                                                            
         MVI   1058(RE),0   YES - REMOVE OVERRIDE INDICATORS SVRTGSHR           
         MVI   1061(RE),0   SVRTGSHR+3                                          
         L     R0,0(R9)            CALCULATE THE SHARE                          
         SRDA  R0,32                                                            
         LTR   R1,R1               BYPASS IF RATING IS ZERO                     
         BZ    DM115                                                            
         OC    36(4,RE),36(RE)     BYPASS IF PUT IS ZERO                        
         BZ    DM115                                                            
         M     R0,=F'1000'                                                      
         D     R0,36(RE)           SVPJPUT                                      
DM115    ST    R1,4(R9)            STORE THE OVERRIDE SHARE                     
         OI    0(R9),X'80'         RATING OVERRIDE BIT                          
*                                                                               
DM120    LA    RE,SAVED                                                         
         L     R1,16(RE)           ACCUMULATE PROJ CUM SHARE SVPJCUM            
         A     R1,4(R9)                                                         
         ST    R1,16(RE)           SVPJCUM                                      
*                                                                               
         LA    R2,L'L1PJPROG(R2)   NEXT STATION                                 
         LA    R8,5(R8)                                                         
         ST    R8,DMCB                                                          
         GOTO1 GTPROG,DMCB         GET PROGRAM ADDRESS                          
*--SEE IF DEMO SHOULD BE PLACED IN ELEMENT                                      
         CLC   DEMADDR,=4X'00'                                                  
         BE    DM160                                                            
         ICM   RE,15,DEMADDR                                                    
         CLC   1(2,RE),=XL2'FFFF'                                               
         BNE   DM140                                                            
         MVC   1(2,RE),2(R9)                                                    
*                                                                               
DM140    ST    R8,DMCB                                                          
         GOTO1 GTDEMO,DMCB                                                      
         LA    R9,8(R9)                                                         
         B     DM60                                                             
DM160    LA    R9,8(R9)                                                         
         B     DM100                                                            
*                                                                               
DM200    BAS   RE,BLDCOMP                                                       
         BAS   RE,FMTPJCUM         FORMAT PROJ CUM SHARE                        
*                                                                               
DMX      XC    DEMADDR,DEMADDR                                                  
         XC    PRGADDR,PRGADDR                                                  
         J     XIT                                                              
         EJECT                                                                  
* FORMAT PROJECTED CUM SHARE                                                    
*                                                                               
FMTPJCUM NTR1                                                                   
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUMPROJ                                                 
         MVI   EBDECS,1                                                         
         LA    R1,SVPJCUM                                                       
         ST    R1,EBAIN                                                         
         LA    R4,SRCCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R1,CUMPROJ                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         OI    SRCCUMH+6,FVOXMT                                                 
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
TRAPERR  GOTO1 ERREX                                                            
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         OI    CONHEADH+6,X'80'    XMIT HEADER                                  
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         L     RD,SAVERD           BACK ALL THE WAY OUT                         
         J     XIT                                                              
         EJECT                                                                  
* BLDCOMP BUIILDS THE COMPETITION RECORD                                        
*                                                                               
BLDCOMP  NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
*                                                                               
         CLI   NODEMSW,1                                                        
         BE    BLDCP020                                                         
         CLI   SVSTANO,0                                                        
         BE    BLDCP140                                                         
         CLI   TWASTA,X'FF'        IF NOT FIRST TIME                            
         BNE   BLDCP140            DONT CREATE RECORD                           
*--BUILD THE KEY                                                                
*                                                                               
BLDCP020 MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,ELEM                                                          
         LA    R2,SVSTA                                                         
         CLC   SVSTA,=5X'00'                                                    
         BE    BLDCP140                                                         
         LA    R3,SVPROJ+2                                                      
         LA    R1,4                                                             
         LA    R9,1                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   0(R4),X'02'                                                      
         MVC   2(2,R4),SVDEM+1                                                  
         LA    R4,4(R4)                                                         
BLDCP050 STC   R9,0(R4)                                                         
         MVC   1(2,R4),0(R3)                                                    
         LA    R1,3(R1)                                                         
         LA    R3,8(R3)                                                         
         LA    R4,3(R4)                                                         
         LA    R9,1(R9)                                                         
         LA    R2,5(R2)                                                         
         CLC   0(5,R2),=5X'00'                                                  
         BE    BLDCP060                                                         
         CLI   0(R2),X'FF'                                                      
         BE    BLDCP060                                                         
         B     BLDCP050                                                         
*                                                                               
BLDCP060 LA    R4,ELEM                                                          
         STC   R1,1(R4)                                                         
         GOTO1 ADDELEM                                                          
         CLI   NODEMSW,1                                                        
         BNE   BLDCP070                                                         
         XC    NODEMSW,NODEMSW                                                  
         B     BLDCP140                                                         
*                                                                               
BLDCP070 LA    R4,ELEM                                                          
         USING CMPEL,R4                                                         
         ZIC   R2,SVSTANO                                                       
         LA    R9,1                                                             
         LA    R3,SVPJPRGS                                                      
*                                                                               
BLDCP100 XC    ELEM,ELEM                                                        
*                                                                               
         MVI   CMPCODE,CMPCODEQ                                                 
         MVI   CMPLEN,X'14'                                                     
         STC   R9,CMPSEQ                                                        
         MVC   CMPPROG(12),0(R3)                                                
         GOTO1 ADDELEM                                                          
         LA    R9,1(R9)                                                         
         LA    R3,L'L1PJPROG(R3)                                                
         BCT   R2,BLDCP100                                                      
         DROP  R4                                                               
*                                                                               
BLDCP140 L     R4,AIO1                                                          
         USING CMEEL,R4                                                         
         MVI   ELCODE,CMECODEQ     LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   CMESRCE,SRCSRC                                                   
         B     BLDCMPEX                                                         
*--BUILD EXTRA INFORMATION ELEMENT                                              
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   CMECODE,CMECODEQ                                                 
         MVI   CMELEN,X'14'                                                     
         MVC   CMESRCE,SRCSRC                                                   
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
BLDCMPEX LA    R2,SRCSTAH                                                       
         L     R4,AIO1                                                          
         USING COMPETE,R4                                                       
         CLC   CMPKLEN,=H'1976'                                                 
         BH    TOOBIG              RECORD IS TOO BIG ERROR                      
         J     XIT                                                              
         DROP  R4,R8                                                            
         EJECT                                                                  
*                                                                               
* GETPROGS GETS THE DEFAULT PROGRAM INFORMATION                                 
*                                                                               
GETPROGS NTR1                                                                   
         LA    R8,SAVED                                                         
         USING SAVED,R8                                                         
*                                                                               
         LA    R2,SVPJPRGS         UPGRADE PROGRAMS                             
         LA    R3,SVSTA                                                         
*                                                                               
         L     R9,AIO2             DETAIL RECORD                                
         USING SIRKEY,R9                                                        
*                                                                               
*--SET UP KEY                                                                   
*                                                                               
GETP050  XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         XC    KEY+10(1),KEY+10                                                 
*--MOVE MARKET/STATION INTO THE KEY                                             
         CLC   0(5,R3),=5X'00'                                                  
         BE    GETPEX                                                           
         CLI   0(R3),X'FF'                                                      
         BE    GETPEX                                                           
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPQMKT,QMKT       SET MKT/STATION                              
         MVC   STAPQSTA,0(R3)                                                   
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   KEY+4(5),STAPMKST                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETP120                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
*--FIND DAY TIME ELEMENT THAT MATCHES REQUESTED DAY TIME                        
         L     R4,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   GETP120                                                          
*                                                                               
         USING EDPELEM,R4                                                       
GETP070  CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BE    GETP090                                                          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    GETP070                                                          
         B     GETP120                                                          
*                                                                               
*--READ DETAIL RECORD                                                           
GETP090  MVC   KEY+10(1),EDPSEQ                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETP095                                                          
         DS    H'0'                                                             
         DROP  R4                                                               
*                                                                               
GETP095  MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*--LOOK FOR PROGRAMMING OVERRIDE ELEMENT                                        
GETP100  L     R4,AIO2                                                          
         MVI   ELCODE,EPRCODEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   GETP120                                                          
         USING EPRELEM,R4                                                       
         MVC   0(12,R2),EPRTEXT                                                 
*                                                                               
GETP120  LA    R3,5(R3)                                                         
         LA    R2,12(R2)                                                        
         B     GETP050                                                          
         DROP  R4                                                               
*                                                                               
GETPEX   J     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
* GETELADD GETS THE PROGRAM AND DEMO ELEMENT LOACATIONS                         
*                                                                               
GETELADD NTR1                                                                   
         LA    R3,SAVED                                                         
         USING SAVED,R3                                                         
         L     R9,AIO1                                                          
         USING COMPETE,R9                                                       
*                                                                               
         XC PRGADDR,PRGADDR                                                     
         XC DEMADDR,DEMADDR                                                     
*                                                                               
         LA    R2,SVPJPRGS                                                      
         ST    R2,PJPADDR                                                       
         LA    R2,SVPROJ                                                        
         ST    R2,PJRADDR                                                       
         LA    R2,SVSTA                                                         
         ST    R2,STAADDR                                                       
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DS    H'0'                                                             
GTEL050  CLC   2(2,R4),SVDEM+1                                                  
         BE    GTEL100                                                          
         BRAS  RE,NEXTEL                                                        
         BE    GTEL050                                                          
         DS    H'0'                                                             
GTEL100  ST    R4,DEMELEM                                                       
         LA    R4,4(R4)                                                         
         ST    R4,DEMADDR                                                       
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAM ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DS    H'0'                                                             
         ST    R4,PRGADDR                                                       
*                                                                               
         CLI   TWASTA,X'FF'                                                     
         BE    GETELAEX                                                         
*                                                                               
         CLI   TWASTA,0                                                         
         BE    GETELAEX                                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,TWASTA                                                      
         L     R2,DEMADDR                                                       
         L     R4,PRGADDR                                                       
         L     R8,STAADDR                                                       
*                                                                               
GTEL200  BRAS  RE,NEXTEL           GET NEXT PROGRAM RECORD                      
         BE    *+6                                                              
         DS    H'0'                                                             
*                                                                               
         LA    R2,3(R2)            GET NEXT DEMO RECORD                         
         LA    R8,5(R8)                                                         
*                                                                               
         BCT   R1,GTEL200                                                       
*                                                                               
         ST    R2,DEMADDR                                                       
         ST    R4,PRGADDR                                                       
         ST    R8,STAADDR                                                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,TWASTA                                                      
         LA    R2,SVPJPRGS                                                      
         LA    R4,SVPROJ                                                        
*                                                                               
GTEL300  LA    R2,12(R2)           GET NEXT PROGRAM RECORD                      
         LA    R4,8(R4)                                                         
         BCT   R1,GTEL300                                                       
*                                                                               
         ST    R2,PJPADDR                                                       
         ST    R4,PJRADDR                                                       
*                                                                               
GETELAEX J     XIT                                                              
         DROP  R3                                                               
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*--FILLREC CHECKS TO SEE THAT ALL STATIONS ON SVSTA                             
*--EXIST ON COMPETION RECORD. IF NOT ROUTINE ALTERS                             
*--THE ELEMENTS TO ACCOMIDATE THE EXTRA OR OMITTED STATIONS.                    
*                                                                               
FILLREC  NTR1                                                                   
         L     R8,AIO1                                                          
         USING COMPETE,R8                                                       
*                                                                               
*--MAKE A COPY OF THE RECORD IN AIO3 TO FIX                                     
*                                                                               
         L     R0,AIO3                                                          
         LA    R1,2000                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
*--DELETE THE OLD 02 AND 03 ELEMENTS  ALL CORRECTIONS MADE TO AIO3              
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'02',AIO3),0,0                        
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'03',AIO3),0,0                        
*                                                                               
*--NOW MAKE NEW X'01' ELEMENT - STATION LIST                                    
*                                                                               
         LA    R9,SAVED                                                         
         USING SAVED,R9                                                         
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'01'        LOOK FOR LIST ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   FILR010                                                          
*--DELETE THE ELEMENT                                                           
         XC    DMCB(16),DMCB                                                    
         MVC   DMCB(4),AIO3                                                     
         MVI   DMCB,C'S'                                                        
         ST    R4,DMCB+4                                                        
         GOTO1 RECUP,DMCB                                                       
*--MAKE THE NEW X'01' ELEMENT                                                   
FILR010  XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0166'     FIXED LENGTH                                
         MVC   ELEM+2(L'SVSTA),SVSTA                                            
*--ADD THE NEW ELEMENT                                                          
         MVC   AIO,AIO3                                                         
         GOTO1 ADDELEM                                                          
         MVC   AIO,AIO1                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,X'01'        LOOK FOR LIST ELEMENT                        
         BRAS  RE,GETEL                                                         
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         ST    R4,ELEMADR          GET READY FOR WHERE TO PUT 03 ELEMS          
*                                                                               
*--MAKE A LIST OF CORRESPONDING OLD STATION NUMBERS TO NEW STATIONS             
*                                                                               
         XC    SVSTANUM,SVSTANUM                                                
         LA    R2,SVSTA            NEW STATION LIST                             
         LA    R3,SVSTANUM         CORRESPONDING CURRENT STATION NUMBER         
FILR069  L     R4,AIO1                                                          
         MVI   ELCODE,X'01'        LOOK FOR CURRENT STATION LIST                
         BRAS  RE,GETEL                                                         
         BE    *+6                 BETTER BE THERE                              
         DC    H'0'                                                             
         LA    R4,2(R4)            BUMP TO FIRST CURRENT STATION                
         LA    R1,1                OLD STATION NUMBER COUNTER                   
FILR070  CLC   0(5,R2),0(R4)                                                    
         BNE   FILR075                                                          
         STC   R1,0(R3)            CORRES CURRENT STATN NUM LIST                
FILR075  LA    R1,1(R1)            NEXT CURRENT NUMBER                          
         LA    R4,5(R4)            NEXT CURRENT STATION                         
         CLI   0(R4),X'00'         END OF CURRENT LIST?                         
         BE    FILR080                                                          
         CH    R1,=H'20'           GONE THROUGH ALL 20 CURRENT STATIONS         
         BH    FILR080                                                          
         B     FILR070                                                          
FILR080  LA    R2,5(R2)            NEXT NEW STATION                             
         LA    R3,1(R3)            NEXT SPOT FOR CORRES. OLD STATN NUM          
         CLI   0(R2),X'00'         END OF NEW LIST                              
         BE    FILR100                                                          
         B     FILR069                                                          
*                                                                               
*--BUMP THROUGH NEW STATIONS AND MAKE NEW X'03' ELEMENTS                        
*                                                                               
FILR100  DS    0H                                                               
         MVI   0(R3),X'FF'         END OF CORRES. NUM LIST                      
         LA    R2,SVSTA            NEW STATION LIST                             
         LA    R3,SVSTANUM         CORRESPONDING CURRENT STATION NUMBER         
         LA    R9,1                CNT FOR NEW STATION IF NEED TO ADD           
*                                                                               
FILR110  XC    ELEM,ELEM           MAKE NEW '03'ELEMENT                         
         MVC   ELEM(2),=X'0314'    FIXED LENGTH                                 
         STC   R9,ELEM+2           NEW STATION NUMBER                           
         MVC   ELEM+3(17),=17X'FF'                                              
         CLI   0(R3),X'00'         IS THERE A CORRES. OLD STATION               
         BE    FILR140             NO, THEN ADD ELEMENT                         
         L     R4,AIO1                                                          
         MVI   ELCODE,X'03'        LOOK FOR PROGRAM ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    FILR130                                                          
         B     FILR140                                                          
*                                                                               
FILR120  BRAS  RE,NEXTEL                                                        
         CLI   0(R4),CMPCODEQ      X'03'                                        
         BNE   FILR140                                                          
*                                                                               
FILR130  DS    0H                                                               
         ZIC   R0,2(R4)                                                         
         N     R0,=X'0000007F'                                                  
         CLM   R0,1,0(R3)          FIND CORRES. OLD STATION NUMBER              
         BNE   FILR120                                                          
         MVC   ELEM+3(17),3(R4)    MOVE NAME INTO ELEMENT                       
         TM    2(R4),X'80'         KEEP OVERRIDE BIT ON IF THERE                
         BZ    FILR140                                                          
         OI    ELEM+2,X'80'                                                     
*                                                                               
*--ADD THE NEW ELEMENT                                                          
FILR140  DS    0H                                                               
         XC    DMCB(16),DMCB                                                    
         MVC   DMCB(4),AIO3                                                     
         MVI   DMCB,C'S'                                                        
         LA    R1,ELEM                                                          
         ST    R1,DMCB+4                                                        
         L     R1,ELEMADR          WHERE TO PUT ELEMENT                         
         ST    R1,DMCB+8                                                        
         GOTO1 RECUP,DMCB                                                       
         L     R1,ELEMADR                                                       
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         ST    R1,ELEMADR          WHERE WE'LL WANT NEXT ELEMENT PUT            
*                                                                               
         LA    R9,1(R9)            BUMP NEW STATION NUMBER COUNTER              
         LA    R3,1(R3)            BUMP TO NEXT CORRES OLD STA NUM              
         CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    FILR200                                                          
         CH    R9,=H'20'                                                        
         BH    FILR200                                                          
         B     FILR110                                                          
*                                                                               
*--NOW MAKE NEW '02' ELEMENTS                                                   
*                                                                               
FILR200  DS    0H                  NOW CHECK FOR CORRES. '02' ELEMENT           
         L     R4,AIO1                                                          
         MVI   ELCODE,X'02'        LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BE    FILR220                                                          
         B     FILR300                                                          
*                                                                               
FILR210  BRAS  RE,NEXTEL                                                        
         CLI   0(R4),CMDCODEQ      X'02'                                        
         BNE   FILR300                                                          
*                                                                               
FILR220  DS    0H                                                               
         XC    ELEM,ELEM           MAKE NEW '02'ELEMENT                         
         LA    R9,1                                                             
         LA    R3,SVSTANUM                                                      
         ST    R4,ELEMADR                                                       
         LR    R0,R4                                                            
         ZIC   R1,1(R4)                                                         
         AR    R0,R1                                                            
         ST    R0,ELEMEND                                                       
         LA    R2,ELEM                                                          
         MVC   ELEM(4),0(R4)                                                    
         LA    R4,4(R4)            FIRST STATION NUMBER                         
         LA    R2,4(R2)            FIRST STATION NUMBER                         
FILR230  STC   R9,0(R2)            NEW STATION NUMBER                           
         MVC   1(2,R2),=X'FFFF'                                                 
FILR233  MVC   CKBYTE,0(R4)                                                     
         NI    CKBYTE,X'7F'        DON'T DROP STUPID OVERRIDE BIT               
         CLC   CKBYTE,0(R3)        MATCH ?                                      
         BE    FILR235                                                          
         LA    R4,3(R4)                                                         
         C     R4,ELEMEND                                                       
         BE    FILR240                                                          
         B     FILR233                                                          
FILR235  MVC   1(2,R2),1(R4)       MOVE INFO INTO ELEMENT                       
         TM    0(R4),X'80'         KEEP OVERRIDE BIT IF THERE                   
         BZ    FILR240                                                          
         OI    0(R2),X'80'                                                      
FILR240  LA    R9,1(R9)                                                         
         LA    R2,3(R2)                                                         
         L     R4,ELEMADR          START AT BEGINNING OF ELEMENT+4              
         LA    R4,4(R4)                                                         
         LA    R3,1(R3)            NEXT OLD NUMBER                              
         CLI   0(R3),X'FF'         END OF NEW LIST                              
         BNE   FILR230                                                          
         BCTR  R9,0                                                             
         MH    R9,=H'3'            NEW LENGTH =(#STATIONS * 3) + 4              
         AH    R9,=H'4'                                                         
         STC   R9,ELEM+1                                                        
*--ADD THE NEW ELEMENT                                                          
         MVC   AIO,AIO3                                                         
         GOTO1 ADDELEM                                                          
         MVC   AIO,AIO1                                                         
         L     R4,ELEMADR          START AT BEGINNING OF ELEMENT                
         B     FILR210                                                          
*                                                                               
*--MOVE NEW RECORD BACK TO AIO1                                                 
*                                                                               
FILR300  DS    0H                                                               
         L     R0,AIO1                                                          
         LA    R1,2000                                                          
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
FILREX   J     XIT                                                              
         DROP  R8,R9                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*--GTDEMO RETURNS THE PROPER DEMO IN DEMADDR                                    
*--R8 CONTAINS CURRENT STATION                                                  
*--DEMELEM HAS THE ADDRES OF THE DEMO ELEMENT                                   
*                                                                               
GTDEMO   NTR1                                                                   
         ICM   R8,15,0(R1)                                                      
         L     R9,AIO1                                                          
         USING COMPETE,R9                                                       
*                                                                               
         XC    DEMADDR,DEMADDR                                                  
         LA    R1,CMSSTA                                                        
         L     R2,DEMELEM                                                       
         LA    R2,4(R2)            POINT TO DEMOS                               
*                                                                               
GTDEM050 CLI   0(R1),0                                                          
         BE    GTDEM080                                                         
         CLC   0(5,R8),0(R1)                                                    
         BE    GTDEM070                                                         
         LA    R1,5(R1)                                                         
         LA    R2,3(R2)                                                         
         B     GTDEM050                                                         
*                                                                               
GTDEM070 ST    R2,DEMADDR                                                       
GTDEM080 J     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*--CHKSRC READS THE 04 ELEMENT ON THE COMPETITION                               
*--RECORDS AND MOVES IT IN THE HEADLINE.                                        
*                                                                               
CHKSRC   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,CMECODEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   CHKSRCEX                                                         
*                                                                               
         CLI   2(R4),X'40'                                                      
         BNH   CHKSRCEX                                                         
*                                                                               
CHKSRC30 XC    SRCSRC,SRCSRC                                                    
         MVC   SRCSRC(1),2(R4)     COMP SOURCE CODE                             
         MVI   SRCSRCH+5,1         SET LENGTH TO 1                              
         LA    R2,SRCSRCH                                                       
         GOTO1 VALISRC                                                          
*                                                                               
         MVC   DEMOSRC,SRCSRC                                                   
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   CHKSRCEX            NO                                           
******   CLI   SRCSRC,C'A'         TEST ARBITRON                                
******   BNE   *+14                NO                                           
******   MVC   SRCSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
******   B     CHKSRCEX                                                         
******   MVC   SRCSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
*                                                                               
CHKSRCEX J     XIT                                                              
         EJECT                                                                  
*                                                                               
*--GTPROG RETURNS THE PROPER PROGRAM PRGADDR                                    
*--R8 CONTAINS CURRENT STATION                                                  
*                                                                               
GTPROG   NTR1                                                                   
         ICM   R8,15,0(R1)                                                      
         L     R9,AIO1                                                          
         USING COMPETE,R9                                                       
*                                                                               
         XC    PRGADDR,PRGADDR                                                  
         L     R4,AIO1                                                          
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAM ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   GTPRG080                                                         
*                                                                               
         LA    R1,CMSSTA                                                        
*                                                                               
GTPRG050 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(5,R8),0(R1)                                                    
         BE    GTPRG070                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   GTPRG080                                                         
         LA    R1,5(R1)                                                         
         B     GTPRG050                                                         
*                                                                               
GTPRG070 ST    R4,PRGADDR                                                       
GTPRG080 J     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*--SETHEAD SETS THE HEADLINE UP WHEN PROGRAM IS                                 
*--ENTERED FROM ANOTHER OVERLAY AND NOT THE BASE                                
*                                                                               
SETHEAD  NTR1                                                                   
*                                                                               
         MVC   CONHEAD(39),=C'RECORD DISPLAYED-NOW ENTER NEXT REQUEST'          
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONSERVH+6,X'81'    SET ON MODIFIED BIT AND XMIT                 
*                                                                               
         MVC   SRCFUT(67),=C'ENTER=NEXT SCREEN PF1=CEST PF2=DETAIL PF11+        
               =NEXT SELECTION PF12=QUIT'                                       
         OI    SRCFUTH+6,X'80'    XMIT                                          
*                                                                               
         CLI   SELSW,X'FF'                                                      
         JE    XIT                                                              
*                                                                               
         LA    R2,CONRECH          PROTECT ALL KEY FIELDS                       
         LA    R3,SRCBK1H                                                       
SD10     OI    1(R2),X'20'         PROTECT                                      
         NI    1(R2),X'FE'         UNMODIFY                                     
         ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         CR    R2,R3                                                            
         BL    SD10                                                             
*--DONT PROTECT DEMO AND SOURCE FIELDS                                          
         LA    R2,SRCSRCH                                                       
         NI    1(R2),X'DF'                                                      
         LA    R2,SRCDEMH                                                       
         NI    1(R2),X'DF'                                                      
*--SET YEAR FOR VALID NUMERIC                                                   
         LA    R2,SRCYRH                                                        
         OI    4(R2),X'08'                                                      
*                                                                               
         MVC   AIO,AIO2            DETAIL RECORD IS IN IO2                      
         L     R3,AIO                                                           
         USING SIRREC,R3                                                        
*                                                                               
         MVC   PERNUM,SIRKMON                                                   
         NI    PERNUM,X'0F'                                                     
*                                                                               
         XC    WORK,WORK           DISPLAY STATION                              
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKST,SIRKMS                                                  
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   SRCSTA,STAPQSTA                                                  
         DROP  R4                                                               
*                                                                               
         OI    SRCSTAH+6,X'80'     XMIT                                         
         CLI   SRCSTA,X'F0'        CABLE HEADEND?                               
         BL    *+8                                                              
         MVI   SRCSTA+4,C'/'       YES                                          
*                                                                               
         MVC   SRCDPT,SIRKDPT      DISPLAY DAYPART                              
         OI    SRCDPTH+6,X'80'     XMIT                                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R4          DISPLAY DAY/TIME                             
         GOTO1 UNDAY,DMCB,EDPDAY,SRCDAY                                         
         OI    SRCDAYH+6,X'80'     XMIT                                         
*                                                                               
         GOTO1 UNTIME,DMCB,EDPTIME,SRCTIME                                      
         OI    SRCTIMEH+6,X'80'    XMIT                                         
         DROP  R4                                                               
*                                                                               
         XC    SRCPER,SRCPER                                                    
         OI    SRCPERH+6,X'80'     XMIT PERIOD NAME                             
*                                                                               
         XC    KEY,KEY             BUILD SCHEME KEY                             
         MVC   KEY(13),0(R3)                                                    
         LA    R3,KEY                                                           
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR SCHEME KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOSCHEM                                                          
*                                                                               
         MVC   AIO,AIO3            USE IO3 FOR SCHEME RECORD                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,EPNCODEQ     LOOK AT PERIOD NAMES ELEMENT                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,2(R4)            GO PAST OVERHEAD                             
SD20     CLC   PERNUM,0(R4)        TEST MATCH ON PERIOD                         
         BE    *+12                                                             
         LA    R4,5(R4)            NEXT PERIOD                                  
         B     SD20                                                             
         MVC   SRCPER,1(R4)        PERIOD NAME                                  
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         L     R3,AIO                                                           
         OI    SRCYRH+6,X'80'      XMIT                                         
         MVC   BYTE,SIRKYEAR       YEAR IN 1'S COMPLEMENT                       
         XI    BYTE,X'FF'                                                       
         ZIC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         UNPK  SRCYR,DUB                                                        
         OI    SRCYR+1,X'F0'       EBCDIC YEAR                                  
*                                                                               
         OI    SRCSCHH+6,X'80'     XMIT                                         
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+14                                                             
         MVC   SRCSCH,=C'ALL'                                                   
         B     SDEXIT                                                           
         GOTO1 CLUNPK,DMCB,SIRKCODE,SRCSCH                                      
         DROP  R3                                                               
*                                                                               
SDEXIT   MVI   SELSW,X'FF'                                                      
         BAS   RE,SETLEN                                                        
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
*--SET THE SCREEN LENGTHS                                                       
*                                                                               
SETLEN   NTR1                                                                   
         LA    R9,SRCBK1H                                                       
         LA    R2,SRCSTAH                                                       
SETL050  LA    RE,8                                                             
         TM    1(R2),X'02'         DOES FIELD HAVE EXTENDED HEADER              
         BZ    SETL070                                                          
         LA    RE,16                                                            
SETL070  SR    R8,R8               FIND LENGTH OF FIELD                         
         ICM   R8,1,0(R2)                                                       
         SR    R8,RE               TOTAL LENGTH-HEADER LENGTH                   
         LA    R1,8(R2)                                                         
         SR    RE,RE                                                            
SETL090  CLI   0(R1),X'00'                                                      
         BE    SETL130                                                          
         CLI   0(R1),X'40'                                                      
         BE    SETL130                                                          
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R8,SETL090                                                       
SETL130  STC   RE,5(R2)            STORE LENGTH IN HEADLINE                     
         SR    RE,RE               BUMP TO NEXT FIELD                           
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
         CR    R2,R9                                                            
         BL    SETL050                                                          
         J     XIT                                                              
         EJECT                                                                  
INVPER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPERM),INVPERM                                       
         B     SOLONG                                                           
INVPERM  DC    C'* ERROR * INVALID PERIOD NAME *'                               
*                                                                               
NODPT    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODPTM),NODPTM                                         
         LA    R2,SRCDPTH                                                       
         B     SOLONG                                                           
NODPTM   DC    C'* ERROR * DAYPART NOT FOUND *'                                 
*                                                                               
INVSCHM  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVSCHMM),INVSCHMM                                     
         B     SOLONG                                                           
INVSCHMM DC    C'* ERROR * INVALID SCHEME NAME *'                               
*                                                                               
NOSCHEM  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSCHEMM),NOSCHEMM                                     
         B     SOLONG                                                           
NOSCHEMM DC    C'* ERROR * SCHEME NOT FOUND *'                                  
*                                                                               
INVYER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVYERM),INVYERM                                       
         LA    R2,SRCYRH                                                        
         B     SOLONG                                                           
INVYERM  DC    C'* ERROR * INVALID YEAR *'                                      
*                                                                               
INVDMO   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDMOM),INVDMOM                                       
         LA    R2,SRCDEMH                                                       
         B     SOLONG                                                           
INVDMOM  DC    C'* ERROR * INVALID DEMO *'                                      
*                                                                               
INVBOOK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVBOOKM),INVBOOKM                                     
         B     SOLONG                                                           
INVBOOKM DC    C'* ERROR * INVALID BOOK *'                                      
*                                                                               
INVINPT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVINPTM),INVINPTM                                     
         B     SOLONG                                                           
INVINPTM DC    C'* ERROR * INVALID INPUT *'                                     
*                                                                               
NORECS   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NORECSM),NORECSM                                       
         B     SOLONG                                                           
NORECSM  DC    C'NO RECORDS EXIST FOR THIS REQUEST'                             
*                                                                               
INVDET   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDETM),INVDETM                                       
         B     SOLONG                                                           
INVDETM  DC    C'* ERROR * NO DETAIL RECORD FOR THIS REQUEST'                   
*                                                                               
INVDMN   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDMNM),INVDMNM                                       
         B     SOLONG                                                           
INVDMNM  DC    C'* ERROR * NO DEFAULT DEMO FOR THIS SCHEME'                     
*                                                                               
TOOBIG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOBIGM),TOOBIGM                                       
         B     SOLONG                                                           
TOOBIGM  DC    C'* ERROR * NO NEW STATIONS OR DEMOS CAN BE ADDED'               
*                                                                               
NODATA   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODATAM),NODATAM                                       
         B     SOLONG                                                           
NODATAM  DC    C'NO DATA TO DISPLAY CHECK DEFAULT BOOKS'                        
*                                                                               
FIRST20  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOMNYST),TOOMNYST                                     
         B     SOLONG                                                           
TOOMNYST DC    C'* ONLY FIRST 20 STATIONS SHOWN *'                              
*                                                                               
SOLONG   GOTO1 ERREX2                                                           
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         OI    CONHEADH+6,X'80'    XMIT HEADER                                  
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         L     RD,SAVERD           BACK ALL THE WAY OUT                         
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
DMWRITE  DC    CL7'DMWRT  '                                                     
MAJBKS   DC    XL4'0205070B'                                                    
*                                                                               
NMAXSTA  EQU   20                  MAX STATIONS                                 
NSTASCR  EQU   7                   STATIONS/SCREEN                              
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         MVC   SVUSEDDS,WORK+3     SAVE TWO BOOK DISPLAY FLAG                   
*                                                                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   VK03                                                             
         BAS   RE,SETHEAD                                                       
*                                                                               
*--TEST SCREEN FIELDS FOR CHANGE                                                
VK03     TM    SRCSTAH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCDPTH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCSRCH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCDAYH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCTIMEH+FVIIND-FVIHDR,FVIVAL                                    
         BZ    VK05                                                             
         TM    SRCYRH+FVIIND-FVIHDR,FVIVAL                                      
         BZ    VK05                                                             
         TM    SRCPERH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCSCHH+FVIIND-FVIHDR,FVIVAL                                     
         BZ    VK05                                                             
         TM    SRCDEMH+FVIIND-FVIHDR,FVIVAL                                     
         BO    VK08                                                             
*                                                                               
VK05     LA    RE,SYSSPARE+1024    CLEAR APPLICATION STORAGE                    
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         MVC   FULL,SAVERD                                                      
         XCEF                                                                   
         MVC   SAVERD,FULL                                                      
*                                                                               
         MVI   TWASTA,FF           INITIALIZE STATION POINTER                   
*                                                                               
*--SET ADDRESS OF COMMON ROUTINES                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOVAL,CDEMOVAL                                                
         DROP  R1                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A71'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DS    H'0'                                                             
         MVC   VEDITOR,DMCB                                                     
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A22'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DS    H'0'                                                             
         MVC   VSPDEMUP,DMCB                                                    
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A21'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DS    H'0'                                                             
         MVC   VSPDEMLK,DMCB                                                    
*                                                                               
VK08     XC    SVUPFILE,SVUPFILE   CLEAR SAVED VALUES                           
         XC    SVUPGRD,SVUPGRD                                                  
         XC    SVUPFRBK,SVUPFRBK                                                
         XC    SVUPINP,SVUPINP                                                  
*                                                                               
         LA    R9,SAVEKEY          PUT KEY IN SAVEKEY, THEN MOVE TO KEY         
         USING SIRKEY,R9                                                        
         XC    SAVEKEY,SAVEKEY                                                  
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         MVC   0(9,R2),=XL9'0900000000010000E3'                                 
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SRCSTAH          MARKET/STATION                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   SIRKMS,BMKTSTA                                                   
*                                                                               
         LA    R2,SRCDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         MVC   DAYPART,SRCDPT                                                   
         MVC   SIRKDPT,DAYPART                                                  
*                                                                               
         LA    R2,SRCYRH           YEAR                                         
         CLI   5(R2),0                                                          
         BE    VK10                                                             
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,5(R2)            GET THE YEAR                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
*  YEAR 2000 FIX                                                                
         C     R3,=F'50'                                                        
         BH    *+8                                                              
         A     R3,=F'100'                                                       
*                                                                               
         STC   R3,SIRKYEAR                                                      
         MVC   SCRYEAR,SIRKYEAR    YEAR IS IN ONE'S COMPLEMENT FORM             
         XI    SIRKYEAR,X'FF'      YEAR IS IN ONE'S COMPLEMENT FORM             
*                                                                               
VK10     LA    R2,SRCSCHH          SCHEME                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         OI    6(R2),X'80'         XMIT                                         
         B     VK30                                                             
*                                                                               
VK20     CLC   =C'ALL',8(R2)       TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK30                                                             
         OC    8(3,R2),=3X'40'                                                  
         GOTO1 CLPACK,DMCB,8(R2),SIRKCODE                                       
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BNE   INVSCHM                                                          
*                                                                               
VK30     LA    R9,KEY              CREATE THE SCHEME KEY                        
         XC    KEY,KEY                                                          
         MVC   SIRKEY,SAVEKEY                                                   
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST SCHEME EXISTS                           
         BNE   NOSCHEM                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,ECLCODEQ     LOOK FOR CLIENT CODE                         
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SCHCLI(2),2(R4)                                                  
*                                                                               
*--READ SOURCE ELEMENT                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,ERSCODEQ     LOOK FOR DEMO MENU NAME                      
         BRAS  RE,GETEL                                                         
         BNE   VK32                                                             
         MVC   DEMOSRC,2(R4)                                                    
         B     VK34                                                             
VK32     MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK34     XC    DEMMENU,DEMMENU                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,EDMCODEQ     LOOK FOR DEMO MENU NAME                      
         BRAS  RE,GETEL                                                         
         BE    VK36                                                             
         MVC   DEMMENU,HLDDMNM                                                  
         B     VK38                                                             
VK36     MVC   DEMMENU,2(R4)                                                    
         OC    DEMMENU,=4X'40'                                                  
*                                                                               
VK38     L     R4,AIO                                                           
         MVI   ELCODE,EDYCODEQ     LOOK FOR DEFAULT YEAR ELEMENT                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R4                                                       
         MVC   CURYEAR,EDYYEAR     GET THE YEAR                                 
         DROP  R4                                                               
*                                                                               
         LA    R9,SAVEKEY                                                       
         CLI   SIRKYEAR,0          TEST YEAR IS ABSENT                          
         BNE   VK40                NO, WE HAVE ONE                              
         MVC   SIRKYEAR,CURYEAR    USE THE CURRENT YEAR                         
         ZIC   R1,SIRKYEAR                                                      
         CVD   R1,DUB                                                           
         UNPK  SRCYR,DUB                                                        
         OI    SRCYR+1,X'F0'                                                    
         OI    SRCYRH+6,X'80'      XMIT THE CURRENT YEAR                        
         XI    SIRKYEAR,X'FF'      SAVE YEAR IN ONE'S COMPLEMENT FORM           
         B     VK50                                                             
*                                                                               
VK40     ZIC   R1,SIRKYEAR         COMPARE GIVEN YEAR TO CURRENT YEAR           
         X     R1,=F'255'          MAKE YEAR POSITIVE                           
         ZIC   R0,CURYEAR                                                       
         SR    R1,R0                                                            
         LPR   R1,R1               DIFFERENCE IN YEARS                          
         CH    R1,=H'1'            NO GREATER THAN ONE YEAR DIFFERENCE          
         BH    INVYER                                                           
*                                                                               
VK50     L     R4,AIO                                                           
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LA    R4,2(R4)            FIRST DAYPART                                
*                                                                               
VK60     CLC   DAYPART,0(R4)       LOOK FOR THE GIVEN DAYPART                   
         BE    *+16                                                             
         LA    R4,8(R4)            NEXT DAYPART                                 
         BCT   R1,VK60                                                          
         B     NODPT                                                            
*                                                                               
         LA    R3,PRGTYPSV         SAVE PROGTYPE CODE/NAME IN PRGTYPSV          
         XC    PRGTYPSV,PRGTYPSV                                                
         L     R4,AIO                                                           
         MVI   ELCODE,EPCCODEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   VK80                PROGTYP IS OPTIONAL                          
*                                                                               
         USING EPCELEM,R4                                                       
         SR    R0,R0                                                            
         ZIC   R1,EPCLEN                                                        
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            LEAVES NO. OF PROGTYPES IN R1                
*                                                                               
VK70     MVC   0(8,R3),EPCDCODE    CODE+NAME                                    
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,VK70                                                          
         DROP  R4                                                               
*                                                                               
VK80     L     R4,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         LA    R4,2(R4)            FIRST PERIOD NAME                            
*                                                                               
         MVI   PERNUM,0            PERIOD                                       
         LA    R2,SRCPERH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         OC    SRCPER,=C'    '     PAD NAME WITH BLANKS                         
*                                                                               
VK90     CLC   SRCPER,1(R4)        LOOK FOR MATCH OF PERIOD NAME                
         BE    *+16                                                             
         LA    R4,5(R4)                                                         
         BCT   R1,VK90                                                          
         B     INVPER                                                           
*                                                                               
         MVC   PERNUM,0(R4)        SAVE THE PERIOD NUMBER                       
         MVC   SIRKMON,PERNUM                                                   
         OI    SIRKMON,SIRKBUYQ                                                 
*                                                                               
         LA    R9,KEY                                                           
         MVC   SIRKEY,SAVEKEY      BUILD THE PERIOD KEY                         
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKDPT,0                                                        
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   SIRKEY,KEYSAVE                                                   
         BNE   INVPER                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R4                                                       
VK100    CLC   PERNUM,EPDNUM       LOOK FOR THE GIVEN PERIOD                    
         BE    *+16                                                             
         BRAS  RE,NEXTEL                                                        
         BE    VK100                                                            
         B     INVPER                                                           
*                                                                               
         MVC   PERBOOKS,EPDBOOKS   DEFAULT BOOKS                                
*                                                                               
         MVI   WORK,C' '           BUILD PERIOD UPGRADE EXPRESSION              
         MVC   WORK+1(L'WORK-1),WORK                                            
         OC    EPDUPFIL(27),EPDUPFIL                                            
         BNZ   *+14                THERE IS A PERIOD DEFAULT UPGRADE            
         MVC   WORK(22),=C'* NO DEFAULT UPGRADE *'                              
         B     VK110                                                            
*                                                                               
         MVC   SVUPFILE(1),EPDUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EPDUPGRD                                              
         MVC   SVUPFRBK(2),EPDUPFBK                                             
         MVC   SVUPINP(16),EPDUPINP                                             
*                                                                               
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EPDUPFIL                                               
         MVC   WORK+4(16),EPDUPINP                                              
*                                                                               
         OC    EPDUPFBK,EPDUPFBK   TEST ANY SHARE BOOK                          
         BZ    VK110                                                            
         LA    R3,WORK+21                                                       
         CLI   0(R3),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EPDUPFBK),(6,3(R3))                               
         DROP  R4                                                               
*                                                                               
VK110    MVC   PERUPG,WORK         SAVE THE UPGRADE EXPRESSION                  
         MVI   SIRKYEAR,0          YEAR IS ABSENT IN UPGRADE KEY                
         MVC   SIRKMON,PERNUM      BUT PERIOD IS THERE                          
         OI    SIRKMON,SIRKBUYQ                                                 
         MVC   SIRKMS,BMKTSTA      SO IS MKT/STA                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR STATION DEFAULT UPGRADE KEY         
         CLC   SIRKEY,KEYSAVE                                                   
         BE    VK115               WE HAVE IT                                   
*                                                                               
         MVC   SIRKEY,KEYSAVE      TRY MARKET DEFAULT UPGRADE                   
         XC    SIRKSTA,SIRKSTA                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST WE HAVE IT                              
         BE    VK115               YES                                          
         MVC   WORK(34),PERUPG     WE DON'T - USE THE PERIOD UPGRADE            
         B     VK130                                                            
*                                                                               
VK115    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET STATION RECORD                           
         L     R4,AIO                                                           
         MVI   ELCODE,EUPCODEQ     STATION DEFAULT UPGRADE ELEMENT              
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EUPELEM,R4                                                       
         MVI   WORK,C' '           BUILD STATION UPGRADE EXPRESSION             
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
*                                                                               
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   VK120               NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   VK120               NO, IT'S A HUT                               
*                                                                               
         MVC   WORK+4(4),=C'PUT/'                                               
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+8)                            
         MVC   WORK+11(2),WORK+12  GET RID OF '/'                               
         MVI   WORK+13,C' '                                                     
         B     *+10                                                             
*                                                                               
VK120    MVC   SVUPFILE(1),EUPUPFIL   STORE UPGRADE EXPRESSION                  
         MVC   SVUPGRD(8),EUPUPGRD                                              
         MVC   SVUPINP(16),EUPUPINP                                             
         XC    SVUPFRBK(2),SVUPFRBK                                             
         MVC   WORK+4(16),EUPUPINP                                              
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    VK130                                                            
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
         MVC   SVUPFRBK,EUPUPFBK   TEST ANY SHARE BOOK                          
*                                                                               
         LA    R3,WORK+21                                                       
         CLI   0(R3),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R3))                               
         DROP  R4                                                               
*                                                                               
VK130    LA    R2,SRCDAYH          DAY                                          
         GOTO1 ANY                                                              
         MVC   SRCDAY,WORK                                                      
         ZIC   R4,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R4),SRCDAY),DAY,WORK                               
         CLI   DAY,0                                                            
         BNE   *+12                                                             
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
*                                                                               
         LA    R2,SRCTIMEH         TIME                                         
         GOTO1 ANY                                                              
         MVC   SRCTIME,WORK                                                     
         ZIC   R4,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R4),SRCTIME),TIME                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,INVTIME                                                    
         B     TRAPERR                                                          
*                                                                               
VK140    MVC   KEY(13),SAVEKEY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VK190                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   VK190                                                            
*                                                                               
         USING EDPELEM,R4                                                       
VK150    CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BNE   *+14                                                             
         MVC   SEQNUM,EDPSEQ                                                    
         B     VK170                                                            
         DROP  R4                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    VK150                                                            
         BNE   VK190                                                            
*                                                                               
VK170    LA    R9,SAVEKEY                                                       
         MVC   SIRKSEQ,SEQNUM      PUT SEQUENCE NUMBER IN KEY                   
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BNE   *+8                                                              
         OI    GENSTAT1,OKADDEL    OK TO ADD BACK DELETED RECORDS               
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         LA    R9,KEY                                                           
         XC    SIRKEY,SIRKEY                                                    
         MVC   SIRKEY(13),SAVEKEY  THE DETAIL RECORD KEY                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST SCHEME EXISTS                           
         BE    VK200                                                            
*-- IF NO DETAIL RECORD EXISTS                                                  
VK190    L     RE,AIO2             CLEAR THE WORK AREA                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   NDETSW,X'FF'                                                     
         B     VK220                                                            
*                                                                               
VK200    MVC   SVDETADR,KEY+14                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE DETAIL RECORD                        
*--SET SCREEN FIELDS TO VALIDATED                                               
VK220    OI    SRCSTAH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCDPTH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCSRCH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCDAYH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCTIMEH+FVIIND-FVIHDR,FVIVAL                                    
         OI    SRCYRH+FVIIND-FVIHDR,FVIVAL                                      
         OI    SRCPERH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCSCHH+FVIIND-FVIHDR,FVIVAL                                     
         OI    SRCDEMH+FVIIND-FVIHDR,FVIVAL                                     
         XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*BOOKSET RESETS THE BOOKS FOR AGENCIES THAT ARE ONLY USING 2 BOOKS              
*                                                                               
BOOKSET  NTR1  BASE=*,LABEL=*                                                   
         USING SAVED,R2                                                         
         OC    PERBOOKS,PERBOOKS   SEE IF FROM PERIOD                           
         BZ    *+14                                                             
         MVC   SVBKS+2(2),SVBKS+6                                               
         B     BOOKEX                                                           
*                                                                               
         MVC   SVBKS(4),SVBKS+4                                                 
*                                                                               
         OC    SVUPFRBK,SVUPFRBK   TEST OVERRIDE SHARE OR PUT BOOK              
         BZ    *+10                                                             
         MVC   SVBKS(2),SVUPFRBK                                                
*                                                                               
         OC    SVUPGRD+2(2),SVUPGRD+2                                           
         BZ    BOOKEX                                                           
         OC    SVUPFRBK,SVUPFRBK                                                
         BNZ   *+14                                                             
         MVC   SVBKS(2),SVUPGRD+2                                               
         B     BOOKEX                                                           
         MVC   SVBKS+2(2),SVUPGRD+2                                             
*                                                                               
BOOKEX   XC    SVBKS+4(4),SVBKS+4                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB6D                                                       
*                                                                               
         ORG   SRCWORK                                                          
RELO     DS    F                   RELOCATION FACTOR                            
SAVERD   DS    F                                                                
DEMADDR  DS    F                                                                
DEMELEM  DS    F                                                                
PRGADDR  DS    F                                                                
PJPADDR  DS    F                                                                
PJRADDR  DS    F                                                                
STAADDR  DS    F                                                                
SAVEKEY  DS    XL13                THE DETAIL KEY                               
COMPKEY  DS    XL13                                                             
LOOPCT   DS    X                                                                
PERNUM   DS    X                   PERIOD                                       
CURYEAR  DS    X                   CURRENT YEAR                                 
SCRYEAR  DS    X                   SCREEN YEAR                                  
COMPER   DS    X                   Y/M COMPETION RECORD                         
PERSTART DS    XL3                 PERIOD START DATE (YMD)                      
PEREND   DS    XL3                 PERIOD END DATE (YMD)                        
DAYPART  DS    C                   DAYPART                                      
DAYTIME  DS    0XL5                                                             
DAY      DS    X                   DAY(S) CODE                                  
TIME     DS    XL4                 START/END TIMES                              
SCHCLI   DS    CL2                 SCHEME CLIENT CODE                           
CKBYTE   DS    X                                                                
SVSTANUM DS    XL(NMAXSTA*1)                                                    
ELEMADR  DS    F                                                                
ELEMEND  DS    F                                                                
PROGTYPE DS    C                   PROGRAM TYPE                                 
YEAR     DS    X                   YEAR                                         
INDEX    DS    X                   SEQUENCE NO. FOR SORTING                     
SEQNUM   DS    X                   SEQUENCE NO. FOR DAY/TIME                    
PERUPG   DS    CL34                PERIOD UPGRADE EXPRESSION                    
DEMMENU  DS    XL4                                                              
SVDEFUPG DS    CL34                SAVE DEFAULT UPGRADE VALUES                  
DEMWRK   DS    CL40                                                             
COSTWRK  DS    CL4                                                              
PERBOOKS DS    XL8                 DEFAULT BOOKS                                
LASTDATE DS    XL3                                                              
PRGTYPSV DS    CL170               PROGRAM TYPE SAVE (CODE+NAME)                
DEMOTYP  DS    XL2                                                              
DEMOSRC  DS    XL1                 DEMO SOURCE                                  
HLDBOOK  DS    XL2                 STATION LOOKUP BOOK                          
STANUM   DS    F                                                                
NEWSNUM  DS    F                                                                
ADDFLAG  DS    C                   'Y' IF ADDREC, 'N' IF PUTREC                 
NDETSW   DS    X                                                                
TWASTA   DS    X                                                                
MORESW   DS    X                                                                
FSTSW    DS    X                                                                
NODEMSW  DS    X                   COMPETITION EXISTS WITHOUT DEMO EL           
STCHG    DS    X                                                                
SELSW    DS    X                   SELECT SWITCH                                
STASW    DS    X                                                                
*                                                                               
*                                                                               
LAPROJ   DS    A                                                                
LANDX    DS    A                                                                
LACOST   DS    A                                                                
LABUPEL  DS    A                                                                
LABOVEL  DS    A                                                                
LABDMEL  DS    A                                                                
LATWA    DS    A                                                                
LASAVE   DS    A                                                                
LOLDSHR  DS    F                                                                
LDNAME   DS    CL7                                                              
LDEM     DS    XL4                                                              
LHDRDA   DS    XL4                                                              
LDEMOVR  DS    XL7                                                              
LNSTASCR DS    XL1                                                              
LBATCH   DS    XL3                                                              
LDMUPBLK DS    (SPDEMUP2)X                                                      
*                                                                               
LCHG     DS    X                                                                
LBK      EQU   X'80'                                                            
LDEMO    EQU   X'40'                                                            
LPRG     EQU   X'20'                                                            
LRTG     EQU   X'10'                                                            
LSHR     EQU   X'08'                                                            
LHDR     EQU   X'04'                                                            
*                                                                               
*--SCREEN HEADER DEFAULTS                                                       
FVIHDR   DS    0XL8                ** EXTRACTED INPUT FIELD HEADER **           
FVTLEN   DS    XL1                 L'FIELD HEADER+L'FIELD                       
FVATRB   DS    XL1                 FIELD ATTRIBUTE                              
FVAPROT  EQU   X'20'               PROTECTED FIELD                              
FVAHIGH  EQU   X'08'               HIGH INTENSITY                               
FVAXTND  EQU   X'02'               EXTENDED HEADER                              
FVAMODF  EQU   X'01'               MODIFIED INPUT FIELD                         
FVABSA   DS    XL2                 SCREEN ABSOLUTE ADDRESS                      
FVIIND   DS    XL1                 INPUT VALIDATION INDICATORS                  
FVITHIS  EQU   X'80'               FIELD INPUT THIS TIME                        
FVILAST  EQU   X'40'               FIELD HAS BEEN INPUT PREVIOUSLY              
FVIVAL   EQU   X'20'               FIELD HAS BEEN VALIDATED PREVIOUSLY          
FVIINV   EQU   X'10'               FIELD IS INVALID                             
FVINUM   EQU   X'08'               FIELD IS VALID NUMERIC                       
FVIALF   EQU   X'04'               FIELD IS VALID ALPHA                         
FVIHEX   EQU   X'02'               FIELD IS VALID HEXADECIMAL                   
FVILEN   DS    XL1                 ACTUAL INPUT DATA LENGTH                     
FVOIND   DS    XL1                 OUTPUT INDICATORS                            
FVOXMT   EQU   X'80'               TRANSMIT FIELD                               
FVOCUR   EQU   X'40'               INSERT CURSOR TO FIELD                       
FVXLEN   DS    XL1                 ACTUAL INPUT DATA LENGTH-1                   
FVIFLD   DS    CL20                                                             
*                                                                               
DETADDR  DS    20CL5                                                            
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
*                                                                               
DEMLKSV  DS    XL32                                                             
*************************************************************                   
**  CAREFUL STORAGE IN SCREEN DOESN'T GO PAST GENCON'S X'E00'                   
*************************************************************                   
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
*                                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE+1024       DON'T CREAM T2170E STORAGE                   
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
VDEMAND  DS    F                                                                
VDEMOVAL DS    F                                                                
VEDITOR  DS    F                                                                
VSPDEMUP DS    F                                                                
VSPDEMLK DS    F                                                                
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
*-- SAVE AREA                                                                   
SAVED    DS    0H                                                               
SVCUMS   DS    XL(4*4)                                                          
SVPJCUM  DS    XL4                                                              
SVPUTVAL DS    XL(4*4)                                                          
SVPJPUT  DS    XL4                                                              
*                                                                               
SVDEMVAL DS    (NMAXSTA*2*4)XL4                                                 
SVDEMVLL EQU   *-SVDEMVAL                                                       
*                                                                               
SVPROJ   DS    (NMAXSTA*2)XL4                                                   
SVPROJL  EQU   *-SVPROJ                                                         
*                                                                               
SVDADDR  DS    20CL5                                                            
         DS    CL1                                                              
SVBKS    DS    XL8                                                              
SVSTA    DS    XL(NMAXSTA*5)                                                    
SVENDSTA DS    XL1                                                              
SVDEM    DS    XL4                                                              
SVPUT    DS    XL4                                                              
SVRTGSHR DS    XL7                                                              
SVSTANO  DS    X                                                                
*                                                                               
SVSELSRC DS    C                                                                
SVSELMED DS    C                                                                
*                                                                               
SVPJPRGS DS    (NMAXSTA)XL(L'L1PJPROG)                                          
SVPJPRGL EQU   *-SVPJPRGS                                                       
*                                                                               
SVPROGS  DS    (NMAXSTA*4)XL(L'L1PROG1)                                         
SVPROGSL EQU   *-SVPROGS                                                        
*                                                                               
SVFLAG   DS    XL1                                                              
SVF2STA  EQU   X'80'                                                            
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
LINE1D   DSECT                                                                  
L1PROG1  DS    CL12                                                             
         DS    XL2                                                              
L1PROG2  DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
L1PJPROG DS    CL12                                                             
         SPACE 2                                                                
LINE2D   DSECT                                                                  
         DS    CL10                                                             
L2RS1    DS    CL12                                                             
         DS    XL2                                                              
L2RS2    DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
L2PJRS   DS    CL12                                                             
         SPACE 2                                                                
CUMLINED DSECT                                                                  
         DS    XL6                                                              
CUM1     DS    CL5                                                              
         DS    XL9                                                              
CUM2     DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
CUMPROJ  DS    CL5                                                              
         EJECT                                                                  
*                                                                               
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 4                                                                
COMPETE  DSECT                                                                  
       ++INCLUDE SPGENCOMP                                                      
         SPACE 4                                                                
COMSTAT  DSECT                                                                  
       ++INCLUDE SPGENCLST                                                      
         SPACE 4                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142SPSFM16   12/13/04'                                      
         END                                                                    
