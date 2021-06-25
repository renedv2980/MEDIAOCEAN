*          DATA SET NEWRI66    AT LEVEL 187 AS OF 05/01/02                      
*          DATA SET NEWRI64    AT LEVEL 100 AS OF 06/08/93                      
*PHASE T32066A,+0                                                               
*INCLUDE RECUP                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32066 - BJNY TAPE TRANSFER'                                    
T32066   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE66**,RR=R2                                                 
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING T32066,RB,R5       NOTE R5 = 2ND BASE REGISTER                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          ANETWS1/ANETWS2/ANETWS3                      
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         BAS   RE,PROCESS          READ TAPE WRITE RECORDS                      
*                                                                               
*        BAS   RE,PRINT            PRINT REPORT                                 
*                                                                               
*        BAS   RE,FINAL            END OF RUN PROCESSING                        
         CLOSE (IN)                                                             
         CLOSE (BJNYSPT)                                                        
         CLOSE (BJNYUNT)                                                        
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'PKCN',PKCOUNT,C'DUMP',4,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'PRCN',PRCOUNT,C'DUMP',4,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'CLIENT',SAVCLI,C'DUMP',3,=C'1D'               
         GOTO1 =V(PRNTBL),DMCB,=C'CLCN',CLCOUNT,C'DUMP',4,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'UNCN',UNCOUNT,C'DUMP',4,=C'1D'                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         OPEN  (IN,(INPUT))                                                     
         OPEN  (BJNYSPT,(OUTPUT))                                               
         OPEN  (BJNYUNT,(OUTPUT))                                               
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
         BAS   RE,PROCINIT                                                      
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,4(R3)                                                         
         ST    R3,RECBUFF                                                       
*                                                                               
         XC    PCOUNT,PCOUNT                                                    
         XC    UCOUNT,UCOUNT                                                    
         XC    RCOUNT,RCOUNT                                                    
         XC    TCOUNT,TCOUNT                                                    
*                                                                               
         L     R0,=A(KEYBUFFS)                                                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,BUFFLOC                                                       
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         XC    TSARBLCK,TSARBLCK                                                
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,24           KEY= 24                                      
         MVC   TSRECL,=XL2'0032'   RECORD = 50                                  
         MVC   TSABUF,BUFFLOC                                                   
         OI    TSIND2,X'40'        EXTENDED COUNTS                              
*- INITIALIZE THE BUFFER                                                        
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSAREC,=A(KEYBUFFS) BUFFER LOCATION                              
*                                                                               
         PRINT GEN                                                              
         GOTO1 ATSAROFF                                                         
         PRINT NOGEN                                                            
*                                                                               
INX      B     XIT                                                              
         DROP  R1                                                               
*                                                                               
KEYBUFFS EQU   9000000                                                          
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* DEMOGRAPHIC INFORMATION FROM THE PC AND RETURNS THE FIRST SCREENFUL           
* OF INFORMATION.  IT ALSO SETS THE INTERNAL CONTROL TO BEGIN THE               
* SEND OF SUBSEQUENT DATA.                                                      
*                                                                               
PROCESS  NTR1                                                                   
         MVI   TWAGYMED,X'83'      AGENCY MEDIA FOR BJNY                        
*                                                                               
PROC020  BAS   RE,GETTAPE          READ TAPE RECORD                             
         BNE   PROCEX              LAST RECORD EXIT                             
*                                                                               
         BAS   RE,GETSTA           READ STATION RECORD                          
*                                                                               
         BAS   RE,SETDEMO          SET THE DEMO LIST                            
*                                                                               
         BAS   RE,BLDPACK          CHECK TO BUILD PACKAGE                       
         BAS   RE,GETPROD          READ CLIENT FOR PRODUCT                      
*                                                                               
         BAS   RE,CALCPROG         CHECK TO BUILD PROGRAM                       
*                                                                               
         BAS   RE,BLDUNIT          CHECK TO BUILD UNIT                          
*                                                                               
         BAS   RE,ADDUNIT          ADD THE UNIT                                 
*                                                                               
         B     PROC020                                                          
*                                                                               
PROCEX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
* READ TAPE RECORD                                                              
*                                                                               
GETTAPE  NTR1                                                                   
         PRINT GEN                                                              
GETTAP10 GET   IN,AIOB                                                          
         PRINT NOGEN                                                            
         LA    R3,AIOB                                                          
         USING TAPEIN,R3                                                        
*        CLC   TCLIENT,=C'G15'                                                  
*        BNE   GETTAP10                                                         
*        CLC   TCLIENT,=C'M2F'                                                  
*        BNE   GETTAP10                                                         
*        CLC   TNET,=C'NBC '                                                    
*        BNE   GETTAP10                                                         
*        CLC   TEST,=C'001'                                                     
*        BNE   GETTAP10                                                         
*        CLC   TPROGNAM(3),=C'NBC'                                              
*        BE    GETTAP10                                                         
GETTAP15 CLC   TCOUNT,=F'100'                                                   
         BH    GETTAP20                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'TAPE',0(R3),C'DUMP',232,=C'1D'                
         L     RE,TCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,TCOUNT                                                        
*                                                                               
GETTAP20 SR    RE,RE                                                            
*                                                                               
GETTAPEX LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
GETSTA   NTR1                                                                   
*        L     R3,AIO1                                                          
         LA    R3,AIOB                                                          
         USING TAPEIN,R3                                                        
*                                                                               
*--FIX STATION ERROR                                                            
*        CLC   TNET(4),=CL4'ZQFS'                                               
*        BNE   *+10                                                             
*        MVC   TNET(4),=CL4'ZGFS'                                               
*                                                                               
         CLC   TNET,TWNET                                                       
         BE    GETSTEX                                                          
*                                                                               
         BAS   RE,STATSET                                                       
*                                                                               
         L     R4,AIO1                                                          
         USING STAREC,R4                                                        
         MVI   0(R4),C'0'                                                       
         MVC   1(14,R4),0(R4)                                                   
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),TNET                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,=CL2'BJ'                                                 
         MVC   KEY(15),0(R4)                                                    
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,(0,COMMAND),FILENAME,(R4),(R4),0                    
         PRINT NOGEN                                                            
         CLC   KEY(15),0(R4)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1                                                          
         MVC   TWPSTTYP,SPTYPE                                                  
         MVC   TWMEDTYP,STYPE                                                   
         PACK  DUB,SMKT(4)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,TWMKT                                                       
         MVC   TWNET,TNET                                                       
*                                                                               
         MVC   AIO,AIO1                                                         
GETSTEX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* BUILD PACKAGE RECORD                                                          
*                                                                               
BLDPACK  NTR1                                                                   
*        L     R4,AIO1                                                          
         LA    R4,AIOB                                                          
         USING TAPEIN,R4           INPUT TAPE DSECT                             
         MVI   TWPKG,1                                                          
         MVC   TWPROD,TPROD                                                     
*                                                                               
         BAS   RE,UNITSET                                                       
         USING NPRECD,R3                                                        
*                                                                               
*                                                                               
*-ADD NEW PACKAGE RECORD                                                        
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R3,RECBUFF                                                       
*-- CLEAR IO1                                                                   
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
*        MVC   NPKTYPE(19),KEYSAVE                                              
         MVI   NPKPACK,1                                                        
*                                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
*--SET UNIVERSE                                                                 
         XC    DUB,DUB                                                          
         PACK  DUB+5(3),TUNIV(4)                                                
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         MVC   TWUNIV,DUB+5        TO ISOLATE UNIVERSE CODE PWO.                
         MVC   NPAKUNCD,TWUNIV                                                  
*        OI    NPAKCNTL,X'40'      SET IMPRESSION BIT                           
*                                                                               
         MVC   NPAKNAME(15),=CL15'BJNY TRANS DATA'                              
         MVI   NPAKDP,C'P'                                                      
         MVI   NPAKACTA,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,0),(2,NPAKACTD)                                   
         MVC   NPAKHUTL(1),=XL1'C0'                                             
         MVI   TWHUTTYP,C'A'                                                    
         MVC   NPAKHTYP,TWHUTTYP                                                
*                                                                               
*--BUILD 77 ELEMENT (TELL SYSTEM RECORD CREATED IN TRANSFER)                    
         XC    WORKAREA(70),WORKAREA                                            
*                                                                               
         MVC   WORKAREA(3),=XL3'770340'                                         
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*                                                                               
*-READ PACKAGE RECORD                                                           
*                                                                               
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,TWAGYMED                                                   
         GOTO1 =V(CLPACK),DMCB,TCLIENT,TWCLIENT                                 
         MVC   NPKCLT,TWCLIENT                                                  
         MVC   NPKNET,TWNET                                                     
         PACK  DUB,TEST(3)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,1,TWEST                                                       
         MVC   NPKEST,TWEST                                                     
         BAS   RE,UNITSET                                                       
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         XC    TSARIO,TSARIO                                                    
         MVI   TSARIO,3            PACKAGE AREA                                 
         MVC   TSARIO+1(20),NPKTYPE                                             
         MVC   WORKAREA(50),TSARIO                                              
*                                                                               
         MVI   TSOFFACT,TSARDH     READ HIGH                                    
         LA    R0,TSARIO                                                        
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSERNF                                                    
         BZ    BLDPEX              RECORD FOUND EXIT                            
*--ADD RECORD TO TSAROFF                                                        
         MVI   TSOFFACT,TSAADD     ADD                                          
         MVC   TSARIO(50),WORKAREA                                              
         GOTO1 ATSAROFF                                                         
         DROP  R1                                                               
*                                                                               
*        GOTO1 ADDREC                                                           
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,NPKRLEN                                                     
         LA    RF,4(RF)                                                         
         STCM  RF,3,0(RE)                                                       
         L     R3,AIO1                                                          
         PUT   BJNYUNT,(R3)                                                     
*                                                                               
         L     RE,PKCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PKCOUNT                                                       
         CLC   PCOUNT,=F'100'                                                   
         BH    BLDPEX                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'PACK',0(R3),C'DUMP',300,=C'1D'                
         L     RE,PCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,PCOUNT                                                        
*                                                                               
BLDPEX   XIT                                                                    
         DROP  R3,R4                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    INITIALIZES ALL VALUES IN WORK SCRATCH SPACE ON THE                        
*      START-UP PASS                                                            
*                                                                               
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
*--LOAD CORE RESIDENT PHASES                                                    
         LA    R2,CORETAB                                                       
         LA    R0,CORES                                                         
         LA    R3,COREFACS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
PIN0100  MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB                                                     
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,PIN0100                                                       
*                                                                               
PIN0099  B     XIT                                                              
CORETAB  DS    0X                                                               
         DC    X'17327D'                                                        
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    VALIDATES AND CONVERTS THE DEMOS INTO 3 BYTE                               
*    FORMAT.  THEN COMPARES THE PACKAGE DEMOS TO THE                            
*    ESTIMATE RECORD DEMOS, AND REMOVES THE DEMOS THAT                          
*    ARE NOT ON BOTH LISTS.                                                     
*                                                                               
SETDEMO  NTR1                                                                   
*        L     R2,AIO1             A(TAPEIN)                                    
         LA    R2,AIOB             A(TAPEIN)                                    
         USING TAPEIN,R2           PACKAGE HEADER DSECT                         
*                                                                               
         XC    WORKAREA(200),WORKAREA                                           
*                                                                               
         LA    RE,WORKAREA+8                                                    
         LA    RF,TDEMO1                                                        
         LA    R1,19                                                            
*                                                                               
STDM020  CLI   0(RF),X'40'                                                      
         BNH   STDM100                                                          
         MVC   0(6,RE),0(RF)                                                    
         CLI   0(RE),C'P'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'V'          CHANGE PEOPLE TO VIEWERS                     
*                                                                               
         LA    R6,6                                                             
STDM040  CLI   0(RE),X'40'                                                      
         BNH   STDM060                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,STDM040                                                       
*                                                                               
STDM060  LA    RF,6(RF)                                                         
         CLI   0(RF),X'40'                                                      
         BNH   STDM100                                                          
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         BCT   R1,STDM020                                                       
*--CALCULATE NUMBER OF DEMO FIELDS                                              
STDM100  MVI   0(RE),X'FF'                                                      
*                                                                               
         LA    RE,7                                                             
         SR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    XIT                 NO DEMOS PROCESSED EXIT                      
*                                                                               
         STCM  RE,1,NFLDS                                                       
*                                                                               
*--CALCULATE THE LENGTH OF THE DEMOS                                            
         LA    RE,WORKAREA+8                                                    
         SR    RF,RF                                                            
STDM120  CLI   0(RE),X'FF'                                                      
         BE    STDM140                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     STDM120                                                          
*                                                                               
STDM140  STCM  RF,1,WORKAREA+5     FIELD LENGTH                                 
         LA    RF,8(RF)                                                         
         STCM  RF,1,WORKAREA                                                    
         MVI   0(RE),0                                                          
*                                                                               
*--CALL DEMOVAL CONVERT TO 3 BYTE FORMAT                                        
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
*                                                                               
         XC    TWDEMOS,TWDEMOS                                                  
         LA    R6,DBDEMOB                                                       
         USING DEMOD,R6                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=C'NTI'                                                   
         GOTO1 CDEMOVAL,DMCB,(0,WORKAREA),(7,TWDEMOS),(0,(R6))                  
*                                                                               
         MVC   TWDEMVE,TEIMPD1                                                  
         MVC   TWDEMVA,TAIMPD1                                                  
*                                                                               
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    FINDS A PROGRAM RECORD THAT MATCHES ON PROGRAM NAME,                       
*    PROGRAM ROTATION, AND PROGRAM TIME. IF A MATCH IS                          
*    THE ROUTINE CHECKS TO SEE THAT THE BUYS CAN FIT ON                         
*    THE PROGRAM. IF THE BUYS CAN FIT THE ROUTINE IS OVER.                      
*    IF THE BUYS CAN'T FIT A NEW PROGRAM RECORD IS CREATED.                     
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
CALCPROG NTR1                                                                   
*                                                                               
*        L    R4,AIO1                                                           
         LA   R4,AIOB                                                           
         USING TAPEIN,R4           INPUT TAPE DSECT                             
*--SAVE DAYPART                                                                 
         MVC   TWDYPT,TDAYPT                                                    
*--SAVE PROGRAM NAME                                                            
         MVC   TWPRNAME,TPROGNAM                                                
         MVC   TCOMCODE,TCOMMCD                                                 
         MVC   TCOMPOST,TCOMPOS                                                 
         MVC   TWBONUS,TBONUS                                                   
*--CALCULATE PROGRAM LENGTH                                                     
         MVI   MADERR,30                                                        
         GOTO1 CHKNUM,DMCB,(3,TLENGTH)                                          
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,TLENGTH                                                      
         CVB   RE,DUB                                                           
         STCM  RE,1,TWPLEN                                                      
*                                                                               
         CLI   TWPLEN,0            ZERO LENGTH INVALID                          
         BE    XIT                                                              
*--CALCULATE ACTUAL COST                                                        
         MVI   MADERR,31                                                        
         GOTO1 CHKNUM,DMCB,(9,TACTUAL)                                          
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,TACTUAL(9)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRACT                                                    
*--CALCULATE INTEGRATION COST                                                   
         MVI   MADERR,31                                                        
         GOTO1 CHKNUM,DMCB,(6,TINTEG)                                           
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,TINTEG(6)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRINT                                                    
*--CALCULATE HOMES IMPRESSION                                                   
         MVI   MADERR,32                                                        
         GOTO1 CHKNUM,DMCB,(6,TEIMPD1)                                          
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,TEIMPD1                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,TWHOMES                                                    
*                                                                               
*--VALIDATE THE PROGRAM DEMOS                                                   
         MVI   MADERR,33                                                        
         GOTO1 CHKNUM,DMCB,(36,TEIMPD2)                                         
         BNZ   XIT                                                              
*                                                                               
*--CALCULATE PROGRAM CODE                                                       
         MVI   TWPRCODE+3,X'FF'                                                 
         XC    TWPRCODE(3),TWPRCODE                                             
         LA    RE,TWPRNAME                                                      
         LA    RF,TWPRCODE                                                      
         LA    R1,16                                                            
*                                                                               
*--FIRST AREA CHECKS FOR BYPASS NAMES                                           
CALCP030 LA    R4,NAMETAB                                                       
CALCP050 CLI   0(R4),X'FF'         END OF NAME TABLE                            
         BE    CALCP100            YES CHECK FOR VOWELS                         
         ZIC   R6,8(R4)            MOVE IN LENGTH OF COMPARE                    
         EX    R6,NAMECOMP                                                      
         BNE   CALCP070                                                         
         LA    R6,1(R6)                                                         
         AR    RE,R6               BUMP PROGRAM NAME TO NEXT WORD               
         SR    R1,R6               SUBTRACT LOOP COUNT BY WORD BUMP             
         LTR   R1,R1                                                            
         BZ    CALCP260            END OF LITERAL EXIT ROUTINE                  
         BP    CALCP030            CHECK NEXT WORD                              
         DC    H'0'                WENT PAST LITERAL                            
CALCP070 LA    R4,9(R4)                                                         
         B     CALCP050                                                         
*                                                                               
*--THIS AREA CHECKS FOR TO BYPASS VOWELS AND SPECIAL VALUES                     
CALCP100 LA    R4,VOWELLST                                                      
CALCP120 CLI   0(RE),X'40'         CHECK FOR WORD BREAK                         
         BNE   CALCP140                                                         
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCTR  R1,0                SUBTRACT FROM COUNT                          
         LTR   R1,R1                                                            
         BZ    CALCP260            END OF LITERAL EXIT ROUTINE                  
         B     CALCP030            CHECK AGAINST WORD TABLE                     
*                                                                               
CALCP140 CLI   0(RE),X'C1'         CHECK CHECK CHARACTER FOR ALPHA/NUM          
         BL    CALCP180                                                         
         CLI   0(RE),X'F9'                                                      
         BH    CALCP180                                                         
*                                                                               
CALCP160 CLI   0(R4),X'FF'         END OF VOWEL TABLE                           
         BE    CALCP200            MOVE LETTER TO CODE                          
         CLC   0(1,RE),0(R4)                                                    
         BE    CALCP180            LETTER IS A VOWEL, BYPASS                    
         LA    R4,1(R4)                                                         
         B     CALCP160                                                         
*                                                                               
CALCP180 LA    RE,1(RE)                                                         
         BCT   R1,CALCP100                                                      
         B     CALCP260                                                         
*                                                                               
CALCP200 CLI   0(RF),X'FF'                                                      
         BE    CALCP260            CODE FIELD IS FILLED                         
         MVC   0(1,RF),0(RE)       MOVE NEXT LETTER OF CODE IN                  
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CALCP100                                                      
*                                                                               
*--THE LITERAL PORTION OF THE PROGRAM CODE MUST BE                              
*--2 POSITIONS LONG. IF IT IS NOT AN ERROR CODE IS                              
*--AND THE PROGRAM BYPASSES THE UNITS UNDER THAT CODE.                          
*--SPECIAL BYPASS CODE                                                          
CALCP260 CLC   TWPRCODE(3),=CL3'TWN'                                            
         BNE   *+10                                                             
         MVC   TWPRCODE(3),=CL3'TWW'                                            
         CLC   TWPRCODE(3),=CL3'LCS'                                            
         BNE   *+10                                                             
         MVC   TWPRCODE(3),=CL3'LCC'                                            
         CLC   TWPRNAME(3),=CL3'M-F'                                            
         BNE   *+10                                                             
         MVC   TWPRCODE(3),=CL3'MFF'                                            
CALCP270 MVC   TWPRCODE+3(3),=XL3'404040'                                       
         MVI   TWPRCODE+2,C'?'                                                  
         CLI   TWPRCODE+1,X'00'                                                 
         BNE   CALCP280                                                         
         MVI   MADERR,34           INVALID PROGRAM NAME                         
         B     XIT                                                              
*                                                                               
CALCP280 MVC   TWPRCODE+3(3),=XL3'F0F0F1'                                       
         CLI   TWPRCODE+2,0                                                     
         BNE   CALCP300                                                         
         MVI   TWPRCODE+2,X'F0'                                                 
*                                                                               
*--CONVERT THE MAD RECORDS ROTATION INTO PROGRAM FORMAT                         
CALCP300 XC    TWPRROT(4),TWPRROT  CLEAR THE DAY FIELDS                         
*        L     R4,AIO1                                                          
         LA    R4,AIOB                                                          
*                                                                               
         LA    RE,TDAY                                                          
         LA    R1,ROTTABLE                                                      
CALCP320 CLI   0(R1),X'FF'                                                      
         BE    CALCP360                                                         
         CLC   0(2,RE),0(R1)                                                    
         BNE   CALCP340                                                         
         MVC   TWPRROT,2(R1)                                                    
         NI    TWPRROTN,X'F0'      CLEAR END DAT NUMBER                         
         OC    TWPRROTN,5(R1)      END DAY NUMBER                               
*                                                                               
         NI    TWPRROTN,X'0F'      CLEAR START DAY NUMBER                       
         OC    TWPRROTN,4(R1)      START DAY NUMBER                             
         MVC   TWDAYNO,5(R1)      START DAY NUMBER (NUMERIC)                    
         MVC   TWDAYHEX,2(R1)     START DAY NUMBER (HEX)                        
CALCP340 LA    R1,6(R1)                                                         
         B     CALCP320                                                         
*                                                                               
*--CONVERT THE START AND END TIMES TO BINARY                                    
*                                                                               
CALCP360 MVI   MADERR,35                                                        
         GOTO1 CHKNUM,DMCB,(4,TSTIME)                                           
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB(8),TSTIME(4)  START TIME                                     
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRSTIM                                                    
*                                                                               
         MVI   MADERR,35                                                        
         GOTO1 CHKNUM,DMCB,(4,TETIME)                                           
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB(8),TETIME(4)  END TIME                                       
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRETIM                                                    
*-CHECK START AND END FOR GREATER THEN 2400                                     
         MVI  MADERR,36                                                         
         CLC  TWPRETIM,=XL2'0960'                                               
         BH   XIT                                                               
*                                                                               
         CLC  TWPRSTIM,=XL2'0960'                                               
         BH   XIT                                                               
*-CHECK START AND END FOR ZERO                                                  
         MVI  MADERR,37                                                         
         OC   TWPRETIM,TWPRETIM                                                 
         BZ   XIT                                                               
*                                                                               
         OC   TWPRSTIM,TWPRSTIM                                                 
         BZ   XIT                                                               
*--GET START QUARTER HOUR                                                       
         BAS   RE,GETSQH                                                        
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         BAS   RE,RDTSPROG         READ HIGH CALL FOR PROGRAM                   
         CLC   TSARIO(8),WORKAREA                                               
         BE    CALCP440                                                         
         B     CALCP540                                                         
*                                                                               
CALCP400 LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         BAS   RE,RDTSPROG         READ HIGH CALL FOR PROGRAM                   
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF                                                         
         LA    R1,TSARBLCK                                                      
*                                                                               
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    CALCP500                                                         
         CLC   WORKAREA(8),TSARIO  RECORD MATCH                                 
         BNE   CALCP500                                                         
*                                                                               
         MVC   WORKAREA(50),TSARIO                                              
         MVC   WORKAREA+50(50),TSARBLCK                                         
         DROP  R1                                                               
*                                                                               
CALCP440 MVC   TWPRCODE+3(3),TSARIO+8   LINE COUNT NUMBER                       
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, DAY,ROTATION TO SEE IF                      
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
*        L     R3,AIO                                                           
*        USING NPGRECD,R3                                                       
*        GOTO1 GETREC                                                           
*                                                                               
*        L     R6,AIO                                                           
*        MVI   ELCODE,X'92'                                                     
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING NPGEL92,R6                                                       
*                                                                               
         CLC   TPROGNAM(16),TSARIO+34                                           
         BNE   CALCP400                                                         
                                                                                
         CLC   TDAY,TSARIO+24                                                   
         BNE   CALCP400                                                         
                                                                                
         CLC   TSTIME(8),TSARIO+26                                              
         BNE   CALCP400                                                         
         B     CALCP700            YES VALID PROGRAM RECORD FOUND               
                                                                                
*        DROP  R3,R6                                                            
*                                                                               
*--CREATE A NEW PROGAM RECORD                                                   
*--TAKE LAST PROGRAM LINE COUNT IN FULL, ADD 1                                  
*--RELOAD BACK INTO FULL USE THAT AS THE NEW                                    
*--PROGRAM LINE COUNT.                                                          
*                                                                               
CALCP500 BAS   RE,SPOTSET                                                       
         PACK  DUB(2),TWPRCODE+3(3) CREATE A NEW PROGRAM LINE COUNT             
         AP    DUB(2),=PL1'1'                                                   
         UNPK  TWPRCODE+3(3),DUB(2)                                             
         OI    TWPRCODE+5,X'F0'                                                 
*                                                                               
*--BUILD NEW PROGRAM CODE                                                       
CALCP540 LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVC   TSARIO(50),WORKAREA       RESTORE TSAR BUFFER                    
         MVC   TSARIO+8(3),TWPRCODE+3                                           
         MVC   TSARIO+24(2),TDAY                                                
         MVC   TSARIO+26(8),TSTIME                                              
         MVC   TSARIO+34(16),TPROGNAM                                           
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF                                                         
         DROP  R1                                                               
*                                                                               
         L     R3,AIO1                                                          
         XCEF  (R3),2000                                                        
         LA    R3,4(R3)                                                         
         USING NPGRECD,R3                                                       
*                                                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG,TWPRCODE                                                
         MVC   NPGKEND,=XL2'C79F'  DEC31/99                                     
*                                                                               
         MVC   NPGRLEN,=XL2'0021'                                               
*                                                                               
         MVC   NPGMAINL(2),=XL2'0108'                                           
         GOTO1 DATCON,DMCB,(5,0),(3,NPGACTD)                                    
         MVI   NPGACT,C'A'                                                      
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKFMS(3),=CL3'EVN'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO1            ADD X'5D' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE '),RECBUFF,WORKAREA                  
*                                                                               
*--BUILD 77 ELEMENT (TELL SYSTEM RECORD CREATED IN TRANSFER)                    
         XC    WORKAREA(70),WORKAREA                                            
*                                                                               
         MVC   WORKAREA(3),=XL3'770340'                                         
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE '),RECBUFF,WORKAREA                  
*                                                                               
*                                                                               
*--BUILD 92 ELEMENT                                                             
         XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NPGEL92,RE                                                       
*                                                                               
         MVC   NPGELEM(2),=XL2'9250'                                            
         MVI   NPGDAY,X'7F'                                                     
         MVC   NPGTIME,TWPRSTIM                                                 
         MVC   NPGNAME,TWPRNAME                                                 
         MVC   NPGROT,TWPRROT                                                   
         MVC   NPGROTNO,TWPRROTN                                                
         MVI   NPGDAYNO,X'17'                                                   
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE '),RECBUFF,WORKAREA                  
*--WRITE THE RECORD OUT                                                         
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,NPGRLEN                                                     
         LA    RF,4(RF)                                                         
         STCM  RF,3,0(RE)                                                       
         L     R3,AIO1                                                          
         PUT   BJNYSPT,(R3)                                                     
*                                                                               
*        GOTO1 ADDREC                                                           
         L     RE,PRCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PRCOUNT                                                       
         CLC   RCOUNT,=F'100'                                                   
         BH    CALCPEX                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'PROG',0(R3),C'DUMP',300,=C'1D'                
         L     RE,RCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RCOUNT                                                        
*                                                                               
         B     CALCPEX                                                          
*                                                                               
*--CHECK TO SEE THAT ALL UNITS REQUESTED HAVE THE                               
*--ROOM TO BE ADDED UNDER THE PROGRAM.                                          
CALCP700 BAS   RE,UNITSET          SET UP TO READ UNIT FILE                     
*                                                                               
         LA    R3,KEY                                                           
         USING NURECD,R3                                                        
*                                                                               
CALCP720 CLI   0(R4),X'40'                                                      
         BNH   CALCPEX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
         MVC   NUKPEST,TWEST                                                    
*-SET DATE TO FIRST DAY OF ROTATION                                             
         MVC   DUB(6),TDATE                                                     
         ZIC   R6,TWDAYNO                                                       
         BCTR  R6,0                                                             
         GOTO1 ADDAY,DMCB,DUB,DUB,(R6)                                          
*-CONVERT THE WEEK DATE                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(2,NUKPDATE)                                 
*-CONVERT THE LINE NUMBER                                                       
         MVI   NUKPSUB,192                                                      
*-READ UNIT 84 KEY IF FOUND THIS MEANS THERE IS                                 
*-NOT ENOUGH ROOM ON THE PROGRAM TO ADD THE UNITS.                              
*-AND A NEW PROGRAM RECORD WILL BE CREATED.                                     
CALCP760 STCM  RF,1,NUKPSUB                                                     
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSARIO,1                                                         
         MVC   TSARIO+1(20),NUKPTYPE                                            
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSERNF                                                    
         BZ    CALCP800                                                         
         B     CALCPEX                                                          
         DROP  R1                                                               
*                                                                               
*-NO ROOM ON THIS PROGRAM TO ADD THE UNITS                                      
*-MUST GO BACK AND FIND ANOTHER PROGRAM TO                                      
*-ADD THESE UNITS TO.                                                           
CALCP800 B     CALCP400                                                         
*                                                                               
*-ALL UNITS PROGRAMS VALID XIT                                                  
CALCPEX  MVI   MADERR,0                                                         
         B     XIT                                                              
*                                                                               
NAMECOMP CLC   0(0,RE),0(R4)       FOR TEST ON NAME TABLE                       
         DROP  R3                                                               
*                                                                               
NAMETAB  DC    CL8'THE     ',XL1'03'                                            
         DC    CL8'NFL     ',XL1'03'                                            
         DC    X'FF'                                                            
VOWELLST DC    CL5'AEIOU'                                                       
         DC    X'FF'                                                            
ROTTABLE DC    CL2'MO',XL4'40401001'         MON                                
         DC    CL2'TU',XL4'20202002'         TUE                                
         DC    CL2'WE',XL4'10103003'         WED                                
         DC    CL2'TH',XL4'08084004'         THU                                
         DC    CL2'FR',XL4'04045005'         FRI                                
         DC    CL2'SA',XL4'02026006'         SAT                                
         DC    CL2'SU',XL4'01017007'         SUN                                
         DC    CL2'MF',XL4'7C041005'         MON-FRI                            
         DC    CL2'WK',XL4'7F041007'         MON-SUN                            
         DC    X'FF'                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS THE BASIC INFORMATION NEEDED TO BUILD                               
*    A UNIT RECORD.                                                             
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
BLDUNIT  NTR1                                                                   
*                                                                               
*                                                                               
         L     R3,AIO1                                                          
         XCEF  (R3),2000                                                        
         LA    R3,4(R3)                                                         
         USING NURECD,R3                                                        
         BAS   RE,UNITSET          SET UP TO READ UNIT FILE                     
*--04 KEY                                                                       
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,TWAGYMED                                                   
         MVC   NUKCLT,TWCLIENT                                                  
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKNET,TWNET                                                     
         MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUKEST,TWEST                                                     
         MVI   NUKDP,C'P'                                                       
*                                                                               
         MVI   NURLEN+1,X'6C'      RECORD LENGTH 108                            
*--SET 01 ELEMENT                                                               
         MVC   NUMAINEL(2),=XL2'0150'                                           
         MVC   NUPACK,TWPKG                                                     
*                                                                               
         CLI   TWBONUS,C'B'                                                     
         BNE   *+8                                                              
         OI    NUUNITST,X'04'                                                   
         MVC   NUPROGNM,TWPRNAME                                                
         MVC   NUUNCODE,TWUNIV                                                  
         MVC   NUHUTTYP,TWHUTTYP                                                
         MVC   NUPRD,TWPRCD                                                     
         MVC   NULEN,TWPLEN                                                     
         MVC   NUDAY,TWDAYHEX                                                   
         MVC   NUTIME,TWPRSTIM                                                  
         MVC   NUACTUAL,TWPRACT                                                 
         MVC   NUINTEG,TWPRINT                                                  
BLDUN040 OI    NUACTWHY,X'80'       NEW BUY                                     
         MVC   NUMARKET,TWMKT                                                   
         MVC   NUALPHA,TWAAGY                                                   
*                                                                               
*--BUILD 02 ELEMENT                                                             
         XC    WORKAREA(50),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUSDRD,RE                                                        
*                                                                               
         MVC   NUSDREL(2),=XL2'0214'                                            
         MVC   NUSTATYP,TWMEDTYP                                                
         MVC   NUSDROT,TWPRROT                                                  
         MVC   NUPOSTYP,TWPSTTYP                                                
*                                                                               
         MVC   AIO,AIO1            ADD X'02' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RE                                                               
*                                                                               
*--BUILD 21 ELEMENT (FOR TRAFFIC)                                               
BLDUN100 XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUCMLEL,RE                                                       
*                                                                               
         MVC   NUCMLEID(2),=XL2'2134'                                           
         CLC   TCOMCODE,=8X'40'                                                 
         BE    *+10                                                             
         MVC   NUCML1,TCOMCODE                                                  
         CLC   TCOMPOST,=3X'40'                                                 
         BE    *+10                                                             
         MVC   NUCMLPOS+1(3),TCOMPOST                                           
         MVI   NUCMLPOS,X'40'                                                   
*                                                                               
         MVC   AIO,AIO1            ADD X'21' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RE                                                               
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         MVC   NUBKFMS(3),=CL3'EIN'                                             
*                                                                               
         MVC   AIO,AIO1            ADD X'5D' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RE                                                               
*                                                                               
*--BUILD 60 ELEMENT SUB DAYPART                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUOTH,RE                                                         
         LA    RF,DPTTAB           POINT RE AT DAYPART TABLE                    
         LA    R0,DAYPARTS         COUNTER                                      
BLDUN180 CLC   TWDYPT,0(RF)        TEST INPUT AGAINST TABLE                     
         BE    BLDUN200                                                         
         LA    RF,L'DPTTAB(RF)                                                  
         BCT   R0,BLDUN180                                                      
         DC    H'0'                                                             
*                                                                               
BLDUN200 MVI   NUOTEL,X'60'                                                     
         MVC   NUOTLEN,9(RF)                                                    
         MVI   NUOTTYP,C'D'                                                     
         MVC   NUOTHER(9),0(RF)                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RE                                                               
*                                                                               
*                                                                               
*--BUILD 77 ELEMENT (TELL SYSTEM RECORD CREATED IN TRANSFER)                    
         XC    WORKAREA(70),WORKAREA                                            
*                                                                               
         MVC   WORKAREA(3),=XL3'770340'                                         
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*                                                                               
*--BUILD 99 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUACTD,RE                                                        
*                                                                               
         MVC   NUACTEL(2),=XL2'990C'                                            
         GOTO1 DATCON,DMCB,(5,0),(3,NUACTADT)                                   
*                                                                               
         MVC   AIO,AIO1            ADD X'99' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RE                                                               
*                                                                               
*--BUILD DD ELEMENT                                                             
         XC    WORKAREA(50),WORKAREA                                            
         USING NUOVD,RE                                                         
         LA    R4,TWDEMOS                                                       
         LA    R2,TWDEMVE                                                       
         LA    R6,7                                                             
*                                                                               
BLDUN300 LA    RE,WORKAREA                                                      
         XC    WORKAREA(50),WORKAREA                                            
         CLI   0(R4),X'FF'         END OF LIST                                  
         BE    BLDUN380                                                         
*                                                                               
         MVC   NUOVEL(2),=XL2'DD0C'                                             
         CLI   1(R4),0             SEE IF DEMO SHOULD BE USED                   
         BE    BLDUN360                                                         
         PACK  DUB,0(6,R2)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,NUOVVAL       MOVE OUT THE VALUE                           
         MVC   NUOVCAT,0(R4)       THE CATEGORY NUMBER                          
         CLI   NUOVCAT,0           CHECK FOR NAD                                
         BE    *+8                                                              
         OI    NUOVFLG,X'80'                                                    
         MVC   NUOVNUM,2(R4)       DEMO NUMBER                                  
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'42'                                                    
         CLI   TWPSTTYP,C'C'                                                    
         BE    BLDUN340                                                         
         CLI   TWPSTTYP,C'O'                                                    
         BE    BLDUN340                                                         
         MVI   NUOVPRE,X'43'                                                    
*                                                                               
BLDUN340 MVC   AIO,AIO1            ADD X'DD' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*        BAS   RE,BLDIMP                                                        
*                                                                               
BLDUN360 LA    R4,3(R4)                                                         
         LA    R2,6(R2)                                                         
         BCT   R6,BLDUN300                                                      
*                                                                               
*--BUILD DE ELEMENT                                                             
BLDUN380 XC    WORKAREA(50),WORKAREA                                            
         USING NUOVD,RE                                                         
         LA    R4,TWDEMOS                                                       
         LA    R2,TWDEMVA                                                       
         LA    R6,7                                                             
*                                                                               
BLDUN400 LA    RE,WORKAREA                                                      
         XC    WORKAREA(50),WORKAREA                                            
         CLI   0(R4),X'FF'         END OF LIST                                  
         BE    BLDUN600                                                         
*                                                                               
         MVC   NUOVEL(2),=XL2'DE0C'                                             
         CLI   1(R4),0             SEE IF DEMO SHOULD BE USED                   
         BE    BLDUN460                                                         
         PACK  DUB,0(6,R2)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,NUOVVAL       MOVE OUT THE VALUE                           
         MVC   NUOVCAT,0(R4)       THE CATEGORY NUMBER                          
         CLI   NUOVCAT,0           CHECK FOR NAD                                
         BE    *+8                                                              
         OI    NUOVFLG,X'80'                                                    
         MVC   NUOVNUM,2(R4)       DEMO NUMBER                                  
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'42'                                                    
         CLI   TWPSTTYP,C'C'                                                    
         BE    BLDUN440                                                         
         CLI   TWPSTTYP,C'O'                                                    
         BE    BLDUN440                                                         
         MVI   NUOVPRE,X'43'                                                    
*                                                                               
BLDUN440 MVC   AIO,AIO1            ADD X'DE' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*        BAS   RE,BLDIMP                                                        
*                                                                               
BLDUN460 LA    R4,3(R4)                                                         
         LA    R2,6(R2)                                                         
         BCT   R6,BLDUN400                                                      
*                                                                               
BLDUN600 B     XIT                                                              
         DROP  RE,R3                                                            
*                                                                               
DPTTAB   DS    0CL10       BYTE 9 = LENGTH OF ENTRY PLUS 1                      
         DC    CL9'DDAYTIME',XL1'0B'                                            
         DC    CL9'FFRINGE',XL1'0A'                                             
         DC    CL9'PPRIME',XL1'09'                                              
         DC    CL9'KKIDS',XL1'08'                                               
         DC    CL9'TTEENS',XL1'09'                                              
         DC    CL9'YYOUTH',XL1'09'                                              
         DC    CL9'SSPORTS',XL1'0A'                                             
         DC    CL9'NNEWS',XL1'08'                                               
         DC    CL9'EEARLY',XL1'09'                                              
         DC    CL9'LLATE',XL1'08'                                               
         DC    CL9'CCABLE',XL1'09'                                              
         DC    CL9'OOLYMPICS',XL1'0C'                                           
         DC    CL9'RRADIO',XL1'09'                                              
         DC    CL9'XSYND.',XL1'09'                                              
         DC    CL9'ISPECIAL',XL1'0B'                                            
         DC    CL9'VOVERNITE',XL1'0C'                                           
         DC    CL9'WWKNDPM',XL1'0A'                                             
         DC    CL9'MWKNDAM',XL1'0A'                                             
         DC    CL9'AACCESS',XL1'0A'                                             
         DC    CL9'BCBLSPORT',XL1'0C'                                           
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS A CORRESPONDING IMPRESSION OVERRIDE ELEMENT                         
*    FOR EVERY VPH ELEMENT.                                                     
*                                                                               
*   INPUT WORKAREA = VPH OVERIDE ELEMENT                                        
*                                                                               
BLDRTG   NTR1                                                                   
         LA    RE,WORKAREA                                                      
         USING NUOVD,RE                                                         
*                                                                               
         CLI   NUOVMOD,C'V'        CHECK FOR VPH                                
         BNE   BLDIMPEX                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,TWHOMES                                                    
         ICM   RF,15,NUOVVAL                                                    
*                                                                               
         MR    R0,RF               HOMES*VPH                                    
         M     R0,=F'10'                                                        
         D     R0,=F'1000'                                                      
         A     R1,=F'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
         STCM  R1,15,NUOVVAL                                                    
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'42'                                                    
*                                                                               
         MVC   AIO,AIO1            ADD X'DD' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*                                                                               
BLDIMPEX B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    FINDS THE UNIT WITH THE HIGHEST LINE NUMBER AND                            
*    STARTS ADDING A UNIT RECORDS THEREAFTER. IT WILL                           
*    SET AN INDICATOR SAYING THE UNIT WAS ADDED THROUGH                         
*    THE UPLOAD.                                                                
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
ADDUNIT  NTR1                                                                   
*        L     R4,AIO1                                                          
         LA    R4,AIOB                                                          
         USING TAPEIN,R4                                                        
*                                                                               
ADDUN050 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NURECD,R3                                                        
*--BUILD THE BUY KEY                                                            
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,TDATE),(2,NUKPDATE)                               
         MVC   TWPDATE,NUKPDATE    SAVE THE BUY DATE                            
*                                                                               
         MVC   NUKPEST,TWEST                                                    
         MVI   NUKPSUB,1                                                        
*                                                                               
         L     R3,RECBUFF                                                       
*--CREATE UNIVERSE ELEMENT                                                      
         BAS   RE,BLDUNIV                                                       
         MVC   AIO,AIO1            ADD X'31' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
*--BUILD 35 ELEMENT (FOR VPH)                                                   
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUEHD,RF                                                         
*                                                                               
         MVC   NUEHEL(3),=XL3'350902'                                           
*                                                                               
         PACK  DUB,TERATING(3)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,3,NUEHRTG                                                     
         PACK  DUB,TEHUTS(2)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,NUEHHUT                                                     
         PACK  DUB,TESHARE(2)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,3,NUEHSHR                                                     
*                                                                               
         MVC   AIO,AIO1            ADD X'35' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RF                                                               
*--BUILD 35 ELEMENT (FOR VPH)                                                   
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUAHD,RF                                                         
*                                                                               
         MVC   NUAHEL(2),=XL2'4509'                                             
*                                                                               
         PACK  DUB,TARATING(3)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,3,NUAHOMES                                                    
         PACK  DUB,TAHUTS(2)                                                    
         CVB   RE,DUB                                                           
         STCM  RE,3,NUAHOMES+2                                                  
         PACK  DUB,TASHARE(2)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,3,NUAHOMES+4                                                  
*                                                                               
         MVC   AIO,AIO1            ADD X'45' ELEMENT                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),RECBUFF,WORKAREA                  
         DROP  RF                                                               
*                                                                               
*                                                                               
ADDUN060 MVI   TWLNCT,0                                                         
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSARIO,1                                                         
         MVC   TSARIO+1(20),KEY                                                 
         MVC   WORKAREA(50),TSARIO                                              
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF                                                         
         B     ADDUN100                                                         
*                                                                               
ADDUN070 MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF                                                         
*                                                                               
ADDUN100 CLC   TSARIO(18),WORKAREA                                              
         BNE   ADDUN140                                                         
ADDUN120 MVC   TWLNCT,TSARIO+18                                                 
         B     ADDUN070                                                         
*                                                                               
*--GET NEXT UNUSED SUB-LINE NUMBER                                              
ADDUN140 ZIC   RE,TWLNCT                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,1,TWLNCT                                                      
         L     R3,RECBUFF                                                       
         MVC   NUKDATE,TWPDATE                                                  
         MVC   NUKSUB,TWLNCT                                                    
         CLI   TWLNCT,1                                                         
         BE    *+10                                                             
         MVC   NUSUBPRT,TWLNCT                                                  
*--WRITE RECORD TO TSAR                                                         
         MVC   TSARIO(50),WORKAREA                                              
         MVC   TSARIO+18(1),TWLNCT                                              
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF                                                         
         DROP  R1                                                               
*--SET TO HANDLE UNIT DATA                                                      
         BAS   RE,UNITSET                                                       
*--WRITE THE UNIT                                                               
*--R3 POINTS TO AIO1                                                            
************************************                                            
         CLC   SAVCLI,SPACES                                                    
         BNH   ADDUN180                                                         
         CLC   TCLIENT,SAVCLI                                                   
         BE    ADDUN200                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'CLIENT',SAVCLI,C'DUMP',3,=C'1D'               
         GOTO1 =V(PRNTBL),DMCB,=C'CLCN',CLCOUNT,C'DUMP',4,=C'1D'                
         XC    CLCOUNT,CLCOUNT                                                  
ADDUN180 MVC   SAVCLI,TCLIENT                                                   
*                                                                               
*        BAS   RE,WRITBREC                                                      
ADDUN200 L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,NURLEN                                                      
         LA    RF,4(RF)                                                         
         STCM  RF,3,0(RE)                                                       
         L     R3,AIO1                                                          
         PUT   BJNYUNT,(R3)                                                     
*                                                                               
         L     RE,UNCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,UNCOUNT                                                       
         L     RE,CLCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,CLCOUNT                                                       
                                                                                
         CLC   UCOUNT,=F'100'                                                   
         BH    ADDUNEX                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'UNIT',0(R3),C'DUMP',600,=C'1D'                
         L     RE,UCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,UCOUNT                                                        
*                                                                               
ADDUNEX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*-THIS ROUTINE SETS UP THE SPOTFILE                                             
*-SETS UP THE PROGRAM KEY DOES A READ                                           
*-HIGH FOR THE PROGRAM RECORD.                                                  
READPROG NTR1                                                                   
         BAS   RE,SPOTSET          SETUP TO READ SPOT FILE                      
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG(6),TWPRCODE                                             
*                                                                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*-THIS ROUTINE SETS UP THE SPOTFILE                                             
*-SETS UP THE PROGRAM KEY DOES A READ                                           
*-HIGH FOR THE PROGRAM RECORD.                                                  
RDTSPROG NTR1                                                                   
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         LA    R3,AIOB                                                          
         USING TAPEIN,R3                                                        
         XC    TSARIO,TSARIO                                                    
         MVI   TSARIO,2            PROGRAM AREA                                 
         MVC   TSARIO+1(2),TWMKT                                                
         MVC   TSARIO+5(6),TWPRCODE                                             
         MVC   WORKAREA(50),TSARIO                                              
         MVC   WORKAREA+50(50),TSARBLCK                                         
*                                                                               
         MVI   TSOFFACT,TSARDH     READ HIGH                                    
         LA    R0,TSARIO                                                        
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF                                                         
*                                                                               
         B     XIT                                                              
         DROP  R1,R3                                                            
         EJECT                                                                  
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
         GETEL (R6),NBDTADSP,ELCODE                                             
         SPACE 3                                                                
*                                                                               
HEADSPC  SSPEC H1,46,C'BJNY ERROR REPORT'                                       
         SSPEC H2,46,C'_________________'                                       
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*--THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                              
*                                                                               
* ITBREC NTR1                                                                   
*                                                                               
*        OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*        MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
*                                                                               
*        BAS   RE,BLDKEY84         BUILD 84 KEY                                 
*        GOTO1 HIGH                                                             
*                                  IF RECORD ISN'T ALREADY THERE                
*        CLC   KEY(20),KEYSAVE                                                  
*        BE    WBR10                                                            
*                                                                               
*        MVC   AIO,AIO1            THEN ADD IT                                  
*        GOTO1 ADDREC                                                           
*        L     RE,UNCOUNT                                                       
*        LA    RE,1(RE)                                                         
*        ST    RE,UNCOUNT                                                       
*        L     RE,CLCOUNT                                                       
*        LA    RE,1(RE)                                                         
*        ST    RE,CLCOUNT                                                       
*        MVC   TWDSKADR,KEY        SAVE THE DISK ADDRESS                        
*        MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                                                               
*        BAS   RE,BLDKEY84         BUILD 84 KEY                                 
*        MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
*        GOTO1 =V(PRNTBL),DMCB,=C'K84',KEY,C'DUMP',28,=C'1D'                    
*        GOTO1 ADD                                                              
*                                                                               
*        BAS   RE,BLDKEY94         BUILD 94 KEY                                 
*        MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
*        GOTO1 =V(PRNTBL),DMCB,=C'K94',KEY,C'DUMP',28,=C'1D'                    
*        GOTO1 ADD                                                              
*                                                                               
*        B     WBRX                                                             
*                                                                               
* R10    MVC   AIO,AIO3                                                         
*        TM    KEY+20,X'80'        IF DELETED                                   
*        BZ    WBR20                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
*        NI    KEY+20,X'FF'-X'83'                                               
*        OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
*        GOTO1 WRITE                                                            
*                                                                               
* R20    BAS   RE,BLDKEY84                                                      
*        GOTO1 HIGH                                                             
*        CLC   KEY(20),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        TM    KEY+20,X'80'        IF DELETED                                   
*        BZ    WBR30                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
*        NI    KEY+20,X'FF'-X'83'                                               
*        OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
*        GOTO1 WRITE                                                            
*                                                                               
* R30    BAS   RE,BLDKEY94                                                      
*        GOTO1 HIGH                                                             
*        CLC   KEY(20),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        TM    KEY+20,X'80'        IF DELETED                                   
*        BZ    WBR40                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
*        NI    KEY+20,X'FF'-X'83'                                               
*        OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
*        GOTO1 WRITE                                                            
*                                                                               
* R40    GOTO1 GETREC              READ OLD RECORD                              
*                                                                               
*        MVC   AIO,AIO1            WRITE OUR RECORD IN ITS PLACE                
*        GOTO1 PUTREC                                                           
*        L     RE,UNCOUNT                                                       
*        LA    RE,1(RE)                                                         
*        ST    RE,UNCOUNT                                                       
*        L     RE,CLCOUNT                                                       
*        LA    RE,1(RE)                                                         
*        ST    RE,CLCOUNT                                                       
*        MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                  DON'T READ DELETED RECORDS                   
* RX     NI    DMINBTS,X'FF'-X'08'                                              
*        MVI   RDUPDATE,C'N'       RESET READ FOR UPDATE                        
*        B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
*--THIS ROUTINE BUILDS THE UNIVERSE ELEMENT                                     
*--ELEMENT RETURNED IN WORKAREA                                                 
*                                                                               
BLDUNIV  NTR1                                                                   
*                                                                               
         L     R3,AIO1                                                          
         USING NURECD,R3                                                        
*                                                                               
         LA    R2,DBDEMOB                                                       
         USING GUVD,R2                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,=CL2'BJ'                                                  
         MVC   GUVCODE,TWUNIV                                                   
         OC    GUVCODE,GUVCODE     TEST FOR UNIVERSE CODE                       
         BNZ   *+10                HAVE ONE                                     
         MVC   GUVDATE,TWPDATE     ELSE USE AIR DATE                            
         XC    WORKAREA(200),WORKAREA                                           
         LA    R3,WORKAREA                                                      
         ST    R3,GUVAOUT          OUTPUT ELEMENT ADDRESS                       
         L     R0,ANETWS3                                                       
         ST    R0,GUVAREC          SET AND CLEAR AREA FOR UNIV. RECORD          
         LA    R1,2000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVNETWK,VNETWEEK                                                
         NI    NUUNST2,X'FF'-X'08' TURN OFF CABLE UNIV                          
         GOTO1 VGETNUN,DMCB,(R2)                                                
         CLI   GUVERROR,0          TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    NUUNST2,X'08'                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--THIS ROUTINE READS THE CLIENT AND RETRIEVES THE                              
*--PRODUCT CODE                                                                 
*                                                                               
GETPROD  NTR1                                                                   
         MVI   TWPRCD,0                                                         
         BAS   RE,SPOTSET                                                       
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         USING CLTREC,R3                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,TWAGYMED                                                  
         MVC   CKEYCLT,TWCLIENT                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    RE,CLIST                                                         
         LA    RF,220                                                           
*                                                                               
GETPR100 CLC   TWPROD,0(RE)                                                     
         BE    GETPR150                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,GETPR100                                                      
         B     XIT                                                              
*                                                                               
GETPR150 MVC   TWPRCD,3(RE)                                                     
         B     XIT                                                              
*                                                                               
*-SUB-ROUTINE TO CHECK CHARACTERS FOR NUMERIC                                   
*                                                                               
CHKNUM   NTR1                                                                   
         ZIC   R4,0(R1)            NUMBER OF BYTES                              
         L     R6,0(R1)            ADDRESS OF LOCATION                          
         LTR   R4,R4                                                            
         BZ    CHKNERR                                                          
*                                                                               
CHKN020  CLI   0(R6),C'0'                                                       
         BL    CHKNERR                                                          
         CLI   0(R6),C'9'                                                       
         BH    CHKNERR                                                          
         LA    R6,1(R6)                                                         
         BCT   R4,CHKN020                                                       
*                                                                               
         SR    R6,R6               SET ZERO RETURN CODE                         
*                                                                               
CHKNERR  LTR   R6,R6                                                            
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETSQH   NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,TWPRSTIM       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,TWQTRHR                                                       
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD THE 84 KEY                                               
*                                                                               
* DKEY84 NTR1                                                                   
*        XC    KEY,KEY                                                          
*        LA    RE,KEY                                                           
*        USING NURECD,RE                                                        
*        MVI   NUKPTYPE,X'84'                                                   
*        MVC   NUKPAM,TWAGYMED                                                  
*        MVC   NUKPCLT,TWCLIENT                                                 
*        MVC   NUKPNET,TWNET                                                    
*        MVC   NUKPPROG,TWPRCODE                                                
*        MVC   NUKPDATE,TWPDATE                                                 
*        MVC   NUKPEST,TWEST                                                    
*        MVC   NUKPSUB,TWLNCT                                                   
*        MVI   NUKPDP,C'P'                                                      
*        OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
*        B     XIT                                                              
*        DROP  RE                                                               
*        SPACE 2                                                                
*-SUB-ROUTINE TO BUILD THE 94 KEY                                               
*                                                                               
* DKEY94 NTR1                                                                   
*        XC    KEY,KEY                                                          
*        LA    RE,KEY                                                           
*        USING NURECD,RE                                                        
*        MVI   NUKDTYPE,X'94'                                                   
*        MVC   NUKDAM,TWAGYMED                                                  
*        MVC   NUKDCLT,TWCLIENT                                                 
*        MVC   NUKDEST,TWEST                                                    
*        MVC   NUKDNET,TWNET                                                    
*        MVC   NUKDDAY,TWDAYNO                                                  
*        MVC   NUKDTIME,TWQTRHR                                                 
*        MVC   NUKDPROG,TWPRCODE                                                
*        MVC   NUKDDATE,TWPDATE                                                 
*        MVC   NUKDSUB,TWLNCT                                                   
*        OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
*        B     XIT                                                              
*        DROP  RE                                                               
         EJECT                                                                  
*--SET TO UNIT FILE                                                             
UNITSET  NTR1                                                                   
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 2                                                                
*--SET TO SPOT FILE                                                             
SPOTSET  NTR1                                                                   
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 2                                                                
*--SET TO STATION FILE                                                          
STATSET  NTR1                                                                   
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVC   LKEY,=H'15'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         MVC   FILENAME,=C'STATION '                                            
         MVC   COMMAND,=C'DMRDHI  '                                             
         B     XIT                                                              
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               EODAD=GETTAPEX,                                         X        
               RECFM=FB,                                               X        
               LRECL=00233,                                            X        
               BLKSIZE=2330,                                           X        
               MACRF=GM                                                         
*                                                                               
BJNYSPT  DCB   DDNAME=BJNYSPT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=PM,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760,                                          X        
               BUFNO=2                                                          
*                                                                               
BJNYUNT  DCB   DDNAME=BJNYUNT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=PM,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760,                                          X        
               BUFNO=2                                                          
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
         DS    0D                                                               
WORKAREA DS    CL600               WORK SPACE FOR DEMO CALCS, ETC               
*                                                                               
*        DC    CL16'C1F1F1D7C140C1C2C340F1F1F7F2F0F0'                           
*        DC    XL16'F1C2E8D9D6D540D5C5D3E2D6D540C7D6'                           
*        DC    XL16'D3C640C3D3C1E2E2C9C3F0F3F0404040'                           
*        DC    XL16'4040404040404040F0F0F4F0F0F0F0F0'                           
*        DC    XL16'F0F0F2F7F6F4F7E2C1F9F1F0F5F0F4F1'                           
*        DC    XL16'F4F3F0F1F6F0F040E2F9F3F1C8D6D4C5'                           
*        DC    XL16'E240D4F2F5F5F4404040404040404040'                           
*        DC    XL16'40404040404040404040404040404040'                           
*        DC    XL16'404040404040F0F0F0F0F0F2F8F0F0F2'                           
*        DC    XL16'F6F0F7F0F0F0F7F8F2F0F0F0F0F0F0F0'                           
*        DC    XL16'F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0'                           
*        DC    XL16'F0F0F0F0F0F0F0F2F9F0F7F0F2F1F0F0'                           
*        DC    XL16'F1F9F5F5F0F0F0F5F6F4F0F0F0F0F0F0'                           
*        DC    XL16'F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0'                           
*        DC    XL8'F0F0F0F0F0F0F0F0'                                            
AIOB     DS    CL250                                                            
ERRTAB   DS    0F                                                               
*-PACKAGE ERRORS                                                                
         DC    AL1(01),CL30'INVALID AGENCY                '                     
         DC    AL1(02),CL30'INVALID POSTING TYPE          '                     
         DC    AL1(03),CL30'INVALID AGENCY MEDIA          '                     
         DC    AL1(04),CL30'INVALID CLIENT                '                     
         DC    AL1(05),CL30'INVALID PRODUCT               '                     
         DC    AL1(06),CL30'INVALID ESTIMATE              '                     
         DC    AL1(07),CL30'INVALID EST DATE RANGE        '                     
         DC    AL1(08),CL30'INVALID STATION               '                     
         DC    AL1(09),CL30'INVALID DEMO BASE             '                     
         DC    AL1(10),CL30'PACKAGE REC DOES NOT EXIST    '                     
         DC    AL1(11),CL30'PROD CODE NOT ON PACKAGE REC  '                     
         DC    AL1(12),CL30'COST NOT EQUAL TO PKG REC     '                     
         DC    AL1(13),CL30'DEMO BASE NOT EQUAL TO PKG REC'                     
         DC    AL1(14),CL30'UNITS EXIST UNDER PACKAGE     '                     
         DC    AL1(15),CL30'ESTIMATE NOT NUMERIC          '                     
         DC    AL1(16),CL30'PACKAGE DATES NOT NUMERIC     '                     
         DC    AL1(17),CL30'PACKAGE COST NOT NUMERIC      '                     
         DC    AL1(18),CL30'PACKAGE CODE NOT NUMERIC      '                     
*-PROGRAM ERRORS                                                                
         DC    AL1(30),CL30'PROG LENGTH NOT NUMERIC       '                     
         DC    AL1(31),CL30'PROGRAM RATE NOT NUMERIC      '                     
         DC    AL1(32),CL30'HOMES IMP. NOT NUMERIC        '                     
         DC    AL1(33),CL30'PROG DEMOS NOT NUMERIC        '                     
         DC    AL1(34),CL30'INVALID PROGRAM NAME          '                     
         DC    AL1(35),CL30'TIMES NOT NUMERIC             '                     
         DC    AL1(36),CL30'TIMES VALUE GREATER 2400      '                     
         DC    AL1(37),CL30'TIMES VALUE EQUALS ZERO       '                     
*-UNIT ERRORS                                                                   
         DC    AL1(60),CL30'DATE NOT NUMERIC              '                     
         DC    AL1(61),CL30'DATE NOT A MONDAY             '                     
         DC    AL1(62),CL30'UNIT COUNT NOT NUMERIC        '                     
         DC    AL1(63),CL30'UNIT COUNT GREATER 192        '                     
         DC    AL1(64),CL30'BILLBOARD COUNT NOT NUMERIC   '                     
         DC    AL1(65),CL30'BILLBOARD GREATER UNIT COUNT  '                     
         DC    AL1(66),CL30'UNIT COUNT = 0                '                     
         DC    AL1(67),CL30'UNIT DATE OUTSIDE MST. RANGE  '                     
*-PROGRAM CODE MESSAGE                                                          
         DC    AL1(99),CL30'PROGRAM CODE                  '                     
         DC    XL1'FF'                                                          
         SPACE 3                                                                
WORKD    DSECT                                                                  
*                                                                               
RELO     DS    F                                                                
*                                                                               
UCOUNT   DS    F                                                                
PCOUNT   DS    F                                                                
RCOUNT   DS    F                                                                
TCOUNT   DS    F                                                                
*                                                                               
*     FOLLOWING FIELDS ARE DEVELOPED FROM THE REQUEST SENT                      
*     FROM THE PC.                                                              
*                                                                               
SCRATCH  DS    0CL1                SCRATCH SPACE                                
*--KEY FIELDS                                                                   
TWAGYMED DS    CL1                 AGENCY/MEDIA                                 
TWCLIENT DS    CL2                 CLIENT                                       
TWNET    DS    CL4                 NETWORK                                      
TWPROD   DS    CL3                 PRODUCT                                      
TWPRCD   DS    CL1                 PRODUCT CODE                                 
TWMKT    DS    CL2                 MARKET NUMBER                                
TWEST    DS    CL1                 ESTIMATE                                     
TWPKG    DS    CL1                 PACKAGE                                      
TWPSTTYP DS    CL1                 POSTING TYPE                                 
TWMEDTYP DS    CL1                 MEDIA TYPE                                   
TWCNTRL  DS    CL1                 CONTROL BYTE                                 
TWHOMES  DS    CL4                 HOMES IMPRESSION                             
TWDEMOS  DS    CL58                DEMO CODES 3 BYTE FORMAT                     
TWDEMVE  DS    CL42                DEMO VALUES EST                              
TWDEMVA  DS    CL42                DEMO VALUES ACT                              
TWPLEN   DS    CL1                 LENGTH                                       
TWPRACT  DS    CL4                 ACTUAL COST                                  
TWPRINT  DS    CL4                 INTEGRATION COST                             
*                                                                               
TWDYPT   DS    CL1                 DAYPART                                      
TWUNIV   DS    CL2                 UNIVERSE CODE                                
TWSREP   DS    CL2                 SPECIAL REP                                  
TWHUTTYP DS    CL1                 HUT TYPE                                     
TWFILTER DS    CL11                FILTER                                       
TWSTATUS DS    CL1                 PACKAGE STATUS                               
*                                                                               
TWPRCODE DS    CL6                 PROGRAM CODE                                 
TWPRROT  DS    CL1                 PROGRAM ROTATION                             
TWPRROTN DS    CL1                 4 BIT START END ROTATION NUMBERS             
TWDAYNO  DS    CL1                 DAY NUMBER (FOR X'94' KEY)                   
TWDAYHEX DS    CL1                 DAY NUMBER (HEX)                             
TWBONUS  DS    CL1                 BONUS UNIT C'B' OR BLANK                     
TWPRSTIM DS    CL2                 PROGRAM START TIME                           
TWPRETIM DS    CL2                 PROGRAM END TIME                             
TWPRNAME DS    CL16                PROGRAM NAME                                 
TWQTRHR  DS    CL1                 START QUARTER HOUR                           
TWPDATE  DS    CL2                 DATE FOR UNIT RECORDS                        
TCOMCODE DS    CL8                 COMMERCIAL CODE                              
TCOMPOST DS    CL3                 COMMERCIAL POSITION                          
*                                                                               
TWLNCT   DS    CL1                 START OF LINE COUNT                          
TWUNTCT  DS    CL1                 NUMBER OF UNITS TO BE ADDED                  
TWBLBCT  DS    CL1                 NUMBER OF BILLBOARDS                         
TWDSKADR DS    CL4                 DISK ADDRESS OF LAST ADD                     
*                                                                               
WRITESW  DS    CL1                 Y=UNIT WAS WRITTEN TO FILE                   
*                                                                               
MADERR   DS    C                   PACKAGE ERROR NUMBER                         
*                                  ERROR1 =AGENCY ERROR                         
*                                  ERROR2 =POSTING TYPE ERROR                   
*                                  ERROR3 =INVALID AGENCY MEDIA                 
*                                  ERROR4 =INVALID CLIENT                       
*                                  ERROR5 =INV MASTER ALLOCATION PRD            
*                                  ERROR6 =INVALID ESTIMATE                     
*                                  ERROR7 =INVALID ESTIMATE DATE RANGE          
*                                  ERROR8 =INVALID STATION                      
*                                  ERROR9 =INVALID DEMO BASE                    
*                                  ERROR10=NO MATCHING PACKAGE RECORD           
*                                  ERROR11=MASTER PROD NOT ON PACKAGE           
*                                  ERROR12=COST DOES NOT MATCH PACKAGE          
*                                  ERROR13=DEMO BASE NOT MATCH PACKAGE          
*                                  ERROR14=UNITS EXIST UNDER PACKAGE            
*                                  ERROR15=EST NOT NUMERIC                      
*                                  ERROR16=PACKAGE DATES NOT NUMERIC            
*                                  ERROR17=PACKAGE COST NOT NUMERIC             
*                                  ERROR18=PACKAGE CODE NOT NUMERIC             
*                                                                               
*                                  PROGRAM ERROR NUMBER                         
*                                  ERROR30=PROGRAM LENGTH NOT NUMERIC           
*                                  ERROR31=PROGRAM RATE NOT NUMERIC             
*                                  ERROR32=HOMES IMP. NOT NUMERIC               
*                                  ERROR33=PROGRAM DEMOS NOT NUMERIC            
*                                  ERROR34=INVALID PROGRAM NAME                 
*                                  ERROR35=TIMES NOT NUMERIC                    
*                                  ERROR36=TIMES VALUE > 2400                   
*                                  ERROR37=TIMES VALUE = 0                      
*                                                                               
*                                  UNIT ERROR NUMBER                            
*                                  ERROR60=DATE NOT NUMERIC                     
*                                  ERROR61=DATE NOT A MONDAY                    
*                                  ERROR62=UNIT COUNT NOT NUMERIC               
*                                  ERROR63=NUMBER OF UNITS > 192                
*                                  ERROR64=BILLBOARD COUNT NOT NUMERIC          
*                                  ERROR65=BILLBOARD COUNT > UNIT COUNT         
*                                  ERROR66=UNIT COUNT = 0                       
*                                  ERROR67=UNIT DATE OUTSIDE EST RANGE          
*                                                                               
DUPSW    DS    C                                                                
NFLDS    DS    C                   NUMBER OF DEMOS ON PACKAGE HEADER            
SAVCLI   DS    CL3                                                              
*                                                                               
TSARBLCK DS    CL50                TSAR DATA BLOCK                              
TSARIO   DS    CL50                TSAR IO AREA                                 
*                                                                               
COREFACS DS    0F                                                               
VNETWEEK DS    V                                                                
VGETNUN  DS    V                                                                
*                                                                               
ATSAROFF DS    A                                                                
*                                                                               
RECBUFF  DS    A                                                                
*                                                                               
BUFFLOC  DS    F                                                                
*                                                                               
PKCOUNT  DS    F                                                                
PRCOUNT  DS    F                                                                
UNCOUNT  DS    F                                                                
CLCOUNT  DS    F                                                                
SAVREGE  DS    F                                                                
*                                                                               
DBDEMOB  DS    CL480               AREA FOR DEMO INTRFACE MODULE                
*                                                                               
LSCRATCH EQU   *-SCRATCH                                                        
         SPACE                                                                  
*                                                                               
MYWORKLE EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE IUNRECDS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDDD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
CLTREC   DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
* TAPE IN DSECT                                                                 
TAPEIN   DSECT                                                                  
*                                                                               
TCLIENT  DS    CL3                 CLIENT                                       
TPROD    DS    CL3                 PRODUCT                                      
TNET     DS    CL4                 NETWORK                                      
TPACK    DS    CL4                 PACKAGE                                      
TEST     DS    CL3                 ESTIMATE                                     
TPROGNAM DS    CL25                PROGRAM NAME                                 
TLENGTH  DS    CL3                 LENGTH                                       
TCOMMCD  DS    CL8                 COMMERCIAL CODE                              
TCOMPOS  DS    CL3                 COMMERCIAL POSITION                          
TACTUAL  DS    CL9                 ACTUAL DOLLARS                               
TINTEG   DS    CL6                 INTEGRATION DOLLARS                          
TDAY     DS    CL2                 DAY OF WEEK                                  
TDATE    DS    CL6                 DATE YYMMDD                                  
TSTIME   DS    CL4                 START TIME                                   
TETIME   DS    CL4                 END TIME                                     
TBONUS   DS    CL1                 BONUS UNIT C'B' OR ' '                       
TDAYPT   DS    CL1                 DAYPART                                      
TUNIV    DS    CL4                 UNIVERSE                                     
TDEMO1   DS    CL6                 DEMO1 ALWAYS HOMES                           
TDEMO2   DS    CL6                 DEMO2                                        
TDEMO3   DS    CL6                 DEMO3                                        
TDEMO4   DS    CL6                 DEMO4                                        
TDEMO5   DS    CL6                 DEMO5                                        
TDEMO6   DS    CL6                 DEMO6                                        
TDEMO7   DS    CL6                 DEMO7                                        
TEHUTS   DS    CL2                 ESTIMATED HUTS                               
TESHARE  DS    CL2                 ESTIMATED SHARE                              
TERATING DS    CL3                 ESTIMATED RATINGS                            
TEIMPD1  DS    CL6                 EST IMPS DEMO 1 ALWAYS HOMES                 
TEIMPD2  DS    CL6                 EST IMPS DEMO 2                              
TEIMPD3  DS    CL6                 EST IMPS DEMO 3                              
TEIMPD4  DS    CL6                 EST IMPS DEMO 4                              
TEIMPD5  DS    CL6                 EST IMPS DEMO 5                              
TEIMPD6  DS    CL6                 EST IMPS DEMO 6                              
TEIMPD7  DS    CL6                 EST IMPS DEMO 7                              
TAHUTS   DS    CL2                 ACTUAL HUTS                                  
TASHARE  DS    CL2                 ACTUAL SHARE                                 
TARATING DS    CL3                 ACTUAL RATINGS                               
TAIMPD1  DS    CL6                 ACT IMPS DEMO 1 ALWAYS HOMES                 
TAIMPD2  DS    CL6                 ACT IMPS DEMO 2                              
TAIMPD3  DS    CL6                 ACT IMPS DEMO 3                              
TAIMPD4  DS    CL6                 ACT IMPS DEMO 4                              
TAIMPD5  DS    CL6                 ACT IMPS DEMO 5                              
TAIMPD6  DS    CL6                 ACT IMPS DEMO 6                              
TAIMPD7  DS    CL6                 ACT IMPS DEMO 7                              
* CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                         
         SPACE 4                                                                
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
DIVISOR  DS    F                   DIVISOR BUCKET                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
HOMSHR   DS    3F                                                               
         DS    0F                                                               
         SPACE 2                                                                
         DS    0F                                                               
         EJECT                                                                  
* EXTRA CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                   
         SPACE 4                                                                
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187NEWRI66   05/01/02'                                      
         END                                                                    
         ^^^^^^^^^^^^^^^                                                        
*                                                                               
*                                                                               
*--FILTER LOGIC                                                                 
*        CLI   INREC+15,C'8'                                                    
*        BNE   GET                                                              
*        CLC   INREC(3),=CL3'WBZ'                                               
*        BE    WRITE                                                            
*        CLC   INREC(4),=CL3'WCVB'                                              
*        BNE   GET                                                              
WRITE    GOTO1 =V(PRNTBL),DMCB,=C'TAPE',INREC,C'DUMP',232,=C'1D'                
         B     GET                                                              
ENDJOB   B     CLSE                                                             
CLSE     CLOSE (IN)                                                             
         CLOSE (IN)                                                             
         CLOSE (IN)                                                             
         MVC   P+2(18),=C'TAPE COPY'                                            
         GOTO1 =V(PRINTER)                                                      
CLSE     XBASE                                                                  
SYSVCON  DC    V(GETDAY)                                                        
         DC    V(ADDAY)                                                         
*                                                                               
GETDAY1  DS    V                                                                
ADDAY1   DS    V                                                                
DUB      DS    D                                                                
DATE     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
WORK     DS    CL20                                                             
DMCB     DS    CL64                                                             
INREC    DS    CL800                                                            
         LTORG                                                                  
*                                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
         END                                                                    
         ^^^^^^^^^^^^^^^                                                        
