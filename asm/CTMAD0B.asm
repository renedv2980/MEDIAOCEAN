*          DATA SET CTMAD0B    AT LEVEL 145 AS OF 05/01/02                      
*PHASE TA0C0BA,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE REGETIUN                                                               
*INCLUDE TIMVAL                                                                 
         TITLE 'TA0C0B - $MAD REP DEMOGRAPHIC RETRIEVAL'                        
**********************************************************************          
*   HISTORY OF CHANGES *                                                        
**********************************************************************          
*   03/27/91   (BU ) --- ORIGINAL ENTRY                              *          
*   10/16/91   (BU ) --- CORRECT ERROR IN SELECTING RANKED ENTRIES   *          
*                        IN ROUTINE RSEN0006                         *          
*                                                                    *          
*   04/07/92   (BU ) --- ADD IO TIMEOUT CONTROL FOR DAYPART PROCESS- *          
*                        ING.  FIX EFFECTIVE DATE FILTERING.         *          
*                                                                    *          
*   04/10/92   (BU ) --- NMOD BASIC SETUP ROUTINES: ADDRESSABILITY   *          
*                                                                    *          
**********************************************************************          
TA0C0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C0B,RA,RR=R2                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
*                                                                               
         ST    R2,RELO             SAVE RELOCATION ADDRESS                      
*                                                                               
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
*                                                                               
         DROP  R7                  DROP 1ST APPLIC COMMON STORAGE               
*                                                                               
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* DEMOGRAPHIC INFORMATION FROM THE PC AND RETURNS THE FIRST SCREENFUL           
* OF INFORMATION.  IT ALSO SETS THE INTERNAL CONTROL TO BEGIN THE               
* SEND OF SUBSEQUENT DATA.                                                      
*                                                                               
PROCSTRT NTR1                                                                   
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
         BAS   RE,PROCINIT                   INITIALIZE SCRATCH AREA            
         GOTO1 =A(PROCRQST),DMCB,(RC),RR=YES PROCESS REQUEST                    
         BAS   RE,PROCDATA         PROCESS DATA                                 
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* 'PROCMID' PROCESSES MIDDLE MODE.  IT PROCESSES AND SENDS THE 2ND              
*  THRU NTH SCREENS OF DATA.                                                    
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
         MVC   SAVRSTRT,RSTRTRTN   SAVE RESTART VALUE FOR DUMPS                 
         BAS   RE,PROCIN02         REINIT PART OF WORKAREA                      
         BAS   RE,RESETRUN         RESET WORKAREA                               
         BAS   RE,PROCDATA         PROCESS DATA                                 
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* 'PROCEND' PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.                 
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    INITIALIZES ALL VALUES IN WORK SCRATCH SPACE ON THE                        
*      START-UP PASS                                                            
*                                                                               
         DS    0H                                                               
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
*                                                                               
         LA    RE,INVSTORE         A(INVENTORY REQUEST STORAGE)                 
         ST    RE,ATWINVS          SAVE IT FOR LOAD                             
         B     PIN0004             SKIP ALTERNATE ENTRY POINT                   
*                                                                               
PROCIN02 NTR1                      ALTERNATE ENTRY POINT                        
*        CLI   RSTRTRTN,0          **TEST**                                     
*        BE    PINTEST             **TEST**                                     
*        DC    H'0'                **TEST**                                     
PINTEST  EQU   *                                                                
         LA    R6,DBDEMOB          A(DEMO INTERFACE AREA)                       
         B     PIN0008                                                          
*                                                                               
PIN0004  EQU   *                                                                
*                                                                               
*        INITIALIZE DEMOGRAPHIC DBLOCK PARAMETERS FOR INVENTORY                 
*                                                                               
         LA    R6,DBDEMOB          A(DEMO INTERFACE AREA)                       
*                                                                               
         USING DEMOD,R6                                                         
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR DEMOUT                 
         MVC   DBSELAGY,SIGNON2C   SET REP CODE FOR AUTHORIZATIONS              
         MVC   DBFILE,=C'INV'      INSERT FILE NAME                             
PIN0008  EQU   *                                                                
         L     RE,AIO              A(IOAREA)                                    
         ST    RE,DBAREC                                                        
         LA    RE,34(RE)           A(1ST ELEMENT IN RECORD)                     
         ST    RE,DBAQUART                                                      
         L     RE,ACOMFACS         A(COMFACS)                                   
         ST    RE,DBCOMFCS                                                      
*                                                                               
         LA    R1,DBEXTRA1         SET UP PRECISION                             
         STCM  R1,15,DBEXTEND                                                   
*                                                                               
         USING DBXTTID,R1                                                       
*                                                                               
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'       1 DEC RTG/PUT                                
         MVI   DBXTTSP,X'01'       1 DEC SHARES                                 
         MVI   DBXTTIP,X'02'       IMPS TO 100'S                                
*                                                                               
         DROP  R1,R6                                                            
*                                                                               
*   GET DEMOUT/DEMAINT ADDRESSES FROM COMFACS LIST                              
*                                                                               
*        CLI   RSTRTRTN,0          **TEST                                       
*        BE    TEST2               **TEST                                       
*        DC    H'0'                **TEST                                       
TEST2    EQU   *                   **TEST                                       
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   DEMAINT,CDEMAINT                                                 
         MVC   DEMOMTH,CDEMOMTH                                                 
         MVC   DATVAL,CDATVAL                                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     RE,AADDIO           USE ADDIO AS INTERIM DEMO STORAGE            
         LA    R6,DBDEMOB                                                       
         USING DEMOD,R6                                                         
*                                                                               
         ST    RE,AINTEREC         A(INTERIM DEMO STORAGE)                      
*                                                                               
         DROP  R6                                                               
*                                                                               
*   DETERMINE MAXIMUM ACCEPTABLE IO'S ALLOWED FOR THIS JOB:                     
*     SET MAXIOCTR TO 90% OF MAX IO'S SHOWN IN GETFACT                          
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)                                     
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLE MAXIMUM IO BY 9                     
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4               TO PRODUCE 90%                               
         STH   R3,MAXIOCTR         SAVE 90% OF MAX COUNT                        
*                                                                               
*   RETRIEVE A(CORE RESIDENT BOOKVAL ROUTINE)                                   
*                                                                               
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D900A00'    T00A00 = BOOKVAL                        
         GOTO1 (RF),(R1),0                                                      
         MVC   BOOKVAL,DMCB                                                     
*                                                                               
PIN0099  EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   RE-READS THE RECORDS IN PROGRESS AT TIME PREVIOUS EXECUTION                 
*     TERMINATED.  ANY ADDITIONAL DATA RESETTING IS ALSO DONE HERE.             
*                                                                               
RESETRUN NTR1                                                                   
*                                                                               
         XC    RSTRTEND,RSTRTEND   RESET RESTART NEEDED FLAG                    
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),RSTRTMST  DISK ADDRESS OF MASTER RECORD                
         MVC   FULL,AIO            SAVE CURRENT A(IO AREA)                      
         L     R1,AIO2             SET IO AREA 2 FOR READ                       
         ST    R1,AIO                                                           
         GOTO1 GETREC              READ MASTER REC FOR RESTART                  
RSET0002 EQU   *                                                                
*                                                                               
         L     R1,AIO1             SET IO AREA 1 FOR READ                       
         ST    R1,AIO                                                           
         XC    KEY,KEY                                                          
         CLI   RSTRTRTN,7          IO TIMEOUT RESTART?                          
         BE    RSET0012            YES                                          
         MVC   KEY+28(4),RSTRTDET  DISK ADDRESS OF DETAIL RECORD                
         GOTO1 GETREC              READ DETAIL REC FOR RESTART                  
         MVC   KEY(27),0(R1)       LOAD KEY                                     
RSET0012 EQU   *                                                                
         MVC   AIO(4),FULL         RESET CURRENT A(IO AREA)                     
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWDEMO          SAVE IT BACK                                 
         L     RF,ATWINVS          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWINVS          SAVE IT BACK                                 
*                                                                               
*   NOTE:  RANKADDR IS BASED ON THE ADDRESS OF 'AADDIO'.  FOR RE-               
*     STARTING, ONLY THE DISPLACEMENT INTO 'ADDIO' IS IN RANKADDR.              
*     IT IS NECESSARY TO ADD THE STARTING ADDRESS 'AADDIO' BACK.                
*                                                                               
         L     RF,RANKADDR         DISPLACEMENT INTO AREA                       
         L     RE,AADDIO           ADDRESS OF WORKAREA                          
         AR    RF,RE               ADDRESS + DISPLACEMENT                       
         ST    RF,RANKADDR         SAVE IT BACK                                 
         CLI   SAVERANK,C'Y'       RANK AREA SAVED?                             
         BNE   RSET0099            NO  - FINISHED                               
         GOTO1 TMPOPEN,DMCB,=C'GET',1200                                        
         L     R2,AADDIO                                                        
         LA    R3,1200                                                          
         GOTO1 GETTMP,DMCB,(R2),(R3)                                            
         GOTO1 TMPCLOSE                                                         
RSET0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*   DETERMINES WHETHER THE REQUEST IS FOR INVENTORY BY DAYPART OR BY            
*     NUMBER, AND CALLS THE APPROPRIATE ROUTINE                                 
*                                                                               
PROCDATA NTR1                                                                   
         MVI   SCRNDATA,C'N'       SET 'NO DATA ON SCREEN'                      
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BE    PD0004              YES - DON'T RESET ADDRESSES                  
         CLI   RSTRTRTN,7          RESTART FOR IO COUNT REASONS?                
         BNE   PD0010              NO  - SKIP RESET OF ADDRESSES                
         MVC   KEY(27),KEY92SAV    YES - RESET LAST KEY                         
         B     PD0010              SKIP RESET OF ADDRESSES                      
*                                                                               
*  ESTABLISH INITIAL ADDRESSES WITHIN ARRAYS                                    
*                                                                               
PD0004   EQU   *                                                                
         LA    R2,TWBOOK           A(1ST BOOK)                                  
         ST    R2,ATWBOOK          A(BOOK IN PROCESS)                           
         LA    R2,TWDEMOS          A(1ST DEMO)                                  
         ST    R2,ATWDEMO          A(DEMO IN PROCESS)                           
         LA    R2,INVSTORE         A(1ST REQUEST)                               
         ST    R2,ATWINVS          A(REQUEST IN PROCESS)                        
*                                                                               
PD0010   EQU   *                                                                
         L     R2,ATWINVS          LOAD IN CASE OF RESTART                      
         CLI   INVRUN,0            INV BY DAYPART OR NUMBER?                    
         BNE   PD0020              NOT ZERO = BY NUMBER                         
         GOTO1 DPCYCLE,DMCB,(R2)   ZERO     = DAYPART                           
         B     PD0099                                                           
PD0020   EQU   *                                                                
         GOTO1 INVCYCLE,DMCB,(R2)                                               
PD0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*     CONTROLS PROCESSING OF INVENTORY BY DAYPARTS.  IT PASSES EACH             
*     DAYPART REQUEST, FILTERING MASTERS BY HIGH LEVEL FILTERS, THEN            
*     RETRIEVES APPROPRIATE TRACKS, AND FILTERS BY LOW LEVEL FILTERS            
*     P1  =  A(REQUEST IN PROCESS)                                              
*                                                                               
DPCYCLE  NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
         USING DIDSTORE,R2                                                      
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    DPCY0004            NO                                           
         CLI   RSTRTRTN,7          RESTART FOR IO COUNT?                        
         BE    DPCY0008            NO                                           
         CLI   RSTRTRTN,11         RESTART FROM RANK?                           
         BE    DPCY0020            YES                                          
         CLI   RSTRTRTN,21         RESTART FROM RANK?                           
         BE    DPCY0020            YES                                          
         B     DPCY0052            OTHER RESTART ENTRY                          
DPCY0004 EQU   *                                                                
*                                                                               
*   IF OPTION FOR SELECTION AFTER RANK IS CHOSEN, AADDIO IS USED                
*      AS THE SORT WORK SPACE                                                   
*                                                                               
         MVC   RANKADDR,AADDIO     A(WORK AREA FOR RANK)                        
         L     RE,AADDIO                                                        
         XCEF  (RE),1200           INITIALIZE SORT AREA                         
*                                                                               
         XC    KEY,KEY             ESTABLISH PASSIVE DAYPART KEY                
         MVI   KEY,X'92'                                                        
         MVC   KEY+3(2),SIGNON2C   INSERT REP CODE                              
         MVC   KEY+5(5),TWSTAT     INSERT STATION + MEDIA                       
         L     R2,ATWINVS          ISOLATE REQUEST IN STORAGE                   
         USING DIDSTORE,R2                                                      
         MVC   KEY+10(1),DIDDPT    INSERT DAYPART FROM REQUEST                  
DPCY0008 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     DPCY0016            CHECK KEY                                    
DPCY0012 EQU   *                                                                
         GOTO1 SEQ                 RETURN NEXT PASSIVE KEY                      
DPCY0016 EQU   *                                                                
         CLC   KEY(11),KEYSAVE     SAME THROUGH DAYPART?                        
         BE    DPCY0024            YES - PROCESS IT                             
*                                                                               
*   END OF DAYPART REQUEST:  HAS REQUEST BEEN FOR A RANK SELECTION?             
*      IF SO, RANK ELEMENTS MUST BE SORTED, THEN SELECTED ITEMS                 
*      MUST BE PROCESSED AS INDIVIDUAL INVENTORY REQUESTS                       
*                                                                               
         CLI   DIDACT,C'='         RANK SELECTION?                              
         BNE   DPCY0064            NO                                           
         GOTO1 RANKDEMS            YES                                          
DPCY0020 EQU   *                                                                
         GOTO1 SENDRANK            SEND THE RANKED DATA                         
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    DPCY0084            YES - WRAP UP AND RESTART                    
         B     DPCY0064            PROCESS NEXT REQUEST                         
*                                                                               
DPCY0024 EQU   *                                                                
         MVC   KEY92SAV(27),KEY    SAVE PASSIVE KEY FOR SEQ PROCESS             
         GOTO1 GETFACT,DMCB,0      GET CURRENT IO COUNT                         
         L     R1,0(R1)            IO'S LEFT?                                   
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)                                     
         BH    DPCY0028            IO'S AVAILABLE - PROCESS                     
         MVI   RSTRTRTN,7          NO  - SEND SCREEN WITH WHATEVER              
*                                  DATA IS LOADED                               
         CLI   SCRNDATA,C'Y'       DATA ON SCREEN?                              
         BE    DPCY0084            YES - SEND IT                                
         MVC   ELTAREA,=C'IO TIMEOUT'                                           
         LA    R2,ITRDMFDM         INSERT DUMMY ITEM TYPE                       
         GOTO1 PUTITEM,DMCB,(R2),10,ELTAREA                                     
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         B     DPCY0084            WRAP UP AND RESTART                          
DPCY0028 EQU   *                   RESTART ENTRY POINT                          
         L     R6,AIO2             USE SECOND AREA TO STORE                     
         ST    R6,AIO              INVENTORY HEADER/MASTER                      
         USING RINVRECD,R6                                                      
*                                                                               
         MVC   RSTRTMST(4),KEY+28  SAVE DISK ADDRESS FOR RESTART                
         GOTO1 GETREC              RETRIEVE MASTER FOR FILTER TESTS             
*                                                                               
*  HIGH LEVEL FILTERS:  DAY, TIME, START/END DATES                              
*                                                                               
         CLI   DIDDYFIL,X'FF'      ANY DAY FILTER?                              
         BE    DPCY0032            NO                                           
         CLC   DIDDYFIL,RINVKDAY   YES - IS INVENTORY FOR DAY?                  
         BNE   DPCY0060            NO  - READ PAST IT                           
DPCY0032 EQU   *                                                                
         ZIC   R5,DIDTMFIL+3       ANY TIME FILTER - START QTR HR               
         LTR   R5,R5               ANY VALUE?                                   
         BZ    DPCY0044            NO  - SKIP  QTR HR TESTS                     
         ZIC   R5,DIDTMFIL+7       ANY TIME FILTER - END QTR HR                 
         LTR   R5,R5               ANY VALUE?                                   
         BNZ   DPCY0036            YES - TREAT AS RANGE                         
         CLC   RINVKQTR,DIDTMFIL+3 NO  - TEST AS EQUAL                          
         BNE   DPCY0060            NOT EQUAL - SKIP IT                          
         B     DPCY0044            NO FURTHER END TEST                          
DPCY0036 EQU   *                                                                
         CLC   RINVKQTR,DIDTMFIL+3 TEST START QTR HR                            
         BL    DPCY0060            BEFORE START QTR HR                          
DPCY0040 EQU   *                                                                
         ZIC   R5,DIDTMFIL+7       ANY TIME FILTER - END QTR HR                 
         LTR   R5,R5               ANY VALUE                                    
         BZ    DPCY0044            NO  - SKIP TESTS                             
         CLC   RINVKQTR,DIDTMFIL+7 TEST END QTR HR                              
         BH    DPCY0060            AFTER END QTR HR                             
*                                                                               
*  TEST START/END DATES AS FILTERS                                              
*                                                                               
DPCY0044 EQU   *                                                                
         CLC   DIDSTDT,MYLOWVAL    ANY START DATE?                              
         BE    DPCY0048            NO                                           
         CLC   RINVKSTD,DIDSTDT    YES - EFFECTIVE EARLIER?                     
         BL    DPCY0060            YES - SKIP IT                                
DPCY0048 EQU   *                                                                
         CLC   DIDENDT,MYLOWVAL    ANY END DATE?                                
         BE    DPCY0052            NO                                           
         CLC   RINVKSTD,DIDENDT    YES - EFFECTIVE LATER?                       
         BH    DPCY0060            YES - SKIP IT                                
DPCY0052 EQU   *                   RESTART ENTRY POINT                          
         CLI   RSTRTRTN,6          RESTART AT 'EOD' SET?                        
         BE    DPCY0076            YES - PUT OUT EOD AND END                    
         CLI   RSTRTRTN,0          ANY OTHER RESTART SET?                       
         BNE   DPCY0056            YES - DON'T RESET BOOK                       
         LA    R1,TWBOOK           A(1ST BOOK):  RESET FOR EACH MASTER          
         ST    R1,ATWBOOK          A(BOOK IN PROCESS)                           
DPCY0056 EQU   *                                                                
         GOTO1 RUNINV,DMCB,(R6)    PROCESS THIS INVENTORY NUMBER                
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    DPCY0084            YES - WRAP UP AND RESTART                    
DPCY0060 EQU   *                                                                
         MVC   KEY(27),KEY92SAV    RESET KEY                                    
         GOTO1 READ                RE-ESTABLISH KEY SEQUENCE                    
         B     DPCY0012                                                         
DPCY0064 EQU   *                                                                
         LA    R6,17               INITIAL SIZE OF ENTRY                        
         TM    DIDFLAGS,X'20'      DEMO FILTER 1 REQUESTED?                     
         BNO   DPCY0072            NO  - FINISHED                               
         LA    R6,8(R6)            YES                                          
         TM    DIDFLAGS,X'10'      SECOND VALUE ENTERED?                        
         BNO   DPCY0068            NO                                           
         LA    R6,4(R6)            YES                                          
DPCY0068 EQU   *                                                                
         TM    DIDFLAGS,X'08'      DEMO FILTER 2 REQUESTED?                     
         BNO   DPCY0072            NO  - FINISHED                               
         LA    R6,8(R6)            YES                                          
         TM    DIDFLAGS,X'04'      SECOND VALUE ENTERED?                        
         BNO   DPCY0072            NO                                           
         LA    R6,4(R6)            YES                                          
DPCY0072 EQU   *                                                                
         AR    R2,R6               L(ENTRY)+A(CURRENT)=A(NEXT)                  
         ST    R2,ATWINVS          SAVE IT BACK                                 
         CLI   0(R2),X'00'         ANY VALUE?                                   
         BNE   DPCY0004            GO BACK FOR NEXT                             
*                                                                               
*  ALL DAYPARTS PROCESSED, NO RESTART NEEDED.  SEND END-OF-DATA ELEMENT         
*      WITH FRAME, SET LAST-FRAME INDICATOR                                     
DPCY0076 EQU   *                                                                
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   DPCY0080            ELEMENT FITS - PROCEED                       
         MVI   RSTRTRTN,6          SET RESTART TO 'EOD NEEDED'                  
         B     XIT                 END FOR RESTART                              
DPCY0080 EQU   *                                                                
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     DPCY0088            EXIT                                         
DPCY0084 EQU   *                                                                
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDEMO          SAVE IT BACK                                 
         L     RF,ATWINVS          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWINVS          SAVE IT BACK                                 
*                                                                               
*   NOTE:  RANKADDR IS BASED ON THE ADDRESS OF 'AADDIO'.  FOR RE-               
*     START, ONLY THE DISPLACEMENT INTO THE AREA IS SAVED.  'AADDIO'            
*     IS SUBTRACTED TO GET THAT.                                                
*                                                                               
         L     RF,RANKADDR         DISPLACEMENT INTO AREA                       
         L     RE,AADDIO           FIRST ADDRESS OF AREA                        
         SR    RF,RE               DISPLACEMENT - ADDRESS                       
         ST    RF,RANKADDR         SAVE IT BACK                                 
DPCY0088 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*       SORTS THE ELEMENTS IN AADDIO, THEN SELECTS THOSE WHICH                  
*       FALL WITHIN THE SELECTION CRITERIA                                      
RANKDEMS NTR1                                                                   
         L     R2,AADDIO           A(RANK ELEMENTS FOR SORTING)                 
         ZIC   R3,RANKCTR          # OF ELEMENTS TO BE RANKED                   
         LA    R4,L'RANKWORK       L(RANK ELEMENT)                              
         GOTO1 QSORT,DMCB,(1,(R2)),(R3),(R4),4,0                                
         B     XIT                                                              
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*       SCANS THE SORTED ENTRIES IN THE WORK AREA, SELECTS THOSE                
*       THAT FALL WITHIN THE PARAMETERS, AND CAUSES DATA TO BE                  
*       DRAWN, FORMATTED, AND SENT                                              
*                                                                               
SENDRANK NTR1                                                                   
         L     R1,ATWINVS          A(REQUEST IN PROCESS)                        
         USING DIDSTORE,R1                                                      
*                                                                               
         MVC   TESTIT,RSTRTRTN     **TEST**                                     
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    RSEN0006            NO                                           
         L     R3,RANKADDR         SET A(RANK ELEMENT IN PROCESS)               
         ZIC   R6,RANKCTR          RESET COUNTER                                
         CLI   RSTRTRTN,11         RESTART AT PULLDEMS?                         
         BE    RSEN0024            YES                                          
         CLI   RSTRTRTN,21         RESTART AT PULLRATS?                         
         BE    RSEN0028            YES                                          
         CLI   RSTRTRTN,41         RESTART AT SENDDESC/PULLDEMS?                
         BE    RSEN0024            YES                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
RSEN0006 EQU   *                                                                
         MVI   SAVERANK,C'N'       SET 'SAVE WORKAREA' TO NO                    
         MVC   RANKADDR,AADDIO     A(STORED ITEMS/ITEMS TO PROCESS)             
         ZIC   R6,RANKCTR          # OF ITEMS TO PROCESS                        
         MVC   FULL,DIDVAL1        GET FIRST VALUE (FROM-COUNTER)               
         L     R5,FULL             LOAD INTO REGISTER                           
*                                                                               
*   'SELECT FROM RANK' MAY ENTER A SELECT RANGE THE START OF WHICH              
*      EXCEEDS THE NUMBER OF ITEMS AVAILABLE.  IN THIS CASE, NO                 
*      DATA WILL BE RETURNED.                                                   
*                                                                               
         CR    R6,R5               # ITEMS VS 1ST ITEM TO SELECT                
         BL    RSEN0099            EXIT                                         
         BCTR  R5,0                - 1 FROM FROM-CTR: ZERO RELATIVE             
         LR    R2,R5               SAVE ZERO-REL FROM-CTR                       
         LTR   R5,R5               WAS FROM-CTR SET TO 1?                       
         BZ    RSEN0012            YES - DON'T SKIP ENTRIES                     
         SR    R6,R2               - # ITEMS TO SKIP FROM # ITEMS               
RSEN0010 EQU   *                                                                
         L     R3,RANKADDR                                                      
         LA    R3,L'RANKWORK(R3)   ADD L(RANK ENTRY)                            
         ST    R3,RANKADDR         SAVE IT BACK                                 
         BCT   R5,RSEN0010         SKIP (FROM-CTR - 1) ENTRIES                  
RSEN0012 EQU   *                                                                
         MVC   FULL,DIDVAL2        GET 2ND VALUE (TO-COUNTER)                   
*                                                                               
         DROP  R1                                                               
*                                                                               
         L     R5,FULL             LOAD INTO REGISTER                           
         SR    R5,R2               - FROM-CTR FOR ABSOLUTE COUNT                
         CR    R5,R6               ABSOLUTE COUNT VS # ENTRIES                  
         BNL   RSEN0016                                                         
         LR    R6,R5               TO-CTR < # ENTRIES: USE IT                   
         STC   R6,RANKCTR          SAVE NEW COUNT FOR RESTART                   
RSEN0016 EQU   *                                                                
         L     R4,AIO2             USE RECORD AREA TO STORE                     
         ST    R4,AIO                 INVENTORY HEADER/MASTER                   
         XC    KEY,KEY                                                          
         L     R3,RANKADDR         A(MASTER/TRACK TO PROCESS)                   
         CLI   TESTIT,0            **TEST**                                     
         BE    TEST1               **TEST**                                     
         LTR   R3,R3               **TEST**                                     
TEST1    EQU   *                   **TEST**                                     
         MVC   KEY+28(4),4(R3)     TAKE D/A MASTER                              
         GOTO1 GETREC              RETRIEVE MASTER                              
         MVC   RSTRTMST(4),KEY+28  SAVE DISK ADDR FOR RESTART                   
         MVC   EXTRAKEY,0(R4)      SET KEY FOR RATIONALE                        
         L     R4,AIO1             USE PRIMARY AREA TO STORE                    
         ST    R4,AIO                 DETAIL TRACK                              
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),8(R3)     TAKE D/A TRACK                               
         GOTO1 GETREC              RETRIEVE TRACK                               
         MVC   KEY(27),0(R4)       LOAD KEY                                     
         MVI   DEMOFLAG,C'N'       SET DESCRIPTIVE DATA FLAG                    
         MVI   RSTRTDEM,11         SET 'PULLDEMS FROM RANK' FLAG                
RSEN0024 EQU   *                                                                
         BAS   RE,PULLDEMS         DRAW DEMS FROM TRACK                         
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    RSEN0098            YES - WRAP UP AND RESTART                    
         MVI   RSTRTRTS,21         SET 'PULLDEMS FROM RANK' FLAG                
RSEN0028 EQU   *                                                                
         BAS   RE,PULLRATS         DRAW RATIONALE FOR THIS MASTER               
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    RSEN0098            YES - WRAP UP AND RESTART                    
         LA    R3,L'RANKWORK(R3)   GO TO NEXT ENTRY                             
         ST    R3,RANKADDR                                                      
         BCT   R6,RSEN0016         GO BACK FOR NEXT                             
         B     RSEN0099            EXIT WITH NO SAVE                            
*                                                                               
RSEN0098 EQU   *                                                                
         STC   R6,RANKCTR          SAVE CURRENT COUNT FOR RESTART               
         BAS   RE,SAVERNK          SAVE THE RANK WORK SPACE                     
RSEN0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*       SAVES THE WORK-SPACE IN AADDIO FOR RESTART                              
*                                                                               
SAVERNK  NTR1                                                                   
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
         L     R2,AADDIO           A(SORT WORK SPACE)                           
         LA    R3,1200             L(SORT WORK SPACE)                           
         GOTO1 PUTTMP,DMCB,(R2),(R3)                                            
         GOTO1 TMPCLOSE                                                         
         MVI   SAVERANK,C'Y'       SET FLAG TO YES                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  THIS ROUTINE:                                                                
*       PROCESSES REQUESTS FOR INVENTORY BY NUMBER, AND WILL AVERAGE            
*       OR COMBINE MULTIPLE LINES, PRODUCING A SINGLE OUTPUT FIGURE             
*  NOTE:  THERE IS NO IO COUNT TIMEOUT FOR THE INVENTORY SIDE OF THIS           
*       PROGRAM, BECAUSE EACH ITEM IS INDIVIDUALLY REQUESTED.  THE USER         
*       CANNOT REQUEST ENOUGH DATA TO CAUSE AN IO TIMEOUT.                      
*                                                                               
INVCYCLE NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BNE   ICY0013             YES                                          
ICY0002  EQU   *                                                                
         XC    KEY,KEY             ESTABLISH ACTIVE KEY                         
         MVI   KEY,X'12'                                                        
         MVC   KEY+10(2),SIGNON2C  INSERT REP CODE                              
         MVC   KEY+12(5),TWSTAT    INSERT STATION + MEDIA                       
         L     R2,ATWINVS          ISOLATE REQUEST IN STORAGE                   
         USING DINSTORE,R2                                                      
*                                                                               
         MVC   KEY+17(3),DININV#   INSERT INVENTORY #                           
         GOTO1 HIGH                                                             
         B     ICY0006                                                          
ICY0004  EQU   *                                                                
         GOTO1 SEQ                                                              
ICY0006  EQU   *                                                                
         CLC   KEY(20),KEYSAVE     SAME KEY?                                    
         BNE   ICY0020             NO  - GET NEXT REQUEST                       
         CLC   DINEFDTE,MYLOWVAL   ANY EFFECTIVE DATE ENTERED?                  
         BE    ICY0008             NO                                           
         CLC   DINEFDTE,KEY+21     YES - CHECK EFF DATE                         
         BL    ICY0004             NOT CORRECT EFFECTIVE DATE - SKIP            
ICY0008  EQU   *                                                                
         CLC   DINSTDT,MYLOWVAL    ANY START DATE ENTERED?                      
         BE    ICY0010             NO                                           
         CLC   DINSTDT,KEY+21      YES - CHECK START DATE                       
         BH    ICY0020             SKIP IT                                      
ICY0010  EQU   *                                                                
         CLC   DINENDT,MYLOWVAL    ANY END DATE ENTERED?                        
         BE    ICY0012             NO                                           
         CLC   DINENDT,KEY+21      YES - CHECK END DATE                         
         BL    ICY0020             SKIP IT                                      
ICY0012  EQU   *                                                                
         L     R6,AIO2             USE RECORD AREA TO STORE                     
         ST    R6,AIO                 INVENTORY HDR/MASTER                      
         USING RINVRECD,R6                                                      
*                                                                               
         MVC   RSTRTMST(4),KEY+28  SAVE DISK ADDRESS FOR RESTART                
         GOTO1 GETREC              RETRIEVE MASTER RECORD                       
*                                                                               
         DROP  R6                                                               
*                                                                               
ICY0013  EQU   *                                                                
         CLI   RSTRTRTN,6          RESTART AT 'EOD' POINT?                      
         BE    ICY0024             YES                                          
         CLI   RSTRTRTN,0          ANY OTHER RESTART?                           
         BNE   ICY0014             YES - DON'T RESET BOOK                       
         LA    R1,TWBOOK           A(1ST BOOK): RESET FOR EACH MASTER           
         ST    R1,ATWBOOK          A(BOOK IN PROCESS)                           
ICY0014  EQU   *                                                                
         GOTO1 RUNINV,DMCB,(R6)    PROCESS THIS INVENTORY NUMBER                
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    ICY0098             YES - WRAP UP AND RESTART                    
ICY0020  EQU   *                                                                
         LR    R3,R2                                                            
*                                                                               
*  IF REQUEST IS FOR AVERAGE OR COMBO, SKIP NEXT REQUEST ALSO                   
*                                                                               
         LA    R3,LDINSTOR(R3)     BUMP TO NEXT REQUEST                         
         CLI   DINAVGCM,C'0'       R2 COVERS REQ JUST PROCESSED                 
         BE    ICY0022             NO ACTION/NOT COMBO OR AVERAGE               
         LR    R2,R3               AVG/COMBO: SET R2 TO NEXT REQ                
         B     ICY0020             KEEP GOING                                   
ICY0022  EQU   *                                                                
         LR    R2,R3               NEXT REQUEST TO PROCESS                      
         ST    R2,ATWINVS          SAVE A(NEW REQUEST)                          
         CLI   0(R2),X'00'         ANY VALUE?                                   
         BNE   ICY0002             YES - PROCESS IT                             
*                                                                               
*  ALL INV #S PROCESSED, NO RESTART NEEDED.  SEND END-OF-DATA ELEMENT           
*      WITH FRAME, SET LAST-FRAME INDICATOR                                     
*                                                                               
ICY0024  EQU   *                                                                
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   ICY0026             ELEMENT FITS - PROCEED                       
         MVI   RSTRTRTN,6          SET RESTART TO 'EOD NEEDED'                  
         B     XIT                 END FOR RESTART                              
ICY0026  EQU   *                                                                
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     ICY0099             EXIT                                         
ICY0098  EQU   *                                                                
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDEMO          SAVE IT BACK                                 
         L     RF,ATWINVS          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWINVS          SAVE IT BACK                                 
ICY0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   USES THE PASSIVE KEY TO ACCESS AN INVENTORY HEADER.                         
*   RETRIEVES THE DEMO TRACKS AND RATIONALE ASSOCIATED WITH THAT                
*        HEADER                                                                 
*   TESTS TRACKS VS LIST OF BOOKS, ETC                                          
*   EXTRACTS DATA FROM ACCEPTED TRACKS                                          
*   FORMATS RETURN FRAME                                                        
*   CONTROLS FRAME-FULL RESTART REQUIREMENTS                                    
*                                                                               
RUNINV   NTR1                                                                   
*                                                                               
         L     R4,0(R1)            LOAD A(IOAREA)                               
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BE    RUIN0004            NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,1          RESTART AT PULLDEMS?                         
         BE    RUIN0024            YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,2          RESTART AT PULLRATS?                         
         BE    RUIN0040            YES - GO TO 'PULLRATS' RTN                   
         CLI   RSTRTRTN,4          RESTART AT SENDDESC FROM PULLDEMS?           
         BE    RUIN0024            YES - GO TO 'PULLDEMS' RTN                   
*                                                                               
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
RUIN0004 EQU   *                                                                
         MVI   INVFOUND,C'N'       SET 'NO INVENTORY FOUND' FLAG                
         L     R4,AIO2             USE SECOND AREA TO STORE                     
         ST    R4,AIO                INVENTORY HEADER/MASTER                    
         USING RINVRECD,R4                                                      
*                                                                               
         MVC   RSTRTMST(4),KEY+28  SAVE DISK ADDRESS FOR RESTART                
         GOTO1 GETREC                                                           
*                                                                               
*   CHECK MASTER EFFECTIVE DATE AGAINST FLIGHT DATES:  SKIP IF ANY OF           
*     THESE CONDITIONS APPLY:                                                   
*        1.  FLIGHT END   DATE  < INVENTORY EFFECTIVE START DATE                
*        2.  FLIGHT START DATE  > INVENTORY EFFECTIVE END   DATE                
*                                                                               
         CLC   TWENDT,RINVPEFF     FLIGHT END VS EFFECT START DATE              
         BL    RUIN0044            FLIGHT ENDS EARLIER - SKIP IT                
         OC    RINVPEFF+2(2),RINVPEFF+2   ANY END DATE FOR INVENTORY?           
         BZ    RUIN0008            NO END DATE - ACCEPTED                       
         CLC   TWSTDT,RINVPEFF+2   FLIGHT START VS EFFECT END DATE              
         BH    RUIN0044            LATER   - SKIP IT                            
*                                                                               
RUIN0008 EQU   *                                                                
         XC    EXTRAKEY,EXTRAKEY                                                
         MVC   EXTRAKEY(27),RINVKEY  SET KEY FROM HEADER RECORD                 
         MVI   DEMOFLAG,C'N'       SET FLAG FOR MASTER DATA                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         L     R2,AIO1             RESET ORIGINAL IO AREA                       
         ST    R2,AIO              FOR TRACK READING                            
         L     R3,ATWBOOK          A(BOOK IN PROCESS)                           
*                                                                               
*   NOTE:  FOR REQUEST IN WHICH DEMO FILTER 1 IS A SELECTION BASED              
*     ON RANK, ONLY THE FIRST BOOK IS CONSIDERED.  THIS WILL BE                 
*     CHANGED AT A LATER DATE TO EITHER USE THE FIRST BOOK, OR A                
*     SELECTED BOOK.                                                            
*                                                                               
RUIN0012 EQU   *                                                                
         LA    RE,SVCLST           A(CONVERSION TABLE)                          
RUIN0016 EQU   *                                                                
         CLC   =X'0000',0(R3)      END OF BOOKS?                                
         BE    RUIN0040            YES                                          
         CLC   3(1,RE),0(R3)       CONVERSION TBL VS BOOK ARRAY                 
         BE    RUIN0020            FOUND                                        
         LA    RE,L'SVCLST(RE)     BUMP A(CONVERSION TABLE)                     
         CLI   0(RE),X'FF'         END OF CONVERSION TABLE?                     
         BE    RUIN0036            YES - SKIP BOOK ARRAY ENTRY                  
*                                                                               
*   NOTE:  THIS IS AN ABORTABLE ENTRY - THE ITEM MUST BE FOUND IN               
* THE TABLE.  HOWEVER, NO ABORTS WILL BE PROGRAMMED IN.                         
*                                                                               
         B     RUIN0016            NO  - KEEP LOOKING                           
RUIN0020 EQU   *                                                                
         MVC   KEY,EXTRAKEY        SET KEY                                      
         MVC   KEY+24(1),2(RE)     INSERT SRCE FROM CONV TABLE                  
         MVC   KEY+25(2),1(R3)     INSERT BOOK FROM BOOK ARRAY                  
         GOTO1 HIGH                FIND THE RECORD                              
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   RUIN0032            NO  - BUMP THROUGH BOOK ARRAY                
RUIN0024 EQU   *                                                                
         L     RE,ATWINVS          A(REQUEST IN PROCESS)                        
         USING DIDSTORE,RE                                                      
*                                                                               
         CLI   DIDACT,C'='         'SELECTION FROM RANK' OPTION?                
         BNE   RUIN0028            NO                                           
         MVC   RANKDEMO(3),DIDDEMO     DEMO UPON WHICH RANKING IS DONE          
         MVC   RANKDEMO+3(3),=X'FFFFFF'                                         
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 SETRANK             SET UP ENTRY FOR RANKING                     
         B     XIT                 FINISHED WITH THIS INVENTORY #               
*                                                                               
RUIN0028 EQU   *                                                                
         MVI   RSTRTDEM,1          SET 'PULLDEMS FROM RUNINV' FLAG              
         BAS   RE,PULLDEMS         YES - TAKE DEMOS FROM TRACK                  
         BZ    RUIN0032            C=ZERO: SKIPPED BY DEMO FILTER               
         MVI   INVFOUND,C'Y'       SET 'INVENTORY FOUND' FLAG                   
RUIN0032 EQU   *                                                                
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    XIT                 YES - END AND RESTART                        
RUIN0036 EQU   *                                                                
         LA    R3,3(R3)            BUMP THROUGH BOOK ARRAY                      
         ST    R3,ATWBOOK          SAVE A(BOOK IN PROCESS)                      
         B     RUIN0012                                                         
RUIN0040 EQU   *                                                                
         CLI   INVFOUND,C'N'       ANY INVENTORY FOUND?                         
         BE    XIT                 NO  - DON'T PULL RATIONALE                   
         MVI   RSTRTRTS,2          SET 'PULLRATS FROM RUNINV' FLAG              
         BAS   RE,PULLRATS         YES - PULL RATIONALE FOR INV #               
         B     XIT                                                              
RUIN0044 EQU   *                                                                
         L     R2,AIO1             RESET ORIGINAL IO AREA                       
         ST    R2,AIO                FOR TRACK READING                          
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    PULLS THE VALUE FOR THE RANK DEMOGRAPHIC, AND BUILDS AN ENTRY              
*    FOR SORTING                                                                
*                                                                               
SETRANK  NTR1                                                                   
         MVC   RANKWORK+4(4),RSTRTMST   DISK ADDRESS OF MASTER                  
         MVC   RANKWORK+8(4),KEY+28     DISK ADDRESS OF TRACK                   
         GOTO1 GETREC                                                           
         LA    R6,DBDEMOB          A(DBLOCK)                                    
         USING DEMOD,R6                                                         
*                                                                               
         MVI   ACTFLAG,C'0'        SET ACTION FLAG TO NO ACTION                 
         MVC   DBFILE,=C'INV'      SET TO INVENTORY PROCESS                     
         L     RE,DBAREC           RESET A(1ST ELEMENT IN RECORD)               
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 DRAWDEMS,DMCB,RANKDEMO                                           
         MVC   RANKWORK(4),WORKAREA    VALUE OF DEMOGRAPHIC REQUESTED           
         L     R5,RANKADDR             A(NEXT SLOT)                             
         MVC   0(L'RANKWORK,R5),RANKWORK        INSERT RANK ELEMENT             
         LA    R5,L'RANKWORK(R5)       BUMP A(NEXT SLOT)                        
         ST    R5,RANKADDR                                                      
         ZIC   R5,RANKCTR          INCREMENT RANK COUNT                         
         LA    R5,1(R5)                                                         
         STC   R5,RANKCTR          SAVE IT BACK                                 
*                                                                               
*  INSERT A COUNT CHECK HERE!!  IF > 150???, DON'T INSERT!!!                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DRAWS DEMOGRAPHIC VALUES FROM A TRACK                                      
*    FORMATS THE DEMOGRAPHIC ITEM FOR THE FRAME                                 
*                                                                               
PULLDEMS NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BE    PUD0002             NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,1          RESTART AT PULLDEMS VIA RUNINV?              
         BE    PUD0018             YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,11         RESTART AT PULLDEMS VIA RANK?                
         BE    PUD0018             YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,4          RESTART: SENDDESC/PULLDEMS/RUNINV?           
         BE    PUD0004             YES - GO TO 'SENDDESC' RTN                   
         CLI   RSTRTRTN,41         RESTART: SENDDESC/PULLDEMS/RANK?             
         BE    PUD0004             YES - GO TO 'SENDDESC' RTN                   
*                                                                               
         DC    H'0'                                                             
*                                                                               
PUD0002  EQU   *                                                                
         MVC   RSTRTDET(4),KEY+28  SAVE DISK ADDR FOR RESTART                   
         MVC   SAVSRCBK,KEY+24     SAVE SOURCE/BOOK FOR AVG/COMBOS              
         GOTO1 GETREC              RETRIEVE DEMOGRAPHIC TRACK                   
         LA    R6,DBDEMOB          A(DBLOCK)                                    
         USING DEMOD,R6                                                         
         MVC   DBFILE,=C'INV'      SET TO INVENTORY PROCESS                     
         L     RE,DBAREC           RESET A(1ST ELEMENT IN RECORD)               
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVI   ACTFLAG,C'0'        SET ACTION FLAG TO 'NO ACTION'               
         CLI   INVRUN,0            DAYPART OR NUMBER RUN?                       
         BE    PUD0002D            ZERO  = DAYPART                              
*                                                                               
*  INVENTORY BY NUMBER MAY BE AVERAGING OR COMBO'ING                            
*                                                                               
         L     R2,ATWINVS          A(REQUEST IN PROCESS)                        
         USING DINSTORE,R2                                                      
*                                                                               
         MVC   ACTFLAG,DINAVGCM                                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
         L     RE,AINTEREC         A(INTERIM STORAGE) FOR DEMAND                
         XCEF  (RE),1000           INITIALIZE FOR COMBO/AVERAGE                 
PUD0002D EQU   *                                                                
         GOTO1 DRAWDEMS,DMCB,TWDEMOS                                            
*                                                                               
         CLI   INVRUN,0            DAYPART OR NUMBER RUN?                       
         BE    PUD0002E            ZERO  = DAYPART                              
*                                                                               
         CLI   ACTFLAG,C'0'        AVERAGE OR COMBO REQUEST?                    
         BE    PUD0002Z            NO                                           
         MVI   AVGWT,1             SET AVERAGE WEIGHT                           
         GOTO1 SUBCYCLE,DMCB,(R2)  PROCESS OTHER INV #S FOR                     
*                                  SAME BOOK                                    
         B     PUD0002Z                                                         
*                                                                               
*  DEMO FILTERS APPLY TO INVENTORY BY DAYPART ONLY                              
*                                                                               
PUD0002E EQU   *                                                                
         BAS   RE,DEMOFILT         TEST DEMO FILTERS                            
         BNZ   PUD0098             FILTERED OUT - SKIP IT                       
*                                                                               
         DROP  R6                                                               
*                                                                               
PUD0002Z EQU   *                                                                
         CLI   DEMOFLAG,C'Y'       DESCRIPTIVE DATA SENT?                       
         BE    PUD0006                                                          
         MVI   DEMOFLAG,C'Y'       SET TO 'SENT'                                
         MVI   RSTRTDES,4          SET 'SENDDESC FROM PULLDEMS/RUNINV'          
         CLI   RSTRTDEM,1          PULLDEMS ENTERED FROM RUNINV?                
         BE    PUD0004             YES                                          
         MVI   RSTRTDES,41         SET 'SENDDESC FROM PULLDEMS/RANK'            
PUD0004  EQU   *                                                                
         BAS   RE,SENDDESC         SEND DESCRIPTIVE DATA                        
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    PUD0099             YES - END AND RESTART                        
PUD0006  EQU   *                                                                
         MVI   DEMODATA,C'N'       TURN OFF COMBO DATA FLAG                     
         L     R6,AIO1             A(DEMO RECORD)                               
         GOTO1 GETELEM,DMCB,X'000000CD'                                         
         BNE   PUD0008             NO ELEMENT                                   
         TM    7(R6),X'80'         COMBO RECORD?                                
         BNO   PUD0008             NO                                           
         MVI   DEMODATA,C'Y'       SET COMBO DATA FLAG                          
PUD0008  EQU   *                                                                
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         L     R6,AIO              PROCESS DEMO TRACK                           
         USING RINVRECD,R6                                                      
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO03,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         MVC   DVCOMBO,DEMODATA    SET COMBO/NO COMBO FLAG                      
*                                                                               
*  CHECK SOURCE FOR ESTIMATE OR PROJECTION FLAG                                 
*                                                                               
         CLI   RINVKSRC,C'B'       ARB PROJECTED?                               
         BE    PUD0008A                                                         
         CLI   RINVKSRC,C'O'       NSI PROJECTED?                               
         BE    PUD0008A                                                         
         CLI   RINVKSRC,C'U'       SRC PROJECTED?                               
         BE    PUD0008A                                                         
         CLI   RINVKSRC,C'E'       ARB ESTIMATED?                               
         BE    PUD0008B                                                         
         CLI   RINVKSRC,C'R'       NSI ESTIMATED?                               
         BE    PUD0008B                                                         
         CLI   RINVKSRC,C'X'       SRC ESTIMATED?                               
         BE    PUD0008B                                                         
         LA    R3,DVBOOK           NO QUALIFIER - SET A(BOOK DATE)              
         B     PUD0008C                                                         
PUD0008A EQU   *                                                                
         MVI   DVBOOK,C'P'         INSERT 'P' FOR PROJECTED                     
         LA    R3,DVBOOK+1         SET A(BOOK DATE)                             
         B     PUD0008C                                                         
PUD0008B EQU   *                                                                
         MVI   DVBOOK,C'E'         INSERT 'P' FOR ESTIMATED                     
         LA    R3,DVBOOK+1         SET A(BOOK DATE)                             
PUD0008C EQU   *                                                                
         MVC   DATECONV,RINVKBK    SET UP DATE OF TRACK                         
         GOTO1 DATCON,DMCB,(3,DATECONV),(6,(R3))                                
         EDIT  TWTOTDEM,(2,DVDEMOCT),FILL=0                                     
         LA    R3,DVDEMLEN         A(DEMOS WITHIN ITEM)                         
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         LA    R4,WORKAREA         A(DEMOS: 4-BYTE ENTRIES)                     
         ZIC   R5,TWTOTDEM         TOTAL # OF DEMOS REQUESTED                   
PUD0010  EQU   *                                                                
         GOTO1 HEXOUT,DMCB,(R4),HEXOUTWK,4,=C'TOG'                              
         LA    R2,HEXOUTWK         A(HEXOUT DEMO VALUE)                         
         LA    R6,8                # OF CHARACTERS TO CHECK                     
PUD0012  EQU   *                                                                
         TM    0(R2),X'0F'         DECL BITS ON IN BYTE?                        
         BM    PUD0014             YES - SIGNIFICANT POSITION                   
         LA    R2,1(R2)            NO  - CHECK NEXT POSITION                    
         BCT   R6,PUD0012                                                       
         MVI   0(R3),C'0'          NO SIGNIFICANT POSITIONS                     
         B     PUD0016             JUST INSERT LENGTH OF ZERO                   
PUD0014  EQU   *                                                                
         EDIT  (R6),(1,(R3))       INSERT 1-CHAR LENGTH COUNT                   
         EX    R6,PUD0080          MOVE VALUE BY LENGTH                         
PUD0016  EQU   *                                                                
         LA    R6,1(R6)            BUMP A(O/P) BY LEN ATTR + LENGTH             
         AR    R3,R6               ADD TO A(O/P)                                
         LA    R4,4(R4)            NEXT DEMO VALUE                              
         BCT   R5,PUD0010          DO EACH                                      
*                                                                               
PUD0018  EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    PUD0020             NO                                           
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         L     R3,RSTRTLEN         RESET L(NEW ELEMENT)                         
         B     PUD0024                                                          
PUD0020  EQU   *                                                                
         LA    R4,ELTAREA          CALCULATE LENGTH OF NEW ITEM                 
         SR    R3,R4               R3=NEXT DEMO VALUE                           
         ST    R3,RSTRTLEN         SAVE LENGTH FOR POSS. RESTART                
PUD0024  EQU   *                                                                
         LA    R2,ITRDMFDV         INSERT ITEM TYPE                             
         GOTO1 PUTITEM,DMCB,(R2),(R3),ELTAREA                                   
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   PUD0099             ITEM FITS - EXIT                             
*                                                                               
         MVC   RSTRTRTN,RSTRTDEM   SET RESTART AS 'PULLDEMS'                    
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     PUD0099                                                          
*                                                                               
PUD0080  MVC   1(0,R3),0(R2)                                                    
*                                                                               
PUD0098  EQU   *                                                                
         SR    R6,R6               SET CC=ZERO: SKIPPED                         
         B     XIT                                                              
PUD0099  EQU   *                                                                
         LTR   RC,RC               SET CC!=ZERO: ALL OTHER CASES                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SETS UP INVENTORY MASTER KEY FOR NEXT REQUEST, WHICH IS TO BE              
*    AVERAGED OR COMBO'D WITH THE CURRENT REQUEST.  CHECKS FILTERS              
*    TO ENSURE THAT MASTER IS ACCEPTABLE.  THEN, TRACK FOR BOOK                 
*    IS ACCESSED TO AVERAGE OR COMBO..                                          
*                                                                               
SUBCYCLE NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
SUB0002  EQU   *                                                                
         LA    R2,LDINSTOR(R2)     POINT TO NEXT REQUEST                        
         XC    KEY,KEY             ESTABLISH ACTIVE KEY                         
         MVI   KEY,X'12'                                                        
         MVC   KEY+10(2),SIGNON2C  INSERT REP CODE                              
         MVC   KEY+12(5),TWSTAT    INSERT STATION + MEDIA                       
*                                                                               
         USING DINSTORE,R2                                                      
*                                                                               
         MVC   KEY+17(3),DININV#   INSERT INVENTORY #                           
         GOTO1 HIGH                                                             
         B     SUB0006                                                          
SUB0004  EQU   *                                                                
         GOTO1 SEQ                                                              
SUB0006  EQU   *                                                                
         CLC   KEY(20),KEYSAVE     SAME KEY?                                    
         BNE   SUB0020             NO  - GET NEXT REQUEST                       
         CLC   DINEFDTE,MYLOWVAL   ANY EFFECTIVE DATE ENTERED?                  
         BE    SUB0008             NO                                           
         CLC   DINEFDTE,KEY+21     YES - CHECK EFF DATE                         
         BL    SUB0004             NOT CORRECT EFFECTIVE DATE - SKIP            
SUB0008  EQU   *                                                                
         CLC   DINSTDT,MYLOWVAL    ANY START DATE ENTERED?                      
         BE    SUB0010             NO                                           
         CLC   DINSTDT,KEY+21      YES - CHECK START DATE                       
         BH    SUB0020             SKIP IT                                      
SUB0010  EQU   *                                                                
         CLC   DINENDT,MYLOWVAL    ANY END DATE ENTERED?                        
         BE    SUB0012             NO                                           
         CLC   DINENDT,KEY+21      YES - CHECK END DATE                         
         BL    SUB0020             SKIP IT                                      
SUB0012  EQU   *                                                                
         MVC   KEY+24(3),SAVSRCBK  INSERT SOURCE/BOOK INTO KEY                  
         GOTO1 READ                READ DEMO TRACK                              
         CLC   KEY(27),KEYSAVE     TRACK FOUND?                                 
         BNE   SUB0020             NO TRACK FOR SOURCE/BOOK                     
         GOTO1 GETREC              RETRIEVE THE DEMO TRACK                      
         ZIC   R1,AVGWT            INCREMENT AVERAGE WEIGHT                     
         LA    R1,1(R1)                                                         
         STC   R1,AVGWT                                                         
         GOTO1 DRAWDEMS,DMCB,TWDEMOS                                            
SUB0020  EQU   *                                                                
         CLI   DINAVGCM,C'0'       AVERAGE/COMBO NEXT?                          
         BNE   SUB0002             YES - DO IT                                  
         GOTO1 LASTCALC,DMCB,WORKAREA    CALCULATE FINAL AVERAGE                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    APPLIES FILTERS TO THE DEMO FIGURES PULLED.  CC=ZERO IS RETURNED           
*    IF VALUE PASSES FILTER.                                                    
*                                                                               
DEMOFILT NTR1                                                                   
         L     R2,ATWINVS                                                       
         USING DIDSTORE,R2                                                      
*                                                                               
         TM    DIDFLAGS,X'20'      DEMO FILTER 1 ENTERED?                       
         BNO   DMF0098             NO  - FINISHED: CC=ZERO                      
         LA    R3,DIDDEMO          ISOLATE DEMO VALUE POSITIONALLY              
         BAS   RE,DEMOISOL                                                      
         LA    R5,WORKAREA         A(DEMO VALUE ARRAY)                          
         AR    R5,R4               A(OFFSET FOR DEMO)                           
         CLI   DIDACT,C'>'         MINIMUM DEMO VALUE?                          
         BNE   DMF0002             NO                                           
         CLC   0(4,R5),DIDVAL1     CHECK AGAINST VALUE 1                        
         BL    DMF0097             LESS THAN MIN:  CC!=ZERO                     
         B     DMF0010             PASSED - CHECK 2ND FILTER                    
DMF0002  EQU   *                                                                
         CLI   0(R3),C'<'          MAXIMUM DATA VALUE                           
         BNE   DMF0010             ACCEPTED                                     
*                                                                               
*  NOTE:  ONLY MIN/MAX DEMO FILTERS IMPLEMENTED HERE                            
*        NO RATES ARE AVAILABLE                                                 
*        DEMO RANKING NOT IMPLEMENTED AT THIS TIME                              
*                                                                               
         CLC   0(4,R5),DIDVAL1     CHECK AGAINST VALUE 1                        
         BH    DMF0097             MORE THAN MAX: CC!=ZERO                      
DMF0010  EQU   *                                                                
         TM    DIDFLAGS,X'08'      DEMO FILTER 2 ENTERED?                       
         BNO   DMF0098             NO  - FINISHED:  CC=ZERO                     
         LA    R3,DIDVAL2          POSSIBLE LOCATION OF 2ND DEMO                
         TM    DIDFLAGS,X'01'      DEMO FILT 1 HAVE 2 VALUES?                   
         BNO   DMF0012             NO  -                                        
         LA    R3,4(R3)            YES - SKIP 2ND VALUE                         
DMF0012  EQU   *                                                                
         BAS   RE,DEMOISOL                                                      
         LA    R5,WORKAREA         A(DEMO VALUE ARRAY)                          
         AR    R5,R4               A(OFFSET FOR DEMOS)                          
         LA    R3,3(R3)            POINT TO ACTION                              
         CLI   0(R3),C'>'          MINIMUM DATA VALUE?                          
         BNE   DMF0014             NO                                           
         CLC   0(4,R5),1(R3)       CHECK AGAINST VALUE 2                        
         BL    DMF0097             LESS THAN MIN:  CC!=ZERO                     
         B     DMF0098             OK:  CC=ZERO                                 
DMF0014  EQU   *                                                                
         CLI   0(R3),C'<'          MAXIMUM DATA VALUE?                          
         BNE   DMF0098             ACCEPTED: SEE PREVIOUS NOTE                  
         CLC   0(4,R5),1(R3)       CHECK AGAINST VALUE 2                        
         BH    DMF0097             MORE THAN MAX:  CC!=ZERO                     
         B     DMF0098             OK:  CC=ZERO                                 
DMF0097  EQU   *                                                                
         LTR   RC,RC               SET CC!=ZERO                                 
         B     XIT                                                              
DMF0098  EQU   *                                                                
         SR    RF,RF               SET CC=ZERO                                  
         B     XIT                                                              
         SPACE 5                                                                
* THIS ROUTINE:                                                                 
*  DETERMINES THE OFFSET INTO THE DEMO VALUE ARRAY (WORKAREA) BY                
*        USING THE REQUEST'S DEMO CODE TO FIND THE CODE IN THE                  
*        REQUESTED DEMOS, AND TO THEN CALCULATE THE POSITIONAL                  
*        RELATIONSHIP IN THE VALUE ARRAY.                                       
*      R3  =  A(DEMO BEING SOUGHT)                                              
*      R4  =  OFFSET VALUE RETURNED                                             
*                                                                               
DEMOISOL EQU   *                                                                
         LA    R4,TWDEMOS+9        A(DEMO CODES) - SKIP RTG,HUT,SHR             
         LA    R5,TW#DEMOS         NUMBER OF DEMOS REQUESTED                    
DISO0010 EQU   *                                                                
         CLC   0(3,R3),0(R4)       DEMO FOUND?                                  
         BE    DISO0012            YES                                          
         LA    R4,3(R4)            NO  - BUMP DEMO LIST                         
         BCT   R5,DISO0010                                                      
         DC    H'0'                SHOULD HAVE BEEN IN LIST                     
DISO0012 EQU   *                                                                
         LA    R4,TW#DEMOS                                                      
         SR    R4,R5               WHICH DEMO FOUND (ZERO RELATIVE)             
         SLA   R4,2                WHOLE-WORD OFFSET                            
         LA    R4,12(R4)           SKIP RATING,HUT,SHARE                        
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    FILTERS RATIONALE                                                          
*    IF ACCEPTED, CONSTRUCTS RATIONALE ITEMS, AND INSERTS THEM INTO             
*        THE FRAME                                                              
*                                                                               
PULLRATS NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BE    PUR0001             NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,2          RESTART AT PULLRATS VIA RUNINV?              
         BE    PUR0026             YES - GO TO 'PULLRATS' RTN                   
         CLI   RSTRTRTN,21         RESTART AT PULLRATS VIA RANK?                
         BE    PUR0026             YES - GO TO 'PULLRATS' RTN                   
*                                                                               
         DC    H'0'                                                             
*                                                                               
PUR0001  EQU   *                                                                
*                                                                               
         MVC   KEY,EXTRAKEY        RESET KEY FOR RATIONALE                      
         MVI   KEY+24,X'FF'        SET FOR RATIONALE                            
PUR0002  EQU   *                                                                
         GOTO1 HIGH                                                             
         B     PUR0006                                                          
PUR0004  EQU   *                                                                
         GOTO1 SEQ                 RETRIEVE NEXT RATIONALE KEY                  
PUR0006  EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME INV #?                                  
         BNE   PUR0099             NO  - FINISHED                               
         MVC   RSTRTDET(4),KEY+28  SAVE DISK ADDR FOR RESTART                   
         GOTO1 GETREC              RETRIEVE RATIONALE RECORD                    
         L     R6,AIO1             A(RATIONALE RECORD)                          
         USING RINVFEL,R6          LOCATE TEXT FILTER ELEMENT                   
         GOTO1 GETELEM,DMCB,2                                                   
         BNE   PUR0022             NO FILTER ELEMENT - USE RECORD               
         CLI   RINVFSRC,0          ANY SERVICE FILTER?                          
         BE    PUR0008             NO                                           
         CLC   TWSERV,RINVFSRC     SAME SERVICE?                                
         BNE   PUR0004             NO  - SKIP IT                                
PUR0008  EQU    *                                                               
         CLI   RINVFYR,0           ANY DATE FILTER?                             
         BE    PUR0012             NO                                           
         LA    R1,TWBOOK           YES - SEARCH REQUESTED BOOKS                 
PUR0010  EQU   *                                                                
         CLI   0(R1),0             END OF BOOKS?                                
         BE    PUR0004             YES - SKIP IT                                
         CLC   1(2,R1),RINVFYR     BOOK FOUND?                                  
         BNE   PUR0014             NO                                           
PUR0012  EQU   *                                                                
         CLI   RINVFBKT,0          BOOK TYPE FILTER?                            
         BE    PUR0016             NO  - NO MORE BOOK TESTING                   
         MVC   DMCB(1),0(R1)                                                    
         NI    DMCB,X'3E'          CLEAR SUPPRESS CPM BIT/SVC BITS              
         MVC   DMCB+1(1),RINVFBK                                                
         NI    DMCB+1,X'3E'        CLEAR SUPPRESS CPM BIT/SVC BITS              
         CLC   DMCB+1(1),DMCB      SAME?                                        
         BE    PUR0016             YES                                          
PUR0014  EQU   *                                                                
         LA    R1,3(R1)            BUMP TO NEXT BOOK                            
         B     PUR0010             CHECK NEXT BOOK                              
PUR0016  EQU   *                                                                
*                                                                               
*   IN AVAIL PRINTER FILTER LOGIC, AT THIS POINT THERE IS A NON-                
*     FUNCTIONAL CHECK FOR LOCAL TEXT.  IF EVER IMPLEMENTED, IT                 
*     GOES HERE.                                                                
*                                                                               
         ZIC   R1,RINVFLEN         CHECK FOR DEMO FILTERS                       
         SH    R1,=H'10'           SUBTRACT L(EVERYTHING ELSE IN ELEM)          
         BZ    PUR0022             NO DEMO FILTERS - PROCESS IT                 
         ZIC   R2,TW#DEMOS         # DEMOS IN REQUEST                           
         LTR   R2,R2               SET CONDITION CODE                           
         BZ    PUR0004             NO DEMOS - CAN'T USE FILT TEXT               
         LA    R3,TWDEMOS+9        A(DEMOS REQUESTED)                           
PUR0018  EQU   *                                                                
         LA    R4,RINVFDEM                                                      
         LR    R5,R1               FILTER ELEMENT DEMO COUNTER                  
PUR0020  EQU   *                                                                
         CLC   2(1,R3),0(R4)       DEMO CATEGORY MATCH?                         
         BE    PUR0022             YES - PROCESS IT                             
         LA    R4,1(R4)            BUMP TO NEXT FILTER DEMO                     
         BCT   R5,PUR0020          SCAN ALL FILTER DEMOS                        
*                                                                               
*  REQUESTED DEMO NOT FOUND IN FILTER DEMO LIST.  BUMP TO NEXT                  
*     REQUESTED DEMO, AND SCAN FILTER DEMO LIST AGAIN.                          
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT REQUESTED DEMO                  
         BCT   R2,PUR0018          RESCAN FILTER DEMOS                          
*                                                                               
*  NO REQUESTED DEMOS FOUND IN FILTER DEMO LIST.  SKIP THIS RAT'L               
*                                                                               
         B     PUR0004                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
PUR0022  EQU   *                                                                
         USING RINVTEL,R6          DSECT FOR RATIONALE ENTRIES                  
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO04,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         MVI   RSTRTRAT,0          INITIALIZE COUNTER                           
         GOTO1 GETELEM,DMCB,1      GET FIRST TEXT ELEMENT, IF ANY               
         B     PUR0025                                                          
PUR0024  EQU   *                                                                
         GOTO1 NEXTELEM,DMCB       GET NEXT TEXT ELEMENT, IF ANY                
PUR0025  EQU   *                                                                
         BNE   PUR0004             NO MORE - GET NEXT RAT'L RECORD              
         ZIC   R5,RSTRTRAT         INCREMENT COUNTER FOR LINE                   
         LA    R5,1(R5)                                                         
         STC   R5,RSTRTRAT         PUT IT BACK                                  
         XC    ELTAREA,ELTAREA                                                  
         L     R5,AIO                                                           
         USING RINVRECD,R5                                                      
         EDIT  (2,RINVKTXT),(2,RIRATL#),FILL=0                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         EDIT  (1,RINVTLIN),(2,RIRATLIN),FILL=0                                 
         ZIC   R5,RINVTLEN         L(TEXT ENTRY) + 6                            
         SH    R5,=H'6'            SUBTRACT L(CONTROL STUFF)                    
         BZ    PUR0024             NO LENGTH = NO OUTPUT                        
         LA    R4,RIRATL           A(TEXT IN ITEM)                              
*                                                                               
         EX    R5,PUR0080          MOVE TEXT BY LENGTH                          
*                                                                               
PUR0026  EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    PUR0028             NO                                           
         LA    R2,ELTAREA          RESET A(ELT BUILD AREA)                      
         GOTO1 READ                READ TO REESTABLISH SEQ                      
         L     R6,AIO1             RESET A(RATIONALE RECORD)                    
         BAS   RE,RESETRAT         REPOSITION TO PROPER LINE                    
         L     R6,DUB              PASS A(NEW ELEMENT) BACK                     
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         L     R5,RSTRTLEN         RESET L(NEW ELEMENT)                         
         B     PUR0030                                                          
PUR0028  EQU   *                                                                
         LA    R5,4(R5)            SET ITEM LENGTH                              
         ST    R5,RSTRTLEN         SAVE LENGTH FOR POSS. RESTART                
PUR0030  EQU   *                                                                
         LA    R3,ITRDMFRA         SET ITEM TYPE                                
         GOTO1 PUTITEM,DMCB,(R3),(R5),ELTAREA                                   
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   PUR0024             ELEMENT FITS - GET NEXT LINE                 
*                                                                               
         MVC   RSTRTRTN,RSTRTRTS   SET RESTART AS 'PULLRATS'                    
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     PUR0099             SHUT DOWN THIS RUNNNNNNNNN                   
*                                                                               
PUR0080  MVC   0(0,R4),RINVTEXT                                                 
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
PUR0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    REPOSITIONS TO THE PROPER LINE WITHIN THE RATIONALE ON A                   
*    RESTART                                                                    
*                                                                               
RESETRAT NTR1                                                                   
         ZIC   R5,RSTRTRAT         LOAD COUNTER                                 
         GOTO1 GETELEM,DMCB,1      GET FIRST TEXT ELEMENT                       
         B     RRAT0004                                                         
RRAT0002 EQU   *                                                                
         GOTO1 NEXTELEM,DMCB       GET NEXT  TEXT ELEMENT                       
RRAT0004 EQU   *                                                                
         BCT   R5,RRAT0002                                                      
         ST    R6,DUB              SAVE A(NEW ELEMENT) TO PASS BACK             
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CALCULATES THE FINAL AVERAGE (IF NOT COMBO), THEN DEVELOPS THE             
*        DEMOS BASED ON THE LIST OF DEMOGRAPHICS.                               
*        P1  =  A(FINAL DEMO VALUES)                                            
*                                                                               
LASTCALC NTR1                                                                   
*                                                                               
         LA    R6,DBDEMOB             SET A(DEMO BLOCK)                         
         USING DEMOD,R6                                                         
*                                                                               
         MVC   STORE16(4),0(R1)       SAVE P1 (A(FINAL DEMO VALUES))            
*                                                                               
         CLI   ACTFLAG,C'2'        COMBO REQUEST?                               
         BE    LAC0004             YES - DON'T AVERAGE                          
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHFCTR+3(1),AVGWT      SET TOTAL WEIGHT                         
*                                                                               
         L     R1,AINTEREC         **TEST**                                     
         GOTO1 DEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC                
*                                                                               
LAC0004  EQU   *                                                                
         MVC   STORE16+4(4),DBAREC     SAVE ORIGINAL VALUES                     
         MVC   STORE16+8(4),DBAQUART                                            
         MVC   DBAREC,AINTEREC         SET A(INTERIM RECORD)                    
         L     RE,AINTEREC             SET A(1ST ELEMENT)                       
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVC   DMCB+8(4),STORE16       LOAD A(FINAL OUTPUT)                     
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',TWDEMOS),DBDEMOB                               
*                                                                               
         MVC   DBAREC,STORE16+4        RESET ORIGINAL VALUES                    
         MVC   DBAQUART,STORE16+8                                               
LAC0099  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CONVERTS A RETRIEVED INVENTORY RECORD TO STANDARD FORMAT, THEN             
*        DRAWS DEMOGRAPHICS FROM IT, LEAVING THEM IN 'WORKAREA'.                
*        P1   =   ADDRESS OF DEMO CODE STRING                                   
*                                                                               
DRAWDEMS NTR1                                                                   
         LA    R6,DBDEMOB          A(DEMO DBLOCK)                               
         USING DEMOD,R6                                                         
         XC    WORKAREA(200),WORKAREA                                           
*                                                                               
         L     R5,0(R1)            LOAD OPTION FOR RANK/REGULAR                 
*                                                                               
*                                                                               
*   CONVERT RETRIEVED RECORD (DBAREC) TO STANDARD FORMAT                        
*                                                                               
DRDM0002 EQU   *                                                                
         XCEFL IUNWORK,2016        INITIALIZE IUN AREA                          
         MVC   IUNWORK(1),DBFILE                                                
         MVC   IUNWORK+20,=H'24'   DUMMY RECORD LENGTH                          
         GOTO1 =V(REGETIUN),DMCB,(9,DBLOCK),IUNWORK+1000,RR=RELO                
         MVC   DBNUMVLS,=H'320'                                                 
         LA    R4,IUNWORK+1000                                                  
         USING IUNREC,R4                                                        
*                                                                               
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                       
         L     R3,DBAQUART         SAVE INITIAL VALUES                          
         L     R4,DBAREC                                                        
*                                                                               
*  RESET DBAREC TO BEGINNING OF IUNWORK.  RECORD WILL BE REBUILT FROM           
*    IUNWORK+1000 INTO IUNWORK                                                  
*                                                                               
         LA    RE,IUNWORK                                                       
         ST    RE,DBAREC           RESET RECORD ADDRESS IN DBLOCK               
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART         RESET STARTING ELEMENT ADDRESS               
         GOTO1 DEMAINT,DMCB,=C'PUT',DBLOCK,IUNWORK+1000,OFORMAT                 
*                                                                               
         DROP  R4                                                               
*                                                                               
         CLI   ACTFLAG,C'0'        'NO ACTION' REQUEST?                         
         BNE   DRDM0016            NO  - AVERAGE/COMBO REQUEST                  
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R5)),DBDEMOB,WORKAREA                         
         B     DRDM0020                                                         
*                                                                               
DRDM0016 EQU   *                                                                
*                                                                               
*  ACCUMULATE REBUILT RECORD FROM IUNWORK INTO A(INTEREC)                       
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         MVI   MTHFCTR+3,1         SET WEIGHT TO 1                              
         MVC   MTHOSRC,=C'NSI'                                                  
         LA    R1,DBLOCK                                                        
         ST    R1,MTHCFACS                                                      
         MVC   MTHIFIL,DBFILE      FILE FORMAT INPUT                            
         MVC   MTHOFIL,DBFILE      FILE FORMAT OUTPUT                           
         GOTO1 DEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                     
*                                                                               
DRDM0020 EQU   *                                                                
         ST    R3,DBAQUART         RESET ORIGINAL ADDRESSES                     
         ST    R4,DBAREC                                                        
*                                                                               
         DROP  R6                                                               
*                                                                               
DRDM0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    EXTRACTS DESCRIPTIVE INFORMATION FROM DEMO HEADER RECORD, WHICH            
*        IS STORED IN THE SECOND IO AREA (AIO2), AND INSERTS IT INTO            
*        THE RETURN FRAME.                                                      
*                                                                               
SENDDESC NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BNE   SDE0006             YES - SKIP TO INSERT ELEMENT                 
*                                                                               
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         L     R6,AIO2             PROCESS HEADER IN IO AREA 2                  
         USING RINVRECD,R6                                                      
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO01,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         L     R3,ATWINVS          INSERT DAYPART IN PROCESS                    
         USING DIDSTORE,R3                                                      
*                                                                               
         MVC   DHDAYPT,DIDDPT                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVC   DHSTAT(5),RINVKSTA  INSERT STATION                               
         MVI   DHREPEAT,C'N'       SET REPEAT FLAG 'NO'                         
         MVI   DHCOMPET,C'N'                                                    
*                                                                               
*  NOTE:  TEMPORARILY (UNTIL THIS MECHANISM IS DEFINED) ALL ELEMENTS            
*        ARE RETURNED AS COMPETITIVE=N                                          
*                                                                               
*  CONSTRUCT EBCDIC REPRESENTATION OF INVENTORY #                               
*                                                                               
         EDIT  (1,RINVKQTR),(2,DHINV#),FILL=0                                   
         MVC   DHINV#+2(2),RINVKDAY         INSERT DAY + LENGTH                 
*                                                                               
*  INSERT FIRST (OR ONLY) LINE OF INVENTORY TITLE INTO ITEM                     
*    AFTER DROPPING TRAILING SPACES                                             
*                                                                               
         LA    R3,RINVPROG+26      A(LAST CHAR, 1ST NAME FIELD)                 
         LA    R4,27               MAX SIZE                                     
SDE0002  EQU   *                                                                
         CLI   0(R3),C' '          SKIP IF SPACE                                
         BNE   SDE0004             NOT SPACE - USE LENGTH                       
         BCTR  R3,0                DECREMENT POINTER                            
         BCT   R4,SDE0002          CHECK PREVIOUS POSITION                      
         B     SDE0006             NO TITLE?  HOW ODD!                          
SDE0004  EQU   *                                                                
         LA    R3,DHTITLE                                                       
         EX    R4,SDE0080          MOVE TITLE BY LENGTH                         
SDE0006  EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    SDE0008             NO                                           
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         L     R4,RSTRTLEN         RESET L(TITLE) FOR RESTART                   
SDE0008  EQU   *                                                                
         LA    R2,ITRDMFDS         SET ITEM TYPE                                
         LA    R3,LDHDATA          SET ITEM LENGTH                              
         AR    R3,R4               ADD LENGTH OF TITLE                          
         ST    R4,RSTRTLEN         SAVE L(TITLE) FOR POSS. RESTART              
         GOTO1 PUTITEM,DMCB,(R2),(R3),ELTAREA                                   
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   SDE0099             ELEMENT FITS - EXIT ROUTINE                  
*                                                                               
         MVC   RSTRTRTN,RSTRTDES   SET 'RESTART AT SENDDESC'                    
*                                  ORIGINATING AT 'RTN' FLAG                    
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     SDE0099             EXIT                                         
*                                                                               
SDE0080  MVC   0(0,R3),RINVPROG                                                 
*                                                                               
SDE0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  LOCAL WORKAREA FOR TABLES, ETC.  THIS IS LOCATED WELL WITHIN THE             
*    2ND COVERING REGISTER, AND DOES NOT REQUIRE RELOCATION FOR USE             
*    WITH THE NMODS, AS COVERING REGISTER RA WILL NOT BE CHANGED.               
*                                                                               
         SPACE 4                                                                
       ++INCLUDE RESVCTAB                                                       
       SPACE 4                                                                  
DATECONV DS    CL2                 THREE-BYTE DATE FIELD                        
DTFILLER DC    XL1'1'              DAY SET TO 1 ALWAYS                          
MYSPACES DC    CL20'                    '                                       
MYLOWVAL DC    XL4'00000000'                                                    
RELO     DS    A                                                                
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
*                                                                               
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
WORKAREA DS    CL600               WORK SPACE FOR DEMO CALCS, ETC               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    ESTABLISHES THE REQUEST IN THE WORK AREA.                                  
*    SETS POINTERS                                                              
*    MAKES 2ND THRU NTH CALL RESTART TRANSPARENT                                
*                                                                               
*                                                                               
         DS    0H                                                               
PROCRQST NMOD1 0,**RQST**                                                       
*                                                                               
         L     RC,0(R1)            RESET A(WORK SPACE)                          
PR0002   EQU   *                                                                
         GOTO1 GETITEM             RETRIEVE ITEM FROM INPUT FRAME               
         BNE   PR0099              ALL ITEMS DONE - SET MISCELLAN.              
         L     R1,TYPENUM          DETERMINE TYPE OF ITEM                       
         LA    R2,ITRIMFDS                                                      
         CR    R1,R2               BASIC HEADER INFO?                           
         BNE   PR0004              NO                                           
         BAS   RE,BASHDINF         YES                                          
         B     PR0002                                                           
PR0004   EQU   *                                                                
         LA    R2,ITRIMFBK                                                      
         CR    R1,R2               BOOK INFO?                                   
         BNE   PR0006              NO                                           
         BAS   RE,BOOKIN           YES                                          
         B     PR0002                                                           
PR0006   EQU   *                                                                
         LA    R2,ITRIMFDC                                                      
         CR    R1,R2               DEMOGRAPHIC CODES?                           
         BNE   PR0008              NO                                           
         BAS   RE,DEMOCODE         YES                                          
         B     PR0002                                                           
PR0008   EQU   *                                                                
         LA    R2,ITRIDREQ                                                      
         CR    R1,R2               INVENTORY BY DAYPARTS?                       
         BNE   PR0010              NO                                           
         BAS   RE,INVBYDPT         YES                                          
         B     PR0002                                                           
PR0010   EQU   *                                                                
         LA    R2,ITRINREQ                                                      
         CR    R1,R2               INVENTORY BY NUMBER?                         
         BNE   PR0012              NO                                           
         BAS   RE,INVBYNUM         YES                                          
         B     PR0002                                                           
PR0012   EQU   *                                                                
         LA    R2,ITEOD                                                         
         CR    R1,R2               END OF DATA?                                 
         BNE   PR0014              NO                                           
         B     PR0099              YES                                          
PR0014   EQU   *                                                                
*   NO PROCESSING FOR AN UNRECOGNIZED TYPE                                      
         B     PR0002                                                           
PR0099   EQU   *                                                                
         B     XITIT                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SETS UP THE BASIC DESCRIPTIVE FIELDS FROM THE HEADLINE, AND                
*    TRANSLATES DATES INTO FORMAT REQUIRED FOR INVENTORY RETRIEVAL.             
*                                                                               
BASHDINF NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT11IN01,R2                                                      
         MVC   TWSTAT,BHSTAT       STATION CALL LETTERS                         
         CLI   TWSTAT+4,C' '       ANY MEDIA ENTERED?                           
         BNE   BH0003              YES                                          
         MVI   TWSTAT+4,C'T'       NO  - INSERT TV                              
BH0003   EQU   *                                                                
         MVC   TWSERV,BHSERV       SERVICE                                      
         GOTO1 DATVAL,DMCB,BHSTDT,WORKAREA                                      
         GOTO1 DATVAL,DMCB,BHENDT,WORKAREA+6                                    
*                                                                               
*  DATES HAVE BEEN VALIDATED AT PC.  NO ERROR CHECKING IS DONE HERE.            
*                                                                               
         GOTO1 DATCON,DMCB,WORKAREA,(2,TWSTDT)                                  
         GOTO1 DATCON,DMCB,WORKAREA+6,(2,TWENDT)                                
         MVC   TWCOMPET,BHCOMPET   SET COMPETITIVE FLAG                         
         MVC   TWRATES,BHRATES     SET RATES FLAG                               
         B     XITIT                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CALLS A ROUTINE TO REPLACE 'PJ1/PJ2' INDICATORS                            
*    BUILDS A PHONY HEADER TO ENABLE 'BOOKVAL' TO WORK                          
*    INSERTS DATA FROM ITEM TO PERMIT TRANSLATION VIA BOOKVAL                   
*    HANDLES THE PJ1/PJ2 INDICATORS IMBEDDED WITHIN STRING THAT                 
*      WOULD BE REJECTED BY BOOKVAL                                             
*                                                                               
BOOKIN   NTR1                                                                   
         BAS   RE,CHECKPJS         REPLACE PJ1/PJ2 WITH F50/F51                 
*                                                                               
         L     R2,ADATA            A(ITEM)                                      
         USING CT11IN02,R2                                                      
*                                                                               
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         L     R3,DATALEN               L(INPUT ITEM DATA)                      
         LR    R4,R3                    SAVE L(INPUT ITEM DATA)                 
         LA    R3,2(R3)                 ADD 2 FOR 'SERVICE+,'                   
         STC   R3,WORKAREA+5            L('FIELD' HEADER)                       
         BCTR  R4,0                     DECREMENT FOR EXECUTE                   
         MVC   WORKAREA+8(1),TWSERV     STRING SERVICE INTO 'FIELD'             
         MVI   WORKAREA+9,C','          STRING COMMA INTO 'FIELD'               
         LA    R5,WORKAREA+10           A(DATA IN 'FIELD')                      
*                                                                               
         EX    R4,BI0080           INSERT DATA IN 'FIELD'                       
*                                                                               
         PRINT GEN                                                              
         GOTO1 BOOKVAL,DMCB,WORKAREA,(8,TWBOOK),SCANNER                         
         PRINT NOGEN                                                            
         B     XITIT                                                            
*                                                                               
BI0080   MVC   0(0,R5),BLBOOKS                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SCANS THE BOOKLIST FIELD, REPLACING 'PJ1/PJ2' INDICATORS                   
*       WITH 'F50(FEB50) AND F51(FEB51) RESPECTIVELY.  THIS                     
*       WILL PERMIT BOOKVAL TO FUNCTION CORRECTLY, AND PROVIDE                  
*       DATES UPON WHICH THE UPGRADE CAN BE KEYED.                              
*                                                                               
CHECKPJS NTR1                                                                   
         L     R1,DATALEN          L(INPUT ITEM DATA)                           
         LR    R2,R1               SET UP COMPARE LOOP CTR                      
         BCTR  R2,0                                                             
         BCTR  R2,0                                                             
         L     R3,ADATA            A(INPUT ITEM DATA)                           
CPJ0004  EQU   *                                                                
         CLC   0(3,R3),=C'PJ1'     PROJECTION1 FOUND?                           
         BNE   CPJ0006             NO                                           
         MVC   0(3,R3),=C'F50'     YES - REPLACE WITH FEB50                     
CPJ0006  EQU   *                                                                
         CLC   0(3,R3),=C'PJ2'     PROJECTION2 FOUND?                           
         BNE   CPJ0008             NO                                           
         MVC   0(3,R3),=C'F51'     YES - REPLACE WITH FEB51                     
CPJ0008  EQU   *                                                                
         LA    R3,1(R3)            CHECK NEXT THREE POSITIONS                   
         BCT   R2,CPJ0004                                                       
         B     XITIT               FINISHED                                     
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    PROCESSES EACH 5-BYTE PC INPUT FIELD, CONVERTING THE 3-BYTE                
*      EBCDIC REPRESENTATION OF THE DEMO # TO A 1-BYTE BINARY                   
*                                                                               
DEMOCODE NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT11IN03,R2                                                      
         MVI   TWDEMOS,X'FF'        FILL BOOK FIELD WITH X'FF'                  
         MVC   TWDEMOS+1(LTWDEMOS-1),TWDEMOS                                    
         MVC   TWDEMOS(9),=X'00D90100E20100D701'                                
*                                                                               
*  ABOVE LOADS FIRST THREE DEMOS AS RATING, HUT, SHARE, WHICH ARE               
*     ALWAYS CALLED FOR                                                         
*                                                                               
         L     R5,DATALEN          L(INPUT ITEM DATA)                           
         SR    R4,R4               CALCULATE # OF 5-BYTE ENTRIES                
         LA    R3,5                                                             
         DR    R4,R3               DIVIDE LEN BY 5 - RESULT IN R5               
         STC   R5,TW#DEMOS         SAVE COUNT                                   
         LR    R3,R5               CALCULATE TOTAL # DEMOS                      
         SLL   R3,1                DOUBLE FOR REG DEMOS + SHARE DEMOS           
         AR    R3,R5               ADD LEVEL DEMOS                              
         LA    R3,3(R3)            ADD MANDATORY RTG, HUT, SHARE                
         STC   R3,TWTOTDEM         SAVE COUNT TOTAL DEMOS                       
         LA    R3,DCDEMOCD         A(INPUT STRING)                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R4,TWDEMOS+9        A(OUTPUT 3-BYTE ENTRIES)                     
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
DEMC0004 EQU   *                                                                
         CLI   0(R3),C'0'          GEOGRAPHIC INDICATOR TRANSLATE               
         BNE   DEMC0008            NOT NEEDED                                   
         MVI   0(R3),X'00'         INSERT BINARY ZEROES                         
DEMC0008 EQU   *                                                                
         MVC   0(1,R4),0(R3)       INSERT GEOGRAPHICAL INDICATOR                
         MVC   1(1,R4),1(R3)       INSERT DEMO QUALIFIER                        
         CLI   1(R4),C' '          DEMO QUALIFIER A SPACE?                      
         BNE   DEMC0012            NO                                           
         MVI   1(R4),C'T'          YES - REPLACE WITH 'T'                       
DEMC0012 EQU   *                                                                
         PACK  WORKAREA(8),2(3,R3) CONVERT EBCDIC TO BINARY                     
         CVB   R6,WORKAREA                                                      
         STC   R6,2(R4)            INSERT DEMO CODE #                           
         LA    R3,5(R3)            BUMP A(INPUT)                                
         LA    R4,3(R4)            BUMP A(OUTPUT)                               
         BCT   R5,DEMC0004         DO EACH FIELD                                
*                                                                               
*  AFTER THE BASIC N DEMOS ARE LOADED, THEY ARE COPIED INTO THE NEXT            
*    N BUCKETS TWICE, THE FIRST TIME HAVING THE QUALIFIER OVERRIDDEN            
*      FOR SHARES, THE SECOND TIME FOR LEVELS.                                  
*                                                                               
         MVI   WORK+20,C'S'        SET OVERRIDE TO 'SHARES OF RATINGS'          
         MVI   WORK+21,C'X'        SET OVERRIDE TO 'SHARES OF TSAS'             
DEMC0016 EQU   *                   R4 POINTS TO NEXT OPEN SLOT                  
         ZIC   R5,TW#DEMOS         RESET # OF DEMOS                             
         LA    R3,TWDEMOS+9        A(START OF DEMOS)                            
DEMC0020 EQU   *                                                                
         MVC   0(3,R4),0(R3)       MOVE DEMO CODE                               
*                                                                               
*   REASSIGNMENT OF DEMOGRAPHIC QUALIFIER CODES FOR 'SHARES' AND                
*     'LEVELS' RETRIEVAL:                                                       
*                                                                               
*   ---------- SHARES ------------  ----------- LEVELS -----------              
*   ' ' -> X  TSA IMPS -> TSA SHRS  ' ' -> Q  TSA IMPS -> TSA TOTS              
*    T  -> X  TSA SHRS == TSA SHRS   T  -> Q  TSA SHRS -> TSA TOTS              
*    Q  == Q  TSA TOTS == TSA TOTS   Q  == Q  TSA TOTS == TSA TOTS              
*                                                                               
*    R  -> S  RATING   -> SHARE      R  -> P  RATING   -> PUT                   
*    S  == S  SHARE    == SHARE      S  -> P  SHARE    -> PUT                   
*    P  == P  PUT      == PUT        P  == P  PUT      == PUT                   
* -------------------------------------------------------------                 
*                                                                               
         CLI   1(R4),C' '          OLD QUALIFIER = TSA IMPS?                    
         BE    DEMC0024            YES                                          
         CLI   1(R4),C'T'          OLD QUALIFIER = TSA SHARES?                  
         BE    DEMC0024            YES                                          
         CLI   1(R4),C'Q'          OLD QUALIFIER = TSA TOTALS?                  
         BE    DEMC0028            YES - DON'T CHANGE                           
         CLI   1(R4),C'P'          OLD QUALIFIER = PUT?                         
         BE    DEMC0028            YES - DON'T CHANGE                           
         MVC   1(1,R4),WORK+20     INSERT NEW QUALIFIER: RATINGS                
         B     DEMC0028                                                         
DEMC0024 EQU   *                                                                
         MVC   1(1,R4),WORK+21     INSERT NEW QUALIFIER: TSAS                   
DEMC0028 EQU   *                                                                
         LA    R4,3(R4)            BUMP A(TO FIELD)                             
         LA    R3,3(R3)            BUMP A(FROM FIELD)                           
         BCT   R5,DEMC0020                                                      
         CLI   WORK+20,C'P'        LEVELS DONE?                                 
         BE    DEMC0099            YES - FINISHED                               
         MVI   WORK+20,C'P'        NO  - SET UP FOR RATING LEVELS               
         MVI   WORK+21,C'Q'        SET UP FOR TSA LEVELS                        
         B     DEMC0016                                                         
DEMC0099 EQU   *                                                                
         B     XITIT                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    STORES THE INVENTORY BY DAYPART REQUESTS FOR LATER PROCESSING              
*                                                                               
INVBYDPT NTR1                                                                   
         L     R4,ADATA            A(ITEM)                                      
         USING CT11IN04,R4                                                      
         L     R2,ATWINVS          A(NEXT AVAILABLE REQ STORAGE)                
         USING DIDSTORE,R2                                                      
*                                                                               
         MVC   DIDDPT,IDDAYPT      STORE DAYPART CODE                           
         MVI   DIDDYFIL,X'FF'      SET TO 'NONE'                                
         CLC   IDDAYFIL,MYSPACES   ANY FILTER FOR DAYS ENTERED?                 
         BE    IBD0008             NO                                           
         LA    R3,DAYFILTS                                                      
IBD0002  EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    IBD0006             YES - SET FILTER TO 'OTHER'                  
         CLC   0(3,R3),IDDAYFIL    DAY FILTER FOUND?                            
         BE    IBD0004             YES                                          
         LA    R3,4(R3)            NO  - BUMP TABLE                             
         B     IBD0002                                                          
IBD0004  EQU   *                                                                
         MVC   DIDDYFIL,3(R3)      INSERT CODE FOR DAY FILTER                   
         B     IBD0008                                                          
IBD0006  EQU   *                                                                
         MVI   DIDDYFIL,C'9'       INSERT CODE FOR 'OTHER'                      
IBD0008  EQU   *                                                                
         CLC   IDTIMES,MYSPACES    ANY FROM-TO TIMES ENTERED?                   
         BE    IBD0010             NO                                           
         LA    R5,11               CALC LENGTH OF INPUT                         
         LA    R3,IDTIMES          A(TIME INPUT)                                
IBD0008A EQU   *                                                                
         CLI   0(R3),C' '          CHECK FOR SPACE                              
         BE    IBD0008B            FOUND - FINISHED                             
         LA    R3,1(R3)            BUMP SCAN                                    
         BCT   R5,IBD0008A         GO BACK FOR NEXT                             
IBD0008B EQU   *                                                                
         LA    R3,11               NOW CALCULATE LENGTH                         
         SR    R3,R5               SUBTRACT NUMBER REMAINING FROM 11            
         PRINT GEN                                                              
         GOTO1 =V(TIMVAL),DMCB,((R3),IDTIMES),DIDTMFIL,RR=RELO                  
         PRINT NOGEN                                                            
         CLI   DMCB,X'FF'          VALID?                                       
         BNE   IBD0008E            YES                                          
         DC    H'0'                                                             
IBD0008E EQU   *                                                                
         MVC   DIDTMFIL+6(2),DIDTMFIL+2    SAVE TO-TIME                         
         ZICM  R3,DIDTMFIL,2       LOAD FIRST TIME VALUE                        
         GOTO1 FIGQTRHR,DMCB,(R3)                                               
         MVC   DIDTMFIL(4),DUB     SAVE FIRST QTR HR                            
         ZICM  R3,DIDTMFIL+6,2     LOAD SECOND TIME VALUE                       
         LTR   R3,R3               ANY SECOND TIME VALUE?                       
         BZ    IBD0010             NO                                           
         GOTO1 FIGQTRHR,DMCB,(R3)                                               
         MVC   DIDTMFIL+4(4),DUB   SAVE SECOND QTR HR                           
IBD0010  EQU   *                                                                
         CLC   IDSTDT,MYSPACES     ANY START DATE FILTER?                       
         BE    IBD0012             NO                                           
         GOTO1 DATVAL,DMCB,IDSTDT,WORKAREA                                      
         GOTO1 DATCON,DMCB,WORKAREA,(3,DIDSTDT)                                 
IBD0012  EQU   *                                                                
         CLC   IDENDT,MYSPACES     ANY END DATE FILTER?                         
         BE    IBD0014                                                          
         GOTO1 DATVAL,DMCB,IDENDT,WORKAREA                                      
         GOTO1 DATCON,DMCB,WORKAREA,(3,DIDENDT)                                 
IBD0014  EQU   *                                                                
         CLI   IDCOMPET,C'Y'       COMPETITIVE WANTED?                          
         BNE   IBD0016             NO                                           
         OI    DIDFLAGS,X'80'      SET FLAG                                     
IBD0016  EQU   *                                                                
         CLI   IDRATES,C'Y'        RATES WANTED?                                
         BNE   IBD0018             NO                                           
         OI    DIDFLAGS,X'40'                                                   
IBD0018  EQU   *                                                                
         L     R5,DATALEN          L(DATA ELEMENT IN PROGRESS)                  
         LA    R6,LIDDAYPT         L(DATA ELEM W/O DEMO FILTERS)                
         CR    R5,R6               SAME?                                        
         BE    IBD0030             YES - FINISHED                               
         OI    DIDFLAGS,X'20'      SET 'DEMO FILTER 1' FLAG                     
         GOTO1 PARSDEMO,DMCB,IDDEMCDE,DIDDEMO,DIDFLAGS                          
         L     R5,DATALEN          L(DATA ELEM IN PROGRESS)                     
         L     R6,DUB              L(1ST DEMO FILTER )                          
         LA    R6,LIDDAYPT(R6)     L(DATA ELEM + 1ST DEMO FILTER)               
         CR    R5,R6               ANY DATA LEFT?                               
         BE    IBD0030             NO  - FINISHED                               
         BH    IBD0020             SECOND DEMO FILTER                           
         DC    H'0'                PROBLEM HERE!!                               
IBD0020  EQU   *                                                                
         OI    DIDFLAGS,X'08'      SET DEMO FILTER 2 FLAG                       
         L     R6,DUB              L(1ST DEMO FILTER)                           
         LA    R3,IDDEMCDE         A(1ST DEMO FILTER INPUT)                     
         AR    R3,R6               A(2ND DEMO FILTER INPUT)                     
         LA    R6,DIDDEMO+8        TWO VALUES FOR FILTER 1?                     
         TM    DIDFLAGS,X'10'      CHECK SELECTION RANGE                        
         BNO   IBD0022             NO                                           
         LA    R6,4(R6)            YES - SKIP SECOND VALUE                      
IBD0022  EQU   *                                                                
         GOTO1 PARSDEMO,DMCB,(R3),(R6),DIDFLAGS                                 
IBD0030  EQU   *                                                                
         LA    R6,17               BASIC REQUEST SIZE: NO DEMO FILTS            
         TM    DIDFLAGS,X'20'      DEMO FILT 1 ENTERED?                         
         BNO   IBD0040             NO  - FINISHED                               
         LA    R6,8(R6)            YES - INCREASE SIZE                          
         TM    DIDFLAGS,X'10'      SECOND VALUE ENTERED?                        
         BNO   IBD0032             NO                                           
         LA    R6,4(R6)            YES                                          
IBD0032  EQU   *                                                                
         TM    DIDFLAGS,X'08'      DEMO FILT 2 ENTERED?                         
         BNO   IBD0040             NO  - FINISHED                               
         LA    R6,8(R6)            YES - INCREASE SIZE                          
         TM    DIDFLAGS,X'04'      SECOND VALUE ENTERED?                        
         BNO   IBD0040             NO                                           
         LA    R6,4(R6)                                                         
IBD0040  EQU   *                                                                
         L     R3,ATWINVS          INCREMENT A(INV) BY LENGTH                   
         AR    R3,R6                                                            
         ST    R3,ATWINVS                                                       
*                                                                               
         B     XITIT                                                            
         DROP  R2,R4                                                            
*                                                                               
         EJECT                                                                  
DAYFILTS EQU   *                                                                
         DC    C'M-F',C'0'                                                      
         DC    C'M  ',C'1'                                                      
         DC    C'TU ',C'2'                                                      
         DC    C'W  ',C'3'                                                      
         DC    C'TH ',C'4'                                                      
         DC    C'F  ',C'5'                                                      
         DC    C'SA ',C'6'                                                      
         DC    C'SU ',C'7'                                                      
         DC    C'M-S',C'8'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   TAKES THE MILITARY TIME VALUE PASSED IN P1, AND CONVERTS IT TO              
*      THE EQUIVALENT QUARTER HOUR NUMBER, WHICH IS PASSED BACK IN              
*      THE FIRST FOUR BYTES OF DUB                                              
*                                                                               
FIGQTRHR NTR1                                                                   
         L     R5,0(R1)            LOAD MILITARY TIME                           
         LA    R1,600              SET TO 6AM AS QTR HR 0                       
         SR    R5,R1                                                            
         SR    R4,R4                                                            
         D     R4,=F'100'          DIVIDE BY 100 FOR HOUR                       
         LR    R1,R4               SAVE REMAINDER                               
         SLA   R5,2                MULTIPLY HR # BY 4                           
         LR    R2,R5               SAVE WHOLE HR QTR #                          
         LR    R5,R1               RELOAD REMAINDER                             
         SR    R4,R4                                                            
         D     R4,=F'15'           DIVIDE REMAINDER BY 15                       
         AR    R2,R5               WHOLE # + REMAINDER #                        
         ST    R2,DUB              STORE IT                                     
         B     XITIT                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   PARSES OUT THE DEMO FILTERS INTO THE APPROPRIATE AREAS FOR THE              
*      PARTICULAR REQUEST, AND RETURNS LENGTH OF INPUT ELEMENT                  
*      P1  =  A(INPUT)                                                          
*      P2  =  A(OUTPUT)                                                         
*      P3  =  A(FLAG BITS IN OUTPUT)                                            
*     DUB  =  LENGTH OF ELEMENT AS RETURN VALUE                                 
*                                                                               
PARSDEMO NTR1                                                                   
         L     R3,0(R1)            A(INPUT)                                     
         L     R4,4(R1)            A(OUTPUT)                                    
         MVC   DUB+4,8(R1)         A(FLAG BITS)                                 
*                                                                               
         CLI   0(R3),C'0'          GEOGRAPHICAL INDICATOR TRANSLATE             
         BNE   PAD0002             NOT NEEDED                                   
         MVI   0(R3),X'00'         INSERT BINARY ZEROES                         
PAD0002  EQU   *                                                                
         MVC   0(1,R4),0(R3)       INSERT GEOGRAPHICAL INDICATOR                
         MVC   1(1,R4),1(R3)       INSERT DEMO QUALIFIER                        
         PACK  WORKAREA(8),2(3,R3) CONVERT EBCDIC TO BINARY                     
         CVB   R6,WORKAREA                                                      
         STC   R6,2(R4)            INSERT DEMO CODE                             
         MVC   3(1,R4),5(R3)       INSERT ACTION CODE                           
         NI    6(R3),X'0F'         TURN OFF ZONE BITS OF LENGTH                 
         ZIC   R2,6(R3)            INSERT LENGTH OF VALUE INTO REG              
         LA    RE,7                L(ELEMENT)=CODE+ACTION+L(LEN)                
         AR    RE,R2               +L(VALUE)                                    
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         LA    R1,7                SET L(WORKAREA) FOR EXECUTE                  
         SLA   R1,4                MOVE TO BITS 8-11                            
         AR    R1,R2               ADD L(VALUE)-1 FOR EXECUTE                   
         LA    R5,WORKAREA         A(WORKAREA)                                  
         LA    R6,7(R3)            A(VALUE 1)                                   
*                                                                               
         EX    R1,PAD0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R1,WORKAREA                                                      
         ST    R1,4(R4)            STORE AS 1ST VALUE OF DEMO FILTER            
         LA    R2,1(R2)            RE-ADD 1 TO L(VALUE)                         
         AR    R6,R2               A(NEXT FIELD) IF THERE IS ONE...             
         CLI   5(R3),C'='          SELECTION RANGE OPTION?                      
         BNE   PAD0014             NO  - NO SECOND VALUE                        
         L     RF,DUB+4                                                         
         OI    0(RF),X'10'         SET '2 VALUES FOR RANKING' FLAG              
         LR    R3,R6                                                            
         NI    0(R3),X'0F'         TURN OFF ZONE BITS OF LENGTH FIELD           
         ZIC   R2,0(R3)            INSERT LENGTH                                
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         LA    R1,7                SET FOR L(WORKAREA) IN EXECUTE               
         SLA   R1,4                SHIFT TO BITS 8-11                           
         AR    R1,R2               ADD L(FIELD)-1 FOR EXECUTE                   
         LA    R5,WORKAREA         A(WORKAREA)                                  
         LA    R6,1(R3)            A(VALUE 2)                                   
*                                                                               
         EX    R1,PAD0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R1,WORKAREA                                                      
         ST    R1,8(R4)            STORE AS 2ND VALUE OF DEMO FILTER            
         LA    R2,2(R2)            RE-ADD 1 TO L(FIELD)+ 1 FOR L(LEN)           
         AR    RE,R2               ADD TO OVERALL LENGTH                        
PAD0014  EQU   *                                                                
         ST    RE,DUB                                                           
         B     PAD0099                                                          
*                                                                               
PAD0080  PACK  0(0,R5),0(0,R6)                                                  
*                                                                               
PAD0099  EQU   *                                                                
         B     XITIT                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*                                                                               
INVBYNUM NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT11IN05,R2                                                      
         L     R3,ATWINVS          A(NEXT AVAILABLE REQUEST STORAGE)            
         USING DINSTORE,R3                                                      
*                                                                               
         MVI   INVRUN,1            SET REQUEST TYPE TO INV BY NUMBER            
         PACK  WORKAREA(8),ININVNUM(2)                                          
         CVB   R1,WORKAREA         PROCESS INV #                                
         STC   R1,DININV#          CONVERT 1ST 2 POS TO BINARY                  
         MVC   DININV#+1(2),ININVNUM+2                                          
         CLC   ININVDTE,MYSPACES   ANY EFFECTIVE DATE?                          
         BE    IBN0002             NO                                           
         GOTO1 DATVAL,DMCB,ININVDTE,WORKAREA                                    
         GOTO1 DATCON,DMCB,WORKAREA,(3,DINEFDTE)                                
IBN0002  EQU   *                                                                
         CLC   INSTDT,MYSPACES     ANY START DATE                               
         BE    IBN0004             NO                                           
         GOTO1 DATVAL,DMCB,INSTDT,WORKAREA                                      
         GOTO1 DATCON,DMCB,WORKAREA,(3,DINSTDT)                                 
IBN0004  EQU   *                                                                
         CLC   INSTDT,MYSPACES     ANY START DATE                               
         BE    IBN0006             NO                                           
         GOTO1 DATVAL,DMCB,INENDT,WORKAREA                                      
         GOTO1 DATCON,DMCB,WORKAREA,(3,DINENDT)                                 
IBN0006  EQU   *                                                                
         MVC   DINCOMP,INCOMPET    COMPETITIVE FLAG                             
         MVC   DINRATES,INRATES    RATES FLAG                                   
         MVC   DINAVGCM,INAVGCOM   AVERAGE/COMBO FLAG                           
IBN0010  EQU   *                                                                
         L     R6,ATWINVS          SET TO NEXT AVAILABLE ADDRESS                
         LA    R6,LDINSTOR(R6)                                                  
         ST    R6,ATWINVS          SAVE IT BACK                                 
IBN0099  EQU   *                                                                
         B     XITIT                                                            
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
XITIT    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CT11DI01:  BASIC HEADLINE INFORMATION                                         
       ++INCLUDE CT11DI01                                                       
         SPACE 4                                                                
* CT11DI02:  BOOK LIST                                                          
         SPACE 4                                                                
       ++INCLUDE CT11DI02                                                       
         SPACE 4                                                                
* CT11DI03:  DEMOGRAPHICS REQUESTED                                             
         SPACE 4                                                                
       ++INCLUDE CT11DI03                                                       
         SPACE 4                                                                
* CT11DI04:  INVENTORY REQUESTED BY DAYPART                                     
         SPACE 4                                                                
       ++INCLUDE CT11DI04                                                       
         SPACE 4                                                                
* CT11DI05:  INVENTORY REQUESTED BY NUMBER                                      
         SPACE 4                                                                
       ++INCLUDE CT11DI05                                                       
         SPACE 4                                                                
* CT1XDO01:  DESCRIPTIVE ITEM                                                   
         SPACE 4                                                                
       ++INCLUDE CT1XDO01                                                       
         SPACE 4                                                                
* CT1XDO03:  DEMOGRAPHIC VALUES DERIVED                                         
         SPACE 4                                                                
       ++INCLUDE CT1XDO03                                                       
         SPACE 4                                                                
* CT1XDO04:  RATIONALE                                                          
         SPACE 4                                                                
       ++INCLUDE CT1XDO04                                                       
         EJECT                                                                  
* CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                         
         SPACE 4                                                                
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
DIVISOR  DS    F                   DIVISOR BUCKET                               
ADATAREC DS    A                   A(DATA RECORD)                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
HOMSHR   DS    3F                                                               
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    A                   A(DBLOCK)                                    
MTHFCTR  DS    F                   WEIGHTING FACTOR FOR X AND /                 
MTHIFIL  DS    CL3                 INPUT FILE                                   
MTHOFIL  DS    CL3                 OUTPUT FILE                                  
MTHOSRC  DS    CL3                 OUTPUT SOURCE                                
         SPACE 2                                                                
ADDSW    DS    C                   Y=ADD DEMOS ONLY                             
IUNSW    DS    C                   Y=INVENTORY REC IN IUN FORMAT                
INDEXUP  DS    C                   Y=INV. REC HAS BEEN INDEX UPGRADED           
U191     DS    X                   X'80' - THERE IS NO U191                     
*                                  X'40' - THERE IS U191                        
DEMCOD   DS    CL3                                                              
         DS    0F                                                               
         EJECT                                                                  
* EXTRA CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                   
         SPACE 4                                                                
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
*                                                                               
*     FOLLOWING FIELDS ARE DEVELOPED FROM THE REQUEST SENT                      
*     FROM THE PC.                                                              
*                                                                               
SCRATCH  DS    0CL1                SCRATCH SPACE                                
TWSTAT   DS    CL5                 STATION                                      
TWSERV   DS    CL1                 SERVICE                                      
TWSTDT   DS    CL2                 FLIGHT START DATE                            
TWENDT   DS    CL2                 FLIGHT END DATE                              
TWCOMPET DS    CL1                 COMPETITIVE REQUESTED FLAG                   
TWRATES  DS    CL1                 RATES REQUESTED FLAG                         
TWBOOK   DS    9CL3                EIGHT BOOK ENTRIES + DELIMITER               
TWDEMOS  DS    34CL3               DEMO CODES + DELIMITER                       
LTWDEMOS EQU   *-TWDEMOS                                                        
TW#DEMOS DS    CL1                 DEMO COUNT REQUESTED                         
TWTOTDEM DS    CL1                 TOTAL # DEMOS                                
ATWBOOK  DS    A                   A(BOOK IN PROGRESS)                          
ATWDEMO  DS    A                   A(DEMO IN PROGRESS)                          
ATWINVS  DS    A                   A(INVENTORY IN PROGRESS)                     
MAXIOCTR DS    H                   90% OF MAXIMUM IO'S                          
AVGWT    DS    XL1                 AVERAGE WEIGHT                               
INVRUN   DS    XL1                 TYPE OF REQUEST                              
*                                  0  =  REQUEST BY DAYPART                     
*                                  1  =  REQUEST BY INVENTORY NUMBER            
ACTFLAG  DS    XL1                 ACTION                                       
*                                  0  =  INDIVIDUAL INVENTORY                   
*                                  1  =  AVERAGE                                
*                                  2  =  COMBO                                  
SAVSRCBK DS    CL3                 SAVE AREA FOR SOURCE/BOOK                    
KEY92SAV DS    CL27                SAVE AREA:X'92' PASSIVE D/P KEY              
DEMODATA DS    CL1                 FLAG: COMBO/NON-COMBO (Y/N)                  
DEMOFLAG DS    CL1                 FLAG: SEND MASTER DATA ONLY ONCE             
INVFOUND DS    CL1                 FLAG: INVENTORY FOUND/NOT FOUND              
PJNAME   DS    CL3                 PJ1/PJ2 LITERAL                              
TESTDUMP DS    CL1                 COUNTER FOR SERIAL DUMPS                     
EXTRAKEY DS    CL27                EXTRA KEY STORAGE                            
RANKWORK DS    CL12                WORK AREA FOR ESTABLISHING RANK              
RANKDEMO DS    CL6                 DEMO UPON WHICH RANK IS BASED                
RANKCTR  DS    CL1                 CTR FOR RANK ELEMENTS                        
RANKADDR DS    F                   A(NEXT SLOT FOR RANK ELEMENTS)               
         DS    0D                  ALIGNMENT                                    
HEXOUTWK DS    CL8                 WORK SPACE FOR HEXOUT                        
WEIGHT   DS    CL4                 WEIGHTING FOR DEMOS                          
ELTAREA  DS    CL100               ELEMENT BUILD AREA                           
DBDEMOB  DS    CL480               AREA FOR DEMO INTERFACE MODULE               
         ORG   DBDEMOB                                                          
DBLOCKA1 DS    CL256               ACTUAL DBLOCK LEN                            
         DS    CL96                SPARE                                        
DBEXTRA1 DS    CL128               ACTUAL DBEXTRA LEN                           
STORE16  DS    4F                  16 BYTES CONTIGUOUS STORAGE                  
NEWRATG  DS    F                   NON-0 IF NEW RATING CALC'D                   
NEWSHARE DS    F                   NON-0 IF NEW SHARE  CALC'D                   
NEWHUT   DS    F                   NON-0 IF NEW HUT    CALC'D                   
NEWDEMO  DS    F                   NON-0 IF NEW DEMO   SUBMITTED                
RSTRTLEN DS    F                   L(NEW ELEMENT) FOR RESTART                   
RSTRTMST DS    A                   DISK ADDRESS OF MASTER IN PROGRESS           
RSTRTDET DS    A                   DISK ADDRESS OF DETAIL IN PROGRESS           
SAVRSTRT DS    CL1                 SAVE RESTART VALUE FOR DUMPS                 
RSTRTRTN DS    CL1                 FLAG TO ROUTINE TERMINATING                  
*                                  1  =  PULLDEMS FROM RUNINV                   
*                                  11 =  PULLDEMS FROM RANK                     
*                                  2  =  PULLRATS FROM RUNINV                   
*                                  21 =  PULLRATS FROM RANK                     
*                                  3  =  UPGRADE                                
*                                  4  =  SENDDESC FROM PULLDEMS                 
*                                  41 =  SENDDESC FROM RANK                     
*                                  6  =  END OF DATA ELEMENT                    
*                                  7  =  IO COUNT RESTART                       
RSTRTDES DS    CL1                 RESTART SENDDESC ORIGINATING RTN             
*                                  4  =  SENDDESC FROM PULLDEMS                 
*                                  5  =  SENDDESC FROM RANK                     
RSTRTDEM DS    CL1                 RESTART PULLDEMS ORIGINATING RTN             
*                                  1  =  PULLDEMS FROM RUNINV                   
*                                  5  =  PULLDEMS FROM RANK                     
RSTRTRTS DS    CL1                 RESTART PULLRATS ORIGINATING RTN             
*                                  4  =  PULLRATS FROM RUNINV                   
*                                  5  =  PULLRATS FROM RANK                     
RSTRTRAT DS    CL1                 RESTART CTR FOR LINE W/IN RAT'LE             
SCRNDATA DS    CL1                 DATA ON SCREEN                               
*                                  Y  =  YES                                    
*                                  N  =  NO                                     
TESTIT   DS    CL1                 **TEST**                                     
RSTRTEND DS    CL1                 NON-0 = END JOB WITH RESTART                 
SAVERANK DS    CL1                 RANK SAVED FLAG                              
*                                                                               
INVSTORE DS    600C                INVENTORY REQUEST STORAGE                    
*                                                                               
DEMOUT   DS    A                   A(DEMOUT)                                    
DEMAINT  DS    A                   A(DEMAINT)                                   
DEMOMTH  DS    A                   A(DEMOMTH)                                   
DATVAL   DS    A                   A(DATVAL)                                    
BOOKVAL  DS    A                   A(BOOKVAL)                                   
IUNWORK  DS    2016C                                                            
LSCRATCH EQU   *-SCRATCH                                                        
*                                                                               
*      END OF PC FIELDS       *                                                 
*                                                                               
*  UPGRADE STORAGE DSECT:                                                       
UPGSTORE DSECT                                                                  
UPGFLAG  DS    CL1                 0  =  UPGRADE HAS MONTH                      
*                                  1  =  UPGRADE HAS ABSOLUTE VALUE             
UBASEBK  DS    CL3                 BASE BOOK                                    
UPGTYPE  DS    XL1                 TYPE OF UPGRADE (NOT DEMO CODE)              
*                                  1  =  PUT (USES SHRS FROM BASE BK)           
*                                  2  =  HPT (USES SHRS FROM BASE BK)           
*                                  3  =  HUT                                    
*                                  4  =  RTG                                    
*                                  5  =  SHR                                    
*                                  6  =  NDX                                    
UPGDEMO  DS    CL3                 DEMO CODE (NOT TYPE OF UPGRADE)              
UMONABS  DS    CL3                 MONTH OR ABS (SEE UPGFLAG)                   
UOPTSHR  DS    XL3                 OPTIONAL ABSOLUTE SHARE VALUE                
*                                                                               
*                                                                               
*  INVENTORY BY DAYPART DSECT                                                   
DIDSTORE DSECT                                                                  
DIDDPT   DS    CL1         0       DAYPART CODE                                 
DIDDYFIL DS    CL1        +1       DAY FILTER: 0=M-F,1=MON,X'FF'=NONE           
DIDTMFIL DS    CL8        +2       TIME FILTER                                  
DIDSTDT  DS    CL3        +10      START DATE FILTER                            
DIDENDT  DS    CL3        +13      END DATE FILTER                              
DIDFLAGS DS    CL1        +16      FLAGS FOR DAYPART                            
*                                  BIT 0  = COMPETITIVE DATA WANTED             
*                                  BIT 1  = RATES WANTED                        
*                                  BIT 2  = DEMO FILTER 1 ENTERED               
*                                  BIT 3  = DEMO FILTER 1 HAS TWO               
*                                           VALUES FOR RANKING                  
*                                  BIT 4  = DEMO FILTER 2 ENTERED               
*                                  BIT 5  = DEMO FILTER 2 HAS TWO               
*                                           VALUES FOR RANKING                  
*                                                                               
*  OCCURRENCE OF FOLLOWING FIELDS DEPENDS ON SETTING OF BITS 2                  
*    AND 4 IN FIELD 'DIDFLAGS'                                                  
*                                                                               
DIDDEMO  DS    CL3        +17      DEMO FILTER                                  
DIDACT   DS    CL1        +20      DEMO FILTER ACTION                           
*                                  >  =  MINIMUM DEMO VALUE                     
*                                  <  =  MAXIMUM DEMO VALUE                     
*                                  B  =  MINIMUM CPP/CPM VALUE                  
*                                  T  =  MAXIMUM CPP/CPM VALUE                  
*                                  =  =  SELECTION RANGE AFTER RANKING          
DIDVAL1  DS    CL4        +21      DEMO FILTER VALUE 1                          
*                                                                               
*  FOLLOWING FIELD WILL ONLY APPEAR IF BIT 3/5 OF DIDFLAGS IS SET               
*                                                                               
DIDVAL2  DS    CL4        +25      DEMO FILTER VALUE 2                          
*                                                                               
*                                                                               
*  INVENTORY BY NUMBER DSECT                                                    
DINSTORE DSECT                                                                  
DININV#  DS    CL3                 INVENTORY #                                  
DINEFDTE DS    CL3                 EFFECTIVE START DATE                         
DINSTDT  DS    CL3                 START DATE FILTER                            
DINENDT  DS    CL3                 END  DATE FILTER                             
DINCOMP  DS    CL1                 COMPETITIVE WANTED FLAG                      
DINRATES DS    CL1                 RATES WANTED FLAG                            
DINAVGCM DS    CL1                 AVERAGE/COMBINE FLAG                         
*                                  0  =  NO ACTION                              
*                                  1  =  AVERAGE FOLLOWING ITEM                 
*                                  2  =  COMBO WITH FOLLOWING ITEM              
LDINSTOR EQU   *-DININV#           LENGTH OF REQUEST ENTRY                      
*                                                                               
         EJECT                                                                  
* DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                  
*                                                                               
*                                                                               
*  REGENINV:  INVENTORY RECORD LAYOUT:  DSECT                                   
RINVRECD DSECT                                                                  
       ++INCLUDE REGENINV                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
*  IUNRECDS:  UNIFORM DEMO DATA DSECT                                           
       ++INCLUDE IUNRECDS                                                       
         EJECT                                                                  
*  DDCOMFACS: ADDRESS ROUTINE DSECT                                             
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
*  FAFACTS:  TO GET ALLOWABLE RUN STATISTICS                                    
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145CTMAD0B   05/01/02'                                      
         END                                                                    
