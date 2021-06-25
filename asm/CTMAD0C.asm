*          DATA SET CTMAD0C    AT LEVEL 099 AS OF 05/10/05                      
*PHASE TA0C0CA                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE REGETIUN                                                               
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
*INCLUDE DAYPAK                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*                                                                               
         TITLE 'TA0C0C - $MAD REP PAV/TP BY DAY/TIME OR NUMBER'                 
**********************************************************************          
*   HISTORY OF CHANGES                                               *          
**********************************************************************          
*   JUN04/91   (BU ) --- ORIGINAL ENTRY                              *          
*                                                                    *          
*   MAR03/92   (BU ) --- ADD TITLE RETURN TO D/T REQUEST             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
TA0C0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C0C,RA,RR=R2                                                
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
         BAS   RE,PROCINIT         INITIALIZE WORK SCRATCH AREA                 
         BAS   RE,PROCRQST         PROCESS REQUEST                              
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
         BAS   RE,PROCIN02         REINITIALIZE WORK AREA                       
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
*    THE ALTERNATE ENTRY POINT 'PROCIN02' SKIPS THE REINITIALIZATION            
*      OF THE SCRATCH SPACE, AND ESTABLISHING THE INITIAL ADDRESS               
*      OF REQUEST STORAGE.                                                      
*                                                                               
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
*                                                                               
         LA    RE,PTPSTORE         A(REQUEST STORAGE)                           
         ST    RE,ATWREQS          SAVE IT FOR LOAD                             
         B     PIN0004             SKIP ALTERNATE ENTRY POINT                   
*                                                                               
PROCIN02 NTR1  ALTERNATE ENTRY POINT                                            
*                                                                               
*        INITIALIZE DEMOGRAPHIC DBLOCK PARAMETERS FOR INVENTORY                 
*                                                                               
PIN0004  EQU   *                                                                
         LA    R6,DBDEMOB          A(DEMO INTERFACE AREA)                       
*                                                                               
         USING DEMOD,R6                                                         
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR DEMOUT                 
         MVC   DBSELAGY,SIGNON2C   SET REP CODE FOR AUTHORIZATIONS              
         L     RE,AIO              A(IOAREA)                                    
         ST    RE,DBAREC                                                        
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
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   DEMAINT,CDEMAINT                                                 
         MVC   DEMOMTH,CDEMOMTH                                                 
         MVC   DATVAL,CDATVAL                                                   
         MVC   DEFINE,CDEFINE                                                   
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
*  IO AREA 2 IS USED FOR INTERIM ACCUMULATIONS FOR AVERAGING AND                
*     COMBO'ING.                                                                
*                                                                               
         L     RE,AIO2             A(AVERAGE/COMBO INTERIM STORAGE)             
         ST    RE,AACCUM                                                        
*                                                                               
*   RETRIEVE A(CORE RESIDENT BOOKVAL ROUTINE)                                   
*                                                                               
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D900A00'    T00A00 = BOOKVAL                        
         GOTO1 (RF),(R1),0                                                      
         MVC   BOOKVAL,DMCB                                                     
*                                                                               
PIN0099  B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    ESTABLISHES THE REQUEST IN THE WORK AREA.                                  
*    SETS POINTERS                                                              
*    MAKES 2ND THRU NTH CALL RESTART TRANSPARENT                                
*                                                                               
*                                                                               
PROCRQST NTR1                                                                   
*                                                                               
PR0002   EQU   *                                                                
         GOTO1 GETITEM             RETRIEVE ITEM FROM INPUT FRAME               
         BNE   PR0099              ALL ITEMS DONE - SET MISCELLAN.              
         L     R1,TYPENUM          DETERMINE TYPE OF ITEM                       
         LA    R2,ITRPMFDS                                                      
         CR    R1,R2               BASIC HEADER INFO?                           
         BNE   PR0004              NO                                           
         BAS   RE,BASHDINF         YES                                          
         B     PR0002                                                           
PR0004   EQU   *                                                                
         LA    R2,ITRPMFBK                                                      
         CR    R1,R2               BOOK INFO?                                   
         BNE   PR0006              NO                                           
         BAS   RE,BOOKIN           YES                                          
         B     PR0002                                                           
PR0006   EQU   *                                                                
         LA    R2,ITRPMFDC                                                      
         CR    R1,R2               DEMOGRAPHIC CODES?                           
         BNE   PR0008              NO                                           
         BAS   RE,DEMOCODE         YES                                          
         B     PR0002                                                           
PR0008   EQU   *                                                                
         LA    R2,ITRPDREQ                                                      
         CR    R1,R2               PAV/TP BY DAY/TIME?                          
         BNE   PR0010              NO                                           
         BAS   RE,PTBYTIME         YES                                          
         B     PR0002                                                           
PR0010   EQU   *                                                                
         LA    R2,ITRPNREQ                                                      
         CR    R1,R2               PAV BY PROGRAM NUMBER?                       
         BNE   PR0012              NO                                           
         BAS   RE,PAVBYNUM         YES                                          
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
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SETS UP THE BASIC DESCRIPTIVE FIELDS FROM THE HEADLINE, AND                
*    TRANSLATES DATES INTO FORMAT REQUIRED FOR INVENTORY RETRIEVAL.             
*                                                                               
BASHDINF NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT12IN01,R2                                                      
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
         GOTO1 DATCON,DMCB,WORKAREA,(3,TWSTDT)                                  
         GOTO1 DATCON,DMCB,WORKAREA+6,(3,TWENDT)                                
         MVC   TWCOMPET,BHCOMPET   SET COMPETITIVE FLAG                         
         MVC   TWRATES,BHRATES     SET RATES FLAG                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS A PHONY HEADER TO ENABLE 'BOOKVAL' TO WORK                          
*    INSERTS DATA FROM ITEM TO PERMIT TRANSLATION VIA BOOKVAL                   
*    HANDLES THE PJ1/PJ2 INDICATORS IMBEDDED WITHIN STRING THAT                 
*      WOULD BE REJECTED BY BOOKVAL                                             
*                                                                               
BOOKIN   NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT12IN02,R2                                                      
*                                                                               
         LA    RE,WORKAREA              A(WORKAREA)                             
         XCEF  (RE),600                                                         
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
         GOTO1 BOOKVAL,DMCB,WORKAREA,(8,TWBOOK),SCANNER                         
         MVC   TWBKCTR,DMCB+4      SAVE BOOK COUNT                              
         B     XIT                                                              
*                                                                               
BI0080   MVC   0(0,R5),BLBOOKS                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    PROCESSES EACH 5-BYTE PC INPUT FIELD, CONVERTING THE 3-BYTE                
*      EBCDIC REPRESENTATION OF THE DEMO # TO A 1-BYTE BINARY                   
*                                                                               
DEMOCODE NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT12IN03,R2                                                      
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
         XCEF  (RE),600                                                         
DEMC0004 EQU   *                                                                
         CLI   0(R3),C'0'          GEOGRAPHIC INDICATOR TRANSLATE               
         BNE   DEMC0008            NOT NEEDED                                   
         MVI   0(R3),X'00'         INSERT BINARY ZEROES                         
DEMC0008 EQU   *                                                                
         MVC   0(1,R4),0(R3)       INSERT GEOGRAPHICAL INDICATOR                
         MVC   1(1,R4),1(R3)       INSERT DEMO QUALIFIER                        
         CLI   1(R4),C' '          IS QUALIFIER A SPACE?                        
         BNE   DEMC0012            NO                                           
         MVI   1(R4),C'T'          YES - CHANGE IT TO 'T'                       
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
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    STORES THE PAV/TP BY DAY/TIME REQUESTS FOR LATER PROCESSING                
*                                                                               
PTBYTIME NTR1                                                                   
         L     R7,ADATA            A(ITEM)                                      
         USING CT12IN04,R7                                                      
         L     R2,ATWREQS          A(NEXT AVAILABLE REQ STORAGE)                
         USING PTTSTORE,R2                                                      
*                                                                               
         MVC   PTTPORT,PTPAVTP     STORE DATA TYPE FLAG                         
         MVC   PTTSTAT,PTSTAT      STORE STATION + MEDIA                        
         LA    R5,12               CALC LENGTH OF INPUT                         
         LA    R3,PTDAYS           A(DAY INPUT)                                 
PBT0002  EQU   *                                                                
         CLI   0(R3),C' '          CHECK FOR SPACE                              
         BE    PBT0003             FOUND - FINISHED                             
         LA    R3,1(R3)            BUMP SCAN                                    
         BCT   R5,PBT0002          GO BACK FOR NEXT                             
PBT0003  EQU   *                                                                
         LA    R3,12               NOW CALCULATE LENGTH                         
         SR    R3,R5               SUBTRACT NUMBER REMAINING FROM 11            
         GOTO1 =V(DAYPAK),DMCB,((R3),PTDAYS),PTTDAYS,PTTDAYS+1,RR=RELO          
         CLI   PTTDAYS,X'00'       VALID?                                       
         BNE   PBT0003E            YES                                          
         DC    H'0'                                                             
PBT0003E EQU   *                                                                
         LA    R5,11               CALC LENGTH OF INPUT                         
         LA    R3,PTTIMES          A(TIME INPUT)                                
PBT0006  EQU   *                                                                
         CLI   0(R3),C' '          CHECK FOR SPACE                              
         BE    PBT0007             FOUND - FINISHED                             
         LA    R3,1(R3)            BUMP SCAN                                    
         BCT   R5,PBT0006          GO BACK FOR NEXT                             
PBT0007  EQU   *                                                                
         LA    R3,11               NOW CALCULATE LENGTH                         
         SR    R3,R5               SUBTRACT NUMBER REMAINING FROM 11            
         GOTO1 =V(TIMVAL),DMCB,((R3),PTTIMES),PTTTIME,RR=RELO                   
         CLI   DMCB,X'FF'          VALID?                                       
         BNE   PBT0008             YES                                          
         DC    H'0'                                                             
PBT0008  EQU   *                                                                
         MVC   PTTAVCM,PTAVGCOM    SET AVERAGE/COMBO/JOINED FLAG                
PBT0018  EQU   *                                                                
         L     R5,DATALEN          L(DATA ELEMENT IN PROGRESS)                  
         LA    R6,LPTPAVTP         L(DATA ELEM W/O BOOK ENTRIES)                
         SR    R3,R3               SET R3 FOR 'NO BOOKS'                        
         XC    PTTBKCTR,PTTBKCTR   SET BOOK COUNTER TO ZERO                     
         CR    R5,R6               SAME?                                        
         BE    PBT0030             YES - FINISHED                               
*                                                                               
*  BOOKS HAVE BEEN ENTERED.  BUILD A PHONY HEADER TO ENABLE                     
*     BOOKVAL TO WORK, PASS BOOKS THROUGH IT                                    
*                                                                               
         LA    RE,WORKAREA              A(WORKAREA)                             
         XCEF  (RE),600                                                         
         SR    R5,R6                    L(BOOK ENTRIES IN ELEMENT)              
         LR    R3,R5                    SAVE L(INPUT BOOK DATA)                 
         LA    R5,2(R5)                 ADD 2 FOR 'SERVICE+,'                   
         STC   R5,WORKAREA+5            L('FIELD' HEADER)                       
         BCTR  R3,0                     DECREMENT FOR EXECUTE                   
         MVC   WORKAREA+8(1),TWSERV     STRING SERVICE INTO 'FIELD'             
         MVI   WORKAREA+9,C','          STRING COMMA INTO 'FIELD'               
         LA    R5,WORKAREA+10           A(DATA IN 'FIELD')                      
*                                                                               
         EX    R3,PBT0080               INSERT DATA IN 'FIELD'                  
*                                                                               
         GOTO1 BOOKVAL,DMCB,WORKAREA,(8,WORKAREA+200),SCANNER                   
         MVC   PTTBKCTR,DMCB+4     SAVE BOOK COUNT                              
         ZIC   R5,DMCB+4           CALC SIZE OF OUTPUT BOOK FIELD               
         SR    R4,R4                                                            
         M     R4,=F'3'            # OF FIELDS * SIZE OF FIELD                  
         LR    R3,R5               SAVE L(CONVERTED BOOKS)                      
         BCTR  R5,0                DECREMENT FOR EXECUTE                        
         LA    R4,PTTBOOKS         A(FINAL OUTPUT)                              
*                                                                               
         EX    R5,PBT0081          MOVE FIELD BY LENGTH                         
*                                                                               
PBT0030  EQU   *                                                                
         LA    R6,LPTTPORT         L(ELEMENT W/O BOOKS)                         
         AR    R6,R3               ADD L(BOOKS) (IF ANY....)                    
         L     R3,ATWREQS          INCREMENT A(REQ) BY LENGTH                   
         AR    R3,R6                                                            
         ST    R3,ATWREQS                                                       
*                                                                               
         B     PBT0099             EXIT                                         
*                                                                               
PBT0080  MVC   0(0,R5),PTBOOKS                                                  
PBT0081  MVC   0(0,R4),WORKAREA+200                                             
*                                                                               
PBT0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R7                                                            
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*      STORES THE PAV BY NUMBER REQUESTS                                        
*                                                                               
PAVBYNUM NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT12IN05,R2                                                      
         L     R4,ATWREQS          A(NEXT AVAILABLE REQUEST STORAGE)            
         USING PAVSTORE,R4                                                      
*                                                                               
         MVC   PAVSTAT,PPSTAT      SET STATION                                  
         MVI   PTPRUN,1            SET REQUEST TYPE TO PAV BY NUMBER            
         MVC   PAVLIT#,PPPGM#      SAVE PAV EBCDIC VALUE                        
         PACK  WORKAREA(8),PPPGM#+1(2)                                          
         CVB   R1,WORKAREA         PROCESS TAPE NUMBER                          
         STC   R1,PAVNUMBR         SAVE CONVERTED TAPE NUMBER                   
         NI    PPPGM#+3,X'0F'      TURN OFF ZONE BITS OF DAY                    
         ZIC   R1,PPPGM#+3         LOAD TO REGISTER                             
         SLL   R1,4                SHIFT HIGH-ORDER 1 NYBBLE                    
         STC   R1,PPPGM#+3         RETURN IT TO STORAGE                         
         OC    PAVNUMBR+1(1),PPPGM#+3     MERGE INTO CONVERTED PGM#             
         CLI   PPPGM#+4,C'0'       CHECK WEEK INDICATOR                         
         BE    PB#0006             ZERO STAYS AS ZERO                           
         TM    PPPGM#+4,X'F0'      WEEK = 1 THRU 7?                             
         BO    PB#0002             YES - DON'T SET LOW-ORDER BIT                
         OI    PAVNUMBR+1,X'01'    NO  - SET LOW-ORDER BIT                      
PB#0002  EQU   *                                                                
         NI    PPPGM#+4,X'0F'      TURN OFF ZONE BITS OF WEEK                   
         ZIC   R1,PPPGM#+4         LOAD TO REGISTER                             
         SLL   R1,1                SHIFT HIGH-ORDER 1 BIT                       
         STC   R1,PPPGM#+4         RETURN IT TO STORAGE                         
         OC    PAVNUMBR+1(1),PPPGM#+4     MERGE INTO CONVERTED PGM#             
PB#0006  EQU   *                                                                
         MVC   PAVAVGCM,PPAVGCOM   AVERAGE/COMBO FLAG                           
         L     R5,DATALEN          L(DATA ELEMENT IN PROGRESS)                  
         LA    R6,LPPSTAT          L(DATA ELEM W/O BOOK ENTRY)                  
         LA    RE,WORKAREA              A(WORKAREA)                             
         XCEF  (RE),600                                                         
         SR    R5,R6                    L(BOOK ENTRIES IN ELEMENT)              
         LR    R3,R5                    SAVE L(INPUT ITEM DATA)                 
         LA    R5,2(R5)                 ADD 2 FOR 'SERVICE+,'                   
         STC   R5,WORKAREA+5            L('FIELD' HEADER)                       
         BCTR  R3,0                     DECREMENT FOR EXECUTE                   
         MVC   WORKAREA+8(1),TWSERV     STRING SERVICE INTO 'FIELD'             
         MVI   WORKAREA+9,C','          STRING COMMA INTO 'FIELD'               
         LA    R5,WORKAREA+10           A(DATA IN 'FIELD')                      
*                                                                               
         EX    R3,PB#0080               INSERT DATA IN 'FIELD'                  
*                                                                               
         GOTO1 BOOKVAL,DMCB,WORKAREA,(8,WORKAREA+200),SCANNER                   
         MVC   PAVBOOK(3),WORKAREA+200 SAVE SINGLE BOOK                         
*                                                                               
PB#0010  EQU   *                                                                
         L     R6,ATWREQS          SET TO NEXT AVAILABLE ADDRESS                
         LA    R6,LPAVSTOR(R6)                                                  
         ST    R6,ATWREQS          SAVE IT BACK                                 
         B     PB#0099             EXIT                                         
*                                                                               
PB#0080  MVC   0(0,R5),PPBOOK                                                   
*                                                                               
PB#0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   MULTIPLIES THE AMOUNT PASSED IN P1 BY THE VALUE PASSED IN P2, AND           
*     RETURNS THE RESULT IN P1.                                                 
*                                                                               
DECALIGN NTR1                                                                   
         L     R3,4(R1)            INSERT VALUE PASSED IN P2                    
         L     R5,0(R1)            INSERT AMOUNT PASSED IN P1                   
         SR    R4,R4                                                            
         MR    R4,R3               MULT AMOUNT BY VALUE                         
         ST    R5,DMCB                                                          
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*   RE-READS THE RECORDS IN PROGRESS AT TIME PREVIOUS EXECUTION                 
*     TERMINATED.  ANY ADDITIONAL DATA RESETTING IS ALSO DONE HERE.             
*                                                                               
RESETRUN NTR1                                                                   
*                                                                               
         XC    RSTRTEND,RSTRTEND   RESET RESTART NEEDED FLAG                    
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWREQS          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWREQS          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWDEMO          SAVE IT BACK                                 
RSET0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*   DETERMINES WHETHER THE REQUEST IS FOR PAV/TP BY DAY/TIME OR PAV             
*     BY NUMBER, AND CALLS THE APPROPRIATE ROUTINE                              
*                                                                               
PROCDATA NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BNE   PD0010              YES - DON'T RESET ADDRESSES                  
*                                                                               
*  ESTABLISH INITIAL ADDRESSES WITHIN ARRAYS                                    
*                                                                               
         LA    R2,TWBOOK           A(1ST BOOK)                                  
         ST    R2,ATWBOOK          A(BOOK IN PROCESS)                           
         LA    R2,TWDEMOS          A(1ST DEMO)                                  
         ST    R2,ATWDEMO          A(DEMO IN PROCESS)                           
         LA    R2,PTPSTORE         A(1ST REQUEST)                               
         ST    R2,ATWREQS          A(REQUEST IN PROCESS)                        
*                                                                               
         MVI   DEMOFLAG,C'N'       SET 'DESCRIPTIVE NOT SENT'                   
*                                                                               
PD0010   EQU   *                                                                
         L     R2,ATWREQS          LOAD IN CASE OF RESTART                      
         CLI   PTPRUN,0            PAV/TP BY D/T OR PAV BY NUMBER?              
         BNE   PD0020              NOT ZERO = BY NUMBER                         
         GOTO1 DTCYCLE,DMCB,(R2)   ZERO     = DAY/TIME                          
         B     PD0099                                                           
PD0020   EQU   *                                                                
         GOTO1 NUMCYCLE,DMCB,(R2)                                               
PD0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*     CONTROLS PROCESSING OF PAV/TP BY DAY/TIME.  IT PASSES EACH                
*     REQUEST, DRAWING THE DEMOGRAPHICS THAT CORRESPOND TO THE                  
*     REQUEST PARAMETERS, THEN FORMATS THE OUTPUT ELEMENTS                      
*     P1  =  A(REQUEST IN PROCESS)                                              
*                                                                               
DTCYCLE  NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
         USING PTTSTORE,R2                                                      
         LA    R6,DBDEMOB          A(DEMO BLOCK)                                
         USING DEMOD,R6                                                         
*                                                                               
         MVI   DBFUNCT,DBGETDEM    SET FUNCTION TO GET DEMO                     
         CLI   PTTPORT,C'P'        PAV OR TIME PERIOD?                          
         BNE   DTC0002             TIME PERIOD                                  
         MVC   DBFILE,=C'PAV'      SET BASIC PAV CALL STRUCTURE                 
         MVI   DBBEST,C'B'         SET PAV SELECTION TO 'BEST'                  
         B     DTC0004                                                          
DTC0002  EQU   *                                                                
         MVC   DBFILE,=C'TP '      SET BASIC TP  CALL STRUCTURE                 
DTC0004  EQU   *                                                                
         MVI   DBSELMED,C'T'       SET TO TV                                    
         MVC   DBSELSRC,TWSERV     INSERT SERVICE                               
         MVC   DBSELSTA,PTTSTAT    INSERT STATION                               
         MVI   DBTPTT,C'T'         ASK FOR TV TYPICAL                           
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    DTC0006             NO                                           
         ZIC   R5,BOOKLOOP         RESET CURRENT COUNT FOR BOOK LOOP            
         L     R4,ATWBOOK          RESET A(CURRENT BOOK)                        
         CLI   RSTRTRTN,3          RESTART AT 'DEMCALL'?                        
         BE    DTC0009             YES                                          
         CLI   RSTRTRTN,5          RESTART AT 'SENDDESC'?                       
         BE    DTC0009             YES - GO THROUGH DEMCALL                     
         CLI   RSTRTRTN,6          RESTART AT 'EOD'?                            
         BE    DTC0014             YES                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
DTC0006  EQU   *                                                                
*                                                                               
*   SET BOOK LOOP TO USE DETAIL LINE BOOKS, IF ENTERED, OR HEADLINE             
*       BOOKS                                                                   
*                                                                               
         LA    R4,TWBOOK           SET A(HEADLINE BOOKS)                        
         ZIC   R5,TWBKCTR          SET COUNT OF HEADLINE BOOKS                  
         CLI   PTTBKCTR,X'00'      ANY DETAIL BOOKS?                            
         BE    DTC0008             NO  - USE HEADLINE BOOKS                     
         LA    R4,PTTBOOKS         YES - USE DETAIL BOOKS                       
         ZIC   R5,PTTBKCTR         SET COUNT OF DETAIL BOOKS                    
DTC0008  EQU   *                                                                
*                                                                               
*  INSERT DAYS/TIMES FOR CURRENT BOOK AT THIS POINT.  THESE VALUES              
*    MAY CHANGE IF REQUEST IS PART OF AN AVERAGE/COMBO, AND MUST                
*    BE REFRESHED AT THIS POINT.                                                
*                                                                               
         MVC   DBSELDAY,PTTDAYS    INSERT DAYS                                  
         MVC   DBSELTIM,PTTTIME    INSERT TIMES                                 
         ST    R4,ATWBOOK          A(BOOK IN PROCESS)                           
         MVC   DBSELBK,1(R4)       INSERT BOOK FROM LIST                        
         MVC   DATEBOOK,DBSELBK    SAVE BOOK IN PROGRESS                        
DTC0009  EQU   *                                                                
         MVC   ACTFLAG,PTTAVCM     NO-ACTION/AVG/COMBO/JOIN FLAG                
*                                                                               
         GOTO1 DEMCALL,DMCB,(R2),0                                              
*                                                                               
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BNE   DTC0010             NO                                           
         STC   R5,BOOKLOOP         SAVE BOOK COUNTER FOR LOOP                   
         B     DTC0098             END AND RESTART                              
DTC0010  EQU   *                                                                
         LA    R4,3(R4)            BUMP A(BOOK IN PROCESS)                      
         CLI   TESTIT,0            **TEST**                                     
         BE    TEST1               **TEST**                                     
         LTR   R5,R5               **TEST**                                     
TEST1    EQU   *                   **TEST**                                     
         BCT   R5,DTC0008          GO BACK FOR NEXT BOOK                        
*                                                                               
DTC0011  EQU   *                                                                
         LR    R3,R2               SAVE A(CURRENT REQUEST)                      
*                                                                               
*   IF REQUEST IS FOR AVERAGE, COMBO, OR JOIN, SKIP NEXT REQUEST TOO            
*                                                                               
         LA    R5,LPTTPORT         INITIAL SIZE OF ENTRY                        
         AR    R3,R5               A(CURRENT)+L(BASIC ENTRY)                    
         CLI   PTTBKCTR,X'00'      ANY DETAIL LINE BOOKS?                       
         BE    DTC0012             NO - R2 NOW = LENGTH ENTRY                   
         GOTO1 CALCLBKS,DMCB,(R2)  YES - CALCULATE LENGTH                       
         L     R5,FULL             FULL = RETURNED LENGTH                       
         AR    R3,R5               ADD L(BOOKS) TO L(ENTRY)                     
DTC0012  EQU   *                                                                
         MVI   DESCFLAG,C'Y'       SET 'SEND DESCRIPTION'                       
         CLI   PTTAVCM,C'0'        R2 COVERS REQ JUST PROCESSED                 
         BE    DTC0013             NO-ACTION/NOT COMBO,AVG,JOIN                 
         CLI   PTTAVCM,C'3'        JOIN?                                        
         BNE   DTC0012E            NO                                           
         MVI   DESCFLAG,C'N'       YES - SUPPRESS DESCRIPTION                   
         B     DTC0013                                                          
DTC0012E EQU   *                                                                
         LR    R2,R3               AVG,COMBO: SET R2 TO NEXT REQ                
         B     DTC0011             KEEP GOING                                   
DTC0013  EQU   *                                                                
         LR    R2,R3               NEXT REQUEST TO PROCESS                      
         ST    R2,ATWREQS          SAVE IT BACK                                 
         MVI   DEMOFLAG,C'N'       RESET 'DESCRIPTIVE NOT SENT'                 
         CLI   0(R2),X'00'         ANY VALUE?                                   
         BNE   DTC0006             YES - PROCESS NEXT REQUEST                   
*                                                                               
*  ALL REQUESTS PROCESSED, NO RESTART NEEDED.  SEND END-OF-DATA ELEMENT         
*      WITH FRAME, SET LAST-FRAME INDICATOR                                     
*                                                                               
DTC0014  EQU   *                                                                
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   DTC0016             ELEMENT FITS - PROCEED                       
         MVI   RSTRTRTN,6          SET RESTART TO 'EOD NEEDED'                  
         B     XIT                 END FOR RESTART                              
DTC0016  EQU   *                                                                
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     DTC0099                                                          
DTC0098  EQU   *                                                                
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWREQS          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWREQS          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDEMO          SAVE IT BACK                                 
DTC0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
*  THIS ROUTINE:                                                                
*       CALCULATES THE LENGTH OF THE BOOK FIELDS IN THE REQUEST, BASED          
*       UPON THE NUMBER OF BOOKS IN FIELD 'PTTBKCTR', AND RETURNS               
*       THAT VALUE IN 'FULL'                                                    
*       P1  =  A(CURRENT REQUEST)                                               
CALCLBKS NTR1                                                                   
         L     R2,0(R1)            LOAD A(CURRENT REQUEST)                      
         USING PTTSTORE,R2                                                      
*                                                                               
         ZIC   R5,PTTBKCTR         GET BOOK COUNTER                             
         SR    R4,R4                                                            
         M     R4,=F'3'            # BOOKS * L(BOOK FIELD) = SIZE               
         ST    R5,FULL             STORE FOR RETURN                             
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  THIS ROUTINE:                                                                
*       PROCESSES REQUESTS FOR PAV BY NUMBER, AND WILL AVERAGE                  
*       OR COMBINE MULTIPLE LINES, PRODUCING A SINGLE OUTPUT FIGURE             
*                                                                               
NUMCYCLE NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
         USING PAVSTORE,R2                                                      
         LA    R6,DBDEMOB          A(DEMO BLOCK)                                
         USING DEMOD,R6                                                         
*                                                                               
         MVC   DBFILE,=C'PAV'      SET BASIC PAV CALL STRUCTURE                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELMED,C'T'       SET TO TV                                    
         MVC   DBSELSRC,TWSERV     INSERT SERVICE                               
         MVC   DBSELSTA,PAVSTAT    INSERT STATION                               
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    NCY0004             NO                                           
         CLI   RSTRTRTN,3          RESTART AT 'DEMCALL'?                        
         BE    NCY0006             YES                                          
         CLI   RSTRTRTN,5          RESTART AT 'SENDDESC'?                       
         BE    NCY0006             YES - GO THROUGH DEMCALL                     
         CLI   RSTRTRTN,6          RESTART AT 'EOD' POINT?                      
         BE    NCY0008             YES                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
NCY0004  EQU   *                                                                
         MVI   DEMOFLAG,C'N'       RESET 'DESCRIPTIVE NOT SENT'                 
         MVC   DBSELBK,PAVBOOK+1   INSERT PAV BOOK                              
         MVC   DATEBOOK,PAVBOOK+1  SAVE BOOK IN PROGRESS                        
         MVC   DBSELPUR,PAVNUMBR   INSERT PROGRAM NUMBER                        
*                                                                               
NCY0006  EQU   *                                                                
         MVC   ACTFLAG,PAVAVGCM    NO-ACTION/AVG/COMBO/JOIN FLAG                
         GOTO1 DEMCALL,DMCB,(R2),1                                              
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    NCY0098             YES - END AND RESTART                        
*                                                                               
NCY0006C EQU   *                                                                
         LR    R3,R2                                                            
*                                                                               
*   IF REQUEST IS FOR AVERAGE, COMBO, JOIN, SKIP NEXT REQUEST ALSO              
*                                                                               
         MVI   DESCFLAG,C'Y'       SET 'SEND DESCRIPTION'                       
         LA    R3,LPAVSTOR(R3)     BUMP TO NEXT REQUEST                         
         CLI   PAVAVGCM,C'0'       R2 COVERS REQ JUST PROCESSED                 
         BE    NCY0006E            NO-ACTION/NOT COMBO,AVG,JOIN                 
         CLI   PAVAVGCM,C'3'       JOIN?                                        
         BNE   NCY0006D            NO                                           
         MVI   DESCFLAG,C'N'       YES - SUPPRESS DESCRIPTION                   
         B     NCY0006E                                                         
NCY0006D EQU   *                                                                
         LR    R2,R3               AVG,COMBO,JOIN: SET R2 TO NEXT REQ           
         B     NCY0006C            KEEP GOING                                   
NCY0006E EQU   *                                                                
         LR    R2,R3                                                            
         ST    R2,ATWREQS          SAVE A(NEW REQUEST)                          
         CLI   0(R2),X'00'         ANY VALUE?                                   
         BNE   NCY0004             YES - PROCESS IT                             
*                                                                               
*  ALL REQUESTS PROCESSED, NO RESTART NEEDED.  SEND EOD ELEMENT                 
*      WITH FRAME, SET LAST-FRAME INDICATOR                                     
*                                                                               
NCY0008  EQU   *                                                                
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   NCY0010             ELEMENT FITS - PROCEED                       
         MVI   RSTRTRTN,6          SET RESTART TO 'EOD NEEDED'                  
         B     XIT                 END FOR RESTART                              
NCY0010  EQU   *                                                                
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     NCY0099                                                          
NCY0098  EQU   *                                                                
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWREQS          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWREQS          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDEMO          SAVE IT BACK                                 
NCY0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
*  THIS ROUTINE                                                                 
*     MAKES A DEMAND CALL, BASED ON THE PARAMETERS SET UP, AND THE              
*     PRODUCES THE DESCRIPTIVE ELEMENT AND TRACK ELEMENT                        
*     P1  =  ADDRESS OF REQUEST IN PROCESS                                      
*     P2  =  FLAG FOR ORIGIN:   0  =  DTCYCLE, 1  =  NUMCYCLE                   
*                                                                               
DEMCALL  NTR1                                                                   
         L     R2,0(R1)            A(REQUEST IN PROCESS)                        
         MVC   DTORNUM,7(R1)       SAVE VALUE IN P2                             
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    DEM0002             NO                                           
         CLI   RSTRTRTN,3          RESTART AT DEMO INSERT?                      
         BE    DEM0024             YES                                          
         CLI   RSTRTRTN,5          RESTART AT 'SENDDESC'?                       
         BE    DEM0012             YES                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
DEM0002  EQU   *                                                                
         LA    RE,WORKAREA         INITIALIZE ENTIRE WORKAREA                   
         XCEF  (RE),600                                                         
         L     RE,AADDIO           INITIALIZE INTERIM DEMO STORAGE              
         XCEF  (RE),1000                                                        
*                                                                               
*   RETRIEVE FIGURES FOR THIS REQUEST                                           
*                                                                               
         GOTO1 DEMNDRTN,DMCB,WORKAREA+400,(R2)                                  
*                                                                               
DEM0010  EQU   *                                                                
         CLI   DEMOFLAG,C'Y'       DESCRIPTIVE DATA SENT?                       
         BE    DEM0014                                                          
         MVI   DEMOFLAG,C'Y'       SET TO 'SENT'                                
DEM0012  EQU   *                   SENDDESC RESTART ENTRY POINT                 
         CLI   DESCFLAG,C'N'       SUPPRESS DURING 'JOIN'?                      
         BE    DEM0014             YES                                          
         GOTO1 SENDDESC,DMCB,(R2)  SEND DESCRIPTIVE DATA                        
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    DEM0099             YES - END AND RESTART                        
DEM0014  EQU   *                                                                
         MVI   DEMODATA,C'N'       TURN OFF COMBO DATA FLAG                     
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         LA    R6,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO03,R6          OUTPUT ELEMENT DSECT                         
*                                                                               
         MVC   DVCOMBO,DEMODATA    SET COMBO/NO COMBO FLAG                      
*                                                                               
         LA    R3,DVBOOK           SET A(BOOK DATE)                             
         MVC   DATECONV,DATEBOOK   SET UP DATE OF TRACK                         
         GOTO1 DATCON,DMCB,(3,DATECONV),(6,(R3))                                
         EDIT  TWTOTDEM,(2,DVDEMOCT),FILL=0                                     
         LA    R3,DVDEMLEN         A(DEMOS WITHIN ITEM)                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R4,WORKAREA+400     A(DEMOS: 4-BYTE ENTRIES)                     
         ZIC   R5,TWTOTDEM         # OF DEMOS REQUESTED                         
DEM0016  EQU   *                                                                
         GOTO1 HEXOUT,DMCB,(R4),HEXOUTWK,4,=C'TOG'                              
         LA    R2,HEXOUTWK         A(HEXOUT DEMO VALUE)                         
         LA    R6,8                # OF CHARACTERS TO CHECK                     
DEM0018  EQU   *                                                                
         TM    0(R2),X'0F'         DECL BITS ON IN BYTE?                        
         BM    DEM0020             YES - SIGNIFICANT POSITION                   
         LA    R2,1(R2)            NO  - CHECK NEXT POSITION                    
         BCT   R6,DEM0018                                                       
         MVI   0(R3),C'0'          NO SIGNIFICANT POSITIONS                     
         B     DEM0022                                                          
DEM0020  EQU   *                                                                
         EDIT  (R6),(1,(R3))       INSERT 1-CHAR LENGTH COUNT                   
         EX    R6,DEM0080          MOVE VALUE BY LENGTH                         
DEM0022  EQU   *                                                                
         LA    R6,1(R6)            BUMP A(O/P) BY LEN ATTR + LENGTH             
         AR    R3,R6                                                            
         LA    R4,4(R4)            NEXT DEMO VALUE                              
         BCT   R5,DEM0016          DO EACH                                      
*                                                                               
DEM0024  EQU   *                                                                
         MVC   TESTIT,RSTRTRTN     **TEST**                                     
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    DEM0026             NO                                           
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         L     R3,RSTRTLEN         RESET L(NEW ELEMENT)                         
         B     DEM0028                                                          
DEM0026  EQU   *                                                                
         LA    R4,ELTAREA          CALCULATE LENGTH OF NEW ITEM                 
         SR    R3,R4               R3=NEXT DEMO VALUE                           
         ST    R3,RSTRTLEN         SAVE LENGTH FOR POSS. RESTART                
DEM0028  EQU   *                                                                
         LA    R2,ITRDMFDV         INSERT ITEM TYPE                             
         GOTO1 PUTITEM,DMCB,(R2),(R3),ELTAREA                                   
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   DEM0099             ITEM FITS - EXIT                             
*                                                                               
         MVI   RSTRTRTN,3          SET RESTART AS 'INSERT DEMOS'                
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     DEM0099                                                          
*                                                                               
DEM0080  MVC   1(0,R3),0(R2)                                                    
*                                                                               
DEM0099  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CONSOLIDATES ALL CALLS TO DEMAND, AS WELL AS SUBSEQUENT DEVELOPING         
*        OF FINAL WEIGHTED AVERAGING.  THE LOCATION OF THE FINAL VALUES         
*        IS PASSED IN AS P1.                                                    
*                                                                               
DEMNDRTN NTR1                                                                   
*                                                                               
         LA    R6,DBDEMOB             SET A(DEMO BLOCK)                         
         USING DEMOD,R6                                                         
*                                                                               
         MVC   STORE16(4),0(R1)    P1  =  A(FINAL DEMO VALUES)                  
         L     R2,4(R1)            P2  =  A(REQUEST IN PROCESS)                 
         LR    R3,R2               SET ADDITIONAL A(REQ IN PROCESS)             
*                                                                               
         L     RE,AACCUM           INITIALIZE AVERAGE/COMBO STORAGE             
         XCEF  (RE),1000                                                        
         MVI   AVGWT,0                                                          
         XC    PROGNAMA(LPROGNAM),PROGNAMA                                      
*                                  WIPE OUT PROGRAM NAMES                       
*                                                                               
DMND0001 EQU   *                                                                
         L     RE,AINTEREC         INITIALIZE INTERIM STORAGE                   
         XCEF  (RE),1000                                                        
*                                                                               
         GOTO1 DEMAND,DMCB,DBDEMOB,DRAWDEMS                                     
*                                                                               
* A CALL TO DEMAND MAY NOT PRODUCE ANYTHING.  HOWEVER, IT WILL BE               
*     CONSIDERED THE RESPONSIBILITY OF THE USER TO ENSURE THAT                  
*     WHAT HAS BEEN ENTERED IS VALID, AND THAT INFORMATION HAS                  
*     BEEN FOUND.  IF NOT FOUND, WILL SEND BACK ALL ZERO WITH THE               
*     NUMBER.  IF PART OF A COMBO/AVERAGE, WILL INFLUENCE THE                   
*     FINAL FIGURES PROPORTIONALLY.                                             
*     (THIS CAN BE CHANGED AS NECESSARY, OF COURSE......)                       
*                                                                               
         ZICM  RF,DBDIVSOR,2       ANYTHING FOUND?                              
         LTR   RF,RF                                                            
         BZ    DMND0005            NO  - SKIP DIVIDE ROUTINE                    
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHFCTR+2(2),DBDIVSOR   SET TOTAL WEIGHT                         
*                                                                               
         GOTO1 DEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC                
*                                                                               
*   IF REQUEST IS TO AVERAGE/COMBO, MULTIPLE PASSES THROUGH 'DEMAND'            
*       MUST BE MADE TO PICK UP THE OTHER COMPONENTS FOR THIS BOOK              
*                                                                               
DMND0005 EQU   *                                                                
         CLI   ACTFLAG,C'0'        NO-ACTION/AVG/COMBO/JOIN?                    
         BE    DMND0030            NO ACTION                                    
*                                                                               
*  ACCUMULATE THE RESULT IN ANOTHER INTERIM AREA - WEIGHT IT '1'                
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
         LA    R1,DBLOCK                                                        
         ST    R1,MTHCFACS                                                      
         MVC   MTHIFIL,DBFILE      FILE FORMAT INPUT                            
         MVC   MTHOFIL,DBFILE      FILE FORMAT OUTPUT                           
*                                                                               
         GOTO1 DEMOMTH,DMCB,=C'ADD',AINTEREC,AACCUM,MATHFAC                     
*                                                                               
         ZIC   R1,AVGWT            INCREMENT AVERAGE WEIGHT                     
         LA    R1,1(R1)                                                         
         STC   R1,AVGWT            SAVE IT BACK                                 
         CLI   DTORNUM,0           DAY/TIME OR NUMBER REQUEST?                  
         BNE   DMND0008            NUMBER REQUEST                               
         CLI   PTTAVCM-PTTBASE(R2),C'0'                                         
         BE    DMND0020            THIS ITEM NOT AVG/COMBO - FINISHED           
         LA    R5,LPTTPORT         INITIAL SIZE OF ENTRY                        
         AR    R2,R5               A(CURRENT)+L(BASIC ENTRY)                    
         CLI   PTTBKCTR-PTTBASE(R2),0   ANY DETAIL LINE BOOKS?                  
         BE    DMND0006            NO  - R2 NOW POINTS TO NEXT REQ              
         GOTO1 CALCLBKS,DMCB,(R3)  YES - CALCULATE LENGTH                       
         L     R5,FULL             FULL = RETURNED LENGTH                       
         AR    R2,R5               ADD L(BOOKS) TO L(ENTRY)                     
DMND0006 EQU   *                                                                
*                                                                               
*   FOR PAV/TP BY DAY/TIME, THE NEW REQUEST'S DAY/TIME STRINGS MUST             
*     BE LOADED INTO THE DBLOCK FIELDS                                          
*                                                                               
         MVC   DBSELDAY,PTTDAYS-PTTBASE(R2)                                     
         MVC   DBSELTIM,PTTTIME-PTTBASE(R2)                                     
         B     DMND0001            GO BACK FOR NEXT REQUEST                     
DMND0008 EQU   *                                                                
*                                                                               
*   FOR PAV BY NUMBER, THE NEW REQUEST'S PROGRAM NUMBER MUST                    
*     BE LOADED INTO THE DBLOCK FIELD                                           
*                                                                               
         CLI   PAVAVGCM-PAVBASE(R2),C'0'                                        
         BE    DMND0020            THIS ITEM NOT AVG/COMBO - FINISHED           
         LA    R2,LPAVSTOR(R2)     BUMP TO NEXT REQUEST                         
         MVC   DBSELPUR,PAVNUMBR-PAVBASE(R2)                                    
         B     DMND0001            GO BACK FOR NEXT REQUEST                     
DMND0020 EQU   *                                                                
         L     R2,AINTEREC         INTERIM WORK AREA                            
         ST    R6,FULL             TEMPORARILY STORE R6                         
         L     R6,AACCUM           AVERAGE/COMBO DEMO STORAGE                   
         LA    R3,1000             LENGTH OF MOVE                               
         LR    R7,R3               LIKEWISE                                     
         MVCL  R2,R6               MOVE ACCUMULATORS TO INTERIM STORE           
         L     R6,FULL             RESET R6                                     
         MVC   DMCB(4),STORE16     LOAD A(FINAL OUTPUT)                         
         GOTO1 LASTCALC,DMCB       CALCULATE FINAL AVERAGE                      
         B     DMND0099            EXIT                                         
*                                                                               
DMND0030 EQU   *                                                                
         MVC   STORE16+4(4),DBAREC     SAVE ORIGINAL VALUES                     
         MVC   STORE16+8(4),DBAQUART                                            
         MVC   DBAREC,AINTEREC         SET A(INTERIM RECORD)                    
         L     RE,AINTEREC             SET A(1ST ELEMENT)                       
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVC   DMCB+8(4),STORE16       LOAD A(FINAL OUTPUT)                     
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',TWDEMOS),DBDEMOB                               
*                                                                               
         MVC   DBAREC,STORE16+4        RESET ORIGINAL VALUES                    
         MVC   DBAQUART,STORE16+8                                               
DMND0099 EQU   *                                                                
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
*                                                                               
         DROP  R4                                                               
*                                                                               
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
*  ACCUMULATE REBUILT RECORD FROM IUNWORK INTO A(INTEREC)                       
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         LH    R1,DBFACTOR         ITEM WEIGHT                                  
         ST    R1,MTHFCTR          SET WEIGHTING VALUE                          
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
         CLI   DTORNUM,0           DAY/TIME OR PROGRAM NUMBER?                  
         BE    XIT                 ZERO = DAY/TIME REQUEST                      
         GOTO1 GETNAME,DMCB        NOT ZERO = PROGRAM NUMBER REQUEST            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*    DOES A 'DEFINE' CALL FOR EACH PROGRAM RECORD, GETTING TITLE                
*    IF TWO TITLES, SEPARATES THEM WITH '-'                                     
*    IF MORE THAN TWO, SEPARATES THEM WITH '/'                                  
*                                                                               
GETNAME  NTR1                                                                   
         LA    R4,PROGNAMA         A(1ST NAME)                                  
         OC    0(16,R4),0(R4)      ANY NAME ALREADY FOUND?                      
         BZ    GNAM0016            NO  - SET FIRST NAME                         
         LA    R4,PROGNAMB         YES - SET SECOND NAME                        
         CLI   PROGSEP,X'00'       ANY SEPARATOR?                               
         BNE   GNAM0004            YES - EITHER '-' OR '/'                      
         MVI   PROGSEP,C'-'        NO  - SET TO '-'                             
         B     GNAM0016                                                         
GNAM0004 EQU   *                                                                
         MVI   PROGSEP,C'/'        SET SEPARATOR TO '/'                         
GNAM0016 EQU   *                                                                
         LA    R6,DBDEMOB          SET A(DEMO BLOCK)                            
         USING DEMOD,R6                                                         
         PRINT GEN                                                              
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,(R4),RR=RELO                      
         PRINT NOGEN                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
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
         LA    RE,23(RE)                                                        
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
*    EXTRACTS DESCRIPTIVE INFORMATION FROM DEMO HEADER RECORD, WHICH            
*        IS STORED IN THE SECOND IO AREA (AIO2), AND INSERTS IT INTO            
*        THE RETURN FRAME.                                                      
*                                                                               
SENDDESC NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         L     R3,0(R1)            SAVE A(REQUEST IN PROCESS)                   
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BNE   SDES0024            YES - SKIP TO INSERT ELEMENT                 
*                                                                               
         XC    DUB,DUB             USE AS A PASS-THROUGH AREA                   
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO01,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         CLI   DTORNUM,0           DAY/TIME REQUEST?                            
         BNE   SDES0016            NO  - PROGRAM # REQUEST                      
*                                                                               
         MVC   DHSTAT(5),PTTSTAT-PTTBASE(R3)                                    
         CLI   PTTAVCM-PTTBASE(R3),C'0'    NO-ACTION?                           
         BE    SDES0012                    YES                                  
         CLI   PTTAVCM-PTTBASE(R3),C'1'    AVERAGE?                             
         BNE   SDES0004                    NO                                   
         MVC   DHINV#,=C'AVG '                                                  
         B     SDES0012                                                         
SDES0004 EQU   *                                                                
         CLI   PTTAVCM-PTTBASE(R3),C'2'    COMBO?                               
         BNE   SDES0008                    NO                                   
         MVC   DHINV#,=C'CMBO'                                                  
         B     SDES0012                                                         
SDES0008 EQU   *                                                                
         CLI   PTTAVCM-PTTBASE(R3),C'3'    JOIN?                                
         BNE   SDES0012                    YES                                  
         MVC   DHINV#,=C'JOIN'                                                  
SDES0012 EQU   *                                                                
         GOTO1 LOADTITL,DMCB,(R2),(R3)                                          
         B     SDES0020                                                         
SDES0016 EQU   *                                                                
         MVC   DHSTAT(5),PAVSTAT-PAVBASE(R3)                                    
         MVC   DHINV#,PAVLIT#+1-PAVBASE(R3)                                     
         GOTO1 LOADNAME,DMCB,(R2),(R3)                                          
*                                                                               
SDES0020 EQU   *                                                                
         MVI   DHCOMPET,C'N'                                                    
         MVI   DHREPEAT,C'N'       SET REPEAT FLAG TO 'NO'                      
*                                                                               
*  NOTE:  TEMPORARILY (UNTIL THIS MECHANISM IS DEFINED) ALL ELEMENTS            
*        ARE RETURNED AS COMPETITIVE=N                                          
SDES0024 EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    SDES0028            NO                                           
         MVI   RSTRTRTN,0          INITIALIZE RESTART FLAG                      
         L     R3,RSTRTLEN         RESET L(TITLE) FOR RESTART                   
         B     SDES0032                                                         
SDES0028 EQU   *                                                                
         LA    R2,ITRDMFDS         SET ITEM TYPE                                
         LA    R3,LDHDATA          SET ITEM LENGTH                              
         L     RF,DUB              ADD L(TITLE) IF ANY                          
         AR    R3,RF                                                            
         ST    R3,RSTRTLEN         SAVE L(TITLE) FOR POSS. RESTART              
SDES0032 EQU   *                                                                
         GOTO1 PUTITEM,DMCB,(R2),(R3),ELTAREA                                   
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   SDES0099            ELEMENT FITS - EXIT ROUTINE                  
*                                                                               
         MVC   RSTRTRTN,5          SET 'RESTART AT SENDDESC'                    
*                                  ORIGINATING AT 'RTN' FLAG                    
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     SDES0099            EXIT                                         
*                                                                               
SDES0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE INSERTS EXPANDED DAY STRING, THEN '/', THEN EXPANDED                 
*    TIME STRING INTO TITLE FIELD.  PASSES BACK LENGTH OF TITLE                 
*    FIELD IN 'DUB'. ON INPUT,                                                  
*        P1  =  A(ELTAREA)                                                      
*        P2  =  A(REQUEST IN PROGRESS)                                          
*                                                                               
LOADTITL NTR1                                                                   
         L     R2,0(R1)            A(ELTAREA)                                   
         USING CT1XO01,R2                                                       
         L     R3,4(R1)                                                         
         GOTO1 =V(DAYUNPK),DMCB,PTTDAYS-PTTBASE(R3),DHTITLE,RR=RELO             
         LA    R4,DHTITLE          A(TITLE FIELD)                               
LOTI0004 EQU   *                                                                
         CLI   0(R4),X'00'         FIND FIRST EMPTY POSITION                    
         BE    LOTI0008                                                         
         LA    R4,1(R4)                                                         
         B     LOTI0004                                                         
LOTI0008 EQU   *                                                                
         MVI   0(R4),C'/'          INSERT SEPARATOR                             
         LA    R4,1(R4)            SET A(NEXT POSITION)                         
         GOTO1 =V(UNTIME),DMCB,PTTTIME-PTTBASE(R3),(R4),RR=RELO                 
LOTI0012 EQU   *                                                                
         CLI   0(R4),X'00'         FIND LAST CHARACTER                          
         BE    LOTI0016                                                         
         LA    R4,1(R4)                                                         
         B     LOTI0012                                                         
LOTI0016 EQU   *                                                                
         LA    RF,DHTITLE          CALCULATE L(TITLE)                           
         SR    R4,RF               A(END) - A(START) = LENGTH                   
         ST    R4,DUB              SET TO PASS BACK                             
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE INSERTS PROGRAM NAME A, THEN SEPARATOR, THEN PROGRAM                 
*    NAME B INTO TITLE FIELD.  PASSES BACK LENGTH OF TITLE                      
*    FIELD IN 'DUB'. ON INPUT,                                                  
*        P1  =  A(ELTAREA)                                                      
*                                                                               
LOADNAME NTR1                                                                   
         L     R2,0(R1)            A(ELTAREA)                                   
         USING CT1XO01,R2                                                       
         SR    R4,R4               ZERO TOTAL TITLE LENGTH                      
         LA    R3,DHTITLE          A(TITLE FIELD)                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R2,PROGNAMA+15      A(LAST POSITION, 1ST NAME)                   
         LA    RF,15               LOOP COUNTER                                 
LNAM0004 EQU   *                                                                
         CLI   0(R2),C' '          FIND FIRST NON-SPACE                         
         BNE   LNAM0008            FOUND                                        
         BCTR  R2,0                GO BACK ONE SPACE                            
         BCT   RF,LNAM0004                                                      
LNAM0008 EQU   *                                                                
         LA    R2,PROGNAMA         SET START OF FIRST NAME                      
         EX    RF,LNAM0080         MOVE BY LENGTH                               
         LA    RF,1(RF)            +1 = LENGTH REMAINING                        
         AR    R4,RF               ACCUMULATE TOTAL LENGTH                      
         AR    R3,RF               FIND NEXT POSITION                           
         CLI   PROGSEP,X'00'       ANY SEPARATOR?                               
         BE    LNAM0096            NO  - ONLY ONE NAME - EXIT                   
         LA    R4,1(R4)            YES - ADD 1 FOR IT                           
         MVC   0(1,R3),PROGSEP     MOVE SEPARATOR                               
         LA    R3,1(R3)            BUMP OVER TO ACCEPT SEPARATOR                
         LA    R2,PROGNAMB+15      A(LAST POSITION, 2ND NAME)                   
         LA    RF,15               LOOP COUNTER                                 
LNAM0012 EQU   *                                                                
         CLI   0(R2),C' '          FIND FIRST NON-SPACE                         
         BNE   LNAM0016            FOUND                                        
         BCTR  R2,0                GO BACK ONE SPACE                            
         BCT   RF,LNAM0012                                                      
LNAM0016 EQU   *                                                                
         LA    R2,PROGNAMB         SET START OF SECOND NAME                     
         EX    RF,LNAM0080         MOVE BY LENGTH                               
         LA    RF,1(RF)            +1 = LENGTH REMAINING                        
         AR    R4,RF               ACCUMULATE TOTAL LENGTH                      
         B     LNAM0096            LOAD OUTPUT AND EXIT                         
*                                                                               
LNAM0080 MVC   0(0,R3),0(R2)       LOAD TITLE BY LENGTH                         
*                                                                               
LNAM0096 EQU   *                                                                
         ST    R4,DUB              SET LENGTH FOR PASS-BACK                     
LNAM0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
         SPACE 4                                                                
       ++INCLUDE RESVCTAB                                                       
       SPACE 4                                                                  
DATECONV DS    CL2                 THREE-BYTE DATE FIELD                        
DTFILLER DC    XL1'1'              DAY SET TO 1 ALWAYS                          
MYSPACES DC    CL20'                    '                                       
MYLOWVAL DC    XL4'00000000'                                                    
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
*                                                                               
PROGNAMA DS    CL16                                                             
PROGSEP  DS    CL1                                                              
PROGNAMB DS    CL16                                                             
LPROGNAM EQU   *-PROGNAMA                                                       
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
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CT12DI01:  BASIC HEADLINE INFORMATION                                         
       ++INCLUDE CT12DI01                                                       
         SPACE 4                                                                
* CT12DI02:  BOOK LIST                                                          
         SPACE 4                                                                
       ++INCLUDE CT12DI02                                                       
         SPACE 4                                                                
* CT12DI03:  DEMOGRAPHICS REQUESTED                                             
         SPACE 4                                                                
       ++INCLUDE CT12DI03                                                       
         SPACE 4                                                                
* CT12DI04:  PAV/TP REQUESTED BY DAY/TIME                                       
         SPACE 4                                                                
       ++INCLUDE CT12DI04                                                       
         SPACE 4                                                                
* CT12DI05:  PAV REQUESTED BY NUMBER                                            
         SPACE 4                                                                
       ++INCLUDE CT12DI05                                                       
         SPACE 4                                                                
*                                                                               
*  NOTE:  THE OUTPUT DSECTS HAVE BEEN NUMBERED TO AGREE WITH THE                
*         PROGRAM NUMBER.  HOWEVER, THE ACTUAL ITEM CODES ARE                   
*         BEING SET TO THE '3000' SERIES.                                       
*                                                                               
* CT1XDO01:  DESCRIPTIVE ITEM                                                   
         SPACE 4                                                                
       ++INCLUDE CT1XDO01                                                       
         SPACE 4                                                                
* CT1XDO03:  DEMOGRAPHIC VALUES DERIVED                                         
         SPACE 4                                                                
       ++INCLUDE CT1XDO03                                                       
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
TWSTDT   DS    CL3                 FLIGHT START DATE                            
TWENDT   DS    CL3                 FLIGHT END DATE                              
TWCOMPET DS    CL1                 COMPETITIVE REQUESTED FLAG                   
TWRATES  DS    CL1                 RATES REQUESTED FLAG                         
TWBKCTR  DS    CL1                 COUNT OF BOOKS ENTERED                       
TWBOOK   DS    9CL3                EIGHT BOOK ENTRIES + DELIMITER               
TWDEMOS  DS    34CL3               DEMO CODES + DELIMITER                       
LTWDEMOS EQU   *-TWDEMOS                                                        
TW#DEMOS DS    CL1                 DEMO COUNT REQUESTED                         
TWTOTDEM DS    CL1                 TOTAL # DEMOS                                
ATWBOOK  DS    A                   A(BOOK IN PROGRESS)                          
ATWDEMO  DS    A                   A(DEMO IN PROGRESS)                          
ATWREQS  DS    A                   A(INVENTORY IN PROGRESS)                     
AVGWT    DS    XL1                 AVERAGE WEIGHT                               
PTPRUN   DS    XL1                 TYPE OF REQUEST                              
*                                  0  =  REQUEST BY DAYPART                     
*                                  1  =  REQUEST BY INVENTORY NUMBER            
ACTFLAG  DS    XL1                 ACTION                                       
*                                  0  =  INDIVIDUAL INVENTORY                   
*                                  1  =  AVERAGE                                
*                                  2  =  COMBO                                  
*                                  3  =  JOIN                                   
SAVSRCBK DS    CL3                 SAVE AREA FOR SOURCE/BOOK                    
BOOKLOOP DS    CL1                 SAVE CURRENT COUNT OF BOOK LOOP              
DATEBOOK DS    CL2                 INTERIM SAVE OF BOOK IN PROGRESS             
DTORNUM  DS    CL1                 DEMCALL ORIGIN: 0=DT,1=NUM                   
KEY92SAV DS    CL27                SAVE AREA:X'92' PASSIVE D/P KEY              
DEMODATA DS    CL1                 FLAG: COMBO/NON-COMBO (Y/N)                  
DEMOFLAG DS    CL1                 FLAG: SEND MASTER DATA ONLY ONCE             
DESCFLAG DS    CL1                 FLAG: SUPPRESS/DON'T SUPP DESCRIPT           
INVFOUND DS    CL1                 FLAG: INVENTORY FOUND/NOT FOUND              
TESTDUMP DS    CL1                 COUNTER FOR SERIAL DUMPS                     
         DS    0D                  ALIGNMENT                                    
HEXOUTWK DS    CL8                 WORK SPACE FOR HEXOUT                        
WEIGHT   DS    CL4                 WEIGHTING FOR DEMOS                          
ELTAREA  DS    CL250               ELEMENT BUILD AREA                           
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
AACCUM   DS    A                   A(INTERIM ACCUMS FOR AVERAGE/COMBO)          
RSTRTRTN DS    CL1                 FLAG TO ROUTINE TERMINATING                  
*                                  3  =  UPGRADE                                
*                                  5  =  SENDDESC                               
*                                  6  =  END OF DATA ELEMENT                    
TESTIT   DS    CL1                 **TEST**                                     
RSTRTEND DS    CL1                 NON-0 = END JOB WITH RESTART                 
SAVERANK DS    CL1                 RANK SAVED FLAG                              
*                                                                               
PTPSTORE DS    600C                INVENTORY REQUEST STORAGE                    
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
*                                                                               
*                                                                               
*  PAV/TP BY DAY/TIME DSECT                                                     
PTTSTORE DSECT                                                                  
PTTBASE  DS    0C                                                               
PTTPORT  DS    CL1         0       DATA TYPE FLAG                               
*                                  P  =  PAV                                    
*                                  T  =  TIME PERIOD                            
PTTSTAT  DS    CL5        +1       STATION + MEDIA                              
PTTDAYS  DS    CL2        +6       DAY FIELD                                    
PTTTIME  DS    CL4        +8       TIME FIELD                                   
PTTAVCM  DS    CL1        +12      AVERAGE/COMBO/JOIN FLAG                      
*                                  0  =  NO ACTION                              
*                                  1  =  AVERAGE FOLLOWING ITEM                 
*                                  2  =  COMBO   FOLLOWING ITEM                 
*                                  3  =  JOIN    FOLLOWING ITEM                 
PTTBKCTR DS    CL1        +13      COUNT OF BOOKS ENTERED                       
*                                                                               
LPTTPORT EQU   *-PTTPORT           L(ELEMENT W/O BOOKS)                         
PTTBOOKS DS    CL1        +14      VARIABLE LENGTH BOOK FIELD                   
*                                                                               
*                                                                               
*  PAV BY NUMBER DSECT                                                          
PAVSTORE DSECT                                                                  
PAVBASE  DS    0C                                                               
PAVSTAT  DS    CL5       +0        STATION + MEDIA                              
PAVNUMBR DS    CL2       +5        PAV NUMBER                                   
PAVLIT#  DS    CL5       +7        PAV NUMBER: EBCDIC                           
PAVAVGCM DS    CL1       +12       AVERAGE/COMBINE FLAG                         
*                                  0  =  NO ACTION                              
*                                  1  =  AVERAGE FOLLOWING ITEM                 
*                                  2  =  COMBO WITH FOLLOWING ITEM              
*                                  3  =  JOIN  WITH FOLLOWING ITEM              
PAVBOOK  DS    CL3       +13       SPECIFIC BOOK FOR PAV                        
*                                                                               
LPAVSTOR EQU   *-PAVSTAT           LENGTH OF REQUEST ENTRY                      
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
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099CTMAD0C   05/10/05'                                      
         END                                                                    
