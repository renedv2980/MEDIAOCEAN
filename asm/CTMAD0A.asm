*          DATA SET CTMAD0A    AT LEVEL 002 AS OF 05/01/02                      
*PHASE TA0C0AA,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE REGETIUN                                                               
         TITLE 'TA0C0A - $MAD REP DEMOGRAPHIC RETRIEVAL'                        
**********************************************************************          
*   HISTORY OF CHANGES                                               *          
**********************************************************************          
*   03/27/91   (BU ) --- ORIGINAL ENTRY                              *          
*   11/22/91   (BU ) --- INCREASE SIZE OF ELTAREA FROM 100 TO 160    *          
*   02/14/92   (BU ) --- CORRECT ASSIGNMENT OF DEMO QUALIFIERS.  ALSO*          
*                        SKIP RESTRUCTURING INVENTORY RECORDS.  FIX  *          
*                        EFFECTIVE DATE FILTERING                    *          
*   02/21/92   (BU ) --- SEND FLAG IN HEADING DESCRIPTIVE ELEMENT IF *          
*                        INVENTORY ITEM HAS BEEN PREVIOUSLY SENT     *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
TA0C0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C0A,RA,RR=R2                                                
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
         MVC   SAVRSTRT,RSTRTRTN   SAVE RESTART VALUE FOR DUMPS                 
         BAS   RE,PROCIN02         REINITIALIZE SOME WORKAREA                   
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
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
         B     PIN0004             SKIP ALTERNATE ENTRY POINT                   
*                                                                               
PROCIN02 NTR1                      ALTERNATE ENTRY POINT                        
         LA    R6,DBDEMOB          A(DEMO INTERFACE AREA)                       
         B     PIN0008             SKIP TO PARTIAL INITIALIZATION               
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
PIN0008  EQU   *                   START OF PARTIAL INITIALIZATION              
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
         DROP  R1                                                               
*                                                                               
*   GET DEMOUT/DEMAINT ADDRESSES FROM COMFACS LIST                              
*                                                                               
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
         LA    R2,ITRDPCDS                                                      
         CR    R1,R2               BASIC HEADER INFO?                           
         BNE   PR0004              NO                                           
         BAS   RE,BASHDINF         YES                                          
         B     PR0002                                                           
PR0004   EQU   *                                                                
         LA    R2,ITRDPCBK                                                      
         CR    R1,R2               BOOK INFO?                                   
         BNE   PR0006              NO                                           
         BAS   RE,BOOKIN           YES                                          
         B     PR0002                                                           
PR0006   EQU   *                                                                
         LA    R2,ITRDPCDC                                                      
         CR    R1,R2               DEMOGRAPHIC CODES?                           
         BNE   PR0008              NO                                           
         BAS   RE,DEMOCODE         YES                                          
         B     PR0002                                                           
PR0008   EQU   *                                                                
         LA    R2,ITRDPCDP                                                      
         CR    R1,R2               DAYPARTS?                                    
         BNE   PR0010              NO                                           
         BAS   RE,DAYPTCDS         YES                                          
         B     PR0002                                                           
PR0010   EQU   *                                                                
         LA    R2,ITRDPCUP                                                      
         CR    R1,R2               UPGRADE FORMULA?                             
         BNE   PR0012              NO                                           
         BAS   RE,UPGRD            YES                                          
         B     PR0002                                                           
PR0012   EQU   *                                                                
         LA    R2,ITEOD                                                         
         CR    R1,R2               END OF DATA?                                 
         BNE   PR0014              NO                                           
         B     PR0099 YES                                                       
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
         USING CT10IN01,R2                                                      
         MVC   TWSTAT,BHSTAT       STATION CALL LETTERS                         
         CLI   TWSTAT+4,C' '       ANY MEDIA ENTERED?                           
         BNE   BH0003              YES                                          
         MVI   TWSTAT+4,C'T'       NO  - INSERT TV                              
BH0003   EQU   *                                                                
         MVC   TWSERV,BHSERV       SERVICE                                      
*                                                                               
         CLC   BHSTDT(16),MYSPACES   ANY DATES ENTERED?                         
         BNE   BH0008                YES                                        
         GOTO1 DATCON,DMCB,(5,WORKAREA),(3,TWSTDT)                              
*                                    NO  - INSERT TODAY'S DATE                  
         MVC   TWENDT,=X'FFFFFF'     LOAD END DATE                              
         B     XIT                   EXIT ROUTINE                               
*                                                                               
BH0008   EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 DATVAL,DMCB,BHSTDT,WORKAREA                                      
         GOTO1 DATVAL,DMCB,BHENDT,WORKAREA+6                                    
*                                                                               
*  DATES HAVE BEEN VALIDATED AT PC.  NO ERROR CHECKING IS DONE HERE.            
*                                                                               
         GOTO1 DATCON,DMCB,WORKAREA,(2,TWSTDT)                                  
         GOTO1 DATCON,DMCB,WORKAREA+6,(2,TWENDT)                                
         PRINT NOGEN                                                            
         B     XIT                                                              
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
         USING CT10IN02,R2                                                      
*                                                                               
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         L     R3,DATALEN               L(INPUT ITEM DATA)                      
         LR    R4,R3               SAVE L(INPUT ITEM DATA)                      
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
         B     XIT                                                              
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
         B     XIT                 FINISHED                                     
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    PROCESSES EACH 5-BYTE PC INPUT FIELD, CONVERTING THE 3-BYTE                
*      EBCDIC REPRESENTATION OF THE DEMO # TO A 1-BYTE BINARY                   
*                                                                               
DEMOCODE NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT10IN03,R2                                                      
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
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    STORES THE DAYPARTS REQUESTED.  EACH DAYPART IS A SINGLE-BYTE              
*      FIELD.                                                                   
DAYPTCDS NTR1                                                                   
         L     R1,ADATA            A(ITEM)                                      
         USING CT10IN04,R1                                                      
*                                                                               
*  NOTE:  R1 REFERENCED IN 'USING' - WILL BE DESTROYED BY ANY                   
*        GOTO1 USED WITHIN THIS CODE!!!                                         
*                                                                               
         L     R2,DATALEN          L(INPUT ITEM DATA)                           
         BCTR  R2,0                DECREMENT BY 1 FOR EXEC                      
         LA    R3,TWDAYPTS                                                      
         EX    R2,DPC0080                                                       
         B     XIT                                                              
*                                                                               
DPC0080  MVC   0(0,R3),DRDAYPTS                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DEVELOPS THE ARGUMENTS FOR UPGRADE FORMULAS.                               
* FORMULAS HAVE BEEN VALIDATED AT THE PC, AND ARE SENT TO THE                   
*      MAINFRAME AS STRING DATA.  BECAUSE MOST OF THE DATA IS FIXED             
*      NO SEPARATORS ARE SENT.                                                  
* FORMULAS ARE OF THE FORMAT:                                                   
* FIELD 1:     BASE BOOK DATE     6 CHARS: MMM/YY                               
* FIELD 2:     TYPE UPGRADE       3 CHARS: PUT,HUT,RTG,SHR,NDX,HPT              
*                   OR                                                          
*              DEMO CODE          5 CHARS                                       
* FIELD 3:     UPGRADE BOOK       6 CHARS: MMM/YY                               
*                   OR                                                          
*              ABSOLUTE VALUE     3 CHARS  IF FIELD 2 IS TYPE UPGRADE           
*                                 VARIABLE IF FIELD 2 IS DEMO CODE              
* FIELD 4:     ABSOLUTE VALUE     VARIABLE                                      
*                                                                               
UPGRD    NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT10IN05,R2                                                      
*                                                                               
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    R5,UPGAREA1         SET A(UPGRADE STORAGE)                       
         CLI   UEUPGRD#,C'1'                                                    
         BE    UPG0004                                                          
         LA    R5,UPGAREA2                                                      
UPG0004  EQU   *                                                                
         USING UPGSTORE,R5                                                      
         MVC   WORKAREA(6),UEBASEBK PROCESS BASE BOOK                           
         GOTO1 DATVAL,DMCB,(2,WORKAREA),WORKAREA+6                              
         GOTO1 DATCON,DMCB,WORKAREA+6,(3,UBASEBK)                               
*                                                                               
*  INITIALIZE DEMO UPGRADE FIELD TO FOXES.  WILL ALWAYS BE INSERTED             
*    AS LAST DEMO ENTRY IN DEMO CODE ARRAY.  IF NOT USED, WILL BE               
*    REGARDED AS END OF CODES SENTINEL, OTHERWISE PICKED UP FOR                 
*    SKEWING REQUIREMENTS.                                                      
*                                                                               
         MVC   UPGDEMO(3),=X'FFFFFF'                                            
         LA    R4,UTYPTAB          CHECK UPGRADE TYPE                           
UPG0006  EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    UPG0020             YES - INPUT TYPE IS <DEMO>                   
         CLC   0(3,R4),UETYPE      TYPE FOUND?                                  
         BE    UPG0008             YES                                          
         LA    R4,LUTYPTAB(R4)     NO  - BUMP A(TABLE)                          
         B     UPG0006                                                          
UPG0008  EQU   *                                                                
         MVC   UPGTYPE,3(R4)       INSERT TYPE OF UPGRADE                       
*                                                                               
*  AFTER THE TYPE, NEXT FIELD MAY BE EITHER A DATE (MMM/YY) OR A                
*    THREE-CHARACTER ABSOLUTE VALUE FOR THE TYPE                                
*                                                                               
         CLI   UETYPDAT,C'Z'       1ST CHARACTER ALPHA (DATE MMM/YY)?           
         BH    UPG0012             > Z = NUMERIC (X'F0'-X'F9')                  
*                                                                               
*  FIELD IS DATE:  CONVERT AND STORE IT AS SUCH                                 
*                                                                               
         MVC   WORKAREA(6),UETYPDAT                                             
         GOTO1 DATVAL,DMCB,(2,WORKAREA),WORKAREA+6                              
         GOTO1 DATCON,DMCB,WORKAREA+6,(3,UMONABS)                               
*                                                                               
*  IS THERE ALSO A SHARE OPTION?  THIS ELEMENT CONTAINS:BASE BOOK (6)           
*    UPGRADE # (1) + UPGRADE BOOK (6) + TYPE (3) + TYPE MONTH (6) = 16          
*    CHARACTERS  +  OPT SHARE (VAR)                                             
*                                                                               
         L     R3,DATALEN          ANY MORE DATA IN ELEMENT?                    
         SH    R3,=H'16'           R3 = L(OPT SHARE)                            
         BZ    UPG0099             NO MORE DATA - FINISHED                      
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    R1,7                SET R3 FOR L(WORKAREA) ALSO                  
         SLA   R1,4                MOVE VALUE TO BITS 8-11                      
         BCTR  R3,0                DECREMENT FOR EXECUTE                        
         AR    R3,R1               ADD R1 TO R3                                 
         LA    R1,WORKAREA                                                      
         LA    R4,UETDTOPT                                                      
*                                                                               
         EX    R3,UPG0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R3,WORKAREA                                                      
         CLI   UPGTYPE,2           HPT UPGRADE?                                 
         BE    UPG0010             YES - DON'T DECIMAL ALIGN                    
         GOTO1 DECALIGN,DMCB,(R3),10   DECIMAL ALIGNMENT: X10                   
         L     R3,DMCB                                                          
UPG0010  EQU   *                                                                
         STCM  R3,7,UOPTSHR        STORE 3 BYTES                                
         B     UPG0099             FINISHED                                     
*                                                                               
*  AFTER THE TYPE FIELD, AN 'ABSOLUTE' VALUE FOR THE TYPE WAS ENTERED.          
*    THERE MAY ALSO BE AN OPTIONAL SHARE VALUE FOR THIS UPGRADE.                
*                                                                               
UPG0012  EQU   *                                                                
         MVI   UPGFLAG,1           SET FLAG TO 'ABSOLUTE'                       
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    R3,3                SET LENGTH OF ABSOLUTE VALUE                 
         LA    R1,7                SET R3 FOR L(WORKAREA) ALSO                  
         SLA   R1,4                MOVE VALUE TO BITS 8-11                      
         BCTR  R3,0                DECREMENT FOR EXECUTE                        
         AR    R3,R1               ADD R1 TO R3                                 
         LA    R1,WORKAREA                                                      
         LA    R4,UETYPABS                                                      
*                                                                               
         EX    R3,UPG0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R3,WORKAREA                                                      
         GOTO1 DECALIGN,DMCB,(R3),10   DECIMAL ALIGNMENT: X10                   
         L     R3,DMCB                                                          
         STCM  R3,7,UMONABS                                                     
*                                                                               
*  IS THERE ALSO A SHARE OPTION?  THIS ELEMENT CONTAINS                         
*    UPGRADE # (1) + BASE BOOK (6) + TYPE (3) + ABSOLUTE VALUE (3)              
*    =  13 CHARACTERS  + SHARE VALUE (VAR)                                      
*                                                                               
         L     R3,DATALEN          ANY MORE DATA IN ELEMENT?                    
         SH    R3,=H'13'                                                        
         BZ    UPG0099             NO MORE DATA - FINISHED                      
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    R1,7                SET R3 FOR L(WORKAREA) ALSO                  
         SLA   R1,4                MOVE VALUE TO BITS 8-11                      
         BCTR  R3,0                DECREMENT FOR EXECUTE                        
         AR    R3,R1               ADD R1 TO R3                                 
         LA    R1,WORKAREA                                                      
         LA    R4,UETABOPT         GET ABSOLUTE OPTIONAL SHARE                  
*                                                                               
         EX    R3,UPG0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R3,WORKAREA                                                      
         GOTO1 DECALIGN,DMCB,(R3),10   DECIMAL ALIGNMENT: X10                   
         L     R3,DMCB                                                          
         STCM  R3,7,UOPTSHR        STORE 3 BYTES                                
         B     UPG0099             FINISHED                                     
         SPACE 3                                                                
*                                                                               
*  A RECOGNIZED 'TYPE' WAS NOT FOUND.  THE FIELD MUST CONTAIN A                 
*    DEMO # (5-POSITION FIELD:  BYTE 1 = GEOGRAPHICAL QUALIFIER,                
*       BYTE 2 = QUALIFIER, BYTES 3=5 = EBCDIC EQUIV OF DEMO #).                
*    THE DEMO # WILL BE FOLLOWED BY A VARIABLE ABSOLUTE VALUE FIELD.            
*                                                                               
UPG0020  EQU   *                                                                
         LA    R4,UPGDEMO          A(UPGRADE AREA FIELD)                        
         LA    R3,UEDEMO           A(DEMO INPUT FIELD)                          
         CLI   0(R3),C'0'          GEOGRAPHICAL INDICATOR TRANSLATE             
         BNE   UPG0021             NOT NEEDED                                   
         MVI   0(R3),X'00'         INSERT BINARY ZERO                           
UPG0021  EQU   *                                                                
         MVC   0(1,R4),0(R3)       INSERT GEOGRAPHICAL INDICATOR                
         MVC   1(1,R4),1(R3)       INSERT DEMOGRAPHIC QUALIFIER                 
         PACK  WORKAREA(8),2(3,R3) CONVERT EBCDIC TO BINARY                     
         CVB   R6,WORKAREA                                                      
         STC   R6,2(R4)            INSERT DEMO CODE #                           
*                                                                               
*  CALCULATE LENGTH OF ABSOLUTE. ELEMENT CONTAINS                               
*    UPGRADE # (1) + BASE BOOK (6) + DEMO CODE (5)                              
*    = 12 CHARACTERS  +  ABSOLUTE VALUE.                                        
*                                                                               
         L     R3,DATALEN                                                       
         SH    R3,=H'12'                                                        
         BNZ   UPG0022             MUST BE DATA                                 
         DC    H'0'                                                             
UPG0022  EQU   *                                                                
         LA    RE,WORKAREA              A(WORKAREA)                             
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    R1,7                SET R3 FOR L(WORKAREA) ALSO                  
         SLA   R1,4                MOVE VALUE TO BITS 8-11                      
         BCTR  R3,0                DECREMENT FOR EXECUTE                        
         AR    R3,R1               ADD R1 TO R3                                 
         LA    R1,WORKAREA                                                      
         LA    R4,UEDEMABS                                                      
*                                                                               
         EX    R3,UPG0080          PACK VALUE BY LENGTH                         
*                                                                               
         CVB   R3,WORKAREA                                                      
         GOTO1 DECALIGN,DMCB,(R3),10   DECIMAL ALIGNMENT: X10                   
         L     R3,DMCB                                                          
         STCM  R3,7,UMONABS        STORE 3 BYTES                                
         B     UPG0099             FINISHED                                     
*                                                                               
UPG0080  PACK  0(0,R1),0(0,R4)                                                  
*                                                                               
UPG0099  EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         SPACE 4                                                                
UTYPTAB  EQU   *                                                                
         DC    C'PUT',X'1'         USES SHARES FROM BASE BOOK                   
LUTYPTAB EQU   *-UTYPTAB                                                        
         DC    C'HPT',X'2'         USES SHARES FROM BASE BOOK                   
         DC    C'HUT',X'3'                                                      
         DC    C'RTG',X'4'                                                      
         DC    C'SHR',X'5'                                                      
         DC    C'NDX',X'6'                                                      
         DC    X'FF'                                                            
         DS    0H                                                               
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
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),RSTRTMST  DISK ADDRESS OF MASTER RECORD                
         MVC   FULL,AIO            SAVE CURRENT A(IO AREA)                      
         L     R1,AIO2             SET IO AREA 2 FOR READ                       
         ST    R1,AIO                                                           
         GOTO1 GETREC              READ MASTER REC FOR RESTART                  
*                                                                               
         L     R1,AIO1             SET IO AREA 1 FOR READ                       
         ST    R1,AIO                                                           
         XC    KEY,KEY                                                          
         CLI   RSTRTRTN,7          IO TIMEOUT RESTART?                          
         BE    RSET0008            YES                                          
         MVC   KEY+28(4),RSTRTDET  DISK ADDRESS OF DETAIL RECORD                
         GOTO1 GETREC              READ DETAIL REC FOR RESTART                  
         MVC   KEY(27),0(R1)       LOAD KEY                                     
RSET0008 EQU   *                                                                
         MVC   AIO(4),FULL         RESET CURRENT A(IO AREA)                     
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWDEMO          SAVE IT BACK                                 
         L     RF,ATWDAYPT         RESET ADDRESSES FOR RESTART                  
         AR    RF,R9               ADD COVERING REGISTER                        
         ST    RF,ATWDAYPT         SAVE IT BACK                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*   ACCESSES THE PASSIVE KEYS (X'92') FOR INVENTORY HEADERS.  FOR               
*     EACH FOUND, A PROCESSING ROUTINE TO HANDLE INVENTORY TRACKS IS            
*     CALLED.                                                                   
*   EACH DAYPART REQUESTED IS REVIEWED.                                         
*                                                                               
PROCDATA NTR1                                                                   
         MVI   SCRNDATA,C'N'       SET 'NO DATA ON SCREEN'                      
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,0          ANY RESTART VALUE?                           
         BE    PROD0004            NO                                           
         CLI   RSTRTRTN,7          RESTART FOR IO COUNT REASONS?                
         BNE   PROD0024            NO  - SKIP TO RUNINV ROUTINE                 
         MVC   KEY(27),KEY92SAV    YES - RESET LAST KEY                         
         MVI   RSTRTRTN,0          RESET RESTART FLAG                           
         B     PROD0012                                                         
*                                                                               
*  ESTABLISH INITIAL ADDRESSES WITHIN ARRAYS                                    
*                                                                               
PROD0004 EQU   *                                                                
         LA    R1,TWBOOK           A(1ST BOOK)                                  
         ST    R1,ATWBOOK          A(BOOK IN PROCESS)                           
         LA    R1,TWDEMOS          A(1ST DEMO)                                  
         ST    R1,ATWDEMO          A(DEMO IN PROCESS)                           
         LA    R1,TWDAYPTS         A(1ST DAYPART)                               
         ST    R1,ATWDAYPT         A(DAYPART IN PROCESS)                        
*                                                                               
PROD0008 EQU   *                                                                
         XC    KEY,KEY             ESTABLISH PASSIVE DAYPART KEY                
         MVI   KEY,X'92'                                                        
         MVC   KEY+3(2),SIGNON2C   INSERT REP CODE                              
         MVC   KEY+5(5),TWSTAT     INSERT STATION + MEDIA                       
         L     R1,ATWDAYPT         INSERT DAYPART IN PROCESS                    
         MVC   KEY+10(1),0(R1)                                                  
PROD0012 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     PROD0020            CHECK KEY                                    
PROD0016 EQU   *                                                                
         GOTO1 SEQ                 RETURN NEXT PASSIVE KEY                      
PROD0020 EQU   *                                                                
         CLC   KEY(11),KEYSAVE     SAME THROUGH DAYPART?                        
         BNE   PROD0032            NO  - BUMP KEY                               
*                                                                               
*  POSSIBLE FILTER HERE FOR DAYPARTS V,S,J: EFFECTIVE DATE VS                   
*   FLIGHT DATE                                                                 
*                                                                               
         MVC   KEY92SAV(27),KEY    SAVE PASSIVE KEY FOR SEQ PROCESS             
         GOTO1 GETFACT,DMCB,0      GET CURRENT IO COUNT                         
         L     R1,0(R1)            IO'S LEFT?                                   
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)                                     
         BH    PROD0024            IO'S AVAILABLE - PROCESS                     
         MVI   RSTRTRTN,7          NO  - SEND SCREEN WITH WHATEVER              
*                                  DATA IS LOADED                               
         CLI   SCRNDATA,C'Y'       DATA ON SCREEN?                              
         BE    PROD0044            YES - SEND IT                                
         MVC   ELTAREA,=C'IO TIMEOUT'                                           
         LA    R2,ITRDMFDM         INSERT DUMMY ITEM TYPE                       
         GOTO1 PUTITEM,DMCB,(R2),10,ELTAREA                                     
         B     PROD0044            WRAP UP AND RESTART                          
PROD0024 EQU   *                   RESTART ENTRY POINT                          
         CLI   RSTRTRTN,6          RESTART AT 'EOD' SET?                        
         BE    PROD0036            YES - PUT OUT EOD AND END                    
         CLI   RSTRTRTN,0          ANY OTHER RESTART SET?                       
         BNE   PROD0028            YES - DON'T RESET BOOK                       
         LA    R1,TWBOOK           A(1ST BOOK):  RESET FOR EACH MASTER          
         ST    R1,ATWBOOK          A(BOOK IN PROCESS)                           
PROD0028 EQU   *                                                                
         BAS   RE,RUNINV           PROCESS THIS INVENTORY NUMBER                
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    PROD0044            YES - WRAP UP AND RESTART                    
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEY92SAV    RESET KEY                                    
         GOTO1 READ                RE-ESTABLISH KEY SEQUENCE                    
         B     PROD0016                                                         
PROD0032 EQU   *                                                                
         L     R1,ATWDAYPT         BUMP A(DAYPART IN PROCESS)                   
         LA    R1,1(R1)            INCREMENT 1 POSITION                         
         ST    R1,ATWDAYPT         SAVE IT BACK                                 
         CLI   0(R1),X'00'         ANY VALUE?                                   
         BNE   PROD0008            GO BACK FOR NEXT                             
*                                                                               
*  ALL DAYPARTS PROCESSED, NO RESTART NEEDED.  SEND END-OF-DATA ELEMENT         
*      WITH FRAME, SET LAST-FRAME INDICATOR                                     
PROD0036 EQU   *                                                                
         MVI   RSTRTRTN,X'0'       INITIALIZE RESTART FLAG                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   PROD0040            ELEMENT FITS - PROCEED                       
         MVI   RSTRTRTN,6          SET RESTART TO 'EOD NEEDED'                  
         B     XIT                 END FOR RESTART                              
PROD0040 EQU   *                                                                
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     PROD0048            EXIT                                         
PROD0044 EQU   *                                                                
         L     RF,ATWBOOK          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWBOOK          SAVE IT BACK                                 
         L     RF,ATWDEMO          RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDEMO          SAVE IT BACK                                 
         L     RF,ATWDAYPT         RESET ADDRESSES FOR RESTART                  
         SR    RF,R9               SUBTRACT COVERING REGISTER                   
         ST    RF,ATWDAYPT         SAVE IT BACK                                 
PROD0048 EQU   *                                                                
         B     XIT                                                              
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
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,X'0'       ANY RESTART VALUE?                           
         BE    RUIN0004            NO  - SKIP TO REGULAR PROCESS                
*                                                                               
* RESET FOR RESTART                                                             
*                                                                               
         L     R2,AIO1             RESET ORIGINAL IO AREA                       
         ST    R2,AIO              FOR TRACK READING                            
         L     R3,ATWBOOK          A(BOOK IN PROCESS)                           
*                                                                               
         CLI   RSTRTRTN,X'1'       RESTART AT PULLDEMS?                         
         BE    RUIN0024            YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,X'2'       RESTART AT PULLRATS?                         
         BE    RUIN0044            YES - GO TO 'PULLRATS' RTN                   
         CLI   RSTRTRTN,X'3'       RESTART AT UPGRADE?                          
         BE    RUIN0028            YES - GO TO 'UPGRADE' RTN                    
         CLI   RSTRTRTN,X'4'       RESTART AT SENDDESC FROM PULLDEMS?           
         BE    RUIN0024            YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,X'5'       RESTART AT SENDDESC FROM UPGRADE?            
         BE    RUIN0028            YES - GO TO 'UPGRADE' RTN                    
*                                                                               
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
RUIN0004 EQU   *                                                                
         MVI   INVFOUND,C'N'       SET 'NO INVENTORY FOUND' FLAG                
         MVI   RPTFLAG,C'N'        SET REPEAT FLAG TO 'NO'                      
         L     R2,AIO2             USE SECOND AREA TO STORE                     
         ST    R2,AIO              INVENTORY HEADER/MASTER                      
         USING RINVRECD,R2                                                      
*                                                                               
         MVC   RSTRTMST(4),KEY+28  SAVE DISK ADDR FOR RESTART                   
         GOTO1 GETREC              GET HEADER REC WITH PASSIVE KEY              
*                                                                               
*   CHECK MASTER EFFECTIVE DATE AGAINST FLIGHT DATES:  SKIP IF ANY OF           
*     THESE CONDITIONS APPLY:                                                   
*        1.  FLIGHT END   DATE  < INVENTORY EFFECTIVE START DATE                
*        2.  FLIGHT START DATE  > INVENTORY EFFECTIVE END   DATE                
*                                                                               
         CLC   TWENDT,RINVPEFF     FLIGHT END VS EFFECT START DATE              
         BL    RUIN0048            FLIGHT ENDS EARLIER - SKIP IT                
         OC    RINVPEFF+2(2),RINVPEFF+2   ANY END DATE FOR INVENTORY?           
         BZ    RUIN0008            NO END DATE - ACCEPTED                       
         CLC   TWSTDT,RINVPEFF+2   FLIGHT START VS EFFECT END DATE              
         BH    RUIN0048            LATER   - SKIP IT                            
*                                                                               
RUIN0008 EQU   *                                                                
         GOTO1 SCANDPTS,DMCB,(R2)                                               
         XC    EXTRAKEY,EXTRAKEY                                                
         MVC   EXTRAKEY(27),RINVKEY  SET KEY FROM HEADER RECORD                 
         MVI   DEMOFLAG,C'N'       SET FLAG FOR MASTER DATA                     
*                                                                               
         DROP  R2                                                               
*                                                                               
         L     R2,AIO1             RESET ORIGINAL IO AREA                       
         ST    R2,AIO              FOR TRACK READING                            
         L     R3,ATWBOOK          A(BOOK IN PROCESS)                           
RUIN0012 EQU   *                                                                
         LA    RE,SVCLST           A(CONVERSION TABLE)                          
RUIN0016 EQU   *                                                                
         CLC   =X'0000',0(R3)      END OF BOOKS?                                
         BE    RUIN0040            YES                                          
         CLI   1(R3),50            BOOK YEAR = 50?                              
         BE    RUIN0028            YES - THIS IS PJ1 UPGRADE                    
         CLI   1(R3),51            BOOK YEAR = 51?                              
         BE    RUIN0028            YES - THIS IS PJ2 UPGRADE                    
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
         XC    KEY,KEY                                                          
         MVC   KEY(27),EXTRAKEY    SET KEY                                      
         MVC   KEY+24(1),2(RE)     INSERT SRCE FROM CONV TABLE                  
         MVC   KEY+25(2),1(R3)     INSERT BOOK FROM BOOK ARRAY                  
         GOTO1 HIGH                FIND THE RECORD                              
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   RUIN0032            NO  - BUMP THROUGH BOOK ARRAY                
RUIN0024 EQU   *                                                                
         BAS   RE,PULLDEMS         YES - TAKE DEMOS FROM TRACK                  
         MVI   INVFOUND,C'Y'       SET 'INVENTORY FOUND' FLAG                   
         B     RUIN0032                                                         
RUIN0028 EQU   *                                                                
         GOTO1 UPGRADE,DMCB,(R3)                                                
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
RUIN0044 EQU   *                                                                
         BAS   RE,PULLRATS         YES - PULL RATIONALE FOR INV #               
         B     XIT                                                              
RUIN0048 EQU   *                                                                
         L     R2,AIO1             RESET ORIGINAL IO AREA                       
         ST    R2,AIO              FOR TRACK READING                            
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DETERMINES WHETHER AN INVENTORY MASTER HAS BEEN PREVIOUSLY                 
*    SENT IN A PASS THROUGH AN EARLIER DAYPART                                  
*                                                                               
SCANDPTS NTR1                                                                   
*                                                                               
         L     R3,0(R1)            A(MASTER RECORD)                             
         USING RINVRECD,R3                                                      
*                                                                               
         L     RE,ATWDAYPT         A(DAYPART IN PROGRESS)                       
         LA    RF,TWDAYPTS         A(DAYPARTS REQUESTED)                        
         CR    RE,RF               1ST DAYPART IN PROGRESS?                     
         BE    SDPT0020            YES - EXIT ROUTINE                           
         SR    RE,RF               NO  - CALCULATE # DPTS ALREADY DONE          
         LA    RF,6                LOOP CTR: DPTS IN MASTER RECORD              
         LA    R1,TWDAYPTS         A(DAYPARTS REQUESTED - AGAIN)                
         LA    R2,RINVDP           A(DAYPARTS IN RECORD)                        
SDPT0004 EQU   *                                                                
         CLC   0(1,R1),0(R2)       DAYPT REQ'ED IN REC'S DAYPARTS?              
         BE    SDPT0012            YES - SET FLAG YES AND EXIT                  
         LA    R2,1(R2)            NO  - NEXT REC DAYPART                       
         BCT   RF,SDPT0004         GO BACK FOR NEXT                             
         LA    R1,1(R1)            NEXT REQUESTED DAYPART                       
         LA    R2,RINVDP           RESET TO 1ST REC DAYPART                     
         LA    RF,6                RESET LOOP CTR: DPTS IN MAST REC             
         BCT   RE,SDPT0004         DO IT AGAIN                                  
         B     SDPT0020            NO HITS - EXIT                               
SDPT0012 EQU   *                                                                
         MVI   RPTFLAG,C'Y'        SET REPEAT FLAG TO YES                       
SDPT0020 EQU   *                                                                
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
         CLI   RSTRTRTN,X'0'       ANY RESTART VALUE?                           
         BE    PUD0002             NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,X'1'       RESTART AT PULLDEMS?                         
         BE    PUD0018             YES - GO TO 'PULLDEMS' RTN                   
         CLI   RSTRTRTN,X'4'       RESTART AT SENDDESC FROM PULLDEMS?           
         BE    PUD0004             YES - GO TO 'SENDDESC' RTN                   
*                                                                               
         DC    H'0'                                                             
*                                                                               
PUD0002  EQU   *                                                                
         MVC   RSTRTDET(4),KEY+28  SAVE DISK ADDR FOR RESTART                   
         GOTO1 GETREC              RETRIEVE DEMOGRAPHIC TRACK                   
         LA    R6,DBDEMOB          A(DBLOCK)                                    
         USING DEMOD,R6                                                         
         MVC   DBFILE,=C'INV'      SET TO INVENTORY PROCESS                     
         L     RE,DBAREC           RESET A(1ST ELEMENT IN RECORD)               
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
         BAS   RE,DRAWDEMS         PROCESS THE INVENTORY RECORD                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   DEMOFLAG,C'Y'       DESCRIPTIVE DATA SENT?                       
         BE    PUD0006                                                          
         MVI   DEMOFLAG,C'Y'       SET TO 'SENT'                                
         MVI   RSTRTDES,4          SET 'SENDDESC FROM PULLDEMS' FLAG            
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
         MVI   RSTRTRTN,X'0'       INITIALIZE RESTART FLAG                      
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
         MVI   RSTRTRTN,1          SET RESTART AS 'PULLDEMS'                    
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     PUD0099                                                          
PUD0080  MVC   1(0,R3),0(R2)                                                    
*                                                                               
PUD0099  EQU   *                                                                
         B     XIT                                                              
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
         CLI   RSTRTRTN,X'0'       ANY RESTART VALUE?                           
         BE    PUR0001             NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,X'2'       RESTART AT PULLRATS?                         
         BE    PUR0026             YES - GO TO 'PULLRATS' RTN                   
*                                                                               
         DC    H'0'                                                             
*                                                                               
PUR0001  EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),EXTRAKEY    RESET KEY FOR RATIONALE                      
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
         MVI   RSTRTRAT,0          INITIALIZE LINE COUNTER                      
         GOTO1 GETELEM,DMCB,1      GET FIRST TEXT ELEMENT, IF ANY               
         B     PUR0025                                                          
PUR0024  EQU   *                                                                
         GOTO1 NEXTELEM,DMCB       GET NEXT TEXT ELEMENT, IF ANY                
PUR0025  EQU   *                                                                
         BNE   PUR0004             NO MORE - GET NEXT RAT'L RECORD              
         ZIC   R5,RSTRTRAT         INCREMENT COUNTER                            
         LA    R5,1(R5)                                                         
         STC   R5,RSTRTRAT         PUT IT BACK                                  
         XC    ELTAREA,ELTAREA                                                  
         L     R5,AIO                                                           
         PRINT GEN                                                              
         USING RINVRECD,R5                                                      
         EDIT  (2,RINVKTXT),(2,RIRATL#),FILL=0                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         EDIT  (1,RINVTLIN),(2,RIRATLIN),FILL=0                                 
         ZIC   R5,RINVTLEN         L(TEXT ENTRY) + 6                            
         PRINT NOGEN                                                            
         SH    R5,=H'6'            SUBTRACT L(CONTROL STUFF)                    
         BZ    PUR0024             NO LENGTH = NO OUTPUT                        
         LA    R4,RIRATL           A(TEXT IN ITEM)                              
*                                                                               
         EX    R5,PUR0080          MOVE TEXT BY LENGTH                          
*                                                                               
PUR0026  EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    PUR0028             NO                                           
         GOTO1 READ                REESTABLISH READ FOR SEQ PROCESS             
         L     R6,AIO1             RESET A(RATIONALE RECORD)                    
         BAS   RE,RESETRAT         POSITION TO PROPER LINE                      
*        CLC   17(3,R6),=X'3CF7F3' **TEST**                                     
*        BNE   XXXX0000            **TEST**                                     
*        DC    H'0'                **TEST**                                     
XXXX0000 EQU   *                                                                
         L     R6,DUB              SET A(NEW ELEMENT)                           
         MVI   RSTRTRTN,X'0'       INITIALIZE RESTART FLAG                      
         L     R5,RSTRTLEN         RESET L(NEW ELEMENT)                         
         LA    R2,ELTAREA          RESET R2 TO BUILD NEW ELEMENT                
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
         MVI   RSTRTRTN,2          SET RESTART AS 'PULLRATS'                    
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
*    UTILIZES THE UPGRADE FORMULA PREVIOUSLY 'DECODED' TO DEVELOP               
*        INFORMATION FOR PROJECTION1 OR PROJECTION2                             
*    P1   =  A(IN BOOK ARRAY) WHERE YEAR = 50 MEANS PROJECTION1                 
*                                   YEAR = 51 MEANS PROJECTION2                 
*                                                                               
UPGRADE  NTR1                                                                   
*                                                                               
*  CHECK RESTART:  IF RSTRTRTN NOT = ZERO, SKIP TO INDICATED ROUTINE            
*                                                                               
         CLI   RSTRTRTN,X'0'       ANY RESTART VALUE?                           
         BE    UGR0001             NO  - SKIP TO REGULAR PROCESS                
         CLI   RSTRTRTN,X'3'       RESTART AT UPGRADE?                          
         BE    UGR0054             YES - GO TO 'UPGRADE' RTN                    
         CLI   RSTRTRTN,X'5'       RESTART AT SENDDESC IN UPGRADE?              
         BE    UGR0042             YES - GO TO 'SENDDESC' IN UPGRD              
*                                                                               
         DC    H'0'                                                             
*                                                                               
UGR0001  EQU   *                                                                
*                                                                               
         LA    R2,UPGAREA1         A(PJ1)                                       
         L     R1,0(R1)            LOAD A(UPGRADE BOOK)                         
         MVC   PJNAME(3),=C'PJ1'   SET LITERAL                                  
         CLI   1(R1),50            IS UPGRADE FOR PJ1?                          
         BE    UGR0002             YES                                          
         LA    R2,UPGAREA2         NO  - A(PJ2)                                 
         MVC   PJNAME(3),=C'PJ2'   SET LITERAL                                  
UGR0002  EQU   *                                                                
         USING UPGSTORE,R2                                                      
*                                                                               
         LA    R6,DBDEMOB          A(DEMO BLOCK)                                
         USING DEMOD,R6                                                         
*                                                                               
         MVC   DBFILE,=C'TP '      SET BASIC TP CALL STRUCTURE                  
         MVI   DBSELMED,C'T'       SET TO TV                                    
         MVC   DBSELBK,UBASEBK     INSERT BASE BOOK DATE                        
         MVC   DBSELSRC,TWSERV     INSERT SERVICE                               
         MVC   DBSELSTA,TWSTAT     INSERT STATION                               
         MVI   DBTPTT,C'P'         ASK FOR TP PURE                              
         MVI   DBFUNCT,DBGETDEM                                                 
         L     R3,AIO2             ACCESS MASTER DAY/TIME                       
         USING RINVRECD,R3                                                      
*                                                                               
         MVC   DBSELDAY,RINVPDAY   INSERT DAY FROM MASTER                       
         MVC   DBSELTIM,RINVPTIM   INSERT TIME STRING FROM MASTER               
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                                                               
*  INSERT DEMO CODE FOR DEMO VALUE SKEW INTO DEMO CODE ARRAY.  IF THIS          
*    UPGRADE HAS BEEN CHOSEN, THIS WILL PULL THE BASE VALUE FOR THE             
*    DEMOGRAPHIC CODE, TO PERMIT CALCULATING THE SKEW.  IF THIS UPGRADE         
*    HAS NOT BEEN CHOSEN, THE FIELD WILL CONTAIN FOXES, WHICH WILL              
*    APPEAR TO BE THE ARRAY SENTINEL.                                           
*                                                                               
         ZIC   R5,TWTOTDEM         TOTAL NUMBER OF DEMOS                        
         SR    R4,R4               MULTIPLY BY FIELD SIZE (3 BYTES)             
         M     R4,=F'3'              (ARRAY IS ZERO RELATIVE)                   
         LA    R4,TWDEMOS          A(DEMO CODE ARRAY)                           
         AR    R4,R5               A(NEXT OPEN ENTRY IN ARRAY)                  
         MVC   0(3,R4),UPGDEMO     INSERT DEMO UPGRADE CODE                     
         ST    R4,STORE16+12       SAVE ADDRESS FOR RESET                       
         LA    RE,WORKAREA         INITIALIZE ENTIRE WORKAREA                   
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         L     RE,AADDIO           INITIALIZE INTERIM DEMO STORAGE              
         LA    RF,1000                                                          
         XCEF  (RE),(RF)                                                        
*                                                                               
*  RETRIEVE BASE BOOK FIGURES FROM TP DATA FOR ALL UPGRADES                     
*                                                                               
*                                                                               
         GOTO1 DEMNDRTN,DMCB,WORKAREA+400                                       
*                                                                               
         L     R4,STORE16+12       RESET DEMO ARRAY                             
         MVC   0(3,R4),=X'FFFFFF'                                               
         CLI   UPGTYPE,0           DEMO UPGRADE?                                
         BE    UGR0034             YES - UPGRADE VIA DEMO SKEW                  
         CLI   UPGTYPE,3           PUT OR HPT UPGRADE?                          
         BNL   UGR0020             NO                                           
*                                                                               
*  RETRIEVE UPGRADE BOOK DEMOS/SHARES/LEVELS FOR PUT/HPT UPGRADE                
*                                                                               
         MVC   DBSELBK,UMONABS     INSERT UPGRADE BOOK                          
         L     RE,AADDIO           INITIALIZE INTERIM DEMO STORAGE              
         LA    RF,1000                                                          
         XCEF  (RE),(RF)                                                        
         GOTO1 DEMNDRTN,DMCB,WORKAREA                                           
         CLI   UPGTYPE,2           HPT UPGRADE?                                 
         BNE   UGR0004             NO  - PUT UPGRADE                            
         PRINT GEN                                                              
         ZICM  R4,UOPTSHR,3        YES - INSERT INDEX VALUE                     
         GOTO1 NDXDEMOS,DMCB,WORKAREA,(R4)                                      
         PRINT NOGEN                                                            
UGR0004  EQU   *                                                                
         GOTO1 LVLXSHR,DMCB,WORKAREA,WORKAREA+400                               
         B     UGR0038             FORMAT RETURN FRAME                          
*                                                                               
UGR0020  EQU   *                                                                
*                                                                               
*  OF RTG, SHR, HUT, OR INDEX, ONLY HUT CAN ENTER A BOOK!                       
*                                                                               
         CLI   UPGFLAG,0           DOES UPGRADE HAVE A MONTH?                   
         BNE   UGR0024             NO  - HAS ABSOLUTE                           
         MVC   DBSELBK,UMONABS     YES - INSERT UPGRADE BOOK                    
         L     RE,AADDIO           INITIALIZE INTERIM DEMO STORAGE              
         LA    RF,1000                                                          
         XCEF  (RE),(RF)                                                        
         GOTO1 DEMNDRTN,DMCB,WORKAREA                                           
         XC    NEWRATG(16),NEWRATG ZERO OUT REPLACEMENT VALUES                  
UGR0024  EQU   *                                                                
         CLI   UPGTYPE,3           HUT?                                         
         BNE   UGR0026             NO                                           
         GOTO1 CALCHUT,DMCB,(R2)   YES                                          
         B     UGR0038             FORMAT RETURN FRAME                          
UGR0026  EQU   *                                                                
         CLI   UPGTYPE,4           RTG?                                         
         BNE   UGR0028             NO                                           
         GOTO1 CALCRTG,DMCB,(R2)   YES                                          
         B     UGR0038             FORMAT RETURN FRAME                          
UGR0028  EQU   *                                                                
         CLI   UPGTYPE,5           SHARE?                                       
         BNE   UGR0030             NO                                           
         GOTO1 CALCSHR,DMCB,(R2)   YES                                          
         B     UGR0038             FORMAT RETURN FRAME                          
UGR0030  EQU   *                                                                
         ZICM  R4,UMONABS,3        INSERT INDEX VALUE                           
         GOTO1 NDXDEMOS,DMCB,WORKAREA+400,(R4)                                  
*                                                                               
*   MOVE INDEXED VALUES TO COMMON (FINAL) WORK SPACE                            
*                                                                               
         MVC   WORKAREA(200),WORKAREA+400                                       
         B     UGR0038             FORMAT RETURN FRAME                          
*                                                                               
*   DEMOGRAPHIC SKEW:  VALUE OF A PARTICULAR DEMO FROM THE BASE                 
*     BOOK IS APPLIED AGAINST A USER-SUPPLIED VALUE FOR THAT DEMO,              
*     AND OTHER DEMOS ARE SKEWED AGAINST THE RESULTING PROPORTION               
*                                                                               
UGR0034  EQU   *                   DEMOGRAPHIC SKEW                             
         GOTO1 CALCDEMO,DMCB,(R2)                                               
         B     UGR0040             FORMAT RETURN FRAME                          
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
UGR0038  EQU   *                                                                
*                                                                               
         GOTO1 NEWVALUE,DMCB,WORKAREA                                           
*                                                                               
UGR0040  EQU   *                                                                
         CLI   DEMOFLAG,C'Y'       DESCRIPTIVE DATA SENT?                       
         BE    UGR0044                                                          
         MVI   DEMOFLAG,C'Y'       SET TO 'SENT'                                
         MVI   RSTRTDES,5          SET 'SENDDESC FROM UPGRADE' FLAG             
UGR0042  EQU   *                   SENDDESC RESTART ENTRY POINT                 
         BAS   RE,SENDDESC         SEND DESCRIPTIVE DATA                        
         CLI   RSTRTEND,1          'END AND RESTART' FLAG SET?                  
         BE    UGR0099             YES - END AND RESTART                        
UGR0044  EQU   *                                                                
         MVI   DEMODATA,C'N'       TURN OFF COMBO DATA FLAG                     
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         L     R6,AIO2             PROCESS HEADER IN IO AREA 2                  
         USING RINVRECD,R6                                                      
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO03,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         MVC   DVCOMBO,DEMODATA    SET COMBO/NO COMBO FLAG                      
         MVC   DVBOOK(3),PJNAME    INSERT PJ1/PJ2 LITERAL                       
         MVC   DVBOOK+3(5),MYSPACES SPACE FILL REMAINDER                        
         EDIT  TWTOTDEM,(2,DVDEMOCT),FILL=0                                     
         LA    R3,DVDEMLEN         A(DEMOS WITHIN ITEM)                         
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         LA    R4,WORKAREA         A(DEMOS: 4-BYTE ENTRIES)                     
         ZIC   R5,TWTOTDEM         # OF DEMOS REQUESTED                         
UGR0046  EQU   *                                                                
         GOTO1 HEXOUT,DMCB,(R4),HEXOUTWK,4,=C'TOG'                              
         LA    R2,HEXOUTWK         A(HEXOUT DEMO VALUE)                         
         LA    R6,8                # OF CHARACTERS TO CHECK                     
UGR0048  EQU   *                                                                
         TM    0(R2),X'0F'         DECL BITS ON IN BYTE?                        
         BM    UGR0050             YES - SIGNIFICANT POSITION                   
         LA    R2,1(R2)            NO  - CHECK NEXT POSITION                    
         BCT   R6,UGR0048                                                       
         MVI   0(R3),C'0'          NO SIGNIFICANT POSITIONS                     
         B     UGR0052                                                          
UGR0050  EQU   *                                                                
         EDIT  (R6),(1,(R3))       INSERT 1-CHAR LENGTH COUNT                   
         EX    R6,UGR0080          MOVE VALUE BY LENGTH                         
UGR0052  EQU   *                                                                
         LA    R6,1(R6)            BUMP A(O/P) BY LEN ATTR + LENGTH             
         AR    R3,R6                                                            
         LA    R4,4(R4)            NEXT DEMO VALUE                              
         BCT   R5,UGR0046          DO EACH                                      
*                                                                               
UGR0054  EQU   *                                                                
         CLI   RSTRTRTN,0          ANY RESTART?                                 
         BE    UGR0056             NO                                           
         MVI   RSTRTRTN,X'0'       INITIALIZE RESTART FLAG                      
         L     R3,RSTRTLEN         RESET L(NEW ELEMENT)                         
         B     UGR0058                                                          
UGR0056  EQU   *                                                                
         LA    R4,ELTAREA          CALCULATE LENGTH OF NEW ITEM                 
         SR    R3,R4               R3=NEXT DEMO VALUE                           
         ST    R3,RSTRTLEN         SAVE LENGTH FOR POSS. RESTART                
UGR0058  EQU   *                                                                
         LA    R2,ITRDMFDV         INSERT ITEM TYPE                             
         GOTO1 PUTITEM,DMCB,(R2),(R3),ELTAREA                                   
         MVI   SCRNDATA,C'Y'       SET 'DATA ON SCREEN'                         
         CLI   EOFFLAG,C'Y'        END OF FRAME REACHED?                        
         BNE   UGR0099             ITEM FITS - EXIT                             
*                                                                               
         MVI   RSTRTRTN,3          SET RESTART AS 'UPGRADE'                     
         MVI   RSTRTEND,1          SET 'END AND RESTART' FLAG                   
*                                                                               
         B     UGR0099                                                          
*                                                                               
UGR0080  MVC   1(0,R3),0(R2)                                                    
*                                                                               
UGR0099  EQU   *                                                                
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
         MVC   STORE16(4),0(R1)       SAVE P1 (A(FINAL DEMO VALUES))            
*                                                                               
         GOTO1 DEMAND,DMCB,DBDEMOB,DRAWDEMS                                     
*                                                                               
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHFCTR+2(2),DBDIVSOR   SET TOTAL WEIGHT                         
*                                                                               
         GOTO1 DEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC                
*                                                                               
         MVC   STORE16+4(4),DBAREC     SAVE ORIGINAL VALUES                     
         MVC   STORE16+8(4),DBAQUART                                            
         MVC   DBAREC,AINTEREC         SET A(INTERIM RECORD)                    
         L     RE,AINTEREC             SET A(1ST ELEMENT)                       
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVC   DMCB+8(4),STORE16       LOAD A(FINAL OUTPUT)                     
*                                                                               
         PRINT GEN                                                              
         GOTO1 DEMOUT,DMCB,(C'L',TWDEMOS),DBDEMOB                               
         PRINT NOGEN                                                            
*                                                                               
         MVC   DBAREC,STORE16+4        RESET ORIGINAL VALUES                    
         MVC   DBAQUART,STORE16+8                                               
DMND0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CONVERTS A RETRIEVED INVENTORY RECORD TO STANDARD FORMAT, THEN             
*        DRAWS DEMOGRAPHICS FROM IT, LEAVING THEM IN 'WORKAREA',                
*    OR                                                                         
*    SERVES AS A HOOK TO DEMAND ROUTINE, RETRIEVES DEMO RECORDS,                
*        WEIGHTS AND ACCUMULATES THEM IN AN INTERIM WORK AREA.                  
*                                                                               
DRAWDEMS NTR1                                                                   
         LA    R6,DBDEMOB          A(DEMO DBLOCK)                               
         USING DEMOD,R6                                                         
         XC    WORKAREA(200),WORKAREA                                           
*                                                                               
         CLC   DBFILE,=C'INV'      INVENTORY CALL?                              
         BE    DRDM0010            YES                                          
*                                                                               
         LA    R1,DBLOCK                                                        
         ST    R1,MTHCFACS                                                      
         LH    R1,DBFACTOR         WEIGHT VALUE                                 
         ST    R1,MTHFCTR                                                       
         MVC   MTHIFIL,DBFILE      FILE FORMAT INPUT                            
         MVC   MTHOFIL,DBFILE      FILE FORMAT OUTPUT                           
         MVI   MTHOSRC,C'N'        FORCE 'NSI' AS SOURCE                        
*                                                                               
*   CONVERT RETRIEVED RECORD (DBAREC) TO STANDARD FORMAT                        
*                                                                               
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
         B     DRDM0016            PROCESS DEMAND CALL                          
*                                                                               
DRDM0010 EQU   *                                                                
         GOTO1 DEMOUT,DMCB,(C'L',TWDEMOS),DBDEMOB,WORKAREA                      
         B     DRDM0099            EXIT - NOTHING IS RESET                      
*                                                                               
DRDM0016 EQU   *                                                                
*                                                                               
*  ACCUMULATE REBUILT RECORD FROM IUNWORK INTO A(INTEREC)                       
*                                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
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
*    CALCULATES DEMOGRAPHIC VALUES X AN INDEX AMOUNT.  THE FIRST                
*        3 DEMOS IN WORKAREA ARE SKIPPED (RTG, HUT, SHARE).                     
*        P1  =  A(INPUT DEMO VALUES)                                            
*        P2  =  INDEX VALUE                                                     
*                                                                               
NDXDEMOS NTR1                                                                   
         L     R2,0(R1)            A(INPUT DEMO VALUES)                         
         LA    R2,12(R2)           SKIP RTG, SHR,HUT                            
         L     R3,4(R1)            SET INDEX VALUE                              
         ZIC   R1,TWTOTDEM         COUNT OF TOTAL DEMOS                         
         SH    R1,=H'3'            SUBTRACT COUNT FOR RTG,SHR,HUT               
         LA    R6,100              DEC'L ROUNDING VALUE                         
NDX0002  EQU   *                                                                
         SR    R4,R4                                                            
         L     R5,0(R2)            INSERT DEMO VALUE                            
         MR    R4,R3               MULTIPLY DEMO VALUE BY INDEX                 
         A     R5,=F'50'           HALF-ROUND FOR DIVISION                      
         DR    R4,R6               DIVISION BY 100                              
         ST    R5,0(R2)            RESTORE NEW INDEXED VALUE                    
         LA    R2,4(R2)            BUMP A(DEMO VALUE)                           
         BCT   R1,NDX0002                                                       
*                                                                               
*  ON AN INDEX, DO RTG/SHR/HUT GET INDEXED.  IF NOT, NEED SCAN OF               
*    DEMO ARRAY FOR TEST                                                        
*                                                                               
NDX0099  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    MULTIPLIES LEVELS BY SHARES, PLACING FINAL VALUES IN WORKAREA.             
*        ROUTINE INTERROGATES DEMO LIST TO DETERMINE WHERE SHARES               
*        AND LEVELS BEGIN IN VARIOUS STRINGS.                                   
* P1  =  A(DEMO VALUES FROM UPGRADE BOOK)                                       
* P2  =  A(DEMO VALUES FROM BASE BOOK)                                          
*                                                                               
LVLXSHR  NTR1                                                                   
         L     R2,0(R1)            A(UPGRADE BOOK DEMO VALUE)                   
         L     R1,4(R1)            A(BASEBOOK DEMO VALUES)                      
         LA    R2,12(R2)           PASS SHARE,LEVEL,HUT OF UPGRADE BK           
         LR    R7,R2               A(DEMOS IN UPGRADE BOOK)                     
*                                                                               
*  NOTE:  R7 IS A MAD COVERING REGISTER USED LOCALLY                            
*                                                                               
         LA    R1,12(R1)           PASS SHARE,LEVEL,HUT OF BASE BK              
         ZIC   R6,TW#DEMOS         INSERT # OF REQUESTED DEMOS                  
         SLL   R6,2                MULT BY FIELD SIZE (4 BYTES)                 
         AR    R1,R6               BASE BK: POINT TO SHARES OF DEMOS            
         ST    R1,DUB              A(SHARES IN BASE BOOK): SAVE                 
         AR    R2,R6               UPGRADE BOOK: A(SHARES OF DEMO)              
         ST    R2,DUB+4            A(SHARES IN UPGRADE BOOK): SAVE              
         AR    R2,R6               UPGRADE BK: A(LEVELS OF DEMOS)               
         SRL   R6,2                RESET COUNT FOR LOOP                         
LXS0002  EQU   *                                                                
         SR    R4,R4                                                            
         L     R5,0(R2)            INSERT LEVEL DEMO VALUE                      
         L     R3,0(R1)            INSERT SHARE DEMO VALUE                      
         MR    R4,R3               MULT SHARE X LEVEL                           
         A     R5,=F'500'          HALF-ROUND FOR DIVIDING                      
         LA    R3,1000             DEC'L ROUNDING VALUE                         
         DR    R4,R3               DIVIDE BY 1000                               
         ST    R5,0(R7)            INSERT NEW SHR X LEVEL VALUE                 
         LA    R2,4(R2)            BUMP A(UPGRADE BOOK)                         
         LA    R1,4(R1)            BUMP A(BASE BOOK)                            
         LA    R7,4(R7)            BUMP A(NEW FIGURE)                           
         BCT   R6,LXS0002                                                       
*                                                                               
*  MOVE SHARES FROM BASE BOOK TO SHARES OF UPGRADE BOOK FOR FINAL               
*      OUTPUT                                                                   
*                                                                               
         L     R1,DUB              A(SHARES IN BASE BOOK)                       
         L     R2,DUB+4            A(SHARES IN UPGRADE BOOK)                    
         ZIC   R3,TW#DEMOS         # OF DEMOS REQUESTED                         
         SLL   R3,2                MULT BY SIZE OF FIELD (4 BYTES)              
*                                                                               
         EX    R3,LXS0080          MOVE BY LENGTH                               
*                                                                               
         B     LXS0099                                                          
*                                                                               
LXS0080  MVC   0(0,R2),0(R1)                                                    
*                                                                               
LXS0099  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DOES A HUT OR HUT/SHARE SKEW, CALCULATING A NEW RATING.                    
*    CHECKS TO DETERMINE IF HUT FOR SKEW CAME FROM A BOOK OR AN                 
*        ABSOLUTE VALUE.                                                        
*    P1            =  A(UPGRADE STORAGE AREA 1 OR 2)                            
*    WORKAREA      =  HUT BOOK, IF MONTH ENTERED                                
*    WORKAREA+400  =  BASE BOOK                                                 
*                                                                               
CALCHUT  NTR1                                                                   
         L     R6,0(R1)            A(UPGRADE STORAGE AREA)                      
         USING UPGSTORE,R6                                                      
*                                                                               
         ZIC   R5,UMONABS,3        SET HUT UPGRAD ABSOLUTE VALUE                
         CLI   UPGFLAG,0           DOES UPGRADE HAVE MONTH?                     
         BNE   CHUT0002            NO  - HAS ABSOLUTE                           
         L     R5,WORKAREA+8       YES - SET HUT FROM BOOK                      
CHUT0002 EQU   *                                                                
         ST    R5,NEWHUT           SAVE FOR REPLACEMENT                         
         SR    R4,R4                                                            
         ZICM  R3,UOPTSHR,3        HUT/SHARE CALCULATION?                       
         ST    R3,NEWSHARE         SAVE FOR REPLACEMENT                         
         LTR   R3,R3                                                            
         BNZ   CHUT0004            YES - SHARE ENTERED                          
         L     R3,WORKAREA+404     NO  - SET ORIG SHR FROM BASE BK              
CHUT0004 EQU   *                                                                
*                                                                               
*  CALCULATE A NEW RATING BY: (((SHARE*HUT)+500)/1000) WHERE                    
*     1000 IS THE DECIMAL ALIGNMENT FACTOR, AND 500 IS THE                      
*     HALF-ADD FOR ROUNDING FACTOR                                              
*                                                                               
         MR    R4,R3               SHARE*HUT                                    
         A     R5,=F'500'          HALF-ADD FOR ROUNDING                        
         D     R4,=F'1000'         DIVIDE FOR ALIGNMENT                         
         ST    R5,NEWRATG          SAVE FOR REPLACEMENT                         
*                                                                               
         GOTO1 CALCSKEW,DMCB,(R5)                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
CHUT0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DOES A RTG SKEW.                                                           
*                                                                               
CALCRTG  NTR1                                                                   
         L     R6,0(R1)            A(UPGRADE STORAGE AREA)                      
         USING UPGSTORE,R6                                                      
*                                                                               
         ZICM  R5,UOPTSHR,3        SAVE FOR LATER USE                           
         ST    R5,DUB              POSSIBLE SHARE VALUE                         
         ZICM  R5,UMONABS,3        SET RTG UPGRAD ABSOLUTE VALUE                
         ST    R5,NEWRATG          SAVE FOR REPLACEMENT                         
*                                                                               
         GOTO1 CALCSKEW,DMCB,(R5)                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
*  IF SHARE ALSO ENTERED, REQUIRES CALCULATION OF A NEW HUT FIGURE              
*                                                                               
         L     R3,DUB              GET POSSIBLE SHARE VALUE                     
         LTR   R3,R3               ANY VALUE?                                   
         BZ    CRTG0099            NO  - FINISHED                               
         ST    R3,NEWSHARE         SAVE FOR REPLACEMENT                         
         L     R5,NEWRATG          GET RTG VALUE                                
         SR    R4,R4                                                            
         M     R4,=F'1000'         MULTIPLY BY 1000                             
         SRL   R3,1                1/2 SHARE VALUE                              
         AR    R5,R3               HALF-ADD FOR ROUNDING                        
         L     R3,DUB              RESET SHARE VALUE                            
         DR    R4,R3               DIVIDE BY SHARE                              
         ST    R5,NEWHUT           SAVE FOR REPLACEMENT                         
*                                                                               
CRTG0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DOES A SHARE SKEW.                                                         
*                                                                               
CALCSHR  NTR1                                                                   
         L     R6,0(R1)            A(UPGRADE STORAGE AREA)                      
         USING UPGSTORE,R6                                                      
*                                                                               
         ZICM  R5,UMONABS,3        SET SHR UPGRAD ABSOLUTE VALUE                
         ST    R5,NEWSHARE         SAVE FOR REPLACEMENT                         
         SR    R4,R4                                                            
         L     R3,WORKAREA+408     SET ORIG HUT FROM BASE BK                    
*                                                                               
*  CALCULATE A NEW RATING BY: (((SHARE*HUT)+500)/1000)                          
*                                                                               
         MR    R4,R3               SHARE*HUT                                    
         A     R5,=F'500'          HALF-ADD FOR ROUNDING                        
         D     R4,=F'1000'         DIVIDE FOR ALIGNMENT                         
         ST    R5,NEWRATG          SAVE FOR REPLACEMENT                         
*                                                                               
         GOTO1 CALCSKEW,DMCB,(R5)                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
CSHR0099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CALCULATES THE SKEW FACTOR AND APPLIES IT TO THE DEMOS IN                  
*      THE ARRAY                                                                
*                                                                               
CALCSKEW NTR1                                                                   
         L     R5,0(R1)            INSERT BASIS FOR SKEW FACTOR                 
         SR    R4,R4               REINITIALIZE                                 
         M     R4,=F'1000'         PROVIDE ANSWER TO 2 DEC'L PLACES             
         L     R3,WORKAREA+400     SET ORIGINAL RTG                             
         SRL   R3,1                DIV ORIG RTG BY 2 FOR ROUNDING               
         AR    R5,R3               HALF-ADD FOR ROUNDING                        
         L     R3,WORKAREA+400     RESET ORIG RTG FROM BASE BK                  
         SR    R4,R4               REINITIALIZE                                 
         DR    R4,R3               NEW RTG/ORIG RTG = SKEW FACTOR               
         LR    R3,R5               SET SKEW FACTOR IN R3                        
         LA    R1,WORKAREA+400     A(DEMOS TO SKEW)                             
         LA    R4,TWDEMOS          A(DEMO CODES)                                
         ST    R4,FULL                                                          
         ZIC   R6,TWTOTDEM         DEMO COUNT                                   
CSKE0006 EQU   *                                                                
         L     R4,FULL             A(DEMO CODE)                                 
         CLI   2(R4),3             HOMES/METROA/METROB?                         
         BNH   CSKE0008            YES - DON'T SKEW IT                          
         SR    R4,R4                                                            
         L     R5,0(R1)            LOAD DEMO TO SKEW                            
         MR    R4,R3               MULT BY SKEW FACTOR                          
         A     R5,=F'500'          HALF-ADD FOR ROUNDING                        
         D     R4,=F'1000'         DIVIDE BY 1000                               
         ST    R5,0(R1)            STORE NEW DEMO                               
CSKE0008 EQU   *                                                                
         LA    R1,4(R1)            BUMP A(OLD DEMO)                             
         L     R4,FULL             BUMP A(DEMO CODES)                           
         LA    R4,3(R4)                                                         
         ST    R4,FULL                                                          
         BCT   R6,CSKE0006                                                      
CSKE0099 EQU   *                                                                
         MVC   WORKAREA(200),WORKAREA+400                                       
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    DOES A DEMO VALUE SKEW                                                     
*                                                                               
CALCDEMO NTR1                                                                   
         L     R6,0(R1)            A(UPGRADE STORAGE AREA)                      
         USING UPGSTORE,R6                                                      
*                                                                               
         MVC   DUB(3),UPGDEMO      SAVE SKEW DEMO                               
*                                                                               
         ZICM  R5,UMONABS,3        SET DEMO UPGRADE ABSOLUTE VALUE              
         ST    R5,NEWDEMO          SAVE FOR REPLACEMENT                         
         SR    R4,R4                                                            
         M     R4,=F'1000'         NEW DEMO * 1000 (3 DECL PLACE)               
*                                                                               
*   THE SKEW DEMO HAS BEEN ADDED TO THE DEMO ARRAY AS 'TOTAL NUMBER             
*    OF DEMOS + 1'.  AS THE DEMO #S ARE RETURNED IN A ZERO-RELATIVE             
*    AREA, THE FOLLOWING CODE WILL POINT TO THE SKEW DEMO BUCKET.               
*                                                                               
         ZIC   R3,TWTOTDEM         TOTAL NUMBER OF DEMOS                        
         SLL   R3,2                MULTIPLY BY FIELD SIZE (4 BYTES)             
         LA    R2,WORKAREA+400     A(DEMO VALUE ARRAY)                          
         AR    R2,R3               A(DEMO UPGRADE VALUE: BASE BOOK)             
*                                                                               
         L     R3,0(R2)            SET ORIGINAL DEMO VALUE                      
         SRL   R3,1                DIV ORIG VALUE BY 2 FOR ROUNDING             
         AR    R5,R3               HALF-ADD FOR ROUNDING                        
         L     R3,0(R2)            RESET ORIG DEMO VALUE FROM BASE BK           
         DR    R4,R3               NEW VALUE/ORIG VALUE = SKEW FACTOR           
         LR    R3,R5               SET SKEW FACTOR IN R3                        
         LA    R1,WORKAREA+400     A(DEMOS TO SKEW)                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R4,TWDEMOS          A(DEMO CODES)                                
         ST    R4,FULL                                                          
         ZIC   R6,TWTOTDEM         DEMO COUNT                                   
CDEM0006 EQU   *                                                                
         L     R4,FULL             A(DEMO CODE)                                 
         CLI   2(R4),3             HOMES/METROA/METROB?                         
         BNH   CDEM0008            YES - DON'T SKEW IT                          
         CLC   DUB(3),0(R4)        SKEW DEMO?                                   
         BNE   CDEM0007            NO                                           
         MVC   0(4,R2),NEWDEMO     YES - INSERT NEW DEMO VALUE                  
         B     CDEM0008                                                         
CDEM0007 EQU   *                                                                
         SR    R4,R4                                                            
         L     R5,0(R1)            LOAD DEMO TO SKEW                            
         MR    R4,R3               MULT BY SKEW FACTOR                          
         A     R5,=F'500'          HALF-ADD FOR ROUNDING                        
         D     R4,=F'1000'         DIVIDE BY 1000                               
         ST    R5,0(R1)            STORE NEW DEMO                               
CDEM0008 EQU   *                                                                
         LA    R1,4(R1)            BUMP A(OLD DEMO)                             
         LA    R2,4(R2)            BUMP A(NEW DEMO)                             
         L     R4,FULL             BUMP A(DEMO CODES)                           
         LA    R4,3(R4)                                                         
         ST    R4,FULL                                                          
         BCT   R6,CDEM0006                                                      
*                                                                               
CDEM0099 EQU   *                                                                
         MVC   WORKAREA(200),WORKAREA+400                                       
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
         CLI   RSTRTRTN,X'0'       ANY RESTART VALUE?                           
         BNE   SDE0006             YES - SKIP TO INSERT ELEMENT                 
*                                                                               
         XC    ELTAREA,ELTAREA     INITIALIZE ELEMENT BUILD AREA                
         L     R6,AIO2             PROCESS HEADER IN IO AREA 2                  
         USING RINVRECD,R6                                                      
         LA    R2,ELTAREA          DEFINE ELEMENT IN PROCESS                    
         USING CT1XO01,R2          OUTPUT ELEMENT DSECT                         
*                                                                               
         L     R3,ATWDAYPT         INSERT DAYPART IN PROCESS                    
         MVC   DHDAYPT,0(R3)                                                    
         MVC   DHSTAT(5),RINVKSTA  INSERT STATION                               
         MVI   DHCOMPET,C'N'                                                    
*                                                                               
*  NOTE:  TEMPORARILY (UNTIL THIS MECHANISM IS DEFINED) ALL ELEMENTS            
*        ARE RETURNED AS COMPETITIVE=N                                          
*                                                                               
*  CONSTRUCT EBCDIC REPRESENTATION OF INVENTORY #                               
*                                                                               
         EDIT  (1,RINVKQTR),(2,DHINV#),FILL=0                                   
         MVC   DHINV#+2(2),RINVKDAY    INSERT DAY + LENGTH                      
         MVC   DHREPEAT,RPTFLAG        INSERT REPEAT FLAG VALUE                 
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
         MVI   RSTRTRTN,X'0'       INITIALIZE RESTART FLAG                      
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
*  THIS ROUTINE:                                                                
*     SCANS THE DEMOGRAPHIC VALUES DEVELOPED BY THE UPGRADE, AND, IF            
*       RATING, SHARE, OR HUT HAVE BEEN REDEVELOPED, INSERTS THE NEW            
*       VALUE(S).                                                               
*  P1  =  A(DEMO VALUES)                                                        
*                                                                               
NEWVALUE NTR1                                                                   
         L     R2,0(R1)            A(DEMO VALUE ARRAY)                          
         ZIC   R6,TWTOTDEM         DEMO COUNT                                   
         LA    R5,TWDEMOS          A(DEMO CODES)                                
NVAL0002 EQU   *                                                                
         CLI   2(R5),1             HOMES (RATING,HUT,SHARE)?                    
         BNE   NVAL0020            NO  - SKIP IT                                
         CLI   1(R5),C'R'          RATING?                                      
         BNE   NVAL0003            NO                                           
         L     R3,NEWRATG          YES - ANY NEW RATING?                        
         LTR   R3,R3                                                            
         BZ    NVAL0020            NO                                           
         ST    R3,0(R2)            YES - INSERT INTO DEMO VALUE ARRAY           
         B     NVAL0020                                                         
NVAL0003 EQU   *                                                                
         CLI   1(R5),C'S'          SHARE?                                       
         BNE   NVAL0004            NO                                           
         L     R3,NEWSHARE         YES - ANY NEW SHARE?                         
         LTR   R3,R3                                                            
         BZ    NVAL0020            NO                                           
         ST    R3,0(R2)            YES - INSERT INTO DEMO VALUE ARRAY           
         B     NVAL0020                                                         
NVAL0004 EQU   *                                                                
         CLI   1(R5),C'P'          HUT?                                         
         BNE   NVAL0020            NO                                           
         L     R3,NEWHUT           YES - ANY NEW HUT?                           
         LTR   R3,R3                                                            
         BZ    NVAL0020            NO                                           
         ST    R3,0(R2)            YES - INSERT INTO DEMO VALUE ARRAY           
NVAL0020 EQU   *                                                                
         LA    R2,4(R2)            BUMP A(DEMO VALUE ARRAY)                     
         LA    R5,3(R5)            BUMP A(DEMO CODES)                           
         BCT   R6,NVAL0002                                                      
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
* CT10DI01:  BASIC HEADLINE INFORMATION                                         
       ++INCLUDE CT10DI01                                                       
         SPACE 4                                                                
* CT10DI02:  BOOK LIST                                                          
         SPACE 4                                                                
       ++INCLUDE CT10DI02                                                       
         SPACE 4                                                                
* CT10DI03:  DEMOGRAPHICS REQUESTED                                             
         SPACE 4                                                                
       ++INCLUDE CT10DI03                                                       
         SPACE 4                                                                
* CT10DI04:  DAYPARTS REQUESTED                                                 
         SPACE 4                                                                
       ++INCLUDE CT10DI04                                                       
         SPACE 4                                                                
* CT10DI05:  UPGRADE EXPRESSION                                                 
         SPACE 4                                                                
       ++INCLUDE CT10DI05                                                       
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
* DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                  
*                                                                               
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
TWBOOK   DS    9CL3                EIGHT BOOK ENTRIES + DELIMITER               
TWDEMOS  DS    34CL3               DEMO CODES + DELIMITER                       
LTWDEMOS EQU   *-TWDEMOS                                                        
TW#DEMOS DS    CL1                 DEMO COUNT REQUESTED                         
TWTOTDEM DS    CL1                 TOTAL # DEMOS                                
TWDAYPTS DS    8CL1                EIGHT DAYPART CODES                          
ATWBOOK  DS    A                   A(BOOK IN PROGRESS)                          
ATWDEMO  DS    A                   A(DEMO IN PROGRESS)                          
ATWDAYPT DS    A                   A(DAYPART IN PROGRESS)                       
MAXIOCTR DS    H                   90% OF MAXIMUM IO'S                          
KEY92SAV DS    CL27                SAVE AREA:X'92' PASSIVE D/P KEY              
DEMODATA DS    CL1                 FLAG: COMBO/NON-COMBO (Y/N)                  
DEMOFLAG DS    CL1                 FLAG: SEND MASTER DATA ONLY ONCE             
INVFOUND DS    CL1                 FLAG: INVENTORY FOUND/NOT FOUND              
PJNAME   DS    CL3                 PJ1/PJ2 LITERAL                              
TESTDUMP DS    CL1                 COUNTER FOR SERIAL DUMPS                     
EXTRAKEY DS    CL48                EXTRA KEY STORAGE                            
UPGAREA1 DS    CL20                UPGRADE # 1 STORAGE                          
UPGAREA2 DS    CL20                UPGRADE # 2 STORAGE                          
         DS    0D                  ALIGNMENT                                    
WORKAREA DS    CL600               WORK SPACE FOR DEMO CALCS, ETC               
HEXOUTWK DS    CL8                 WORK SPACE FOR HEXOUT                        
WEIGHT   DS    CL4                 WEIGHTING FOR DEMOS                          
ELTAREA  DS    CL160               ELEMENT BUILD AREA                           
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
RSTRTRTN DS    CL1                 FLAG TO ROUTINE TERMINATING                  
*                                  1  =  PULLDEMS                               
*                                  2  =  PULLRATS                               
*                                  3  =  UPGRADE                                
*                                  4  =  SENDDESC FROM PULLDEMS                 
*                                  5  =  SENDDESC FROM UPGRADE                  
*                                  6  =  END OF DATA ELEMENT                    
*                                  7  =  IO COUNT RESTART                       
RSTRTDES DS    CL1                 RESTART SENDDESC ORIGINATING RTN             
*                                  4  =  SENDDESC FROM PULLDEMS                 
*                                  5  =  SENDDESC FROM UPGRADE                  
RSTRTRAT DS    CL1                 RESTART LINE W/IN RATIONALE                  
RSTRTEND DS    CL1                 NON-0 = END JOB WITH RESTART                 
SAVRSTRT DS    CL1                 SAVE AREA FOR RESTART VALUE                  
SCRNDATA DS    CL1                 DATA ON SCREEN:                              
*                                  Y  =  YES                                    
*                                  N  =  NO                                     
RPTFLAG  DS    CL1                 INVENTORY ITEM SENT > 1                      
*                                  Y  =  YES                                    
*                                  N  =  NO                                     
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
         EJECT                                                                  
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
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTMAD0A   05/01/02'                                      
         END                                                                    
