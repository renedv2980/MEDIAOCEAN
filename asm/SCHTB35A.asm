*          DATA SET SCHTB35A   AT LEVEL 034 AS OF 05/01/02                      
*          DATA SET NEBUY35A   AT LEVEL 014 AS OF 04/06/00                      
*PHASE T31135A,+0                                                               
*INCLUDE NETINTG                                                                
         SPACE 2                                                                
********************************************************************            
* GENERAL UNIT EDIT/RECORD MAINTENANCE MODULE                      *            
*                                                                  *            
* P1   BYTE 0    = COMMAND (I, E, N, X, S, D, F, P)                *            
*      BYTES 1-3 = A(UNBLOCK)                                      *            
* P2   BYTE 0    = C'O' TO GENERATE OLD POINTER (COMMAND 'P')      *            
*                  SET UNOVALS WHEN INITIALIZING UNBLOCK           *            
*      BYTES 1-3 = A(OUTPUT AREA - COMMAND 'P')                    *            
*                                                                  *            
* ON ENTRY MODULE ASSUMES PROGRAM AND PACKAGE RECORDS HAVE BEEN    *            
* READ, AND THAT ON CHANGE, THE NETBLOCK POINTS TO AND DESCRIBES   *            
* THE UNIT RECORD TO BE MAINTAINED.  ON ADD, SKELETON KEY MUST     *            
* BE BUILT BY CALLER IN CLEARED I/O AREA.                          *            
*                                                                  *            
* TO USE MODULE, FOR EACH UNIT RECORD CLEAR AND INITIALIZE THE     *            
* UNBLOCK, THEN CALL EDIT MODULE IN FOLLOWING SEQUENCE -           *            
*      I - INITIALIZES FOR EDIT                                    *            
*      E, X, OR N - EDITS SCREEN FIELDS AND SET UNIT VALUES        *            
*      S - CHECKS SYNTAX ON INPUT EXTRF RATEBY FVAL TO FLDH/FLD    *            
*      D - GENERATES ESTIMATED DEMO VALUES                         *            
*      F - WRAP UP PROCESSING ON UNIT RECORD                       *            
*      P - GENERATES AND MAINTAINS DIRECTORY POINTERS              *            
********************************************************************            
         TITLE 'GENERAL UNIT EDIT/RECORD MAINTENANCE MODULE - T31135'           
T31135   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UNED**,RA,RR=RE                                              
         LA    RC,2048(RA)         RC=THIRD BASE REGISTER                       
         LA    RC,2048(RC)                                                      
         USING T31135+8192,RC                                                   
         L     R8,0(R1)                                                         
         LA    R8,0(R8)            R8 POINTS TO UNIT BLOCK                      
         USING UNBLOCKD,R8                                                      
         L     R9,UNAGLOB          R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         LA    R6,NEBLOCKA         R6 COVERS NETBLOCK                           
         USING NEBLOCKD,R6                                                      
         L     R7,UNALOCAL         R7 POINTS TO LOCAL WORKING STORAGE           
         USING TEMPD,R7                                                         
         ST    R1,MYPARM                                                        
         ST    RE,MYRELO                                                        
         ST    RD,MYAWORK          SAVE RD FOR DIRECT EXIT                      
         MVC   COMMAND,0(R1)       EXTRACT COMMAND VALUE                        
         L     R5,ABUYVALS         R5 COVERS BUY VALUES                         
         USING BUYVALD,R5                                                       
         L     R4,UNAREC           R4 POINTS TO UNIT RECORD                     
         USING NURECD,R4                                                        
*                                                                               
         LA    R2,COMMANDS         COUNTER                                      
         LA    R1,COMMTAB          POINT TO TABLE                               
         CLC   COMMAND,0(R1)                                                    
         BE    *+14                                                             
         LA    R1,L'COMMTAB(R1)                                                 
         BCT   R2,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         ICM   RF,7,1(R1)          ROUTINE ADDRESS                              
         LA    RF,0(RE,RF)         RELOCATE BRANCH ADDRESS                      
         BASR  RE,RF                                                            
         SPACE 1                                                                
UNX      XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZATION CODE (COMMAND = C'I')                                          
*                                                                               
IN       ST    RE,SAVEREG                                                       
         MVI   UNERROR,0                                                        
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         ZIC   R1,ACTION                                                        
         STC   R1,UNACTION         SAVE ACTION NUMBER IN EDIT BLOCK             
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MVI   UNACTSW,C'A'        SET FOR ACTION ADD                           
         CH    R0,=H'1'            TEST FOR REMAINDER OF 1                      
         BE    *+8                                                              
         MVI   UNACTSW,C'C'        NO-ITS A CHANGE                              
INA      XC    UNDEMS,UNDEMS       CLEAR ESTIMATED DEMOS                        
         CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BNE   *+16                                                             
         MVC   UNSHR(4),NBESTSHR   SET INPUT VALUE = RECORD VALUES              
         MVC   UNRAT,NBESTHOM+2    SINCE ANY OF 3 MAY BE INPUT                  
         XC    UNAMISS,UNAMISS     AND MISSING RECORD ADCON                     
         XC    UNDATE,UNDATE                                                    
         MVI   UNSQH,0                                                          
         TM    UNBITOPT,X'20'                                                   
         BO    *+12                                                             
         CLI   UNACTION,COPYU                                                   
         BE    IN6                                                              
         CLI   UNACTSW,C'C'                                                     
         BNE   IN2                                                              
         MVC   UNDATE,NUKDATE      FOR CHANGE, SET KEY DATE/SQH                 
         MVC   UNSQH,NUKTIME                                                    
         MVI   ELCODE,X'B3'                                                     
***      MVI   ELCODE,X'B1'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    UNBITOPT,X'40'                                                   
         MVI   ELCODE,X'B4'                                                     
***      MVI   ELCODE,X'B2'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    UNBITOPT,X'40'                                                   
         LA    R1,NUMAINEL-NUKEY(R4) R1 POINTS TO FIRST ELEMENT                 
         SR    R0,R0                                                            
         MVI   UNBILLSW,NO         INITIALIZE BILLED/PAID SWITCHES              
         MVI   UNPAYSW,NO                                                       
         SPACE 1                                                                
* UNBILLED BILLING ELEMENTS IGNORED - UNIT WITH ONLY UNBILLED                   
* ELEMENTS IS TREATED AS THOUGH IT WERE NEVER BILLED.                           
*                                                                               
IN1      CLI   0(R1),0             TEST FOR END-OF-RECORD                       
         BE    IN2                                                              
         CLI   0(R1),X'10'         TEST FOR BILLING ELEMENT                     
         BNE   *+16                                                             
         USING NUBILD,R1                                                        
         TM    NUBILST,X'20'       TEST FOR UNBILLED ELEMENT                    
         BO    *+8                 YES-IGNORE IT                                
         MVI   UNBILLSW,YES                                                     
         CLI   0(R1),X'12'         TEST FOR PAYING ELEMENT                      
         BNE   *+8                                                              
         MVI   UNPAYSW,YES                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0               NEXT ELEMENT                                 
         B     IN1                                                              
         DROP  R1                                                               
         SPACE 1                                                                
IN2      CLI   UNACTION,COPYU                                                   
         BE    IN6                                                              
         MVC   NUPACK,PACK         PACKAGE NUMBER                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUPACKST,NPAKSTAT   PACKAGE STATUS                               
         CLI   UNACTION,B          BUYING SINGLE UNIT                           
         BE    *+12                                                             
         CLI   UNACTION,BM         BUYING MULTIPLE UNIT                         
         BNE   *+10                                                             
         MVC   NUPOSTDT,NPAKPDT    PACKAGE METER                                
         L     RF,APROGEL                                                       
         TM    NPGSTAT-NPGELEM(RF),X'80'                                        
         BO    *+10                                                             
         MVC   NUHUTPCT,NPAKHPCT   PACKAGE HUT ADJ                              
         MVC   NUHUTTYP,NPAKHTYP   PACKAGE HUT TYP                              
         NI    NUACTWHY,X'03'      ZERO ALL BITS EXCEPT CONVERSION/COPY         
         MVC   NUMARKET,NETMARK                                                 
         MVC   NUALPHA,AGYALPH                                                  
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   IN4                                                              
         OI    NUACTWHY,X'80'      NEW BUY                                      
         CLI   BUYUNFR,C'Y'        SEE IF UNFROZEN OVERRIDE SET                 
         BE    *+16                                                             
         TM    NUPACKST,X'80'      TEST FOR FROZEN PACKAGE                      
         BZ    *+8                                                              
         OI    NUUNITST,X'04'      SET FOR PFB                                  
         TM    NPAKHUTL,X'80'      TEST FOR HUTS FROM DEMO FILE                 
         BZ    *+8                 NO                                           
         OI    NUUNST2,X'80'                                                    
         TM    NPAKHUTL,X'40'      TEST FOR 52 WEEK HUT CALENDAR                
         BZ    *+8                                                              
         OI    NUUNST2,X'40'                                                    
         MVI   NUMAINEL,X'01'                                                   
         MVI   NUMAINLN,80                                                      
         ZIC   R1,NUMAINLN                                                      
         LA    R1,NUMAINEL-NUKEY+1(R1) SET INITIAL RECORD LENGTH                
         STH   R1,NURLEN                                                        
         SPACE                                                                  
IN4      L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   NUPRFILT,NPGFILT    PROGRAM FILTERS                              
         SPACE                                                                  
IN6      CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   IN8                                                              
         MVI   ELCODE,X'08'                                                     
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(ELCODE,(R4)),(1,=C'K')              
         ICM   R3,15,APAKFEL                                                    
         BZ    *+8                                                              
         BAS   RE,PUTEL                                                         
*                                                                               
         MVI   ELCODE,X'09'                                                     
         BAS   RE,DELEL                                                         
         ICM   R3,15,APAKAEL                                                    
         BZ    IN8                                                              
         BAS   RE,PUTEL                                                         
         SPACE 1                                                                
IN8      MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   IN10                                                             
         L     RE,12(R1)                                                        
         MVI   5(RE),0             CLEAR THE LAST ACTIVE BYTE                   
         CLC   NUKDATE,=XL2'B125'  DONT TAG IF UNIT BEFORE SEP5/88              
         BL    *+22                                                             
         MVC   6(1,RE),NBSTSTAT    LOAD IN STATION TYPE                         
         MVC   7(1,RE),NBSUBMED    LOAD IN SUB-MEDIA                            
         MVC   12(1,RE),NBSTPSTT   LOAD IN POST-TYPE                            
         CLI   UNACTION,B                                                       
         BE    *+12                                                             
         CLI   UNACTION,BM                                                      
         BNE   IN15                                                             
         L     RF,APACKREC                                                      
         USING NPRECD,RF                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE SET                           
         BZ    IN15                NO, DON'T SET STATUS                         
         OI    17(RE),X'20'        SET STATUS TO NEW                            
         B     IN15                                                             
         DROP  RF                                                               
*                                                                               
IN10     XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVC   0(2,R3),=X'0214'                                                 
         CLC   NUKDATE,=XL2'B125'  DONT TAG IF UNIT BEFORE SEP5/88              
         BL    *+22                                                             
         MVC   6(1,R3),NBSTSTAT    LOAD IN STATION TYPE                         
         MVC   7(1,R3),NBSUBMED    LOAD IN SUB-MEDIA                            
         MVC   12(1,R3),NBPOSTYP   LOAD IN POST-TYPE                            
         CLI   UNACTION,B          ACTION BUY                                   
         BE    *+12                                                             
         CLI   UNACTION,BM         ACTION BUY MULTIPLE                          
         BNE   IN11                                                             
         L     RF,APACKREC                                                      
         USING NPRECD,RF                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE SET                           
         BZ    IN11                NO, DON'T SET STATUS                         
         OI    17(R3),X'20'        SET STATUS TO NEW                            
IN11     BAS   RE,PUTEL                                                         
         DROP  RF                                                               
         SPACE                                                                  
*--SET UP DEFAULT 21 ELEMENT FOR TRAFFIC (FOR BUY MULTIPLE)                     
IN15     CLI   UNACTION,BM                                                      
         BNE   IN20                                                             
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    IN20                                                             
* ON NEW BUYS ADD DEFAULT 21 ELEMENT                                            
*                                                                               
         XC    IOA(80),IOA                                                      
         LA    R3,IOA                                                           
         USING NUCMLEL,R3                                                       
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,80                                                      
         BAS   RE,PUTEL                                                         
*--SET POSTING TYPE IN THE STATUS FIELD                                         
IN20     LR    RF,R4                                                            
         BAS   RE,SETSTAT                                                       
*                                                                               
INX      L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RE,R3                                                            
         EJECT                                                                  
* EDIT CODE (COMMANDS 'E', 'X' AND 'S')                                         
*                                                                               
* FCNY CAN NO LONGER ADD BUYS AFTER OCT1/90 CHECK                               
*        CLC   AGYALPH,=CL2'FC'                                                 
*        BE    *+14                                                             
*        CLC   AGYALPH,=CL2'OH'                                                 
*        BNE   INA                                                              
*        CLI   UNACTSW,C'A'                                                     
*        BNE   INA                                                              
*        CLC   NUKDATE,=XL2'B541'                                               
*        BL    INA                                                              
*        MVI   UNERROR,INVERR                                                   
*        B     UNX                                                              
*                                                                               
ED       ST    RE,SAVEREG                                                       
*        LA    R3,TYPTAB                                                        
         L     R3,=A(TYPTAB)                                                    
         A     R3,MYRELO                                                        
         LA    R1,TYPES                                                         
         CLC   UNEDATA,0(R3)       FIND DATA TYPE IN TABLE                      
         BE    ED2                                                              
         LA    R3,L'TYPTAB(R3)                                                  
         BCT   R1,*-14                                                          
         DC    H'0'                                                             
         SPACE                                                                  
ED2      CLI   COMMAND,C'E'        TEST FOR EDIT FROM SCREEN                    
         BE    ED3                                                              
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    *+8                                                              
         CVB   R0,DUB              PUT PACKED NUMBER FROM FVAL IN R0            
         B     ED4                                                              
         SPACE                                                                  
ED3      TM    4(R3),NOEXTR        TEST FOR EXTRACT OVERRIDE                    
         BO    ED4                                                              
         L     R2,UNFLDH           GET FIELD HEADER ADDRESS                     
         GOTO1 VGETFLD                                                          
         SPACE 1                                                                
ED4      SR    RF,RF                                                            
         ICM   RF,7,1(R3)                                                       
         A     RF,MYRELO                                                        
         BASR  RE,RF               GO TO VALIDATING ROUTINE                     
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* GET DEFAULT PROCESSING (COMMAND = 'N')                                        
*                                                                               
DEF      ST    RE,SAVEREG                                                       
         MVI   UNERROR,0                                                        
*        LA    R1,TYPTAB           POINT TO TYPE TABLE                          
         L     R1,=A(TYPTAB)                                                    
         A     R1,MYRELO                                                        
         LA    R2,L'TYPTAB         SET BXLE INCREMENT                           
         SPACE                                                                  
DEF2     TM    4(R1),SETDEF        TEST IF DEFAULT AVAILABLE                    
         BZ    DEF4                NO                                           
         CLI   UNEDATA,0           TEST FOR DATA TYPE FILTER                    
         BE    DEF3                NO                                           
         CLC   UNEDATA,0(R1)       TEST IF RIGHT ENTRY LOCATED                  
         BNE   DEF4                NO-BUMP TO NEXT ENTRY                        
         SPACE                                                                  
DEF3     SR    RF,RF                                                            
         ICM   RF,7,1(R1)          ROUTINE ADDRESS                              
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         SPACE                                                                  
DEF4     AR    R1,R2                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   DEF2                                                             
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* FINAL PROCESSING (COMMAND 'F')                                                
*                                                                               
FINAL    NTR1                                                                   
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   FINAL1                                                           
         CLI   NUKSUB,1            INSERT SUB-LINE FOR LINES GT 1               
         BE    *+10                                                             
         MVC   NUSUBPRT,NUKSUB                                                  
         SPACE                                                                  
FINAL1   XC    WORK,WORK                                                        
         LA    R3,WORK             BUILD ACTIVITY ELEMENT                       
         USING NUACTD,R3                                                        
         MVI   NUACTEL,X'99'                                                    
         MVI   NUACTLEN,22                                                      
         GOTO1 VDATCON,DMCB2,(2,TODAYC),(3,THREE)                               
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN,AUDREASN   REASON CODE                                  
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BNE   FINAL2              NO ELEMENT FOUND                             
         L     RE,12(R1)                                                        
         MVC   NUACTACD,2(RE)      COPY OVER ADD AUTH. CODE                     
         MVC   NUACTADT,4(RE)      AND THE CREATION DATE                        
         CLI   1(RE),13                                                         
         BL    FINAL2                                                           
         MVC   NUACTAID,12(RE)     COPY OVER ADD PERSONAL ID                    
         MVC   NUACTAGD,16(RE)     AND SECURITY AGENCY                          
         SPACE                                                                  
FINAL2   BAS   RE,DELEL                                                         
         BAS   RE,PUTEL                                                         
         B     UNX                                                              
         DROP  R3                                                               
         EJECT                                                                  
* POINTER GENERATION (OUPUT 25 BYTE ENTRIES TERMINATED BY X'FF')                
*                                                                               
* UNOVALS - ORIGINAL RECORD VALUES MUST BE SET BEFORE RECORD IS                 
* CHANGED SO THAT OLD POINTER CALL IS CORRECT.                                  
*                                                                               
POINT    ST    RE,SAVEREG                                                       
         L     R1,MYPARM                                                        
         L     R2,4(R1)            OUTPUT ADDRESS                               
         LA    R2,0(R2)            ZERO HOB                                     
*                                                                               
         MVC   KEYDATE,NUKDATE     INITIALIZE POINTER VALUES                    
         MVC   KEYSUB,NUKSUB                                                    
         MVC   KEYTIME,NUKTIME     START QH                                     
         MVC   KEYDAY,NUDAY                                                     
         CLI   4(R1),C'O'          TEST FOR OLD POINTER                         
         BNE   POINT2              NO                                           
         MVC   KEYDATE,UNODATE     YES-USE ORIGINAL RECORD VALUES               
         MVC   KEYSUB,UNOSUB                                                    
         MVC   KEYTIME,UNOTIME                                                  
         MVC   KEYDAY,UNODAY                                                    
         SPACE                                                                  
POINT2   MVI   DAYCODE,X'0A'       PRE-SET DAY CODE TO VAR                      
         LA    RE,DAYTAB                                                        
         LA    R0,DAYS                                                          
         CLC   KEYDAY,0(RE)                                                     
         BE    *+16                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R0,*-14                                                          
         B     *+10                NOT IN TABLE                                 
         MVC   DAYCODE,1(RE)       EXTRACT DAY VALUE FROM TABLE                 
*                                                                               
         PUSH  USING                                                            
         DROP  R4                                                               
         USING NUKPKEY,R2                                                       
         XC    0(NDIRLEN,R2),0(R2)                                              
         MVI   NUKPTYPE,X'84'      PROGRAM-DATE PASSIVE POINTER                 
         MVC   NUKPAM,NUKAM-NUKEY(R4)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R4)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R4)                                         
         MVC   NUKPPROG,NUKPROG-NUKEY(R4)                                       
         MVC   NUKPDATE,KEYDATE                                                 
         MVC   NUKPEST,NUKEST-NUKEY(R4)                                         
         MVC   NUKPSUB,KEYSUB                                                   
         MVC   NUKPDP,NUKDP-NUKEY(R4)                                           
         USING NUKEY,R2                                                         
         MVC   NUDA,NDXDA          DISK ADDRESS                                 
*                                                                               
*--SET POSTING TYPE IN THE STATUS FIELD                                         
         LR    RF,R2                                                            
         BAS   RE,SETSTAT                                                       
*                                                                               
         LA    R2,NDIRLEN(R2)      NEXT POINTER POSITION                        
         USING NUKDKEY,R2                                                       
         XC    0(NDIRLEN,R2),0(R2)                                              
         MVI   NUKDTYPE,X'94'      DAY-TIME PASSIVE POINTER                     
         MVC   NUKDAM,NUKAM-NUKEY(R4)                                           
         MVC   NUKDCLT,NUKCLT-NUKEY(R4)                                         
         MVC   NUKDEST,NUKEST-NUKEY(R4)                                         
         MVC   NUKDNET,NUKNET-NUKEY(R4)                                         
         MVC   NUKDDAY,DAYCODE                                                  
         MVC   NUKDTIME,KEYTIME                                                 
         MVC   NUKDPROG,NUKPROG-NUKEY(R4)                                       
         MVC   NUKDDATE,KEYDATE                                                 
         MVC   NUKDSUB,KEYSUB                                                   
         USING NUKEY,R2                                                         
         MVC   NUDA,NDXDA          DISK ADDRESS                                 
*                                                                               
*--SET POSTING TYPE IN THE STATUS FIELD                                         
         LR    RF,R2                                                            
         BAS   RE,SETSTAT                                                       
*                                                                               
         MVI   NDIRLEN(R2),X'FF'   LIST TERMINATOR                              
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         POP   USING                                                            
         EJECT                                                                  
*--SET POSTING TYPE IN THE STATUS FIELD                                         
SETSTAT  NI    20(RF),X'FC'                                                     
         NI    NURSTAT,X'FC'                                                    
*                                                                               
         CLI   NBPOSTYP,C'N'       NETWORK                                      
         BE    SETSTEX                                                          
         CLI   NBPOSTYP,C'C'       CABLE                                        
         BNE   *+16                                                             
         OI    20(RF),X'01'                                                     
         OI    NURSTAT,X'01'                                                    
         B     SETSTEX                                                          
         CLI   NBPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+16                                                             
         OI    20(RF),X'02'                                                     
         OI    NURSTAT,X'02'                                                    
         B     SETSTEX                                                          
         OI    20(RF),X'03'        OTHER                                        
         OI    NURSTAT,X'03'                                                    
SETSTEX  BR    RE                                                               
*                                                                               
* PRODUCT CODE(S) (SYNTAX IS CODE*CODE)                                         
*                                                                               
PRD      NTR1                                                                   
*--CHECK FOR MORE THEN 2 PRODUCTS                                               
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRD1A               TOO MANY PRODUCTS TO EDIT                    
         CLI   UNACTION,CPR        IS ACTION CHANGE PRODUCT                     
         BE    PRD1A               ALLOW                                        
         CLI   UNACTION,CM         IS ACTION CHANGE MULTIPLE                    
         BE    PRD1A               ALLOW                                        
         CLI   UNACTION,DM         IS ACTION DISPLAY MULTIPLE                   
         BNE   UNX                 NO, BYPASS                                   
*                                                                               
PRD1A    CLI   COMMAND,C'N'        TEST FOR GET DEFAULT                         
         BE    PRD1H                                                            
         CLI   COMMAND,C'S'                                                     
         BE    PRD2                                                             
         MVI   NUPRD,0                                                          
         MVI   NUPRD2,0                                                         
         NI    NUUNST2,X'FF'-X'20' FROZEN PROD. ALLOCATION BIT OFF              
         MVI   FREEZESW,NO                                                      
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   PRD2                                                             
         CLI   UNACTSW,C'A'        TEST FOR ACTION ADD                          
         BE    PRD1H               YES-GET DEFAULT                              
*                                  NO INPUT MEANS NO PRODUCTS ON CHANGE         
         OC    NBPRD(2),NBPRD      TEST IF UNIT HAD BEEN ALLOCATED              
         BZ    PRD15               UNIT HAD NO PRODUCTS-NO UNALLOCATION         
         OI    NUACTWHY,X'40'      BRAND/COST CHANGE                            
         BAS   RE,CHKTRA                                                        
         BNE   PRD1F                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'40'      SET ALLOC. CHANGE FLAG FOR TRAFFIC           
*                                                                               
PRD1F    CLI   UNPAYSW,YES         TEST IF UNIT IS PAID                         
         BE    PRD1G               YES - ERROR CANNOT UNALLOCATE PAID           
         CLI   UNBILLSW,YES         TEST IF UNIT IS PAID                        
         BNE   PRD15               NO-OK TO UNALLOCATE                          
PRD1G    MVI   UNERROR,UNALLERR                                                 
         B     PRDX                                                             
         SPACE                                                                  
PRD1H    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUPRD,NPAKMAST      DEFAULT IS PACKAGE MASTER ALLOCATION         
         B     PRDX                                                             
         SPACE 1                                                                
PRD2     CLI   FLD,DASH            TEST FOR DASH STARTING FIELD                 
         BNE   PRD3                NO                                           
         MVI   FREEZESW,YES        YES-SET FLAG                                 
         BAS   RE,REMOVE                                                        
         BE    PRDI                NOTHING ELSE IN FIELD                        
         SPACE 1                                                                
PRD3     GOTO1 VSCANNER,DMCB2,FLDH,(1,WORK),C',=,*'                             
         CLI   4(R1),0                                                          
         BE    PRDI                INVALID INPUT                                
         LA    R2,WORK             POINT R2 AT BLOCK                            
         CLI   0(R2),2                                                          
         BE    *+12                                                             
         CLI   0(R2),3                                                          
         BNE   PRDI                PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         ZIC   RF,0(R2)                                                         
         LA    R1,12(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDI                                                             
         MVC   THREE,12(R2)                                                     
         BAS   RE,VALPRD           CHECK INPUT                                  
         CLI   UNERROR,0                                                        
         BNE   PRDX                                                             
         CLI   COMMAND,C'S'                                                     
         BE    PRD4                                                             
         MVC   NUPRD,BYTE          PRODUCT NUMBER                               
         SPACE                                                                  
PRD4     CLI   1(R2),0             TEST FOR SECOND PRODUCT                      
         BNE   PRD5                YES-PROCESS                                  
         CLI   COMMAND,C'S'                                                     
         BE    PRD10                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRD10                                                            
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         TM    NUSDST3,X'40'       CHECK FOR COPY-SPLIT BIT                     
         BNZ   PRDI                IF C-SPLIT NO SECOND PROD ERROR              
         B     PRD10                                                            
*                                                                               
PRD5     CLI   1(R2),2                                                          
         BE    *+12                                                             
         CLI   1(R2),3                                                          
         BNE   PRDI                                                             
         ZIC   RF,1(R2)                                                         
         LA    R1,22(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDI                                                             
         MVC   THREE,22(R2)                                                     
         BAS   RE,VALPRD                                                        
         CLI   UNERROR,0                                                        
         BNE   PRDX                                                             
         CLI   COMMAND,C'S'                                                     
         BE    PRDX                                                             
         MVC   NUPRD2,BYTE                                                      
*        CLC   NUPRD,NUPRD2        TEST IF SAME CODE INPUT TWICE                
*        BE    PRDI                                                             
         SPACE                                                                  
PRD10    CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    PRDX                                                             
         CLI   FREEZESW,YES        TEST TO FREEZE ALLOCATION                    
         BNE   *+8                                                              
         OI    NUUNST2,X'20'       YES                                          
         CLI   UNACTSW,C'C'        TEST FOR CHANGE                              
         BNE   PRDX                NO                                           
         CLC   NUPRD(2),NBPRD      TEST FOR ALLOCATION CHANGE                   
         BE    PRDX                NO                                           
         OC    NBAFFTIM,NBAFFTIM   SEE IF AFFID SEEDED                          
         BZ    *+12                                                             
         MVI   UNERROR,AFFIDLCK                                                 
         B     PRDX                                                             
         OI    NUACTWHY,X'40'      BRAND/COST CHANGE                            
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BNE   PRD11               NO                                           
         MVI   UNERROR,ALLCHERR    YES-ERROR TO CHANGE ALLOC.                   
         B     PRDX                                                             
         SPACE 1                                                                
PRD11    BAS   RE,CHKTRA                                                        
         BNE   PRDX                                                             
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'40'      SET ALLOC. CHANGE FLAG FOR TRAFFIC           
         B     PRDX                                                             
*                                                                               
PRD15    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRDX                                                             
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         TM    NUSDST3,X'40'       CHECK FOR COPY-SPLIT                         
         BNZ   PRDI                IF C-SPLIT AND NO PROD ERROR                 
         B     PRDX                                                             
         SPACE                                                                  
PRDI     MVI   UNERROR,INVERR                                                   
         B     PRDX                                                             
         SPACE                                                                  
PRDX     B     UNX                                                              
         DROP  R3,RE                                                            
* PRDFORM-CHECKS FOR ALPHA IN FIRST PRODUCT FIELD, AND ALPHA-NUMERIC            
* IN THE REST.                                                                  
*        RF=LENGTH OF FIELD                                                     
*        R1=FIELD                                                               
*                                                                               
PRDFORM  NTR1                                                                   
         BCTR  RF,0                                                             
*--CHECK FIRST CHARACTER FOR ALPHA                                              
         CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
         LA    R1,1(R1)                                                         
*                                                                               
PRDFM20  CLI   0(R1),X'F0'                                                      
         BL    PRDFM30                                                          
         CLI   0(R1),X'F9'                                                      
         BNH   PRDFM40                                                          
         B     PRDBFRM                                                          
PRDFM30  CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
PRDFM40  LA    R1,1(R1)                                                         
         BCT   RF,PRDFM20                                                       
*                                                                               
         SR    R1,R1               SET GOOD RETURN                              
PRDBFRM  LTR   R1,R1                                                            
         B     UNX                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO REMOVE A LEADING CHARACTER FROM FLD                            
* ON EXIT, CC=EQ FOR NOTHING LEFT IN FIELD, NEQ FOR SOMETHING THERE             
*                                                                               
REMOVE   ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZR   RE                                                               
         STC   R1,FLDH+5                                                        
         EX    R1,MOVEFLD          SHIFT FLD OVER TO THE LEFT                   
         LA    RF,FLD(R1)          POINT TO LAST CHAR POSITION                  
         MVI   0(RF),C' '                                                       
         CLI   FLDH+5,0                                                         
         BR    RE                                                               
*                                                                               
MOVEFLD  MVC   FLD(0),FLD+1                                                     
         EJECT                                                                  
* SUB-ROUTINE TO CHECK INPUT BRAND CODE AGAINST CLIENT LIST (AT ENTRY           
* THREE CONTAINS INPUT, AT EXIT BRAND NUMBER IN BYTE)                           
*                                                                               
VALPRD   NTR1                                                                   
         LA    R0,220              COUNTER                                      
         LA    RF,CLILIST                                                       
         MVI   BYTE,0                                                           
         CLC   THREE,=C'POL'       TEST FOR POOL                                
         BE    *+14                YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BNE   VALPRD2             NO                                           
         MVI   UNERROR,INVERR      NOT A VALID ALLOCATION                       
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD2  OC    0(4,RF),0(RF)       TEST FOR E-O-L.                              
         BZ    VALPRD3             YES                                          
         CLC   THREE,0(RF)         COMPARE INPUT VS ENTRY                       
         BE    VALPRD4             FOUND IT                                     
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD2                                                       
         SPACE                                                                  
VALPRD3  MVI   UNERROR,PRDERR                                                   
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD4  MVC   BYTE,3(RF)          EXTRACT PRODUCT NUMBER                       
         SPACE                                                                  
VALPRD6  XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
         MVI   EKEYTYPE,X'00'                                                   
         MVC   EKEYAM,AGYMED                                                    
         MVC   EKEYCLT,CLIPK                                                    
         MVC   EKEYPRD,THREE                                                    
         MVC   EKEYEST,EST                                                      
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(L'EKEY),KEYSAVE TEST IF ESTIMATE FOUND                       
         BE    VALPRDX             YES                                          
         MVI   UNERROR,PRESTERR                                                 
         MVC   XTRA(L'THREE),THREE                                              
         SPACE 1                                                                
VALPRDX  B     UNX                                                              
         DROP  R3                                                               
         EJECT                                                                  
* PRODUCT 1 SHARE                                                               
*                                                                               
PRDSHR   NTR1                                                                   
*--CHECK FOR MORE THEN 2 PRODUCTS                                               
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRDS20              TOO MANY PRODUCTS TO EDIT                    
         CLI   UNACTION,CPR        IS ACTION CHANGE PRODUCT                     
         BE    PRDS20              ALLOW                                        
         CLI   UNACTION,CM         IS ACTION CHANGE MULTIPLE                    
         BE    PRDS20              ALLOW                                        
         CLI   UNACTION,DM         IS ACTION DISPLAY MULTIPLE                   
         BNE   UNX                 NO, BYPASS                                   
*                                                                               
PRDS20   CLI   COMMAND,C'S'                                                     
         BE    *+14                                                             
         XC    NUP1SHR,NUP1SHR                                                  
         NI    NUUNST2,X'FF'-X'04'                                              
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    PRDSHRX                                                          
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    PRDSHRE                                                          
         SR    R0,R0                                                            
         ICM   R0,3,6(R1)                                                       
         BZ    PRDS100                                                          
         C     R0,=F'10000'        CANNOT EXCEED 100 PERCENT                    
         BH    PRDSHRE                                                          
         CLI   COMMAND,C'S'                                                     
         BE    *+8                                                              
         STCM  R0,3,NUP1SHR                                                     
         B     PRDSHRX                                                          
         SPACE                                                                  
PRDS100  CLI   COMMAND,C'S'                                                     
         BE    *+8                                                              
         OI    NUUNST2,X'04'       1ST PRD HAS 0 SHARE                          
         B     PRDSHRX                                                          
         SPACE                                                                  
PRDSHRE  MVI   UNERROR,INVERR                                                   
         SPACE                                                                  
PRDSHRX  B     UNX                                                              
         EJECT                                                                  
* PREEMPT STATUS                                                                
*                                                                               
PRE      CLI   FLDH+5,0                                                         
         BE    PREX                                                             
         CLI   FLDH+5,1                                                         
         BH    PREER                                                            
         MVC   BYTE,NUUNITST                                                    
         NI    BYTE,X'BF'                                                       
*                                                                               
         CLI   FLD,C'N'                                                         
         BE    PRE20                                                            
         CLI   FLD,C'Y'                                                         
         BNE   PREER                                                            
         OI    BYTE,X'40'                                                       
*                                                                               
PRE20    CLI   COMMAND,C'S'                                                     
         BE    PREX                                                             
         MVC   NUUNITST,BYTE                                                    
PREX     B     UNX                                                              
*                                                                               
PREER    MVI   UNERROR,INVERR                                                   
         B     UNX                                                              
         EJECT                                                                  
* DELETE HOMES IMPRESSION                                                       
*                                                                               
DHI      ST    RE,SAVEREG1                                                      
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),(R9),RR=MYRELO                     
DHIX     L     RE,SAVEREG1                                                      
         BR    RE                                                               
         EJECT                                                                  
* UNIT STATUS (DUMMY ROUTINE DOES A YES NO CHECK)                               
*                                                                               
STATUS   CLI   FLDH+5,0                                                         
         BE    PREX                                                             
         CLI   FLDH+5,1                                                         
         BH    STATER                                                           
*                                                                               
         CLI   FLD,C'N'                                                         
         BE    PRE20                                                            
         CLI   FLD,C'Y'                                                         
         BNE   STATER                                                           
STATEX   B     UNX                                                              
*                                                                               
STATER   MVI   UNERROR,INVERR                                                   
         B     UNX                                                              
         EJECT                                                                  
* PROGRAM NAME                                                                  
*                                                                               
PRGN     ST    RE,SAVEREG1                                                      
         ST    R1,FULL                                                          
         CLI   COMMAND,C'S'                                                     
         BE    PRGNX                                                            
         MVC   NUPROGNM,SPACES                                                  
         SPACE                                                                  
PRGN2    CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    PRGN3               YES                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   PRGN4               YES                                          
         CLI   COMMAND,C'E'                                                     
         BNE   PRGN8                                                            
         CLI   UNACTSW,C'A'                                                     
         BNE   PRGN8                                                            
         SPACE                                                                  
PRGN3    L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   NUPROGNM,NPGNAME    PROGRAM NAME FROM PROGRAM RECD.              
         B     PRGN8                                                            
         SPACE                                                                  
PRGN4    MVC   NUPROGNM,FLD        INPUT DATA                                   
         SPACE                                                                  
*--CHECK WINDOW STATUS                                                          
PRGN8    L     R3,APROGREC                                                      
         LA    R3,24(R3)                                                        
PRGN9    CLI   0(R3),0                                                          
         BE    PRGNX                                                            
         CLI   0(R3),3                                                          
         BE    PRGN10                                                           
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     PRGN9                                                            
*                                                                               
PRGN10   MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         NI    NUSDST3,X'F7'         CLEAR WINDOW BYTE                          
*                                                                               
         TM    20(R3),X'80'        SEE IF WINDOW SET IN PROGRAM RECORD          
         BZ    PRGN12                                                           
         OI    NUSDST3,X'08'         SET WINDOW BYTE                            
*                                                                               
*--CHECK MIRROR STATUS                                                          
PRGN12   MVC   NUMIRTYP,21(R3)     MOVE MIRROR CODE INTO THE BUY                
*                                                                               
         XC    NUSDPT,14(RE)     CLEAR SUB-DAYPART                              
         MVC   NUSDPT,22(R3)     MOVE IN SUB-DAYPART                            
*                                                                               
         OC    NUSTCAR,NUSTCAR   UNIT SPECIFIC TCAR?                            
         BNZ   *+10                                                             
         MVC   NUSTCAR,25(R3)    MOVE IN DEFAULT PROG LEVEL TCAR                
*                                                                               
*--CHECK MULTI-RUNS                                                             
         CLI   26(R3),0            ANY MULTI-RUNS ?                             
         BE    PRGN13                                                           
*                                                                               
         MVI   ELCODE,X'60'                                                     
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'6004'                                                 
         MVI   WORK+2,C'L'         MULTI-RUN COUNT (NPGMRUN)                    
         MVC   WORK+3(1),26(R3)                                                 
         LA    R3,WORK                                                          
         BAS   RE,PUTEL                                                         
*                                                                               
         L     R3,APROGREC                                                      
         LA    R3,24(R3)                                                        
*                                                                               
*--SEED NAD DEFINITION NUMBER                                                   
PRGN13   MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRGN14                                                           
         L     RE,12(R1)                                                        
         MVC   2(6,RE),2(R3)       MOVE NAD CODE TO UNIT                        
         B     PRGNX                                                            
*                                                                               
PRGN14   LR    RE,R3               SAVE PROGRAM RECORD POINTER                  
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVC   0(2,R3),=X'620C'                                                 
         MVC   2(6,R3),2(RE)       DONT TAG IF UNIT BEFORE SEP5/88              
         BAS   RE,PUTEL                                                         
*                                                                               
PRGNX    L     RE,SAVEREG1                                                      
         L     R1,FULL                                                          
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* LENGTH (SECONDS)                                                              
*                                                                               
LEN      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    LEN2A                                                            
         CLI   FLD,C'C'            TEST FOR COPY-SPLIT CHARACTER                
         BE    *+16                                                             
         CLI   NBPRDNO,3           IF MORE THEN 3 PRDS MUST BE A COPY           
         BH    LENR                                                             
         B     LEN1                                                             
         CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BE    LENR                CANNOT CREAT COPY-SPLIT ON CM ACTION         
LEN1     MVI   NULEN,0                                                          
         MVI   NULEN1,0                                                         
         NI    NUUNST2,X'FF'-X'10'                                              
         MVI   FREEZESW,NO                                                      
         CLI   COMMAND,C'N'        TEST FOR SET DEFAULT                         
         BE    LEN1A                                                            
         CLI   FLDH+5,0                                                         
         BNE   LEN2                                                             
         CLI   COMMAND,C'E'        TEST FOR SCREEN EDIT                         
         BNE   LENM                                                             
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   LENM                                                             
         SPACE                                                                  
LEN1A    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVI   NULEN,30            DEFAULT IS 30 SECONDS                        
         CLI   NPAKLENG,0          CHECK PACKAGE DEFAULT                        
         BE    LENX                                                             
         MVC   NULEN,NPAKLENG      PACKEGE LEN                                  
         B     LENX                                                             
         DROP  RE                                                               
         SPACE                                                                  
LENM     MVI   UNERROR,MISERR                                                   
         B     LENX                                                             
*                                                                               
*                                                                               
*-- CHECK FOR SUBSIDIARY 01 ELEMENT                                             
LEN2     MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   LEN2A                                                            
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         NI    NUSDST3,X'BF'       RESET COPY-SPLIT BIT                         
         NI    NBUNST3,X'BF'                                                    
*                                                                               
LEN2A    CLI   FLD,C'C'            TEST FOR COPY-SPLIT CHARACTER                
         BNE   LEN3                                                             
         BAS   RE,REMOVE                                                        
         BE    LENR                                                             
         CLI   COMMAND,C'S'                                                     
         BE    LEN3                                                             
*                                                                               
         CLI   NUSDRD,X'02'        WAS ELEMENT FOUND                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NUSDST3,X'40'       SET COPY-SPLIT BIT                           
         OI    NBUNST3,X'40'       SET COPY-SPLIT BIT                           
         B     LEN3                                                             
*                                                                               
*LEN2B   XC    WORK,WORK           IF NO '02' THAN BUILD                        
*        LA    R3,WORK                                                          
*        USING NUSDRD,R3                                                        
*        MVI   NUSDREL,X'02'       BUILD A SUBSIDIARY 01 ELEMENT                
*        MVI   NUSDRLEN,20                                                      
*        OI    NUSDST3,X'40'       UNIT IS SET AS A COPY SPLIT                  
*        OI    NBUNST3,X'40'       SET COPY-SPLIT BIT                           
*        BAS   RE,PUTEL                                                         
*                                                                               
         SPACE 1                                                                
LEN3     TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BO    LEN4                YES                                          
         CLI   FLD,DASH            TEST FOR FREEZE CHARACTER                    
         BNE   LEN4                                                             
         MVI   FREEZESW,YES                                                     
         BAS   RE,REMOVE                                                        
         BE    LENR                                                             
*                                                                               
LEN4     GOTO1 VSCANNER,DMCB2,FLDH,(1,WORK),C',=,*'                             
         CLI   4(R1),0                                                          
         BE    LENR                INVALID INPUT                                
*                                                                               
         LA    R2,WORK             POINT R2 AT BLOCK                            
         CLI   0(R2),0                                                          
         BE    LENR                PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         TM    2(R2),X'80'         TEST IF NUMERIC                              
         BZ    LENR                                                             
         MVC   THREE,12(R2)                                                     
         MVC   BYTE,0(R2)                                                       
         BAS   RE,VALLEN           CHECK INPUT                                  
         CLI   COMMAND,C'S'                                                     
         BE    *+8                                                              
         STC   R0,NULEN                                                         
*                                                                               
         SR    R0,R0                                                            
         CLI   1(R2),0                                                          
         BE    LEN6                PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         TM    3(R2),X'80'         TEST IF NUMERIC                              
         BZ    LENR                                                             
         MVC   THREE,22(R2)                                                     
         MVC   BYTE,1(R2)                                                       
         BAS   RE,VALLEN           CHECK INPUT                                  
         CLI   COMMAND,C'S'                                                     
         BE    LENX                                                             
         ZIC   RE,NULEN                                                         
         STC   RE,NULEN1           STORE 1ST LEN IN NULEN1 IF 2                 
         AR    R0,RE                                                            
         STC   R0,NULEN            STORE TOTAL LENGTH IN NULEN                  
*                                                                               
LEN6     CLI   COMMAND,C'S'                                                     
         BE    LENX                                                             
*                                                                               
         CLI   FREEZESW,YES        TEST FOR FREEZE                              
         BNE   *+8                                                              
         OI    NUUNST2,X'10'       YES-SET INDICATOR                            
*                                                                               
         CLI   UNACTSW,C'C'        TEST FOR CHANGE ACTION                       
         BNE   LENX                                                             
         CLC   NULEN,NBLEN         TEST FOR LENGTH CHANGE                       
         BNE   *+14                NO                                           
         CLC   NULEN1,NBLEN1       TEST SECOND LENGTH FOR CHANGE                
         BE    LENX                NO                                           
         OC    NBAFFTIM,NBAFFTIM   SEE IF AFFID SEEDED                          
         BZ    LEN8                                                             
         MVI   UNERROR,AFFIDLCK                                                 
         B     LENX                                                             
LEN8     BAS   RE,CHKTRA                                                        
         BNE   LENX                                                             
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'80'      SET LENGTH CHANGE FLAG FOR TRAFFIC           
         B     LENX                                                             
         SPACE 1                                                                
LENR     MVI   UNERROR,INVERR                                                   
         SPACE 1                                                                
LENX     B     UNX                                                              
         DROP  R3                                                               
         SPACE 2                                                                
VALLEN   ST    RE,SAVEREG1                                                      
         CLI   BYTE,3                                                           
         BH    LENR                                                             
         ZIC   R1,BYTE                                                          
         LR    R0,R1               SAVE DATA LENGTH                             
         LA    RE,FLD              RE=DATA POINTER                              
VALL100  CLI   0(RE),C'0'          TEST FOR NUMERIC DATA                        
         BL    LENR                                                             
         CLI   0(RE),C'9'                                                       
         BH    LENR                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,VALL100                                                       
*                                                                               
         LR    R1,R0               RESTORE ORIGINAL LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,THREE(0)                                                     
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    LENR                                                             
         CH    R0,=H'255'          TEST FOR MAXIMUM VALUE                       
         BH    LENR                                                             
         L     RE,SAVEREG1                                                      
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* DAY AND AIR DATE                                                              
*                                                                               
DAY      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    DAY2                                                             
         MVI   NUDAY,0                                                          
         L     R3,APROGEL                                                       
         USING NPGEL92,R3                                                       
         MVC   HALF(1),NPGDAY      MOVE DAY VALUES TO HALF                      
         MVC   HALF+1(1),NPGDAYNO                                               
         CLI   NPGROT,0            IF ROTATION NUMBERS PRESENT                  
         BE    *+16                SUBSTITUTE ROTATION NUMBERS                  
         MVC   HALF(1),NPGROT      FOR DAY NUMBERS.                             
         MVC   HALF+1(1),NPGROTNO                                               
*                                                                               
         CLI   COMMAND,C'N'        TEST FOR GET DEFAULT                         
         BE    DAY1                                                             
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   DAY2                YES-GO VALIDATE IT                           
         CLI   UNACTSW,C'A'        ELSE GET THE DEFAULT FOR ACTION ADD          
         BE    DAY1                                                             
         MVI   UNERROR,MISERR      MUST INPUT DAY ON CHANGE                     
         B     DAY12                                                            
         SPACE                                                                  
DAY1     MVC   NUDAY,HALF          TAKE DAY FROM PROGRAM RECORD                 
         MVC   DAYNO,HALF+1        AND DAY NUMBERS                              
         CLI   BUYPROF+12,YES      DAY & DATE MUST MATCH                        
         BNE   DAY1A                                                            
         GOTO1 VDATCON,DMCB2,(2,NUKDATE),DUB                                    
         GOTO1 VGETDAY,(R1),DUB,THREE                                           
         ZIC   RE,0(R1)                                                         
         LA    RF,X'80'            DAY NUMBER OF AIR DATE                       
         SRL   RF,0(RE)                                                         
         STC   RF,NUDAY                                                         
         MVC   BYTE,HALF                                                        
         NC    BYTE,NUDAY          MAKE SURE ITS WITHIN PRG DAYS                
         BZ    UNCVERR                                                          
         MVC   DUB2,DUB                                                         
         B     DAY7                                                             
*                                                                               
DAY1A    ZIC   RE,DAYNO            ISOLATE THE START DAY NUMBER                 
         SRL   RE,4                                                             
         LA    R1,X'80'            SET UP A SINGLE DAY MASK                     
         SRL   R1,0(RE)            AND SHIFT TO ITS POSITION                    
         STC   R1,BYTE                                                          
         NC    NUDAY,BYTE          MAKE SURE ITS THE ONLY BIT ON                
         B     DAY4                                                             
         SPACE                                                                  
DAY2     ZIC   R0,FLDH+5                                                        
         GOTO1 VDAYVAL,DMCB2,((R0),FLD),WORK,DAYNO                              
         CLI   WORK,0              TEST FOR ERROR                               
         BNE   *+12                NO                                           
         MVI   UNERROR,DAYERR                                                   
         B     DAYX                                                             
         CLI   COMMAND,C'S'                                                     
         BE    DAYX                                                             
         MVC   NUDAY,WORK          SET DAY VALUE ON UNIT RECORD                 
         MVC   BYTE,NUDAY          EXTRACT DAY BITS AND TEST THAT               
         NC    BYTE,HALF           PROGRAM DAYS COVER UNIT DAYS                 
         CLC   BYTE,NUDAY                                                       
         BE    DAY3                YES THEY DO                                  
UNCVERR  MVI   UNERROR,COVERR                                                   
         B     DAY12                                                            
         SPACE                                                                  
* DAY CANNOT BE CHANGED IF UNIT IS MISSED OR MAKE-GOOD                          
*                                                                               
DAY3     CLI   UNACTSW,C'C'        TEST FOR CHANGE                              
         BNE   DAY5                                                             
         CLC   NUDAY,NBDAY         TEST IF DAY CHANGED                          
         BE    DAY5                NO                                           
         TM    NBUNITST,X'03'      TEST IF MISSED/MADE-GOOD                     
         BZ    DAY4                NEITHER                                      
         MVI   UNERROR,DAYCGERR                                                 
         B     DAY12                                                            
DAY4     OC    NBAFFTIM,NBAFFTIM   SEE IF AFFID SEEDED                          
         BZ    DAY5                                                             
         MVI   UNERROR,AFFIDLCK                                                 
         B     DAY12                                                            
* GENERATE THE AIR DATE (ASSUME DATE INPUT AS 'WEEK OF' IS IN KEY)              
*                                                                               
DAY5     GOTO1 VDATCON,DMCB2,(2,NUKDATE),DUB                                    
         GOTO1 VGETDAY,(R1),DUB,THREE                                           
         ZIC   R2,0(R1)            DAY NUMBER OF AIR DATE                       
         ZIC   R0,DAYNO            DAY NUMBER OF UNIT'S DAY                     
         SRL   R0,4                USE THE START DAY'S NUMBER                   
         MVC   DUB2,DUB            SET EBCDIC AIR DATE                          
         SR    R0,R2               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    DAY7                THERE IS NONE                                
         CLI   UNACTION,B                                                       
         BNE   *+20                                                             
         CLI   BUYPROF+12,YES      DAY & DATE MUST MATCH                        
         BNE   *+12                                                             
         MVI   UNERROR,DAYDTEMT                                                 
         B     DAY12                                                            
         GOTO1 VADDAY,(R1),DUB,DUB2,(R0)                                        
         SPACE                                                                  
DAY7     MVI   UNERROR,STERR                                                    
         CLC   DUB2(L'ESTSTART),ESTSTART  TEST IF BEFORE ESTIMATE               
         BL    DAY12                                                            
         MVI   UNERROR,ENDERR                                                   
         CLC   DUB2(L'ESTEND),ESTEND  TEST IF AFTER ESTIMATE                    
         BNH   DAY8                                                             
         CLI   UNACTION,C                                                       
         BE    *+12                                                             
         CLI   UNACTION,B                                                       
         BNE   DAY12                                                            
         SR    R0,R0                                                            
         ICM   R0,1,BUYPROF+14        TEST IF MAKEGOOD ALLOWED BEYOND           
         BZ    DAY12                  - ESTIMATE                                
         GOTO1 VADDAY,(R1),ESTEND,DUB,(R0)                                      
         CLC   DUB(6),DUB2                                                      
         BL    DAY12                                                            
*                                                                               
DAY8     MVI   UNERROR,0                                                        
         GOTO1 VDATCON,(R1),DUB2,(2,UNDATE)                                     
         MVC   NUKDATE,UNDATE      SET KEY DATE                                 
         SPACE                                                                  
* DATE HAS BEEN SET - NOW IF DATE CHANGE, TEST IF IT IS ALLOWED                 
*                                                                               
DAY9     CLI   UNACTSW,C'C'        TEST FOR CHANGE                              
         BNE   DAY12               NO                                           
         CLC   NUKDATE,NBACTDAT    TEST DATE CHANGE                             
         BE    DAY12               NO                                           
         MVI   UNERROR,DATCHERR                                                 
         TM    NBUNITST,X'03'      TEST IF UNIT MISSED/MAKE-GOOD                
         BNZ   DAY12               YES-ONE OF ABOVE                             
         CLI   UNBILLSW,YES        TEST IF UNIT BILLED                          
         BE    DAY12               YES-STOP CHANGE                              
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BE    DAY12               YES                                          
         MVI   UNERROR,0                                                        
*                                                                               
         BAS   RE,CHKTRA                                                        
         BNE   DAY12                                                            
         MVI   ELCODE,X'21'        SET DATE CHANGE FLAG FOR TRAFFIC             
         BAS   RE,GETEL                                                         
         BNE   DAY12                                                            
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'20'      DATE CHANGE                                  
         SPACE                                                                  
*                                                                               
DAY12    CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    DAYX                                                             
         SPACE                                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         CLC   NUDAY,NBDAY         TEST FOR CHANGE IN DAY                       
         BE    DAY14                                                            
         OI    5(RE),X'80'         DAY CHANGE INDICATOR                         
DAY14    CLC   NUKDATE,NBACTDAT    TEST FOR CHANGE IN DATE                      
         BE    DAYX                                                             
         OI    5(RE),X'40'         DATE CHANGE INDICATOR                        
         SPACE                                                                  
DAYX     B     UNX                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
* TIME                                                                          
*                                                                               
TIME     NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    TIME4                                                            
         XC    NUTIME,NUTIME                                                    
         SPACE                                                                  
TIME2    CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    TIME3                                                            
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   TIME4               VALIDATE INPUT                               
         CLI   COMMAND,C'E'        TEST FOR EDIT                                
         BNE   TIMEM               TIME NOT INPUT - ERROR                       
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   TIMEM               TIME LEFT OUT - ITS AN ERROR                 
         SPACE 1                                                                
TIME3    L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   FULL,NPGTIME                                                     
         CLC   FULL(3),=C'VARY'                                                 
         BNE   *+10                                                             
         MVC   FULL,=AL2(0600,2400) SCREEN OUT VARIOUS TIME                     
         B     TIME6                                                            
         SPACE                                                                  
TIMEM    MVI   UNERROR,MISERR                                                   
         B     TIME8                                                            
         SPACE                                                                  
TIME4    ZIC   R0,FLDH+5                                                        
         GOTO1 VTIMVAL,DMCB2,((R0),FLD),FULL                                    
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    TIMER                                                            
         CLC   FULL,=C'NONE'       SCREEN OUT SPECIAL CHARS.                    
         BE    TIMER                                                            
         CLC   FULL,=C'VARY'                                                    
         BNE   TIME6                                                            
         SPACE                                                                  
TIMER    MVI   UNERROR,TIMERR                                                   
         B     TIMEX                                                            
         SPACE                                                                  
TIME6    CLI   COMMAND,C'S'                                                     
         BE    TIMEX                                                            
         MVC   NUTIME,FULL                                                      
         BAS   RE,GETSQH           GET THE START QUARTER HOUR                   
         MVC   NUKTIME,UNSQH       SET KEY START QH                             
         SPACE                                                                  
TIME8    CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    TIMEX                                                            
         CLC   NUTIME,NBTIME       TEST FOR CHANGE IN TIME                      
         BE    TIMEX                                                            
         OC    NBAFFTIM,NBAFFTIM   SEE IF AFFID SEEDED                          
         BZ    TIME9                                                            
         MVI   UNERROR,AFFIDLCK                                                 
         B     TIMEX                                                            
         SPACE                                                                  
TIME9    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         OI    5(RE),X'20'         TIME CHANGE INDICATOR                        
TIMEX    B     UNX                                                              
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETSQH   SR    R1,R1                                                            
         ICM   R1,3,NUTIME         START TIME                                   
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
         STC   R1,UNSQH                                                         
         BR    RE                                                               
         EJECT                                                                  
* AFFIDAVIT TIME                                                                
*                                                                               
AFFID    NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(7,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* ASSIGNED COST                                                                 
*                                                                               
ASS      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    ASS2                                                             
* IF SECONDARY COST IS SET UP NO CHANGING OF                                    
* THE ASSIGNED COST WILL BE ALLOWED.                                            
         OC    CLICOST2,CLICOST2                                                
         BNZ   ASSX                                                             
         OC    ESTCOST2,ESTCOST2                                                
         BNZ   ASSX                                                             
*                                                                               
         MVC   FULL,NUASSIGN                                                    
         XC    NUASSIGN,NUASSIGN                                                
         NI    NUUNITST,X'FF'-X'88' TURN OFF MINUS UNIT,ASSGND CST BIT          
         MVI   MINUSSW,NO          INITIALIZE MINUS COST SWITCH                 
         SPACE 1                                                                
ASS2     CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    ASSX                                                             
         ZIC   R1,FLDH+5                                                        
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   ASS4                                                             
         MVI   0(RE),C' '                                                       
         MVI   MINUSSW,YES                                                      
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
ASS4     LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ASSR                                                             
         CLI   COMMAND,C'S'                                                     
         BE    ASSX                                                             
         ICM   RE,15,4(R1)         GET AMOUNT                                   
         CLI   MINUSSW,YES         TEST FOR MINUS AMOUNT                        
         BNE   *+6                                                              
         LNR   RE,RE               YES-FORCE COST NEGATIVE                      
         STCM  RE,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BZ    *+14                                                             
         CLC   FULL,NUASSIGN                                                    
         BE    *+8                                                              
         OI    NUUNITST,X'08'      ASSIGNED COST INPUT                          
*-- CHECK FOR SUBSIDIARY 01 ELEMENT                                             
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ASS6                                                             
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         OC    NUASSIGN,NUASSIGN                                                
         BZ    *+14                                                             
         CLC   FULL,NUASSIGN                                                    
         BE    *+8                                                              
         OI    NUSDST3,X'80'       ASSIGNED COST OVERRIDE BIT SET               
         B     ASS8                                                             
*                                                                               
ASS6     XC    WORK,WORK           IF NO '02' THAN BUILD                        
         LA    R3,WORK                                                          
         USING NUSDRD,R3                                                        
         MVI   NUSDREL,X'02'       BUILD A SUBSIDIARY 01 ELEMENT                
         MVI   NUSDRLEN,20                                                      
         OC    NUASSIGN,NUASSIGN                                                
         BZ    *+14                                                             
         CLC   FULL,NUASSIGN                                                    
         BE    *+8                                                              
         OI    NUSDST3,X'80'       ASSIGNED COST OVERIDE                        
         BAS   RE,PUTEL                                                         
*                                                                               
ASS8     CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    ASSX                                                             
         CLC   NUASSIGN,NBASSIGN   TEST FOR CHANGE IN ASSIGNED COST             
         BE    ASSX                                                             
         SPACE                                                                  
ASS10    OI    NUACTWHY,X'20'      ASSIGNED COST CHANGE INDICATOR               
         SPACE                                                                  
ASSX     B     UNX                                                              
         SPACE 1                                                                
ASSR     MVI   UNERROR,INVERR                                                   
         B     ASSX                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* ACTUAL COST                                                                   
*                                                                               
ACT      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    ACT2                                                             
         XC    NUACTUAL,NUACTUAL                                                
         NI    NUUNITST,X'FF'-X'20' TURN OFF COST OVERRIDE BIT                  
         MVI   MINUSSW,NO          INITIALIZE MINUS COST SWITCH                 
*                                                                               
         MVI   WORK,X'FF'                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACT2                                                             
         MVI   WORK,X'00'                                                       
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         XC    NUSDSRT,NUSDSRT                                                  
         SPACE 1                                                                
ACT2     BAS   RE,CHKRATE          CHECK RATE TYPE CODE                         
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    ACT10                                                            
         CLI   UNERROR,INVERR                                                   
         BE    INTX                                                             
         ZIC   R1,FLDH+5                                                        
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   ACT4                                                             
         MVI   MINUSSW,YES         MINUS COST                                   
         MVI   0(RE),C' '                                                       
         SH    R1,=H'1'                                                         
         BZ    ACTR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
ACT4     LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ACTR                                                             
         OC    4(4,R1),4(R1)                                                    
         BZ    ACT5                                                             
         TM    NBUNST3,X'02'       CHECK ADU UNIT                               
         BZ    ACT5                ONLY ZERO COST ALLOWED FOR ADU               
         MVI   UNERROR,ADUERR                                                   
         B     ACTX                                                             
ACT5     CLI   COMMAND,C'S'                                                     
         BE    ACT10                                                            
         ICM   RE,15,4(R1)                                                      
         CLI   MINUSSW,YES                                                      
         BNE   *+6                                                              
         LNR   RE,RE                                                            
         STCM  RE,15,NUACTUAL                                                   
         OI    NUUNITST,X'20'      ANY INPUT MEANS COST OVERRIDE                
         CLI   UNACTSW,C'C'        TEST FOR CHANGE ACTION                       
         BNE   ACT10               NO                                           
         CLC   NUACTUAL,NBACTUAL   TEST FOR ACTUAL COST CHANGE                  
         BE    ACT10                                                            
         OC    NBAFFTIM,NBAFFTIM   SEE IF AFFID SEEDED                          
         BZ    *+12                                                             
         MVI   UNERROR,AFFIDLCK                                                 
         B     ACTX                                                             
*                                                                               
         SPACE                                                                  
ACT10    CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    ACTX                                                             
         CLC   NUACTUAL,NBACTUAL   TEST FOR CHANGE IN ACTUAL COST               
         BE    ACTX                NO CHANGE                                    
*                                                                               
*        TM    UNBITOPT,X'80'      ALLOW CHANGE IF PAYED                        
*        BO    ACT10                                                            
         CLI   BUYPROF2+13,C'N'    CAN COST CHANGE ON PAID UNIT                 
         BE    ACT12               NO                                           
         CLI   BUYPROF2+13,C'A'    CAN COST CHANGE ON PAID UNIT                 
         BNE   ACT14               NO                                           
*                                                                               
ACT12    CLI   UNPAYSW,YES         WAS UNIT PAID                                
         BNE   ACT14                                                            
         MVI   UNERROR,PAYCHGNA    ERROR                                        
         B     ACTX                                                             
         SPACE                                                                  
ACT14    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         OI    5(RE),X'10'         ACTUAL COST CHANGE INDICATOR                 
*  CHECK SECOND COST PERCENTAGE                                                 
ACTX     GOTO1 =A(OVFLRTN),DMCB,(5,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         SPACE 1                                                                
ACTR     MVI   UNERROR,INVERR                                                   
         B     ACTX                                                             
*                                                                               
* RATE TYPE VALIDATOR                                                           
*                                                                               
CHKRATE NTR1                                                                    
         XC    HALF,HALF                                                        
*--CHECK BUY ACTUAL FIELD                                                       
         GOTO1 FLDCHK,DMCB2,RTFLDTAB,FLD,HALF          CHECK RATE               
         CLI   HALF,0                                                           
         BE    CHK20                                                            
         BAS   RE,CUTFLD                                                        
         GOTO1 FLDCHK,DMCB2,CVFLDTAB,FLD,HALF+1        CHECK COVERAGE           
         CLI   HALF+1,0                                                         
         BE    CHK200              BRANCH TO WRITE TO ELEMENT                   
         BAS   RE,CUTFLD                                                        
         B     CHK200                                                           
*--CHECK ESTIMATE RECORD                                                        
CHK20    GOTO1 FLDCHK,DMCB2,RTRECTAB,ESTRATE,HALF      CHECK RATE               
         CLI   ESTRATEC,X'40'                                                   
         BL    *+10                                                             
         MVC   HALF+1(1),ESTRATEC                                               
         CLI   HALF,0                                                           
         BNE   CHK200                                                           
*--CHECK CLIENT RECORD                                                          
CHK40    MVC   BYTE,CLIPRO+14                                                   
         GOTO1 FLDCHK,DMCB2,RTRECTAB,BYTE,HALF         CHECK RATE               
         CLI   CLIEXTRA+14,X'40'                                                
         BL    *+10                                                             
         MVC   HALF+1(1),CLIEXTRA+14                                            
         CLI   HALF,0                                                           
         BE    UNX                                                              
*                                                                               
CHK200   CLI   COMMAND,C'S'                                                     
         BE    UNX                                                              
         CLI   WORK,X'FF'          SEE IF X'02' ELEMENT EXISTS                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NUSDSRT,HALF        MOVE IN RATE                                 
         MVI   NUSDRTCV,0                                                       
         CLI   HALF+1,C'A'         DONT MOVE DEFAULT VALUE OUT                  
         BE    *+10                                                             
         MVC   NUSDRTCV,HALF+1     MOVE IN COVERAGE                             
         B     UNX                                                              
*                                                                               
*****    REMOVES FIRST CHARACTER FROM FIELD                                     
CUTFLD   NTR1                                                                   
         MVC   FLD(9),FLD+1                                                     
         MVI   FLD+9,X'40'                                                      
         ZIC   R3,FLDH+5                                                        
         BCTR  R3,0                                                             
         STC   R3,FLDH+5                                                        
         B     UNX                                                              
*                                                                               
*****   INPUT          P1=A(TABLE)                                              
*                      P2=RATE                                                  
*       OUTPUT         P3=1 BYTE OUTPUT SET TO ZERO NOT FOUND                   
FLDCHK   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
FLDCH50  CLI   0(R2),C' '                                                       
         BE    FLDCH100                                                         
         CLC   0(1,R2),0(R3)                                                    
         BE    FLDCH70                                                          
         LA    R2,2(R2)                                                         
         B     FLDCH50                                                          
FLDCH70  MVC   0(1,R4),1(R2)                                                    
         B     FLDCHEX                                                          
FLDCH100 MVI   0(R4),0                                                          
FLDCHEX  B     UNX                                                              
*                                                                               
RTFLDTAB DC    CL17'FFCCWWYYHHTTRRJJ '                                          
RTRECTAB DC    CL9'2F8C9WYY '                                                   
CVFLDTAB DC    CL7'AAIITT '                                                     
         EJECT                                                                  
* INTEGRATION RATE                                                              
*                                                                               
INT      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    INT2                                                             
         MVC   FULL,NUINTEG                                                     
         XC    NUINTEG,NUINTEG                                                  
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         NI    NUSDST4,X'FF'-X'80' RESET COST INPUTTED BIT                      
         DROP  RE                                                               
*                                                                               
         MVI   MINUSSW,NO          INITIALIZE MINUS COST SWITCH                 
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    INT1                YES                                          
         CLI   FLDH+5,0                                                         
         BNE   INT2                                                             
         CLI   COMMAND,C'E'                                                     
         BNE   INTX                                                             
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST FOR TABLE DEFAULT                       
         BO    TESTITBL            YES READ FOR TABLE                           
         CLI   UNACTSW,C'A'                                                     
         BNE   INTX                                                             
         SPACE                                                                  
INT1     L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         ICM   RF,15,UNINGTBL                                                   
         BNZ   TESTITBL            IF 0 CHECK FOR INTG TABLE LOOK UP            
*                                                                               
         TM    NUUNITST,X'04'      CHECK FOR PFB                                
         BO    INTX                ONLY ZERO COST ALLOWED FOR PFB               
         TM    NBUNST3,X'02'       CHECK ADU UNIT                               
         BO    INTX                ONLY ZERO COST ALLOWED FOR ADU               
         MVC   NUINTEG,NPAKINT                                                  
         B     INTX                                                             
*                                                                               
         DROP  RE                                                               
         SPACE                                                                  
INT2     ZIC   R1,FLDH+5           VALIDATE INPUT                               
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   INT4                                                             
         MVI   MINUSSW,YES                                                      
         MVI   0(RE),C' '                                                       
         SH    R1,=H'1'                                                         
         BZ    INTR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
INT4     CLC   =C'TBL',FLD                                                      
         BE    TESTITBL                                                         
         CLI   FLDH+5,0            NO INPUT IN INTEG                            
         BNE   *+16                                                             
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST FOR TABLE DEFAULT                       
         BO    TESTITBL            YES READ FOR TABLE                           
         DROP  RE                                                               
*                                                                               
         LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    INTR                                                             
         OC    4(4,R1),4(R1)       CHECK ZERO AMOUNT                            
         BZ    INT4B                                                            
         MVI   UNERROR,ADUERR                                                   
         TM    NUUNITST,X'04'      CHECK FOR PFB                                
         BO    INTX                ONLY ZERO COST ALLOWED FOR PFB               
         TM    NBUNST3,X'02'       CHECK ADU UNIT                               
         BO    INTX                ONLY ZERO COST ALLOWED FOR ADU               
         MVI   UNERROR,0                                                        
INT4B    CLI   COMMAND,C'S'                                                     
         BE    INTX                                                             
         ICM   RE,15,4(R1)                                                      
INT5     CLI   MINUSSW,YES         TEST FOR MINUS COST                          
         BNE   *+6                                                              
         LNR   RE,RE               YES-FORCE AMOUNT NEGATIVE                    
         STCM  RE,15,NUINTEG                                                    
         OC    NUINTEG,NUINTEG                                                  
         BNZ   INT9                                                             
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         OI    NUSDST4,X'80'       SET INT INPUTTED BIT                         
         DROP  RE                                                               
*                                                                               
INT9     CLI   UNACTSW,C'C'        TEST FOR CHANGE ACTION                       
         BNE   INTX                NO                                           
         CLC   NUINTEG,NBINTEG     TEST FOR INTEGRATION CHANGE                  
         BE    INTX                                                             
*                                                                               
*        TM    UNBITOPT,X'80'      ALLOW CHANGE IF PAYED                        
*        BO    INTX                                                             
         CLI   BUYPROF2+13,C'N'    CAN COST CHANGE ON PAID UNIT                 
         BE    INT11                                                            
         CLI   BUYPROF2+13,C'A'    CAN COST CHANGE ON PAID UNIT                 
         BNE   INTX                                                             
*                                                                               
INT11    CLI   UNPAYSW,YES                                                      
         BNE   INTX                                                             
         MVI   UNERROR,PAYCHGNA                                                 
         B     INTX                                                             
         SPACE 1                                                                
INTR     MVI   UNERROR,INVERR                                                   
         B     INTX                                                             
         SPACE                                                                  
INTX     B     UNX                                                              
         SPACE                                                                  
TESTITBL ICM   R3,15,UNINGTBL      USE INTEGTRATION TABLE                       
         BZ    INTX                                                             
*                                                                               
         TM    NUUNITST,X'04'      CHECK FOR PFB                                
         BO    INTX                ONLY ZERO COST ALLOWED FOR PFB               
         TM    NBUNST3,X'02'       CHECK ADU UNIT                               
         BO    INTX                ONLY ZERO COST ALLOWED FOR ADU               
*                                                                               
         CLI   INTGREAD,0          READ INTEGTRATION TABLE                      
         BNE   INT10               - ALREADY READ                               
         XC    SPACES,SPACES                                                    
         LA    R2,SPACES           SET UP INTEG BLOCK                           
         USING NIBLKD,R2                                                        
         MVC   NIBAIO,INTGAIO      A(2K BLOCK)                                  
         ST    R6,NIBNETB          A(NETBLOCK)                                  
         MVC   NIBAM,NBACTAM                                                    
         MVC   NIBNTWK,NBACTNET                                                 
         OI    NIBNTWK+3,X'40'     MAKE LAST BYTE X'40'                         
         MVC   NIBAIO2,INTHLDIT    SAVE INTEG VALUES HERE                       
         OI    NIBCNTL,X'80'       1ST PASS                                     
         MVC   NIBSTRT(4),INTGSTDT DATE RANGE                                   
         GOTO1 =V(NETINTG),DMCB,(R2),RR=MYRELO                                  
         BNZ   INTER                                                            
         MVI   INTGREAD,1          SET READ ONCE INDICATOR                      
*                                                                               
INT10    XC    SPACES,SPACES                                                    
         LA    R2,SPACES           SET UP INTEG BLOCK                           
         USING NIBLKD,R2                                                        
         MVC   NIBAIO,INTGAIO      A(2K BLOCK)                                  
         ST    R6,NIBNETB          A(NETBLOCK)                                  
         MVC   NIBAIO2,INTHLDIT    SAVE INTEG VALUES HERE                       
         MVC   NIBDSKSV,INTDKA     LAST A(OF INTEG RECORD)                      
         MVC   NIBAM,NBACTAM                                                    
         MVC   NIBNTWK,NBACTNET                                                 
         OI    NIBNTWK+3,X'40'     MAKE LAST BYTE X'40'                         
         MVC   NIBDAY,NUDAY                                                     
         MVC   NIBDPT,NBACTDP                                                   
         MVC   NIBTIME,NUTIME                                                   
         MVC   NIBPROG,NUKPROG                                                  
         MVC   NIBSTRT,INTGSTDT    DATE RANGE                                   
         GOTO1 =V(NETINTG),DMCB,(R2),RR=MYRELO                                  
         BNZ   INTER                                                            
*                                                                               
         MVC   INTDKA,NIBDSKSV     SET READ ONCE INDICATOR                      
         ICM   RE,15,NIBRATE       SET READ ONCE INDICATOR                      
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         B     INT5                                                             
         SPACE                                                                  
INTER    MVI   UNERROR,INTGNF                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         B     INTX                                                             
         DROP  R2                                                               
         EJECT                                                                  
* UNIVERSE CODE                                                                 
*                                                                               
UNIV     NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* UNIVERSE PERCENTAGE                                                           
*                                                                               
UPCT     NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    UPCT4                                                            
         XC    NUUNIV,NUUNIV                                                    
         SPACE 1                                                                
UPCT2    CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    UPCT3               YES                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   UPCT4               YES-GO VALIDATE IT                           
         CLI   COMMAND,C'E'                                                     
         BNE   UPCTX                                                            
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BNE   UPCTX               NO-EXIT                                      
         SPACE 1                                                                
UPCT3    L     RE,APACKREC         GET DEFAULT VALUE                            
         USING NPRECD,RE                                                        
         MVC   NUUNIV,NPAKUNIV                                                  
         B     UPCTX                                                            
         SPACE 1                                                                
UPCT4    ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BNE   *+12                                                             
         MVI   UNERROR,INVERR                                                   
         B     UPCTX                                                            
*                                                                               
         CLI   COMMAND,C'S'                                                     
         BE    *+10                                                             
         MVC   NUUNIV,6(R1)                                                     
         SPACE                                                                  
UPCTX    B     UNX                                                              
         DROP  RE                                                               
         EJECT                                                                  
* FEED PERCENTAGE                                                               
*                                                                               
FEED     NTR1                                                                   
         CLI   UNACTION,CPR        IS ACTION CHANGE PRODUCT                     
         BE    FEED1               BYPASS TEST                                  
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    UNX                 TOO MANY PRODUCTS TO EDIT                    
*                                                                               
FEED1    CLI   COMMAND,C'S'                                                     
         BE    FEED5                                                            
         XC    NUFEED,NUFEED                                                    
*                                                                               
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    FEED4                                                            
         CLI   FLDH+5,0                                                         
         BNE   FEED5               VALIDATE INPUT                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   FEED3                                                            
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         TM    NUSDST3,X'40'       CHECK FOR COPY-SPLIT BIT                     
         BNZ   FEED8               IF C-SPLIT NO SECOND PROD ERROR              
         DROP  R3                                                               
*                                                                               
FEED3    CLI   COMMAND,C'E'                                                     
         BNE   FEEDX                                                            
         CLI   UNACTSW,C'A'                                                     
         BNE   FEEDX                                                            
         SPACE 1                                                                
FEED4    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUFEED,NPAKFEED     DEFAULT FROM PACKAGE RECORD                  
         B     FEEDX                                                            
         SPACE                                                                  
FEED5    ZIC   R3,FLDH+5                                                        
         BCTR  R3,0                                                             
         LA    RF,FLD                                                           
         AR    RF,R3                                                            
         CLI   0(RF),C'R'          SHOULD FEED AFFECT RATINGS                   
         BE    FEED6                                                            
         LA    R3,1(R3)                                                         
         B     FEED7                                                            
*                                                                               
FEED6    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         OI    2(RE),X'01'         SET FEED PCT FOR RATING BIT                  
*                                                                               
FEED7    GOTO1 VCASHVAL,DMCB2,FLD,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
FEED8    MVI   UNERROR,INVERR                                                   
         B     FEEDX                                                            
         SPACE                                                                  
*                                                                               
         CLI   COMMAND,C'S'                                                     
         BE    FEEDX                                                            
         MVC   NUFEED,6(R1)                                                     
         SPACE                                                                  
FEEDX    B     UNX                                                              
         DROP  RE                                                               
         EJECT                                                                  
* IMPACT PERCENTAGE                                                             
*                                                                               
IMP      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    IMP4                                                             
         XC    NUIMPACT,NUIMPACT                                                
         SPACE                                                                  
IMP2     CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    IMP3                                                             
         CLI   FLDH+5,0                                                         
         BNE   IMP4                VALIDATE INPUT                               
         CLI   COMMAND,C'E'                                                     
         BNE   IMPX                                                             
         CLI   UNACTSW,C'A'                                                     
         BNE   IMPX                                                             
         SPACE 1                                                                
IMP3     L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUIMPACT,NPAKIMP    DEFAULT FROM PACKAGE RECORD                  
         B     IMPX                                                             
         SPACE                                                                  
IMP4     ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   UNERROR,INVERR                                                   
         B     IMPX                                                             
         SPACE                                                                  
*                                                                               
         CLI   COMMAND,C'S'                                                     
         BE    *+10                                                             
         MVC   NUIMPACT,6(R1)                                                   
         SPACE                                                                  
IMPX     B     UNX                                                              
         DROP  RE                                                               
         EJECT                                                                  
* PACKAGE GUARANTEE FACTOR                                                      
*                                                                               
PG       NTR1                                                                   
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    PG2                 YES                                          
         MVI   ELCODE,X'B3'        DELETE PACKAGE GUARANTEE EL                  
***      MVI   ELCODE,X'B1'        DELETE PACKAGE GUARANTEE EL                  
         BAS   RE,DELEL                                                         
         SPACE 1                                                                
PG2      CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    PGX                 NONE-EXIT                                    
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,(4,FLD),(R0)                                      
***      GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BNE   PG4                                                              
         MVI   UNERROR,INVERR                                                   
         B     PGX                                                              
         SPACE 1                                                                
PG4      CLI   COMMAND,C'S'                                                     
         BE    PGX                                                              
         MVC   FULL,4(R1)          GET GUARANTEE FACTOR                         
         OC    FULL,FULL           TEST FOR ZERO                                
***      MVC   HALF,6(R1)          GET GUARANTEE FACTOR                         
***      OC    HALF,HALF           TEST FOR ZERO                                
         BZ    PGX                 YES-JUST DELETE ELEMENT                      
*                                                                               
         XC    DUB,DUB                                                          
         LA    R3,DUB                                                           
         USING NUNGUD,R3                                                        
         MVI   NUNGUEL,X'B3'       BUILD PACKAGE GUARANTEE ELEMENT              
         MVI   NUNGULEN,NUNGUELN                                                
         MVC   NUNGUFAC,FULL                                                    
***      USING NUGUAD,R3                                                        
***      MVI   NUGUAEL,X'B1'       BUILD PACKAGE GUARANTEE ELEMENT              
***      MVI   NUGUALEN,NUGUAELN                                                
***      MVC   NUGUAFAC,HALF                                                    
         BAS   RE,PUTEL                                                         
         SPACE 1                                                                
PGX      B     UNX                                                              
         EJECT                                                                  
* DEMO GUARANTEE FACTOR                                                         
*                                                                               
DG       NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(8,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* NTI CODE                                                                      
*                                                                               
NTI      NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* NSI CODE                                                                      
*                                                                               
NSI      NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* HUT AVERAGE CODE                                                              
*                                                                               
HAVE     ST    RE,SAVEREG1                                                      
         CLI   COMMAND,C'S'        TEST FOR DEFAULT                             
         BE    HAVE2                                                            
         MVI   NUHUTAVE,0                                                       
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    HAVE1                                                            
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   HAVE2                                                            
         CLI   COMMAND,C'E'        TEST FOR EDIT                                
         BNE   HAVEX                                                            
         CLI   UNACTSW,C'A'        GET DEFAULT WHEN ADDING                      
         BNE   HAVEX                                                            
         SPACE                                                                  
HAVE1    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUHUTAVE,NPAKHUTA   DEFAULT FROM PACKAGE RECORD                  
         BAS   RE,HUTFL            CHECK HUT FLAVOR                             
         B     HAVEX                                                            
         SPACE                                                                  
HAVE2    CLI   FLDH+5,2                                                         
         BH    HAVER                                                            
         CLI   FLD,C'W'            WEEKLY, MONTHLY, OR QUARTERLY                
         BE    HAVE3                                                            
         CLI   FLD,C'M'                                                         
         BE    HAVE3                                                            
         CLI   FLD,C'Q'                                                         
         BNE   HAVER                                                            
         SPACE 1                                                                
HAVE3    CLI   COMMAND,C'S'        TEST FOR DEFAULT                             
         BE    HAVEX                                                            
         MVC   NUHUTAVE,FLD                                                     
         BAS   RE,HUTFL            CHECK HUT FLAVOR                             
         B     HAVEX                                                            
         SPACE 1                                                                
HAVER    MVI   UNERROR,INVERR                                                   
         SPACE                                                                  
HAVEX    L     RE,SAVEREG1                                                      
         BR    RE                                                               
*                                                                               
HUTFL    NTR1                                                                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    HUTFL60                                                          
*                                                                               
         XC    WORK,WORK           IF NO '02' THAN BUILD                        
         LA    R3,WORK                                                          
         MVI   WORK,X'02'          BUILD A SUBSIDIARY 01 ELEMENT                
         MVI   WORK+1,20                                                        
         BAS   RE,PUTEL                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HUTFL60  L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR DEFAULT                             
         BE    HUTFL100                                                         
         MVI   NUSDHFL,0                                                        
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    HUTFL80                                                          
         CLI   FLDH+5,1            TEST FOR INPUT                               
         BH    HUTFL100                                                         
         CLI   COMMAND,C'E'        TEST FOR EDIT                                
         BNE   HAVEX                                                            
         CLI   UNACTSW,C'A'        GET DEFAULT WHEN ADDING                      
         BNE   HAVEX                                                            
*                                                                               
HUTFL80  L     RE,APACKREC                                                      
         OC    NPAKHUTF,NPAKHUTF   DEFAULT FIRST FROM PACKAGE                   
         BZ    HUTFLEX                                                          
         MVC   NUSDHFL,NPAKHUTF                                                 
         B     HUTFLEX                                                          
*                                                                               
HUTFL100 CLI   FLD+1,C'C'                                                       
         BE    HUTFL120                                                         
         CLI   FLD+1,C'B'                                                       
         BE    HUTFL120                                                         
         CLI   FLD+1,C'H'                                                       
         BE    HUTFL120                                                         
         CLI   FLD+1,C'N'                                                       
         BNE   HAVER                                                            
HUTFL120 MVC   NUSDHFL,FLD+1                                                    
*                                                                               
HUTFLEX  B     UNX                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
* HUT SCHEME                                                                    
*                                                                               
HUTS     NTR1                                                                   
         CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    HUTS2                                                            
         MVI   NUHUTSCM,0                                                       
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    HUTS1                                                            
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   HUTS2                                                            
         CLI   COMMAND,C'E'        TEST FOR EDIT                                
         BNE   HUTSX                                                            
         CLI   UNACTSW,C'A'        GET DEFAULT WHEN ADDING                      
         BNE   HUTSX                                                            
         SPACE                                                                  
HUTS1    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUHUTSCM,NPAKHUTS   DEFAULT FROM PACKAGE RECORD                  
         B     HUTSX                                                            
         SPACE                                                                  
HUTS2    CLI   FLDH+5,1                                                         
         BNE   HUTSR                                                            
         CLI   COMMAND,C'S'                                                     
         BE    HUTSX                                                            
         MVC   NUHUTSCM,FLD                                                     
         B     HUTSX                                                            
         SPACE 1                                                                
HUTSR    MVI   UNERROR,INVERR                                                   
         SPACE                                                                  
HUTSX    B     UNX                                                              
         DROP  RE                                                               
         EJECT                                                                  
* BILLBOARD CODE (TRAFFIC)                                                      
*                                                                               
BB       NTR1                                                                   
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    BBX                                                              
         CLI   FLD,DASH            TEST FOR INPUT                               
         BE    *+14                                                             
         CLC   FLD(2),=C'BB'       TEST FOR INPUT                               
         BNE   BBR                                                              
*                                                                               
         MVI   ELCODE,X'14'        CHECK IF TRIGGY BACK                         
         BAS   RE,GETEL                                                         
         BNE   BB100                                                            
         L     RE,12(R1)                                                        
         TM    2(RE),X'80'         IS IT A TRIGGY?                              
         BO    BBR2                                                             
*                                                                               
BB100    CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    BBX                 YES-ALL DONE                                 
         LA    RE,27(R4)           LOOK FOR '21' ELEM                           
BB300    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    BB400                                                            
         CLI   0(RE),X'21'                                                      
         BNE   BB300                                                            
         CLI   FLD,DASH            TEST FOR INPUT                               
         BE    *+12                                                             
         OI    NUCMLFLG-NUCMLEL(RE),X'04'                                       
         B     BBX                                                              
         NI    NUCMLFLG-NUCMLEL(RE),X'FB'                                       
*        XC    NUCMLBSL(17),NUCMLBSL CLEAR BILLBOARD INFO                       
*--CLEAR 23 ELEMENTS                                                            
*BB350   ZIC   RF,1(RE)                                                         
*        AR    RE,RF                                                            
*        CLI   0(RE),0                                                          
*        BE    BBX                                                              
*        CLI   0(RE),X'23'                                                      
*        BNE   BB350                                                            
*        XC    22(17,RE),22(RE)      CLEAR BILLBOARD INFO                       
*        B     BB350                                                            
         B     BBX                                                              
*                                                                               
BB400    CLI   FLD,DASH            TEST FOR INPUT                               
         BE    BBX                                                              
         XC    SPACES,SPACES       IF NO '21' THAN BUILD                        
         LA    R3,SPACES                                                        
         USING NUCMLEL,R3                                                       
         MVI   NUCMLEID,X'21'      BUILD A BILLBOARD COMMERCIAL                 
         MVI   NUCMLELN,80                                                      
         OI    NUCMLFLG,X'04'      BILLBOARD BIT                                
         BAS   RE,PUTEL                                                         
         MVI   SPACES,C' '         RESET SPACES                                 
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         B     BBX                                                              
         SPACE 1                                                                
BBR      MVI   UNERROR,INVERR                                                   
         B     BBX                                                              
BBR2     MVI   UNERROR,NOBBTRIG                                                 
         B     BBX                                                              
         SPACE 1                                                                
BBX      B     UNX                                                              
         DROP  R3                                                               
         EJECT                                                                  
* FEED CODE (TRAFFIC)                                                           
*                                                                               
FCD      NTR1                                                                   
         CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    FC120               YES                                          
         MVI   ELCODE,X'22'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
* ONLY LOOK AT MEDIA TRAFFIC COMML.                                             
         TM    NBUNST3,X'40'       IF BUY A COPY SPLIT                          
         BZ    FC050               IF NOT DELETE 23 ELEMENTS                    
         CLI   DUB,0                                                            
         BNE   FC120               IF SECOND PASS DONT DELETE 23 ELEM           
FC050    XC    DUB(16),DUB         CLEAR DUB/DUB2                               
         LA    R2,DUB              STORE CODES THAT CAN'T BE CHANGED            
         MVI   HALF,0                                                           
         LA    RE,27(R4)                                                        
FC100    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    FC120                                                            
         CLI   0(RE),X'23'                                                      
         BNE   FC100                                                            
         TM    NUFDCFL2-NUFDCEL(RE),X'01'                                       
         BZ    FC100                                                            
         TM    NUFDCFL2-NUFDCEL(RE),X'40'                                       
         BNZ   *+12                                                             
         OI    NUFDCFL2-NUFDCEL(RE),X'80'                                       
         B     FC100                                                            
*                                                                               
         ZIC   RF,HALF                                                          
         LA    RF,1(RF)                                                         
         STC   RF,HALF                                                          
         MVC   0(4,R2),2(RE)                                                    
         LA    R2,4(R2)                                                         
         B     FC100                                                            
         SPACE 1                                                                
FC120    CLI   FLDH+5,0                                                         
         BE    FCDX                                                             
         LA    R2,IOA              POINT R2 AT BLOCK                            
         SPACE 1                                                                
         GOTO1 VSCANNER,DMCB2,FLDH,(5,(R2)),C',=//'                             
         CLI   4(R1),0                                                          
         BE    FCDR                INVALID INPUT                                
         CLI   0(R2),1             TEST FOR LEN OF 1                            
         BNE   FC140                                                            
         CLI   12(R2),DASH         TEST FOR DASH                                
         BNE   FC140               YES-ITS A SPECIAL DELETE CHARACTER           
         CLI   HALF,0                                                           
         BNE   FC170                                                            
         B     FCDX                                                             
*                                                                               
FC140    SR    R0,R0                                                            
         ICM   R0,1,HALF           CHECK FOR FEEDS THAT CAN,T BE DELETE         
         BZ    FC200                                                            
         LA    R1,DUB              STORE CODES THAT CAN'T BE CHANGED            
FC150    LR    RE,R2                                                            
         LA    RF,5                                                             
FC160    CLI   0(RE),0                                                          
         BE    FC170                                                            
         CLC   12(4,RE),0(R1)                                                   
         BE    FC180                                                            
         LA    RE,32(RE)                                                        
         BCT   RF,FC160                                                         
FC170    MVI   UNERROR,FEDCNDEL                                                 
         B     FCDX                                                             
*                                                                               
FC180    LA    R1,4(R1)                                                         
         BCT   R0,FC150                                                         
*                                                                               
FC200    LA    R0,4                CHECK DUPLICATES                             
FC220    LA    RE,32(R2)                                                        
         LR    RF,R0                                                            
FC240    CLI   0(RE),0                                                          
         BE    FC280                                                            
         CLC   12(4,RE),12(R2)                                                  
         BNE   *+12                                                             
         MVI   UNERROR,DUPFEDC                                                  
         B     FCDX                                                             
*                                                                               
FC260    LA    RE,32(RE)                                                        
         BCT   RF,FC240                                                         
*                                                                               
FC280    LA    R2,32(R2)                                                        
         CLI   0(R2),0                                                          
         BE    FC300                                                            
         BCT   R0,FC220                                                         
*                                                                               
FC300    LA    R2,IOA                                                           
         LA    RE,5                                                             
FC320    STC   RE,HALF+1                                                        
         CLI   0(R2),0                                                          
         BE    FCDX                                                             
         CLI   0(R2),4                                                          
         BH    FCDR                                                             
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         CLC   12(2,R2),=C'*N'                                                  
         BNE   *+12                                                             
         NI    NUCMLFL2,X'FE'                                                   
         B     FC680                                                            
         CLC   12(2,R2),=C'**'     NO NATIONAL                                  
         BNE   FC350                                                            
         OI    NUCMLFL2,X'01'      SET NO NATIONAL BIT                          
         B     FC680                                                            
         DROP  RE                                                               
*                                                                               
FC350    XC    KEY,KEY             VALIDATE FEED CODE                           
         LA    R3,KEY                                                           
         USING FEEDRECD,R3                                                      
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,AGYMED                                                   
         MVC   FEEDKNET,NET                                                     
         MVC   FEEDKFD,12(R2)                                                   
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   FEEDKEY,KEYSAVE     TEST IF CODE FOUND                           
         BE    FC400               YES                                          
*                                                                               
         XC    FEEDKEY,FEEDKEY     CHECK FOR A CLIENT FEED CODE                 
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,AGYMED                                                   
         MVC   FEEDKNET,NET                                                     
         MVC   FEEDKCLT,CLIPK                                                   
         MVC   FEEDKFD,12(R2)                                                   
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   FEEDKEY,KEYSAVE                                                  
         BE    FC400                                                            
         MVI   UNERROR,FEEDCERR                                                 
         B     FCDX                                                             
         SPACE 1                                                                
FC400    CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    FCDX                YES-ALL DONE                                 
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         ZIC   RE,HALF+1                                                        
         LA    RF,5                                                             
         CR    RE,RF                                                            
         BNE   FC600               NOT 1ST TIME MULT 23 ELEMS                   
         TM    NBUNST3,X'40'       IF BUY A COPY SPLIT                          
         BNZ   FC600               CREATE 23 ELEMENTS                           
         CLI   32(R2),0                                                         
         BNE   FC600                                                            
         USING NUFEDEL,R3                                                       
         MVI   NUFEDEID,X'22'      BUILD A FEED CODE ELEMENT                    
         MVI   NUFEDLEN,NUFEDELN                                                
         MVC   NUFEEDCD,12(R2)     FEED CODE                                    
         BAS   RE,PUTEL                                                         
         B     FCDX                                                             
         SPACE 1                                                                
FC600    LA    RE,27(R4)                                                        
FC620    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    FC660                                                            
         CLI   0(RE),X'23'                                                      
         BNE   FC620                                                            
         CLC   NUFDCFED-NUFDCEL(4,RE),12(R2)                                    
         BNE   FC620                                                            
         NI    NUFDCFL2-NUFDCEL(RE),X'7F'                                       
         OI    NUFDCFL2-NUFDCEL(RE),X'01'                                       
         MVC   NUFDCPRD-NUFDCEL(1,RE),BYTE       PRODUCT CODE                   
         B     FC680                                                            
         USING NUFDCEL,R3                                                       
FC660    MVI   NUFDCEID,X'23'      BUILD A FEED WITH COMML ELEMENT              
         MVI   NUFDCLEN,NUFDCELN                                                
         MVC   NUFDCFED,12(R2)     FEED CODE                                    
         MVC   NUFDCPRD,BYTE       PRODUCT CODE                                 
         MVI   NUFDPPOS,0          CLEAR PRODUCT POSITION FIELD                 
         TM    NBUNST3,X'40'       IF BUY A COPY SPLIT                          
         BNZ   *+8                 DONT SET FLAG                                
         OI    NUFDCFL2,X'01'                                                   
         XC    SPACES(2),SPACES    ELEMENT IS 66 (WORK 64 SPACES FOLLOW         
         BAS   RE,PUTEL                                                         
         MVC   SPACES(2),SPACES+2  RESET SPACES                                 
*                                                                               
FC680    LA    R2,32(R2)                                                        
         ZIC   RE,HALF+1                                                        
         BCT   RE,FC320                                                         
         B     FCDX                                                             
         SPACE 1                                                                
FCDR     MVI   UNERROR,INVERR                                                   
         SPACE 1                                                                
FCDX     B     UNX                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ROTATION                                                                      
*                                                                               
ROT      NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    ROT4                                                             
         USING NUSDRD,R3                                                        
         L     R2,APROGEL                                                       
         USING NPGEL92,R2                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         MVI   NUSDROT,0                                                        
         CLI   FLDH+5,1                                                         
         BNE   ROT2                                                             
         CLI   FLD,C'*'                                                         
         BE    ROTX                                                             
*                                                                               
         SPACE                                                                  
ROT2     CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    ROT3                YES                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   ROT4                YES                                          
         CLI   COMMAND,C'E'                                                     
         BNE   ROTX                                                             
         CLI   UNACTSW,C'A'                                                     
         BNE   ROTX                                                             
         SPACE                                                                  
ROT3     MVC   NUSDROT,NPGROT      MOVE ROTATION FROM PROGRAM RECORD            
         B     ROTX                                                             
         SPACE                                                                  
ROT4     ZIC   R0,FLDH+5           FIELD LENGTH                                 
         GOTO1 VDAYVAL,DMCB,((R0),FLD),HALF,HALF+1                              
         OC    HALF(1),HALF        ZERO RETURN ERROR                            
         BZ    ROTERR                                                           
         CLI   COMMAND,C'S'                                                     
         BE    ROTX                                                             
         MVC   BYTE,NPGDAY                                                      
         OC    BYTE,HALF                                                        
         CLC   BYTE,NPGDAY                                                      
         BNE   ROTCVERR                                                         
         MVC   NUSDROT,HALF                                                     
         B     ROTX                                                             
*                                                                               
         SPACE                                                                  
ROTCVERR MVI   UNERROR,COVERR      COVER ERROR                                  
         B     UNX                                                              
ROTERR   MVI   UNERROR,INVERR      INVALID INPUT                                
ROTX     B     UNX                                                              
         DROP  R3,R2                                                            
         EJECT                                                                  
* FEED MARKET GROUP                                                             
*                                                                               
*MG      ST    RE,SAVEREG1                                                      
*        XC    NUFEEDMG,NUFEEDMG                                                
*        CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
*        BE    FMG1                                                             
*        CLI   FLDH+5,0            TEST FOR INPUT                               
*        BNE   FMG2                                                             
*        CLI   COMMAND,C'E'        TEST FOR EDIT                                
*        BNE   FMGX                                                             
*        CLI   UNACTSW,C'A'        GET DEFAULT WHEN ADDING                      
*        BNE   FMGX                                                             
*        SPACE                                                                  
*MG1     L     RE,APACKREC                                                      
*        USING NPRECD,RE                                                        
*        MVC   NUFEEDMG,NPAKFMG    DEFAULT FROM PACKAGE RECORD                  
*        B     FMGX                                                             
*        SPACE                                                                  
*MG2     CLI   FLDH+5,3                                                         
*        BNE   FMGR                                                             
*        CLI   FLD,C'G'                                                         
*        BL    FMGR                                                             
*        CLI   FLD,C'K'                                                         
*        BH    FMGR                                                             
*                                                                               
*        XC    KEY,KEY                                                          
*        LA    R3,KEY                                                           
*        USING MKGRECD,R3                                                       
*        MVC   MKGKTYP,=X'0D02'                                                 
*        MVC   MKGKAGMD,AGYMED                                                  
*        MVC   MKGKMID,FLD                                                      
*        PACK  DUB(2),FLD+1(3)                                                  
*        MVC   MKGKMGRP(1),DUB     LEFT ALIGNED 2-DIGIT PWO                     
*        GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
*        CLC   KEY(L'MKGKEY),KEYSAVE                                            
*        BE    *+12                                                             
*        MVI   UNERROR,NOTFOUND                                                 
*        B     FMGX                                                             
*                                                                               
*        MVC   NUFEEDMG,MKGKMID                                                 
*        B     FMGX                                                             
*        DROP  R3                                                               
*        SPACE 1                                                                
*MGR     MVI   UNERROR,INVERR                                                   
*        SPACE                                                                  
*MGX     L     RE,SAVEREG1                                                      
*        BR    RE                                                               
*        DROP  RE                                                               
*        EJECT                                                                  
* HUT ADJUSTMENT PERCENTAGE                                                     
*                                                                               
HUTA     NTR1                                                                   
         CLI   COMMAND,C'S'                                                     
         BE    HUT100                                                           
         XC    NUHUTPCT,NUHUTPCT                                                
         L     RF,APROGEL                                                       
         TM    NPGSTAT-NPGELEM(RF),X'80'                                        
         BO    HUT100                                                           
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUHUTPCT,NPAKHPCT                                                
         DROP  RE                                                               
HUT100   CLI   FLDH+5,0                                                         
         BE    HUTAX                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BNE   *+12                                                             
         MVI   UNERROR,INVERR                                                   
         B     HUTAX                                                            
*                                                                               
         CLI   COMMAND,C'S'                                                     
         BE    HUTAX                                                            
         MVC   NUHUTPCT,6(R1)                                                   
         SPACE                                                                  
HUTAX    B     UNX                                                              
         EJECT                                                                  
* SPECIAL REP                                                                   
*                                                                               
SREP     NTR1                                                                   
         GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),(R9),RR=MYRELO                     
         B     UNX                                                              
         EJECT                                                                  
* HUT OVERRIDE                                                                  
*                                                                               
HUT      ST    RE,SAVEREG1                                                      
         CLI   FLDH+5,0                                                         
         BE    HUTX                                                             
*                                                                               
*--SCAN THE HUT FIELD                                                           
         GOTO1 =A(OVFLRTN),DMCB,                                       X        
               (3,DUB),                                                X        
               (RC),                                                   X        
               (R9),                                                   X        
               (C'P',HALF),                                            X        
               RR=MYRELO                                                        
*                                                                               
         CLI   UNERROR,0           TEST FOR ERROR                               
         BNE   HUTX                YES-GET OUT                                  
         CLI   COMMAND,C'S'                                                     
         BE    HUTX                                                             
         MVC   UNHUT,HALF                                                       
         SPACE 1                                                                
HUTX     L     RE,SAVEREG1                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SHARE OVERRIDE                                                                
*                                                                               
SHR      ST    RE,SAVEREG1                                                      
         CLI   FLDH+5,0                                                         
         BE    SHRX                                                             
*                                                                               
*--SCAN THE SHARE FIELD                                                         
         GOTO1 =A(OVFLRTN),DMCB,                                       X        
               (3,DUB),                                                X        
               (RC),                                                   X        
               (R9),                                                   X        
               (C'S',HALF),                                            X        
               RR=MYRELO                                                        
*                                                                               
         CLI   UNERROR,0           TEST FOR ERROR                               
         BNE   SHRX                YES-GET OUT                                  
         CLI   COMMAND,C'S'                                                     
         BE    SHRX                                                             
         MVC   UNSHR,HALF                                                       
         SPACE 1                                                                
SHRX     L     RE,SAVEREG1                                                      
         BR    RE                                                               
         SPACE 2                                                                
* RATING OVERRIDE                                                               
*                                                                               
RAT      ST    RE,SAVEREG1                                                      
         CLI   FLDH+5,0                                                         
         BE    RT100                                                            
         SPACE                                                                  
         CLI   FLD,C'X'                                                         
         BNE   RT120                                                            
         CLI   FLDH+5,1                                                         
         BNE   RATERR                                                           
RT100    MVI   HALF,X'FF'          RESET OVERRIDE WITH REAL VALUES              
         B     RT200                                                            
         SPACE                                                                  
*                                                                               
*--SCAN THE SHARE FIELD                                                         
RT120    GOTO1 =A(OVFLRTN),DMCB,                                       X        
               (3,DUB),                                                X        
               (RC),                                                   X        
               (R9),                                                   X        
               (C'R',HALF),                                            X        
               RR=MYRELO                                                        
*                                                                               
         CLI   UNERROR,0           TEST FOR ERROR                               
         BNE   RATX                YES-GET OUT                                  
RT200    CLI   COMMAND,C'S'                                                     
         BE    RATX                                                             
         MVC   UNRAT,HALF                                                       
         SPACE 1                                                                
RATX     L     RE,SAVEREG1                                                      
         BR    RE                                                               
         SPACE                                                                  
RATERR   MVI   UNERROR,INVERR                                                   
         B     RATX                                                             
         SPACE                                                                  
* OTHER OVERRIDE                                                                
*                                                                               
OTH      NTR1                                                                   
         LA    R3,SPACES+100                                                    
         USING NUOTH,R3                                                         
         MVI   NUOTEL,X'60'                                                     
*                                                                               
         ST    RE,SAVEREG1                                                      
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OT100               YES                                          
*                                                                               
         CLI   UNACTION,B          BUYING SINGLE UNIT                           
         BE    OTO20                                                            
         CLI   UNACTION,BM         BUYING MULTIPLE UNIT                         
         BE    OTO20                                                            
         CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BE    OT100                                                            
         CLI   UNACTION,DM         TEST FOR DISPLAY MULTIPLE                    
         BE    OT100                                                            
         MVI   ELCODE,X'60'        DELETE OTHER INFO ELEMENT                    
         BAS   RE,DELEL                                                         
*                                                                               
*--CHECK FOR DEFAULT ZONE FROM PACKAGE RECORD                                   
OTO20    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         CLI   NPAKZONE,0                                                       
         BE    OT050                                                            
         MVI   NUOTLEN,4                                                        
         MVI   NUOTTYP,C'Z'                                                     
         MVC   NUOTHER(1),NPAKZONE                                              
         BAS   RE,PUTEL                                                         
         DROP  RE                                                               
*                                                                               
*--CHECK FOR DEFAULT BUY TYPE FROM PACKAGE RECORD                               
OT050    DS    0H                                                               
         CLI   UNACTION,C          ONLY ON ACTIONS B & BM                       
         BE    OT075                                                            
*                                                                               
         L     RE,APACKREC                                                      
         USING NPK2D,RE                                                         
*                                                                               
         LA    RE,87(RE)           POINT TO ELEM AFTER X'01'                    
         CLI   0(RE),X'02'         IS IT AN '02' ELEMENT?                       
         BNE   OT075                                                            
*                                                                               
         OC    NPK2BTYP,NPK2BTYP   IF NO BUY TYPE, DON'T ADD ELEM               
         BZ    OT075                                                            
*                                                                               
         MVI   NUOTLEN,4                                                        
         MVI   NUOTTYP,C'F'        BUY TYPE                                     
         MVC   NUOTHER(1),NPK2BTYP                                              
         BAS   RE,PUTEL                                                         
         DROP  RE                                                               
*                                                                               
OT075    L     RE,APACKREC                                                      
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    OTHX                YES                                          
*                                                                               
OT100    CLI   FLDH+5,0                                                         
         BE    OTH996                                                           
         XC    DMCB2,DMCB2                                                      
         XC    IOA(256),IOA                                                     
         GOTO1 VSCANNER,DMCB2,(25,FLDH),(5,IOA)                                 
         LA    R2,IOA                                                           
         MVI   NUOTEL,X'60'                                                     
         SPACE                                                                  
*                                                                               
OT120    XC    1(30,R3),1(R3)      CLEAR EVERYTHING BUT ELEM CODE               
         ZIC   RE,0(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,AUTHCOMP                                                      
         BE    OT150                                                            
         EX    RE,REASCOMP                                                      
         BE    OT180                                                            
         EX    RE,ZONECOMP                                                      
         BE    OT200                                                            
         EX    RE,PROMCOMP                                                      
         BE    OT240                                                            
         EX    RE,POSICOMP                                                      
         BE    OT280                                                            
         EX    RE,ECOSCOMP                                                      
         BE    OT320                                                            
         EX    RE,DYPTCOMP                                                      
         BE    OT380                                                            
         EX    RE,PIGBCOMP                                                      
         BE    OT460                                                            
         EX    RE,WNDWCOMP                                                      
         BE    OT500                                                            
         EX    RE,CMTCOMP                                                       
         BE    OT600                                                            
         EX    RE,CAGYCOMP                                                      
         BE    OT630                                                            
         EX    RE,TAGYCOMP                                                      
         BE    OT660                                                            
         EX    RE,SOFCOMP                                                       
         BE    OT690                                                            
         EX    RE,BTYPCOMP                                                      
         BE    OT710                                                            
         EX    RE,TSUPCOMP                                                      
         BE    OT730                                                            
         EX    RE,EFTCOMP                                                       
         BE    OT750                                                            
         EX    RE,AFTCOMP                                                       
         BE    OT750                                                            
         EX    RE,MRNCOMP                                                       
         BE    OT780                                                            
         B     OTHERR                                                           
*                                                                               
OT150    MVI   NUOTTYP,C'A'                                                     
         CLI   1(R2),8                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'A'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT180    MVI   NUOTTYP,C'R'                                                     
         CLI   1(R2),16                                                         
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'R'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT200    MVI   NUOTTYP,C'Z'                                                     
         CLI   1(R2),1                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   22(R2),C'^'                                                      
         BE    OT210                                                            
         CLI   22(R2),C'C'                                                      
         BE    OT210                                                            
         CLI   22(R2),C'M'                                                      
         BE    OT210                                                            
         CLI   22(R2),C'P'                                                      
         BNE   OTHERR                                                           
*                                                                               
OT210    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'Z'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT240    MVI   NUOTTYP,C'P'                                                     
         CLI   22(R2),C'^'                                                      
         BE    OT250                                                            
         CLI   1(R2),4                                                          
         BNE   OTHERR                                                           
*                                                                               
OT250    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'P'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT280    MVI   NUOTTYP,C'O'                                                     
         CLI   1(R2),3                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'O'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT320    MVI   NUOTTYP,C'E'                                                     
         CLI   1(R2),9                                                          
         BH    OTHERR                                                           
*--CHECK FOR NUMERIC                                                            
         CLI   22(R2),C'^'                                                      
         BE    OT350                                                            
         TM    3(R2),X'80'                                                      
         BZ    OTHERR                                                           
*                                                                               
OT350    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'E'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT380    MVI   NUOTTYP,C'D'                                                     
         CLI   1(R2),8                                                          
         BH    OTHERR                                                           
*-CHECK DAYPART LITERAL                                                         
         CLI   22(R2),C'^'                                                      
         BE    OT430                                                            
         LA    RE,DPTTAB           POINT RE AT DAYPART TABLE                    
         LA    R0,DAYPARTS         COUNTER                                      
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1               ONE BYTE INPUT                               
         BZ    OT405               CHECK THE CODE                               
OT400    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R2),1(RE)        TEST INPUT AGAINST TABLE                   
         BE    OT420                                                            
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R0,OT400                                                         
         B     OTHERR                                                           
*-CHECK DAYPART CODE                                                            
OT405    LA    RE,DPTTAB           POINT RE AT DAYPART TABLE                    
         LA    R0,DAYPARTS         COUNTER                                      
OT410    CLC   22(1,R2),0(RE)        TEST INPUT AGAINST TABLE                   
         BE    OT420                                                            
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R0,OT410                                                         
         B     OTHERR                                                           
*                                                                               
OT420    MVC   1(1,R2),9(RE)                                                    
         MVC   22(9,R2),0(RE)                                                   
OT430    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'D'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT460    MVI   NUOTTYP,C'G'                                                     
         CLI   1(R2),1                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   22(R2),C'^'                                                      
         BE    OT470                                                            
         CLI   22(R2),C'Y'                                                      
         BE    OT470                                                            
         CLI   22(R2),C'N'                                                      
         BNE   OTHERR                                                           
*                                                                               
OT470    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'G'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK WINDOW STATUS                                                          
OT500    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OT550               YES                                          
         L     RE,APROGREC                                                      
         LA    RE,24(RE)                                                        
OT510    CLI   0(RE),0                                                          
         BE    OTHERR                                                           
         CLI   0(RE),3                                                          
         BE    OT520                                                            
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     OT510                                                            
OT520    TM    20(RE),X'80'        SEE IF WINDOW SET IN PROGRAM RECORD          
         BZ    OTHERR                                                           
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         OI    2(RE),X'08'                                                      
         SPACE                                                                  
*                                                                               
         MVI   NUOTTYP,C'W'                                                     
         MVI   NUOTLEN,5                                                        
OT550    GOTO1 VDATVAL,DMCB2,(0,22(R2)),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    OTHERR                                                           
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         GOTO1 VDATCON,DMCB2,(0,DUB),(2,NUOTHER)                                
         MVI   BYTE,C'W'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH993                                                           
*                                                                               
*--CHECK COMMENT                                                                
OT600    MVI   NUOTTYP,C'C'                                                     
         CLI   1(R2),24                                                         
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'C'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK COMMISION AGENCY                                                       
OT630    MVI   NUOTTYP,C'B'                                                     
         CLI   1(R2),4                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'B'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK TRAFFIC AGENCY                                                         
OT660    MVI   NUOTTYP,C'T'                                                     
         CLI   1(R2),4                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'T'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK SOURCE OF FUNDS                                                        
OT690    MVI   NUOTTYP,C'S'                                                     
         CLI   1(R2),10                                                         
         BH    OTHERR                                                           
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'S'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*--CHECK BUY TYPE                                                               
*                                                                               
OT710    MVI   NUOTTYP,C'F'                                                     
         CLI   1(R2),1                                                          
         BH    OTHERR                                                           
*                                                                               
         CLI   22(R2),C'S'         SCATTER                                      
         BE    OT720                                                            
         CLI   22(R2),C'O'         OPPRTUNISTIC                                 
         BE    OT720                                                            
         CLI   22(R2),C'U'         UPFRONT                                      
         BNE   OTHERR                                                           
*                                                                               
OT720    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'F'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK TRAFFIC SUPPLIER                                                       
OT730    MVI   NUOTTYP,C'H'                                                     
         CLI   1(R2),5                                                          
         BH    OTHERR                                                           
*                                                                               
*  SEE IF TRAFFIC SUPPLIER EXISTS                                               
         XC    KEY,KEY             NOW CHECK IF RECORD IS THERE                 
         LA    RE,KEY                                                           
         USING TSUPNRECD,RE                                                     
         MVI   TSUPKID,X'27'                                                    
         MVC   TSUPKAM,AGYMED                                                   
         MVC   TSUPKTS,22(R2)     TRAFFIC SUPPLIER                              
         GOTO1 AIO,DMCB2,UNT+DIR+HIGH                                           
         CLC   KEY(19),KEYSAVE                                                  
         BNE   OTHERR                   YES                                     
         DROP  RE                                                               
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'H'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*--CHECK TVQ BOOK                                                               
OT750    MVI   NUOTTYP,C'J'        ESTIMATE BOOK CODE                           
         CLI   12(R2),C'A'         CHECK ACTUAL BOOK                            
         BNE   *+8                                                              
         MVI   NUOTTYP,C'K'        ACTUAL BOOK CODE                             
*                                                                               
         CLI   22(R2),C'^'                                                      
         BE    OT760                                                            
         GOTO1 VDATVAL,DMCB2,(0,22(R2)),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    OTHERR                                                           
*                                                                               
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
         MVI   1(R2),4                                                          
         MVC   22(1,R2),4(R1)                                                   
         MVC   23(1,R2),8(R1)                                                   
         GOTO1 VDATCON,(R1),(0,DUB),(2,24(R2))                                  
*                                                                               
OT760    CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'J'           ESTIMATE BOOK CODE                           
         CLI   12(R2),C'A'         CHECK ACTUAL BOOK                            
         BNE   *+8                                                              
         MVI   BYTE,C'K'           ACTUAL BOOK CODE                             
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
OT780    MVI   NUOTTYP,C'L'                                                     
         CLI   1(R2),2                                                          
         BH    OTHERR                                                           
*--CHECK FOR NUMERIC                                                            
         TM    3(R2),X'80'                                                      
         BZ    OTHERR                                                           
         CLI   11(R2),2                                                         
         BL    OTHERR                                                           
         CLI   11(R2),15                                                        
         BH    OTHERR                                                           
         MVI   1(R2),1                                                          
         MVC   22(1,R2),11(R2)     MOVE BINARY VALUE TO ELEMENT                 
*                                                                               
         CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OTH996              YES                                          
         MVI   BYTE,C'L'                                                        
         BAS   RE,DELOTH                                                        
         B     OTH990                                                           
*                                                                               
*                                                                               
*--COMMON CREATE ELEMENT AREA                                                   
*                                                                               
OTH990   CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BNE   *+12                                                             
         CLI   22(R2),C'^'         DELETE SYMBOL                                
         BE    OTHX                                                             
         ZIC   RE,1(R2)                                                         
         CLI   1(R2),0                                                          
         BE    OTHERR                                                           
         BCTR  RE,0                                                             
         EX    RE,OTHMOVE                                                       
         LA    RE,4(RE)                                                         
         STC   RE,NUOTLEN                                                       
OTH993   BAS   RE,PUTEL                                                         
         CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BE    OTHX                                                             
         CLI   UNACTION,DM         TEST FOR DISPLAY MULTIPLE                    
         BE    OTHX                                                             
         B     OTH996                                                           
*                                                                               
OTHMOVE  MVC   NUOTHER(0),22(R2)                                                
         SPACE 1                                                                
OTH996   LA    R2,47(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   OT120                                                            
*                                                                               
OTHX     MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         GOTO1 VBLDTVQ,DMCB,AIOAREA1                                            
         B     UNX                                                              
         SPACE                                                                  
OTHERR   MVI   UNERROR,INVERR                                                   
         B     OTHX                                                             
*                                                                               
DELOTH   LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(X'60',(R4)),(1,BYTE)                
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TAGYCOMP CLC   12(0,R2),=CL2'TA'                                                
CAGYCOMP CLC   12(0,R2),=CL2'CA'                                                
SOFCOMP  CLC   12(0,R2),=CL2'SF'                                                
ZONECOMP CLC   12(0,R2),=CL4'ZONE'                                              
AUTHCOMP CLC   12(0,R2),=CL4'AUTH'                                              
REASCOMP CLC   12(0,R2),=CL6'REASON'                                            
PROMCOMP CLC   12(0,R2),=CL5'PROMO'                                             
POSICOMP CLC   12(0,R2),=CL8'POSITION'                                          
ECOSCOMP CLC   12(0,R2),=CL5'ECOST'                                             
DYPTCOMP CLC   12(0,R2),=CL2'DP'                                                
PIGBCOMP CLC   12(0,R2),=CL2'PB'                                                
WNDWCOMP CLC   12(0,R2),=CL6'WINDOW'                                            
CMTCOMP  CLC   12(0,R2),=CL3'CMT'                                               
MIRRCOMP CLC   12(0,R2),=CL6'MIRROR'                                            
BTYPCOMP CLC   12(0,R2),=CL5'BTYPE'                                             
TSUPCOMP CLC   12(0,R2),=CL5'TSUPP'                                             
EFTCOMP  CLC   12(0,R2),=CL5'EFT'                                               
AFTCOMP  CLC   12(0,R2),=CL5'AFT'                                               
MRNCOMP  CLC   12(0,R2),=CL5'MR'                                                
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO CHECK THE TRAFFIC ELEMENTS TO SEE IF THE                           
* STATUS BITS SHOULD BE CHANGED                                                 
*                                                                               
CHKTRA   NTR1                                                                   
         CLI   BUYPROF2+12,C'Y'                                                 
         BNE   *+8                                                              
         BAS   RE,CLRBLBD          CLEAR BILLBOARD INFORMATION                  
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKTRBEX                                                         
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OC    NUCML1(16),NUCML1                                                
         BNZ   CHKTRGEX                                                         
         OC    NUCMLBSN(16),NUCMLBSN                                            
         BNZ   CHKTRGEX                                                         
*                                                                               
CHKTRBEX CR    RB,RE               NOT EQUAL CONDITION                          
         B     CHKTREX                                                          
*                                                                               
CHKTRGEX CR    RB,RB               EQUAL CONDITION                              
CHKTREX  XIT1                                                                   
         DROP  RE                                                               
         SPACE 3                                                                
*-- CLEAR THE BILLBOARD INFORMATION IN THE TRAFFIC                              
*-- ELEMENTS (21,23).                                                           
CLRBLBD  NTR1                                                                   
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   BILBD100                                                         
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         XC    NUCMLBSL(17),NUCMLBSL                                            
         DROP  RE                                                               
*                                                                               
BILBD100 MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   BILBDEX                                                          
         L     RE,12(R1)                                                        
         USING NUFDCEL,RE                                                       
         XC    NUFDCBSL(17),NUFDCBSL                                            
*                                                                               
BILBDEX  B     UNX                                                              
         EJECT                                                                  
*--CALL TO DEMO ROUTINE                                                         
DEMO     GOTO1 =A(DEMORTN),RR=MYRELO                                            
         B     UNX                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO DELETE ELEMENT (ELEMENT CODE IN ELCODE)                        
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(ELCODE,(R4)),0                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PUT AN ELEMENT (R3 POINTS TO ELEMENT)                          
*                                                                               
PUTEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'P',UNTFILE),(ELCODE,(R4)),(R3),0                 
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BER   RE                                                               
         L     RD,MYAWORK                                                       
         MVI   UNERROR,TOOLARGE                                                 
         CLI   12(R1),5                                                         
         BE    UNX                 UNWIND OUT OF DEMO CODE                      
         DC    H'0'                DUMP FOR OTHER ERRORS                        
         SPACE 2                                                                
* SUB-ROUTINE TO GET ELEMENT (AT ENTRY, ELCODE CONTAINS ELEMENT CODE)           
* ON EXIT, CC=EQ IF ELEMENT FOUND, CC=NEQ IF NOT FOUND                          
*                                                                               
GETEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(ELCODE,(R4)),0                      
         CLI   12(R1),0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFIL'                                                      
*--TABLES                                                                       
* TABLE OF DAY BITS AND THEIR DAY CODE VALUES FOR PASSIVE POINTER               
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    X'4001'             MONDAY                                       
         DC    X'2002'             TUESDAY                                      
         DC    X'1003'             WED                                          
         DC    X'0804'             THURS                                        
         DC    X'0405'             FRI                                          
         DC    X'0206'             SAT                                          
         DC    X'0107'             SUN                                          
         DC    X'7C08'             M-F                                          
         DC    X'7F09'             M-SU                                         
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         SPACE 2                                                                
*                                                                               
* TABLE OF COMMANDS AND THEIR ROUTINES                                          
*                                                                               
COMMTAB  DS    0CL4                                                             
         DC    C'I',AL3(IN)        INITIALIZE                                   
         DC    C'E',AL3(ED)        EDIT SCREEN HEADER                           
         DC    C'N',AL3(DEF)       NO INPUT - GET DEFAULT VALUE                 
         DC    C'X',AL3(ED)        EXTRACTED INPUT - VALIDATE IT                
         DC    C'S',AL3(ED)        SYNTAX EDIT - EXTRACTED INPUT                
         DC    C'D',AL3(DEMO)      ESTIMATED DEMO GENERATION                    
         DC    C'F',AL3(FINAL)     FINAL RECORD PROCESSING                      
         DC    C'P',AL3(POINT)     POINTER GENERATION                           
COMMANDS EQU   (*-COMMTAB)/L'COMMTAB                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  DAYPART TABLE                                                                
DPTTAB   DS    0CL10       BYTE 9 = LENGTH OF ENTRY PLUS 1                      
         DC    C'D',CL8'DAYTIME',XL1'8'                                         
         DC    C'F',CL8'FRINGE',XL1'7'                                          
         DC    C'P',CL8'PRIME',XL1'6'                                           
         DC    C'K',CL8'KIDS',XL1'5'                                            
         DC    C'T',CL8'TEENS',XL1'6'                                           
         DC    C'Y',CL8'YOUTH',XL1'6'                                           
         DC    C'S',CL8'SPORTS',XL1'7'                                          
         DC    C'N',CL8'NEWS',XL1'5'                                            
         DC    C'E',CL8'EARLY',XL1'6'                                           
         DC    C'L',CL8'LATE',XL1'5'                                            
         DC    C'H',CL8'OTHER',XL1'6'                                           
         DC    C'J',CL8'PROMO-ID',XL1'9'                                        
         DC    C'C',CL8'CABLE',XL1'6'                                           
         DC    C'O',CL8'OLYMPICS',XL1'9'                                        
         DC    C'R',CL8'RADIO',XL1'6'                                           
         DC    C'X',CL8'SYND.',XL1'6'                                           
         DC    C'I',CL8'SPECIAL',XL1'8'                                         
         DC    C'V',CL8'OVERNITE',XL1'9'                                        
         DC    C'W',CL8'WKNDPM',XL1'7'                                          
         DC    C'M',CL8'WKNDAM',XL1'7'                                          
         DC    C'A',CL8'ACCESS',XL1'7'                                          
         DC    C'B',CL8'CBLSPORT',XL1'9'                                        
         DC    C'Q',CL8'INTRACTV',XL1'9'                                        
         DC    C'U',CL8'UNWIRED',XL1'8'                                         
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         EJECT                                                                  
* TABLE OF FIELD DATA TYPES AND THEIR ROUTINES                                  
*                                                                               
* BYTE 0    = FIELD DATA TYPE                                                   
* BYTES 1-3 = ROUTINE ADDRESS                                                   
* BYTE 4    = DATA TYPE INDICATORS (I.E. SETS DEFAULT ETC.)                     
*                                                                               
         DS    0F                                                               
TYPTAB   DS    0XL5                                                             
         DC    AL1(UPRD),AL3(PRD),AL1(SETDEF)                                   
         DC    AL1(UP1SHR),AL3(PRDSHR),X'00'                                    
         DC    AL1(UPRGN),AL3(PRGN),AL1(SETDEF)                                 
         DC    AL1(ULEN),AL3(LEN),AL1(SETDEF)                                   
         DC    AL1(UDAY),AL3(DAY),AL1(SETDEF)                                   
         DC    AL1(UTIME),AL3(TIME),AL1(SETDEF)                                 
         DC    AL1(UAFFID),AL3(AFFID),X'00'                                     
         DC    AL1(UASS),AL3(ASS),X'00'                                         
         DC    AL1(UACT),AL3(ACT),X'00'                                         
         DC    AL1(UTCAR),AL3(TCAR),X'00'                                       
         DC    AL1(UINT),AL3(INT),AL1(SETDEF)                                   
         DC    AL1(UFEED),AL3(FEED),AL1(SETDEF)                                 
         DC    AL1(UIMP),AL3(IMP),AL1(SETDEF)                                   
         DC    AL1(UPG),AL3(PG),X'00'                                           
         DC    AL1(UDG),AL3(DG),X'00'                                           
         DC    AL1(UNTI),AL3(NTI),AL1(SETDEF)                                   
         DC    AL1(UNSI),AL3(NSI),AL1(SETDEF)                                   
         DC    AL1(UUNCD),AL3(UNIV),AL1(SETDEF)                                 
         DC    AL1(UUNPC),AL3(UPCT),AL1(SETDEF)                                 
         DC    AL1(UHAVE),AL3(HAVE),AL1(SETDEF)                                 
         DC    AL1(UHSC),AL3(HUTS),AL1(SETDEF)                                  
         DC    AL1(UFEEDCD),AL3(FCD),X'00'                                      
         DC    AL1(UBB),AL3(BB),X'00'                                           
         DC    AL1(UROT),AL3(ROT),AL1(SETDEF)                                   
         DC    AL1(UHUTADJ),AL3(HUTA),X'00'                                     
         DC    AL1(USREP),AL3(SREP),AL1(SETDEF)                                 
         DC    AL1(UHUT),AL3(HUT),X'00'                                         
         DC    AL1(USHR),AL3(SHR),X'00'                                         
         DC    AL1(UPRE),AL3(PRE),X'00'                                         
         DC    AL1(UDHI),AL3(DHI),X'00'                                         
         DC    AL1(USTAT),AL3(STATUS),X'00'                                     
         DC    AL1(URAT),AL3(RAT),X'00'                                         
         DC    AL1(UOTHER),AL3(OTH),AL1(SETDEF)                                 
         DC    X'FF'                                                            
TYPES    EQU   (*-TYPTAB-1)/L'TYPTAB                                            
         EJECT                                                                  
*                                                                               
* TCAR                                                                          
*                                                                               
TCAR     NTR1  BASE=*,LABEL=*                                                   
         L     R4,UNAREC           R4 POINTS TO UNIT RECORD                     
         USING NURECD,R4                                                        
*                                                                               
         CLI   COMMAND,C'S'                                                     
         BE    TCAR10                                                           
*                                                                               
         L     R2,APROGEL                                                       
         USING NPGEL92,R2                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
*                                                                               
TCAR10   DS    0H                                                               
         CLI   FLDH+5,0                                                         
         BE    TCARX                                                            
         CLI   FLDH+5,1                                                         
         BNE   TCARERR                                                          
*                                                                               
         CLI   FLD,C'X'            DELETE IT?                                   
         BE    TCAR20                                                           
*                                                                               
         CLI   FLD,C'1'            MUST BE VALID NUMERIC BET 1 AND 5            
         BE    TCAR20                                                           
         CLI   FLD,C'2'                                                         
         BE    TCAR20                                                           
         CLI   FLD,C'3'                                                         
         BE    TCAR20                                                           
         CLI   FLD,C'4'                                                         
         BE    TCAR20                                                           
         CLI   FLD,C'5'                                                         
         BE    TCAR20                                                           
         B     TCARERR                                                          
*                                                                               
TCAR20   DS    0H                                                               
         CLI   COMMAND,C'S'                                                     
         BE    TCARX                                                            
         XC    NUSTCAR,NUSTCAR                                                  
         CLI   FLD,C'X'            DELETE TCAR                                  
         BE    *+10                                                             
         MVC   NUSTCAR,FLD                                                      
         B     TCARX                                                            
*                                                                               
TCARERR  MVI   UNERROR,INVERR                                                   
TCARX    J     UNX                                                              
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  RA,RB,RC                                                         
OVFLRTN  NMOD1 0,**35OV**                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     R9,8(R1)                                                         
         USING BUYWRKD,R9                                                       
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     OVDHI                                                            
         B     OVNTI                                                            
         B     OVNSI                                                            
         B     OVSCN                                                            
         B     OVSREP                                                           
         B     OVCOST2                                                          
         B     OVUNIV                                                           
         B     OVAFFID                                                          
         B     OVDG                                                             
*        B     OVTVQBKS                                                         
         EJECT                                                                  
* DELETE HOMES IMPRESSION                                                       
*                                                                               
OVDHI    CLI   FLD,C'N'                                                         
         BE    OVDHIX                                                           
         MVI   ELCODE,X'DD'                                                     
         MVC   DUB(4),=XL4'0000E301' T1                                         
         PRINT GEN                                                              
         GOTO1 VHELLO,DMCB,(C'D',OVUNTFIL),(ELCODE,AIOAREA1),(4,DUB)            
         MVC   DUB(4),=XL4'0000C801' H1                                         
         GOTO1 VHELLO,DMCB,(C'D',OVUNTFIL),(ELCODE,AIOAREA1),(4,DUB)            
         PRINT NOGEN                                                            
         MVI   FLD,C'Y'                                                         
OVDHIX   B     OVEXIT                                                           
         EJECT                                                                  
* NTI CODE                                                                      
*                                                                               
OVNTI    CLI   COMMAND,C'S'                                                     
         BE    NTI2                                                             
         XC    NUNTI,NUNTI         CLEAR DATA FIELD                             
         CLI   COMMAND,C'N'        TEST FOR DEFAULT                             
         BE    NTI1                                                             
         CLI   FLDH+5,0                                                         
         BNE   NTI2                                                             
         CLI   COMMAND,C'E'        SET DEFAULT FOR EDIT/ADD                     
         BNE   NTIX                                                             
         CLI   UNACTSW,C'A'                                                     
         BNE   NTIX                                                             
         SPACE 1                                                                
NTI1     L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   NUNTI,NPGPPNO                                                    
         B     NTIX                                                             
         SPACE 1                                                                
NTI2     TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BO    *+12                                                             
         MVI   UNERROR,NUMERR                                                   
         B     NTIX                                                             
         C     R0,=F'65536'        TEST FOR MAXIMUM VALUE                       
         BL    *+12                                                             
         MVI   UNERROR,INVERR                                                   
         B     NTIX                                                             
         CLI   COMMAND,C'S'                                                     
         BE    NTIX                                                             
         STCM  R0,3,NUNTI                                                       
         SPACE                                                                  
NTIX     B     OVEXIT                                                           
         DROP  RE                                                               
         EJECT                                                                  
* NSI CODE                                                                      
*                                                                               
OVNSI    CLI   COMMAND,C'S'                                                     
         BE    NSI4                                                             
         XC    NUNSI,NUNSI         CLEAR DATA FIELD                             
         SPACE 1                                                                
NSI2     CLI   COMMAND,C'N'                                                     
         BE    NSI3                                                             
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   NSI4                YES-GO VALIDATE IT                           
         CLI   COMMAND,C'E'                                                     
         BNE   NSIX                                                             
         CLI   UNACTSW,C'A'                                                     
         BNE   NSIX                                                             
         SPACE 1                                                                
NSI3     L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   NUNSI,NPGNSI        EXTRACT NSI CODE FROM PROGRAM RECORD         
         B     NSIX                                                             
         SPACE                                                                  
NSI4     TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BNO   NSI5                                                             
         LTR   R0,R0                                                            
         BZ    NSIR                                                             
         CLI   COMMAND,C'S'                                                     
         BE    *+8                                                              
         STCM  R0,3,NUNSI                                                       
         B     NSIX                                                             
         SPACE                                                                  
NSI5     CLI   FLD,C'T'            TEST FOR PREFIX OF 'T'                       
         BNE   NSIR                                                             
         CLI   FLDH+5,1                                                         
         BE    NSIR                                                             
         CLI   FLDH+5,5                                                         
         BH    NSIR                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LR    R0,R1               SAVE LENGTH OF NUMBER                        
         LA    R2,FLD+1            POINT TO FIRST CHAR AFTER 'T'                
         SPACE                                                                  
NSI6     CLI   0(R2),C'0'          TEST FOR NUMERIC DATA                        
         BL    NSIR                                                             
         CLI   0(R2),C'9'                                                       
         BH    NSIR                                                             
         LA    R2,1(R2)                                                         
         BCT   R1,NSI6                                                          
*                                                                               
         LR    R1,R0               RESTORE NUMBER LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    NSIR                                                             
         CLI   COMMAND,C'S'                                                     
         BE    NSIX                                                             
         STCM  R0,3,NUNSI                                                       
         OI    NUNSI,X'80'                                                      
         B     NSIX                                                             
         SPACE                                                                  
NSIR     MVI   UNERROR,INVERR                                                   
         B     NSIX                                                             
         SPACE                                                                  
NSIX     B     OVEXIT                                                           
         DROP  RE                                                               
         EJECT                                                                  
* SPECIAL REP                                                                   
*                                                                               
OVSREP   CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    SREP4                                                            
         XC    NUSREP,NUSREP                                                    
         SPACE 1                                                                
SREP2    CLI   COMMAND,C'N'        TEST FOR DEFAULT CALL                        
         BE    SREP3               YES                                          
         CLI   FLDH+5,0                                                         
         BNE   SREP4               FIELD HAS INPUT                              
         CLI   UNACTSW,C'A'        TEST FOR ADDING RECORD                       
         BE    SREP3               YES-GET DEFAULT                              
         OC    NBSREP,NBSREP       TEST IF RECORD HAS A SPECIAL REP             
         BZ    SREPX               NO                                           
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BNE   SREPX               NO                                           
         MVI   UNERROR,REPDERR     YES-ERASING REP IS NOT ALLOWED               
         B     SREPX                                                            
         SPACE 1                                                                
SREP3    L     RE,APACKREC         GET DEFAULT FROM PACKAGE                     
         USING NPRECD,RE                                                        
         MVC   NUSREP,NPAKSREP                                                  
         B     SREPX                                                            
         SPACE 1                                                                
SREP4    TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BO    *+12                                                             
         MVI   UNERROR,NUMERR                                                   
         B     SREPX                                                            
*                                                                               
         LTR   R0,R0               REP MUST BE 1-999                            
         BZ    SREPR                                                            
         CH    R0,=H'999'                                                       
         BH    SREPR                                                            
         CLI   COMMAND,C'S'                                                     
         BE    SREP5                                                            
*                                                                               
         STCM  R0,3,NUSREP                                                      
         CLC   NUSREP,NBSREP       TEST IF REP CHANGED                          
         BE    SREP5               NO                                           
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BNE   SREP5               NO                                           
         MVI   UNERROR,REPCHERR    YES-ERROR                                    
         B     SREPX                                                            
         SPACE 1                                                                
SREP5    OI    DUB+7,X'0F'                                                      
         UNPK  THREE,DUB           CHARACTER REP CODE                           
         B     SREP6                                                            
         SPACE 1                                                                
SREPR    MVI   UNERROR,INVERR                                                   
         B     SREPX                                                            
         SPACE 1                                                                
SREP6    XC    KEY,KEY             NOW VALIDATE THE REP                         
         LA    R3,KEY                                                           
         USING REPRECD,R3                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,THREE                                                    
         MVC   REPKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,IOA                                       
         CLC   KEY(L'REPKEY),KEYSAVE  TEST IF REP FOUND                         
         BE    SREPX                  YES                                       
         MVI   UNERROR,REPERR                                                   
         SPACE 1                                                                
SREPX    B     OVEXIT                                                           
         DROP  R3,RE                                                            
         EJECT                                                                  
* IF SECOND COST OPTION INPUTTED CALCULATE THE                                  
* SECOND COST AND MOVE INTO THE ASSIGNED COST FIELD                             
*                                                                               
OVCOST2  CLI   COMMAND,C'S'        TEST FOR SYNTAX EDIT                         
         BE    OVEXIT                                                           
         MVC   FULL,CLICOST2       MOVE CLIENT PCT.                             
         OC    ESTCOST2,ESTCOST2   WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    *+10                                                             
         MVC   FULL,ESTCOST2                                                    
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    OVEXIT              NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    OV2CS50              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     OVEXIT                                                           
*                                                                               
OV2CS50  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   OVEXIT                                                           
         OI    NUUNITST,X'08'                                                   
         B     OVEXIT                                                           
         EJECT                                                                  
* ROUTINE TO SCAN HUT, SHARE, AND RATING FIELDS (RETURNS OUTPUT                 
* TO ONE DECIMAL PLACE IN R0)                                                   
*                                                                               
OVSCN    MVC   BYTE,12(R1)         DEMO MODIFIER S,R,P                          
         L     R4,12(R1)           ADDRESS OF OUTPUT                            
*                                                                               
         ZIC   R1,FLDH+5           R1 CONTAINS FIELD LENGTH                     
         CLI   FLD,STAR            STRIP LEADING ASTERISK BEFORE SCAN           
         BNE   SCAN2                                                            
*                                                                               
         SH    R1,=H'1'                                                         
         BZ    SCANR                                                            
         STC   R1,FLDH+5           ADJUST DATA LENGTH FOR STAR                  
         BCTR  R1,0                                                             
         EX    R1,FLDMOVE                                                       
         LA    R1,1(R1)                                                         
         EX    R1,DUBMOVE                                                       
         SPACE 1                                                                
SCAN2    SR    R0,R0               CLEAR R0 FOR NEW VALUE                       
         LA    RE,FLD              RE POINTS TO START OF FIELD                  
         LR    R2,R1               SAVE TOTAL LENGTH IN R2                      
         SPACE 1                                                                
SCAN3    CLI   0(RE),PERIOD        TEST FOR DECIMAL POINT                       
         BE    SCAN4                                                            
         CLI   0(RE),C'0'          VALIDATE FOR NUMERIC DATA                    
         BL    SCANR                                                            
         CLI   0(RE),C'9'                                                       
         BH    SCANR                                                            
         LA    RE,1(RE)            NEXT BYTE                                    
         BCT   R1,SCAN3                                                         
*--CHECK PRECISION FACTOR                                                       
SCAN4    LA    R3,DEMPREC                                                       
         LA    RF,7                                                             
SCAN5    CLC   BYTE(1),0(R3)                                                    
         BE    SCAN7                                                            
         LA    R3,2(R3)                                                         
         BCT   RF,SCAN5                                                         
         DC    H'0'                                                             
SCAN7    TM    1(R3),X'82'                                                      
         BO    SCAN20                                                           
         SPACE 1                                                                
*--NUMBER TO TENTHS PLACE EDIT                                                  
         LR    R3,R2               LENGTH OF FIELD                              
         SR    R3,R1               LENGTH OF DATA SO FAR                        
         BZ    SCAN8               NOTHING BEFORE POINT                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    SCANR                                                            
         MH    R0,=H'10'           SCALE RATING TO TENTHS                       
         LTR   R1,R1               TEST IF WHOLE FIELD EDITED                   
         BZ    SCAN10              YES                                          
         SPACE 1                                                                
SCAN8    CH    R1,=H'2'            ONLY CAN BE POINT AND DIGIT                  
         BNE   SCANR                                                            
         CLI   1(RE),C'0'                                                       
         BL    SCANR                                                            
         CLI   1(RE),C'9'                                                       
         BH    SCANR                                                            
         NI    1(RE),X'0F'         TURN OFF ZONE BITS                           
         ZIC   RF,1(RE)                                                         
         AR    R0,RF                                                            
         SPACE                                                                  
SCAN10   B     SCANX                                                            
         SPACE 1                                                                
*--NUMBER TO HUNDREDS PLACE EDIT                                                
SCAN20   LR    R3,R2               LENGTH OF FIELD                              
         SR    R3,R1               LENGTH OF DATA SO FAR                        
         BZ    SCAN26              NOTHING BEFORE POINT                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    SCANR                                                            
         MH    R0,=H'100'          SCALE RATING TO TENTHS                       
         LTR   R1,R1               TEST IF WHOLE FIELD EDITED                   
         BZ    SCAN28              YES                                          
         SPACE 1                                                                
SCAN26   CH    R1,=H'3'            ONLY CAN BE POINT AND 2 DIGITS               
         BH    SCANR                                                            
         BCTR  R1,0                                                             
         LR    R3,R1                                                            
         BCTR  R3,0                                                             
         XC    DUB,DUB                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,RE)                                                      
         CVB   R3,DUB                                                           
         CH    R1,=H'2'            IF ONLY ONE DIGIT AFTER DECIMAL              
         BE    SCAN27              THEN MULTIPLY THE DIGIT BY TEN               
         MH    R3,=H'10'           TO PUT INTO CORRECT PRECISION                
*                                                                               
SCAN27   CLI   1(RE),C'0'          VALIDATE FOR NUMERIC DATA                    
         BL    SCANR                                                            
         CLI   1(RE),C'9'                                                       
         BH    SCANR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,SCAN27                                                        
*                                                                               
         AR    R0,R3                                                            
         SPACE                                                                  
SCAN28   B     SCANX                                                            
         SPACE 1                                                                
SCANR    MVI   UNERROR,INVERR                                                   
         SPACE                                                                  
SCANX    STH   R0,0(R4)            SAVE OUTPUT                                  
         B     OVEXIT                                                           
         SPACE 2                                                                
FLDMOVE  MVC   DUB(0),FLD+1                                                     
DUBMOVE  MVC   FLD(0),DUB                                                       
         EJECT                                                                  
* UNIVERSE CODE                                                                 
*                                                                               
OVUNIV   CLI   COMMAND,C'S'                                                     
         BE    UNIV4                                                            
         XC    NUUNCODE,NUUNCODE   CLEAR DATA FIELD                             
         SPACE 1                                                                
UNIV2    CLI   COMMAND,C'N'        TEST FOR GET DEFAULT                         
         BE    UNIV3               YES                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   UNIV4               YES-THERE IS SOME                            
         CLI   COMMAND,C'E'        TEST FOR EDIT                                
         BNE   UNIVX               NO-ALL DONE                                  
         CLI   UNACTSW,C'A'        TEST FOR ACTION ADD                          
         BNE   UNIVX               NO-EXIT                                      
         SPACE 1                                                                
UNIV3    L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUUNCODE,NPAKUNCD   EXTRACT IT FROM PACKAGE RECORD               
         B     UNIVX               EXIT                                         
         SPACE                                                                  
UNIV4    TM    FLDH+4,X'08'        VALIDATE ACTUAL OR EXTRACTED INPUT           
         BO    *+12                                                             
         MVI   UNERROR,NUMERR                                                   
         B     UNIVX                                                            
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         CLI   COMMAND,C'S'                                                     
         BE    *+10                                                             
         MVC   NUUNCODE,DUB+5      TO ISOLATE UNIVERSE CODE                     
         SPACE 1                                                                
UNIV6    XC    KEY,KEY             NOW CHECK IF RECORD IS THERE                 
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         MVC   NUNKTYP,=X'0D22'                                                 
         MVC   NUNKAGY,AGYALPH                                                  
         MVI   NUNKTYPE,1          UNIVERSE CODE                                
         MVC   NUNKCODE,DUB+5                                                   
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(L'NUNKEY),KEYSAVE    TEST IF FOUND                           
         BE    UNIVX                    YES                                     
         MVI   UNERROR,UNIVERR     NO-SET ERROR                                 
         SPACE                                                                  
UNIVX    B     OVEXIT                                                           
         DROP  R3,RE                                                            
         EJECT                                                                  
* AFFIDAVIT TIME                                                                
*                                                                               
OVAFFID  CLI   COMMAND,C'S'                                                     
         BE    AFFID2                                                           
         XC    NUAFFTIM,NUAFFTIM                                                
         SPACE 1                                                                
AFFID2   CLI   FLDH+5,0                                                         
         BE    AFFIDX                                                           
         CLI   FLDH+5,1                                                         
         BNE   AFFID3                                                           
         CLI   FLD,C'*'                                                         
         BE    AFFIDX                                                           
AFFID3   ZIC   R0,FLDH+5                                                        
         GOTO1 VTIMVAL,DMCB2,((R0),FLD),FULL                                    
         CLI   DMCB2,X'FF'                                                      
         BE    AFFIDR                                                           
         CLI   COMMAND,C'S'                                                     
         BE    *+10                                                             
         MVC   NUAFFTIM,FULL       SET AFFIDAVIT TIME                           
         CLC   FULL,=C'NONE'       SCREEN OUT SPECIAL CHARS.                    
         BE    AFFIDR                                                           
         CLC   FULL,=C'VARY'                                                    
         BNE   AFFIDX                                                           
         SPACE                                                                  
AFFIDR   MVI   UNERROR,TIMERR                                                   
         B     AFFIDX                                                           
         SPACE                                                                  
AFFIDX   B     OVEXIT                                                           
         EJECT                                                                  
* SET DEFAULT TVQ BOOK VALUES                                                   
*                                                                               
OVTVQBKS LA    RE,ESTDEMS                                                       
         ZIC   RF,ESTNDEMS                                                      
*                                                                               
OVTVQ050 CLI   0(RE),171            CHECK TVQ DEMO                              
         BE    OVTVQ100                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,OVTVQ050                                                      
         B     OVTVQEX                                                          
*                                                                               
* CHECK IF ESTIMATED BOOK ALREADY LOADED                                        
OVTVQ100 GOTO1 VHELLO,DMCB,(C'G',OVUNTFIL),(X'60',AIOAREA1),(1,=C'J')           
         CLI   12(R1),0                                                         
         BE    OVTVQ200                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'6007D1'                                               
*                                                                               
         MVC   DUB(8),=CL8'MAR15/00'   ***  CURRENT LATEST BOOK  ***            
         CLI   NBSTSTAT,C'C'          CHECK CABLE BOOK                          
         BNE   *+10                                                             
         MVC   DUB(8),=CL8'DEC15/99'  ***  CURRENT LATEST BOOK  ***             
         GOTO1 VDATVAL,DMCB2,(0,DUB),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
*        MVI   1(R2),4                                                          
         MVC   WORK+3(1),4(R1)                                                  
         MVC   WORK+4(1),8(R1)                                                  
         GOTO1 VDATCON,(R1),(0,DUB),(2,WORK+5)                                  
         GOTO1 VHELLO,DMCB,(C'P',OVUNTFIL),(X'60',AIOAREA1),WORK,0              
*                                                                               
OVTVQ200 GOTO1 VHELLO,DMCB,(C'G',OVUNTFIL),(X'60',AIOAREA1),(1,=C'K')           
         CLI   12(R1),0                                                         
         BE    OVTVQEX                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'6007D2'                                               
*                                                                               
         MVC   WORK+5(2),NUKDATE                                                
         GOTO1 VDATCON,(R1),(2,NUKDATE),(0,DUB)                                 
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
         MVC   WORK+3(1),4(R1)                                                  
         MVC   WORK+4(1),8(R1)                                                  
         GOTO1 VHELLO,DMCB,(C'P',OVUNTFIL),(X'60',AIOAREA1),WORK,0              
*                                                                               
OVTVQEX  B     OVEXIT                                                           
         EJECT                                                                  
* DEMO GUARANTEE FACTOR                                                         
*                                                                               
OVDG     CLI   COMMAND,C'S'        TEST FOR SYNTAX ONLY EDIT                    
         BE    OVDG2               YES                                          
         MVI   ELCODE,X'B4'        DELETE DEMO GUARANTEE EL                     
         GOTO1 VHELLO,DMCB,(C'D',OVUNTFIL),(ELCODE,AIOAREA1),0                  
         SPACE 1                                                                
OVDG2    CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    OVDGX               NONE-EXIT                                    
*                                                                               
         MVC   WORK(8),FLDH        BUILD SCREEN HEADER                          
         LA    R2,FLD                                                           
         LA    RF,WORK+8                                                        
         MVI   WORK+5,0                                                         
         LA    R0,9                                                             
OVDG220  CLI   0(R2),C'/'                                                       
         BE    OVDG240                                                          
         MVC   0(1,RF),0(R2)                                                    
         LA    RF,1(RF)                                                         
         LA    R2,1(R2)                                                         
         ZIC   R1,WORK+5                                                        
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5                                                        
         BCT   R0,OVDG220                                                       
OVDGERR  MVI   UNERROR,INVERR                                                   
         B     OVDGX                                                            
OVDG240  LA    R2,1(R2)                                                         
         ZIC   RE,WORK+5                                                        
         LA    RE,8(RE)                                                         
         STC   RE,WORK                                                          
         ST    R2,WORK+20                                                       
         LA    R2,DBLOCKA                                                       
         USING DBLOCKD,R2                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'EVN'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'V'                                                    
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         GOTO1 CDEMOVAL,DMCB,WORK,(1,FULL),DBLOCKA                              
         CLI   4(R1),0                                                          
         BE    OVDGERR                                                          
         MVC   THREE,FULL           EXTRACT CATEGORY                            
**       MVC   BYTE,FULL+2         EXTRACT CATEGORY                             
*                                                                               
         ZIC   RE,WORK+5           POINT R2 AT BLOCK                            
         ZIC   R0,FLDH+5                                                        
         BCTR  R0,0                                                             
         SR    R0,RE                                                            
         L     R2,WORK+20                                                       
         GOTO1 VCASHVAL,DMCB,(4,0(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    OVDGERR                                                          
         SPACE 1                                                                
OVDG4    CLI   COMMAND,C'S'                                                     
         BE    OVDGX                                                            
         MVC   FULL,4(R1)          GET GUARANTEE FACTOR                         
         OC    FULL,FULL           TEST FOR ZERO                                
         BZ    OVDGX               YES-JUST DELETE ELEMENT                      
*                                                                               
         XC    DUB,DUB                                                          
         LA    R3,DUB                                                           
         USING NUNDGD,R3                                                        
         MVI   NUNDGEL,X'B4'       BUILD DEMO GUARANTEE ELEMENT                 
         MVI   NUNDGLEN,NUNDGELN                                                
         MVC   NUNDGDEM,THREE                                                   
         MVC   NUNDGFAC,FULL                                                    
         GOTO1 VHELLO,DMCB,(C'P',OVUNTFIL),(X'B4',AIOAREA1),DUB,0               
         SPACE 1                                                                
OVDGX    B     OVEXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
OVEXIT   XMOD1 1                                                                
         SPACE 2                                                                
OVUNTFIL DC    CL8'UNTFIL'                                                      
         LTORG                                                                  
* ESTIMATED DEMOS (COMMAND 'D')                                                 
*                                                                               
* PUT ESTIMATED DEMOS ON A PFB UNDER ACTION ADD SO THAT POST                    
* CAN TAKE ACTUALS FROM ESTIMATED IF NECESSARY                                  
*                                                                               
         DS    0F                                                               
         DROP  RA                                                               
DEMORTN  NMOD1 0,**DEMO**                                                       
         LA    RC,2048(RB)         RC IS SECOND BASE REGISTER                   
         LA    RC,2048(RC)                                                      
         USING DEMORTN+4096,RC                                                  
*                                                                               
         MVI   UNERROR,0                                                        
         CLI   UNACTSW,C'C'        TEST FOR ACTION CHANGE                       
         BNE   DEMO1                                                            
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         CLI   BUYUNFR,C'Y'        SEE IF UNFROZEN OVERRIDE SET                 
         BE    DEMO1                                                            
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    DEMOX                                                            
         DROP  RE                                                               
         SPACE                                                                  
DEMO1    XC    MISSELS,MISSELS                                                  
         CLI   UNLUPVPH,YES        JUST LOOK UP NEW DEMO'S                      
         BE    DEMO4                                                            
         CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    *+12                                                             
         CLI   UNNOLOOK,YES        TEST FOR SUPPRESSED LOOKUP                   
         BE    DEMO12                                                           
         OC    UNAMISS,UNAMISS     TEST FOR PASSED MISSING RECD ADCON           
         BZ    DEMO2               NONE                                         
         BAS   RE,MISSDEM          GET MISSING DEMO EL ADDRESSES                
         SPACE                                                                  
* UNIVERSE LOOKUP                                                               
*                                                                               
DEMO2    CLI   UNACTSW,C'A'        TEST FOR ADDING RECORD                       
         BE    DEMO2A              YES-PERFORM LOOKUP                           
         CLI   UNESTLK,YES         TEST FOR FORCED LOOKUP                       
         BE    DEMO2A              YES                                          
         CLC   NUUNCODE,NBUNCODE   TEST FOR CHANGE IN UNIVERSE CODE             
         BNE   DEMO2A              YES-LOOKUP UNIVERSES AGAIN                   
         CLC   NUKDATE,NBACTDAT    TEST FOR CHANGE IN DATE                      
         BE    DEMO4               NO-SKIP UNIVERSE LOOKUP                      
         OC    NUUNCODE,NUUNCODE   YES-NOW TEST FOR A UNIVERSE CODE             
         BNZ   DEMO4               YES-SKIP LOOKUP                              
         SPACE 1                                                                
DEMO2A   ICM   R3,15,AMSUNVEL                                                   
         BNZ   DEMO3               ALREADY HAVE MISSING RECD UNIVERSES          
*                                                                               
         LA    R2,UNIVBLK                                                       
         USING GUVD,R2                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,NUALPHA                                                   
         MVC   GUVCODE,NUUNCODE                                                 
         OC    GUVCODE,GUVCODE     TEST FOR UNIVERSE CODE                       
         BNZ   *+10                HAVE ONE                                     
         MVC   GUVDATE,NUKDATE     ELSE USE AIR DATE                            
         XC    WORK,WORK                                                        
         LA    R3,UNIVEL                                                        
         ST    R3,GUVAOUT          OUTPUT ELEMENT ADDRESS                       
         LA    R0,IOA                                                           
         ST    R0,GUVAREC          SET AND CLEAR AREA FOR UNIV. RECORD          
         LA    R1,L'IOA                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVNETWK,VNETWEEK                                                
         NI    NUUNST2,X'FF'-X'08' TURN OFF CABLE UNIV                          
         GOTO1 VGETNUN,DMCB2,(R2)                                               
         CLI   GUVERROR,0          TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    NUUNST2,X'08'                                                    
         SPACE                                                                  
DEMO3    MVI   ELCODE,X'31'                                                     
         BAS   RE,DEMDELEL                                                      
         BAS   RE,DEMPUTEL                                                      
         MVI   BYTE,C'U'                                                        
         BAS   RE,DEMDELND                                                      
*                                                                               
         OC    UNAMISS,UNAMISS     TEST FOR MISSING RECORD                      
         BZ    *+16                                                             
         L     R2,UNAMISS          YES-POINT R2 AT ITS FIRST ELEMENT            
         LA    R2,NUMAINEL-NUKEY(R2)                                            
         B     *+16                                                             
         CLI   GUVRECSW,YES        TEST FOR RETURNED UNIVERSE RECORD            
         BNE   DEMO3A              NO-SKIP USER DEMO LOOKUP                     
         LA    R2,IOA+24           YES-POINT TO FIRST ELEMENT                   
         GOTO1 GETUSER,DMCB2,(X'C1',(R2))                                       
         GOTO1 GETNAD,DMCB2,(X'DD',(R2))                                        
         B     DEMO4                                                            
DEMO3A   MVC   HALF,GUVDEMBK                                                    
         BAS   RE,BLDNUNIV         BLD DEFAULT NAD UNIV DEMOS                   
         DROP  R2                                                               
         SPACE                                                                  
* VPH LOOKUP                                                                    
*                                                                               
DEMO4    CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    *+12                YES-MAINTAIN VPH ELEMENT                     
         CLI   UNESTLK,YES         TEST FOR FORCED LOOKUP ON CHANGE             
         BNE   DEMO6               NO-PRESERVE VPH ELEMENT                      
         ICM   R3,15,AMSVPHEL                                                   
         BNZ   DEMO5                                                            
*                                                                               
         L     R2,APROGEL                                                       
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),X'93'                                                      
         BNE   DEMO4A                                                           
         USING NPG2ELEM,R2                                                      
         XC    WORK(119),WORK                                                   
         LA    R3,WORK                                                          
         USING NUEVD,R3                                                         
         MVI   NUEVEL,X'33'        VPH ELEMENT CODE                             
         MVI   NUEVLEN,119         ELEMENT LENGTH                               
         MVI   NUEVELEN,X'42'                                                   
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BO    DEMO5               YES DONT MOVE OUT THE VPH'S                  
         MVC   NUEVPHS,NPG2VPHS    VPH'S FROM PROGRAM RECORD                    
         B     DEMO5                                                            
         DROP  R2,R3,RE                                                         
*                                                                               
DEMO4A   L     R2,APROGEL                                                       
         USING NPGELEM,R2                                                       
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NUEVD,R3                                                         
         MVI   NUEVEL,X'33'        VPH ELEMENT CODE                             
         MVI   NUEVLEN,37          ELEMENT LENGTH                               
         MVI   NUEVELEN,1                                                       
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BO    DEMO5               YES DONT MOVE OUT THE VPH'S                  
         MVC   NUEVPHS(34),NPGVPHS     VPH'S FROM PROGRAM RECORD                
         DROP  R2,R3,RE                                                         
         SPACE                                                                  
DEMO5    MVI   ELCODE,X'33'                                                     
         BAS   RE,DEMDELEL                                                      
         BAS   RE,DEMPUTEL                                                      
         MVI   SPACES,X'40'        RESET SPACES                                 
         MVC   SPACES+1(132),SPACES                                             
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BZ    DEMO5A              NO, HANDLE VPH LOGIC                         
* IMPRESSION BASED LOGIC                                                        
         MVI   BYTE,C'T'                                                        
         BAS   RE,DEMDELND                                                      
         MVI   BYTE,C'H'                                                        
         BAS   RE,DEMDELND                                                      
         MVI   BYTE,C'R'                                                        
         BAS   RE,DEMDELND                                                      
*                                                                               
         GOTO1 GETNAD,DMCB2,(X'DD',(R2))                                        
         B     DEMO5C                                                           
         DROP  RE                                                               
*                                                                               
DEMO5A   MVI   BYTE,C'V'                                                        
         BAS   RE,DEMDELND                                                      
         MVI   BYTE,C'R'                                                        
         BAS   RE,DEMDELND                                                      
         L     R2,APROGREC                                                      
         LA    R2,24(R2)           POINT TO FIRST ELEMENT OF PROG.              
         OC    UNAMISS,UNAMISS     TEST FOR MISSING RECORD                      
         BZ    *+12                                                             
         L     R2,UNAMISS          YES-GET USER DEMOS FROM IT                   
         LA    R2,NUMAINEL-NUKEY(R2)                                            
         GOTO1 GETUSER,DMCB2,(X'C3',(R2))                                       
         GOTO1 GETNAD,DMCB2,(X'DD',(R2))                                        
*                                                                               
DEMO5C   ICM   R3,15,AMSBKEL       FETCH MISSING REC BK ELEM                    
         BNZ   *+12                YES-ADCON IS SET                             
         ICM   R3,15,ABOOKEL       NO-TRY FOR PROG REC BK EL                    
         BZ    DEMO6               NO ALSO SKIP BOOK ELEMENT                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BZ    *+10                YES DONT MOVE OUT THE VPH'S                  
         MVC   2(3,R3),=CL3'EIN'   IMPRESSION BASED                             
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,DEMDELEL                                                      
         BAS   RE,DEMPUTEL                                                      
         DROP  RE                                                               
         SPACE                                                                  
* HUT LOOKUP                                                                    
*                                                                               
DEMO6    CLI   UNACTSW,C'A'        TEST FOR ADD                                 
         BE    DEMO7               YES-GENERATE A HOMES ELEMENT                 
         CLI   UNESTLK,YES         TEST FOR FORCED LOOKUP ON CHANGE             
         BE    DEMO7               YES                                          
         BAS   RE,HUTCHG           NO-SEE IF HUT LOOKUP NEEDED ANYWAY           
         BE    DEMO12              NO                                           
         SPACE                                                                  
DEMO7    ICM   R3,15,AMSHOMEL                                                   
         BNZ   DEMO10                                                           
*                                                                               
         LA    R2,HUTBLK                                                        
         USING GETHUTD,R2                                                       
         CLI   DEMFRST,YES         TEST FOR FIRST CALL                          
         BNE   *+14                NO                                           
         XC    GHBLOCK,GHBLOCK                                                  
         MVI   DEMFRST,NO          TURN OFF SWITCH                              
         MVC   GHDAY,NUDAY                                                      
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         CLI   NPGDAY,X'7C'        TEST FOR M-F ROTATION                        
         BE    *+12                                                             
         CLI   NPGDAY,X'7F'        TEST FOR M-SU ROTATION                       
         BNE   DEMO8                                                            
         MVC   GHDAY,NPGDAY        DEFAULT IS AVERAGE HUT                       
         CLI   BUYPROF+2,C'D'      TEST FOR SINGLE DAY HUT OPTION               
         BNE   DEMO8               NO-USE THE WEEKLY AVERAGE                    
*                                                                               
         GOTO1 VDATCON,DMCB2,(2,NUKDATE),DUB2                                   
         GOTO1 VGETDAY,(R1),DUB2,FULL2                                          
         ZIC   RF,0(R1)            DAY NUMBER                                   
         LA    R1,X'80'            BIT MASK                                     
         SRL   R1,0(RF)            FORCE SINGLE DAY SINCE                       
         STC   R1,GHDAY            UNIT DAY COULD BE ROTATION ALSO              
         DROP  RE                                                               
         SPACE                                                                  
DEMO8    MVC   GHMILTIM,NUTIME                                                  
         MVC   GHAGYMED,AGYMED                                                  
         MVC   GHDAYPRT,NUKDP      DAYPART FROM KEY                             
*                                                                               
         MVC   GHDATE,NUKDATE                                                   
         MVC   GHAVE,NUHUTAVE                                                   
         CLI   GHAVE,0                                                          
         BNE   *+10                                                             
         MVC   GHAVE,NBUSER        DEFAULT TO NETWORK PROFILE FOR AVG.          
         MVC   GHYEAR,ESTBOOK      LAST YEAR FOR HUTS                           
         MVC   GHNYEARS,ESTBOOK+1  NUMBER OF YEARS BACK                         
*                                                                               
         MVI   GHBKTYPE,0                                                       
         CLI   NUHUTTYP,C'D'                                                    
         BE    *+10                                                             
         MVC   GHBKTYPE,NUHUTTYP                                                
         MVC   GHSCHEME,NUHUTSCM                                                
         CLI   GHSCHEME,0          TEST FOR SCHEME                              
         BNE   *+16                YES                                          
         TM    NUUNST2,X'80'       TEST FOR DEMO FILE DEFAULT                   
         BZ    *+8                                                              
         MVI   GHSCHEME,X'FE'      YES                                          
         TM    NUUNST2,X'40'       TEST FOR 52 WEEK CALENDAR                    
         BZ    *+8                                                              
         MVI   GH52,YES            YES-SET 52 WEEK OPTION                       
         CLI   BUYPROF+4,YES       TEST FOR DEFAULTING TO DDS HUTS              
         BNE   *+8                                                              
         MVI   GHDEFDEM,YES                                                     
*                                                                               
         MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHNETWK,VNETWEEK                                                 
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,DEMGETEL                                                      
         BNE   DEMO9                                                            
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         MVC   GHFLAVOR,NUSDHFL    LOAD HUT FLAVOR                              
         CLI   GHFLAVOR,0                                                       
         BNE   *+10                                                             
DEMO9    MVC   GHFLAVOR,BUYPROF2+2  DEFAULT TO NETWORK PROFILE FOR AVG.         
         CLI   NUPOSTYP,C'H'                                                    
         BNE   *+8                                                              
         MVI   GHSURVY,C'H'        LOAD HISPANIC SURVEY                         
         DROP  RE                                                               
*                                                                               
         GOTO1 VGETHUT,DMCB2,(R2)                                               
         CLI   GHERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ESTHUT,GHHUT                                                     
         XC    UNDEMS(4),UNDEMS    ERASE OVERRIDES AND IGNORE INPUT             
         MVC   UNRAT,=H'-1'                                                     
         DROP  R2                                                               
         SPACE                                                                  
DEMO10   MVI   ELCODE,X'35'                                                     
         BAS   RE,DEMDELEL                                                      
         BAS   RE,DEMPUTEL                                                      
         SPACE                                                                  
* PACKAGE/DEMO GUARANTEE FACTORS FROM MISSING RECORD                            
*                                                                               
DEMO11   OC    UNAMISS,UNAMISS     TEST MISSING RECORD PASSED                   
         BZ    DEMO12              NO                                           
*                                                                               
         MVI   ELCODE,X'B3'        PACKAGE GUARANTEE ELEMENT                    
         BAS   RE,DEMDELEL                                                      
         ICM   R3,15,AMSPGEL       GET MISSING REC ELEMENT                      
         BZ    *+8                 NONE                                         
         BAS   RE,DEMPUTEL                                                      
***      MVI   ELCODE,X'B1'        PACKAGE GUARANTEE ELEMENT                    
***      BAS   RE,DEMDELEL                                                      
***      ICM   R3,15,AMSPGEL       GET MISSING REC ELEMENT                      
***      BZ    *+8                 NONE                                         
***      BAS   RE,DEMPUTEL                                                      
*                                                                               
         MVI   ELCODE,X'B4'        NEW DEMO GUARANTEE ELEMENT                   
         BAS   RE,DEMDELEL                                                      
         ICM   R3,15,AMSDGEL                                                    
         BZ    *+8                                                              
         BAS   RE,DEMPUTEL                                                      
***      MVI   ELCODE,X'B2'        DEMO GUARANTEE ELEMENT                       
***      BAS   RE,DEMDELEL                                                      
***      ICM   R3,15,AMSDGEL                                                    
***      BZ    *+8                                                              
***      BAS   RE,DEMPUTEL                                                      
         SPACE 1                                                                
* HOMES AND MISSING RECORD OVERRIDES                                            
*                                                                               
DEMO12   MVC   DUB2(4),NBESTSHR    SHARE AND HUT                                
         MVC   DUB2+4(2),NBESTHOM+2 RATING                                      
         OC    UNAMISS,UNAMISS     TEST FOR MISSING OVERRIDE                    
         BZ    DEMO15              NONE                                         
         BAS   RE,MISSOVER         ADD OVERRIDES FROM MISSING RECORD            
         CLI   UNERROR,0                                                        
         BNE   DEMOX                                                            
         CLI   UNACTSW,C'A'        TEST FOR ADDING RECORD                       
         BE    DEMOX                                                            
         SPACE                                                                  
DEMO15   L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BO    DEMOX               YES DONT CALC HOMES VALUES                   
         CLI   UNESTLK,YES         TEST FOR FORCED LOOKUP                       
         BE    DEMOX               DON'T CHANGE OVERRIDES                       
         BAS   RE,HOMOVER                                                       
         B     DEMOX                                                            
         DROP  RE                                                               
         SPACE                                                                  
*                                                                               
DEMOX    MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         B     DEMUNX                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DELETE ELEMENT (ELEMENT CODE IN ELCODE)                        
*                                                                               
DEMDELEL LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(ELCODE,(R4)),0                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DELETE NAD VPH AND UNIVERSE ELEMENTS                           
*  INPUT BYTE= DEMO MODIFIER                                                    
*                                                                               
DEMDELND NTR1                                                                   
         USING NUOVD,R2                                                         
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,DEMGETEL                                                      
         BE    DEMDN10                                                          
*                                                                               
         MVI   ELCODE,X'DE'                                                     
         BAS   RE,DEMGETEL                                                      
         BNE   DEMDNEX                                                          
*                                                                               
DEMDN10  L     R2,12(R1)                                                        
         B     DEMDN40                                                          
*                                                                               
*-GET NEXT ELEMENT                                                              
DEMDN30  ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),0                                                          
         BE    DEMDNEX                                                          
         CLI   0(R2),X'DE'                                                      
         BH    DEMDNEX                                                          
*                                                                               
DEMDN40  TM    NUOVFLG,X'80'       CHECK FOR NAD                                
         BZ    DEMDN30                                                          
         CLC   BYTE,NUOVMOD        SEE IF RIGHT MODIFIER                        
         BNE   DEMDN30                                                          
*                                                                               
         MVC   ELCODE,NUOVEL                                                    
         MVC   WORK(10),NUOVZER                                                 
         GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(ELCODE,(R4)),(10,WORK)              
         B     DEMDN40                                                          
*                                                                               
DEMDNEX  B     DEMUNX                                                           
         DROP  R2                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PUT AN ELEMENT (R3 POINTS TO ELEMENT)                          
*                                                                               
DEMPUTEL LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VHELLO,DMCB2,(C'P',FILAREA),(ELCODE,(R4)),(R3),0                 
         PRINT NOGEN                                                            
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BER   RE                                                               
         L     RD,MYAWORK                                                       
         MVI   UNERROR,TOOLARGE                                                 
         CLI   12(R1),5                                                         
         BE    DEMUNX              UNWIND OUT OF DEMO CODE                      
         DC    H'0'                DUMP FOR OTHER ERRORS                        
         SPACE 2                                                                
* SUB-ROUTINE TO GET ELEMENT (AT ENTRY, ELCODE CONTAINS ELEMENT CODE)           
* ON EXIT, CC=EQ IF ELEMENT FOUND, CC=NEQ IF NOT FOUND                          
*                                                                               
DEMGETEL LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'G',FILAREA),(ELCODE,(R4)),0                      
         CLI   12(R1),0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
DEMUNX   XMOD1 1                                                                
* SUB-ROUTINE TO SET ESTIMATED DEMO ELEMENT ADDRESSES ON MISSING REC.           
*                                                                               
MISSDEM  L     R1,UNAMISS                                                       
         SR    R0,R0                                                            
         LA    R1,NUMAINEL-NUKEY(R1) POINT R1 AT FIRST ELEMENT                  
         SPACE                                                                  
MISSDEM1 CLI   0(R1),0             TEST FOR E-O-R                               
         BE    MISSDEMX                                                         
         CLI   0(R1),X'31'         LOOK FOR UNIVERSE ELEMENT                    
         BNE   *+8                                                              
         ST    R1,AMSUNVEL                                                      
         CLI   0(R1),X'33'         NEXT SEARCH FOR VPH'S                        
         BNE   *+8                                                              
         ST    R1,AMSVPHEL                                                      
         CLI   0(R1),X'35'         LAST TRY FOR HOMES ELEMENT                   
         BNE   *+8                                                              
         ST    R1,AMSHOMEL                                                      
         CLI   0(R1),X'5D'         TEST FOR BOOK ELEMENT                        
         BNE   *+8                                                              
         ST    R1,AMSBKEL                                                       
         CLI   0(R1),X'B3'         TEST FOR PACKAGE GUARANTEE ELEMENT           
         BNE   *+8                                                              
         ST    R1,AMSPGEL                                                       
***      CLI   0(R1),X'B1'         TEST FOR PACKAGE GUARANTEE ELEMENT           
***      BNE   *+8                                                              
***      ST    R1,AMSPGEL                                                       
         CLI   0(R1),X'B4'         TEST FOR NEW DEMO GUARANTEE ELEMENT          
         BNE   *+8                                                              
         ST    R1,AMSDGEL                                                       
***      CLI   0(R1),X'B2'         TEST FOR DEMO GUARANTEE ELEMENT              
***      BNE   *+8                                                              
***      ST    R1,AMSDGEL                                                       
         IC    R0,1(R1)                                                         
         AR    R1,R0               POINT TO NEXT ELEMENT                        
         B     MISSDEM1                                                         
         SPACE                                                                  
MISSDEMX BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET USER DEMO ELEMENTS FROM AN INPUT RECORD                    
*                                                                               
* AT ENTRY     P1 BYTE  0   = USER DEMO ELEMENT CODE                            
*                 BYTES 1-3 = A(FIRST ELEMENT OF INPUT RECORD)                  
*                                                                               
*              ASSUMES R4 POINTS TO OUTPUT RECORD                               
*                                                                               
GETUSER  NTR1                                                                   
         MVC   ELCODE,0(R1)                                                     
         L     R3,0(R1)            R3 IS ELEMENT POINTER                        
         BAS   RE,DEMDELEL         DELETE USER DEMOS FROM OUTPUT RECORD         
         SR    R2,R2               CLEAR WORK REGISTER                          
         SPACE 1                                                                
GETUSER2 CLI   0(R3),0             TEST FOR EOR                                 
         BE    GETUSERX            YES-EXIT                                     
         CLC   ELCODE,0(R3)        TEST FOR USER DEMO                           
         BNE   GETUSER4            NO                                           
         LA    R1,4                COUNTER OF USER DEMO NAMES                   
         LA    RE,ESTUSNS          RE POINTS TO NAMES                           
         CLC   2(7,R3),0(RE)       TEST ELEMENT NAME AGAINST LIST               
         BE    GETUSER3            FOUND IT                                     
         LA    RE,7(RE)            TRY NEXT ENTRY                               
         BCT   R1,*-14                                                          
         B     GETUSER4                                                         
         SPACE 1                                                                
GETUSER3 BAS   RE,DEMPUTEL         PUT TO OUTPUT RECORD                         
         SPACE 1                                                                
GETUSER4 IC    R2,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R2                                                            
         B     GETUSER2                                                         
         SPACE                                                                  
GETUSERX B     DEMUNX                                                           
         EJECT                                                                  
* SUB-ROUTINE TO GET NAD DEMO ELEMENTS FROM AN INPUT RECORD                     
*                                                                               
* AT ENTRY     P1 BYTE  0   = USER DEMO ELEMENT CODE                            
*                 BYTES 1-3 = A(FIRST ELEMENT OF INPUT RECORD)                  
*                                                                               
*              ASSUMES R4 POINTS TO OUTPUT RECORD                               
*                                                                               
GETNAD   NTR1                                                                   
         MVC   ELCODE,0(R1)                                                     
         L     R3,0(R1)            R3 IS ELEMENT POINTER                        
         SR    R2,R2               ESTIMATE DEMOS                               
         SPACE 1                                                                
GETNAD2  CLI   0(R3),0             TEST FOR EOR                                 
         BE    GETNADX             YES-EXIT                                     
         CLC   ELCODE,0(R3)        TEST FOR NAD DEMO                            
         BNE   GETNAD8             NO                                           
*                                                                               
         L     RE,APACKREC         IS BUY IMP BASED                             
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'                                                   
         BO    GETNAD3             YES PASS ALL THE DEMOS                       
         CLI   4(R3),C'T'          IS DEMO AN IMP                               
         BE    GETNAD8             YES BYPASS                                   
         CLI   4(R3),C'H'          IS DEMO AN IMP                               
         BE    GETNAD8             YES BYPASS                                   
         B     GETNAD4                                                          
*--IMPRESSION BASED BUY EDIT                                                    
GETNAD3  CLI   4(R3),C'V'          IS DEMO AN VPH                               
         BE    GETNAD8             YES BYPASS                                   
         CLI   4(R3),C'R'          IS DEMO AN VPH                               
         BE    GETNAD8             YES BYPASS                                   
         DROP  RE                                                               
*                                                                               
GETNAD4  ZIC   R1,ESTNDEMS                                                      
         LA    RE,ESTDEMS          RE POINTS TO NAMES                           
         CLC   3(1,R3),0(RE)       TEST ELEMENT NAME AGAINST LIST               
         BNE   GETNAD5             NO MATCH                                     
         CLC   5(1,R3),2(RE)       TEST ELEMENT NAME AGAINST LIST               
         BE    GETNAD6             FOUND IT                                     
GETNAD5  LA    RE,3(RE)            TRY NEXT ENTRY                               
         BCT   R1,*-24                                                          
         B     GETNAD8                                                          
         SPACE 1                                                                
GETNAD6  BAS   RE,DEMPUTEL         PUT TO OUTPUT RECORD                         
         CLI   4(R3),C'R'          IS DEMO A RTG                                
         BNE   *+8                 YES BYPASS                                   
         BAS   RE,EIMP             BUILD IMPRESSION OVERRIDE                    
         SPACE 1                                                                
GETNAD8  IC    R2,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R2                                                            
         B     GETNAD2                                                          
         SPACE                                                                  
GETNADX  B     DEMUNX                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CALCULATE THE ESTIMATE IMP AND TO ADD AN OVERRIDE              
*                                                                               
* AT ENTRY R3 CONTAINS RTG OVERRIDE ELEMENT                                     
* AT ENTRY BYTE CONTAINS OVERRIDE RATING PRECISION FACTOR                       
*                                                                               
EIMP     NTR1                                                                   
         CLI   4(R3),X'21'         CHECK FOR USER DEMO                          
         BE    EIMPX                                                            
         CLI   5(R3),1             TEST FOR HOMES                               
         BE    EIMPX                                                            
*                                                                               
         LA    R2,DBLOCKA                                                       
         USING DBLOCKD,R2                                                       
         MVC   DBFILE,=C'EVN'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         ST    R4,DBAREC           UNIT RECORD                                  
         LA    RE,NUMAINEL-NUKEY(R4)                                            
         ST    RE,DBAQUART                                                      
         MVC   WORK(3),3(R3)                                                    
         MVI   WORK+1,C'U'                                                      
         MVI   WORK+3,X'FF'                                                     
         GOTO1 VDEMOUT,DMCB,(C'L',WORK),DBLOCK,WORK+8                           
*                                                                               
         L     R1,WORK+8           LOAD THE UNIVERSE                            
         L     R0,8(R3)            PUT RATING IN R0                             
         MR    R0,R0               RATING*UNIVERSE                              
         TM    7(R3),X'82'         CHECK PRECISION FOR HUNDREDS RATING          
         BO    EIMP20              ADJUST THE ROUND                             
*                                                                               
         AH    R1,=H'50'           ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'100'                                                       
         TM    7(R3),X'82'         CHECK PRECISION FOR HUNDREDS RATING          
         BNO   EIMP30              ADJUST THE ROUND                             
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
         B     EIMP30                                                           
*                                                                               
EIMP20   AH    R1,=H'500'          ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'1000'                                                      
         TM    7(R3),X'82'         CHECK PRECISION FOR HUNDREDS RATING          
         BNO   EIMP30              ADJUST THE ROUND                             
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
*                                                                               
EIMP30   XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING NUOVD,RE                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),3(R3)                                                 
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'43'                                                    
         TM    7(R3),X'82'         CHECK PRECISION FOR HUNDREDS RATING          
         BNO   *+12                ADJUST THE ROUND                             
         MVI   NUOVMOD,C'H'                                                     
         MVI   NUOVPRE,X'42'                                                    
         MVI   NUOVFLG,X'80'                                                    
         STCM  R1,15,NUOVVAL                                                    
         LR    R3,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(X'DD',(R4)),(5,WORK+2)              
         BAS   RE,DEMPUTEL                                                      
*                                                                               
EIMPX    B     DEMUNX                                                           
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD NAD UNIVERSE ELEMENTS FROM DEFAULT UNIVERSE              
*   INPUT HALF=DEMO BOOK                                                        
*                                                                               
*                                                                               
BLDNUNIV NTR1                                                                   
         L     R6,NBADEM           ADDRESS OF DEMO BLK                          
         LA    R3,IOA                                                           
*                                                                               
BLDNU20  CLI   0(R6),X'FF'         END OF LIST                                  
         BE    BLDNU60                                                          
         CLI   0(R6),0             CHECK FOR NAD DEMO                           
         BE    BLDNU40                                                          
         CLI   1(R6),C'U'          CHECK FOR UNIVERSE                           
         BNE   BLDNU40                                                          
         MVC   0(3,R3),0(R6)       MOVE DEMO TO LIST                            
         LA    R3,3(R3)                                                         
BLDNU40  LA    R6,3(R6)            BUMP TO NEXT DEMO                            
         B     BLDNU20             CLEAR WORK REGISTER                          
         SPACE 1                                                                
BLDNU60  MVI   0(R3),X'FF'         SET END OF LIST MARK                         
         CLI   IOA,X'FF'           CHECK NO INPUT                               
         BE    BLDNUEX                                                          
         XC    WORK,WORK           BUILD SKELETON DUMMY RECORD                  
         MVI   WORK,C'P'                                                        
         MVC   WORK+20(2),=H'24'                                                
*                                                                               
         LA    R2,DBLOCKA                                                       
         USING DBLOCKD,R2                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,NUALPHA                                                 
         LA    RE,WORK                                                          
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELBK,HALF                                                     
         ZIC   RE,DBSELBK+1                                                     
         SRL   RE,2                                                             
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK+1                                                     
         SPACE 1                                                                
         GOTO1 VDEMOUT,DMCB,(C'L',IOA),DBLOCK,IOA+70                            
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NUOVD,R3                                                         
         LA    R5,IOA                                                           
         LA    R6,IOA+70                                                        
BLDNU100 CLI   0(R5),X'FF'         TEST FOR END OF TABLE                        
         BE    BLDNUEX             YES-EXIT                                     
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),0(R5)                                                 
         MVI   NUOVFLG,X'80'                                                    
         MVC   NUOVVAL,0(R6)                                                    
         MVI   NUOVPRE,X'44'       COUNTER OF USER DEMO NAMES                   
         BAS   RE,DEMPUTEL         RE POINTS TO NAMES                           
         LA    R5,3(R5)                                                         
         LA    R6,4(R6)                                                         
         B     BLDNU100                                                         
*                                                                               
BLDNUEX  B     DEMUNX                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DETERMINE IF HUTS SHOULD BE LOOK-UP ON A CHANGE                
* ON EXIT CC=NEQ FOR LOOK-UP AND EQ FOR SKIPPING LOOK-UP                        
*                                                                               
* FOR ROTATORS, THE DAY OR WEEKLY AVERAGE PROFILE OPTION CONTROLS               
* RE-LOOKUP ON A DATE CHANGE.  FOR WEEKLY AVERAGE, THE HUT WILL BE              
* LOOKED-UP ONLY IF THE USER MOVES THE UNIT TO ANOTHER WEEK.  FOR               
* THE SINGLE DAY OPTION, THE HUT WILL BE LOOKED UP AGAIN FOR ANY                
* DATE CHANGE.                                                                  
*                                                                               
HUTCHG   ST    RE,SAVEREG1         SAVE RETURN POINT                            
         CLC   NUHUTTYP,NBHUTTYP   TEST FOR CHANGE IN HUT TYPE                  
         BE    HC100               YES-LOOK UP HUT AGAIN                        
         CLI   NBHUTTYP,0          IF OLD ZERO                                  
         BNE   HUTLOOK                                                          
         CLI   NUHUTTYP,C'D'       IF C'D' THAN EQUAL                           
         BNE   HUTLOOK                                                          
HC100    CLC   NUHUTSCM,NBHUTSCM   TEST FOR CHANGE IN HUT SCHEME                
         BNE   HUTLOOK             YES-LOOK UP HUT AGAIN                        
         CLC   NUHUTAVE,NBHUTAVE   TEST FOR CHANGE IN HUT AVERAGE               
         BNE   HUTLOOK                                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,DEMGETEL                                                      
         BE    HC150                                                            
         OC    NBHUTFLR,NBHUTFLR                                                
         BNZ   HUTLOOK                                                          
         B     HC200                                                            
HC150    L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         CLC   NUSDHFL,NBHUTFLR    TEST FOR CHANGE IN HUT FLAVOR                
         BNE   HUTLOOK                                                          
         DROP  RE                                                               
*                                                                               
HC200    CLC   NUTIME,NBTIME       CHANGE IN TIME                               
         BNE   HUTLOOK                                                          
         CLC   NUKDATE,NBACTDAT    CHANGE IN UNIT DATE                          
         BE    HUTKEEP             NO CHANGE IN DATE                            
         L     R1,APROGEL                                                       
         USING NPGELEM,R1                                                       
         CLI   NPGDAY,X'7C'        TEST FOR ROTATION                            
         BE    *+12                                                             
         CLI   NPGDAY,X'7F'                                                     
         BNE   HUTLOOK             NOT A ROTATION-LOOK UP HUT                   
*                                                                               
         CLI   BUYPROF+2,C'D'      TEST FOR SINGLE DAY OPTION                   
         BE    HUTLOOK             YES-LOOKUP HUT AGAIN                         
*                                                                               
         GOTO1 VDATCON,DMCB2,(2,NBACTDAT),OLDDATE                               
         GOTO1 (RF),(R1),(2,NUKDATE),NEWDATE                                    
         GOTO1 VGETDAY,(R1),OLDDATE,FULL2                                       
         ZIC   R0,0(R1)            DAY OF ORIGINAL UNIT DATE                    
         BCTR  R0,0                                                             
         LNR   R0,R0               FIND THE MONDAY OF ITS WEEK                  
         GOTO1 VADDAY,(R1),OLDDATE,MONDATE,(R0)                                 
         LA    R0,6                NOW FIND SUNDAY OF WEEK                      
         GOTO1 (RF),(R1),MONDATE,SUNDATE,(R0)                                   
         CLC   NEWDATE,MONDATE     TEST IF NEW UNIT DATE IS IN                  
         BL    HUTLOOK             SAME WEEK                                    
         CLC   NEWDATE,SUNDATE     YES-LEAVE HUT ALONE                          
         BH    HUTLOOK             NO-LOOK UP HUT                               
         SPACE                                                                  
HUTKEEP  CR    RB,RB               SET CC TO EQ                                 
         B     *+6                                                              
HUTLOOK  LTR   RB,RB               SET CC TO NEQ                                
         L     RE,SAVEREG1                                                      
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE ESTIMATED HOMES ELEMENT (AT ENTRY R1 POINTS           
* TO GHHUT, AT EXIT R3 POINTS TO HOMES ELEMENT)                                 
*                                                                               
ESTHUT   NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NUEHD,R3                                                         
         MVI   NUEHEL,X'35'                                                     
         MVI   NUEHLEN,9                                                        
         MVI   NUEHELEN,2                                                       
         MVC   NUEHHUT,0(R1)                                                    
*                                                                               
         L     R1,APROGEL          PROGRAM ELEMENT                              
         USING NPGELEM,R1                                                       
         TM    NPGSTAT,X'80'       TEST FOR SHARE OR RATING                     
         BZ    ESTHUT2             SHARE                                        
         MVC   NUEHRTG,NPGSHARE    SET DEFAULT RATING                           
         SR    R1,R1                                                            
         ICM   R1,3,NUEHRTG        SHARE = RATING / HUT                         
         M     R0,=F'10000'        SCALE THE DIVIDEND                           
         SR    RE,RE                                                            
         ICM   RE,3,NUEHHUT                                                     
         BZ    ESTHUT4             ZERO DIVISOR                                 
         DR    R0,RE               COMPUTE SHARE TO 2 DECIMAL PLACES            
         AH    R1,=H'5'            ROUND BACK UP TO 1                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         STCM  R1,3,NUEHSHR                                                     
         B     ESTHUT4                                                          
         SPACE 1                                                                
ESTHUT2  MVC   NUEHSHR,NPGSHARE                                                 
         SR    R0,R0               RATING = SHARE X HUT                         
         ICM   R0,3,NUEHSHR                                                     
         SR    R1,R1                                                            
         ICM   R1,3,NUEHHUT                                                     
         MR    R0,R0                                                            
         AH    R1,=H'500'          ROUND TO ONE DECIMAL PLACE                   
         D     R0,=F'1000'                                                      
         STCM  R1,3,NUEHRTG                                                     
         SPACE 1                                                                
ESTHUT4  SR    RE,RE                                                            
         ICM   RE,3,NUEHHUT        SCALE THE HUT TO TWO DECIMAL PLACES          
         MH    RE,=H'10'           FOR THE DEMO ELEMENT                         
         STCM  RE,3,NUEHHUT                                                     
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      IS PACKAGE IMPRESSION BASED                  
         BZ    *+10                                                             
         XC    NUEHRTG(6),NUEHRTG  YES DONT MOVE OUT THE RATING                 
         XIT1  REGS=(R3)                                                        
         DROP  R1,R3,RE                                                         
         EJECT                                                                  
* SUB-ROUTINE TO ADD ESTIMATED DEMO OVERRIDE ELEMENTS FROM MISSING              
* RECORD TO UNIT RECORD                                                         
*                                                                               
MISSOVER NTR1                                                                   
         L     R2,UNAMISS                                                       
         SR    R0,R0                                                            
         LA    R2,NUMAINEL-NUKEY(R2) POINT TO FIRST ELEMENT                     
         SPACE 1                                                                
MISSOV2  CLI   0(R2),0             TEST FOR E-O-R                               
         BE    MISSOVX                                                          
         MVI   ELCODE,X'CD'        USER DEMO ESTIMATED OVERRIDE                 
         MVI   BYTE,8              LENGTH OF MODIFIER/USER DEMO NAME            
         CLC   ELCODE,0(R2)                                                     
         BE    MISSOV3                                                          
         MVI   ELCODE,X'DD'        ESTIMATE DEMO OVERRIDE                       
         MVI   BYTE,2              LENGTH OF MODIFIER/DEMO                      
         CLI   1(R2),9                                                          
         BL    *+16                                                             
         TM    6(R2),X'80'         CHECK IF NAD DEMO                            
         BO    MISSOV4             YES BYPASS                                   
         MVI   BYTE,4              NEW OVERRIDE DEMO                            
         CLC   ELCODE,0(R2)                                                     
         BNE   MISSOV4                                                          
         SPACE 1                                                                
MISSOV3  GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(ELCODE,(R4)),(BYTE,2(R2))           
         GOTO1 (RF),(R1),(C'P',FILAREA),(ELCODE,(R4)),(R2)                      
         CLI   12(R1),0                                                         
         BE    MISSOV4                                                          
         MVI   UNERROR,TOOLARGE                                                 
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    MISSOVX                                                          
         DC    H'0'                                                             
         SPACE                                                                  
MISSOV4  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     MISSOV2                                                          
         SPACE                                                                  
MISSOVX  B     DEMUNX                                                           
         EJECT                                                                  
* SUB-ROUTINE TO ADD THE ESTIMATED HOMES OVERRIDE ELEMENTS                      
*                                                                               
HOMOVER  NTR1                                                                   
         LA    R2,UNDEMS           POINTER TO INPUT                             
         LA    R5,DUB2             POINTER TO EXISTING VALUES                   
         LA    R6,DEMLIST          POINTER TO DEMO LIST                         
         MVI   RECALC,NO           INITIALIZE RATING RE-CALC. FLAG              
         SPACE                                                                  
HOMOV2   CLI   0(R6),X'FF'         TEST FOR E-O-L                               
         BE    HOMOV6                                                           
         CLC   0(2,R2),0(R5)       TEST FOR OVERRIDE                            
         BE    HOMOV4              NO CHANGE                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+1(3),0(R6)                                                  
         GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(X'DD',(R4)),(5,WORK)                
         CLI   1(R6),C'R'          TEST FOR RATING                              
         BNE   *+16                YES                                          
         CLI   0(R2),X'FF'         RESET RATING DEMO'S                          
         BE    HOMOV4                                                           
         B     *+14                                                             
*                                                                               
         OC    0(2,R2),0(R2)       TEST FOR ZERO INPUT                          
         BZ    HOMOV4              JUST DELETE OLD OVERRIDE                     
*                                                                               
         CLC   NUKDATE,=XL2'B32B'  SEP11/89                                     
         BNL   HOMOV3              ALOWED TO OVERRIDE GUARENTEE BUYS            
         TM    UNBITOPT,X'40'      IF PACKAGE GUART. NO CHG ALLOWED             
         BZ    HOMOV3                                                           
         MVI   UNERROR,PGNOSHR                                                  
         MVI   0(R2),X'FD'         INDICATE WHICK IN ERROR                      
         B     DEMUNX                                                           
*                                                                               
HOMOV3   XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVMOD(2),1(R6)    DEMO MODIFIER/NUMBER                         
         MVC   NUOVVAL+2(2),0(R2)  VALUE                                        
         BAS   RE,SETPRE                                                        
         BAS   RE,DEMPUTEL                                                      
         OI    NUACTWHY,X'08'      LAST ACTIVITY-DEMO OVERRIDE                  
         CLI   1(R6),C'R'          TEST FOR RATING                              
         BE    *+8                 YES                                          
         MVI   RECALC,YES          FOR SHARE OR HUT, FORCE RECALC               
         SPACE                                                                  
HOMOV4   LA    R2,2(R2)            BUMP INPUT POINTER                           
         LA    R5,2(R5)            BUMP RECORD VALUE POINTER                    
         LA    R6,3(R6)            BUMP DEMLIST POINTER                         
         B     HOMOV2                                                           
         SPACE                                                                  
HOMOV6   CLI   RECALC,YES                                                       
         BNE   HOMOVX                                                           
         XC    WORK,WORK                                                        
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVMOD(2),RATING+1                                              
         BAS   RE,SETPRE                                                        
         SR    R0,R0               RATING = SHARE X HUT                         
         ICM   R0,3,UNSHR          GET INPUT SHARE                              
         SR    R1,R1                                                            
         ICM   R1,3,UNHUT          AND HUT                                      
         MR    R0,R0                                                            
*        TM    NUOVPRE,X'82'       CHECK 2 DECIMAL PLACES                       
*        BO    HOMOV7              ROUND ACCORDINGLY                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,NUOVVAL+2                                                     
         B     HOMOV9                                                           
*--FOR RATINGS IN HUNDREDS                                                      
*OMOV7   AH    R1,=H'50'                                                        
*        D     R0,=F'100'                                                       
*        STH   R1,NUOVVAL+2                                                     
*                                                                               
HOMOV9   XC    WORK+20(5),WORK+20                                               
         MVC   WORK+21(3),RATING                                                
         GOTO1 VHELLO,DMCB2,(C'D',FILAREA),(X'DD',(R4)),(5,WORK+20)             
         BAS   RE,DEMPUTEL                                                      
         SPACE                                                                  
HOMOVX   B     DEMUNX                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R3 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R5,4(R3)            MODIFIER                                     
         LA    R6,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
*                                                                               
SETP20   CLC   0(1,R5),0(R6)                                                    
         BE    SETP40                                                           
         LA    R6,2(R6)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R3),1(R6)                                                    
         B     DEMUNX                                                           
         EJECT                                                                  
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
FILAREA  DC    CL8'UNTFIL'                                                      
* ESTIMATED HOMES DEMO LIST                                                     
*                                                                               
DEMLIST  DC    X'00',C'S',X'01'                                                 
         DC    X'00',C'P',X'01'                                                 
RATING   DC    X'00',C'R',X'01'                                                 
         DC    X'FF'                                                            
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYPARM   DS    A                                                                
MYRELO   DS    A                                                                
MYAWORK  DS    A                                                                
SAVEREG  DS    A                   COMMAND ROUTINES RETURN POINT                
SAVEREG1 DS    A                   VALIDATION ROUTINES RETURN POINT             
COMMAND  DS    X                                                                
DAYNO    DS    X                                                                
DAYCODE  DS    X                   DAY CODE FOR DAY-TIME POINTER                
RECALC   DS    C                   FLAG FOR RE-CALCULATING HOMES RATING         
MINUSSW  DS    C                   MINUS COST INPUT (Y/N)                       
FREEZESW DS    C                   FREEZE PRODUCT ALLOCATION (Y/N)              
*                                                                               
KEYDATE  DS    XL2                 PASSIVE POINTER DATE                         
KEYSUB   DS    X                   PASSIVE POINTER SUB-LINE                     
KEYTIME  DS    X                   PASSIVE POINTER SQH                          
KEYDAY   DS    X                   X'94' POINTER DAY VALUE                      
*                                                                               
OLDDATE  DS    CL6                 ORIGINAL UNIT DATE (YYMMDD)                  
NEWDATE  DS    CL6                 UNIT DATE AFTER CHANGE (YYMMDD)              
MONDATE  DS    CL6                 MONDAY OF ORIGINAL WEEK                      
SUNDATE  DS    CL6                 SUNDAY OF ORIGINAL WEEK                      
*                                                                               
         DS    0F                                                               
MISSELS  DS    0XL24                                                            
AMSUNVEL DS    A                   A(MISSING RECORD UNIVERSE ELEMENT)           
AMSVPHEL DS    A                   A(MISSING RECORD VPH ELEMENT)                
AMSHOMEL DS    A                   A(MISSING RECORD HOMES ELEMENT)              
AMSBKEL  DS    A                   A(MISSING RECORD BOOK ELEMENT)               
AMSPGEL  DS    A                   A(MISSING REC PACKAGE GUARANTEE EL)          
AMSDGEL  DS    A                   A(MISSING REC DEMO GUARANTEE EL)             
*                                                                               
         DS    0D                                                               
UNIVBLK  DS    XL(L'GUVBLOCK)      GETNUN BLOCK AREA                            
UNIVEL   DS    CL180               UNIVERSE ELEMENT AREA                        
*                                                                               
IOA      DS    CL600                                                            
         DS    D                                                                
LOCLEN   EQU   *-MYPARM                                                         
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SETDEF   EQU   X'01'               ROUTINE SETS DEFAULT VALUE                   
NOEXTR   EQU   X'02'               INPUT EXTRACTION OVERRIDE                    
PERIOD   EQU   C'.'                                                             
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENUNIV                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENUNIV                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPTRNTSUPP                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPTRNTSUPP                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* SPTRNFEED (FEEDRECD)                                                          
         PRINT OFF                                                              
FEEDRECD DSECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENMKG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENREP (REPRECD)                                                            
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NEGETNUND                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGETNUND                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NEGETHUTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGETHUTD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NEINTGBLK                                                                     
*        PRINT OFF                                                              
       ++INCLUDE NEINTGBLK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SCHTB35A  05/01/02'                                      
         END                                                                    
