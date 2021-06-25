*          DATA SET DDPANACEA  AT LEVEL 110 AS OF 09/14/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PANACEAA                                                                 
*INCLUDR PANLMOD(PAN#1)              *** NOTE: DDNAME PANLMOD IN LINK           
*INCLUDR PANLMOD(PAM)                *** NOTE: DDNAME PANLMOD IN LINK           
*ENTRY PANO111                                                                  
         TITLE 'PANACEA - PAN#1 USER EXIT - MVS'                                
PANEXIT  CSECT                                                                  
*                                                                               
         SYSSTATE ARCHLVL=3                                                     
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
* AFTER A PAN#1 ADD/LEVEL/UPDATE SEQ THE FOLLOWING CONTROL CARDS WILL           
* CAUSE THE APPROPRIATE DATA TO BE CONDITIONALLY WRITTEN TO THE PAN             
* WORK AND PUNCH FILES.                                                         
*                                                                               
* &&ASSEMBLY   ASSEMBLY                                                         
* &&CATALP X.. ASSEMBLY & CATALOGUE OBJECT DECK TO PAN LIB (RM...)              
* &&CATALR X.. ASSEMBLY & CATALOGUE OBJECT DECK TO LOAD LIBRARY                 
* &&PHASE X... ASSEMBLY & LINK EDIT WITH &&LINKEDT STATEMENTS                   
*                                                                               
         REQUS ,                                                                
         USING *,RB                PROGRAM BASE (W/ LONG DISPLACEMENTS)         
         STM   RE,RC,12(RD)        SAVE PANVALET'S REGISTERS                    
         LR    RB,RF                                                            
         LR    RC,RD                                                            
         LARL  RD,SAVEAREA                                                      
         ST    RC,4(RD)                                                         
         ST    RD,8(RC)                                                         
*                                                                               
         USING COMMON,RC                                                        
         LARL  RC,COMMON           A(LITERALS), COMMON STORAGE, ETC.            
*                                                                               
         ST    R1,APARM            R2=A(PAN RECORD)                             
         LM    R2,R3,0(R1)         R3=A(PAN IOCODES)                            
         CLI   FRSTTIME,C'Y'                                                    
         JNE   PANNEXT                                                          
         EJECT                                                                  
         MVI   FRSTTIME,C'N'                                                    
*                                                                               
         LARL  R6,INCLS                                                         
         OPEN  ((R6),OUTPUT)       OPEN DATASET WITH INCLUDE BOOKNAMES          
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL OPEN                            
*                                                                               
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =CL4'ACEE',0(RF)         VALID ACEE?                             
         JE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         MVC   RACFUSER,(ACEEUSRI-ACEE)(RF)                                     
*                                                                               
         MVC   PRCNAME,SPACES      GET PROCEDURE NAME FROM DDS DD STAT          
         LA    R5,DUB                                                           
         EXTRACT (R5),FIELDS=TIOT                                               
         L     R5,DUB                                                           
         LA    R5,24(R5)                                                        
PANFRS1A CLI   0(R5),0             TEST FOR END OF TIOT TABLE                   
         JE    PANFRS2                                                          
         CLC   4(3,R5),=C'DDS'     TEST DDNAME=DDS.....                         
         JE    PANFRS1B                                                         
         LLC   R0,0(R5)            GET LENGTH OF THIS ENTRY                     
         AR    R5,R0                                                            
         J     PANFRS1A                                                         
PANFRS1B MVC   PRCNAME,4(R5)       SAVE PROCEDURE NAME                          
         MVC   PRCNAME(3),=C'PAN'                                               
*                                                                               
PANFRS2  LARL  R5,PRCTBL           SEARCH FOR PROCEDURE NAME IN TABLE           
         USING PRCTBLD,R5                                                       
PANFRS2A DS    0H                                                               
         CLC   PRCNAME,PRCTBLNM    PROC NAME                                    
         JE    PANFRS3                                                          
         LA    R5,PRCTBLLQ(R5)                                                  
         CLI   0(R5),0                                                          
         JNE   PANFRS2A                                                         
         DROP  R5                                                               
*                                                                               
         LA    R5,ACTTBL           SET ALL ACTIONS VALID FOR UNKNOWN            
         USING ACTTBLD,R5                                                       
         OI    ACTFLAGS,ACTFLAGS_ACTION_AVAILABLE                               
         LA    R5,ACTTBLLQ(R5)                                                  
         CLI   0(R5),0                                                          
         JNE   *-12                                                             
         MVC   ACTDFLT,ACTTBL+(ACTNAME-ACTTBLD) FIRST ACTION IS DEFAULT         
         J     PANFRS4                                                          
         DROP  R5                                                               
*                                                                               
PANFRS3  DS    0H                                                               
         USING PRCTBLD,R5                                                       
         MVC   PRCNUM,PRCTBL#      R5=A(PROC DATA FOR THIS PROC)                
         MVC   PRCFLAG,PRCFLAGS                                                 
         LA    R6,PRCTBLLQ(R5)                                                  
         LA    R5,PRCACTS          POINT TO FIRST ACTION NUM FOR PROC           
         SR    R6,R5                                                            
         LA    R0,ACTTBLLQ                                                      
         STH   R0,DUB                                                           
         SR    RF,RF                                                            
PANFRS3A SR    RE,RE               GET NEXT ACTION NUMBER                       
         ICM   RE,1,0(R5)                                                       
         JZ    PANFRS4                                                          
         BCTR  RE,0                                                             
         MH    RE,DUB                                                           
         LA    RE,ACTTBL(RE)                                                    
         USING ACTTBLD,RE                                                       
         OI    ACTFLAGS,ACTFLAGS_ACTION_AVAILABLE                               
         LTR   RF,RF                                                            
         JNZ   *+14                                                             
         LA    RF,1                                                             
         MVC   ACTDFLT,ACTNAME                                                  
         LA    R5,1(R5)                                                         
         BRCT  R6,PANFRS3A                                                      
         DROP  RE                                                               
         DROP  R5                                                               
*                                                                               
PANFRS4  DS    0H                                                               
         XC    PACB(PACBLQ),PACB   CLEAR PAM CONTROL BLOCK                      
         MVI   PACBFUNC,C'O'       SET OPEN FUNCTION CODE                       
         L     RF,=V(PAM)                                                       
         CALL  (15),(PACB,PANDD1,NOENTRY),VL                                    
         LTR   RF,RF               OPEN SUCCESSFUL                              
         JZ    *+6                 YES.. CONTINUE                               
         DC    H'0'                                                             
*                                                                               
         LA    R1,AINFRBLK         A(DYNAMIC INF. RETRIEVAL BLOCK)              
         DYNALLOC ,                                                             
         LTR   RF,RF               IS PANDD11 ALLOCATED?                        
         JZ    *+14                YES: OPEN IT                                 
         XC    PANDD11,PANDD11     NO: REMEMBER THAT WE DON'T HAVE IT           
         J     PANFRS6             DON'T TRY TO OPEN PANDD11                    
*                                                                               
         XC    PACB_11(PACBLQ),PACB_11  CLEAR PANDD11 PAM CONTROL BLOCK         
         MVI   PACBFUNC_11,C'O'    SET OPEN FUNCTION CODE                       
         L     RF,=V(PAM)          OPEN PANDD11                                 
         CALL  (15),(PACB_11,PANDD11,NOENTRY),VL                                
         LTR   RF,RF               PANDD11 OPEN SUCCESSFUL?                     
         JZ    *+10                YES.. CONTINUE                               
         XC    PANDD11,PANDD11     NO: REMEMBER THAT WE DON'T HAVE IT           
*                                                                               
PANFRS6  DS    0H                                                               
         DATE  DUB,FUNNY=NO        CONSTRUCT COMPILE DATE                       
         MVC   COUNTRY,DUB+6       SAVE COUNTRY                                 
         MVI   SAVCDATE+5,C'/'     YEAR DELIMITER                               
         MVC   SAVCDATE+6(2),DUB   YY                                           
         MVC   SAVCDATE+3(2),DUB+4 DD                                           
         MVC   HALF,DUB+2          MM                                           
         PACK  DUB,HALF                                                         
         CVB   R1,DUB              R1 = MONTH NUMBER                            
         BCTR  R1,0                MONTHS ARRAY IS ONE-BASED                    
         MHI   R1,3                3 CHARACTERS IN MONTH ABBREVIATION           
         LA    R1,MONTHS(R1)                                                    
         MVC   SAVCDATE(3),0(R1)   MONTH IN MMM FORMAT                          
*                                                                               
         THMS  ,                                                                
         ST    R1,FULL             R1=0HHMMSS+                                  
         OI    FULL+3,X'0F'                                                     
         UNPK  DUB(6),FULL         CONSTRUCT COMPILE TIME                       
         MVC   SAVCTIME(2),DUB     HOURS                                        
         MVI   SAVCTIME+2,C':'                                                  
         MVC   SAVCTIME+3(2),DUB+2 MINUTES                                      
*                                                                               
PANNEXT  CLI   1(R3),C'1'                                                       
         JNE   RETURN              EXIT IF NOT PAN#1                            
         CLI   0(R3),C'1'                                                       
         JE    BRC                 PAN#1 BEFORE READ CARD                       
         CLI   0(R3),C'2'                                                       
         JE    ARC0                PAN#1 AFTER READ CARD                        
         CLI   0(R3),C'3'                                                       
         JE    BPT                 PAN#1 BEFORE PRINT                           
         CLI   0(R3),C'5'                                                       
         JE    BWW                 PAN#1 BEFORE WRITE WORK                      
         CLI   0(R3),C'6'                                                       
         JE    URR                 USER REQUESTED RETURN                        
         CLI   0(R3),C'7'                                                       
         JE    EOF                 PAN#1 EOF                                    
         CLI   0(R3),C'8'                                                       
         JE    EOJ                 PAN#1 EOJ                                    
*                                                                               
RETURN   DS    0H                                                               
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BSM   0,RE                                                             
         EJECT                                                                  
BPT      L     R4,AINCLSTX         R4 = A(NEXT AVAILABLE TABLE ENTRY)           
         CLC   =C'++INCLUDE ',8(R2)                                             
         JNE   *+12                                                             
         LA    R5,18(R2)           R5 = A(++INCLUDED BOOK NAME)                 
         J     BPT90                                                            
*                                                                               
         CLC   =C'PUNCH,RM',9(R2)                                               
         JNE   *+12                                                             
         LA    R5,15(R2)           R5 = A(*INCLUDED BOOK NAME)                  
         J     BPT90                                                            
*                                                                               
         J     RETURN                                                           
*                                                                               
BPT90    MVC   0(L'INCLST,R4),0(R5) PUT BOOK NAME IN TABLE                      
         AHI   R4,L'INCLST                                                      
         ST    R4,AINCLSTX         UPDATE TABLE POINTER                         
         L     R4,INCLCNT                                                       
         AHI   R4,1                                                             
         ST    R4,INCLCNT                                                       
         CHI   R4,MAXINCLQ         DON'T OVERFLOW THE TABLE                     
         JL    RETURN                                                           
         DC    H'0'                NEED TO INCREASE MAXINCLQ                    
         EJECT                                                                  
EOJ      DS    0H                                                               
         LARL  R6,INCLS                                                         
         ICM   R5,15,INCLCNT       R5 = NUMBER OF INCLUDED PAN BOOKS            
         JZ    EOJX                NONE!                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BOOKNAME),BOOKNAME                                        
         PUT   (R6),WORK           1ST REC IS THE BOOK BEING ASSEMBLED          
         L     R4,=A(INCLST)       R4 = A(INCLUDE NAMES TABLE)                  
EOJ20    DS    0H                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'INCLST),0(R4)                                             
         PUT   (R6),WORK           PUT BOOKNAME TO RELOS DATASET                
         AHI   R4,L'INCLST         BUMP TO NEXT ENTRY                           
         BRCT  R5,EOJ20                                                         
*                                                                               
EOJX     CLOSE ((R6))                                                           
         J     RETURN                                                           
         EJECT                                                                  
*                                                                               
* PAN#1 BEFORE A WRITE TO WORK FILE                                             
*                                                                               
BWW      TM    WFLAG,X'01'         EXIT IF NOT WRITING SOURCE BOOK              
         JZ    RETURN                                                           
*                                                                               
         CLC   0(3,R2),=C'*&&&&'   PROCESS *&& CARDS (INCLUDE IF "Y")           
         JE    *+14                                                             
         CLC   0(3,R2),=C'*&&!'    PROCESS *&! CARDS (INCLUDE IF "N")           
         JNE   BWW0N                                                            
*                                                                               
         CLI   8(R2),C' '          BLANK DELIMITER IN COLUMN 9?                 
         JNE   BWW0B               NO: INVALID                                  
         CLC   5(3,R2),=C'*&&&&'   IS IT A SINGLE *&&XX*&& CARD?                
         JE    BWW0I               YES                                          
         CLC   5(3,R2),=C'*&&!'    IS IT A SINGLE *&!XX*&! CARD?                
         JE    BWW0I               YES                                          
         CLI   14(R2),C' '         IN CASE IT'S A SET/START/END...              
         JNE   BWW0B               ...CONFIRM BLANK DELIMITER IN COL 15         
*                                                                               
         CLC   3(5,R2),SPACES      UNNAMED *&& CARD                             
         JNE   BWW0E                                                            
         CLC   9(5,R2),=C'SET  '   CHECK FOR SET CARD                           
         JE    BWW0C                                                            
         CLC   9(5,R2),SPACES      CHECK FOR END DELETE MODE                    
         JE    *+14                                                             
         CLC   9(5,R2),=C'END  '                                                
         JNE   BWW0B                                                            
*                                                                               
BWW0A1   LH    RE,INCLV            DECR LEVEL AT END OF INCLUDE                 
         LA    RF,INCCHECK(RE)                                                  
         CLC   0(3,R2),=C'*&&!'    ASKING TO END A NEGATIVE INCLUDE?            
         JNE   *+12                NO                                           
         CLI   0(RF),C'N'          YES: CONFIRM CLAUSE STARTED WITH *&!         
         J     *+8                                                              
         CLI   0(RF),C'Y'          O/W, CONFIRM CLAUSE STARTED WITH *&&         
         JNE   BWW0B               UNBALANCED: FORCE ERROR                      
*                                                                               
         SHI   RE,1                                                             
         JM    BWW0B               ERROR IF TOO LOW                             
         STH   RE,INCLV                                                         
*                                                                               
BWW0A2   MVI   INCSW,C'Y'          RESET INCLUDE SWITCH                         
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   INCTBL(0),INCCHECK  ALL VALUES SATISFACTORY TO INCLUDE?          
         JE    *+8                 YES                                          
         MVI   INCSW,C'N'                                                       
         J     RETURN                                                           
*                                                                               
BWW0B    XC    INCLV,INCLV         RESET LEVEL IF INVALID *&& CARD              
         MVI   INCSW,C'Y'                                                       
BWW0B1   MVI   0(R2),C' '          FORCE ASSEMBLY SYNTAX ERROR                  
         J     RETURN                                                           
*                                                                               
BWW0C    CLI   INCSW,C'Y'          DONT PROCESS SET IF IN DELETE MODE           
         JE    *+14                                                             
         MVC   51(20,R2),=C'*** SET BYPASSED ***'                               
         J     RETURN                                                           
*                                                                               
         MVC   SETDATAC,SPACES                                                  
         MVC   SETDATAC(56),15(R2)                                              
*                                                                               
BWW0D    DS    0H                                                               
         MVI   SETFRSW,SET_VIA_SOURCE   VALUE SET WITHIN SOURCE CODE            
         BRAS  RA,SETDATA                                                       
         CLI   SETDATAC,0          CHECK FOR ERRORS                             
         JE    BWW0B1                                                           
         J     RETURN                                                           
*                                                                               
BWW0E    CLC   9(5,R2),SPACES      NAMED *&&CARD                                
         JE    BWW0F                                                            
         CLC   9(5,R2),=C'START'   ALLOW NAMED START                            
         JE    BWW0F                                                            
         CLC   9(5,R2),=C'END  '   ALLOW NAMED END                              
         JE    BWW0A1                                                           
         CLC   9(5,R2),=C'SET  '                                                
         JNE   BWW0B                                                            
*                                                                               
         CLI   15(R2),C'N'         ALLOW NAMED SET                              
         JE    *+12                                                             
         CLI   15(R2),C'Y'                                                      
         JNE   BWW0B1                                                           
         CLI   16(R2),C' '                                                      
         JNE   BWW0B1                                                           
         CLI   INCSW,C'Y'          DONT PROCESS SET IF IN DELETE MODE           
         JE    *+14                                                             
         MVC   51(20,R2),=C'*** SET BYPASSED ***'                               
         J     RETURN                                                           
*                                                                               
         MVC   SETDATAC,SPACES     CONVERT SHORT TO FULL SYNTAX                 
         MVC   SETDATAC(5),3(R2)                                                
         LA    R7,SETDATAC                                                      
         CLI   0(R7),C' '                                                       
         JE    *+12                                                             
         LA    R7,1(R7)                                                         
         J     *-12                                                             
         MVI   0(R7),C'='                                                       
         MVC   1(1,R7),15(R2)                                                   
         J     BWW0D                                                            
*                                                                               
BWW0F    LARL  R7,SETAREA          START OF NEW INCLUDE LEVEL                   
         USING SETAREAD,R7                                                      
BWW0FA   CLI   0(R7),C' '                                                       
         JE    BWW0G                                                            
         CLI   0(R7),0                                                          
         JE    BWW0G                                                            
         CLC   SETNAME,3(R2)                                                    
         JE    BWW0GA              USE TABLE VALUE IF FOUND                     
         LA    R7,SETAREAQ(R7)                                                  
         J     BWW0FA                                                           
*                                                                               
BWW0G    LARL  R7,SETAREA          USE ALL VALUE IF NOT FOUND                   
BWW0GA   LH    RE,INCLV                                                         
         LA    RE,1(RE)                                                         
         STH   RE,INCLV            BUMP INCLUDE LEVEL                           
         LA    RF,INCCHECK(RE)                                                  
         LA    RE,INCTBL(RE)                                                    
         CLI   0(RE),X'FF'                                                      
         JE    BWW0B               ERROR IF TOO HIGH                            
*                                                                               
         MVI   0(RF),C'Y'          ASSUME IT'S A POSITIVE INCLUDE               
         CLC   0(3,R2),=C'*&&!'                                                 
         JNE   *+8                                                              
         MVI   0(RF),C'N'          IT'S A NEGATIVE INCLUDE                      
         MVC   0(1,RE),SETVALUE                                                 
         LH    RE,INCLV                                                         
         J     BWW0A2              GO SET INCSW FROM INCTBL VALUES              
*                                                                               
BWW0I    CLI   INCSW,C'Y'          PROCESS SINGLE *&&XX*&& CARD                 
         JNE   RETURN                                                           
         CLI   3(R2),C' '                                                       
         JE    BWW0B1                                                           
         CLC   0(3,R2),5(R2)                                                    
         JNE   BWW0B1              CAN'T MIX BOTH *&& AND *&!                   
         LARL  R7,SETAREA                                                       
BWW0J    CLI   0(R7),C' '          SEARCH SET TABLE FOR TWO CHR ID              
         JE    BWW0L                                                            
         CLI   0(R7),0                                                          
         JE    BWW0L                                                            
         CLC   SETNAME2,3(R2)                                                   
         JE    *+12                                                             
         LA    R7,SETAREAQ(R7)                                                  
         J     BWW0J                                                            
         CLC   SETNAME2_BLANKS,SPACES  IGNORE ENTRIES WITH LENGTH > 2           
         JNE   *-14                                                             
*                                                                               
BWW0K    DS    0H                                                               
         CLC   0(3,R2),=C'*&&!'    IS IT A NEGATIVE INCLUDE?                    
         JNE   *+12                NO                                           
         CLI   SETVALUE,C'N'       YES: IS IT TO BE INCLUDED?                   
         J     *+8                                                              
         CLI   SETVALUE,C'Y'       IS IT TO BE INCLUDED                         
         JNE   RETURN              NO- LEAVE AS COMMENT                         
*                                                                               
         MVC   DUB,0(R2)           YES-REMOVE *&&XX*&&                          
         MVC   0(8,R2),SPACES      AND PUT XX IN COLS 70-71 IF AVAIL            
         TM    PRCFLAG,PRCFLAGS_DFSORT                                          
         JNO   *+18                                                             
         MVC   72(4,R2),DUB+1      FOR DFSORT SOURCE, USE COLS 73-77            
         MVI   76(R2),C' '                                                      
         J     RETURN                                                           
         CLC   68(4,R2),SPACES                                                  
         JNE   *+10                                                             
         MVC   69(2,R2),DUB+3                                                   
         J     RETURN                                                           
*                                                                               
BWW0L    LARL  R7,SETAREA          IF NOT FOUND LOOK AT ALL VALUE               
         J     BWW0K                                                            
         DROP  R7                                                               
*                                                                               
BWW0N    DS    0H                                                               
         CLI   INCSW,C'Y'          ARE WE INCLUDING CARDS                       
         JE    *+14                YES                                          
         MVC   0(2,R2),=C'**'      COMMENT IT OUT (AND MAKE IT OBVIOUS)         
         MVI   71(R2),C' '                                                      
*                                                                               
         CLC   =C'*CATALP ',0(R2)  IS THIS A *CATALP CARD?                      
         JNE   *+8                                                              
         MVI   CATALFLG,C'Y'       YES                                          
*                                                                               
         CLC   =C'*$PANAPT01S$',0(R2)   CHECK FOR PANAPT EYE-CATCHER            
         JNE   *+8                 "SRCE" LIBCODE FLOWER BOX PRESENT?           
         MVI   FLOWRFLG,C'Y'       YES                                          
*                                                                               
         CLI   0(R2),C'*'          SKIP COMMENT CARDS                           
         JE    BWW2                                                             
         CLC   0(80,R2),SPACES     SKIP BLANK CARDS                             
         JE    BWW2                                                             
*                                                                               
         CLC   0(2,R2),=C'/*'      EOF ?                                        
         JNE   BWW0P                                                            
         CLC   2(78,R2),SPACES                                                  
         JNE   BWW0P                                                            
         CLI   ENDEOFSW,C'Y'       YES: FORCE END STATEMENT ON EOF?             
         JNE   BWW0P                                                            
         MVC   0(4,R2),=C' END'    YES: REPLACE /* WITH END STATEMENT           
*                                                                               
BWW0P    DS    0H                                                               
         TM    PRCFLAG,PRCFLAGS_DFSORT  DFSORT SOURCE?                          
         JO    RETURN              YES: THIS ISN'T ASSEMBLY LANGUAGE            
*                                                                               
* CHECK FOR DDS-FLAGGED INSTRUCTIONS                                            
*                                                                               
         MVC   LABELFLD,SPACES     SAVE LABEL FIELD HERE                        
         LR    R1,R2               A(START OF STATEMENT)                        
         LA    RF,LABELFLD                                                      
BWW0Q    DS    0H                                                               
         CLI   0(R1),C' '          LOOK FOR THE FIRST BLANK...                  
         JE    BWW0R               ...TO BYPASS THE NAME FIELD                  
         MVC   0(1,RF),0(R1)       MOVE IN ONE BYTE AT A TIME                   
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         J     BWW0Q                                                            
*                                                                               
BWW0R    DS    0H                                                               
         LA    R1,1(R1)            BUMP PAST THE BLANK DELIMITER                
         CLI   0(R1),C' '          THE FIRST NON-BLANK...                       
         JNE   *+12                ...IS THE OPERATION FIELD                    
         LA    R1,1(R1)                                                         
         J     *-12                                                             
*                                                                               
         MVC   OPERFLD,SPACES      SAVE OPERATION FIELD HERE                    
         LR    RE,R1               A(START OF OPERATION FIELD)                  
         LA    RF,OPERFLD                                                       
         LHI   R0,L'OPERFLD                                                     
BWW0T    DS    0H                                                               
         MVC   0(1,RF),0(RE)       MOVE IN ONE BYTE AT A TIME                   
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '          OPERATION ENDS WITH A BLANK                  
         JE    *+8                                                              
         BRCT  R0,BWW0T            OPERATION NAME CAN'T EXCEED 8 BYTES          
*                                                                               
         CLC   =C'CSECT ',OPERFLD  IBM RECOMMENDS THAT ALL CSECT STMTS          
         JE    BWW0U                HAVE LABELS (TO AVOID PRIVATE CODE)         
         CLC   =C'RSECT ',OPERFLD                                               
         JE    BWW0U                                                            
         CLC   =C'START ',OPERFLD  START IS EQUIVALENT TO FIRST CSECT           
         JNE   BWW0V                                                            
         MVC   OPERFLD,=C'CSECT   '                                             
*                                                                               
BWW0U    DS    0H                                                               
         CLC   LABELFLD,SPACES     IS THIS CONTROL SECTION LABELED?             
         JNE   *+20                YES                                          
         LARL  RF,CSECTNOL         NO: GENERATE MNOTE ERROR                     
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
*                                                                               
         CLI   RSECTSW,C'Y'        CHANGE CSECT STMTS TO RSECT STMTS?           
         JNE   *+16                                                             
         MVC   OPERFLD,=C'RSECT   ' YES                                         
         MVC   0(5,R1),OPERFLD                                                  
*                                                                               
         CLC   FSTSECTN,SPACES     SAVE FIRST CONTROL SECTION NAME              
         JNE   *+16                                                             
         MVC   FSTSECTN,LABELFLD                                                
         MVC   FSTSECTP,OPERFLD    SAVE FIRST CONTROL SECTION TYPE              
*                                                                               
         MVC   LSTSECTN,LABELFLD   SAVE LAST CONTROL SECTION NAME               
         MVC   LSTSECTP,OPERFLD    SAVE LAST CONTROL SECTION TYPE               
*                                                                               
         J     BWW00                                                            
*                                                                               
BWW0V    DS    0H                                                               
*                                                                               
*&&DO                                                                           
* NOTE: THIS CODE WAS COMMENTED OUT AFTER IBM FIXED THE BUG DESCRIBED           
*       BELOW. THE CODE IS BEING LEFT HERE AS AN EXAMPLE, IN CASE WE            
*       NEED TO DO SOMETHING SIMILAR IN THE FUTURE. -- DEIS.                    
*                                                                               
* THE COMPARE *LOGICAL* AND BRANCH/JUMP INSTRUCTIONS DON'T WORK (I.E.,          
* THE RESULTS ARE UNPREDICTABLE) WHEN SINGLE-STEPPING IN IDF.                   
* UNTIL/UNLESS IBM FIXES THIS, WE GENERATE A WARNING TO THE PROGRAMMER.         
*                                                                               
         CLC   =C'CLRB',OPERFLD                                                 
         JE    BWW0V5                                                           
         CLC   =C'CLGRB',OPERFLD                                                
         JE    BWW0V5                                                           
         CLC   =C'CLRJ',OPERFLD                                                 
         JE    BWW0V5                                                           
         CLC   =C'CLGRJ',OPERFLD                                                
         JE    BWW0V5                                                           
         CLC   =C'CLIB',OPERFLD                                                 
         JE    BWW0V5                                                           
         CLC   =C'CLGIB',OPERFLD                                                
         JE    BWW0V5                                                           
         CLC   =C'CLIJ',OPERFLD                                                 
         JE    BWW0V5                                                           
         CLC   =C'CLGIJ',OPERFLD                                                
         JE    BWW0V5                                                           
         J     BWW0V7                                                           
*                                                                               
BWW0V5   DS    0H                                                               
         LARL  RF,SETAREA          FIND "ONLINE" FLAG IN SETAREA                
         USING SETAREAD,RF                                                      
BWW0V20  DS    0H                                                               
         CLC   =C'ONLIN',SETNAME   &&ONLIN OPTION?                              
         JNE   BWW0V30             NO: SKIP TO NEXT OPTION IN TABLE             
         CLI   SETVALUE,C'Y'       ONLIN=Y (ONLINE-ONLY MODULE) ?               
         JE    BWW0V7              YES: OKAY                                    
         CLI   SETVALUE,C'N'       ONLIN=N (NOT ONLINE-ONLY MODULE) ?           
         JE    BWW0V40             YES: GENERATE MNOTE ERROR                    
         DC    H'0'                INVALID VALUE FOR &&ONLIN OPTION             
BWW0V30  DS    0H                                                               
         CLI   0(RF),C' '          EOT?                                         
         JH    *+6                 NO                                           
         DC    H'0'                IMPOSSIBLE: "ONLIN" ENTRY IS PRESET!         
         LA    RF,SETAREAQ(RF)     BUMP TO NEXT TABLE ENTRY                     
         J     BWW0V20                                                          
         DROP  RF                                                               
*                                                                               
BWW0V40  LARL  RF,NO_IDF_SUPPORT_FOR_COMPARE_AND_BRANCH                         
         MVC   48(6,RF),OPERFLD    PUT INSTRUCTION MNEMONIC IN MNOTE            
         MVC   65(1,RF),OPERFLD    SUGGEST ARITHMETIC EQUIVALENT                
         MVC   66(4,RF),OPERFLD+2  (E.G., CIJNE INSTEAD OF CLIJNE)              
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
*                                                                               
BWW0V7   DS    0H                                                               
*&&                                                                             
         CLC   =C'BAL ',OPERFLD    BRANCH AND LINK INSTRUCTIONS...              
         JE    *+14                ...ARE NO LONGER PERMITTED                   
         CLC   =C'BALR ',OPERFLD                                                
         JNE   *+20                                                             
         LARL  RF,NOBALMSG         GENERATE MNOTE ERROR                         
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
*                                                                               
* IF IT LOOKS LIKE A DFSORT "SORT FIELDS" CARD, CHECK FOR A "WORK=1"            
* PARAMETER, AND GENERATE A SEVERITY 8 ERROR IF IT ISN'T THE LAST               
* PARAMETER ON THE CARD. "WORK=1" IS OBSOLETE AND IS STRIPPED OFF BY            
* DDSORTER, BUT ANYTHING THAT FOLLOWS "WORK=1" WILL ALSO GET STRIPPED           
* OFF, AND THAT'S BAD. (NOTE THAT WE DON'T CATCH ALL OCCURRENCES OF             
* THE ERROR HERE, BUT WE DO GET MOST OF THEM.)                                  
*                                                                               
         CLC   =C' DC    CL80''SORT FIELDS=(',8(R2)                             
         JNE   *+12                                                             
         LA    R1,24(R1)           BUMP PAST SUSPICIOUS STRING                  
         J     BWW0V50                                                          
         CLC   =C' DC    C''SORT FIELDS=(',8(R2)                                
         JNE   BWW0V55             CARD IS OKAY                                 
         LA    R1,21(R1)           BUMP PAST SUSPICIOUS STRING                  
BWW0V50  CLC   =C',WORK=1,',0(R1)  WORK=1 ISN'T THE LAST PARAMETER?             
         JNE   *+20                                                             
         LARL  RF,WORK_PARM_ON_SORTCARD  RIGHT: GENERATE MNOTE ERROR            
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER                       
         CLI   0(R1),C' '          END OF RECORD (PROBABLY)?                    
         JE    BWW00               YES: IT'S OKAY                               
         J     BWW0V50             NO: KEEP LOOKING FOR WORK=1                  
*                                                                               
BWW0V55  DS    0H                                                               
         CLC   =C'EX ',OPERFLD     EX INSTRUCTIONS MAY NOT HAVE A...            
         JNE   BWW0W               ...TARGET OF *+4 (IDF RESTRICTION)           
         LA    R1,3(R1)            BUMP PAST THE BLANK DELIMITER                
         CLI   0(R1),C','          FIND THE COMMA BETWEEN THE OPERANDS          
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
         SHI   R1,3                                                             
         CLC   =C' R0,',0(R1)      IS THE FIRST OPERAND R0 ?                    
         JNE   *+20                NO                                           
         LARL  RF,BAD_R0           YES: GENERATE MNOTE ERROR                    
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
         LA    R1,3(R1)            BUMP UP TO COMMA BETWEEN OPERANDS            
         CLC   =C'*+4 ',1(R1)      IS THE SECOND OPERAND *+4 ?                  
         JNE   BWW00               NO: IT'S OKAY                                
         J     BWW0Y10             YES: CHECK "ONLINE" FLAG                     
*                                                                               
BWW0W    DS    0H                                                               
         CLC   =C'EXRL ',OPERFLD   EXRL INSTRUCTIONS MAY NOT HAVE A...          
         JNE   BWW0Y               ...TARGET OF *+6 (IDF RESTRICTION)           
         LA    R1,5(R1)            BUMP PAST THE BLANK DELIMITER                
         CLI   0(R1),C','          FIND THE COMMA BETWEEN THE OPERANDS          
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
         SHI   R1,3                                                             
         CLC   =C' R0,',0(R1)      IS THE FIRST OPERAND R0 ?                    
         JNE   *+20                NO                                           
         LARL  RF,BAD_R0           YES: GENERATE MNOTE ERROR                    
         MVC   MNOTE,0(RF)                                                      
         J     BWW0Z                                                            
         LA    R1,3(R1)            BUMP UP TO COMMA BETWEEN OPERANDS            
         CLC   =C'*+6 ',1(R1)      IS THE SECOND OPERAND *+6 ?                  
         JNE   BWW00               NO: IT'S OKAY                                
         J     BWW0Y10             YES: CHECK "ONLINE" FLAG                     
*                                                                               
BWW0Y    DS    0H                                                               
         CLC   =C'BXLE ',OPERFLD   BXLE RX,RX,* AND...                          
         JE    BWW0Y05                                                          
         CLC   =C'JXLE ',OPERFLD   JXLE RX,RX,* AND...                          
         JE    BWW0Y05                                                          
         CLC   =C'BRXLE ',OPERFLD  BRXLE RX,RX,* ARE ALSO PROBLEMATIC           
         JNE   BWW00                                                            
*                                                                               
BWW0Y05  DS    0H                                                               
         CLI   0(R1),C' '          FIND FIRST BLANK AFTER THE MNEMONIC          
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
         LA    R1,1(R1)            BUMP PAST THE BLANK DELIMITER                
         CLI   0(R1),C','          FIND THE COMMA AFTER R(1)                    
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
         LA    R1,1(R1)            BUMP PAST THE COMMA                          
         CLI   0(R1),C','          FIND THE COMMA AFTER R(2)                    
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
         CLC   =C'* ',1(R1)                                                     
         JNE   BWW00               IT'S OKAY                                    
*                                                                               
BWW0Y10  LARL  RF,SETAREA          FIND "ONLINE" FLAG IN SETAREA                
         USING SETAREAD,RF                                                      
BWW0Y20  DS    0H                                                               
         CLC   =C'ONLIN',SETNAME   &&ONLIN OPTION?                              
         JNE   BWW0Y30             NO: SKIP TO NEXT OPTION IN TABLE             
         CLI   SETVALUE,C'Y'       ONLIN=Y (ONLINE-ONLY MODULE) ?               
         JE    BWW00               YES: OKAY                                    
         CLI   SETVALUE,C'N'       ONLIN=N (NOT ONLINE-ONLY MODULE) ?           
         JE    BWW0YIDF            YES: GENERATE MNOTE ERROR                    
         DC    H'0'                INVALID VALUE FOR &&ONLIN OPTION             
BWW0Y30  DS    0H                                                               
         CLI   0(RF),C' '          EOT?                                         
         JH    *+6                 NO                                           
         DC    H'0'                IMPOSSIBLE: "ONLIN" ENTRY IS PRESET!         
         LA    RF,SETAREAQ(RF)     BUMP TO NEXT TABLE ENTRY                     
         J     BWW0Y20                                                          
         DROP  RF                                                               
*                                                                               
BWW0YIDF DS    0H                                                               
         LARL  RF,BAD4IDF          GENERATE MNOTE ERROR                         
         MVC   MNOTE,0(RF)                                                      
*                                                                               
BWW0Z    DS    0H                                                               
         MVI   2(R3),C'Y'          REQUEST USER RETURN                          
         J     RETURN                                                           
*                                                                               
BWW00    DS    0H                                                               
         CLC   =C'PRINT ',OPERFLD  IS THIS A "PRINT" STATEMENT?                 
         JNE   BWW2                NO                                           
*                                                                               
         LR    R1,R2               A(START OF CARD)                             
         CLC   =C' PRINT ',0(R1)                                                
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-14                                                             
         LA    R1,7(R1)            BUMP PAST "PRINT" OPERATOR                   
         CLI   0(R1),C' '          FIND NEXT SIGNIFICANT CHARACTER              
         JNE   *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
*                                                                               
         CLI   PONSW,C'Y'          IF FORCE PRINT ON REQUIRED                   
         JNE   BWW1                                                             
         CLC   =C'OFF ',0(R1)      IS THIS A "PRINT OFF" STATEMENT ?            
         JNE   BWW1                                                             
         MVC   0(4,R2),=C'*PON'    SET TO COMMENT CARD                          
         J     BWW2                                                             
*                                                                               
BWW1     CLI   GENSW,C'Y'          IF FORCE PRINT GEN REQUIRED                  
         JNE   BWW2                                                             
         CLC   =C'NOGEN ',0(R1)    IS THIS A "PRINT NOGEN" STATEMENT ?          
         JNE   BWW2                                                             
         MVC   0(4,R2),=C'*GEN'    SET TO COMMENT CARD                          
*                                                                               
BWW2     CLC   PREFIX,SPACES       IF PREFIX SET                                
         JE    BWW2D                                                            
         CLC   PREFBOOK,SPACES     TEST BOOKNAME FILTER DEFINED                 
         JE    *+14                                                             
         CLC   PREFBOOK,THISBOOK   MATCH ON BOOKNAME                            
         JNE   BWW2D                                                            
         BRAS  RA,DOPREFIX         GO SET PREFIX                                
*                                                                               
BWW2D    CLC   0(8,R2),=C'*PREFIX='  IF THIS IS SPECIAL PREFIX CARD             
         JNE   BWW3                                                             
         MVC   PREFIX,8(R2)        SET PREFIX                                   
         MVC   PREFBOOK,SPACES                                                  
         CLI   11(R2),C'='         TEST FOR BOOKNAME FILTER                     
         JNE   BWW3                                                             
         MVC   PREFBOOK,12(R2)     SAVE IT                                      
*                                                                               
*                                  LOOK FOR PAN "SPECIAL COMMENT" CARD          
BWW3     CLC   BOOKLIT1,BOOKLIT1-SPCLCMNT(R2)                                   
         JNE   BWW3D                                                            
         CLC   BOOKLIT2,BOOKLIT2-SPCLCMNT(R2)                                   
         JNE   BWW3D                                                            
         CLC   BOOKLIT3,BOOKLIT3-SPCLCMNT(R2)                                   
         JNE   BWW3D                                                            
         MVC   THISBOOK,SPCLCMBK-SPCLCMNT(R2) SAVE THIS BOOKNAME                
*                                                                               
BWW3D    DS    0H                                                               
         CLI   0(R2),C'*'          SKIP COMMENTS                                
         JE    BWW5                                                             
         CLI   0(R2),C'&&'         SKIP MACRO SYMBOLIC PARAMETERS               
         JE    BWW5                                                             
         CLI   0(R2),C'.'          SKIP MACRO STATEMENTS                        
         JE    BWW5                                                             
*                                                                               
         CLC   =C'END ',OPERFLD    END STATEMENT?                               
         JNE   BWW4F               NO                                           
*                                                                               
         BRAS  RA,CLEARWRK         REPLACE END BY LTORG/CSECT/DC/END            
         MVC   9(11,R5),=C'DC    X''00'''                                       
         LA    R5,80(R5)                                                        
*                                                                               
         CLC   FSTSECTN,SPACES     DID WE SEE A LABELED CSECT STMT?             
         JNE   BWW3F               YES                                          
         CLI   ENDEOFSW,C'Y'       FORCED END STATEMENT GENERATED?              
         JE    BWW3F               YES                                          
         LARL  RF,NOCSECT          NO: GENERATE MNOTE ERROR                     
         MVC   0(80,R5),0(RF)                                                   
         LA    R5,80(R5)                                                        
*                                                                               
BWW3F    DS    0H                                                               
         MVC   0(8,R5),FSTSECTN                                                 
         MVC   9(5,R5),FSTSECTP    'CSECT' OR 'RSECT'                           
         LA    R5,80(R5)                                                        
         CLC   FSTSECTN,LSTSECTN   > 1 CONTROL SECTION PRESENT?                 
         JNE   *+14                IF SO, DON'T GENERATE LTORG NOW              
         MVC   9(11,R5),=C'LTORG      '                                         
         LA    R5,80(R5)                                                        
*                                                                               
         MVC   LEVELBK,BOOKNAME                                                 
*                                                                               
         CLI   SETOVFLG,C'Y'       &&SET OVERRIDE(S) FOUND?                     
         JNE   *+8                                                              
         MVI   PROMOTE_FLAG,SET_FLAG_OVERRIDE  PREVENT PANAPT PROMOTION         
*                                   (FORCE LEVEL STAMP MISMATCH)                
*                                                                               
         CLI   RSECTSW,C'Y'        CSECT STMTS CHANGED TO RSECT STMTS?          
         JNE   *+8                                                              
         MVI   PROMOTE_FLAG,RSECT_OVERRIDE     PREVENT PANAPT PROMOTION         
*                                   (FORCE LEVEL STAMP MISMATCH)                
*                                                                               
         MVC   LEVELNUM,SAVLEVEL                                                
*                                                                               
         MVC   HALF,SAVDATE        IN US, FORMAT IS MM/DD/YY                    
         MVC   LEVELDAT+3(2),SAVDATE+3                                          
         CLC   COUNTRY,=C'UK'                                                   
         JNE   *+16                                                             
         MVC   HALF,SAVDATE+3      IN UK, FORMAT IS DD/MM/YY                    
         MVC   LEVELDAT+3(2),SAVDATE                                            
         PACK  DUB,HALF                                                         
         CVB   R1,DUB              R1 = MONTH NUMBER                            
         CHI   R1,12                                                            
         JH    *+2                 MONTH IS ABOVE DECEMBER ?!?                  
         BCTR  R1,0                MONTHS ARRAY IS ONE-BASED                    
         MHI   R1,3                3 CHARACTERS IN MONTH ABBREVIATION           
         LA    R1,MONTHS(R1)                                                    
         MVC   LEVELDAT(3),0(R1)   MONTH IN MMM FORMAT                          
         MVC   LEVELDAT+6(2),SAVDATE+6   YEAR NUMBER                            
         MVC   LEVELTIM,SAVTIME    LAST UPDATE TIME                             
*                                                                               
         MVC   LEVELCUS,RACFUSER   USERID OF PERSON WHO DID COMPILE             
         MVC   LEVELCDT,SAVCDATE   COMPILE DATE                                 
         MVC   LEVELCTM,SAVCTIME   COMPILE TIME                                 
*                                                                               
         CLI   LSTAMPSW,C'N'       GENERATE LEVEL STAMP?                        
         JE    BWW4                NO                                           
*                                                                               
         MVC   9(12,R5),=C'DC    AL1(0)' FORCE THE ASSEMBLER TO...              
         LA    R5,80(R5)                 ...GENERATE THE LEVEL STAMP...         
         MVC   9(9,R5),=C'ORG   *-1'     ...AT THE *START* OF A TXT REC         
         LA    R5,80(R5)                                                        
*                                                                               
         MVC   0(LEVEL1LQ,R5),LEVELDC1  LEVEL-STAMP CARD 1                      
         LA    R5,80(R5)                                                        
*                                                                               
         MVC   0(LEVEL2LQ,R5),LEVELDC2  LEVEL-STAMP CARD 2                      
         LA    R5,80(R5)                                                        
*                                                                               
         CLI   CATALFLG,C'Y'       IS THIS A "PANACAT" MODULE?                  
         JNE   BWW4                NO                                           
         MVC   LEVELCAS,=C' AS '   PUT RELO NAME IN LEVEL STAMP                 
         MVC   LEVELCRM,=C'RM'                                                  
         LA    R1,LNKCARD+8        POINT JUST PAST "*CATALP "                   
         CLI   0(R1),C' '          POINT R1 TO FIRST CHAR IN RELO NAME          
         JH    *+12                                                             
         LA    R1,1(R1)                                                         
         J     *-12                                                             
*                                                                               
         LA    RF,LEVELCNM         RELO NAME (WITHOUT "RM")                     
BWW3H    CLI   0(R1),C'A'          ALPHANUMERIC?                                
         JNL   BWW3I               YES: OKAY                                    
         CLI   0(R1),C'$'          ALSO PERMIT PANVALET-SUPPORTED...            
         JE    BWW3I               ..."SPECIAL" CHARACTERS                      
         CLI   0(R1),C'#'                                                       
         JE    BWW3I                                                            
         CLI   0(R1),C'@'                                                       
         JE    BWW3I                                                            
         CLI   0(R1),C' '          BLANK TERMINATES THE NAME                    
         JE    BWW3J                                                            
         LA    RE,LNKCARD+8        INVALID: POINT PAST "*CATALP "...            
         MVC   0(8,RE),=C'DUMMY   '  ...AND GENERATE DUMMY RELO                 
         J     BWW3J                                                            
BWW3I    MVC   0(1,RF),0(R1)       BUILD RELO NAME ONE BYTE AT A TIME           
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         J     BWW3H                                                            
*                                                                               
BWW3J    DS    0H                                                               
         MVC   0(LEVEL3LQ,R5),LEVELDC3  LEVEL-STAMP CARD 3                      
         LA    R5,80(R5)                                                        
*                                                                               
         TM    PRCFLAG,PRCFLAGS_CATALP  ACTION CATALP?                          
         JZ    BWW4                NO: DON'T GENERATE ++AUDIT DATASET           
*                                                                               
         LARL  R6,AUDCARDS                                                      
         OPEN  ((R6),OUTPUT)       WILL CONTAIN ++AUDIT RECORDS                 
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL OPEN                            
*                                                                               
         MVC   WORK,SPACES         CONSTRUCT ++AUDIT CARD                       
         MVC   WORK(7),=C'++AUDIT'                                              
         MVC   WORK+8(L'LEVELCBK),LEVELCBK                                      
         LA    RF,WORK+8+L'LEVELCBK-1                                           
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         MVI   1(RF),C','          BOOKNAME TERMINATED BY A COMMA               
         MVI   WORK+71,C'C'        CONTINUATION CHARACTER                       
         PUT   (R6),WORK           1ST REC IS THE BOOK BEING ASSEMBLED          
*                                                                               
*                                  CONSTRUCT AUDIT DATA RECORDS                 
         MVC   WORK,SPACES         LEVEL STAMP RECORD 1                         
         MVC   WORK(4),=C'A1=\'                                                 
         MVC   WORK+4(AUDREC1Q),AUDREC1                                         
         MVI   WORK+4+AUDREC1Q,C'$'  EOR MARKER                                 
         MVC   WORK+68(4),=C'\, C'                                              
         PUT   (R6),WORK           FIELD AUDIT1V                                
         MVC   WORK,SPACES         LEVEL STAMP RECORD 2                         
         MVC   WORK(4),=C'A2=\'                                                 
         MVC   WORK+4(AUDREC2Q),AUDREC2                                         
         MVI   WORK+4+AUDREC2Q,C'$'  EOR MARKER                                 
         MVC   WORK+68(4),=C'\, C'                                              
         PUT   (R6),WORK           FIELD AUDIT2V                                
         MVC   WORK,SPACES         LEVEL STAMP RECORD 3                         
         MVC   WORK(4),=C'A3=\'                                                 
         MVC   WORK+4(AUDREC3Q),AUDREC3                                         
         MVI   WORK+4+AUDREC3Q,C'$'  EOR MARKER                                 
         MVI   WORK+68,C'\'        NO CONTINUATION ON LAST CARD                 
         PUT   (R6),WORK           FIELD AUDIT3V                                
*                                                                               
         CLOSE ((R6))                                                           
*                                                                               
BWW4     DS    0H                                                               
         CLC   FSTSECTN,LSTSECTN   > 1 CONTROL SECTION PRESENT?                 
         JE    BWW4A               NO                                           
         MVC   0(8,R5),LSTSECTN    PUT LTORG IN LAST CONTROL SECTION            
         MVC   9(5,R5),LSTSECTP    'CSECT' OR 'RSECT'                           
         LA    R5,80(R5)                                                        
         MVC   9(11,R5),=C'LTORG      '                                         
         LA    R5,80(R5)                                                        
*                                                                               
BWW4A    DS    0H                                                               
         MVC   9(8,R5),=C'DC    0D'  DOUBLEWORD ALIGN (FOR LOCTR CODE)          
         LA    R5,80(R5)                                                        
         MVC   0(80,R5),0(R2)      'END' STATEMENT                              
         LA    R5,80(R5)                                                        
         C     R5,=A(WRKAREAX)                                                  
         JNL   *+2                 INCREASE MAX_WRKAREA_ENTRIES                 
         MVI   0(R5),0                                                          
         MVI   FLUSHWRK,C'Y'       RELEASE ALL BUFFERED CARDS TO PAN            
         J     URR                                                              
*                                                                               
BWW4F    DS    0H                                                               
         CLI   71(R2),C' '         IF THIS STMT IS CONTINUED, THEN...           
         JNE   BWW5                 DON'T WRAP ACONTROL STMTS AROUND IT         
*                                                                               
         LARL  RF,TROUBLESOME_IBM_MACROS  THESE NEED ACONTROL STMTS             
         USING TROUBLED,RF                                                      
BWW4H    DS    0H                                                               
         LLC   R1,TRUBPFXL         NUMBER OF SIGNIFICANT CHARACTERS             
         BCTR  R1,0                                                             
         EXRL  R1,*+10             EXECUTED CLC ALLOWS FOR PREFIXES             
         J     *+10                                                             
         CLC   OPERFLD(0),TRUBMACR IS THIS ONE OF THE MACROS?                   
         JE    *+20                YES (OR AT LEAST, PROBABLY)                  
         LA    RF,TROUBLEQ(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         JNE   BWW4H                                                            
         J     BWW5                IT'S NOT IN THE MACRO TABLE                  
         DROP  RF                                                               
*                                                                               
         BRAS  RA,CLEARWRK         SURROUND MACRO WITH ACONTROL STMTS           
         MVC   9(14,R5),=C'PUSH  ACONTROL'                                      
         LA    R5,80(R5)                                                        
         MVC   9(42,R5),=C'ACONTROL COMPAT(NOCASE),FLAG(NOPG0,NOCONT)'          
         LA    R5,80(R5)                                                        
         MVC   0(80,R5),0(R2)      MACRO CALL                                   
         LA    R5,80(R5)                                                        
         MVC   9(14,R5),=C'POP   ACONTROL'                                      
         LA    R5,80(R5)                                                        
         C     R5,=A(WRKAREAX)                                                  
         JNL   *+2                 INCREASE MAX_WRKAREA_ENTRIES                 
         MVI   0(R5),0                                                          
         MVI   FLUSHWRK,C'Y'       RELEASE ALL BUFFERED CARDS TO PAN            
         J     URR                                                              
*                                                                               
BWW5     TM    WFLAG,X'04'+X'08'   TEST PHASE/CATAL ACTION                      
         JNZ   BWW5X                                                            
         CLI   INCSW,C'Y'          IGNORE CARDS DELETED BY *&& ACTION           
         JNE   BWW5X                                                            
         OC    SAVLEVEL,SAVLEVEL   SAVE FIRST LEVEL NUMBER                      
         JNZ   BWW5X                                                            
*                                  LOOK FOR PAN "SPECIAL COMMENT" CARD          
         CLC   BOOKLIT1,BOOKLIT1-SPCLCMNT(R2)                                   
         JNE   BWW5X                                                            
         CLC   BOOKLIT2,BOOKLIT2-SPCLCMNT(R2)                                   
         JNE   BWW5X                                                            
         CLC   BOOKLIT3,BOOKLIT3-SPCLCMNT(R2)                                   
         JNE   BWW5X                                                            
         MVC   SAVLEVEL,SPCLCML#-SPCLCMNT(R2)   PAN LEVEL NUMBER                
         MVC   SAVDATE,SPCLCMDT-SPCLCMNT(R2)    LAST UPDATE DATE                
BWW5X    DS    0H                                                               
*                                                                               
         TM    WFLAG,X'04'         PHASE SOURCE WRITE                           
         JZ    BWWE                                                             
         TM    WFLAG,X'40'         HAS FIRST *PHASE CARD BEEN FOUND             
         JO    BWWA                YES                                          
         TM    WFLAG,X'80'         TEST INITIALIZE SEARCH                       
         JO    BWW7                                                             
         OI    WFLAG,X'80'                                                      
         BRAS  RA,CLEARWRK                                                      
         XC    WCOUNT,WCOUNT                                                    
         XC    SAVLEVEL,SAVLEVEL                                                
*                                                                               
BWW7     L     R5,WRKADDR          LOOK FOR EMBEDDED PHASE CARD IN BOOK         
         CLI   0(R2),C'*'                                                       
         JNE   BWW8                                                             
         CLI   INCSW,C'Y'          IGNORE CARDS DELETED BY *&& ACTION           
         JNE   BWW8                                                             
         OC    SAVLEVEL,SAVLEVEL   SAVE FIRST LEVEL NUMBER                      
         JNZ   BWW7A                                                            
*                                  LOOK FOR PAN "SPECIAL COMMENT" CARD          
         CLC   BOOKLIT1,BOOKLIT1-SPCLCMNT(R2)                                   
         JNE   BWW7A                                                            
         CLC   BOOKLIT2,BOOKLIT2-SPCLCMNT(R2)                                   
         JNE   BWW7A                                                            
         CLC   BOOKLIT3,BOOKLIT3-SPCLCMNT(R2)                                   
         JNE   BWW7A                                                            
         MVC   SAVLEVEL,SPCLCML#-SPCLCMNT(R2)   PAN LEVEL NUMBER                
         MVC   SAVDATE,SPCLCMDT-SPCLCMNT(R2)    LAST UPDATE DATE                
*                                                                               
BWW7A    CLC   1(6,R2),=CL8'PHASE'                                              
         JE    *+14                                                             
         CLC   1(5,R2),=CL8'NAME'                                               
         JNE   BWW8                                                             
         LA    R7,LNKCARD          IF FOUND POINT TO INPUT PHASE CARD           
         CLC   6(66,R7),SPACES     DOES INPUT PHASE HAVE A NAME                 
         JNE   *+10                YES USE IT                                   
         MVC   1(71,R7),1(R2)      NO USE *PHASE CARD                           
         LA    R7,1000                                                          
         STH   R7,WCOUNT           FORCE END OF SEARCH                          
*                                                                               
BWW8     LH    R7,WCOUNT           BUMP WRITE TO WORK COUNT                     
         LA    R7,1(R7)                                                         
         STH   R7,WCOUNT                                                        
         CLC   BOOKNAME(2),=C'LK'  DONT OUTPUT ANY LK.. BOOK CARDS              
         JE    *+14                                                             
         CLC   BOOKNAME(2),=C'LM'                                               
         JNE   *+8                                                              
         MVI   0(R3),C'F'          DELETE WORK CARD                             
         CIJL  R7,100,BWWX         HAVE MAX WORK CARDS BEEN EXAMINED?           
*                                    IF NOT, CONTINUE                           
*                                                                               
         MVI   0(R5),0             SET END OF *PHASE SEARCH                     
         OI    WFLAG,X'40'                                                      
BWW9A    LA    R7,LNKCARD+5        FIND START OF PHASE CARD                     
         CLI   0(R7),C' '                                                       
         JE    *+8                                                              
         LA    R7,1(R7)                                                         
         LA    R0,20                                                            
         CLI   0(R7),C' '                                                       
         JNE   *+16                                                             
         LA    R7,1(R7)                                                         
         BRCT  R0,*-12                                                          
         J     BWW9C                                                            
*                                                                               
         LR    RE,R7               R7=A(START OF PHASE NAME)                    
         LA    R0,9                                                             
BWW9B    CLI   0(RE),C','                                                       
         JE    BWW9D                                                            
         CLI   0(RE),C'('                                                       
         JE    BWW9D                                                            
         CLI   0(RE),C' '          PHASENAME IS TERMINATED BY A BLANK?          
         JNE   BWW9B5              NO: CONTINUE SCANNING THE CARD               
         CLI   1(RE),C' '          YES: ANOTHER BLANK FOLLOWS?                  
         JE    BWW9D               YES: TREAT REST OF STMT AS COMMENT           
*                                                                               
         LHI   R0,8                NO: IF TOKEN IS EXACTLY 8 CHARS...           
         LA    RF,1(RE)            ...ASSUME IT'S AN ALTERNATE BASENAME         
         CLI   0(RF),C' '          IS TOKEN < 8 CHARACTERS LONG?                
         JE    BWW9D               YES: ASSUME IT'S A COMMENT                   
         LA    RF,1(RF)                                                         
         BRCT  R0,*-12             EXAMINE 8 CHARACTERS, AT MOST                
         CLI   0(RF),C' '          IF THE 9TH CHARACTER ISN'T BLANK...          
         JNE   BWW9D               ...IT MUST BE A COMMENT                      
         LARL  RF,NOALTPHS         ALT. BASENAMES NO LONGER SUPPORTED           
         MVC   MNOTE,0(RF)                                                      
         J     BWW9D                                                            
*                                                                               
BWW9B5   DS    0H                                                               
         LA    RE,1(RE)                                                         
         BRCT  R0,BWW9B                                                         
*                                                                               
BWW9C    MVC   LNKCARD,SPACES                                                   
         LARL  RF,INVALPHS         INVALID (OR MISSING) *PHASE CARD             
         MVC   MNOTE,0(RF)                                                      
*                                                                               
BWW9D    LR    RF,RE                                                            
         SR    RF,R7               RF=L'PHASE NAME                              
         CLI   AMPLVL,C' '                                                      
         JE    BWW9E                                                            
         CIJNL RF,8,BWW9C          WILL MAKE PHASE NAME INVALID                 
         MVC   WORK(60),0(RE)                                                   
         MVC   0(1,RE),AMPLVL      APPEND PHASE LEVEL CHARACTER                 
         MVC   1(60,RE),WORK                                                    
         LA    RF,1(RF)                                                         
*                                                                               
BWW9E    DS    0H                                                               
         BCTR  RE,0                RE = A(LAST CHARACTER IN PHASE NAME)         
         CLI   0(RE),C'@'          PHASE NAME CAN'T END IN RESERVED...          
         JE    BWW9C               ...TEMPORARY CHARACTER (FOR PANAPT)          
         CLI   0(RE),C'S'          PHASE NAME CAN'T END IN RESERVED...          
         JE    BWW9C               ...BACKUP CHARACTER (FOR PANAPT)             
         CLI   0(RE),C'X'          PHASE NAME CAN'T END IN RESERVED...          
         JE    BWW9C               ...BACKUP CHARACTER (FOR PANAPT)             
*                                                                               
         STH   RF,AMPLEN                                                        
         CLI   AMPLIB,C'C'         SET PHASE CARD                               
         JNE   BWW9X                                                            
         LARL  R7,WRKAREA                                                       
         LA    R7,80(R7)                                                        
         MVC   0(80,R7),LNKCARD                                                 
*                                                                               
BWW9X    CLC   BOOKNAME(2),=C'LK'  DONT OUTPUT ANY LK.. BOOK CARDS              
         JE    *+14                                                             
         CLC   BOOKNAME(2),=C'LM'                                               
         JNE   *+8                                                              
         MVI   0(R3),C'F'                                                       
         J     BWWX                                                             
*                                                                               
BWWA     CLI   0(R2),C'*'          LOOK FOR EMBEDDED LNKEDT CARDS               
         JNE   BWWX                                                             
         CLI   INCSW,C'Y'          IGNORE CARDS DELETED BY *&& ACTION           
         JNE   BWWX                                                             
         CLC   FSTSECTN,SPACES     HAVE WE SEEN A CSECT/RSECT/START ?           
         JNE   BWWX                YES: DON'T INTERPRET AS BINDER CMND          
         LA    R7,1(R2)                                                         
         CLC   0(6,R7),=CL8'PHASE'                                              
         JE    BWWC                                                             
         CLC   0(5,R7),=CL8'NAME'                                               
         JE    BWWC                                                             
         CLC   0(8,R7),=CL8'INCLUDR'                                            
         JE    BWWC                                                             
         CLC   0(8,R7),=CL8'INCLUDP'                                            
         JE    BWWC                                                             
         CLC   0(8,R7),=CL8'INCLUDL'                                            
         JE    BWWC                                                             
*                                                                               
         LARL  RF,LNKTBL                                                        
         USING LNKTBLD,RF                                                       
BWWB     DS    0H                                                               
         LLC   RE,LNKACTLN                                                      
         LTR   RE,RE                                                            
         JZ    BWWD                                                             
         BCTR  RE,0                                                             
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R7),LNKACTNM                                                 
         JE    BWWC                                                             
         LA    RF,LNKTBLLQ(RF)                                                  
         J     BWWB                                                             
         DROP  RF                                                               
*                                                                               
BWWC     L     R6,LNKADDR          SAVE EMBEDDED LINK STATEMENT                 
         MVI   0(R6),C'*'          SET EMBEDDED CODE                            
         MVC   1(71,R6),0(R7)                                                   
         LA    R6,80(R6)                                                        
         ST    R6,LNKADDR                                                       
*                                                                               
BWWD     CLC   BOOKNAME(2),=C'LK'  DONT WRITE ANY LK.. RECS TO WORK             
         JE    *+14                                                             
         CLC   BOOKNAME(2),=C'LM'                                               
         JNE   *+8                                                              
         MVI   0(R3),C'F'                                                       
         J     BWWX                                                             
*                                                                               
BWWE     TM    WFLAG,X'08'         CATAL SOURCE WRITE                           
         JZ    BWWX                                                             
         TM    WFLAG,X'40'                                                      
         JO    BWWX                EXIT IF HAS BEEN COMPLETED                   
         TM    WFLAG,X'80'                                                      
         JO    BWWF                                                             
         OI    WFLAG,X'80'         SET INITIALISED FLAG                         
         BRAS  RA,CLEARWRK                                                      
         XC    WCOUNT,WCOUNT                                                    
         XC    SAVLEVEL,SAVLEVEL                                                
*                                                                               
BWWF     CLI   0(R2),C'*'          LOOK FOR EMBEDDED CATAL CARD IN BOOK         
         JNE   BWWH                                                             
         CLI   INCSW,C'Y'          IGNORE CARDS DELETED BY *&& ACTION           
         JNE   BWWI                                                             
         OC    SAVLEVEL,SAVLEVEL                                                
         JNZ   BWWG                                                             
*                                  LOOK FOR PAN "SPECIAL COMMENT" CARD          
         CLC   BOOKLIT1,BOOKLIT1-SPCLCMNT(R2)                                   
         JNE   BWWG                                                             
         CLC   BOOKLIT2,BOOKLIT2-SPCLCMNT(R2)                                   
         JNE   BWWG                                                             
         CLC   BOOKLIT3,BOOKLIT3-SPCLCMNT(R2)                                   
         JNE   BWWG                                                             
         MVC   SAVLEVEL,SPCLCML#-SPCLCMNT(R2)   SAVE FIRST LEVEL NUMBER         
         MVC   SAVDATE,SPCLCMDT-SPCLCMNT(R2)    LAST UPDATE DATE                
         J     BWWI                                                             
*                                                                               
BWWG     CLC   1(5,R2),=CL8'CATALR'                                             
         JNE   BWWI                                                             
         LA    R7,6(R2)                                                         
         CLI   0(R7),C' '                                                       
         JE    *+8                                                              
         LA    R7,1(R7)                                                         
         CLC   1(5,R7),=C'MACRO'                                                
         JNE   *+12                                                             
         OI    WFLAG,X'20'         SET CATAL MACRO FOUND                        
         J     BWWI                                                             
         LA    R7,LNKCARD          POINT TO INPUT CATAL CARD                    
         CLC   7(65,R7),SPACES     DOES INPUT CATAL HAVE A NAME                 
         JNE   *+10                YES USE IT                                   
         MVC   1(71,R7),1(R2)      NO USE *CATAL CARD                           
         LA    R7,1000                                                          
         STH   R7,WCOUNT           FORCE END OF SEARCH                          
         J     BWWI                                                             
*                                                                               
BWWH     TM    WFLAG,X'20'         LOOK FOR PUNCH STATEMENT                     
         JZ    BWWI                                                             
         CLC   0(9,R2),SPACES                                                   
         JNE   BWWI                                                             
         CLC   9(6,R2),=CL8'PUNCH'                                              
         JNE   BWWI                                                             
         MVI   0(R2),C'*'          CONVERT PUNCH TO COMMENT                     
         LA    R7,LNKCARD                                                       
         CLC   7(65,R7),SPACES     DOES INPUT CATAL HAVE A NAME                 
         JNE   BWWH1               YES USE IT                                   
         MVC   LNKCARD,SPACES                                                   
         MVC   LNKCARD+1(5),=CL8'PUNCH'                                         
         LA    RE,16(R2)                                                        
         LA    R0,54                                                            
         CLI   0(RE),C''''                                                      
         JE    *+12                                                             
         LA    RE,1(RE)                                                         
         BRCT  R0,*-12                                                          
         LA    RF,16(R2)                                                        
         SR    RE,RF                                                            
         JZ    BWWH1                                                            
         BCTR  RE,0                                                             
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         MVC   LNKCARD+7(0),16(R2) MOVE PUNCH CARD TEXT TO LNKCARD              
BWWH1    LA    R7,1000                                                          
         STH   R7,WCOUNT           FORCE END OF SEARCH                          
*                                                                               
BWWI     LH    R7,WCOUNT           BUMP WRITE TO WORK COUNT                     
         LA    R7,1(R7)                                                         
         STH   R7,WCOUNT                                                        
         CIJL  R7,100,BWWX                                                      
*                                                                               
         OI    WFLAG,X'40'         SET END OF *CATAL SEARCH                     
*                                                                               
BWWX     DS    0H                                                               
         CLC   MNOTE,SPACES        ANY DELIBERATE MNOTE SET?                    
         JNE   BWW0Z               YES: REQUEST USER RETURN                     
*                                                                               
* AS PER THE HLASM MANUAL, *PROCESS STATEMENTS ARE ONLY VALID IF THEY           
* APPEAR AT THE VERY BEGINNING OF THE SOURCE MODULE. UNFORTUNATELY, CA          
* THWARTS US, BECAUSE PANVALET ALWAYS GENERATES ITS "SPECIAL COMMENT"           
* CARD AS THE *FIRST* STATEMENT IN ANY MEMBER OF LANGUAGE TYPE "BAL".           
*                                                                               
* IT'S WORTH NOTING THAT PANVALET *DOES* HANDLE THE ICTL STATEMENT              
* CORRECTLY, IN THE SENSE THAT IF THERE IS AN ICTL STATEMENT PRESENT            
* AT THE START OF AN ASSEMBLER SOURCE MODULE, PANVALET IS CLEVER ENOUGH         
* TO GENERATE THE SPECIAL COMMENT CARD *AFTER* THE ICTL STATEMENT. BUT          
* CA HAS BEEN DEAF TO OUR PLEAS TO TREAT *PROCESS STATEMENTS IN THE             
* SAME MANNER. SO IN ORDER FOR US TO SUPPORT *PROCESS STATEMENTS, WE            
* HAVE TO DEAL WITH THIS ISSUE OURSELVES. THIS MEANS:                           
*                                                                               
* 1. WE HAVE TO HOLD BACK PANVALET'S SPECIAL COMMENT CARD UNTIL AFTER           
*     ALL *PROCESS STATEMENTS HAVE BEEN RELEASED.                               
* 2. TO MAKE THINGS EASIER ALGORITHMICALLY, WE ALWAYS RELEASE AT LEAST          
*     ONE *PROCESS STATEMENT BEFORE WE RELEASE THE "SPECIAL COMMENT"            
*     CARD. AFTER THAT FIRST *PROCESS STATEMENT IS RELEASED, WE RELEASE         
*     ANY ADDITIONAL USER-SUPPLIED *PROCESS STATEMENTS.                         
* 3. ANYTHING THAT LOOKS LIKE A *PROCESS STATEMENT, AND WHICH ISN'T             
*     LOCATED AT THE BEGINNING OF THE SOURCE MODULE, IS ASSUMED TO BE           
*     AN INCORRECTLY-PLACED *PROCESS STATEMENT, AND WILL TRIGGER THE            
*     GENERATION OF A SEVERITY 8 MNOTE. (THIS MEANS THAT OCCASIONALLY,          
*     A TRUE COMMENT STATEMENT WILL BE MISINTERPRETED AS A *PROCESS             
*     STATEMENT. IN THAT CASE, THE COMMENT STATEMENT MUST BE CHANGED.)          
*                                                                               
         CLC   SPCLCMBK,SPACES     SPECIAL COMMENT PROCESSED ALREADY?           
         JNE   BWWX10              YES: IGNORE ANY OTHERS ENCOUNTERED           
*                                                                               
*                                  LOOK FOR PAN "SPECIAL COMMENT" CARD          
         CLC   BOOKLIT1,BOOKLIT1-SPCLCMNT(R2)                                   
         JNE   BWWX10                                                           
         CLC   BOOKLIT2,BOOKLIT2-SPCLCMNT(R2)                                   
         JNE   BWWX10                                                           
         CLC   BOOKLIT3,BOOKLIT3-SPCLCMNT(R2)                                   
         JNE   BWWX10                                                           
*                                                                               
         MVC   SPCLCMNT,0(R2)      SAVE THE SPECIAL COMMENT CARD                
         MVC   0(80,R2),SPACES     AND RELEASE THE FIRST *PROCESS STMT          
         LARL  RE,PROCWARN                                                      
         MVC   0(L'PROCWARN,R2),0(RE)                                           
         J     RETURN                                                           
*                                                                               
BWWX10   DS    0H                                                               
         CLI   PROCESSW,C'Y'       *PROCESS WINDOW IS OPEN?                     
         JNE   *+18                NO                                           
         CLC   =C'*PROCESS ',0(R2) TRUE *PROCESS STATEMENT?                     
         JE    RETURN              YES: RELEASE IT                              
         J     BWWX20                                                           
*                                                                               
         CLI   FLOWRFLG,C'Y'       WAS THERE A PANAPT "SRCE" FLOWERBOX?         
         JE    RETURN              YES: RELEASE THIS STATEMENT                  
         CLC   =C'*PROCESS ',0(R2) LOOKS LIKE A *PROCESS STATEMENT?             
         JNE   RETURN              NO: RELEASE IT                               
         LARL  RF,PROCESS_STATEMENT_BAD_LOCATION                                
         MVC   MNOTE,0(RF)         PROBABLY A MISLOCATED *PROCESS STMT          
         J     BWW0Z                                                            
*                                                                               
BWWX20   DS    0H                                                               
         MVI   PROCESSW,C'N'       *PROCESS STATEMENT WINDOW IS OVER            
         BRAS  RA,CLEARWRK                                                      
*                                                                               
         IF (TM,PRCFLAG,PRCFLAGS_DFSORT,Z)  ONLY IF NOT DFSORT:                 
           LAY   RE,PEQUSMAC         GENERATE PROGRAM TYPE EQUATES...           
           MVC   0(L'PEQUSMAC,R5),0(RE)   ...VIA PEQUS MACRO CALL               
           LA    R5,80(R5)                                                      
           MVC   0(80,R5),SPCLCMNT   RELEASE BUFFERED "SPECIAL COMMENT"         
           LA    R5,80(R5)                                                      
         ENDIF ,                                                                
*                                                                               
         MVC   0(80,R5),0(R2)      FOLLOWED BY THE CURRENT STATEMENT            
         LA    R5,80(R5)                                                        
         C     R5,=A(WRKAREAX)                                                  
         JNL   *+2                 INCREASE MAX_WRKAREA_ENTRIES                 
         MVI   0(R5),0             MARK END OF BUFFER...                        
         MVI   FLUSHWRK,C'Y'       ...AND FLUSH CARDS BACK TO PANVALET          
         J     URR                                                              
         EJECT                                                                  
*              ROUTINE HANDLES INSERTING PREFIX CODES                           
*                                                                               
*                                  R2=A(CARD)                                   
DOPREFIX DS    0H                                                               
         CLI   0(R2),C'*'          IGNORE COMMENT CARDS                         
         JE    DPRFX                                                            
         CLI   71(R2),C' '         IGNORE IF CONTINUATION CARD                  
         JNE   DPRFX                                                            
*                                                                               
         LR    RF,R2               A(START OF STATEMENT)                        
         CLI   0(RF),C' '          LOOK FOR THE FIRST BLANK...                  
         JE    *+12                ...TO BYPASS THE NAME FIELD                  
         LA    RF,1(RF)                                                         
         J     *-12                                                             
         LA    RF,1(RF)            BUMP PAST THE BLANK DELIMITER                
         CLI   0(RF),C' '          THE FIRST NON-BLANK...                       
         JNE   *+12                ...IS THE OPERATION FIELD                    
         LA    RF,1(RF)                                                         
         J     *-12                                                             
*                                                                               
         CLC   =C'DS ',0(RF)       IF THIS IS DEFINITION                        
         JNE   DPRF10                                                           
         LA    RF,1(RF)            FIND FIRST BLANK AFTER THE MNEMONIC          
         CLI   0(RF),C' '                                                       
         JNE   *-8                                                              
         LA    RF,1(RF)            FIND FIRST OPERAND                           
         CLI   0(RF),C' '                                                       
         JE    *-8                                                              
         CLI   0(RF),C'('          TEST FOR (LABEL)...                          
         JNE   DPRF40                                                           
         CLI   1(RF),C'0'          IGNORE CONSTANTS                             
         JNL   DPRF40                                                           
         LA    R6,1(RF)            SET INSERTION POINT                          
         LA    R7,71(R2)                                                        
         SR    R7,R6               R7 = NO. OF CHARS TO SHIFT RIGHT             
         J     DPRF35              GO INSERT PREFIX AT LABEL                    
*                                                                               
DPRF10   CLC   =C'EQU ',0(RF)      IF THIS IS EQU STATEMENT                     
         JNE   DPRF20                                                           
         LA    RF,1(RF)            FIND FIRST BLANK AFTER THE MNEMONIC          
         CLI   0(RF),C' '                                                       
         JNE   *-8                                                              
         LA    RF,1(RF)            FIND FIRST OPERAND                           
         CLI   0(RF),C' '                                                       
         JE    *-8                                                              
         CLI   0(RF),C'('          IS THIS A PARENTHESIZED EXPRESSION?          
         JNE   *+8                                                              
         LA    RF,1(,RF)           YES: BUMP PAST THE OPEN PARENTHESIS          
         CLI   1(RF),C''''         IGNORE CONSTANTS                             
         JE    DPRF40                                                           
         CLI   0(RF),C'0'          IGNORE CONSTANTS                             
         JNL   DPRF40                                                           
         CLC   =C'* ',0(RF)        IGNORE ASTERISK ONLY                         
         JE    DPRF40                                                           
         CLC   =C'*-',0(RF)        TEST FOR *-LABEL                             
         JNE   DPRF30                                                           
         LA    R6,2(RF)            ADJUST INSERTION POINT                       
         LA    R7,(71-1)(R2)                                                    
         SR    R7,R6               R7 = NO. OF CHARS TO SHIFT RIGHT             
         J     DPRF35              GO INSERT PREFIX AT LABEL                    
*                                                                               
DPRF20   CLC   =C'ORG    ',0(RF)   ORG STATEMENT WITH NO OPERAND?               
         JE    DPRF40                                                           
         CLC   =C'ORG ',0(RF)      NO: ORG STATEMENT WITH AN OPERAND?           
         JNE   DPRF40                                                           
         LA    RF,1(RF)            NO: FIND 1ST BLANK AFTER "ORG"               
         CLI   0(RF),C' '                                                       
         JNE   *-8                                                              
         LA    RF,1(RF)            FIND FIRST OPERAND                           
         CLI   0(RF),C' '                                                       
         JE    *-8                                                              
         CLI   0(RF),C'*'          OPERAND STARTS WITH ASTERISK?                
         JE    DPRF40              YES                                          
*                                                                               
         LA    R6,1(RF)            SCAN FOR A SECOND LABEL                      
         LA    R7,(71-3)(R2)                                                    
         SR    R7,R6               R7 = NO. OF CHARS TO SHIFT RIGHT             
*                                                                               
DPRF25   CLI   0(R6),C' '          TEST FOR END OF INSTRUCTION                  
         JE    DPRF30                                                           
         CLC   =C'+L''',0(R6)      TEST FOR LABEL+L'LABEL2                      
         JE    *+14                                                             
         LA    R6,1(R6)            KEEP ON TRYING                               
         BCTR  R7,0                                                             
         J     DPRF25                                                           
*                                                                               
         LA    R6,3(R6)                                                         
         BRAS  RE,INSPREF          INSERT PREFIX AT LABEL2                      
*                                                                               
DPRF30   LR    R6,RF               SET TO INSERT PREFIX AT LABEL                
         LA    R7,71(R2)                                                        
         SR    R7,R6               R7 = NO. OF CHARS TO SHIFT RIGHT             
*                                                                               
DPRF35   BRAS  RE,INSPREF          INSERT PREFIX AT LABEL                       
*                                                                               
DPRF40   CLC   0(8,R2),SPACES      IF TAG IS PRESENT                            
         JE    *+14                                                             
         LR    R6,R2                                                            
         LA    R7,71                                                            
         BRAS  RE,INSPREF          INSERT PREFIX AT COLUMN 1                    
*                                                                               
         MVI   71(R2),C' '         ENSURE NOT CREATING CONTINUATION             
*                                                                               
DPRFX    BR    RA                                                               
         EJECT                                                                  
*              ROUTINE INSERTS PREFIX CODE                                      
*                                                                               
*                                  R6=A(INSERTION POINT)                        
*                                  R7=L'DATA TO SHIFT                           
INSPREF  DS    0H                                                               
         MVC   WORK,0(R6)          SAVE CURRENT DATA                            
*                                                                               
         MVC   0(L'PREFIX,R6),PREFIX  MOVE IN PREFIX                            
*                                                                               
         LA    R1,1                                                             
         CLI   PREFIX+1,C' '       DETERMINE L'PREFIX                           
         JE    IPRF20                                                           
         LA    R1,1(R1)                                                         
         CLI   PREFIX+2,C' '                                                    
         JE    IPRF20                                                           
         LA    R1,1(R1)                                                         
*                                                                               
IPRF20   AR    R6,R1               R6=A(START OF DATA ON ADJ CARD)              
         SR    R7,R1               R7=L'DATA TO ADJUST                          
         EXRL  R7,*+8                                                           
         BR    RE                                                               
         MVC   0(0,R6),WORK                                                     
         EJECT                                                                  
*                                                                               
* PAN#1 USER REQUESTED RETURN                                                   
*  WE GET HERE EITHER BECAUSE WE'RE INSERTING A STATEMENT AFTER THE             
*  CURRENT ONE (LIKE AN MNOTE), OR TO INSERT STATEMENTS BEFORE THE              
*  CURRENT ONE (LIKE THE LTORG/CSECT/LEVEL BEFORE THE 'END' STATEMENT).         
*                                                                               
URR      DS    0H                                                               
         CLI   FLUSHWRK,C'Y'       FLUSH WRK BUFFER BACK TO PANVALET?           
         JE    URR10               YES                                          
*                                                                               
         CLC   MNOTE,SPACES        NO: DO WE WANT TO ADD AN MNOTE?              
         JE    URRX                NO                                           
         MVC   0(80,R2),MNOTE      YES: MOVE IT TO PAN'S IOAREA                 
         MVC   MNOTE,SPACES        CLEAR IT (SO WE REMEMBER WE'RE DONE)         
         MVI   0(R3),C'E'          SET WRITE TO WORK IOCODE                     
         MVI   2(R3),C'Y'          REQUEST USER RETURN                          
         J     URRX                                                             
*                                                                               
URR10    DS    0H                                                               
         L     R5,WRKADDR                                                       
         MVC   0(80,R2),0(R5)      SET WORK FILE DATA                           
         MVI   0(R3),C'E'          SET WRITE TO WORK IOCODE                     
         LA    R5,80(R5)                                                        
         ST    R5,WRKADDR                                                       
         CLI   0(R5),0             LAST WORK FILE DATA                          
         JE    *+12                YES                                          
         MVI   2(R3),C'Y'          REQUEST USER RETURN                          
         J     URRX                                                             
         MVI   FLUSHWRK,C'N'       RESET WRK BUFFER FLUSH FLAG                  
*                                                                               
URRX     DS    0H                                                               
         J     RETURN                                                           
         EJECT                                                                  
*                                                                               
* PAN#1 BEFORE A READ OF NEXT INPUT CARD                                        
*                                                                               
BRC      CLI   EOFFLAG,0           TEST IF PAN#1 AT END OF FILE                 
         JNE   *+12                                                             
         CLI   WHATIDID,0                                                       
         JE    RETURN              PAN#1 PROVIDED LAST INPUT CARD               
         L     R5,GENADDR                                                       
         CLI   0(R5),X'FF'         END OF GEN?                                  
         JE    BRC1                                                             
         CLI   0(R5),1             DELETED?                                     
         JNL   BRC2                                                             
         CLI   EOFFLAG,0                                                        
         JNE   EOF3                                                             
         MVI   WHATIDID,0          END OF NORMAL GENERATION                     
         J     RETURN                                                           
BRC1     MVI   0(R3),C'G'          TELL PAN#1 EOF                               
         J     RETURN                                                           
BRC2     MVC   0(80,R2),0(R5)      GIVE PAN#1 NEXT INPUT CARD                   
         MVI   0(R3),C'A'                                                       
         LA    R5,80(R5)           BUMP TO NEXT GENERATED CARD                  
         ST    R5,GENADDR                                                       
         CLI   0(R5),1             WAS PREVIOUS GENERATED CARD GENUINE          
         JNE   *+12                                                             
         MVI   WHATIDID,0          NO SET PAN#1 PROVIDED LAST CARD              
         J     ARC0                                                             
*                                                                               
         MVI   WHATIDID,2          YES SET I PROVIDED LAST CARD                 
         CLC   72(8,R2),SPACES                                                  
         JNE   RETURN                                                           
         MVC   72(8,R2),AMACTN     SET ACTION IN COLS 73-80                     
         J     RETURN                                                           
         EJECT                                                                  
*                                                                               
* PAN#1 AFTER A READ OF INPUT CARD                                              
*                                                                               
ARC0     DS    0H                                                               
         CLI   WHATIDID,2          DID I GIVE PAN THIS CARD                     
         JE    RETURN              YES NO NEED TO PROCESS IT                    
*                                                                               
         MVC   72(8,R2),SPACES     THROW AWAY COLS 73-80 OF INPUT CARD          
         CLC   0(2,R2),=C'&&&&'                                                 
         JNE   *+18                                                             
         MVC   AC,0(R2)            SAVE &&CARD IN AC                            
         OI    ACFLAG,X'02'        SET INPUT FLAG                               
         J     ARCAM                                                            
*                                                                               
         CLC   LASTCARD(2),=C'&&&&'                                             
         JE    EOF0                LAST CARD WAS LAST && IN SEQUENCE            
*                                                                               
         CLC   0(2,R2),=C'++'                                                   
         JE    *+14                                                             
         CLC   0(2,R2),=C'--'                                                   
         JNE   ARCX                                                             
*                                                                               
         CLC   2(L'SOAM,R2),SOAM                                                
         JNE   *+18                                                             
         MVC   AC,0(R2)            SPECIAL STAND ALONE CARD                     
         OI    ACFLAG,X'02'                                                     
         J     ARCAM5                                                           
*                                                                               
         LARL  R5,PANTBL           THIS CARD IS PAN#1 ACTION                    
         USING PANTBLD,R5                                                       
ARC2     DS    0H                                                               
         LLC   R6,PANACTLN         SEARCH PAN ACTION TABLE                      
         SHI   R6,1                                                             
         JM    ARCX                                                             
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         CLC   2(0,R2),PANACTNM                                                 
         JE    *+12                IF PRESENT EXTRACT DATA SET NAME             
         LA    R5,PANTBLLQ(R5)                                                  
         J     ARC2                                                             
*                                                                               
         MVC   BOOKNAME,SPACES     INITIALISE FOR NEW &&SEQUENCE                
         MVC   PANACTN,SPACES                                                   
         MVC   AMACTN,SPACES                                                    
         MVC   FSTSECTN,SPACES                                                  
         MVC   LSTSECTN,SPACES                                                  
         MVI   AMERR,C'N'                                                       
         MVI   WFLAG,0                                                          
         MVI   INCSW,C'Y'                                                       
         XC    INCLV,INCLV                                                      
         MVC   INCCHECK,=C'YYYYY'                                               
         MVI   SETSW,C'N'                                                       
         MVC   AA,SPACES                                                        
         MVC   AC,SPACES                                                        
         MVI   ACFLAG,0                                                         
         CLI   PANACTFL,C'Y'       CAN &&S FOLLOW THIS ACTION ?                 
         JNE   ARCX                NO EXIT                                      
         DROP  R5                                                               
*                                                                               
         MVC   PANACTN,2(R2)       YES SAVE ACTION & BOOK NAME                  
         LA    R5,3(R6,R2)                                                      
         LA    R0,11                                                            
         CLI   0(R5),C' '                                                       
         JNE   *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-12                                                          
         J     ARCX                                                             
         LR    R6,R5                                                            
         LA    R0,11                                                            
         CLI   0(R5),C','                                                       
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-12                                                          
         J     ARCX                                                             
         SR    R5,R6                                                            
         JZ    ARCX                                                             
         BCTR  R5,0                                                             
         EXRL  R5,*+10                                                          
         J     *+10                                                             
         MVC   BOOKNAME(0),0(R6)                                                
*                                                                               
         LA    R5,1(R5,R6)         SEARCH FOR COMMENT ON PAN ACTION             
         LA    R0,72(R2)                                                        
         SR    R0,R5                                                            
         CLI   0(R5),C' '                                                       
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-12                                                          
         J     ARCX                                                             
         CLI   0(R5),C' '                                                       
         JNE   *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-12                                                          
         J     ARCX                                                             
         CLC   0(2,R5),=C'&&&&'    CHECK IF COMMENT IS &&DATA                   
         JNE   ARCX                                                             
         LR    R6,R5                                                            
         LR    R5,R0                                                            
         CIJL  R5,6,ARCX                                                        
         BCTR  R5,0                                                             
         EXRL  R5,*+10                                                          
         J     ARCX                                                             
         MVC   AA(0),0(R6)         SAVE ACTION &&DATA IN AA                     
*                                                                               
ARCX     MVC   LASTCARD,0(R2)      SAVE LAST PAN#1 PROVIDED INPUT CARD          
         J     RETURN                                                           
         EJECT                                                                  
*                                                                               
* PAN#1 AFTER A READ OF AN && CARD                                              
*                                                                               
ARCAM    DS    0H                  INPUT IS AN &&CARD                           
         CLI   AMACTN,C' '                                                      
         JE    *+16                                                             
         CLI   AMERR,C'Y'          NOT FIRST OF SEQUENCE                        
         JE    ARCAMERR            DELETE IF PREVIOUS && CARD BAD               
         J     ARCAM8                                                           
         CLI   BOOKNAME,C' '       FIRST OF SEQUENCE                            
         JE    ARCAMERR            ERROR IF NO BOOKNAME                         
         MVC   AMACTN,SPACES                                                    
*                                                                               
         CLC   AC+2(4),=C'SET  '   CHECK FOR &&SET CARD BEFORE ACTION           
         JNE   ARCAM0                                                           
         CLI   AC+6,C' '                                                        
         JE    ARCAMERR                                                         
         MVC   SETDATAC,SPACES                                                  
         MVC   SETDATAC(74),AC+6                                                
         MVI   SETFRSW,SET_VIA_OVERRIDE   TREAT AS OVERRIDE VALUE               
         BRAS  RA,SETDATA                                                       
         CLI   SETDATAC,0                                                       
         JE    ARCAMERR            IGNORE AND DELETE IF ERROR                   
         MVI   WHATIDID,0          DELETE &&SET CARD                            
         MVI   0(R3),C'B'                                                       
         J     ARCX                                                             
*                                                                               
ARCAM0   LA    R5,ACTTBL                                                        
         USING ACTTBLD,R5                                                       
ARCAM1   CLI   0(R5),0             SEARCH &&ACTION TABLE                        
         JE    ARCAMERR            ERROR IF NOT PRESENT                         
         TM    ACTFLAGS,ACTFLAGS_ACTION_AVAILABLE                               
         JZ    ARCAM1A             IGNORE IF NOT AVAIL ACTION                   
         LLC   R6,ACTLEN                                                        
         LA    RE,AC+2(R6)                                                      
         CLI   0(RE),C' '          MUST BE SPACE AFTER ACTION                   
         JNE   ARCAM1A                                                          
         BCTR  R6,0                                                             
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         CLC   AC+2(0),ACTNAME                                                  
         JE    ARCAM1C                                                          
*                                                                               
ARCAM1A  DS    0H                                                               
         LA    R5,ACTTBLLQ(R5)                                                  
         J     ARCAM1                                                           
*                                                                               
ARCAM1C  DS    0H                                                               
         MVC   ACTNUM,ACTTBL#      SAVE ACTION NUMBER                           
         MVC   ACTFLAG,ACTFLAGS    SAVE ACTION FLAGS                            
         DROP  R5                                                               
*                                                                               
         TM    PRCFLAG,PRCFLAGS_LK LK.... ONLY PROC ?                           
         JZ    ARCAM3                                                           
         CLC   BOOKNAME(2),=C'LK'                                               
         JE    *+14                                                             
         CLC   BOOKNAME(2),=C'LM'                                               
         JNE   ARCAMERR                                                         
*                                                                               
ARCAM3   LH    R5,AMPCOUNT         BUMP COUNT OF && CARDS                       
         LA    R5,1(R5)                                                         
         STH   R5,AMPCOUNT                                                      
         CIJH  R5,1,ARCAMERR       MAX OF ONE PER EXEC                          
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         MVC   AMACTN(0),AC+2      SAVE ACTION ON 1ST && CARD                   
*                                                                               
         MVC   SETDATAC,SPACES     SEARCH FOR SET DATA ON 1ST &&CARD            
         LA    R5,AC                                                            
         LA    R6,70                                                            
         CLC   0(7,R5),=C' &&&&SET '                                            
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R6,*-14                                                          
         J     ARCAM4                                                           
*                                                                               
         LA    R5,7(R5)                                                         
         CLI   0(R5),C' '                                                       
         JE    ARCAMERR                                                         
         LA    R6,AC+79                                                         
         SR    R6,R5                                                            
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         MVC   SETDATAC(0),0(R5)   MOVE TO SET DATA CARD                        
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         MVC   0(0,R5),SPACES      CLEAR SET DATA ON 1ST &&CARD                 
         SHI   R5,7                                                             
         MVC   0(7,R5),SPACES                                                   
*                                                                               
ARCAM4   MVI   SETFRSW,SET_VIA_OVERRIDE    TREAT AS OVERRIDE                    
         BRAS  RA,SETDATA          INITIALISE SET DATA AREA                     
         CLI   SETDATAC,0                                                       
         JE    ARCAMERR                                                         
         BRAS  RA,CLEARGEN         INITIALISE GENERATE AREA                     
         MVC   MSG3+13(10),BOOKNAME                                             
         MVC   MSG7+14(10),BOOKNAME                                             
         CLC   AMACTN,=CL8'ASSEMBLY'                                            
         JE    ARCAMAS                                                          
         CLC   AMACTN,=CL8'ASM'                                                 
         JE    ARCAMAS                                                          
         CLC   AMACTN,=CL8'DFSORT'                                              
         JE    ARCAMAS                                                          
         CLC   AMACTN(5),=CL8'CATALR'                                           
         JE    ARCAMCA                                                          
         CLC   AMACTN,=CL8'PHASE'                                               
         JE    ARCAMPH                                                          
         CLC   AMACTN(4),=C'LINK'                                               
         JE    ARCAMPH                                                          
         DC    H'0'                                                             
*                                                                               
ARCAM5   MVC   BOOKNAME,SPACES     INITIALISE FOR STAND ALONE                   
         MVC   PANACTN,SOAM                                                     
         MVC   AMACTN,SPACES                                                    
         MVI   AMERR,C'N'                                                       
         MVI   WFLAG,0                                                          
         MVI   INCSW,C'Y'                                                       
         XC    INCLV,INCLV                                                      
         MVC   INCCHECK,=C'YYYYY'                                               
         MVI   SETSW,C'N'                                                       
         MVC   AA,SPACES                                                        
*                                                                               
         MVI   PONSW,C'N'          SEARCH FOR PON COMMAND ON 1ST &&CARD         
         LA    R5,AC                                                            
         LHI   R0,70                                                            
         CLC   0(7,R5),=C' &&&&PON '                                            
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   PONSW,C'Y'          SUPPRESS PRINT OFF STATEMENTS                
         MVC   0(7,R5),SPACES      REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         MVI   GENSW,C'N'          SEARCH FOR GEN COMMAND ON 1ST &&CARD         
         LA    R5,AC                                                            
         LHI   R0,70                                                            
         CLC   0(7,R5),=C' &&&&GEN '                                            
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   GENSW,C'Y'          SUPPRESS PRINT NOGEN STATEMENTS              
         MVC   0(7,R5),SPACES      REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         LA    R5,AC                                                            
         LHI   R0,70                                                            
         CLC   0(8,R5),=C' &&&&WARN '                                           
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+22                                                             
         LARL  RE,PROCWARN                                                      
         MVC   9(15,RE),=C'USING(WARN(15))' GENERATE ALL USING WARNINGS         
         MVC   0(8,R5),SPACES      REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         MVI   RSECTSW,C'N'        SEARCH FOR RSECT CMD ON 1ST &&CARD           
         LA    R5,AC                                                            
         LHI   R0,70                                                            
         CLC   0(9,R5),=C' &&&&RSECT '                                          
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   RSECTSW,C'Y'        CHANGE CSECT STMTS TO RSECT STMTS            
         MVC   0(9,R5),SPACES      REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         LA    R5,AC               SEARCH FOR NOLVLSTAMP ON 1ST &&CARD          
         LHI   R0,70                                                            
         CLC   =C' &&&&NOLVLSTAMP ',0(R5)                                       
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   LSTAMPSW,C'N'       DON'T GENERATE A LEVEL STAMP                 
         MVC   0(14,R5),SPACES     REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         LA    R5,AC               SEARCH FOR ENDONEOF ON 1ST &&CARD            
         LHI   R0,70                                                            
         CLC   =C' &&&&ENDONEOF ',0(R5)                                         
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   ENDEOFSW,C'Y'       FORCE END STATEMENT GENERATION               
         MVC   0(12,R5),SPACES     REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         LA    R5,AC               SEARCH FOR NOEXPAND ON 1ST &&CARD            
         LHI   R0,70                                                            
         CLC   =C' &&&&NOEXPAND ',0(R5)                                         
         JE    *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-14                                                          
         J     *+14                                                             
         MVI   EXPANDSW,C'N'       SUPPRESS ++INCLUDE EXPANSION                 
         MVC   0(12,R5),SPACES     REMOVE PARAMETER FROM INPUT CARD             
*                                                                               
         LA    R5,AC+2+L'SOAM      SCAN FOR BOOKNAME                            
         CLI   0(R5),C' '                                                       
         JNE   ARCAMERR            MUST BE ADD LEAST ONE BLANK                  
         LA    R5,1(R5)                                                         
         LA    R0,20                                                            
         CLI   0(R5),C' '                                                       
         JNE   *+16                                                             
         LA    R5,1(R5)                                                         
         BRCT  R0,*-12                                                          
         J     ARCAMERR                                                         
*                                                                               
         LR    R6,R5               R5=A(START OF BOOKNAME)                      
         LA    R0,11                                                            
         SR    RE,RE               SET BOOKNAME DELIMITED BY SPACE              
ARCAM5A  CLI   0(R6),C','                                                       
         JE    ARCAM5B                                                          
         CLI   0(R6),C' '                                                       
         JE    ARCAM5B1                                                         
         LA    R6,1(R6)                                                         
         BRCT  R0,ARCAM5A                                                       
         J     ARCAMERR            ERROR BOOKNAME TOO LONG                      
*                                                                               
ARCAM5B  LA    RE,1                SET BOOKNAME DELIMITED BY COMMA              
ARCAM5B1 SR    R6,R5               R6=L'BOOKNAME                                
         JZ    ARCAMERR                                                         
         BCTR  R6,0                                                             
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         MVC   BOOKNAME(0),0(R5)   SAVE BOOKNAME                                
*                                                                               
         XC    PACBACTN,PACBACTN   CLEAR ACTION CODE                            
         MVI   PACBFUNC,C'S'       SET SEARCH FUNCTION CODE                     
         MVC   AUDIT,=C'AUDIT    ' TELL PAM TO RETURN AUDIT DATA                
         ST    RE,FULL             SAVE RE                                      
         L     RF,=V(PAM)          A(PAM)                                       
         CALL  (15),(PACB,WORK,BOOKNAME,NOENTRY,ALLAUDT,NOENTRY),VL             
         LTR   RF,RF               WAS MEMBER FOUND IN PANDD1?                  
         JNZ   *+14                NO: MAYBE IT'S IN PANDD11                    
         MVC   SAVTIME,LUPDTIM     LAST UPDATE TIME                             
         J     ARCAM5B5            YES                                          
*                                                                               
         OC    PANDD11,PANDD11     WAS PANDD11 SUCCESSFULLY OPENED?             
         JZ    ARCAM5B5            NO                                           
         XC    PACBACTN_11,PACBACTN_11   CLEAR ACTION CODE                      
         MVI   PACBFUNC_11,C'S'    SET SEARCH FUNCTION CODE                     
         MVC   AUDIT,=C'AUDIT    ' TELL PAM TO RETURN AUDIT DATA                
         L     RF,=V(PAM)          A(PAM)                                       
         CALL  (15),(PACB_11,WORK,BOOKNAME,NOENTRY,ALLAUDT,NOENTRY),VL          
         LTR   RF,RF               WAS MEMBER FOUND IN PANDD1?                  
         JNZ   ARCAM5B5            NO (WHY NOT ?!?)                             
         MVC   SAVTIME,LUPDTIM     LAST UPDATE TIME                             
*                                                                               
ARCAM5B5 DS    0H                                                               
         L     RE,FULL             RESTORE RE                                   
*                                                                               
         LA    R6,2(R5,R6)         R6=A(FIRST ACTION CHR)                       
         MVC   WORK,0(R6)          SAVE ACTION DATA                             
         MVC   AC(2),=C'&&&&'                                                   
         MVC   AC+2(78),SPACES                                                  
         LA    RF,AC+2                                                          
         LTR   RE,RE               IF COMMA THEN ACTION FOLLOWS IT              
         JNZ   ARCAM5C                                                          
         MVC   AC+2(8),ACTDFLT     IF SPACE THEN SUPPLY DEFAULT ACTION          
         LA    RF,AC+10                                                         
         CLI   0(RF),C' '          SET RF TO POINT TO END OF ACTION             
         JNE   *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         LA    RF,2(RF)                                                         
ARCAM5C  LA    R5,AC+79                                                         
         SR    R5,R6                                                            
         EXRL  R5,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RF),WORK                                                     
         MVC   0(80,R2),AC                                                      
         J     ARCAM               REPROCESS AS &&CARD                          
*                                                                               
ARCAM8   LARL  R5,LNKTBL           SEARCH &&LNKEDT TABLE                        
         USING LNKTBLD,R5                                                       
ARCAM9   DS    0H                                                               
         LLC   R6,LNKACTLN                                                      
         SHI   R6,1                                                             
         JM    ARCAMERR            SET ERROR & DELETE IF NOT THERE              
         EXRL  R6,*+10                                                          
         J     *+10                                                             
         CLC   2(0,R2),LNKACTNM                                                 
         JE    *+12                                                             
         LA    R5,LNKTBLLQ(R5)                                                  
         J     ARCAM9                                                           
         DROP  R5                                                               
*                                                                               
         CLC   AMACTN,=CL8'PHASE'                                               
         JE    *+14                                                             
         CLC   AMACTN(4),=C'LINK'                                               
         JNE   ARCAMERR                                                         
         CLC   AC+2(7),=CL8'INCLUDE'                                            
         JNE   ARCAMA4                                                          
         LA    R6,AC+10                                                         
         LA    R7,9                                                             
ARCAMA0  CLI   0(R6),C' '          FIND END OF OBJECT NAME                      
         JE    ARCAMA1                                                          
         CLI   0(R6),C','                                                       
         JE    ARCAMA1                                                          
         LA    R6,1(R6)                                                         
         BRCT  R7,ARCAMA0                                                       
         J     ARCAMERR            ERROR LONGER THAN 8 CHRS                     
ARCAMA1  LR    R7,R6                                                            
         LA    R0,AC+10                                                         
         SR    R7,R0                                                            
         JZ    ARCAMERR                                                         
         BCTR  R7,0                R7=L'OBJECT NAME1                            
         LARL  R6,LNKAREA                                                       
         J     *+8                                                              
ARCAMA2  LA    R6,80(R6)           SEARCH FOR EMBEDDED OBJECT NAME              
         CLI   0(R6),C' '                                                       
         JNE   *+12                                                             
         MVI   AC+8,C'R'                                                        
         J     ARCAMA4             END OF TABLE                                 
         CLI   0(R6),C'*'                                                       
         JNE   ARCAMA2                                                          
         CLC   1(7,R6),=CL8'INCLUDE'                                            
         JNE   ARCAMA2                                                          
         EXRL  R7,*+10                                                          
         J     *+10                                                             
         CLC   AC+10(0),9(R6)                                                   
         JNE   ARCAMA2                                                          
         MVI   7(R6),C'R'          IF FOUND CHANGE INCLUDE TO INCLUDR           
         J     ARCAMA5                                                          
ARCAMA4  L     R6,LNKADDR          SAVE INPUT LINK STATEMENT                    
         MVC   0(71,R6),AC+1       SET INPUT CODE AND SAVE INPUT CARD           
         LA    R6,80(R6)                                                        
         ST    R6,LNKADDR                                                       
ARCAMA5  MVI   WHATIDID,0          DELETE LNKEDT STATEMENT                      
         MVI   0(R3),C'B'                                                       
         J     ARCX                                                             
*                                                                               
ARCAMERR MVI   AMERR,C'Y'          PROCESS INVALID && CARD                      
         BRAS  RA,CLEARGEN                                                      
         MVI   0(R5),0                                                          
         MVC   WORK,SPACES         CONVERT TO INVALID PAN#1 CARD                
         MVC   WORK(9),=C'++COMMENT'                                            
         MVC   WORK+10(70),AC                                                   
         TM    ACFLAG,X'01'        TEST IF &&CARD GIVEN BY EOF                  
         JO    EOF1                                                             
         MVC   0(80,R2),WORK                                                    
         MVC   LASTCARD,WORK                                                    
         J     RETURN                                                           
*                                                                               
ARCAMGX  MVI   WHATIDID,1          DELETE && CARD AND SET END OF GEN            
         MVI   0(R5),0                                                          
         TM    ACFLAG,X'01'        TEST IF &&CARD GIVEN BY EOF                  
         JO    EOF1                                                             
         MVI   0(R3),C'B'                                                       
         J     ARCX                                                             
         EJECT                                                                  
*                                                                               
* &&ASSEMBLY CARD GENERATION                                                    
*                                                                               
ARCAMAS  MVC   0(L'MSG3,R5),MSG3   --WRITE WORK,BOOKNAME,/*                     
         CLC   PANACTN,SOAM                                                     
         JNE   *+10                                                             
         MVC   0(2,R5),=C'++'                                                   
         LA    RE,13(R5)                                                        
         CLI   0(RE),C' '                                                       
         JE    *+12                                                             
         LA    RE,1(RE)                                                         
         J     *-12                                                             
         MVC   0(3,RE),=C',/*'                                                  
         CLI   EXPANDSW,C'Y'       EXPAND ++INCLUDE STATEMENTS?                 
         JE    *+8                                                              
         MVI   71(R5),C'N'         DON'T EXPAND ++INCLUDE STATEMENTS            
         LA    R5,80(R5)                                                        
         MVI   WFLAG,X'01'         WRITE SOURCE                                 
         J     ARCAMGX                                                          
         EJECT                                                                  
*                                                                               
* &&CATAL CARD GENERATION                                                       
*                                                                               
ARCAMCA  MVC   LNKCARD,SPACES      SAVE AND PROCESS INPUT CATAL CARD            
         MVC   LNKCARD+1(78),AC+2                                               
         MVC   0(L'MSG3,R5),MSG3   --WRITE WORK,BOOKNAME,/*                     
         CLC   PANACTN,SOAM                                                     
         JNE   *+10                                                             
         MVC   0(2,R5),=C'++'                                                   
         LA    RE,13(R5)                                                        
         CLI   0(RE),C' '                                                       
         JE    *+12                                                             
         LA    RE,1(RE)                                                         
         J     *-12                                                             
         MVC   0(3,RE),=C',/*'                                                  
         CLI   EXPANDSW,C'Y'       EXPAND ++INCLUDE STATEMENTS?                 
         JE    *+8                                                              
         MVI   71(R5),C'N'         DON'T EXPAND ++INCLUDE STATEMENTS            
         LA    R5,80(R5)                                                        
         MVI   WFLAG,X'0B'         WRITE SOURCE/SEARCH END/CATAL                
         J     ARCAMGX                                                          
         EJECT                                                                  
*                                                                               
* &&PHASE AND &&LINK CARD GENERATION                                            
*                                                                               
ARCAMPH  MVC   LNKCARD,SPACES      SAVE AND PROCESS INPUT PHASE CARD            
         MVC   LNKCARD+1(78),AC+2                                               
         MVI   AMPLVL,C' '                                                      
*                                                                               
ARCAMPH0 CLI   AMACTN,C'P'         SET DEFAULT CIL FOR &&PHASE                  
         JNE   ARCAMPH1                                                         
         MVI   AMPLIB,C'C'                                                      
         J     ARCAMPH3                                                         
*                                                                               
ARCAMPH1 MVC   AMPLIB,AMACTN+4     SET LIBRARY FROM LINKX CARD                  
         MVC   AMACTN,=CL8'PHASE'                                               
         CLI   AMPLIB,C' '                                                      
         JNE   ARCAMPH2                                                         
*                                                                               
         MVC   WORK(74),LNKCARD+6  CONVERT &&LINK X.. TO &&PHASE X..            
         MVI   LNKCARD+6,C' '                                                   
         MVC   LNKCARD+7(73),WORK                                               
         MVC   LNKCARD+1(5),=CL8'PHASE'                                         
         J     ARCAMPH0                                                         
*                                                                               
ARCAMPH2 MVC   LNKCARD+1(5),=CL8'PHASE'                                         
ARCAMPH3 CLC   LNKCARD+7(5),=C'TEST='                                           
         JNE   ARCAMPH4                                                         
         MVC   AMPLVL,LNKCARD+12   SAVE TEST LEVEL = A/B/C                      
         CLI   AMPLVL,C'A'                                                      
         JL    ARCAMERR                                                         
         CLI   AMPLVL,C'C'                                                      
         JH    ARCAMERR                                                         
         CLC   LNKCARD+13(20),SPACES                                            
         JNE   ARCAMERR                                                         
         MVC   LNKCARD+7(6),SPACES                                              
ARCAMPH4 BRAS  RA,CLEARLNK         CLEAR LINK AREA                              
         CLC   BOOKNAME(2),=C'LK'                                               
         JE    *+14                                                             
         CLC   BOOKNAME(2),=C'LM'                                               
         JNE   ARCAMPH5                                                         
         MVC   0(L'MSG7,R5),MSG7   --WRITE PRINT,BOOKNAME                       
         CLC   PANACTN,SOAM                                                     
         JNE   *+10                                                             
         MVC   0(2,R5),=C'++'                                                   
         LA    R5,80(R5)                                                        
ARCAMPH5 MVC   0(L'MSG3,R5),MSG3   --WRITE WORK,BOOKNAME                        
         CLC   PANACTN,SOAM                                                     
         JNE   *+10                                                             
         MVC   0(2,R5),=C'++'                                                   
         CLC   BOOKNAME(2),=C'LK'                                               
         JE    ARCAMPH6                                                         
         CLC   BOOKNAME(2),=C'LM'                                               
         JE    ARCAMPH6                                                         
         LA    RE,13(R5)                                                        
         CLI   0(RE),C' '                                                       
         JE    *+12                                                             
         LA    RE,1(RE)                                                         
         J     *-12                                                             
         MVC   0(3,RE),=C',/*'                                                  
         CLI   EXPANDSW,C'Y'       EXPAND ++INCLUDE STATEMENTS?                 
         JE    *+8                                                              
         MVI   71(R5),C'N'         DON'T EXPAND ++INCLUDE STATEMENTS            
ARCAMPH6 LA    R5,80(R5)                                                        
         MVI   WFLAG,X'07'         WRITE SOURCE/SEARCH END/PHASE                
         J     ARCAMGX                                                          
         EJECT                                                                  
SETDATA  CLI   SETSW,C'N'          ONLY INITIALIZE THE SET AREA ONCE            
         JNE   SETDATA1                                                         
         MVI   SETSW,C'Y'                                                       
*                                                                               
         LARL  R5,SETAREA                                                       
         USING SETAREAD,R5                                                      
         LHI   R0,SETAREA#         MAXIMUM # OF SET ENTRIES                     
         MVC   0(SETAREAQ,R5),SPACES  INITIALIZE SET AREA TO SPACES             
         LA    R5,SETAREAQ(R5)                                                  
         BRCT  R0,*-10                                                          
*                                                                               
         LARL  R5,SETAREA                                                       
         MVC   SETNAME(3),=C'ALL'  SET ALL STANDARD NAME                        
*                                   "ALL" MUST BE *FIRST* TABLE ENTRY!          
         MVI   SETVALUE,C'Y'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
*                                                                               
         MVC   SETNAME2,=C'UK'     SET UK STANDARD NAMES                        
         MVI   SETVALUE,C'Y'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
         MVC   SETNAME2,=C'US'                                                  
         MVI   SETVALUE,C'N'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
         CLC   COUNTRY,=C'UK'      IF NOT IN THE UK...                          
         JE    SETDATA0                                                         
*                                                                               
         SHI   R5,2*SETAREAQ       ...OVERWRITE THE DEFAULT UK VALUES           
         MVC   SETNAME2,=C'US'     SET US STANDARD NAMES                        
         MVI   SETVALUE,C'Y'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
         MVC   SETNAME2,=C'UK'                                                  
         MVI   SETVALUE,C'N'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
*                                                                               
SETDATA0 DS    0H                                                               
         MVC   SETNAME2,=C'OS'     SET DOS & MVS STANDARD NAMES                 
         MVI   SETVALUE,C'Y'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
         MVC   SETNAME2,=C'DO'                                                  
         MVI   SETVALUE,C'N'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
*                                                                               
         MVC   SETNAME,=C'ONLIN'   SET ONLINE PROGRAM FLAG                      
         MVI   SETVALUE,C'N'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
*                                                                               
         MVC   SETNAME,=C'MAP  '   SET RECORD MAP FLAG                          
         MVI   SETVALUE,C'N'                                                    
         MVI   SETWHERE,SET_VIA_DEFAULT                                         
         LA    R5,SETAREAQ(R5)                                                  
*                                                                               
         ST    R5,SETADDR                                                       
*                                                                               
SETDATA1 LA    RE,SETDATAC         POINT TO SET DATA CARD                       
         CLI   0(RE),C' '                                                       
         JE    SETDATAX            EXIT IF NO SET DATA                          
         MVI   80(RE),C' '                                                      
*                                                                               
SETDATA2 LA    R0,L'SETNAME+1      SCAN FOR SET DATA NAME (MAX=5)               
         LR    RF,RE               SAVE A(START OF NAME)                        
         CLI   0(RE),C'='                                                       
         JE    SETDATA3                                                         
         LA    RE,1(RE)                                                         
         BRCT  R0,*-12                                                          
         J     SETDATAE            ERROR TOO LONG                               
*                                                                               
SETDATA3 LR    R1,RE               SAVE A(END OF NAME)                          
         CLI   1(RE),C'Y'                                                       
         JE    SETDATA4                                                         
         CLI   1(RE),C'N'                                                       
         JE    SETDATA4                                                         
         J     SETDATAE            ERROR VALUE NOT Y OR N                       
SETDATA4 SR    RE,RF                                                            
         JZ    SETDATAE                                                         
         BCTR  RE,0                RE=L'NAME-1                                  
*                                                                               
         LARL  R5,SETAREA          SEARCH FOR NAME IN SET AREA                  
SETDATA5 CLI   0(R5),0                                                          
         JE    SETDATAE            ERROR END OF TABLE                           
         CLI   0(R5),C' '                                                       
         JE    SETDATA6            NAME NOT FOUND                               
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   0(0,RF),SETNAME                                                  
         JE    *+12                                                             
         LA    R5,SETAREAQ(R5)                                                  
         J     SETDATA5                                                         
*                                                                               
         CLI   SETWHERE,SET_VIA_OVERRIDE   PREVIOUSLY SET AS OVERRIDE?          
         JE    SETDATA7            YES: DON'T CHANGE IT                         
         MVC   SETWHERE,SETFRSW    REMEMBER HOW VALUE WAS SET                   
         MVC   SETVALUE,1(R1)      SET VALUE                                    
         J     SETDATA7                                                         
*                                                                               
SETDATA6 EXRL  RE,*+10             INSERT NEW NAME/VALUE                        
         J     *+10                                                             
         MVC   SETNAME(0),0(RF)                                                 
         MVC   SETVALUE,1(R1)                                                   
         MVC   SETWHERE,SETFRSW    REMEMBER HOW VALUE WAS SET                   
         CLI   SETWHERE,SET_VIA_OVERRIDE   SET AS OVERRIDE?                     
         JNE   *+8                                                              
         MVI   SETOVFLG,C'Y'       YES: PREVENT PANAPT PROMOTION!               
         LA    R5,SETAREAQ(R5)                                                  
         ST    R5,SETADDR                                                       
*                                                                               
SETDATA7 LA    RE,2(R1)            RESTORE CARD POINTER                         
         CLI   0(RE),C' '                                                       
         JE    SETDATAX            END OF CARD                                  
         CLI   0(RE),C','                                                       
         JNE   SETDATAE            ERROR INVALID DELIMITER                      
         LA    RE,1(RE)                                                         
         J     SETDATA2            BACK FOR NEXT NAME                           
*                                                                               
SETDATAE MVI   SETDATAC,0          RETURN ERROR VALUE                           
*                                                                               
SETDATAX BR    RA                                                               
         DROP  R5                                                               
         EJECT                                                                  
CLEARGEN LARL  R5,GENAREA          CLEAR GENERATION AREA                        
         ST    R5,GENADDR                                                       
         LR    R7,R5                                                            
CLEARG1  SHI   R7,2                                                             
         LH    R0,0(R7)                                                         
         LA    R7,2(R7)                                                         
         MVC   0(80,R7),SPACES                                                  
         LA    R7,80(R7)                                                        
         BRCT  R0,*-10                                                          
         BR    RA                                                               
*                                                                               
CLEARLNK LARL  R6,LNKAREA          CLEAR LINK STATEMENT AREA                    
         ST    R6,LNKADDR                                                       
         LR    R7,R6                                                            
         J     CLEARG1                                                          
*                                                                               
CLEARWRK LARL  R5,WRKAREA          CLEAR WORK STATEMENT AREA                    
         ST    R5,WRKADDR                                                       
         LR    R7,R5                                                            
         J     CLEARG1                                                          
         EJECT                                                                  
*                                                                               
* GENERATE CATAL CONTROL STATEMENTS                                             
*                                                                               
GENCAT   DS    0H                                                               
         MVI   FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
*                                                                               
         CLC   LNKCARD+1(5),=CL8'PUNCH'                                         
         JNE   GENC2                                                            
         MVC   0(54,R5),LNKCARD+7  TEXT FROM ASSEMBLER PUNCH STATEMENT          
         CLC   0(2,R5),=C'++'                                                   
         JNE   *+8                                                              
         MVI   0(R5),C'$'          PAN CONVENTION FOR COL-1                     
         CLC   0(2,R5),=C'--'                                                   
         JNE   *+8                                                              
         MVI   0(R5),C'$'                                                       
         LA    R5,80(R5)                                                        
         J     GENCX                                                            
*                                                                               
GENC2    LA    R7,LNKCARD+6        SEARCH FOR OBJECT NAME                       
         CLI   0(R7),C' '                                                       
         JE    *+8                                                              
         LA    R7,1(R7)                                                         
         LA    R0,20                                                            
         CLI   0(R7),C' '                                                       
         JNE   *+16                                                             
         LA    R7,1(R7)                                                         
         BRCT  R0,*-12                                                          
         J     GENC2C                                                           
         LR    RE,R7               R7=A(START OF OBJECT NAME)                   
         LA    R0,11                                                            
*                                                                               
GENC2A   CLI   0(RE),C' '                                                       
         JE    GENC2B                                                           
         CLI   0(RE),C','                                                       
         JE    GENC2B                                                           
         LA    RE,1(RE)                                                         
         BRCT  R0,GENC2A                                                        
         J     GENC2C                                                           
*                                                                               
GENC2B   LR    RF,RE                                                            
         SR    RF,R7               RF=L'OBJECT NAME                             
         JZ    GENC2C                                                           
         CIJNH RF,8,GENC3                                                       
         TM    PRCFLAG,PRCFLAGS_CATALP  ALLOW RLXXXXXXXX AND RMXXXXXXXX         
         JZ    GENC2C              FOR CATAL TO PAN LIBRARY ONLY                
         CLC   0(2,R7),=C'RL'                                                   
         JE    GENC3                                                            
         CLC   0(2,R7),=C'RM'                                                   
         JNE   GENC3                                                            
*                                                                               
GENC2C   LA    R7,LNKCARD+6        SET CATAL NAME FOR ERROR                     
         CLI   LNKCARD+6,C' '                                                   
         JE    *+8                                                              
         LA    R7,1(R7)                                                         
         MVC   0(20,R7),SPACES                                                  
         LA    R7,1(R7)                                                         
         LA    RF,5                                                             
         MVC   0(5,R7),=C'DUMMY'                                                
*                                                                               
GENC3    DS    0H                                                               
         TM    PRCFLAG,PRCFLAGS_CATALP   CATALOGUE OBJECT MODULE TO PAN         
         JZ    GENC4                                                            
         LA    RE,0(R7,RF)         RE=A(CHR AFTER OBJECT NAME)                  
         CLI   0(RE),C','                                                       
         JNE   GENC3A                                                           
         CLC   1(3,RE),=C'ADD'     SET ACTION TO ADD IF SPECIFIED               
         JNE   *+12                                                             
         MVI   DUB,C'A'                                                         
         J     GENC3B                                                           
         CLC   1(3,RE),=C'UPD'     SET ACTION TO UPDATE IF SPECIFIED            
         JNE   *+12                                                             
         MVI   DUB,C'U'                                                         
         J     GENC3B                                                           
*                                                                               
GENC3A   MVI   DUB,C'U'            DEFAULT TO ++UPDATE                          
         CLC   SAVLEVEL,=C'001'                                                 
         JNE   *+8                                                              
         MVI   DUB,C'A'            DEFAULT TO ++ADD IF LEVEL IS 001             
*                                                                               
GENC3B   MVC   0(5,R5),=C'$+ADD'                                                
         LA    RE,6(R5)                                                         
         CLI   DUB,C'A'                                                         
         JE    *+14                                                             
         MVC   0(8,R5),=C'$+UPDATE'                                             
         LA    RE,9(R5)                                                         
         CLC   0(2,R7),=C'RL'      TEST IF RLXXXXXXXX                           
         JE    GENC3C                                                           
         CLC   0(2,R7),=C'RM'      TEST IF RMXXXXXXXX                           
         JE    GENC3C                                                           
         MVC   0(2,RE),=C'RM'                                                   
         LA    RE,2(RE)                                                         
*                                                                               
GENC3C   BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RE),0(R7)       MOVE OBJECT NAME                             
         LA    RE,1(RE,RF)                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         CLI   DUB,C'A'                                                         
         JNE   *+14                                                             
         MVC   0(6,RE),=C'OBJECT'  ++ADD RMXXXXXXXX,OBJECT                      
         J     *+10                                                             
         MVC   0(5,RE),=C'0,ALL'   ++UPDATE RMXXXXXXXX,0,ALL                    
         LA    R5,80(R5)                                                        
         J     GENCX                                                            
*                                                                               
GENC4    MVC   1(4,R5),=CL8'NAME'  CATALOGUE OBJECT MODULE TO IBM               
         LA    RE,6(R5)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RE),0(R7)       NAME XXXXXXXX(R)                             
         LA    RE,1(RE,RF)                                                      
         MVC   0(3,RE),=C'(R)'                                                  
         LA    R5,80(R5)                                                        
*                                                                               
GENCX    DS    0H                                                               
         BR    RA                                                               
         EJECT                                                                  
*                                                                               
* GENERATE TRAILING LINK EDIT JCL FROM LNKAREA                                  
*                                                                               
GENLNK   DS    0H                  GENERATE LINK EDIT JCL                       
         L     R6,LNKADDR                                                       
         MVI   0(R6),0             SET END OF LINK AREA                         
         LARL  R6,LNKAREA                                                       
         ST    R6,LNKADDR                                                       
         MVI   FLAG,0                                                           
         MVC   OBJSAVE,SPACES                                                   
         J     GENL1A                                                           
*                                                                               
GENL1    LA    R6,80(R6)           BUMP TO NEXT LINK AREA CARD                  
GENL1A   CLI   0(R6),0                                                          
         JE    GENL6               END OF TABLE                                 
         CLC   1(5,R6),=CL8'PHASE'                                              
         JE    GENL6               NEW PHASE                                    
         CLC   1(4,R6),=CL8'NAME'                                               
         JE    GENL6               NEW PHASE                                    
         CLC   1(6,R6),=CL8'INCLUDR'                                            
         JNE   GENL2                                                            
         TM    FLAG,X'40'          PROCESS INCLUDE CARDS ON 1ST PASS            
         JO    GENL1                                                            
         J     GENL4                                                            
*                                                                               
GENL2    LARL  RE,LNKTBL           SEARCH LINK EDIT COMMANDS TABLE              
         USING LNKTBLD,RE                                                       
GENL2A   SR    RF,RF                                                            
         ICM   RF,1,LNKACTLN                                                    
         JZ    GENL1               IGNORE IF NOT IN TABLE                       
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         CLC   LNKACTNM(0),1(R6)                                                
         JE    *+12                                                             
         LA    RE,LNKTBLLQ(RE)                                                  
         J     GENL2A                                                           
*                                                                               
         LA    RF,2(RF,R6)         IGNORE IF BAD SYNTAX                         
         CLI   0(RF),C' '                                                       
         JNE   GENL1                                                            
         TM    LNKACTFL,LNKACTFL_INSERT_AT_END                                  
         JZ    GENL3A              NO                                           
         TM    FLAG,X'40'          YES PROCESS IF 2ND PASS                      
         JO    GENL3B                                                           
         OI    FLAG,X'80'          SET 2ND PASS REQUIRED                        
         J     GENL1                                                            
*                                                                               
GENL3A   TM    FLAG,X'40'          IGNORE IF 2ND PASS                           
         JO    GENL1                                                            
*                                                                               
GENL3B   TM    FLAG,X'01'                                                       
         JO    *+18                                                             
         OI    FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
         MVC   1(79,R5),1(R6)      LNKEDT CONTROL CARD                          
         LA    R5,80(R5)                                                        
         J     GENL1                                                            
         DROP  RE                                                               
*                                                                               
GENL4    CLI   7(R6),C'E'          INCLUDE OBJECT MODULE FROM PAN               
         JE    *+12                                                             
         CLI   7(R6),C'P'          INCLUDE/INCLUDP                              
         JNE   GENL5                                                            
         CLI   8(R6),C' '                                                       
         JNE   GENL1               IGNORE IF BAD SYNTAX                         
         LA    R7,9(R6)                                                         
         LA    R0,20                                                            
         CLI   0(R7),C' '          R7=A(START OF OBJECT NAME)                   
         JNE   *+16                                                             
         LA    R7,1(R7)                                                         
         BRCT  R0,*-12                                                          
         J     GENL1                                                            
         LR    RE,R7                                                            
         LA    R0,11                                                            
*                                                                               
GENL4A   CLI   0(RE),C' '                                                       
         JE    GENL4B                                                           
         CLI   0(RE),C','                                                       
         JE    GENL4B                                                           
         LA    RE,1(RE)                                                         
         BRCT  R0,GENL4A                                                        
         J     GENL1                                                            
*                                                                               
GENL4B   LR    RF,RE                                                            
         SR    RF,R7               RF=L'OBJECT NAME                             
         JZ    GENL1                                                            
         CIJNH RF,8,GENL4C                                                      
         CLC   0(2,R7),=C'RL'                                                   
         JE    GENL4C                                                           
         CLC   0(2,R7),=C'RM'                                                   
         JNE   GENL1                                                            
*                                                                               
GENL4C   MVC   OBJNAME,SPACES      MOVE OUT OBJECT NAME                         
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   OBJNAME(0),0(R7)                                                 
         CLC   OBJNAME(2),=C'IJ'   IGNORE DOS NAMES                             
         JE    GENL1                                                            
         CLC   OBJNAME,OBJSAVE                                                  
         JE    GENL1               IGNORE DUPLICATES                            
*                                                                               
         MVC   OBJSAVE,OBJNAME                                                  
         NI    FLAG,X'FE'                                                       
         MVC   0(L'MSG6,R5),MSG6   --WRITE PUNCH,RMXXXXXXXX                     
         CLC   OBJNAME(2),=C'RL'                                                
         JE    *+14                                                             
         CLC   OBJNAME(2),=C'RM'                                                
         JNE   *+14                                                             
         MVC   14(10,R5),OBJNAME                                                
         J     *+16                                                             
         MVC   14(2,R5),=C'RM'                                                  
         MVC   16(8,R5),OBJNAME                                                 
         LA    R5,80(R5)                                                        
         J     GENL1                                                            
*                                                                               
GENL5    CLI   7(R6),C'R'          INCLUDE OBJECT MODULE FROM IBM               
         JE    *+12                                                             
         CLI   7(R6),C'L'          INCLUDR/INCLUDL                              
         JNE   GENL1                                                            
         CLI   8(R6),C' '                                                       
         JNE   GENL1               IGNORE IF BAD SYNTAX                         
         LA    R0,20                                                            
         LA    R7,9(R6)                                                         
         CLI   0(R7),C' '          R7=A(START OF OBJECT NAME)                   
         JNE   *+16                                                             
         LA    R7,1(R7)                                                         
         BRCT  R0,*-12                                                          
         J     GENL1                                                            
*                                                                               
         LR    RE,R7                                                            
         LA    R0,9                                                             
GENL5A   CLI   0(RE),C' '                                                       
         JE    GENL5B                                                           
         CLI   0(RE),C'('                                                       
         JE    GENL5B                                                           
         LA    RE,1(RE)                                                         
         BRCT  R0,GENL5A                                                        
         J     GENL1                                                            
*                                                                               
GENL5B   LR    RF,RE                                                            
         SR    RF,R7               RF=L'OBJECT NAME                             
         CLI   0(RE),C' '          TEST IF OS SYNTAX                            
         JE    GENL5C              NO                                           
         TM    FLAG,X'01'                                                       
         JO    *+18                                                             
         OI    FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
         MVC   1(79,R5),1(R6)      INCLUDE DDNAME(OBJNAME)                      
         MVC   0(9,R5),=C' INCLUDE '                                            
         LA    R5,80(R5)                                                        
         J     GENL1                                                            
*                                                                               
GENL5C   CLC   0(2,R7),=C'IJ'      IGNORE DOS NAMES                             
         JE    GENL1                                                            
         TM    FLAG,X'01'                                                       
         JO    *+18                                                             
         OI    FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
         MVC   0(9,R5),=C' INCLUDE '                                            
         MVC   9(L'OBJLIB,R5),OBJLIB                                            
         LA    RE,9+L'OBJLIB(R5)                                                
         MVI   0(RE),C'('          INCLUDE OBJLIB(OBJNAME)                      
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   1(0,RE),0(R7)                                                    
         LA    RE,2(RE,RF)                                                      
         MVI   0(RE),C')'                                                       
         LA    R5,80(R5)                                                        
         J     GENL1                                                            
*                                                                               
GENL6    TM    FLAG,X'40'          TEST IF END OF 2ND PASS                      
         JO    GENL7               YES                                          
         TM    FLAG,X'80'          TEST IF 2ND PASS REQUIRED                    
         JZ    GENL7               NO                                           
         NI    FLAG,X'7F'                                                       
         OI    FLAG,X'40'          SET 2ND PASS                                 
         L     R6,LNKADDR                                                       
         J     GENL1A                                                           
*                                                                               
GENL7    ST    R6,LNKADDR          SAVE A(NEXT LNKEDT SET)                      
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(4),=CL8'NAME'                                             
         CLC   LNKCARD+1(5),=CL8'PHASE'                                         
         JE    GENL8                                                            
         CLC   LNKCARD+1(4),=CL8'NAME'                                          
         JE    GENL8                                                            
         J     GENL8D              ERROR IF NO VALID DATA IN LNKCARD            
*                                                                               
GENL8    CLC   1(4,R6),=CL8'NAME'                                               
         JNE   *+14                                                             
         MVC   WORK+1(79),1(R6)                                                 
         J     GENL9                                                            
*                                                                               
         CLI   LNKCARD+6,C' '                                                   
         JNE   GENL8D                                                           
         LA    RE,LNKCARD+7                                                     
         LA    R0,20                                                            
         CLI   0(RE),C' '                                                       
         JNE   *+16                                                             
         LA    RE,1(RE)                                                         
         BRCT  R0,*-12                                                          
         J     GENL8D                                                           
*                                                                               
         LR    RF,RE                                                            
         LA    R0,9                                                             
GENL8A   CLI   0(RE),C' '                                                       
         JE    GENL8B                                                           
         CLI   0(RE),C','                                                       
         JE    GENL8B                                                           
         LA    RE,1(RE)                                                         
         BRCT  R0,GENL8A                                                        
         J     GENL8D                                                           
*                                                                               
GENL8B   SR    RE,RF                                                            
         JZ    GENL8D                                                           
GENL8C   BCTR  RE,0                RF=A(NAME) AND RE=L'NAME                     
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         MVC   WORK+6(0),0(RF)                                                  
*                                                                               
* EFFECTIVE JUL/2018, THERE ARE NEW RULES FOR LOAD MODULE OVERWRITES.           
* WE ARE LEVERAGING A CHANGE WHICH WAS MADE TO PANACEA IN SEP/2012, IN          
* WHICH WE GENERATED AN "IDENTIFY" BINDER COMMAND TO STAMP THE USERID           
* OF THE DEVELOPER WHO DID THE BIND INTO THE LOAD MODULE'S "USER DATA".         
* THIS MAKES IT EASY TO KNOW WHETHER SOMEONE IS ATTEMPTING TO REPLACE           
* A LOAD MODULE WHICH WAS ADDED BY SOMEONE ELSE.                                
*                                                                               
* A LOAD MODULE MAY BE WRITTEN TO THE TARGET LOAD LIBRARY IF AND ONLY           
* IF ONE OF THE FOLLOWING CONDITIONS IS MET:                                    
*   1. THE LOAD MODULE DOES NOT ALREADY EXIST.                                  
*   2. THE LOAD MODULE EXISTS, BUT THE PREVIOUS LINKER'S USERID IS              
*       UNKNOWN (BECAUSE IT IS TOO OLD TO HAVE "USER DATA" PRESENT).            
*   3. THE LOAD MODULE EXISTS, AND THE "USER DATA" SHOWS THAT THE               
*       PREVIOUS LINKER'S USERID MATCHES THE CURRENT LINKER'S USERID.           
*                                                                               
* I.E.: IF THE LOAD MODULE ALREADY EXISTS, AND WE KNOW THAT IT WAS              
* LINKED BY SOMEONE ELSE, IT WILL NOT BE OVERWRITTEN. THE BINDER "NAME"         
* COMMAND WILL BE WRITTEN *WITHOUT* THE (R) PARAMETER, THEREBY                  
* PREVENTING A REPLACE OF THE LOAD MODULE. THE BIND WILL FAIL WITH              
* RC=12, AND PRODUCE MESSAGE IEW2626S ("DUPLICATE MEMBER IN LIBRARY").          
*                                                                               
         ST    RE,FULL             SAVE RE                                      
*                                                                               
* FIRST MAKE SURE THAT THE JCL HAS A SYSLMOD DD STATEMENT (REFERRING            
* TO THE TARGET LOAD LIBRARY). IF IT DOESN'T, THEN WE WILL NEVER                
* REPLACE AN EXISTING LOAD MODULE.                                              
*                                                                               
         MVC   TXTDDN+6(8),=CL8'SYSLMOD'  TARGET LOAD LIBRARY DDNAME            
         LA    R1,AINFRBLK         A(DYNAMIC INF. RETRIEVAL BLOCK)              
         DYNALLOC ,                                                             
         LTR   RF,RF               IS SYSLMOD ALLOCATED?                        
         JZ    *+14                YES                                          
         XC    PREVUSER,PREVUSER   NO: SET UP FOR ERROR MESSAGE                 
         J     GENL9               LOAD MODULE REPLACE NOT PERMITTED            
*                                                                               
* DETERMINE IF THE MEMBER ALREADY EXISTS                                        
*                                                                               
         IEABRCX ENABLE                                                         
         OPEN  SYSLMOD             OPEN THE TARGET LOAD LIBRARY                 
         LTR   RF,RF                                                            
         JNZ   *+2                 UNSUCCESSFUL OPEN ?!?                        
         IEABRCX DISABLE                                                        
*                                                                               
         MVC   BLDLMBR,WORK+6      SET MEMBER NAME IN BLDL PARM LIST            
         BLDL  SYSLMOD,BLDLLIST    SEE IF MEMBER IS IN TARGET LOADLIB           
         CHI   RF,4                MEMBER FOUND?                                
         JE    GENL8C5             NO: IT'S AN ADD. (R) IS ALWAYS SAFE.         
         JH    *+2                 FATAL BLDL ERROR ?!?                         
*                                                                               
* THE MEMBER DOES EXIST. DYNAMICALLY ALLOCATE IT.                               
*                                                                               
         MVC   AINFRTXT,=A(TXTDSN) RETRIEVE THE DSN (AND L'DSN)                 
         LA    R1,AINFRBLK         A(DYNAMIC INF. RETRIEVAL BLOCK)              
         DYNALLOC ,                                                             
         LTR   RF,RF               DSN WAS RETREIVED SUCCESSFULLY?              
         JNZ   *+2                 NO: SOMETHING IS TERRIBLY WRONG              
*                                                                               
         MVC   TXTDDN+6(8),=CL8'OLDLDMOD' TARGET MEMBER DDNAME                  
         MVC   TXTDSN(2),=AL2(DALDSNAM)   WE ARE PROVIDING THE DSN...           
         MVC   TXTMEMBR+6(8),WORK+6       ...AND MEMBER NAME                    
         LA    R1,AALOCBLK                REQUEST BLOCK POINTER                 
         DYNALLOC ,                                                             
         LTR   RF,RF               LOAD MODULE ALLOCATED SUCCESSFULLY?          
         JNZ   *+2                 NO: SOMETHING IS TERRIBLY WRONG              
*                                                                               
* READ THE MEMBER, LOOKING FOR AN IDR "USER DATA" RECORD.                       
*                                                                               
         IEABRCX ENABLE                                                         
         OPEN  OLDLDMOD            OPEN THE PRE-EXISTING LOAD MODULE            
         LTR   RF,RF                                                            
         JNZ   *+2                 UNSUCCESSFUL OPEN ?!?                        
         IEABRCX DISABLE                                                        
         DO UNTIL=(CLI,0(R1),EQ,X'80',AND,TM,2(R1),X'08',O)                     
           GET OLDLDMOD                                                         
         ENDDO ,                                                                
         CLC   =C'ASM:',9(R1)      IS THIS USER DATA WE EXPECT?                 
         JNE   *+10                NO: REPLACE NOT PERMITTED                    
         MVC   PREVUSER,13(R1)     13(R1) = A(PREVIOUS LINKER'S USERID)         
*                                                                               
CLOSELM  DS    0H                                                               
         IEABRCX ENABLE                                                         
         CLOSE OLDLDMOD            CLOSE THE PRE-EXISTING LOAD MODULE           
         LTR   RF,RF                                                            
         JNZ   *+2                 UNSUCCESSFUL CLOSE ?!?                       
         IEABRCX DISABLE                                                        
*                                                                               
* OCT/2018: RATHER THAN INSIST ON A MATCH OF THE ENTIRE TSO USERID, WE          
*  ONLY MATCH ON THE FIRST THREE CHARACTERS. THIS IS BECAUSE SOME DEVS          
*  HAVE MULTIPLE USERIDS (E.G.: COLIN BLAKEMORE HAS CBLA AND CBL2).             
*  WE REALLY DON'T CARE WHETHER THE USERID MATCHES, IT'S THE *PERSON*           
*  WE'RE TRYING TO IDENTIFY. PROVIDED WE STICK TO THE CONVENTION OF             
*  KEEPING THE FIRST THREE CHARACTERS OF EACH TSO USERID UNIQUE ACROSS          
*  ALL DEVELOPERS, THIS SEEMS REASONABLE.                                       
*                                                                               
         CLC   PREVUSER,SPACES     IS THE PREVIOUS LINKER KNOWN?                
         JE    *+14                NO: REPLACE *IS* PERMITTED                   
         CLC   PREVUSER(3),RACFUSER   IS THIS LINKER SAME AS BEFORE?            
         JNE   GENL9               NO: REPLACE *NOT* PERMITTED                  
*                                                                               
GENL8C5  DS    0H                                                               
         MVC   PREVUSER,SPACES     FLAG THE REPLACE AS BEING OKAY               
         L     RE,FULL             RESTORE RE                                   
         LA    RE,WORK+7(RE)                                                    
         MVC   0(3,RE),=C'(R)'                                                  
         J     GENL9                                                            
*                                                                               
GENL8D   DS    0H                                                               
* WE DON'T HAVE A VALID LOAD MODULE NAME. GENERATE A "NAME DUMMY" CARD.         
* IF "DUMMY" DOESN'T EXIST, IT WILL BE CREATED. IF IT DOES EXIST, THE           
* BIND WILL FAIL WITH A "DUPLICATE MEMBER" SEVERITY 12 ERROR.                   
         MVC   WORK+6(5),=C'DUMMY' GENERATE BINDER "NAME DUMMY" CARD            
*                                                                               
GENL9    DS    0H                                                               
         TM    PRCFLAG,PRCFLAGS_LK LK..... ONLY PROC ?                          
         JZ    GENLA                                                            
         CLI   LSTAMPSW,C'N'       GENERATE LEVEL STAMP?                        
         JE    GENLA               NO                                           
*                                                                               
         LARL  RE,LNKSTAMP                                                      
L        USING LNKSTAMP,RE                                                      
*                                                                               
         MVC   L.LNKBOOK,BOOKNAME  FILL IN LEVEL STAMP FIELDS                   
*                                                                               
         CLI   SETOVFLG,C'Y'       &&SET OVERRIDE(S) FOUND?                     
         JNE   *+8                                                              
         MVI   L.LNK_PROMOTE_FLAG,SET_FLAG_OVERRIDE  PREVENT PROMOTION          
*                                   (FORCE LEVEL STAMP MISMATCH)                
*                                                                               
         CLI   RSECTSW,C'Y'        CSECT STMTS CHANGED TO RSECT STMTS?          
         JNE   *+8                                                              
         MVI   PROMOTE_FLAG,RSECT_OVERRIDE     PREVENT PANAPT PROMOTION         
*                                   (FORCE LEVEL STAMP MISMATCH)                
*                                                                               
         MVC   L.LNKLVL,SAVLEVEL                                                
         MVC   L.LNKTIME,SAVTIME                                                
         MVC   L.LNKCUSER,RACFUSER                                              
*                                                                               
         MVC   HALF,SAVDATE        IN US, FORMAT IS MM/DD/YY                    
         MVC   L.LNKDATE+3(2),SAVDATE+3                                         
         CLC   COUNTRY,=C'UK'                                                   
         JNE   *+16                                                             
         MVC   HALF,SAVDATE+3      IN UK, FORMAT IS DD/MM/YY                    
         MVC   L.LNKDATE+3(2),SAVDATE                                           
         PACK  DUB,HALF                                                         
         CVB   R1,DUB              R1 = MONTH NUMBER                            
         CHI   R1,12                                                            
         JH    *+2                 MONTH IS ABOVE DECEMBER ?!?                  
         BCTR  R1,0                MONTHS ARRAY IS ONE-BASED                    
         MHI   R1,3                3 CHARACTERS IN MONTH ABBREVIATION           
         LA    R1,MONTHS(R1)                                                    
         MVC   L.LNKDATE(3),0(R1)  MONTH IN MMM FORMAT                          
         MVC   L.LNKDATE+6(2),SAVDATE+6   YEAR NUMBER                           
*                                                                               
         MVC   L.LNKCDATE,SAVCDATE COMPILE DATE                                 
         MVC   L.LNKCTIME,SAVCTIME COMPILE TIME                                 
         DROP  L                                                                
*                                                                               
         LHI   R0,LNKCRDSQ         NUMBER OF CARDS IN OBJECT MODULE             
         TM    FLAG,X'01'                                                       
         JO    *+18                                                             
         OI    FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
GENL9A   MVC   0(80,R5),0(RE)      LNKSTAMP OBJECT DECK                         
         LA    R5,80(R5)                                                        
         LA    RE,80(RE)                                                        
         BRCT  R0,GENL9A                                                        
*                                                                               
GENLA    DS    0H                                                               
         TM    FLAG,X'01'          WORK HAS THE NAME LINK EDIT CARD             
         JO    *+18                                                             
         OI    FLAG,X'01'                                                       
         MVC   0(L'MSG4,R5),MSG4   --INSERT PUNCH                               
         LA    R5,80(R5)                                                        
*                                                                               
* CHECK FOR AN ATTEMPT TO OVERWRITE A PRE-EXISTING LOAD MODULE THAT WAS         
* ADDED BY ANOTHER DEVELOPER. IF SO, GENERATE BINDER COMMENTS (THEY             
* START WITH AN ASTERISK IN COLUMN 1) TO ILLUSTRATE THE PROBLEM.                
*                                                                               
         IF (CLC,PREVUSER,NE,SPACES) IF A REPLACE IS BEING STOPPED:             
           MVI   0(R5),C'*'            PRODUCE BINDER ERROR COMMENTS            
           MVC   1(58,R5),0(R5)        EYE-CATCHER                              
           LA    R5,80(R5)                                                      
           MVC   0(19,R5),=C'* FATAL LINK ERROR!'                               
           LA    R5,80(R5)                                                      
           IF (OC,PREVUSER,PREVUSER,NZ)                                         
             MVC   0(57,R5),=C'* PRE-EXISTING LOAD MODULE XXXXXXXX WAS +        
               LINKED BY XXXXXXX'                                               
             MVC   27(8,R5),WORK+6       LOAD MODULE NAME                       
             MVC   50(7,R5),PREVUSER     PREVIOUS LINKER'S USERID               
             LA    R5,80(R5)                                                    
           ELSE  ,                                                              
             MVC   0(41,R5),=C'* PANACEA REQUIRES A SYSLMOD DD STATEMEN+        
               T'                                                               
             LA    R5,80(R5)                                                    
             MVC   0(38,R5),=C'* REFERENCING THE TARGET LOAD LIBRARY.'          
             LA    R5,80(R5)                                                    
           ENDIF ,                                                              
           MVI   0(R5),C'*'                                                     
           MVC   1(58,R5),0(R5)                                                 
           LA    R5,80(R5)                                                      
         ENDIF ,                                                                
*                                                                               
         MVC   1(8,R5),=C'IDENTIFY'                                             
         MVC   10(8,R5),FSTSECTN                                                
         TM    PRCFLAG,PRCFLAGS_LK LK..... ONLY PROC ?                          
         JZ    *+10                                                             
         MVC   10(8,R5),=C'LNKSTAMP'                                            
         LA    R1,18(R5)                                                        
         CLI   0(R1),C' '                                                       
         JNE   *+10                                                             
         BCTR  R1,0                                                             
         J     *-10                                                             
         MVC   1(2,R1),=C'('''                                                  
         LA    R1,3(R1)                                                         
         MVC   0(4,R1),=C'ASM:'    THIS IS *ASSEMBLY* INFORMATION               
         MVC   4(7,R1),RACFUSER    USER WHO SUBMITTED THE ASSEMBLY              
         MVC   12(10,R1),BOOKNAME  PAN MEMBER NAME                              
         MVC   23(8,R1),SAVCDATE   COMPILE DATE                                 
         MVC   32(5,R1),SAVCTIME   COMPILE TIME                                 
         MVC   37(2,R1),=C''')'                                                 
         LA    R5,80(R5)                                                        
*                                                                               
         MVC   0(80,R5),WORK       NAME XXXXXXXX(R)                             
         LA    R5,80(R5)                                                        
*                                                                               
         CLI   0(R6),0             TEST IF END OF LNKAREA                       
         JE    GENLX                                                            
         MVI   LNKCARD,C' '        SAVE FIRST CARD OF SET IN LNKCARD            
         MVC   LNKCARD+1(79),1(R6)                                              
         LA    R6,80(R6)                                                        
         ST    R6,LNKADDR          SAVE A(FIRST CARD IN SET)                    
         NI    FLAG,X'01'                                                       
         MVC   OBJSAVE,SPACES                                                   
         J     GENL1A              BACK TO PROCESS NEXT LNKAREA SET             
*                                                                               
GENLX    DS    0H                                                               
         BR    RA                                                               
         EJECT                                                                  
*                                                                               
* PAN#1 AT EOF OR END OF &&CARD SEQUENCE                                        
*                                                                               
EOF      OI    EOFFLAG,X'01'       SET PAN#1 END OF FILE                        
EOF0     BRAS  RA,CLEARGEN                                                      
         CLC   LASTCARD(2),=C'&&&&' WAS LAST CARD &&CARD                        
         JE    EOF3                YES                                          
         OC    AMPCOUNT,AMPCOUNT                                                
         JNZ   EOF6                                                             
         MVC   AC,SPACES           SET DEFAULT &&CARD                           
         MVC   AC(2),=C'&&&&'                                                   
         MVC   AC+2(8),ACTDFLT                                                  
         CLI   AA,C' '             CAN BE OVER WRITTEN BY ACTION &&DATA         
         JE    *+10                                                             
         MVC   AC,AA                                                            
         OI    ACFLAG,X'01'        SET &&CARD CREATED BY EOF                    
         J     ARCAM                                                            
*                                                                               
EOF1     MVC   LASTCARD,AC         RETURN FROM ARCAM                            
         CLI   AMERR,C'Y'                                                       
         JE    *+12                                                             
         MVI   0(R3),C'B'                                                       
         J     RETURN                                                           
         MVC   0(80,R5),WORK                                                    
         LA    R5,80(R5)                                                        
         MVI   0(R5),0                                                          
         MVC   LASTCARD,WORK                                                    
         MVI   WHATIDID,1                                                       
         MVI   0(R3),C'B'                                                       
         J     RETURN                                                           
*                                                                               
EOF3     MVI   WFLAG,0                                                          
         BRAS  RA,CLEARGEN                                                      
         CLC   AMACTN,=CL8'ASSEMBLY'                                            
         JE    EOF4                                                             
         CLC   AMACTN,=CL8'ASM'                                                 
         JE    EOF4                                                             
         CLC   AMACTN(5),=CL8'CATALR'                                           
         JNE   *+12                                                             
         BRAS  RA,GENCAT                                                        
         J     EOF4                                                             
         CLC   AMACTN,=CL8'PHASE'                                               
         JNE   *+12                                                             
         BRAS  RA,GENLNK                                                        
         J     EOF4                                                             
         CLC   LASTCARD(9),=C'++COMMENT'                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         OC    AMPCOUNT,AMPCOUNT                                                
         JNZ   EOF6                                                             
*                                                                               
EOF4     TM    EOFFLAG,X'01'       PREVIOUS CARD WAS &&CARD                     
         JO    EOF5                                                             
         MVC   0(80,R5),0(R2)      SAVE THIS CARD                               
         LA    R5,80(R5)                                                        
         MVI   0(R5),1             SET TEMPORARY DELETE                         
         MVI   WHATIDID,1                                                       
         MVI   0(R3),C'B'                                                       
         J     ARCX                                                             
*                                                                               
EOF5     MVI   0(R5),X'FF'         SET END OF GEN                               
         MVI   WHATIDID,1                                                       
         MVI   0(R3),C'B'                                                       
*                                                                               
         J     ARCX                                                             
*                                                                               
EOF6     MVI   0(R3),C'G'          TELL PAN#1 EOF                               
         J     RETURN                                                           
         EJECT                                                                  
COMMON   DS    0D                                                               
*                                                                               
* ACTTBL DEFINES VALID NAMES FOR THE FIRST && CARD ACTION                       
* AL1    ACTION NUMBER                                                          
* XL1    ACTION FLAGS - X'80'=ACTION AVAIL FOR THIS PROC                        
* AL1    ACTION LENGTH                                                          
* CL8    ACTION NAME                                                            
*                                                                               
ACTTBL   DS    0C                                                               
         DC    AL1(01),X'00',AL1(3),CL8'ASM'                                    
         DC    AL1(02),X'00',AL1(8),CL8'ASSEMBLY'                               
         DC    AL1(03),X'00',AL1(5),CL8'CATAL'                                  
         DC    AL1(04),X'00',AL1(6),CL8'CATALP'                                 
         DC    AL1(05),X'00',AL1(6),CL8'CATALR'                                 
         DC    AL1(06),X'00',AL1(5),CL8'PHASE'                                  
         DC    AL1(07),X'00',AL1(4),CL8'LINK'                                   
         DC    AL1(08),X'00',AL1(5),CL8'LINKC'                                  
         DC    AL1(09),X'00',AL1(5),CL8'LINKP'                                  
         DC    AL1(10),X'00',AL1(6),CL8'DFSORT'                                 
         DC    AL1(00)             EOT                                          
         SPACE 3                                                                
ACTTBLD  DSECT ,                                                                
ACTTBL#  DS    AL1                 ACTION NUMBER                                
ACTFLAGS DS    XL1                 ACTION FLAGS                                 
ACTFLAGS_ACTION_AVAILABLE EQU X'80'  ACTION AVAILABLE FOR THIS PROC             
ACTLEN   DS    AL1                 ACTION LENGTH                                
ACTNAME  DS    CL8                 ACTION NAME                                  
ACTTBLLQ EQU   *-ACTTBLD                                                        
         SPACE 2                                                                
PANEXIT  CSECT                                                                  
         EJECT                                                                  
* NOTE: THE STRUCTURE OF THIS LEVEL STAMP MUST STAY IN SYNC WITH THE            
*       STRUCTURE OF LNKSTAMP                                                   
*                                                                               
LEVELDC1 DS    0C                                                               
         DC    C'         DC    C'''                                            
AUDREC1  DS    0C                                                               
         DC    C'BOOK='                                                         
LEVELBK  DC    C'??????????'                                                    
*                                                                               
PROMOTE_FLAG DC C' '               SET THIS TO A NON-BLANK TO FORCE...          
*                                   ...A PANAPT LEVEL STAMP MISMATCH            
SET_FLAG_OVERRIDE EQU C'S'         &&SET OVERRIDE FOUND ON ++DUMMY CARD         
RSECT_OVERRIDE    EQU C'R'         &&RSECT PARAMETER ON ++DUMMY CARD            
*                                                                               
         DC    C'LEVEL='                                                        
LEVELNUM DC    C'NNN'                                                           
         DC    C' DATE='                                                        
LEVELDAT DC    C'MMMDD/YY'                                                      
         DC    C' TIME='                                                        
LEVELTIM DC    C'HH:MM:SS'                                                      
AUDREC1Q EQU   *-AUDREC1                                                        
         DC    C''''                                                            
LEVEL1LQ EQU   *-LEVELDC1                                                       
*                                                                               
LEVELDC2 DS    0C                                                               
         DC    C'         DC    C'''                                            
AUDREC2  DS    0C                                                               
         DC    C' COMPILED ON '                                                 
LEVELCDT DC    C'MMMDD/YY'                                                      
         DC    C' AT '                                                          
LEVELCTM DC    C'HH:MM'                                                         
         DC    C' BY '                                                          
LEVELCUS DC    C'XXXXXXXX'                                                      
AUDREC2Q EQU   *-AUDREC2                                                        
         DC    C''''                                                            
LEVEL2LQ EQU   *-LEVELDC2                                                       
*                                                                               
LEVELDC3 DS    0C                  THIS CARD IS FOR PANACAT ONLY                
         DC    C'         DC    C'''                                            
AUDREC3  DS    0C                                                               
LEVELCAS DC    CL4' '              SET TO " AS "                                
LEVELCBK DS    0CL10               RELO BOOKNAME                                
LEVELCRM DC    CL2' '              SET TO "RM"                                  
LEVELCNM DC    CL8' '              RELO MODULE NAME (WITHOUT "RM")              
AUDREC3Q EQU   *-AUDREC3                                                        
         DC    C''''                                                            
LEVEL3LQ EQU   *-LEVELDC3                                                       
         EJECT                                                                  
MONTHS   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         SPACE 2                                                                
SOAM     DC    C'DUMMY'            STAND ALONE DUMMY ++CARD                     
OBJLIB   DC    C'ASMLIB'           DEFAULT OBJECT LIBRARY                       
PRCNUM   DC    X'00'                                                            
PRCFLAG  DC    X'00'                                                            
PRCNAME  DC    CL8' '                                                           
ACTNUM   DC    X'00'                                                            
ACTFLAG  DC    X'00'                                                            
ACTDFLT  DC    CL8' '                                                           
         SPACE 2                                                                
MSG3     DC    C'--WRITE WORK,XXXXXXXXXX'                                       
MSG4     DC    C'--INSERT PUNCH'                                                
MSG6     DC    C'--WRITE PUNCH,XXXXXXXXXX'                                      
MSG7     DC    C'--WRITE PRINT,XXXXXXXXXX'                                      
         SPACE 2                                                                
APARM    DS    A                                                                
SETADDR  DS    A                                                                
GENADDR  DS    A                                                                
LNKADDR  DS    A                                                                
WRKADDR  DS    A                                                                
         SPACE 2                                                                
*** PAN ACB AREA (FOR PANDD1)                                                   
PACB     DS    0F                  PAM ACCESS CONTROL BLOCK                     
PACBACTN DS    F                   MUST BE NULLS BEFORE EACH CALL               
*                                  RETURN CODE SET BY PAM                       
         DS    XL3                 RESERVED                                     
PACBFUNC DS    CL1                 MUST SET THIS BEFORE EACH CALL               
         DS    F                   RESERVED FOR USE BY PAM                      
PACBLQ   EQU   *-PACB                                                           
         SPACE 2                                                                
*** PAN ACB AREA (FOR PANDD11)                                                  
PACB_11  DS    0F                  PAM ACCESS CONTROL BLOCK                     
PACBACTN_11 DS F                   MUST BE NULLS BEFORE EACH CALL               
*                                  RETURN CODE SET BY PAM                       
         DS    XL3                 RESERVED                                     
PACBFUNC_11 DS CL1                 MUST SET THIS BEFORE EACH CALL               
         DS    F                   RESERVED FOR USE BY PAM                      
         SPACE 2                                                                
PANDD1   DC    CL8'PANDD1'                                                      
PANDD11  DC    CL8'PANDD11'                                                     
NOENTRY  DC    CL8'NO-ENTRY'                                                    
         SPACE 2                                                                
DUB      DS    D                                                                
SAVEAREA DS    20F                 FOR PAM CALLS                                
AINCLSTX DC    A(INCLST)                                                        
INCLCNT  DC    F'0'                NUMBER OF INCLUDES FOUND                     
FULL     DS    F                                                                
HALF     DS    H                                                                
WCOUNT   DC    H'0'                                                             
AMPCOUNT DC    H'0'                                                             
RACFUSER DS    CL8                 PROGRAMMER'S RACF USERID                     
PREVUSER DC    CL7' '              PREVIOUS LINKER'S USERID                     
COUNTRY  DS    CL2                 'US' OR 'UK'                                 
SAVLEVEL DS    CL3                                                              
SAVDATE  DS    CL8                 LAST UPDATE DATE (FROM DIRECTORY)            
SAVTIME  DC    CL8'HH:MM:SS'       LAST UPDATE TIME (FROM AUDIT DATA)           
SAVCDATE DS    CL8                 COMPILE DATE                                 
SAVCTIME DS    CL5                 COMPILE TIME                                 
FSTSECTN DC    CL8' '              1ST CONTROL SECTION (CSECT OR RSECT)         
FSTSECTP DC    CL5' '              'CSECT' OR 'RSECT'                           
LSTSECTN DC    CL8' '              LAST CONTROL SECTN (CSECT OR RSECT)          
LSTSECTP DC    CL5' '              'CSECT' OR 'RSECT'                           
OPERFLD  DS    CL8                 PARSED OPERATION FIELD                       
LABELFLD DS    CL63                PARSED LABEL FIELD                           
OBJNAME  DS    CL10                                                             
OBJSAVE  DS    CL10                                                             
FRSTTIME DC    C'Y'                                                             
ACFLAG   DC    X'00'                                                            
AMERR    DC    C'N'                                                             
FLUSHWRK DC    C'N'                                                             
INCSW    DC    C'Y'                                                             
INCLV    DC    H'0'                                                             
INCTBL   DC    C'Y    ',X'FF'                                                   
INCCHECK DC    C'YYYYY'            'Y'=*&&, 'N'=*&! (PER LEVEL)                 
SETSW    DC    C'N'                                                             
SETFRSW  DS    C                   SHOW FROM WHENCE THE &&SET CAME              
SETOVFLG DC    C'N'                'Y' IF ANY &&SET STMT IS OVERRIDDEN          
PONSW    DC    C'N'                                                             
GENSW    DC    C'N'                                                             
PROCESSW DC    C'Y'                'Y' = *PROCESS STATEMENT WINDOW OPEN         
RSECTSW  DC    C'N'                'Y' IF &&RSECT OPTION IS SET                 
LSTAMPSW DC    C'Y'                'N' IF &&NOLVLSTAMP OPTION IS SET            
ENDEOFSW DC    C'N'                'Y' IF &&ENDONEOF OPTION IS SET              
EXPANDSW DC    C'Y'                'N' IF &&NOEXPAND OPTION IS SET              
CATALFLG DC    C'N'                'Y' IF *CATALP CARD WAS SEEN                 
FLOWRFLG DC    C'N'                'Y' IF PANAPT "SRCE" FLOWERBOX SEEN          
PREFIX   DC    CL3' '                                                           
PREFBOOK DC    CL10' '                                                          
THISBOOK DC    CL10' '                                                          
*                                                                               
SPCLCMNT DS    0CL80               PANVALET SPECIAL COMMENT CARD                
BOOKLIT1 DC    C'*          DATA SET '                                          
SPCLCMBK DC    CL10' '             BOOKNAME                                     
BOOKLIT2 DC    C' AT LEVEL '                                                    
SPCLCML# DS    CL3                 LEVEL NUMBER                                 
BOOKLIT3 DC    C' AS OF '                                                       
SPCLCMDT DS    CL8                 LAST UPDATE DATE                             
         DS    CL(L'SPCLCMNT-(*-SPCLCMNT))                                      
*                                                                               
EOFFLAG  DC    X'00'                                                            
WFLAG    DC    X'00'                                                            
FLAG     DC    X'00'                                                            
WHATIDID DC    X'00'                                                            
PANACTN  DC    C' '                                                             
BOOKNAME DC    CL10' '                                                          
AMACTN   DC    CL8' '                                                           
LASTCARD DC    CL10' '                                                          
SPACES   DC    CL80' '                                                          
WORK     DS    CL80                                                             
SETDATAC DS    CL80                                                             
AMPLEN   DS    H                                                                
AMPLVL   DS    CL1                                                              
AMPLIB   DS    CL1                                                              
LNKCARD  DS    CL80                                                             
AA       DS    CL80                                                             
AC       DS    CL80                                                             
MNOTE    DC    CL80' '             FOR DDS-GENERATED MNOTES                     
         EJECT                                                                  
*                                                                               
* DYNALLOC DATA STRUCTURES                                                      
*                                                                               
* NOTE: THESE STRUCTURES MAY BE SHARED/MODIFIED BY MULTIPLE CALLS TO            
*       DYNALLOC!                                                               
*                                                                               
         DS    0F                                                               
AINFRBLK DC    X'80',AL3(INFRBLK)  A(DYNAMIC INF. RETRIEVAL BLOCK)              
INFRBLK  DC    0XL20               DYNAMIC INFORMATION RETRIEVAL BLOCK          
         DC    AL1(20)             S99RBLN                                      
         DC    AL1(S99VRBIN)       S99VERB                                      
         DC    XL6'00'             S99FLAG1/S99ERROR/S99INFO                    
         DC    A(AINFRTXT)         S99TXTPP                                     
         DC    XL8'00'                                                          
*                                                                               
AINFRTXT DC    A(TXTITYPE)                                                      
         DC    A(TXTDDN)                                                        
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
         DS    0F                                                               
AALOCBLK DC    X'80',AL3(ALLOCBLK) A(DYNAMIC ALLOCATION BLOCK)                  
ALLOCBLK DC    0XL20               DYNAMIC ALLOCATION BLOCK                     
         DC    AL1(20)             S99RBLN                                      
         DC    AL1(S99VRBAL)       S99VERB                                      
         DC    XL6'00'             S99FLAG1/S99ERROR/S99INFO                    
         DC    A(AALOCTXT)         S99TXTPP                                     
         DC    XL8'00'                                                          
*                                                                               
AALOCTXT DC    A(TXTDDN)                                                        
         DC    A(TXTDSN)                                                        
         DC    A(TXTMEMBR)                                                      
         DC    A(TXTDISP)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
TXTDDN   DC    AL2(DALDDNAM),AL2(1),AL2(8),CL8'PANDD11' REQUEST DDNAME          
TXTDSN   DC    AL2(DINRTDSN),AL2(1),AL2(44),CL44' ' RETURN DSN AREA             
TXTMEMBR DC    AL2(DALMEMBR),AL2(1),AL2(8),CL8' '   MEMBER NAME                 
TXTDISP  DC    AL2(DALSTATS),AL2(1),AL2(1),X'08'    DISP=SHR                    
TXTITYPE DC    AL2(DINRTTYP),AL2(1),AL2(1)          TYPE SPECIFICATION          
*                                                                               
*        BLDL PARAMETER LIST                                                    
*                                                                               
BLDLLIST DC    H'1'                NUMBER OF ENTRIES IN THIS BLDL LIST          
         DC    H'12'               LENGTH OF BLDL LIST                          
BLDLMBR  DC    CL8' '              MEMBER NAME                                  
BLDLTTRK DC    XL4'00'             TTR ,K                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDPANAUDIT                                                     
         EJECT                                                                  
* NOTE: DCBNAME DOESN'T EQUAL DDNAME!                                           
INCLS    DCB   DDNAME=RELOS,MACRF=PM,DSORG=PS,RECFM=FB,LRECL=80                 
         SPACE 2                                                                
AUDCARDS DCB   DDNAME=AUDCARDS,MACRF=PM,DSORG=PS,RECFM=FB,LRECL=80              
         SPACE 2                                                                
OLDLDMOD DCB   DDNAME=OLDLDMOD,DSORG=PS,MACRF=GL,EODAD=CLOSELM                  
         SPACE 2                                                                
SYSLMOD  DCB   DDNAME=SYSLMOD,DSORG=PS,MACRF=GL                                 
         EJECT                                                                  
* THESE CARDS WERE CONSTRUCTED BY ASSEMBLING DDLNKSTAMP, BROWSING               
* RMLNKSTAMP, AND COPYING THE RESULTS. WE PLUG IN THE LEVEL STAMP INFO          
* FOR LNK MEMBERS ONLY INTO THESE CARDS THEN INSERT THEM IN SYSPUNCH,           
* WHICH GIVES US A LOAD MODULE LEVEL STAMP FOR LK* AND LM* BOOKS.               
*                                                                               
* NOTE: THERE IS ONE DIFFERENCE BETWEEN THESE CARDS AND RMLNKSTAMP.             
*       BECAUSE RMLNKSTAMP IS A CATALP, IT CONTAINS THE CLAUSE                  
*       ' AS RMLNKSTAMP' AT THE END OF THE LEVEL STAMP. THAT IS                 
*       UNWANTED HERE, BECAUSE THIS IS A STAMP FOR A LOAD MODULE, NOT           
*       A RELO. THEREFORE, THAT CLAUSE IS OMITTED.                              
*                                                                               
LNKSTAMP DS    0D                  OBJECT MODULE FOR DDLNKSTAMP                 
*                                                                               
         DS    0CL80               ESD CARD                                     
         DC    AL1(02)                                                          
         DC    CL3'ESD'                                                         
         DC    CL6' '                                                           
         DC    AL2(16)                                                          
         DC    CL2' '                                                           
         DC    AL2(1)                                                           
         DC    CL8'LNKSTAMP'                                                    
         DC    AL4(0)                                                           
         DC    AL4(111)            CSECT LENGTH                                 
         DC    CL40' '                                                          
         DC    CL5'00001'                                                       
         DC    CL3' '                                                           
*                                                                               
         DS    0CL80               TXT CARD 1                                   
         DC    AL1(02)                                                          
         DC    CL3'TXT'                                                         
         DC    CL1' '                                                           
         DC    AL3(0)                                                           
         DC    CL2' '                                                           
         DC    AL2(11)                                                          
         DC    CL2' '                                                           
         DC    AL2(1)                                                           
         DC    CL10'*LNKSTAMP*'                                                 
         DC    AL1(0)                                                           
         DC    CL45' '                                                          
         DC    CL5'00002'                                                       
         DC    CL3' '                                                           
*                                                                               
         DS    0CL80               TXT CARD 2                                   
         DC    AL1(02)                                                          
         DC    CL3'TXT'                                                         
         DC    CL1' '                                                           
         DC    AL3(16)                                                          
         DC    CL2' '                                                           
         DC    AL2(56)                                                          
         DC    CL2' '                                                           
         DC    AL2(1)                                                           
         DC    CL5'BOOK='                                                       
LNKBOOK  DC    CL10'*LNKBOOK**'                                                 
LNK_PROMOTE_FLAG DC C' '           SET THIS TO A NON-BLANK TO FORCE...          
*                                   ...A PANAPT LEVEL STAMP MISMATCH            
         DC    CL6'LEVEL='                                                      
LNKLVL   DC    CL3'NNN'                                                         
         DC    CL6' DATE='                                                      
LNKDATE  DC    CL8'MMMDD/YY'                                                    
         DC    CL6' TIME='                                                      
LNKTIME  DC    CL8'HH:MM:SS'                                                    
         DC    CL3' CO'                                                         
         DC    CL5'00003'                                                       
         DC    CL3' '                                                           
*                                                                               
         DS    0CL80               TXT CARD 3                                   
         DC    AL1(02)                                                          
         DC    CL3'TXT'                                                         
         DC    CL1' '                                                           
         DC    AL3(72)                                                          
         DC    CL2' '                                                           
         DC    AL2(39)                                                          
         DC    CL2' '                                                           
         DC    AL2(1)                                                           
         DC    CL10'MPILED ON '                                                 
LNKCDATE DC    CL8'MMMDD/YY'                                                    
         DC    CL4' AT '                                                        
LNKCTIME DC    CL5'HH:MM'                                                       
         DC    CL4' BY '                                                        
LNKCUSER DC    CL8'XXXXXXXX'                                                    
         DC    CL14' '             BLANKS: NOT "AS RMLNKSTAMP" !                
         DC    CL3' '                                                           
         DC    CL5'00004'                                                       
         DC    CL3' '                                                           
*                                                                               
         DS    0CL80               END CARD                                     
         DC    AL1(02)                                                          
         DC    CL3'END'                                                         
         DC    CL28' '                                                          
         DC    CL10'1569623400'                                                 
         DC    CL1' '                                                           
         DC    CL9'010406210'                                                   
         DC    CL20' '                                                          
         DC    CL5'00005'                                                       
         DC    CL3' '                                                           
*                                                                               
LNKCRDSQ EQU   (*-LNKSTAMP)/80     NUMBER OF CARDS TO GENERATE                  
         EJECT                                                                  
* PANTBL DEFINES PAN ACTIONS                                                    
* AL1    LENGTH OF PAN ACTION                                                   
* CL8    PAN ACTION NAME                                                        
* CL1    PAN ACTION FLAG - C'Y'=&& CAN FOLLOW - C'N'=NO && ALLOWED              
*                                                                               
PANTBL   DS    0H                                                               
         DC    AL1(6),CL8'UPDATE  ',C'Y'                                        
         DC    AL1(3),CL8'ADD     ',C'Y'                                        
         DC    AL1(5),CL8'LEVEL   ',C'Y'                                        
         DC    AL1(4),CL8'COPY    ',C'N'                                        
         DC    AL1(6),CL8'RENAME  ',C'N'                                        
         DC    AL1(6),CL8'STATUS  ',C'N'                                        
         DC    AL1(6),CL8'INSERT  ',C'N'                                        
         DC    AL1(5),CL8'WRITE   ',C'N'                                        
         DC    AL1(6),CL8'SELECT  ',C'N'                                        
         DC    AL1(0)              EOT                                          
         SPACE 2                                                                
PANTBLD  DSECT ,                                                                
PANACTLN DS    AL1                 PAN ACTION LENGTH                            
PANACTNM DS    CL8                 PAN ACTION NAME                              
PANACTFL DS    CL1                 PAN ACTION FLAG                              
PANTBLLQ EQU   *-PANTBLD                                                        
         SPACE 2                                                                
PANEXIT  CSECT                                                                  
         SPACE 2                                                                
* PRCTBL DEFINES VALID PROCEDURES AND THE ACTIONS AVAILABLE                     
* AL1    PROC NUMBER                                                            
* XL1    PROC FLAGS - SEE EQUATES BELOW                                         
* CL8    PROC NAME                                                              
* AL1    VALID ACTION NUMBERS FOR THIS PROC (MAX OF FOUR)                       
*                                                                               
PRCTBL   DS    0H                                                               
         DC    AL1(01),X'00',CL8'PANA    ',AL1(01,02,00,00)                     
         DC    AL1(02),X'40',CL8'PANACAT ',AL1(03,04,00,00)                     
         DC    AL1(03),X'40',CL8'PANACATP',AL1(03,04,00,00)                     
         DC    AL1(04),X'20',CL8'PANACATR',AL1(03,05,00,00)                     
         DC    AL1(05),X'00',CL8'PANALNK ',AL1(06,07,08,00)                     
         DC    AL1(06),X'80',CL8'PANLNK  ',AL1(06,07,08,00)                     
         DC    AL1(07),X'10',CL8'PANSORT ',AL1(10,00,00,00)  DFSORT             
         DC    AL1(00)             EOT                                          
         SPACE 3                                                                
PRCTBLD  DSECT ,                                                                
PRCTBL#  DS    AL1                 PROC NUMBER                                  
PRCFLAGS DS    XL1                 PROC FLAGS                                   
PRCFLAGS_LK     EQU X'80'                                                       
PRCFLAGS_CATALP EQU X'40'                                                       
PRCFLAGS_CATALR EQU X'20'                                                       
PRCFLAGS_DFSORT EQU X'10'                                                       
PRCTBLNM DS    CL8                 PROC NAME                                    
PRCACTS  DS    4AL1                VALID ACTION NUMBERS                         
PRCTBLLQ EQU   *-PRCTBLD                                                        
         SPACE 2                                                                
PANEXIT  CSECT                                                                  
         SPACE 2                                                                
* LNKTBL DEFINES &&CARDS THAT CAN FOLLOW &&PHASE                                
* AL1    LENGTH OF LINK ACTION NAME                                             
* XL1    LINK ACTION FLAGS - X'80'=INSERT AT END OF LINK CONTROL                
* CL8    LINK ACTION NAME                                                       
*                                                                               
LNKTBL   DS    0H                                                               
         DC    AL1(05),X'80',CL8'ALIAS'                                         
         DC    AL1(06),X'00',CL8'CHANGE'                                        
         DC    AL1(05),X'80',CL8'ENTRY'                                         
         DC    AL1(06),X'00',CL8'EXPAND'                                        
         DC    AL1(08),X'00',CL8'IDENTIFY'                                      
         DC    AL1(07),X'00',CL8'INCLUDE'                                       
         DC    AL1(07),X'00',CL8'LIBRARY'                                       
         DC    AL1(05),X'80',CL8'ORDER'                                         
         DC    AL1(07),X'00',CL8'REPLACE'                                       
         DC    AL1(07),X'80',CL8'SETCODE'                                       
         DC    AL1(06),X'00',CL8'SETOPT'                                        
         DC    AL1(06),X'80',CL8'SETSSI'                                        
         DC    AL1(00)             EOT                                          
         SPACE 2                                                                
LNKTBLD  DSECT ,                                                                
LNKACTLN DS    AL1                 LINK ACTION LENGTH                           
LNKACTFL DS    XL1                 LINK ACTION FLAGS                            
LNKACTFL_INSERT_AT_END EQU X'80'    INSERT AT END OF LINK CONTROL               
LNKACTNM DS    CL8                 LINK ACTION NAME                             
LNKTBLLQ EQU   *-LNKTBLD                                                        
         SPACE 2                                                                
PANEXIT  CSECT                                                                  
         EJECT                                                                  
* THE MACROS LISTED IN THIS TABLE ARE IBM MACROS WHICH, FOR ONE REASON          
* OR ANOTHER, PRODUCE WARNINGS WHEN ASSEMBLED. THE BIGGEST ISSUE WE             
* HAVE (AS OF Z/OS 2.2) IS THAT IBM MACROS SOMETIMES CONTAIN LOWERCASE          
* CHARACTERS IN SYMBOLS, WHICH WE DON'T PERMIT. THERE ARE ALSO PAGE0            
* REFERENCES WITHOUT AN EXPLICIT BASE REGISTER OF R0, WHICH CAUSES OUR          
* ASSEMBLER LISTING EXIT TO GENERATE A SEVERITY 8 MNOTE.                        
*                                                                               
* PANACEA SURROUNDS EACH OCCURRENCE OF THESE MACROS WITH THE                    
* NECESSARY ACONTROL STATEMENTS TO PERMIT THEM TO ASSEMBLE CLEANLY.             
* THIS OBVIATES THE NEED FOR THE APPLICATION TO PROVIDE THE ACONTROL            
* STATEMENTS WHICH WOULD OTHERWISE BE NECESSARY.                                
*                                                                               
* EACH MACRO IS PRECEDED BY THE NUMBER OF CHARACTERS ON WHICH TO DO AN          
* EXECUTED COMPARE. THIS ALLOWS US TO HANDLE ALL MACROS WITH A GIVEN            
* PREFIX. IF WE WANT TO ENFORCE AN EXACT MATCH ON THE MACRO NAME IN THE         
* TABLE, THEN WE ADD 1 TO THE COMPARE LENGTH, WHICH FORCES A COMPARE ON         
* AN ADDITIONAL BLANK CHARACTER.                                                
*                                                                               
* NOTE THAT THE LOGIC IS NOT PERFECT. IN PARTICULAR, WE DON'T HANDLE            
* SITUATIONS WHERE A MACRO CALL IS CONTINUED FROM ONE STATEMENT TO THE          
* NEXT. IN SUCH CASES, THE PROGRAMMER MUST PROVIDE THE ACONTROL                 
* STATEMENTS HIM/HERSELF.                                                       
*                                                                               
TROUBLESOME_IBM_MACROS DS 0H                                                    
         DC    AL1(3),CL8'CMQ'       LOWERCASE CHARS                            
         DC    AL1(4+1),CL8'BLDL'    LOWERCASE CHARS                            
         DC    AL1(3),CL8'BPX'       LOWERCASE CHARS                            
         DC    AL1(3),CL8'IEF'       LOWERCASE CHARS                            
         DC    AL1(3),CL8'IHA'       LOWERCASE CHARS                            
         DC    AL1(3),CL8'IKJ'       LOWERCASE CHARS                            
         DC    AL1(5+1),CL8'MODCB'   PAGE0 REF. (VIA IDACB2)                    
         DC    AL1(6+1),CL8'SHOWCB'  PAGE0 REF. (VIA ANOTHER MACRO)             
         DC    AL1(5+1),CL8'GENCB'   PAGE0 REF. (VIA ANOTHER MACRO)             
         DC    AL1(5+1),CL8'CHECK'   LOWERCASE CHARS                            
         DC    AL1(5+1),CL8'POINT'   LOWERCASE CHARS                            
         DC    AL1(7+1),CL8'SYNADAF' LOWERCASE CHARS                            
         DC    AL1(8),CL8'SYNADRLS'  LOWERCASE CHARS                            
         DC    AL1(6),CL8'IFASMF'    LOWERCASE CHARS                            
         DC    AL1(7+1),CL8'TRKCALC' CONTINUATION STATEMENT ISSUE               
         DC    AL1(3+1),CL8'CVT'     LOWERCASE CHARS                            
         DC    X'FF'                                                            
         SPACE 2                                                                
TROUBLED DSECT ,                                                                
TRUBPFXL DS    AL1                 MACRO COMPARE PREFIX LENGTH                  
TRUBMACR DS    CL8                 MACRO PREFIX CHARACTERS                      
TROUBLEQ EQU   *-TROUBLED                                                       
         SPACE 2                                                                
PANEXIT  CSECT                                                                  
         EJECT                                                                  
* MNOTE STATEMENTS SHOULD BE EXACTLY 80 CHARACTERS, BUT ONLY THE FIRST          
*  71 CHARACTERS CAN BE USED. OTHERWISE, THERE IS A CHANCE THAT THE             
*  GENERATED MNOTE WILL CAUSE A SYNTAX ERROR.                                   
*                                                                               
         DS    0H                                                               
NOBALMSG DS    0CL80                                                            
         DC    C' MNOTE 8,''BAL/BALR PROHIBITED. USE BAS/BASR INSTEAD.'+        
               ''                                                               
         DC    CL(72-(*-NOBALMSG))' '                                           
         DC    CL8' '                                                           
*                                                                               
BAD4IDF  DS    0CL80                                                            
         DC    C' MNOTE 4,''WARNING: THIS INSTRUCTION FAILS UNDER IDF!'+        
               ''                                                               
         DC    CL(72-(*-BAD4IDF))' '                                            
         DC    CL8' '                                                           
*                                                                               
*&&DO                                                                           
NO_IDF_SUPPORT_FOR_COMPARE_AND_BRANCH DS  0CL80                                 
         DC    C' MNOTE 2,''WARNING: IDF STMTSTEP UNSUPPORTED FOR XXXXX+        
               X: CONSIDER XXXXX'''                                             
         DC    CL(72-(*-NO_IDF_SUPPORT_FOR_COMPARE_AND_BRANCH))' '              
         DC    CL8' '                                                           
*&&                                                                             
*                                                                               
CSECTNOL DS    0CL80                                                            
         DC    C' MNOTE 8,''UNLABELED CSECT. CONTINUE PRIOR CSECT BY RE+        
               USING ITS LABEL.'''                                              
         DC    CL(72-(*-CSECTNOL))' '                                           
         DC    CL8' '                                                           
*                                                                               
NOCSECT  DS    0CL80                                                            
         DC    C' MNOTE 8,''NO CSECT STATEMENT WAS FOUND IN THIS SOURCE+        
                MODULE.'''                                                      
         DC    CL(72-(*-NOCSECT))' '                                            
         DC    CL8' '                                                           
*                                                                               
BAD_R0   DS    0CL80                                                            
         DC    C' MNOTE 8,''USE OF "R0," INVALID. IF INTENDED, SPECIFY +        
               "0," INSTEAD.'''                                                 
         DC    CL(72-(*-BAD_R0))' '                                             
         DC    CL8' '                                                           
*                                                                               
NOALTPHS DS    0CL80                                                            
         DC    C' MNOTE 8,''ALTERNATE 8-CHARACTER PHASE NAMES ARE NO LO+        
               NGER SUPPORTED.'''                                               
         DC    CL(72-(*-NOALTPHS))' '                                           
         DC    CL8' '                                                           
*                                                                               
INVALPHS DS    0CL80                                                            
         DC    C' MNOTE 8,''INVALID OR MISSING *PHASE CARD'''                   
         DC    CL(72-(*-INVALPHS))' '                                           
         DC    CL8' '                                                           
*                                                                               
WORK_PARM_ON_SORTCARD DS 0CL80                                                  
         DC    C' MNOTE 8,''REMOVE OBSOLETE ",WORK=1" PARAMETER FROM "S+        
               ORT FIELDS" CARD.'''                                             
         DC    CL(72-(*-WORK_PARM_ON_SORTCARD))' '                              
         DC    CL8' '                                                           
*                                                                               
PROCESS_STATEMENT_BAD_LOCATION DS 0CL80                                         
         DC    C' MNOTE 8,''*PROCESS STMTS MUST BE LOCATED AT THE START+        
                OF THE SOURCE.'''                                               
         DC    CL(72-(*-PROCESS_STATEMENT_BAD_LOCATION))' '                     
         DC    CL8' '                                                           
*                                                                               
* NOTE: THE "*PROCESS NOINFO" STATEMENT WILL BE GENERATED ONLY IF IT            
*       IS *NOT* OVERRIDDEN BY A "*PROCESS USING(WARN(15))" STATEMENT.          
*       IT REALLY MAKES NO DIFFERENCE EITHER WAY WHETHER WE GENERATE            
*       THE "*PROCESS NOINFO" STATEMENT OR NOT, AND THIS WAS THE                
*       EASIEST WAY TO FORCE THE OPTIONAL GENERATION OF A USING(WARN)           
*       OVERRIDE.                                                               
PROCWARN DC    C'*PROCESS NOINFO         ' INNOCUOUS *PROCESS STATEMENT         
*                                                                               
PEQUSMAC DC    CL80'         PEQUS ,'      PEQUS MACRO CALL                     
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SETAREA'                                                    
SETAREA  DS    (SETAREA#)CL(SETAREAQ)                                           
         DC    X'0000'                                                          
SETAREA# EQU   256/SETAREAQ        MAXIMUM NUMBER OF SET ENTRIES                
         SPACE 2                                                                
MAX_WRKAREA_ENTRIES EQU 12         MAX ENTRIES IN WORK RECORD AREA              
         DS    0D                                                               
         DC    CL8'*WRKAREA'                                                    
         DC    AL2(MAX_WRKAREA_ENTRIES)                                         
WRKAREA  DS    (MAX_WRKAREA_ENTRIES)CL80                                        
WRKAREAX EQU   *                                                                
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'*LNKAREA'                                                    
         DC    AL2(LNKAREAQ)       NUM ENTRYS IN LNKEDT STATEMENT AREA          
LNKAREA  DS    (LNKAREAQ)CL80                                                   
LNKAREAQ EQU   150                                                              
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'*GENAREA'                                                    
         DC    AL2(GENAREAQ)       NUM ENTRYS IN GEN AREA                       
GENAREA  DS    (GENAREAQ)CL80                                                   
GENAREAQ EQU   (3*LNKAREAQ)+12                                                  
         SPACE 2                                                                
         DS    0D                                                               
MAXINCLQ EQU   500                                                              
         DC    C'*INCLS**'                                                      
INCLST   DC    (MAXINCLQ)XL10'00'  TABLE OF *INCLUDES AND ++INCLUDES            
         EJECT                                                                  
SETAREAD DSECT ,                                                                
SETNAME  DS    CL5                 &&SET SYMBOL NAME                            
         ORG   SETNAME                                                          
SETNAME2 DS    CL2                 FOR 2-CHARACTER NAMES (E.G., &&US)           
SETNAME2_BLANKS DS CL3              (REMAINDER ARE BLANKS)                      
         ORG                                                                    
SETVALUE DS    C                   &&SET SYMBOL VALUE                           
SETWHERE DS    C                   &&SET SYMBOL ORIGIN                          
SET_VIA_DEFAULT  EQU C'D'          VALUE SET AS DEFAULT BY PANACEA              
SET_VIA_OVERRIDE EQU C'O'          OVERRIDE VALUE FOR THIS ASSEMBLY             
SET_VIA_SOURCE   EQU C'S'          VALUE SET WITHIN SOURCE CODE                 
SETAREAQ EQU   *-SETAREAD                                                       
         SPACE 2                                                                
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
         IHAPSA                                                                 
         POP   ACONTROL                                                         
*                                                                               
         IHAACEE                                                                
*                                                                               
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
         IHAASCB                                                                
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         POP   ACONTROL                                                         
*                                                                               
         IHAASXB                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110DDPANACEA 09/14/20'                                      
         END                                                                    
