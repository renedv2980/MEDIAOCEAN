*          DATA SET DDSCRAMPAN AT LEVEL 048 AS OF 10/20/15                      
*PHASE SCRAMPA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PANIC                                                                  
         TITLE 'COMPILE SCRIPTS FROM PAN LIBRARY'                               
         PRINT NOGEN                                                            
         EJECT                                                                  
**********************************************************************          
* SCRIPT SOURCE ONE PASS COMPILER                                    *          
**********************************************************************          
SCRAMBLE CSECT                                                                  
         NBASE WORKX-WORKD,SCRAMBLE,R6,WORK=V(REGSAVE)                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
*                                                                               
SCRAM010 GOTO1 =V(CARDS),PARMS,CARD,=C'RE00'                                    
         CLC   CARD(2),=C'/*'                                                   
         BE    SCRAM020                                                         
         CLC   =C'DDSIO=',CARD  ALLOW DDSIO OVERRIDE                            
         BNE   SCRAM012                                                         
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     SCRAM010                                                         
*                                                                               
         USING SSBD,RE                                                          
SCRAM012 CLC   =C'DSPACE=',CARD                                                 
         BNE   SCRAM014                                                         
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
         B     SCRAM010                                                         
*                                                                               
SCRAM014 CLC   =C'WRITE=N',CARD                                                 
         BNE   SCRAM015                                                         
         MVI   MCWRITE,C'N'                                                     
                                                                                
         L     RE,=A(SSB)                                                       
         OI    SSOMTIND,SSOWRTN                                                 
         B     SCRAM010                                                         
         DROP  RE                                                               
*                                                                               
SCRAM015 MVC   PANBOOK(10),CARD    NAME OF BOOK                                 
         MVC   SCRPNAME,CARD+2     DEFAULT NAME OF SCRIPT                       
         B     SCRAM010                                                         
*                                                                               
SCRAM020 CLI   PANBOOK,C' '                                                     
         BH    SCRAM040                                                         
         MVC   PLINE(15),=C'BOOK NOT INPUT '                                    
         BAS   RE,PRNTLINE                                                      
         B     XBASEX                                                           
*                                                                               
SCRAM040 GOTO1 =V(DATAMGR),PARMS,DMOPEN,DMSYS,DMFLIST                           
         B     SCRM02                                                           
*                                                                               
EXIT     L     RD,SAVERD                                                        
*                                                                               
         GOTO1 =V(DATAMGR),PARMS,DMCLSE,DMSYS                                   
*                                                                               
         GOTO1 =V(DMENQDEQ),PARMS,(C'D',=C'CTRL')                               
*                                                                               
XBASEX   XBASE                                                                  
         EJECT                                                                  
**********************************************************************          
*        MAIN CONTROL LOOP                                           *          
**********************************************************************          
SCRM02   XC    ASMLINE,ASMLINE     RESET COMPILED LINE                          
         BAS   RE,LINEIN           GET NEXT LINE                                
         BL    SCRM12              NO MORE LINES                                
         CLC   =C'@@',WORK1        CONDITIONAL ASSEMBLY - NOTHING ELSE          
         BE    SCRM12                                                           
         LH    RF,LINENUM          INCREMENT LINE COUNT                         
         LA    RF,1(RF)                                                         
         STH   RF,LINENUM                                                       
         CVD   RF,DUB1                                                          
         OI    DUB1+L'DUB1-1,X'0F'                                              
         UNPK  DATANUM,DUB1        PUT LINE NUMBER ON PRINTLINE                 
         MVC   DATA1,WORK1         SAVE A COPY OF THIS LINE                     
*                                                                               
         TM    SCPFLAG1,SF1NAME    GOT NAME & BUILT DUMMY RECORD?               
         BO    SCRM08              YES                                          
*                                                                               
         CLI   WORK1,C'*'          COMMENT BEFORE NAME                          
         BNE   SCRM04              NO                                           
*                                                                               
         CLC   WORK1(19),=C'*          DATA SET'                                
         BNE   SCRM03                                                           
*                                                                               
         MVC   PLINE,WORK1                                                      
         MVC   PANBOOK,WORK1+20                                                 
         MVC   PANDATE,WORK1+50                                                 
         MVC   PANLEVL,WORK1+40                                                 
*                                                                               
SCRM03   BAS   RE,PRNTLINE         YES - IGNORE IT                              
         B     SCRM02                                                           
*                                                                               
SCRM04   CLC   =C'#SCRIPT',WORK1 SCRIPT NAME CARD?                              
         BNE   SCRM06              NO                                           
         BAS   RE,GETNAME          BUILD SCRIPT NAME                            
         XC    ASM1,ASM1                                                        
         BAS   RE,PRNTLINE                                                      
         B     SCRM02                                                           
*                                                                               
SCRM06   BAS   RE,BLDFRST                                                       
         OI    SCPFLAG1,SF1NAME    SET RECORD BUILT FLAG                        
*                                                                               
SCRM08   CLC   =CL4'#DEF',WORK1    LINE DEFINES EQUATES                         
         BNE   SCRM10              NO                                           
         BAS   RE,EQUATES          SET UP EQUATE TABLE                          
         B     SCRM02                                                           
*                                                                               
SCRM10   BAS   RE,SUBEQUS          SUBSTITUTE EQUATES                           
         BAS   RE,COMPILE          COMPILE SCRIPT CODE + PRINT LINE             
*                                                                               
         TM    SCPFLAG1,SF1ERRS    ANY COMPILE ERRORS                           
         BO    SCRM02              YES - DON'T BOTHER WITH RECORD               
         BAS   RE,BLDELS           BUILD ELEMENTS                               
         B     SCRM02                                                           
*                                                                               
SCRM12   OI    SCPFLAG1,SF1LAST    FLAG FOR LAST TIME                           
         BAS   RE,SETLBL           SORT OUT LABELS                              
         TM    SCPFLAG1,SF1ERRS    ANY ERRORS?                                  
         BZ    SCRM14              NO                                           
*                                                                               
         MVC   PLINE,MYSPACES                                                   
         XC    ASM1,ASM1                                                        
         MVC   PLINE+30(L'ERCNTMSG),ERCNTMSG                                    
         LH    R1,TOTERRS                                                       
         CVD   R1,DUB1                                                          
         OI    DUB1+L'DUB1-1,X'0F'                                              
         UNPK  PLINE+34(4),DUB1                                                 
         BAS   RE,PRNTLINE                                                      
         B     EXIT                                                             
*                                                                               
SCRM14   BAS   RE,BLDELS           BUILD LAST ELEMENT                           
         TM    SCPFLAG1,SF1ERRS    ANY ERRORS?                                  
         BO    EXIT                NO                                           
*                                                                               
         MVC   PLINE+30(L'RECUPMSG),RECUPMSG                                    
         BAS   RE,PRNTLINE                                                      
*                                                                               
         TM    SCPFLAG1,SF1ADD                                                  
         BNZ   SCRM15                                                           
*                                                                               
         GOTO1 =V(DATAMGR),PARMS,DMWRITE,CTFILE,IOKEY,AIO2                      
         B     SCRM16                                                           
*                                                                               
SCRM15   GOTO1 =V(DATAMGR),PARMS,DMADD,CTFILE,IOKEY,AIO2                        
*                                                                               
SCRM16   CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
XITH     CLI   *,0                 SET CC HIGH                                  
         B     XIT                                                              
XITL     CLI   *,FF                SET CC LOW                                   
         B     XIT                                                              
XITOK    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
XIT      XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP EQUATE SUBSTITUTION TABLE                         *         
***********************************************************************         
EQUATES  NTR1                                                                   
         GOTO1 =V(SQUASHER),PARMS,WORK1,L'WORK1                                 
         L     R0,4(R1)            L'INPUT AFTER SQUASHING                      
         LA    RF,WORK1+4          START OF INPUT STRING + #DEF                 
         LR    R3,RF               R3 = POINTER TO CURRENT POSN.                
*                                                                               
EQU02    LA    RE,WORK1            START OF LINE                                
         SR    RF,RE               CURRENT DISPLACEMENT INTO LINE               
         CR    RF,R0               REACHED END OF LINE?                         
         BNL   EQU24               YES                                          
*                                                                               
         CLI   0(R3),C' '          SPACE PRESENT?                               
         BH    *+8                 NO                                           
         LA    R3,1(R3)            IGNORE IT                                    
         LR    R1,R3               SAVE A(START OF EQUATED FROM)                
*                                                                               
EQU04    CLI   0(R3),C' '          END OF EQUATED FROM VALUE?                   
         BE    EQU06               YES                                          
         LA    R3,1(R3)            NEXT CHARACTER ON LINE                       
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0               REACHED END OF LINE?                         
         BL    EQU04               NO                                           
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         LR    RF,R3               CURRENT POSITION                             
         SR    RF,R1               GIVES LENGTH OF EQUATED FROM VALUE           
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
         B     EQU24                                                            
*                                                                               
         USING EQUTABD,R2                                                       
EQU06    LA    R2,EQUTABLE         TABLE OF EQUATES                             
         LR    RF,R3                                                            
         SR    RF,R1               RF NOW HOLDS L'EQUATED FROM VALUE            
         CHI   RF,MAXEQFR          MAXIMUM LABEL LENGTH                         
         BNH   EQU14                                                            
*                                                                               
EQU08    MVI   THISERR,9           LABEL IS TOO BIG                             
         MVI   ASMERR,FF                                                        
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
*                                                                               
EQU10    CLI   0(R3),C';'          LOOK FOR DELIMETER                           
         BE    EQU12                                                            
         LA    R3,1(R3)            BUMP ON                                      
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0               REACHED END OF LINE?                         
         BL    EQU10               NO                                           
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         LR    RF,R3               CURRENT POSITION                             
         SR    RF,R1               RF = L'EQUATED FROM VALUE                    
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
         B     EQU24                                                            
*                                                                               
EQU12    LA    R3,1(R3)            BUMP PAST ;                                  
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU14    CLI   EQULEN,0            FIND NEXT FREE IN EQUATE TABLE               
         BE    EQU16                                                            
         XR    RE,RE                                                            
         IC    RE,EQULEN                                                        
         LA    R2,0(RE,R2)                                                      
         B     EQU14                                                            
*                                                                               
EQU16    MVC   EQUNAME,MYSPACES    SPACE FILL EQUATED NAME AREA                 
         BCTR  RF,0                                                             
         MVC   EQUNAME(0),0(R1)    MOVE IN THE EQUATED NAME VALUE               
         EX    RF,*-6                                                           
*                                                                               
         LA    R3,1(R3)            BUMP PAST SPACE AT END OF NAME               
         LR    R1,R3               SAVE A(START OF EQUATED TO VALUE)            
         LA    RE,WORK1                                                         
*                                                                               
EQU18    CLI   0(R3),C';'          END OF EQUATED TO DELIMITER?                 
         BE    EQU20               YES                                          
         LA    R3,1(R3)                                                         
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0                                                            
         BL    EQU18               STILL WITHIN BOUNDARIES                      
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
         B     EQU24                                                            
*                                                                               
EQU20    LR    RF,R3                                                            
         SR    RF,R1               RF = L'EQUATED VALUE                         
         CHI   RF,MAXEQTO          MAXIMUM OPERAND LENGTH                       
         BL    EQU22               LENGTH IS OK                                 
*                                                                               
         MVI   THISERR,9           LABEL TOO LONG                               
         MVI   ASMERR,FF                                                        
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
         LA    R3,1(R3)            BUMP PAST THE ;                              
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU22    BCTR  RF,0                                                             
         MVC   EQUDATA,0(R1)       MOVE IN THE EQUATED VALUE                    
         EX    RF,*-6                                                           
         LA    RF,EQLNQ+1(RF)      SET TOTAL LENGTH                             
         STC   RF,EQULEN           SAVE THE TOTAL LENGTH OF ELEMENT             
         LA    R2,0(RF,R2)                                                      
         MVI   EQULEN,0            ENSURE EOT DELIMITED                         
         LA    R3,1(R3)            FOR ;                                        
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU24    XC    ASM1,ASM1                                                        
         BAS   RE,PRNTLINE                                                      
         B     XITOK                                                            
         DROP  R2                                                               
*                                                                               
EQUTABD  DSECT                                                                  
EQULEN   DS    CL1                 L'INPUT                                      
EQUNAME  DS    CL(MAXEQFR)         EQUATE NAME                                  
EQUDATA  DS    0C                  EQUATE VALUE                                 
EQLNQ    EQU   *-EQUTABD                                                        
*                                                                               
MAXEQFR  EQU   8                                                                
MAXEQTO  EQU   32                                                               
*                                                                               
SCRAMBLE CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SUSTITUTE EQUATED VALUES ON THE ASSEMBLED LINE           *         
***********************************************************************         
         SPACE 1                                                                
SUBEQUS  NTR1                                                                   
         CLI   WORK1,C'*'          IGNORE THIS LINE                             
         BE    XITOK               YES                                          
         GOTO1 =V(SQUASHER),PARMS,WORK1,L'WORK1                                 
         L     R0,4(R1)            L'INPUT AFTER SQUASHING                      
         LA    R3,WORK1            R3 = CURRENT CHARACTER POINTER               
*                                                                               
SUB02    LR    RF,R3                                                            
         LA    RE,WORK1                                                         
         SR    RF,RE                                                            
         CR    RF,R0                                                            
         BH    XITOK               REACHED END OF LINE                          
*                                                                               
         CLI   0(R3),SUBCHAR       SUBSTITUTION CHARACTER?                      
         BE    SUB04               YES                                          
         LA    R3,1(R3)                                                         
         B     SUB02               NEXT ON LINE                                 
*                                                                               
SUB04    CLI   1(R3),SUBCHAR       TWO SUBCHARS MEANS LEAVE IN 1                
         BE    SUB18                                                            
         LR    RF,R3               START OF STRING TO REPLACE                   
         LA    R3,1(R3)            EQUATE TO REPLACE                            
*                                                                               
SUB06    CLI   0(R3),C';'          END                                          
         BE    SUB10                                                            
         CLI   0(R3),C' '          MISSING DELIMETER                            
         BE    SUB08                                                            
         LA    R3,1(R3)                                                         
         B     SUB06               NEXT ON LINE                                 
*                                                                               
SUB08    MVI   THISERR,5                                                        
         MVI   ASMERR,0                                                         
         LR    R1,RF               A(START OF TEXT)                             
         LR    RF,R3                                                            
         SR    RF,R1               L'TEXT (PRESERVES R3)                        
         GOTO1 ERRHNDL,PARMS,((RF),(R1))                                        
         LA    R3,1(R3)            BUMP PAST SPACE                              
         B     SUB02                                                            
*                                                                               
SUB10    LR    R1,R3               END OF EQUATE NAME                           
         SR    R1,RF               START OF EQUATE NAME INCLUDING %             
         SHI   R1,2                1 FOR % 1FOR THE EX                          
         LA    R2,EQUTABLE                                                      
         USING EQUTABD,R2                                                       
         XR    RE,RE                                                            
*                                                                               
SUB12    CLI   EQULEN,0                                                         
         BE    SUB14               EQUATE NOT FOUND                             
         EX    R1,*+8                                                           
         BE    SUB16                                                            
         CLC   EQUNAME(0),1(RF)    RF POINTS TO THE % REMEMBER                  
         IC    RE,EQULEN                                                        
         LA    R2,0(RE,R2)                                                      
         B     SUB12                                                            
*                                                                               
SUB14    MVI   THISERR,8                                                        
         MVI   ASMERR,0                                                         
         LA    R1,1(R1)            L' OF EQUATE                                 
         GOTO1 ERRHNDL,PARMS,((R1),(RF))                                        
         LA    R3,1(R3)            FOR THE ;                                    
         B     SUB02                                                            
*                                                                               
SUB16    IC    RE,0(R2)            L' WHOLE THING EQUATE ELEMENT                
         SHI   RE,EQLNQ            L' WHAT MUST BE MOVED IN                     
*                                                                               
         LA    R4,WORK1                                                         
         AR    R4,R0                                                            
         SR    R4,R3               R4=L' WHAT'S LEFT ON LINE                    
         AR    R0,RE                                                            
         SR    R0,R1               NEW LENGTH OF THIS LINE IN R0                
         BCTR  R4,0                                                             
         MVC   ASM1(0),0(R3)       SAVE WHAT'S LEFT OF LINE                     
         EX    R4,*-6                                                           
*                                                                               
         BCTR  RE,0                                                             
         MVC   0(0,RF),EQUDATA     MOVE IN EQUATE VALUE                         
         EX    RE,*-6                                                           
         LA    R3,1(RE,RF)         SET R3 TO END OF THIS VALUE                  
*                                                                               
         LA    RF,WORK1+L'WORK1-1                                               
         SR    RF,R3                                                            
         MVC   0(0,R3),MYSPACES    CLEAR DOWN LINE                              
         EX    RF,*-6                                                           
*                                                                               
         MVC   0(0,R3),ASM1        MOVE BACK WHAT`S LEFT                        
         EX    R4,*-6                                                           
         XC    ASM1,ASM1                                                        
         B     SUB02                                                            
*                                                                               
SUB18    LA    R3,1(R3)            LEAVE IN FIRST %                             
         LR    RF,R3                                                            
         LA    RE,WORK1                                                         
         SR    RF,RE               CURRENT TEXT LENGTH PROCESSED                
         LR    R1,R0               TOTAL TEXT LENGTH                            
         SR    R1,RF                                                            
         BCTR  R1,0                L' LEFT TO MOVE -1 FOR MVC                   
         MVC   0(0,R3),1(R3)       CLOSE UP 1 CHARACTER                         
         EX    R1,*-6                                                           
         SHI   R0,1                REDUCE LINE LENGTH                           
         B     SUB02                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO COMPILE A LINE FROM WORK1 TO ASMLINE                     *         
***********************************************************************         
         SPACE 1                                                                
SAVE     USING ASMTABD,SVASM                                                    
COMPILE  NTR1                                                                   
*                                                                               
         L     RF,DISPTOT                                                       
         EDIT  (RF),(5,ASMDISP),ZERO=NOBLANK                                    
*                                                                               
         GOTO1 =V(SQUASHER),PARMS,WORK1,L'WORK1                                 
         ICM   R0,15,4(R1)         L'INPUT AFTER SQUASHING                      
         BZ    CMPLX               NO INPUT                                     
         ST    R0,ALINLN                                                        
         LA    R3,WORK1            SOURCE LINE                                  
         LA    R4,ASMLINE                                                       
*                                                                               
         LR    RF,R0               SUBSTITUTE ' ' FOR 'º'                       
         LA    RE,WORK1            TO ALLOW CHECKING FOR SPACES                 
         CLI   0(RE),C'º'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
CMPL02   XC    SVASM,SVASM         CLEAR LAST ASMTAB ENTRY                      
         CLI   0(R3),C' '          SPACE BEFORE INSTRUCTION?                    
         BNE   *+8                                                              
         LA    R3,1(R3)            BUMP PAST SPACE                              
*                                                                               
         TM    SCPFLAG1,SF1CMT     INSIDE /*....*/ PAIR?                        
         BO    CMPL04              YES                                          
*                                                                               
         TM    SCPFLAG1,SF1LCMT    LAST PROCESSED WAS COMMENT CLOSE             
         BO    *+12                                                             
         CLI   WORK1,C'*'          WHOLE LINE IS A COMMENT?                     
         BE    CMPLX               YES                                          
*                                                                               
         NI    SCPFLAG1,FF-SF1LCMT RESET COMMENT CLOSE FLAG                     
*                                                                               
         CLC   =C'//',0(R3)        COMMENT TO END OF LINE?                      
         BE    CMPLX               YES - NOTHING LEFT TO PROCESS                
*                                                                               
         CLC   =C'/*',0(R3)        START OF /*....*/ PAIR?                      
         BNE   CMPL08              NO                                           
         OI    SCPFLAG1,SF1CMT     TURN ON INDICATOR BYTE                       
*                                                                               
CMPL04   CLC   =C'*/',0(R3)        LOOK FOR */ TO CLOSE COMMENT                 
         BE    CMPL06              GOT ONE!                                     
         LA    R3,1(R3)            NEED TO GET DISPLACEMENT INTO LINE           
         LR    RE,R3                                                            
         LA    RF,WORK1            START OF INPUT LINE                          
         SR    RE,RF                                                            
         CR    RE,R0               END OF LINE?                                 
         BH    CMPLX               YES                                          
         B     CMPL04              NOT YET - STILL WITHIN COMMENT               
*                                                                               
CMPL06   NI    SCPFLAG1,FF-SF1CMT TURN OFF COMMENT FLAG                         
         LA    R3,2(R3)            GO PAST THE */                               
         LR    RE,R3                                                            
         LA    RF,WORK1            START OF INPUT LINE                          
         SR    RE,RF               DISPLACEMENT INTO LINE                       
         CR    RE,R0               END OF LINE?                                 
         BNL   CMPLX               YES                                          
         OI    SCPFLAG1,SF1LCMT    SET COMMENT CLOSE FLAG                       
         B     CMPL02              CARRY ON PROCESSING                          
*                                                                               
CMPL08   LA    R2,ASMTAB           OPCODE TABLE                                 
         USING ASMTABD,R2                                                       
         LA    RE,ASMTABLQ         L'ASMTAB                                     
         L     RF,=A(ASMTABX)      END OF TABLE                                 
         XR    R1,R1                                                            
*                                                                               
CMPL10   IC    R1,MTCHLEN          LENGTH OF MNEMONIC-1                         
         EX    R1,CMPLCMP                                                       
         BE    CMPL12              MATCH ON OP-CODE                             
         BXLE  R2,RE,CMPL10                                                     
         BAS   RE,INVOPCDE         ROUTINE TO DEAL WITH INVALID OPCODE          
         BE    CMPL16              PROCESS NEXT INSTRUCTION                     
         B     CMPLX               NOTHING LEFT ON LINE                         
*                                                                               
CMPLCMP  CLC   MNEMONIC(0),0(R3)                                                
*                                                                               
CMPL12   MVC   SVASM,ASMTABD       SAVE ASMTAB ENTRY                            
         MVC   0(2,R4),SAVE.OPCODE PUT OPCODE ONTO ASMLINE                      
         LA    R4,2(R4)            NEXT FREE ON ASMLINE                         
         LA    R3,1(R1,R3)         NEXT AFTER MNEMONIC                          
         CLI   SAVE.MXOPERS,0      ANY OPERANDS?                                
         BE    CMPL16              NO                                           
*                                                                               
         CLI   0(R3),C' '          FOR SPACE TWIXT OPCODEºOPERAND               
         BNE   *+8                 (IF OPCODE HAS OPERANDS)                     
         LA    R3,1(R3)                                                         
*                                                                               
         TM    SAVE.MXOPERS,MXLBL+MXBRNCH                                       
         BZ    *+8                                                              
         BAS   RE,SETLBL           SAVE BRANCH EQUATES                          
         LA    RF,DATATAB                                                       
*                                                                               
CMPL14   CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN DATA TYPE - DIE AT ONCE              
*                                                                               
         CLC   SAVE.DATATYPE,0(RF) MATCH ON PROCESSING TYPE?                    
         BE    *+12                YES                                          
         LA    RF,DATATABL(RF)     BUMP TO NEXT                                 
         B     CMPL14                                                           
*                                                                               
         ICM   RF,15,1(RF)         GO VALIDATE THIS INPUT                       
         BASR  RE,RF                                                            
         BNE   CMPLX                                                            
*                                                                               
CMPL16   CLI   0(R3),C' '          NEXT CHARACTER IS A SPACE?                   
         BNE   *+8                 NO                                           
         LA    R3,1(R3)            BUMP PAST SPACE                              
         CLI   0(R3),C';'          NEXT CHARACTER MUST BE A DELIMETER           
         BE    CMPL18              ELSE SOME COCK-UP HAS OCCURRED               
         BAS   RE,INVDLMTR                                                      
         BNE   CMPLX               END OF LINE REACHED                          
*                                                                               
CMPL18   LA    R3,1(R3)            FOR ; DELIMITER                              
         LR    RF,R3               SAVE THIS ADDRESS                            
         LA    RE,WORK1            START OF LINE                                
         LA    RF,1(RF)            FOR SPACE AT END                             
         SR    RF,RE               GIVES DISPLACEMENT INTO INPUT LINE           
         CR    RF,R0               REACHED END OF LINE?                         
         BNH   CMPL02              NO                                           
*                                                                               
CMPLX    MVC   ASM1,ASMLINE                                                     
         GOTO1 PRNTLINE                                                         
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET A LINE OF SOURCE                                     *         
***********************************************************************         
         SPACE 1                                                                
LINEIN   NTR1                                                                   
         MVC   WORK1,MYSPACES                                                   
         GOTO1 =V(PANIC),PARMS,=C'READ',=C'PAN',PANBOOK,WORK1                   
         CLI   8(R1),0                                                          
         BE    PANGET1                                                          
         CLI   8(R1),X'80'                                                      
         BE    XITL                                                             
         CLI   8(R1),X'10'         INDICATES BOOK NOT FOUND                     
         BNE   *+14                                                             
         MVC   PLINE(15),=C'BOOK ERROR     '                                    
         BAS   RE,PRNTLINE                                                      
         B     EXIT                                                             
*                                                                               
PANGET1  MVC   WORK1+72(8),MYSPACES    GET RID OF THE FUCKING NUMBERS           
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PRINT OUT A LINE OF COMPILED SCRIPT & OBJECT CODE        *         
***********************************************************************         
         SPACE 1                                                                
PRNTLINE NTR1                                                                   
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
         MVC   TITLE(26),=C'OFF - LINE SCRIPT COMPILER'                         
         MVC   P(192),MYSPACES                                                  
         MVC   P,PLINE                                                          
         TR    P,TRTAB             REMOVE NON DISPLAY CHRS                      
*                                                                               
         ZIC   R0,8(R1)            # OF ERRORS ON THIS LINE                     
         L     R2,8(R1)            A(1ST CL80 ERROR BLOCK)                      
*                                                                               
         GOTO1 =V(PRINTER)         PRINT FIRST I/O LINE                         
                                                                                
         LTR   R0,R0               ANY ERRORS?                                  
         B     XITOK               NO                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD NAME FOR SCRIPT FROM #SCRIPT LINE                             *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         LA    R1,WORK1+8          FIRST PAST THE #SCRIPT MARKER                
*                                                                               
NAM02    CLI   0(R1),C' '          LOOK FOR FIRST NON-SPACE AFTER               
         BH    *+12                #SCRIPT CARD                                 
         LA    R1,1(R1)                                                         
         B     NAM02                                                            
*                                                                               
         MVC   SCRPNAME,0(R1)      SAVE NAME OF SCRIPT                          
         LA    R0,L'SCRPNAME                                                    
         LA    RF,SCRPNAME+L'SCRPNAME-1                                         
*                                                                               
NAM04    CLI   0(RF),C' '          CHANGE ALL FUNNIES TO SPACES                 
         BH    *+8                                                              
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,NAM04                                                         
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD CT7KEY AND FORMAT FIRST RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
BLDFRST  NTR1                                                                   
         LA    R1,SCRPNAME+L'CT7KCODE                                           
         CLI   0(R1),C' '                                                       
         BE    BDFS02                                                           
         MVI   THISERR,11          NAME IS TOO LONG                             
         BAS   RE,ERRHNDL                                                       
         BAS   RE,PRNTLINE                                                      
         B     XITL                                                             
*                                                                               
BDFS02   LA    R2,IOKEY                                                         
         USING CT7REC,R2                                                        
         XC    CT7KEY,CT7KEY                                                    
         MVI   CT7KTYP,CT7KTYPQ                                                 
         MVC   CT7KCODE,SCRPNAME                                                
*                                                                               
         GOTO1 =V(DATAMGR),PARMS,DMREAD,CTFILE,IOKEY,AIO2                       
         CLI   8(R1),0                                                          
         BE    BDFS04              FOUND NORMALLY                               
*                                                                               
         TM    8(R1),X'10'         RECORD FOUND?                                
         BZ    *+12                NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     BDFS04                                                           
*                                                                               
         TM    8(R1),X'02'         RECORD DELETED?                              
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
BDFS04   L     R2,AIO2             SET UP AN EMPTY RECORD                       
         MVC   CT7KEY,IOKEY                                                     
         XC    CT7STAT,CT7STAT     RESET RECORD STATUS                          
         MVC   CT7LEN,=AL2(CT7KEYL+1)                                           
         MVI   CT7DATA,0                                                        
         B     XITOK                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE EBCDIC INPUT                                    *         
***********************************************************************         
         SPACE 1                                                                
EBCDIC   NTR1                                                                   
         XR    R1,R1                                                            
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
*                                                                               
         LR    R2,R3               R3=A(START OF OPERAND)                       
         CLI   0(R2),C'"'          INPUT DELIMITED BY "..."                     
         BNE   *+12                                                             
         OI    SCPFLAG1,SF1DINK    SET DELIMITED BY "..."                       
         B     EBCD06                                                           
*                                                                               
EBCD02   CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BE    EBCD14                                                           
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    EBCD04                                                           
         BAS   RE,INVDLMTR         MISSING DELIMITER DEALT WITH HERE            
         B     XIT                                                              
*                                                                               
EBCD04   IC    R1,LENNOW           INCREMENT OPERAND LENGTH                     
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            NEXT CHARACTER                               
         BCT   R0,EBCD02           TRY AGAIN                                    
         BAS   RE,INVOPRND         MISSING DELIMITER DEALT WITH HERE            
         B     XIT                                                              
*                                                                               
EBCD06   IC    R1,LENNOW           INCREMENT OPERAND LENGTH                     
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            GO PAST FIRST " IN PAIR                      
*                                                                               
EBCD08   CLI   0(R2),C'"'          LOOK FOR SECOND DINK                         
         BNE   EBCD10                                                           
         IC    R1,LENNOW           INCREMENT LENGTH FOR SECOND DINK             
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            NEXT CHARACTER                               
         CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BE    EBCD14                                                           
         BAS   RE,INVDLMTR         MISSING DELIMITER                            
         B     XIT                                                              
*                                                                               
EBCD10   ZIC   R1,LENNOW           BUMP CURRENT LENGTH                          
         LA    R1,1(R1)            ..                                           
         STC   R1,LENNOW           AND SAVE IT                                  
         CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BNE   EBCD12                                                           
         BAS   RE,INVDLMTR         INVALID OPERAND                              
         B     XIT                                                              
*                                                                               
EBCD12   LA    R2,1(R2)            NEXT CHARACTER                               
         BCT   R0,EBCD08                                                        
         BAS   RE,INVDLMTR         MISSING DELIMITER                            
         B     XIT                                                              
*                                                                               
EBCD14   ST    R2,SAVER3           POINT TO DELIMITER                           
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    EBCD16                                                           
         BAS   RE,INVOPRND         MISSING OPERAND                              
         B     XIT                                                              
*                                                                               
EBCD16   XR    R1,R1                                                            
         IC    R1,LENNOW           LENGTH OF INPUT OPERAND                      
         TM    SCPFLAG1,SF1DINK    INPUT SURROUNDED BY "..." ?                  
         BZ    *+8                 NO                                           
         SHI   R1,2                NEED TO REDUCE LENGTH BY 2                   
*                                                                               
         CVD   R1,DUB1             PACK LENGTH OF OPERAND                       
         OI    DUB1+L'DUB1-1,X'0F' ZONE IT                                      
         UNPK  0(2,R4),DUB1        GET THE LENGTH IN EBCDIC - 2 CHARS           
         LA    R4,2(R4)            NEXT FREE ON ASMLINE                         
*                                                                               
         BCTR  R1,0                                                             
         LR    RF,R3                                                            
         TM    SCPFLAG1,SF1DINK    IN 'DINK' MODE                               
         BZ    *+8                 NO                                           
         LA    RF,1(RF)            BUMP PAST THE FIRST "                        
         MVC   0(0,R4),0(RF)                                                    
         EX    R1,*-6              MOVE IN OPERAND - DINKS                      
*                                                                               
         LA    R4,1(R1,R4)                                                      
         L     R3,SAVER3                                                        
         NI    SCPFLAG1,FF-SF1DINK TURN OFF DINK MODE                           
*                                                                               
         CR    RB,RB               SET CONDITION CODE EQUAL                     
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE LABELS                                          *         
***********************************************************************         
LABEL    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
LBL02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    LBL06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    LBL04                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,LBL02            CONTINUE                                     
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL06    ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    LBL08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         MVC   0(0,R4),0(R3)                                                    
         EX    R1,*-6              MOVE IN OPERAND                              
         LA    R4,1(R1,R4)         NEXT FREE ON ASMLINE                         
         L     R3,SAVER3           A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE GAPPED INPUT                                    *         
***********************************************************************         
GAPPED   NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
GAP02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    GAP06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    GAP03                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP03    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   GAP04               NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,GAP02            CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP06    ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    GAP08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  DUB1,0(0,R3)                                                     
         OI    DUB1+L'DUB1-1,X'0F' ZONE IT                                      
*                                                                               
         CVB   R1,DUB1             ENSURE WITHIN RANGE 1-99                     
         CHI   R1,99                                                            
         BNH   GAP12               INVALID OPERAND                              
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP12    UNPK  0(2,R4),DUB1        SET LENGTH                                   
         LA    R4,2(R4)                                                         
         BCTR  R1,0                                                             
         MVC   0(0,R4),MYSPACES                                                 
         EX    R1,*-6              MOVE IN OPERAND                              
         LA    R4,1(R1,R4)         Next free on ASMLINE                         
         L     R3,SAVER3           A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE FIXED INPUT                                     *         
***********************************************************************         
         SPACE 1                                                                
FIXED    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
FIX02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    FIX06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    FIX03                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX03    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   FIX04               NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,FIX02            CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX06    ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    FIX08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  DUB1,0(0,R3)                                                     
         OI    DUB1+L'DUB1-1,X'0F' ZONE IT                                      
*                                                                               
         CVB   R1,DUB1             ENSURE WITHIN RANGE 1-99                     
         CHI   R1,99                                                            
         BNH   FIX12               INVALID OPERAND                              
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX12    IC    R1,SAVE.DATALEN     FIXED LENGTH INPUT FIELD                     
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R4),DUB1        UNPACK DATA ONTO OUTPUT LINE                 
*                                                                               
         IC    R1,SAVE.DATALEN                                                  
         LA    R4,0(R1,R4)         NEXT FREE ON PRINT LINE                      
         L     R3,SAVER3           A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
***********************************************************************         
* ROUTINE TO VALIDATE HEXADECIMAL INPUT                               *         
***********************************************************************         
HEXED    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         LLC   R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
HEX02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    HEX12               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    HEX04                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX04    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    HEX06               NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BH    HEX08               NO                                           
         B     HEX10                                                            
*                                                                               
HEX06    CLI   0(R2),C'A'          TEST VALID LETTER                            
         BL    HEX08               NO                                           
         CLI   0(R2),C'F'          TEST VALID LETTER                            
         BH    HEX08               NO                                           
         B     HEX10                                                            
*                                                                               
HEX08    BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX10    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,HEX02            CONTINUE                                     
         BAS   RE,INVOPRND         ERROR HANDLER OPERAND TOO LONG               
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX12    ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    HEX14                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX14    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         MVC   0(0,R4),0(R3)                                                    
         EX    R1,*-6              MOVE IN OPERAND                              
         LA    R4,1(R1,R4)         NEXT FREE IN ASMLINE                         
         L     R3,SAVER3           A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE NUMERIC INPUT                                   *         
***********************************************************************         
         SPACE 1                                                                
NUMERIC  NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         LLC   R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
NUMB02   CLI   0(R2),C';'          DELIMITER?                                   
         BE    NUMB08              YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    NUMB04                                                           
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB04   CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   NUMB06              NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB06   IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,NUMB02           CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB08   ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    NUMB10                                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB10   IC    R1,LENNOW           LENGTH OF OPERAND                            
         CVD   R1,DUB1             PACK OPERAND LENGTH                          
         OI    DUB1+L'DUB1-1,X'0F' ZONE IT                                      
         UNPK  0(2,R4),DUB1        SET LENGTH IN COMPILE LINE                   
         LA    R4,2(R4)            BUMP TO NEXT FREE ON LINE                    
*                                                                               
         LA    R0,FF               MAX VALUE OF BYTE                            
         CLI   SAVE.DATALEN,5                                                   
         BNE   *+10                                                             
         XR    R0,R0                                                            
         ICM   R0,3,=XL2'7FFF'     MAX VALUE OF HALFWORD                        
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  DUB1,0(0,R3)                                                     
*                                                                               
         CVB   RF,DUB1                                                          
         CR    R0,RF               ENSURE OPERAND IS WITHIN LIMITS              
         BH    NUMB12                                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB12   MVC   0(0,R4),0(R3)                                                    
         EX    R1,*-6              MOVE IN OPERAND                              
         LA    R4,1(R1,R4)         NEXT FREE IN ASMLINE                         
         L     R3,SAVER3           A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* INVALID OPCODE ERROR PROCESSED HERE                                 *         
***********************************************************************         
         SPACE 1                                                                
INVOPCDE NTR1  ,                                                                
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,WORK1            A(START OF LINE)                             
         AR    RE,R0               A(END OF LINE)                               
         LR    RF,R3               SAVE CURRENT POINT                           
*                                                                               
XOPC02   CLI   0(RF),C';'          DELIMITER FOR NEXT INSTRUCTION               
         BE    XOPC04                                                           
         CLI   0(RF),C' '          SPACE BEFORE OPERAND                         
         BE    XOPC04                                                           
         LA    RF,1(RF)            NEXT POSITION IN LINE                        
         CR    RF,R3               RE = LAST POSITION ON LINE                   
         BH    XOPC02              NOT REACHED END OF LINE YET                  
         LR    RF,RE                                                            
*                                                                               
XOPC04   ST    RF,ARSPR                                                         
         SR    RF,R3               GET L'OPCODE                                 
         MVI   THISERR,3           ERROR #                                      
         MVI   ASMERR,FF           FLAG FOR ASSEMBLY ERROR                      
         GOTO1 ERRHNDL,PARMS,((RF),(R3))                                        
*                                                                               
         LA    RE,WORK1                                                         
         AR    RE,R0               RE = END OF INPUT LINE                       
         L     R3,ARSPR                                                         
         LR    RF,R3                                                            
XOPC06   CLI   0(R3),C';'          DELIMITER FOUND?                             
         BE    XOPC08              YES                                          
         LA    R3,1(R3)            BUMP ALONG 1                                 
         CR    RE,R3               END OF LINE?                                 
         BNL   XOPC06              NO                                           
*                                                                               
         SR    R3,RF                                                            
         MVI   THISERR,5           MISSING DELIMITER                            
         MVI   ASMERR,FF           SET ASSEMBLY ERROR                           
         GOTO1 ERRHNDL,PARMS,((R3),(RF))                                        
         B     XITL                                                             
*                                                                               
XOPC08   CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)        KEEP CURRENT POINTERS                        
         SPACE 2                                                                
***********************************************************************         
* MISSING DELIMITER DEALT WITH HERE                                   *         
***********************************************************************         
         SPACE 1                                                                
INVDLMTR NTR1                                                                   
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,WORK1            RE=A(START OF LINE)                          
         LR    RF,R3               R3=A(CURRENT POINTER)                        
*                                                                               
XDLM02   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XDLM04                                                           
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XDLM02                                                           
*                                                                               
XDLM04   AR    RE,R0               RE=A(END OF LINE)                            
*                                                                               
XDLM06   CLI   0(R3),C';'          NEXT DELIMITER?                              
         BE    XDLM08                                                           
         CLI   0(R3),C' '          NEXT OPERAND                                 
         BE    XDLM08                                                           
         LA    R3,1(R3)                                                         
         CR    R3,RE               END OF LINE REACHED?                         
         BNH   XDLM06              NOT YET                                      
*                                                                               
XDLM08   ST    R3,ARSPR            COPY R3                                      
         SR    R3,RF               GET LENGTH OF ERROR STRING                   
         MVI   THISERR,5           ERROR MISSING DELIMITER                      
         MVI   ASMERR,FF                                                        
         GOTO1 ERRHNDL,PARMS,((R3),(RF))                                        
*                                                                               
         L     R3,ARSPR            RESTORE CURRENT LINE POINTER                 
         LA    R1,WORK1                                                         
         AR    R1,R0                                                            
         CR    R3,R1                                                            
         BNL   XITL                THIS WAS LAST INSTRUCTION                    
*                                                                               
         LA    RE,WORK1            RE = START OF LINE                           
         LR    RF,R3                                                            
*                                                                               
XDLM10   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XDLM12                                                           
*                                                                               
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XDLM10                                                           
*                                                                               
XDLM12   AR    RE,R0               RE = A(END OF LINE)                          
         CLI   0(R3),C';'          NEXT INSTRUCTION                             
         BE    XDLM14                                                           
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XDLM12              NO                                           
*                                                                               
         SR    R3,RF                                                            
         GOTO1 ERRHNDL,PARMS,((R3),(RF))                                        
         B     XITL                END OF LINE AND NO DELIMITER                 
*                                                                               
XDLM14   CR    RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* INVALID OPERAND DEALT WITH HERE                                     *         
***********************************************************************         
         SPACE 1                                                                
INVOPRND NTR1  ,                                                                
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,WORK1            A(START OF LINE)                             
         LR    RF,R3                                                            
*                                                                               
XOPD02   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XOPD04                                                           
*                                                                               
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XOPD02                                                           
*                                                                               
XOPD04   AR    RE,R0               RE = A(END OF LINE)                          
*                                                                               
XOPD06   CLI   0(R3),C';'          DELIMITER?                                   
         BE    XOPD08                                                           
         CLI   0(R3),C' '          EMBEDDED SPACE OR MISSING DELIMITER          
         BE    XOPD08                                                           
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XOPD06              NO                                           
*                                                                               
XOPD08   ST    R3,ARSPR                                                         
         SR    R3,RF               GET LENGTH OF INVALID CODE                   
         MVI   THISERR,6           ERROR INVALID OPERAND                        
         MVI   ASMERR,FF                                                        
         GOTO1 ERRHNDL,PARMS,((R3),(RF))                                        
*                                                                               
         LA    RE,WORK1                                                         
         L     R3,ARSPR                                                         
         LR    RF,R3                                                            
*                                                                               
XOPD10   BCTR  RF,0                                                             
         CLI   0(RF),C';'             PREVIOUS DELIMITER?                       
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XOPD12                                                           
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XOPD10                                                           
*                                                                               
XOPD12   AR    RE,R0               RE = A(END OF LINE)                          
*                                                                               
XOPD14   CLI   0(R3),C';'          DELIMITER?                                   
         BE    XOPD16              YES                                          
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XOPD14              NO                                           
*                                                                               
         MVI   THISERR,5           ERROR MISSING DELIMITER                      
         MVI   ASMERR,FF                                                        
         SR    R3,RF                                                            
         GOTO1 ERRHNDL,PARMS,((R3),(RF))                                        
         B     XITL                                                             
*                                                                               
XOPD16   CR    RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ERROR HANDLING ROUTINE                                              *         
***********************************************************************         
         SPACE 1                                                                
ERRHNDL  NTR1                                                                   
         LH    RF,TOTERRS          INCREMENT TOTAL ERROR COUNT                  
         LA    RF,1(RF)                                                         
         STH   RF,TOTERRS                                                       
*                                                                               
         MVC   PSAVE,PLINE         SAVE ORIGINAL LINE                           
         LA    R2,PLINE                                                         
*                                                                               
         LA    RF,ERRTAB           TABLE OF ERROR MESSAGES                      
HERR04   CLI   0(RF),EOT           END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN ERROR                                
*                                                                               
         CLC   THISERR,0(RF)       MATCH ON ERROR CODE?                         
         BE    *+12                YES                                          
         LA    RF,L'ERRTAB(RF)                                                  
         B     HERR04                                                           
*                                                                               
         MVC   40(31,R2),1(RF)     MOVE IN ERROR MESSAGE                        
         MVC   30(9,R2),=C'**ERROR**' IDENTIFIER                                
*                                                                               
         CLI   ASMERR,0            TEXT TO OUTPUT?                              
         BE    HERR06              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R1)            L' FAULTY CODE SEGMENT                       
         L     RE,0(R1)            A(START OF ERROR)                            
*                                                                               
         MVC   0(30,R2),MYSPACES                                                
*                                                                               
         BCTR  RF,0                L'BAD DATA ON WORK1                          
         CHI   RF,28               MAX L' SUPPORTED FOR PRINTING                
         BL    *+8                 IS 29 CHARACTERS                             
         LA    RF,28                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE IN BAD DATA TO ERROR LINE               
*                                                                               
         CLI   ASMERR,FF           ASSEMBLY ERROR?                              
         BNE   HERR06                                                           
         MVC   0(4,R4),=C'@@@@'    INDICATE ERROR ON O/P LINE                   
         LA    R4,4(R4)                                                         
*                                                                               
HERR06   BAS   RE,PRNTLINE                                                      
         MVC   PLINE,PSAVE                                                      
         XC    ASMERR,ASMERR       CLEAR ASMERR                                 
         OI    SCPFLAG1,SF1ERRS    ERROR FLAG ON                                
         XIT1  REGS=(R4)                                                        
*                                                                               
*                           1234567890123456789012345678901                     
ERRTAB   DS    0CL32                                                            
         DC    AL1(01),CL31'?????????????AVAILABLE?????????'                    
         DC    AL1(02),CL31'Compile error - No record write'                    
         DC    AL1(03),CL31'Command not found              '                    
         DC    AL1(04),CL31'?????????????AVAILABLE?????????'                    
         DC    AL1(05),CL31'Unable to find delimiting ;    '                    
         DC    AL1(06),CL31'Invalid operand value          '                    
         DC    AL1(07),CL31'Script has not been named      '                    
         DC    AL1(08),CL31'Equate name not found          '                    
         DC    AL1(09),CL31'Equate name not valid          '                    
         DC    AL1(10),CL31'Script requires END statement  '                    
         DC    AL1(11),CL31'Script name is invalid         '                    
         DC    AL1(12),CL31'Label is duplicated            '                    
         DC    AL1(13),CL31'Too many labels - contact DDS  '                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE LABEL EQUATES                                       *         
***********************************************************************         
         SPACE 1                                                                
SETLBL   NTR1                                                                   
         TM    SCPFLAG1,SF1LAST    WANT TO RESOLVE ALL EQUATES?                 
         BO    SLBL20              YES                                          
*                                                                               
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         LLC   R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AHI   R0,1                TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
SLBL02   CLI   0(R2),C';'          DELIMITER?                                   
         BE    SLBL04              YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BNH   XIT                 LET ERROR HANDLING TAKE OVER                 
*                                                                               
         IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,SLBL02           CONTINUE                                     
         B     XIT                 LET ERROR HANDLING TAKE OVER                 
*                                                                               
SLBL04   ST    R2,SAVER3           SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BNH   XIT                 LET ERROR HANDLER COPE                       
*                                                                               
         TM    SAVE.MXOPERS,MXLBL  IS THIS A LABEL                              
         BO    SLBL10              YES                                          
*                                                                               
         LA    R4,BCHTAB           BRANCH TABLE                                 
         XR    R0,R0                                                            
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    SLBL18              NO                                           
         CHI   R0,LBLALLOW                                                      
         BNH   SLBL06                                                           
         MVI   THISERR,13                                                       
         BAS   RE,ERRHNDL                                                       
         B     XIT                                                              
*                                                                               
SLBL06   LA    RF,2(R4)                                                         
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
*                                                                               
SLBL08   EX    R1,SLBMCH           SEE IF OPERAND IN TABLE ALREADY              
         BE    XIT                 YES - GOOD                                   
         LA    RF,LBLLEN(RF)                                                    
         BCT   R0,SLBL08                                                        
         B     SLBL18                                                           
*                                                                               
SLBL10   LA    R4,LBLTAB           BRANCH TABLE                                 
         XR    R0,R0                                                            
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    SLBL18              NO                                           
         CHI   R0,LBLALLOW                                                      
         BNH   SLBL12                                                           
         MVI   THISERR,13                                                       
         BAS   RE,ERRHNDL                                                       
         B     XIT                                                              
*                                                                               
SLBL12   LA    RF,2(R4)                                                         
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
*                                                                               
SLBL14   EX    R1,SLBMCH           SEE IF OPERAND IN TABLE ALREADY              
         BE    SLBL16              YES - DUPLICATE LABEL                        
         LA    RF,LBLLEN(RF)                                                    
         BCT   R0,SLBL08                                                        
         B     SLBL18                                                           
*                                                                               
SLBL16   MVI   THISERR,12          DUPLICATE LABEL                              
         MVI   ASMERR,1                                                         
         GOTO1 ERRHNDL,PARMS,(2,(R3))                                           
         B     XIT                                                              
*                                                                               
SLBMCH   CLC   2(0,RF),0(R3)                                                    
*                                                                               
SLBL18   XR    RF,RF                                                            
         ICM   RF,3,0(R4)          NUMBER OF ENTRIES IN TABLE                   
         LR    RE,RF               COPY IT                                      
         LA    RE,1(RE)            INCREMENT AND SAVE                           
         STCM  RE,3,0(R4)                                                       
         MHI   RF,LBLLEN           LENGTH OF 1 ENTRY                            
         LA    RF,2(RF,R4)         INDEX INTO TABLE                             
*                                                                               
         MVC   0(2,RF),LINENUM     SAVE LINE NUMBER                             
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         MVC   2(0,RF),0(R3)                                                    
         EX    R1,*-6              MOVE IN OPERAND                              
         B     XIT                                                              
*                                                                               
SLBL20   XR    R0,R0                                                            
         LA    R4,BCHTAB           ONLY NEED RESOLVE BRANCHES                   
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    XITOK               NO                                           
*                                                                               
         LA    R2,2(R4)            FIRST BRANCH INSTRUCTION                     
*                                                                               
SLBL22   LA    R3,LBLTAB                                                        
         ICM   RF,3,0(R3)          COUNT OF BRANCHES                            
         BZ    SLBL26                                                           
         LA    R3,2(R3)                                                         
*                                                                               
SLBL24   CLC   2(2,R2),2(R3)       SEE IF OPERAND IN BOTH TABLES                
         BE    SLBL28              YES - GOOD                                   
         LA    R3,LBLLEN(R3)                                                    
         BCT   RF,SLBL24                                                        
*                                                                               
SLBL26   MVC   PLINE+30(L'ERINVBCH),ERINVBCH                                    
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         CVD   RF,DUB1                                                          
         OI    DUB1+L'DUB1-1,X'0F'                                              
         UNPK  PLINE+61(5),DUB1                                                 
         MVC   PLINE+50(2),2(R2)                                                
         BAS   RE,PRNTLINE                                                      
         LH    RF,TOTERRS          INCREMENT ERROR COUNT                        
         LA    RF,1(RF)                                                         
         STH   RF,TOTERRS                                                       
         OI    SCPFLAG1,SF1ERRS    SET ERROR ON BUILD                           
*                                                                               
SLBL28   LA    R2,LBLLEN(R2)                                                    
         BCT   R0,SLBL22                                                        
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* BUILD AND ADD ELEMENTS                                              *         
***********************************************************************         
         SPACE  1                                                               
         PUSH  USING                                                            
         USING CTSCRD,ELEM                                                      
BLDELS   NTR1                                                                   
         LA    RF,L'ASMLINE        GET LENGTH OF DATA ON ASMLINE                
         LA    R1,ASMLINE+L'ASMLINE-1                                           
         CLI   0(R1),0                                                          
         BH    BLDL02              FOUND LAST CHARACTER                         
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         TM    SCPFLAG1,SF1LAST    TEST LAST TIME                               
         BO    BLDL12              YES                                          
         B     XITOK               NO DATA FOUND                                
*                                                                               
BLDL02   L     RE,DISPTOT          INCREMENT CUMULATIVE DISPLACEMENT            
         AR    RE,RF                                                            
         ST    RE,DISPTOT                                                       
*                                                                               
         TM    SCPFLAG1,SF1FRST    FIRST TIME IN?                               
         BO    BLDL04              NO                                           
*                                                                               
         XC    ELEM,ELEM           FIRST TIME IN - CLEAR ELEMENT                
         MVI   CTSCREL,CTSCRELQ    ELEMENT CODE                                 
         XC    SEQUENCE,SEQUENCE   CLEAR SEQUENCE NUMBER                        
         MVC   CTSCRSEQ,SEQUENCE                                                
         BCTR  RF,0                OK TO MOVE IN ALL THIS LINE                  
         MVC   CTSCRDTA(0),ASMLINE MOVE IN LINE                                 
         EX    RF,*-6                                                           
         LA    RF,4(RF)            ADD FIXED TO LENGTH                          
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         OI    SCPFLAG1,SF1FRST    SET FLAG ON SO DON`T DO AGAIN                
         B     XITOK                                                            
*                                                                               
BLDL04   XR    R0,R0                                                            
         IC    R0,CTSCRLEN         LENGTH OF ELEMENT AT PRESENT                 
         XC    ELNOWLQ,ELNOWLQ     CLEAR TEMP LENGTH HOLDER                     
         LA    R2,ELEM                                                          
         AR    R2,R0               FIRST FREE DATA SLOT                         
         LA    R1,FF               AS MUCH AS CAN BE FITTED IN                  
         SR    R1,R0                                                            
         STC   R1,ELNOWLQ          HOW MUCH MORE CAN BE ADDED                   
         CR    R1,RF               CAN ALL THIS STUFF MOVE IN OK?               
         BL    BLDL06              NO                                           
*                                                                               
         BCTR  RF,0                OK TO MOVE IN ALL THIS LINE                  
         MVC   0(0,R2),ASMLINE     MOVE IN LINE                                 
         EX    RF,*-6                                                           
         LR    RE,R0               R0 HOLDS CURRENT LENGTH REMEMBER             
         LA    RF,1(RE,RF)         ADD LENGTH TO TOTAL                          
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         CLI   CTSCRLEN,FF         EXACTLY FILLED ELEMENT?                      
         BNE   XITOK               NO                                           
         XC    ELNOWLQ,ELNOWLQ     RESET AMOUNT TO MOVE INTO NEXT EL.           
         B     BLDL08                                                           
*                                                                               
BLDL06   SR    RF,R1               RF HOLDS WHAT MUST BE MOVED                  
         STC   RF,ELNOWLQ          STORE L'LEFT AFTER THIS MOVE                 
         LR    R4,R1               SAVE AMOUNT MOVED THIS TIME                  
         BCTR  R1,0                                                             
         MVC   0(0,R2),ASMLINE     FILL THIS ELEMENT                            
         EX    R1,*-6                                                           
*                                                                               
BLDL08   MVI   CTSCRLEN,FF         SET ELEMENT LENGTH TO MAXIMUM                
         XR    R1,R1                                                            
         IC    R1,SEQUENCE         BUMP SEQUENCE NO.                            
         LA    R1,1(R1)                                                         
         STC   R1,SEQUENCE                                                      
         MVC   CTSCRSEQ,SEQUENCE   SET SEQUENCE NUMBER                          
*                                                                               
         L     R2,AIO2                                                          
         USING CT7REC,R2                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,MAXLEN                                                      
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS RECORD & GET NEXT                 
*                                                                               
         GOTO1 =V(HELLO),PARMS,(C'P',CTFBIG),AIO2,ELEM,ADDEND                   
         CLI   12(R1),0                                                         
         BE    BLDL10                                                           
         DC    H'0'                                                             
*                                                                               
BLDL10   XC    ELEM,ELEM           MOVE IN REMAINDER AND SET LENGTHS            
         MVI   CTSCREL,CTSCRELQ                                                 
         MVI   CTSCRLEN,3                                                       
         XR    RF,RF                                                            
         ICM   RF,1,ELNOWLQ        LENGTH LEFT TO MOVE IN                       
         BZ    XITOK                                                            
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,ASMLINE(R4)      START OF NEXT PIECE OF DATA                  
         MVC   CTSCRDTA(0),0(RE)                                                
         EX    RF,*-6                                                           
         LA    RF,4(RF)            2 FOR FIXED, 1 FOR MVC                       
         STC   RF,CTSCRLEN         SAVE LENGTH                                  
         B     XITOK                                                            
*                                                                               
BLDL12   XR    R1,R1                                                            
         ICM   R1,1,SEQUENCE       SEQUENCE NUMBER IS 0 OR 1...N                
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,CTSCRSEQ                                                      
*                                                                               
         IC    R1,CTSCRLEN                                                      
         LA    R1,ELEM(R1)                                                      
         SHI   R1,2                                                             
         CLC   =C'**',0(R1)        DOES SCRIPT END WITH AN END?                 
         BE    BLDL16              YES                                          
*                                                                               
         CLI   CTSCRLEN,(FF-2)     WILL IT FIT IN THIS ELEMENT?                 
         BH    BLDL14              NO                                           
         LA    R1,2(R1)                                                         
         MVC   0(2,R1),=C'**'                                                   
         XR    R1,R1                                                            
         IC    R1,CTSCRLEN                                                      
         LA    R1,2(R1)                                                         
         STC   R1,CTSCRLEN                                                      
         B     BLDL16                                                           
*                                                                               
BLDL14   MVI   THISERR,10                                                       
         BAS   RE,ERRHNDL                                                       
         B     XITOK               NO WRITE IF ERRORS                           
*                                                                               
         USING CT7REC,R2                                                        
BLDL16   L     R2,AIO2                                                          
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,MAXLEN                                                      
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS SEQUENCE AND GET NEXT             
*                                                                               
         GOTO1 =V(HELLO),PARMS,(C'P',CTFBIG),AIO2,ELEM,ADDEND                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    PANBOOK,PANBOOK     ANY PAN DETAILS                              
         BZ    XITOK                                                            
*                                                                               
         GOTO1 =V(HELLO),PARMS,(C'P',CTFBIG),AIO2,PANELEM,ADDEND                
         CLI   12(R1),5            RECORD IS TOO BIG?                           
         BE    XITOK                                                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XITOK                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A CONTROL FILE RECORD AND INITIALISE NEXT RECORD     *         
***********************************************************************         
         PUSH  USING                                                            
RECPUT   NTR1  ,                                                                
         LH    R1,RECSEQ           INCREMENT SEQUENCE # COUNTER                 
         LA    R1,1(R1)                                                         
         STH   R1,RECSEQ                                                        
*                                                                               
         L     R2,AIO2                                                          
         USING CT7REC,R2                                                        
         OC    CT7SEQNO,CT7SEQNO   FIRST RECORD (SEQUENCE #0)                   
         BZ    RPUT02              YES - SEQUENCE IS 0 OR 1..N                  
*                                                                               
         TM    SCPFLAG1,SF1ADD                                                  
         BNZ   RECPUT1                                                          
*                                                                               
         GOTO1 =V(DATAMGR),PARMS,DMWRITE,CTFILE,IOKEY,AIO2                      
         B     RECPUT2                                                          
*                                                                               
RECPUT1  GOTO1 =V(DATAMGR),PARMS,DMADD,CTFILE,IOKEY,AIO2                        
*                                                                               
RECPUT2  CLI   8(R1),0                                                          
         BE    RPUT08                                                           
         DC    H'0'                                                             
*                                                                               
RPUT02   L     R0,AIO1             SAVE COPY OF SEQUENCE #0 RECORD              
         L     RE,AIO2                                                          
         LH    R1,MAXLEN                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    SCPFLAG1,SF1ADD     ADDING SEQUENCE #0 RECORD?                   
         BO    RPUT04              YES                                          
*                                                                               
         L     R2,AIO2             SET SEQUENCE #0 DELETE FLAG                  
         OI    CT7STAT,X'80'                                                    
         GOTO1 =V(DATAMGR),PARMS,DMWRITE,CTFILE,IOKEY,AIO2                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
K7       USING CT7REC,IOKEY                                                     
RPUT04   NI    SCPFLAG1,FF-SF1ADD  RESET ADD FLAG                               
*                                                                               
         L     R2,AIO1             COPY OF RECORD 0                             
         MVC   K7.CT7KEY,CT7KEY    MOVE KEY COPY INTO IOKEY                     
         MVC   K7.CT7SEQNO,RECSEQ  SET SEQUENCE #1                              
         GOTO1 =V(DATAMGR),PARMS,(X'80',DMREAD),CTFILE,IOKEY,AIO2               
         CLI   8(R1),0                                                          
         BE    RPUT06              FOUND SEQUENCE #1 RECORD                     
*                                                                               
         TM    8(R1),X'10'         RECORD FOUND?                                
         BZ    RPUT05              NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     RPUT06                                                           
*                                                                               
RPUT05   TM    8(R1),X'02'         RECORD DELETED?                              
         BO    RPUT06              YES - THAT'S OK                              
         DC    H'0'                                                             
*                                                                               
RPUT06   L     R0,AIO1             RESTORE COPY OF SEQUENCE #0 RECORD           
         L     RE,AIO2                                                          
         LH    R1,MAXLEN                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIO2                                                          
         MVC   CT7SEQNO,RECSEQ     SET SEQUENCE # TO 1                          
*                                                                               
         TM    SCPFLAG1,SF1ADD                                                  
         BNZ   RECPUT3                                                          
*                                                                               
         GOTO1 =V(DATAMGR),PARMS,DMWRITE,CTFILE,IOKEY,AIO2                      
         B     RECPUT4                                                          
*                                                                               
RECPUT3  GOTO1 =V(DATAMGR),PARMS,DMADD,CTFILE,IOKEY,AIO2                        
*                                                                               
RECPUT4  CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    R1,RECSEQ           NEXT ONE TO BE DEALT WITH IS #2              
         LA    R1,1(R1)                                                         
         STH   R1,RECSEQ                                                        
*                                                                               
RPUT08   NI    SCPFLAG1,FF-SF1ADD  RESET ADD FLAG                               
*                                                                               
         L     R2,AIO2                                                          
         MVC   K7.CT7KEY,CT7KEY    MOVE KEY INTO IOKEY                          
         MVC   K7.CT7SEQNO,RECSEQ  SET NEXT SEQUENCE #                          
         GOTO1 =V(DATAMGR),PARMS,(X'80',DMREAD),CTFILE,IOKEY,AIO2               
         CLI   8(R1),0                                                          
         BE    RPUT10              FOUND RECORD                                 
*                                                                               
         TM    8(R1),X'10'         RECORD FOUND?                                
         BZ    *+12                NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     RPUT10                                                           
*                                                                               
         TM    8(R1),X'02'         RECORD DELETED?                              
         BO    RPUT10              YES - THAT'S OK                              
         DC    H'0'                                                             
*                                                                               
RPUT10   L     R2,AIO2             SET UP AN EMPTY RECORD                       
         MVC   CT7KEY,K7.CT7KEY                                                 
         XC    CT7STAT,CT7STAT     RESET RECORD STATUS                          
         MVC   CT7LEN,=AL2(CT7KEYL+1)                                           
         MVI   CT7DATA,0                                                        
*                                                                               
         XC    SEQUENCE,SEQUENCE   RESET SEQUENCE NUMBER                        
         B     XITOK               RETURN                                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
LBLLEN   EQU   4                   LENGTH OF A TABLE ENTRY                      
LBLALLOW EQU   64                  MAX NUMBER OF LABELS                         
SUBCHAR  EQU   C'%'                SCRAMBLE SUBSTITUTION CHARACTER              
FF       EQU   255                                                              
*                                                                               
MCWRITE  DC    C'Y'                                                             
                                                                                
PANELEM  DC    X'E4'               ELCODE E0                                    
         DC    AL1(34)             LEN=34                                       
         DC    C'BOOK='                                                         
PANBOOK  DC    XL10'00'                                                         
         DC    C' LVL='                                                         
PANLEVL  DC    CL3'   '                                                         
         DC    C' '                                                             
PANDATE  DC    CL8'        '                                                    
*                                                                               
CTFBIG   DC    C'CTFBIG '                                                       
CTFILE   DC    C'CTFILE '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
ADDEND   DC    C'ADD=END'                                                       
*AXLEN   DC    H'3800'             MAX LENGTH OF CONTROL FILE RECORD            
MAXLEN   DC    H'2000'             MAX LENGTH OF CONTROL FILE RECORD            
EOT      EQU   255                                                              
*                                                                               
DMFLIST  DC    C'UCTFILE X'                                                     
DMCLSE   DC    C'DMCLSE'                                                        
DMOPEN   DC    C'OPEN'                                                          
DMSYS    DC    C'CONTROL'                                                       
*                                                                               
TRTAB    DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-DF                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
*                                                                               
*                   12345678901234567890123456789012345678901234567890          
ERCNTMSG DC    CL30'***      Errors on compile ***'                             
RECUPMSG DC    CL34'***Record updated without error***'                         
ERTOOMNY DC    CL39'**ERROR** Too many errors in this line'                     
ERINVBCH DC    CL45'**ERROR** Branch to XX on line XXXXX unknown'               
*                                                                               
MYSPACES DS    CL80' '                                                          
*                                                                               
DATATAB  DS    0X              *** TABLE OF SCRIPT PARAMETERS***                
         DC    C'E',AL4(EBCDIC)    E - CHARACTER INPUT                          
DATATABL EQU   *-DATATAB                                                        
         DC    C'N',AL4(NUMERIC)   N - NUMERIC (0-9)                            
         DC    C'F',AL4(FIXED)     F - NUMERIC FIXED LENGTH OF 2                
         DC    C'P',AL4(GAPPED)    P - PAIN IN THE ARSE - SPACES IN             
         DC    C'X',AL4(HEXED)     X - HEX (NOT VALIDATED)                      
         DC    C'L',AL4(LABEL)     L - 2 CHARACTER LABEL VALIDATION             
         DC    AL1(EOT)                                                         
*                                                                               
* DDSCRAMTAB                                                                    
       ++INCLUDE DDSCRAMTAB                                                     
         SPACE 2                                                                
*                                                                               
SSB      CSECT                                                                  
         DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    X'0000FF02'    NO RECOVERY                                       
         ORG                                                                    
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
PARMS    DS    6F                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL80                                                             
CARD     DS    CL80                                                             
IOKEY    DS    XL32                                                             
*                                                                               
SAVERD   DS    A                                                                
DISPTOT  DS    F                                                                
ALINLN   DS    F                                                                
ARSPR    DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
*                                                                               
SCPFLAG1 DS    XL1                                                              
SF1NAME  EQU   X'80'               SCRIPT HAS A NAME                            
SF1ERRS  EQU   X'40'               ERROR DURING COMPILE                         
SF1ADD   EQU   X'20'               NEED TO ADD SCRIPT RECORD                    
SF1FRST  EQU   X'10'               FIRST TIME FOR ACTION                        
SF1LAST  EQU   X'08'               LAST TIME FOR ACTION                         
SF1CMT   EQU   X'04'               INSIDE /*...*/ PAIR                          
SF1LCMT  EQU   X'02'               LAST PROCESSED WAS COMMENT CLOSE             
SF1DINK  EQU   X'01'               INSIDE "..." PAIR                            
*                                                                               
SAVER3   DS    A                                                                
*                                                                               
SCRPNAME DS    XL(L'CTJKID)        SAVED SCRIPT NAME FOR SCRAMBLE               
*                                                                               
LINENUM  DS    H                   CURRENT LINE NUMBER                          
RECSEQ   DS    H                   RECORD SEQUENCE #                            
TOTERRS  DS    H                   TOTAL ERRORS ON COMPILE                      
SEQUENCE DS    X                   ELEMENT SEQUENCE #                           
ELNOWLQ  DS    X                   LENGTH OF THIS ELEMENT                       
LASM     DS    X                   LENGTH OF AN ASSEMBLED LINE                  
LDATA    DS    X                   LENGTH OF INPUT LINE                         
LENNOW   DS    X                   CURRENT LENGTH OF I/P PARAMETER              
THISERR  DS    X                   NUMBER OF THIS ERROR                         
ASMERR   DS    X                   IF ERROR ON ASMLINE = FF                     
*                                                                               
REPCMP   DS    XL10                SCRIPT COMPARATOR                            
REPLEN   DS    X                   LENGTH TO COMPARE                            
*                                                                               
ASMLINE  DS    XL132               ASSEMBLED SCRIPT OUTPUT LINE                 
ERROROUT DS    9XL80               TABLE OF OUTPUT ERRORS                       
*                                                                               
SVASM    DS    XL(ASMTABLQ)        SAVED ASMTAB ENTRY                           
LBLTAB   DS    XL256               LABEL TABLE  (LBLALLOW*LBLLEN)               
BCHTAB   DS    XL256               BRANCH TABLE (LBLALLOW*LBLLEN)               
ELEM     DS    XL256               ELEMENT AREA                                 
EQUTABLE DS    1024C               TABLE OF EQUS BUILT FOR THIS SCRIPT          
*                                                                               
PSAVE    DS    CL165                                                            
*                                                                               
PLINE    DS    0XL165                                                           
DATA1    DS    XL72                ORIGINAL I/P BEFORE SQUASHING                
DATANUM  DS    XL5                 LINE NUMBER                                  
         DS    XL2                                                              
ASMDISP  DS    XL5                 CUMULATIVE DISPLACEMENT                      
         DS    XL2                                                              
ASM1     DS    XL79                DUP OF ASMLINE FOR PRINT ROUTINE             
*                                                                               
IOFORPAN DS    1024C                                                            
IOAREA1  DS    4096C                                                            
IOAREA2  DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
ASMTABD  DSECT                                                                  
MNEMONIC DS    CL6                 MNEMONIC CORRESPONDING TO OP-CODE            
MTCHLEN  DS    AL1                 LENGTH-1 FOR EXECUTED COMPARE                
OPCODE   DS    CL2                 2 CHARACTER OP-CODE                          
MXOPERS  DS    AL1                 MAX # OF OPERANDS FOR THIS OP-CODE           
MXLBL    EQU   X'80'               INSTRUCTION IS A LABEL                       
MXBRNCH  EQU   X'40'               INSTRUCTION IS A BRANCH                      
DATATYPE DS    CL1                 WHICH CLASS OF DATA TO VALIDATE              
DATALEN  DS    AL1                 MAX LENGTH OF DATA TO FOLLOW                 
ASMTABLQ EQU   *-ASMTABD                                                        
*                                                                               
ASMERRS  DS    0X                  ERROR LINES COME OUT HERE                    
*                                                                               
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
*                                                                               
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048DDSCRAMPAN10/20/15'                                      
         END                                                                    
