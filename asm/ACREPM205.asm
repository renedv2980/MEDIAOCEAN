*          DATA SET ACREPM205  AT LEVEL 048 AS OF 01/28/13                      
*PHASE ACM205A                                                                  
*INCLUDE ACSALHST                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SUBROUTINES'                                                    
ACM205   CSECT                                                                  
         DC    AL4(ENTRYTAB-ACM205)                                             
         PRINT NOGEN                                                            
CNL      NMOD1 0,**CNL**,R9                                                     
         USING MAND,RA                                                          
         L     RC,BASERC                                                        
         USING ACWORKD,RC                                                       
***********************************************************************         
* BUILD SUPERLEDGER CONTROL TABLE                                     *         
***********************************************************************         
         MVI   SRCHLEN,13          MINIMUM LENGTH FOR SEARCH COMPARE            
         SR    R3,R3               COUNT NUMBER OF ENTRIES                      
         MVC   NXTSLP,ASLP         SET ADDRESS OF NEXT AVAILABLE                
         L     R2,ASLPWRK                                                       
         LA    R5,SLTMX            MAXIMUM NUMBER OF ENTRIES                    
         USING SLPD,R2                                                          
         LA    R6,DKEY             BUILD A KEY FOR THE SUPERLEDGER              
         USING ACTRECD,R6                                                       
         MVC   DKEY,SPACES                                                      
         MVC   ACTKEY(1),RCCOMPFL                                               
         MVC   ACTKEY+1(2),CONLEDG                                              
         GOTO1 ADMGR,READ                                                       
         BE    *+6                 FOUND LEDGER RECORD                          
         DC    H'0'                                                             
*                                                                               
CNL2     GOTO1 ADMGR,RSEQ                                                       
         CLC   DKEY(3),DIR         IS THIS OUR SUPERLEDGER                      
         BNE   CNL20               END OF LEDGER                                
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         MVC   WORK(14),ACTKEY+1   SAVE (TO ACCOUNT) CODE                       
         SR    R0,R0                                                            
         LA    R6,ACTRFST                                                       
CNL3     CLI   0(R6),0                                                          
         BE    CNL2                                                             
         CLI   0(R6),X'20'                                                      
         BE    *+14                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNL3                                                             
*                                                                               
         USING NAMELD,R6                                                        
         MVC   WORK+14(36),SPACES  SAVE THE(TO ACCOUNT) NAME                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
*MN      EX    R1,*+4                                                           
         MVC   WORK+14(0),NAMEREC                                               
         EX    R1,*-6                                                           
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,14                                                            
         BAS   RE,CNL16            SEE IF IT VARIES BY YEAR                     
         L     R6,AIO1                                                          
         USING ACTRECD,R6                                                       
         LA    R6,ACTRFST                                                       
*                                                                               
CNL4     CLI   0(R6),0                                                          
         BE    CNL2                                                             
         CLI   0(R6),X'15'                                                      
         BNE   CNL11                                                            
*                                                                               
         USING GLPELD,R6                                                        
         XC    SLPREC,SLPREC                                                    
         MVC   SLPTACC,WORK        TO(RECEIVING) ACCOUNT                        
         MVC   SLPTNME,WORK+14     TO ACCOUNT NAME                              
         CLC   GLPACC1,SPACES                                                   
         BE    CNL4                NO GIVING ACCOUNT SKIP IT                    
         MVC   SLPFA1C,GLPACC1     FROM(GIVING) ACCOUNT                         
         LA    R1,SLPFA1C+13                                                    
         BAS   RE,CNL12            GET LENGTH OF FROM ACCOUNT                   
         STC   R0,SLPFA1L          SAVE LENGTH OF FROM ACCOUNT                  
         CLC   SLPFA1L,SRCHLEN                                                  
         BNL   *+8                                                              
         STC   R0,SRCHLEN          SEARCH LENGTH HAS LOWEST VALUE               
         LA    R1,SLPFA1C          SEE IF FROM ACCOUNT VARIES BY YEAR           
         BAS   RE,CNL16                                                         
         MVC   SLPFAST,BYTE        TYPE(REGULAR OR WILD)                        
*                                                                               
*              SPECIAL LENGTH CONSIDERATIONS FOR WILD DATA                      
*                                                                               
         TM    SLPFAST,SLPWLD      IS THERE WILD DATA?                          
         BNO   CNL5                                                             
         SR    RF,RF                                                            
         LA    R1,SLPFA1C                                                       
         CLI   0(R1),0             IS THIS THE WILD DATA                        
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         AH    RF,=H'1'                                                         
         B     *-16                                                             
         CLM   RF,1,SRCHLEN                                                     
         BNL   *+8                                                              
         STC   RF,SRCHLEN          SEARCH LENGTH HAS LOWEST VALUE               
*                                                                               
CNL5     MVC   SLPFLT,GLPFLTS      FILTERS                                      
         OC    SLPFLT,SPACES                                                    
         MVC   SLPBKT,GLPBTYP      BUCKET TYPE                                  
         MVI   SLPSIGN,C'+'        AND SIGN                                     
         CLI   GLPACTN,C'-'                                                     
         BNE   *+8                                                              
         MVI   SLPSIGN,C'-'                                                     
*                                                                               
         CLI   GLPLN,40            JUST A FROM AND TO ACCOUNT                   
         BL    CNL8                                                             
         CLC   GLPACC2,SPACES      IS THERE A HIGH RANGE ACCOUNT                
         BE    CNL7                NO RANGE                                     
         CLI   SLPFAST,SLPWLD      IS THE FROM ACCOUNT WILD                     
         BNE   *+6                                                              
         DC    H'0'                IF SO, THERE SHOULD NOT BE A RANGE           
         MVI   SLPFAST,SLPRNG                                                   
         MVC   SLPFA2C,GLPACC2                                                  
         LA    R1,SLPFA2C+13                                                    
         BAS   RE,CNL12            GET LENGTH OF SECOND PART OF RANGE           
         STC   R0,SLPFA2L                                                       
         LA    R1,SLPFA2C                                                       
         BAS   RE,CNL16            AND LOOK FOR YEARS AND WILD                  
*                                                                               
CNL7     CLI   GLPLN,54            ANY CONTRA FILTER                            
         BL    CNL8                                                             
         MVC   SLPFC1C,GLPCON1     CONTRA-ACCOUNT CODE                          
         LA    R1,SLPFC1C+13                                                    
         BAS   RE,CNL12            GET LENGTH OF CONTRA ACCOUNT                 
         STC   R0,SLPFC1L          SAVE LENGTH OF FROM ACCOUNT                  
         LA    R1,SLPFC1C                                                       
         BAS   RE,CNL16            YEAR OR WILD                                 
         MVC   SLPFCST,BYTE        TYPE(REGULAR OR WILD)                        
*                                                                               
         CLI   GLPLN,68                                                         
         BL    CNL8                                                             
         CLI   SLPFCST,SLPWLD      IS THERE WILD DATA IN CONTRA                 
         BNE   *+6                                                              
         DC    H'0'                IF SO, THERE SHOULD NOT BE A RANGE           
         MVI   SLPFCST,SLPRNG                                                   
         MVC   SLPFC2C,GLPCON2     RANGE OF CONTRA-ACCOUNT                      
         LA    R1,SLPFC2C+13       LENGTH OF CONTRA-ACCOUNT 2                   
         BAS   RE,CNL12                                                         
         STC   R0,SLPFC2L                                                       
         LA    R1,SLPFC2C                                                       
         BAS   RE,CNL16                                                         
*                                                                               
CNL8     AH    R3,=H'1'            ADD TO COUNTER                               
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
*&&US                                                                           
         CLI   GLPACTN,C'%'        IS IT A PERCENT LINE                         
         BE    *+12                                                             
         CLI   GLPACTN,C'/'        OR DIVIDE                                    
         BNE   CNL10                                                            
         L     RF,NXTSLP                                                        
         MVC   0(SLPLNQ,RF),SLPREC COPY ENTRY                                   
         MVC   SLPFA1C-SLPD(L'SLPFA1C,RF),SLPFA2C                               
         MVC   SLPFA1L-SLPD(L'SLPFA1L,RF),SLPFA2L                               
         XC    SLPFA2C-SLPD(L'SLPFA2C,RF),SLPFA2C-SLPD(RF)                      
         XC    SLPFA2L-SLPD(L'SLPFA2L,RF),SLPFA2L-SLPD(RF)                      
         OI    SLPHPCT-SLPD(RF),HPDIV       DIVISOR                             
         CLI   GLPACTN,C'%'        IS IT A PERCENT LINE                         
         BNE   *+8                                                              
         OI    SLPHPCT-SLPD(RF),HPPCT                                           
         MVI   SLPFAST-SLPD(RF),0                                               
         XC    SLPFA2C,SLPFA2C                                                  
         MVI   SLPFA2L,0                                                        
         MVI   SLPFAST,0                                                        
         OI    SLPHPCT,HPDID       DIVIDEND                                     
         CLI   GLPACTN,C'%'        IS IT A PERCENT LINE                         
         BNE   *+8                                                              
         OI    SLPHPCT-SLPD(RF),HPPCT                                           
         AH    R3,=H'1'                                                         
         AH    RF,=Y(SLPLNQ)                                                    
         ST    RF,NXTSLP                                                        
         SH    R5,=H'1'                                                         
*&&                                                                             
*                                                                               
CNL10    L     RF,NXTSLP           NEXT AVAILABLE SPACE                         
         L     RE,ASLPWRK                                                       
         MVC   0(SLPLNQ,RF),0(RE)                                               
         AH    RF,=Y(SLPLNQ)                                                    
         ST    RF,NXTSLP                                                        
         MVI   0(RF),X'FF'         MARK END OF TABLE                            
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         SH    R5,=H'1'                                                         
         BP    *+6                                                              
         DC    H'0'                NOT ENOUGH ROOM FOR SUPERLEDGER              
*                                                                               
CNL11    SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     CNL4                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE LENGTH OF AN ACCOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
CNL12    LA    R0,14                                                            
         CLI   0(R1),X'40'                                                      
         BHR   RE                                                               
         BCTR  R1,0                                                             
         BCT   R0,*-8                                                           
         DC    H'0'                NO DATA IN ACCOUNT FIELD                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO FIND WILD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
CNL16    MVI   BYTE,0                                                           
CNL17    CLI   0(R1),C'*'          DOES ACCOUNT HAVE WILD(*)                    
         BNE   *+16                                                             
         MVI   0(R1),0             MAKE THE WILD 00'S                           
         MVI   BYTE,SLPWLD         TURN ON WILD TYPE                            
         B     CNL18                                                            
         CLC   0(2,R1),=C'?T'      OR VARY BY YEAR                              
         BNE   *+10                                                             
         MVC   0(2,R1),CURRYRQ     THIS YEAR                                    
         CLC   0(2,R1),=C'?L'                                                   
         BNE   *+10                                                             
         MVC   0(2,R1),LASTYRQ     LAST YEAR                                    
         CLC   0(2,R1),=C'?N'                                                   
         BNE   *+10                                                             
         MVC   0(2,R1),NEXTYRQ     NEXT YEAR                                    
CNL18    LA    R1,1(R1)                                                         
         BCT   R0,CNL17                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SORT IN ASCENDING SEQUENCE                                          *         
***********************************************************************         
         SPACE 1                                                                
CNL20    ST    R3,SLPNUM           SAVE NUMBER OF ENTRIES                       
         L     R2,ASLP             R2=START OF TABLE                            
         LA    R0,L'SLPREC         R0=LENGTH OF RECORD                          
         GOTO1 QSORTR,DMCB,(R2),(R3),(R0),(R0),0                                
         ORG   *-2                                                              
         O     RF,=X'80000000'     SET 31 BIT MODE                              
         BASSM RE,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD AND SAVE THE CRITICAL REGISTERS FOR THE BINARY SEARCH         *         
***********************************************************************         
         SPACE 1                                                                
         LR    R3,R2               R3=SAVE START OF TABLE ADDRESS               
         L     R1,SLPNUM           NUMBER OF ENTRIES                            
         MH    R1,=Y(SLPLNQ)       X LENGTH                                     
         LA    R1,0(R2,R1)         R1=END OF SEARCH TABLE-1                     
         BCTR  R1,0                                                             
         LA    R0,SLPLNQ           LENGTH OF ENTRY                              
         LR    R4,R0                                                            
         SR    R2,R0               R2=A(START OF TABLE-L'ENTRY)                 
         SR    R1,R2                                                            
         BXLE  R0,R0,*             R0=LOWEST POWER OF 2 GE R1                   
         AR    R1,R2               R1=A(END OF LAST ENTRY)                      
         STM   R0,R4,SRCREGS       SAVE REGISTERS FOR BINARY SEARCH             
         EJECT                                                                  
***********************************************************************         
* RESOLVE 'TO' POINTERS                                               *         
***********************************************************************         
         SPACE 1                                                                
         L     R2,ASLP             R2=SUPERLEDGER TABLE                         
         USING SLPD,R2                                                          
         L     R5,ALNK             R5 TO START OF LINK LIST ENTRIES             
         USING LNKD,R5                                                          
         LH    R3,=Y(LKTMX)        NUMBER OF ENTRIES                            
         MH    R3,=Y(LKTLN)        X LENGTH                                     
         AR    R3,R5               R3=END OF TABLE                              
         XC    0(8,R5),0(R5)       CLEAR PARENT AND NUMBER                      
         LA    R7,LNKCHLD          R7=LINK LIST                                 
*                                                                               
CNL23    CLI   SLPFA1C,C'F'        'FROM' LEDGER A SUPERLEDGER                  
         BE    CNL27               DON'T ESTABLISH POINTERS                     
         CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    CNL28                                                            
         LA    R4,SLPLNQ(R2)       R4=NEXT ENTRY                                
         CLC   0(SLPLQFR,R2),0(R4) IS IT THE SAME AS PREVIOUS                   
         BNE   CNL26               IF NOT, PROCESS THIS ENTRY                   
         LR    R2,R4               GET R2 TO END OF THIS 'FROM' DATA            
         B     CNL23                                                            
*                                                                               
CNL26    L     R0,POS              ASSUME POSITIVE                              
         CLI   SLPSIGN,C'+'                                                     
         BE    *+8                                                              
         L     R0,NEG              UNLESS IT'S A SUBTRACT                       
         OR    R2,R0                                                            
         ST    R2,LNKPRNT          SAVE A(POSTING ENTRY) IN LIST                
*MNNEW   LA    RE,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RE,=X'80000000'                                                  
*MNNEW   BSM   0,RE                                                             
         OI    RNSW,RN31B          SET - STAY IN 31 BIT MODE                    
         BAS   RE,LNK              BUILD THE LINK LIST FOR THIS ENTRY           
         OC    LNKNUM,LNKNUM       ANY ENTRIES                                  
         BZ    CNL27               BRANCH IF NONE                               
         ST    R5,SLPLNK           SAVE A(LINK LIST) IN POSTING KEY             
         LR    R5,R7               R5=NEXT LINK AREA                            
         XC    0(8,R5),0(R5)       CLEAR PARENT AND NUMBER                      
         LA    R7,LNKCHLD          R7=LINK LIST                                 
*                                                                               
CNL27    LA    R2,SLPLNQ(R2)       R2=NEXT ENTRY                                
         B     CNL23                                                            
*                                                                               
CNL28    CR    R5,R3               NEXT LINK ENTRY VS. END                      
         BNH   *+6                                                              
         DC    H'0'                LINK LIST EXCEEDS MAXIMUM                    
         OI    SLOPTN,SLINIT       INITIALIZED                                  
*                                                                               
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
         NI    RNSW,ALL-RN31B      TURNOFF - STAY IN 31 BIT MODE                
         EJECT                                                                  
***********************************************************************         
* PRINT SUPERLEDGER RULES                                             *         
***********************************************************************         
         SPACE 1                                                                
CNL45    TM    UPSI,RULES          SEARCH FOR A SPECIAL ACCOUNT                 
         BZ    CNLXIT                                                           
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,YES                                                     
         L     R2,ASLP             START OF TABLE                               
*                                                                               
CNL46    LR    R3,R2               SAVE R2                                      
*MNNEW   LA    R1,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     R1,=X'80000000'                                                  
*MNNEW   BSM   0,R1                                                             
         L     RE,ASLPWRK                                                       
         MVC   0(SLPLNQ,RE),0(R2)                                               
         MVC   CURRSIGN,SLPSIGN                                                 
         CLI   SLPFA1C,C'F'        IGNORE UNIT F                                
         BE    CNL49                                                            
*MNNEW   LA    R1,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,R1                                                             
         GOTO1 ACREPORT                                                         
         BAS   RE,CNL50            PRINT THE FIRST ENTRY                        
*MNNEW   LA    R1,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     R1,=X'80000000'                                                  
*MNNEW   BSM   0,R1                                                             
         OC    SLPLNK,SLPLNK       IS THERE A LINK ENTRY                        
         BZ    CNL49               NOT LINKED                                   
         L     R5,SLPLNK                                                        
         USING LNKD,R5                                                          
         L     R0,LNKNUM           NUMBER OF ENTRIES IN THIS LIST               
         LA    R7,LNKCHLD                                                       
*                                                                               
CNL47    L     R2,0(R7)                                                         
         L     RE,ASLPWRK                                                       
         MVC   0(SLPLNQ,RE),0(R2)                                               
         MVI   CURRSIGN,C'+'       SAVE CURRENT SIGN                            
         TM    0(R7),X'80'                                                      
         BNO   *+8                                                              
         MVI   CURRSIGN,C'-'                                                    
*MNNEW   LA    R1,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,R1                                                             
         BAS   RE,CNL50            PRINT THE ENTRY                              
*MNNEW   LA    R1,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     R1,=X'80000000'                                                  
*MNNEW   BSM   0,R1                                                             
         LA    R7,4(R7)                                                         
         BCT   R0,CNL47                                                         
*                                                                               
CNL49    LR    R2,R3                                                            
         LA    R2,SLPLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   CNL46                                                            
*MNNEW   LA    R1,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,R1                                                             
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,YES                                                     
         B     CNLXIT                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT AND PRINT A SUPERLEDGER ENTRY                                *         
***********************************************************************         
         SPACE 1                                                                
CNL50    NTR1  ,                                                                
         L     R2,ASLPWRK                                                       
         USING SLPD,R2                                                          
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         MVC   XP+2(14),SLPFA1C    FROM ACCOUNT                                 
         MVC   XP+18(14),SLPFA2C   ACCOUNT RANGE                                
         MVC   XP+33(14),SLPFC1C   FROM CONTRA                                  
         MVC   XP+49(14),SLPFC2C   FROM CONTRA                                  
         MVC   XP+65(5),SLPFLT     FILTERS                                      
         MVC   XP+73(1),SLPBKT     BUCKET TYPE                                  
         MVC   XP+76(1),SLPSIGN    THIS SIGN                                    
         MVC   XP+80(1),CURRSIGN   ACTION                                       
         MVC   XP+84(14),SLPTACC   TO ACCOUNT                                   
         GOTO1 ACREPORT            PRINT THE "TO" ACCOUNT                       
CNLXIT   XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* LINK ALL 'TO' POINTERS                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING SLPD,R2                                                          
         USING LNKD,R5                                                          
         USING ARGD,R6                                                          
LNK      NTR1  ,                                                                
         AP    LNKCNT,=P'1'        OLD APG HAD A MAX OF 2                       
         CP    LNKCNT,LNKMAX       THIS SHOULD PREVENT LOOPS                    
         BH    LNKX                                                             
         LR    R1,R0               SAVE CALLING SIGN                            
         LR    R4,R2               AND POINTER                                  
         MVI   LASTARG,X'FF'       FORCE SEARCH                                 
         LA    R6,SRCHARG                                                       
         MVC   ARGACC(ARGLEN),SPACES                                            
         MVC   ARGACC,SLPTACC      'TO' ACCOUNT TO SEARCH ARGUMENT              
         GOTO1 ASRCH               SEARCH FOR 'FROM' ENTRY                      
         TM    SLOPTN,SLFND        WAS IT FOUND                                 
         BNO   LNKX                NOT FOUND                                    
         ICM   R2,15,ARGRTN        R2=A(RESOLUTION)                             
*                                                                               
LNK05    LA    RE,POS                                                           
         CLI   SLPSIGN,C'+'        NEW SIGN IS NEGATIVE                         
         BE    *+8                                                              
         LA    RE,NEG              REVERSE THE ACTION                           
         X     R0,0(RE)            SWITCH THE SIGN IF NEEDED                    
         OR    R2,R0               CURRENT SIGN INTO R2                         
         CLM   R2,15,LNKPRNT       IS R5 ENTRY POINTING TO ITSELF               
         BE    LNKX                SKIP IT                                      
*                                                                               
*                                  ADD POINTER (R2) TO LINK LIST                
         L     R7,LNKNUM           NUMBER                                       
         LR    R3,R7                                                            
         SLL   R7,2                X 4                                          
         LA    R7,LNKCHLD(R7)                                                   
         ST    R2,0(R7)            SAVE CURRENT ENTRY                           
         LA    R7,4(R7)            R7=NEW END OF LIST                           
         LA    R3,1(R3)            UPDATE COUNT                                 
         ST    R3,LNKNUM           AND SAVE IT                                  
         SR    R3,R3               LINKED                                       
         BAS   RE,LNK              RESOLVE NEXT 'TO' POINTER                    
         MVC   ARGACC(ARGLEN),SPACES                                            
         MVC   ARGACC,SLPTACC-SLPD(R4)  ACCOUNT TO SEARCH ARGUMENT              
*                                                                               
LNK07    LA    R3,0(R2)                                                         
         SH    R3,=Y(SLPLNQ)       R3 TO PREVIOUS ENTRY                         
         SR    RE,RE                                                            
         IC    RE,SRCHLEN          MINIMUM SEARCH LENGTH                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R2)       IS IT THE SAME FROM ACCOUNT                  
         BNE   LNKX                IF NOT THE SAME, EXIT THIS LIST              
         LR    R2,R3               R2=PREVIOUS ENTRY                            
         OI    SLOPTN,SLFLT        ONLY FILTER THIS ONE ITEM                    
         GOTO1 ASRCH                                                            
         NI    SLOPTN,X'FF'-SLFLT  TURN-OFF FILTER OPTION                       
         TM    SLOPTN,SLFND        SHOULD IT BE INCLUDED                        
         BNO   LNK07               DO NOT INCLUDE                               
         LR    R0,R1               RESTORE THE CALLING SIGN                     
         B     LNK05                                                            
*                                                                               
LNKX     SP    LNKCNT,=P'1'                                                     
         XIT1  REGS=(R7)                                                        
         DROP  R2,R5,R6                                                         
*                                                                               
POS      DC    F'0'                                                             
NEG      DC    X'80000000'                                                      
LNKCNT   DC    PL1'0'                                                           
LNKMAX   DC    PL1'3'                                                           
NXTSLP   DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R9                                                            
         EJECT                                                                  
***********************************************************************         
* SEARCH SUPERLEDGER FOR SUBSTITUTION ACCOUNT                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SLPD,R2                                                          
         USING ARGD,R6                                                          
SRCH     DS    0D                                                               
         NMOD1 0,*SRCH**,R9                                                     
         L     RC,BASERC                                                        
*MNNEW   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
*MNNEW   O     RF,=X'80000000'                                                  
*MNNEW   BSM   0,RF                                                             
         NI    SLOPTN,X'FF'-SLFND  TURN OFF THE FOUND INDICATOR                 
         TM    SLOPTN,SLFLT        ONLY FILTER THIS ONE ITEM                    
         BO    SRCH13                                                           
         XC    ARGRTN,ARGRTN                                                    
         OC    ARGFLT,SPACES                                                    
         CLC   ARGACC(ARGLEN),LASTARG IF SAME SEARCH ARGUMENT                   
         BNE   SRCH01                                                           
         LR    R1,R6                                                            
         LA    R6,LASTARG                                                       
         ICM   R2,15,ARGRTN        RETURN SAME ADDRESS                          
         LR    R6,R1                                                            
         B     SRCH40                                                           
*                                                                               
SRCH01   LM    R0,R4,SRCREGS       LOAD THE SEARCH REGISTERS                    
         SR    R5,R5                                                            
         IC    R5,SRCHLEN          LENGTH OF SEARCH ARGUMENT                    
         BCTR  R5,0                                                             
*                                                                               
SRCH08   SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,R4               TEST IF LESS THAN AN ENTRY LENGTH            
         BNL   SRCH10              NOT AT END OF TABLE                          
         B     SRCH35              ACCOUNT - NOT FOUND                          
*                                                                               
SRCH10   BXH   R2,R0,SRCH11        COMPUTE NEW TABLE START ADDRESS              
         LA    RF,SLPFA1C                                                       
         EX    R5,SRCHACC          SEARCH ACCOUNT VS. TABLE                     
         BE    SRCH12              GOT A MATCH                                  
         BH    SRCH08                                                           
SRCH11   SR    R2,R0                                                            
         B     SRCH08                                                           
*                                                                               
SRCH12   LA    R1,SLPLNQ(R2)       R1 TO NEXT ITEM IN TABLE                     
         LA    RF,SLPFA1C-SLPD(R1)                                              
         EX    R5,SRCHACC          SEARCH ACCOUNT VS. TABLE                     
         BNE   SRCH14              R2 NOW AT LAST ITEM THAT MATCHES             
         LR    R2,R1               R2 TO NEXT ENTRY                             
         B     SRCH12              GET NEXT FOR THIS ACCOUNT                    
*                                                                               
SRCH13   SR    R5,R5               FROM THE LNK ROUTINE                         
*                                                                               
SRCH14   CLI   SLPFAST,SLPRNG      IS THERE RANGE DATA                          
         BE    SRCH15                                                           
         CLI   SLPFAST,SLPWLD      IS THERE WILD DATA                           
         BE    SRCH16              HANDLE WILD DATA IN ACCOUNT                  
         IC    R5,SLPFA1L          FROM LENGTH                                  
         BCTR  R5,0                                                             
         LA    RF,SLPFA1C                                                       
         EX    R5,SRCHACC          SEARCH ACCOUNT VS. TABLE                     
         BE    SRCH20              CHECK FILTERS AND CONTRA                     
         B     SRCH30              GET PREVIOUS ENTRY                           
*                                                                               
SRCH15   IC    R5,SLPFA1L          LENGTH OF LOW RANGE                          
         BCTR  R5,0                                                             
         LA    RF,SLPFA1C                                                       
         EX    R5,SRCHACC          SEARCH CONTRA VS. TABLE                      
         BL    SRCH30              GET PREVIOUS ENTRY                           
         IC    R5,SLPFA2L          LENGTH OF HIGH RANGE                         
         BCTR  R5,0                                                             
         LA    RF,SLPFA2C                                                       
         EX    R5,SRCHACC                                                       
         BNH   SRCH20              CHECK FILTERS AND CONTRA                     
         B     SRCH30              GET PREVIOUS ENTRY                           
         SPACE 2                                                                
*              FROM ACCOUNT HAS WILD ENTRIES                                    
         SPACE 1                                                                
SRCH16   IC    R5,SLPFA1L          FROM LENGTH                                  
         LA    RF,SLPFA1C                                                       
         LA    RE,ARGACC                                                        
*                                                                               
SRCH17   CLI   0(RF),0             DON'T CHECK WILD DATA                        
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   SRCH30              NO MATCH                                     
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R5,SRCH17                                                        
         EJECT                                                                  
***********************************************************************         
* COMPARE FILTERS AND CONTRA ACCOUNTS                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCH20   LA    R5,L'SLPFLT         COMPARE FILTERS                              
         LA    RF,SLPFLT                                                        
         LA    RE,ARGFLT                                                        
*                                                                               
SRCH21   CLI   0(RF),X'40'         NO FILTER - SPACES                           
         BE    SRCH22                                                           
         CLI   0(RF),C'*'          OR STAR                                      
         BE    SRCH22                                                           
         CLC   0(1,RE),0(RF)       ARGFLT VS. SLPFLT                            
         BNE   SRCH30              NO MATCH                                     
*                                                                               
SRCH22   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R5,SRCH21                                                        
*                                                                               
         CLI   SLPBKT,C' '         BUCKET FILTER                                
         BNH   *+14                                                             
         CLC   SLPBKT,ARGBKT                                                    
         BNE   SRCH30                                                           
         CLI   SLPFC1L,0           NO CONTRA FILTER                             
         BE    SRCH40              IT'S MATCHED                                 
         CLI   SLPFCST,SLPRNG      IS THERE RANGE DATA                          
         BE    SRCH24                                                           
         CLI   SLPFCST,SLPWLD      IS THERE WILD DATA IN CONTRA                 
         BE    SRCH26              HANDLE WILD DATA IN ACCOUNT                  
         IC    R5,SLPFC1L          FROM LENGTH                                  
         BCTR  R5,0                                                             
         LA    RF,SLPFC1C                                                       
         EX    R5,SRCHCON          SEARCH ACCOUNT VS. TABLE                     
         BE    SRCH40              FOUND A MATCH                                
         B     SRCH30              GET PREVIOUS ENTRY                           
*                                                                               
SRCH24   IC    R5,SLPFC1L          LENGTH OF LOW RANGE                          
         BCTR  R5,0                                                             
         LA    RF,SLPFC1C                                                       
         EX    R5,SRCHCON          SEARCH CONTRA VS. TABLE                      
         BL    SRCH30              GET PREVIOUS ENTRY                           
         IC    R5,SLPFC2L          LENGTH OF HIGH RANGE                         
         BCTR  R5,0                                                             
         LA    RF,SLPFC2C                                                       
         EX    R5,SRCHCON                                                       
         BNH   SRCH40              IT'S MATCHED                                 
         B     SRCH30              GET PREVIOUS ENTRY                           
         SPACE 2                                                                
*              FROM CONTRA HAS WILD ENTRIES                                     
         SPACE 1                                                                
SRCH26   IC    R5,SLPFC1L          FROM LENGTH                                  
         LA    RF,SLPFC1C                                                       
         LA    RE,ARGCON                                                        
*                                                                               
SRCH25   CLI   0(RF),0             DON'T CHECK WILD DATA                        
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   SRCH30              NO MATCH                                     
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R5,SRCH25                                                        
         B     SRCH40             FOUND A MATCH                                 
*                                                                               
SRCH30   TM    SLOPTN,SLFLT       FILTERING OPTION (ONE ITEM AT A TIME)         
         BO    SRCH35                                                           
         SH    R2,=Y(SLPLNQ)      BACK-UP ONE ENTRY                             
         CR    R2,R3              AT START OF TABLE                             
         BL    SRCH35             NOT FOUND                                     
         LA    RF,SLPFA1C                                                       
         IC    R5,SRCHLEN         LENGTH OF SEARCH ARG                          
         BCTR  R5,0                                                             
         EX    R5,SRCHACC         SEARCH CONTRA VS. TABLE                       
         BE    SRCH14                                                           
*                                                                               
SRCH35   SR    R2,R2              ITEM NOT FOUND                                
*                                                                               
SRCH40   STCM  R2,15,ARGRTN       RETURN ADDRESS OF ENTRY                       
         LTR   R2,R2              TEST ITEM NOT FOUND                           
         BZ    SRCHX                                                            
         OI    SLOPTN,SLFND       FOUND INDICATOR                               
         L     RE,ASLPWRK                                                       
         MVC   0(SLPLNQ,RE),0(R2)       RETURN ITEM                             
*                                                                               
SRCHX    MVC   LASTARG(ARGLNQ),ARGACC SAVE THIS FOR NEXT TIME                   
         TM    RNSW,RN31B          STAY IN 31 BIT MODE                          
*MNNEW   BO    *+10                                                             
         BO    SRCHXIT                                                          
*MNNEW   LA    RF,*+6              SET 24 BIT MODE                              
*MNNEW   BSM   0,RF                                                             
SRCHXIT  XIT1                                                                   
*                                                                               
SRCHACC  CLC   ARGACC(0),0(RF)     SEARCH ARGUMENT ACCOUNT VS. TABLE            
SRCHCON  CLC   ARGCON(0),0(RF)     SEARCH ARGUMENT CONTRA  VS. TABLE            
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP   RB,R9                                                           
         TITLE 'SUBROUTINES - DOWNLOAD ROUTINES'                                
***********************************************************************         
* DOWNLOAD ANY REPORT                                                 *         
*  R1=MODE                                                            *         
***********************************************************************         
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**,R9                                                    
         L     RC,BASERC                                                        
         STC   R1,DWNMODE          SAVE CURRENT MODE                            
         SR    R8,R8               SAVE SUB-PROGRAM                             
         IC    R8,RCSUBPRG                                                      
         MVI   RCSUBPRG,99         SET TO AVOID RUN SPECS                       
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         L     R7,ADWNBUF                                                       
         USING DLCBD,R7                                                         
*                                                                               
         TM    DWNSTAT,DWNNOPRT    DON'T PRINT THIS ONE                         
         BO    DWNX                                                             
         TM    DWNSTAT,DWNINIT     HAS IT BEEN INITIALIZED?                     
         BO    DWNL5                                                            
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVI   BOXYORN,NO          NO BOXES                                     
         MVI   BOXSHADE,0          NO SHADING                                   
         MVC   BOXWIDTH,=AL4(L'XP)                                              
         MVI   DLCBACT,DLCBINIT    START OF REPORT                              
         LA    RE,XP                                                            
         ST    RE,DLCBAPL          PRINT LINE AREA                              
         LA    RE,DWNHOOK          HOOK ROUTINE ADDRESS                         
         ST    RE,DLCBAPR          PRINT ROUTINE                                
         MVI   DLCXMAXL+1,L'XP     LINE LENGTH                                  
         MVI   DLCXDELC,C' '       DELEMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT                                         
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT                               
         MVI   DLCXEOLC,X'5E'      END OF LINE ,SEMI-COLON                      
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 DLFLD,DLCBD                                                      
         MVI   FORCEHED,YES        TO SEPERATE REQUEST DETAILS                  
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLX,XSPACES                                                  
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD FOR TEXT                  
         OI    DWNSTAT,DWNINIT                                                  
*                                                                               
DWNL5    TM    DWNOPTN1,DWNOXHED   EXCLUDE HEADLINES                            
         BO    DWNL7                                                            
         TM    DWNSTAT,DWNHEAD     HEADLINES PRINTED                            
         BO    DWNL7                                                            
         BAS   RE,DWNH             DOWNLOAD THE HEADLINE                        
         OI    DWNSTAT,DWNHEAD                                                  
*                                                                               
DWNL7    CLI   DWNMODE,DWNTEXT     PUT TEXT                                     
         BNE   DWNL9                                                            
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         MVC   DLCBFLX,P                                                        
         B     DWNLX                                                            
*                                                                               
DWNL9    CLI   DWNMODE,DWNROWS     DOWNLOAD ROW CODES/ROW NAMES                 
         BNE   DWNL15                                                           
         SR    R1,R1                                                            
         IC    R1,REPCODE          REPORT CODE                                  
         BCTR  R1,0                                                             
         M     R0,WRSTACK          X WIDTH                                      
         L     R2,ARSTACK          R2=RSTACK                                    
         AR    R2,R1                                                            
*                                                                               
         USING RSTACKD,R2                                                       
         TM    DWNOPTN1,DWNONOMD   TEST DON'T PRINT MODULE                      
         BO    DWNL10                                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLX(6),MODULE+2 MODULE #                                     
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNL10   L     R0,RSNROWS          #ROWS                                        
         SR    R2,R2                                                            
         LA    R4,SORTLEVS                                                      
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
*                                                                               
DWNL11   MVC   WORK,XSPACES                                                     
         MVC   DLCBFLX(L'SORTKEY),SORTKEY-SORTLV(R4)                            
         LA    RE,SORTNAMS                                                      
         LR    RF,R2                                                            
         MH    RF,=H'36'                                                        
         AR    RE,RF                                                            
         MVC   WORK(36),0(RE)                                                   
         CLC   WORK(6),=C'FILLER'  SKIP 'FILLER' ROWS                           
         BE    *+12                                                             
         CLI   WORK,C'='           SKIP TOTAL ROWS                              
         BNE   *+16                                                             
         MVC   DLCBFLX,XSPACES                                                  
         MVC   WORK,XSPACES                                                     
         TM    NAMETYPE,NAMETCDE   TEST CODE WANTED                             
         BNO   DWNL12                                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNL12   TM    DWNOPTN1,DWNOXNME   OPTION TO SKIP ROW NAMES                     
         BO    DWNL13                                                           
         TM    NAMETYPE,NAMETNME   TEST NAME OR BOTH                            
         BNO   DWNL13                                                           
         MVC   DLCBFLX(36),WORK                                                 
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNL13   LA    R2,1(R2)            BUMP COUNTER                                 
         LA    R4,SORTKLQ(R4)                                                   
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,DWNL11                                                        
         STC   R8,RCSUBPRG         RESTORE CODE                                 
         MVI   STOPHOOK,NO                                                      
         B     DWNX                                                             
*                                                                               
DWNL15   CLI   DWNMODE,DWNNUMB     PUT NUMBER                                   
         BNE   DWNL17                                                           
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBNUM     NUMBER                                       
         MVC   DLCBFLD,P                                                        
         B     DWNLX                                                            
*                                                                               
DWNL17   CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BNE   DWNL19                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         B     DWNLX                                                            
*                                                                               
DWNL19   CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR                                                  
*                                                                               
DWNLX    GOTO1 DLFLD,DLCBD                                                      
         STC   R8,RCSUBPRG         RESTORE CODE                                 
         MVI   STOPHOOK,NO                                                      
         B     DWNX                                                             
         DROP  R2,R4,R5,R6,R7                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DOWLOAD HEADLINES                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING DLCBD,R7                                                         
DWNH     NTR1  ,                                                                
         SR    R1,R1                                                            
         IC    R1,REPCODE          REPORT CODE                                  
         BCTR  R1,0                                                             
         M     R0,WRSTACK          X WIDTH                                      
         L     R2,ARSTACK          R2=RSTACK                                    
         AR    R2,R1                                                            
         TM    DWNSTAT,DWNMICRO    MICRO-CONTROL REPORTING                      
         BZ    DWNH15              NO, REGULAR HEADINGS                         
*                                                                               
         USING RSTACKD,R2                                                       
         LA    R3,DLCBFLX          R3=DOWNLOAD TEXT FIELD                       
         LA    R4,RSARNAME                                                      
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
         LA    R0,10                                                            
*                                                                               
DWNH3    ICM   RF,15,0(R4)         LOOK AT ROWNAME SPEC                         
         BZ    DWNH9               NONE, SO TRY NEXT ROW                        
         CLI   3(RF),1             IS IT A HEADLINE                             
         BNE   DWNH9                                                            
         SR    R1,R1                                                            
         IC    R1,1(RF)            YES - DIG OUT TITLE                          
         SH    R1,=H'6'                                                         
         BM    DWNH5                                                            
*MN      EX    R1,*+4                                                           
         MVC   0(0,R3),5(RF)       MOVE IN PREFIX TITLE                         
         EX    R1,*-6                                                           
*                                                                               
DWNH5    SR    R1,R1                                                            
         ICM   R1,1,RSBIGH         WIDEST TITLE                                 
         LA    R1,0(R1,R3)         GO DISTANCE OF LONGEST ROW PREFIX            
         BZ    *+8                                                              
         AH    R1,=H'01'                                                        
         TM    2(RF),X'02'         ROW CODE?                                    
         BNO   *+10                NO                                           
         MVC   0(14,R1),NAMEEFF    YES, MOVE IN CODE                            
         SR    RE,RE                                                            
         ICM   RE,1,RSBIGC         WIDEST CODE                                  
         BZ    *+8                                                              
         AH    RE,=H'01'                                                        
         AR    R1,RE               DISPLACE BY WIDEST CODE                      
         TM    2(RF),X'01'         ROWNAME USED?                                
         BNO   DWNH7               NO                                           
         CLI   NAMENAME,C'='       DON'T PRINT ACCOUNTS WITH "="                
         BE    DWNH7                                                            
         OI    NAMENAME+6,X'40'                                                 
         CLC   NAMENAME(7),=C'FILLER '  DON'T PRINT ACCTS CALLED FILLER         
         BE    DWNH7                                                            
         MVC   0(36,R1),NAMENAME   AND MOVE IN THE NAME                         
*                                                                               
DWNH7    MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         MVI   DLCBLEN,66          SET FIELD WIDTH                              
         GOTO1 DLFLD,DLCBD                                                      
         MVI   DLCBACT,DLCBEOL     SET EOL                                      
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNH9    LA    R4,4(R4)                                                         
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,DWNH3                                                         
         B     DWNX                                                             
*                                                                               
DWNH15   LA    R0,3                DOWNLOAD THREE SET OF HEADINGS               
         LA    R4,RSHEAD1          COLUMN HEADING                               
*                                                                               
DWNH17   ST    R0,SVR0                                                          
         ST    R4,DWNCOLHD         SAVE NEXT LINE                               
         TM    DWNOPTN1,DWNONOMD   TEST DON'T PRINT MODULE                      
         BO    DWNH19                                                           
         MVI   DLCBACT,DLCBPUT     PUT FOR FIRST REPORT CODE                    
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNH19   L     R0,RSNROWS          NUMBER OF ROWS                               
         LA    R3,RSARNAME                                                      
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
*                                                                               
DWNH22   TM    DWNOPTN1,DWNOXNME   EXCLUDE NAME?                                
         BO    DWNH23                                                           
         ICM   RE,15,0(R3)         A(PREFIX OF ROW)                             
         BZ    DWNH23              NO PREFIX                                    
         SR    RF,RF                                                            
         IC    RF,1(RE)            LENGTH OF ELEMENT                            
         SH    RF,=H'06'           NON-DATA AND EX                              
         BM    DWNH23                                                           
*MN      EX    RF,*+4                                                           
         MVC   DLCBFLX(0),5(RE)                                                 
         EX    RF,*-6                                                           
         XC    0(4,R3),0(R3)       CLEAR FOR NEXT TIME THROUGH                  
*                                                                               
DWNH23   TM    NAMETYPE,NAMETCDE   TEST CODE WANTED                             
         BNO   DWNH24                                                           
         MVI   DLCBACT,DLCBPUT     PUT FOR CODE                                 
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNH24   TM    NAMETYPE,NAMETNME   TEST NAME WANTED                             
         BNO   DWNH26                                                           
         MVI   DLCBACT,DLCBPUT     PUT FOR NAME                                 
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNH26   LA    R3,4(R3)            BUMP TO NEXT ROW PREFIX ADDRESS              
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,DWNH22                                                        
*                                                                               
DWNH30   L     R5,RSNCOLS          NUMBER OF COLS                               
         LA    R3,RSCOLS           POINT TO COLUMN INFO                         
DWNH32   CLI   8(R3),1             HIDDEN COLUMN IF W=1                         
         BE    DWNH38              LOOP                                         
         MVC   DLCBLEN,8(R3)       MOVE IN LENGTH                               
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         SR    RF,RF                                                            
         IC    RF,8(R3)            LENGTH OF COLUMN                             
         BCTR  RF,0                                                             
*MN      EX    RF,*+4                                                           
         MVC   DLCBFLX(0),0(R4)    MOVE IN COLUMN HEADING DATA                  
         EX    RF,*-6                                                           
         LA    R4,2(R4,RF)         NEXT COLUMN HEADING DATA                     
         GOTO1 DLFLD,DLCBD                                                      
DWNH38   LA    R3,29(R3)           NEXT COLUMN INFO                             
         BCT   R5,DWNH32                                                        
*                                                                               
         L     R4,DWNCOLHD                                                      
         LA    R4,L'RSHEAD1(R4)    BUMP TO NEXT COLUMN                          
         MVI   DLCBACT,DLCBEOL     MARK END OF LINE                             
         GOTO1 DLFLD,DLCBD                                                      
         L     R0,SVR0                                                          
         BCT   R0,DWNH17           LOOP TO PUT NEXT HEAD-LINE                   
DWNX     XIT1                                                                   
         DROP  R2,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* HOOK FOR DOWN-LOAD PRINT ROUTINE                                    *         
***********************************************************************         
         SPACE 1                                                                
DWNHOOK  MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         MVI   SKIPSPEC,C'N'       THIS WILL IGNORE IF FOOTLINE AT END          
         MVI   FORCEHED,C'N'                                                    
         L     RF,ACREPORT                                                      
         BR    RF                                                               
*                                                                               
DWNCOLHD DS    A                   CURRENT COLUMN HEADLINE (1, 2 OR 3)          
         EJECT                                                                  
         LTORG                                                                  
         DROP   RB,R9                                                           
         TITLE 'SUBROUTINES - TSAR BUFFER CONTROL FOR CONTRAS'                  
***********************************************************************         
* TSAR BUFFER CONTROL                                                 *         
*  R1=MODE                                                            *         
***********************************************************************         
         SPACE 1                                                                
BUFC     DS    0D                                                               
         NMOD1 0,**BUFC**,R9                                                    
         L     RC,BASERC                                                        
         STC   R1,TSRMODE          SAVE CURRENT MODE                            
         LA    R1,CONTBLK          CONTRA BLOCK                                 
         USING TSARD,R1                                                         
         L     R6,ACONSR           CONTRA SUMMARY RECORD                        
         USING CONSD,R6                                                         
         TM    TSRSTAT,TSRSINIT    INITIALIZED?                                 
         BO    BUFC3                                                            
         OI    TSRSTAT,TSRSINIT                                                 
         XC    TSARD(TSARDL),TSARD CLEAR TSAR BLOCK                             
         MVC   TSACOM,ADCOMFAC     ADDR COMFACS                                 
         MVC   TSABUF,ACONBF       ADDR OF TSAR BUFFER                          
         MVC   TSAREC,=A(CONLN)    SIZE OF BUFFER                               
         MVI   TSKEYL,CONSKLNQ     KEY LENGTH                                   
         MVC   TSRECL,=Y(CONSLNQ)  RECORD LENGTH                                
         MVI   TSOFFACT,TSAINI     ACTION (INIT) IS HOB OF BUFFER               
         OI    TSIND2,TSI2MANY     USE FULL WORD COUNTERS                       
         GOTO1 TSAROFF                                                          
         BE    BUFC3                                                            
         DC    H'0'                                                             
*                                                                               
BUFC3    TM    TSRMODE,TSRMPUT     PUT                                          
         BNO   BUFC20                                                           
         LA    R2,LBUFR                                                         
         ST    R2,TSAREC           RECORD TO BE READ                            
         LA    R3,L'LBUFR          MOVE TO LOCAL STORAGE                        
         LR    R5,R3                                                            
         LA    R4,CONSKEY                                                       
         MVCL  R2,R4                                                            
         MVI   TSOFFACT,TSARDH     READ FOR RECORD                              
         OC    TSPRECN(4),TSPRECN  ANY RECORDS IN BUFFER                        
         BZ    BUFC5               NO, JUST ADD IT                              
         GOTO1 TSAROFF                                                          
         TM    TSERRS,TSERNF       RECORD FOUND                                 
         BNO   BUFC7               YES, ADD ACCUMS FROM NEW RECORD              
*                                                                               
BUFC5    LA    R2,CONSKEY                                                       
         ST    R2,TSAREC           RECORD TO BE READ                            
         MVI   TSOFFACT,TSAADD     ADD NEW RECORD                               
         GOTO1 TSAROFF                                                          
         BE    BUFCXY              RECORD ADDED, ALL DONE                       
         TM    TSERRS,TSEEOF       BUFFER FULL                                  
         BO    BUFC30              WRITE TO BUFFWRK                             
         DC    H'0'                ERROR                                        
*                                                                               
BUFC7    LA    R4,LBUFR+(CONSACS-CONSKEY)                                       
         LA    R5,CONSACS          DISPLACEMENT TO COLUMNS(OLD)                 
         LA    R0,NAC                                                           
         AP    0(8,R4),0(8,R5)     ADD ACCUMULATORS                             
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         MVI   TSOFFACT,TSAWRT     WRITE BACK NEW RECORD                        
         GOTO1 TSAROFF                                                          
         BE    BUFCXY                                                           
         DC    H'0'                                                             
*                                                                               
BUFC20   TM    TSRMODE,TSRMGET                                                  
         BNO   BUFC23                                                           
         TM    TSRSTAT,TSRSBUFF    ANY RECORDS IN BUFFWK ?                      
         BO    BUFC40                                                           
         MVI   TSOFFACT,TSARDH     READ RECORD                                  
         B     BUFC25                                                           
*                                                                               
BUFC23   TM    TSRMODE,TSRMNXT                                                  
         BO    *+6                                                              
         DC    H'0'                UNKNOWN ACTION                               
         TM    TSRSTAT,TSRSBUFF    ANY RECORDS IN BUFFWK?                       
         BO    BUFC43                                                           
         MVI   TSOFFACT,TSANXT     NEXT RECORD                                  
*                                                                               
BUFC25   LA    R2,CONSKEY                                                       
         ST    R2,TSAREC           RECORD TO BE READ                            
         OC    TSPRECN(4),TSPRECN  ANY RECORDS IN BUFFER                        
         BZ    BUFCXN              NO, SET NOT FOUND                            
         GOTO1 TSAROFF                                                          
         TM    TSERRS,TSEEOF                                                    
         BNO   BUFCXY                                                           
         B     BUFCXN                                                           
*                                                                               
BUFC30   TM    TSRSTAT,TSRSBUFF    HAS BUFFWK BEEN OPENED?                      
         BO    BUFC33                                                           
         OI    TSRSTAT,TSRSBUFF                                                 
         L     R3,ABUFFWK                                                       
         OPEN  ((R3),(OUTPUT))                                                  
         LA    R1,CONTBLK          CONTRA BLOCK                                 
*                                                                               
BUFC33   BAS   RE,BUFCLR           WRITE TSAR BUFFER TO BUFFWK                  
         B     BUFC5               AND TRY THIS AGAIN                           
         EJECT                                                                  
* GET RECORDS FROM BUFFWK                                                       
*                                                                               
BUFC40   BAS   RE,BUFCLR           MOVE REMAINING TO BUFFWK                     
         L     R3,ABUFFWK                                                       
         CLOSE ((R3))                                                           
         OI    TSRSTAT,TSRSFRST    TURN ON FIRST TIME                           
         L     R3,ABUFFWK                                                       
         OPEN  ((R3),(INPUT))                                                   
*                                                                               
BUFC43   TM    TSRSTAT,TSRSEOF     HAS EOF BEEN SET                             
         BNO   *+6                                                              
         DC    H'0'                SHOULD NOT BE HERE                           
         L     R3,ABUFFWK                                                       
         LA    R2,CONSKEY                                                       
         GET   (R3),(R2)                                                        
         B     BUFCXY                                                           
*                                                                               
BUFC99   OI    TSRSTAT,TSRSEOF     SET EOF                                      
         L     R3,ABUFFWK                                                       
         CLOSE ((R3))                                                           
         B     BUFCXN                                                           
         EJECT                                                                  
* CLEAR TSAR BUFFER BY WRITING ALL RECORDS TO BUFFWK                            
*                                                                               
BUFCLR   NTR1  ,                                                                
         LA    R1,CONTBLK          PUT ALL TSAR RECORDS TO BUFFWK               
         LA    R2,LBUFR                                                         
         ST    R2,TSAREC           SET A(TSAR RECORD)                           
         L     R3,ABUFFWK                                                       
*                                                                               
BUFCL3   LR    RE,R2                                                            
         LH    RF,=Y(L'LBUFR)                                                   
         XCEFL                                                                  
         LA    R1,CONTBLK          PUT ALL TSAR RECORDS TO BUFFWK               
         MVI   TSOFFACT,TSARDH     READ FOR FIRST RECORD                        
         GOTO1 TSAROFF                                                          
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    BUFCL5                                                           
         CLI   TSRMODE,TSRMGET     IF ACTION IS 'GET', MUST CLEAR ALL           
         BE    *+14                                                             
         CLC   0(L'CONSCON,R2),CONSCON IS RECORD RETURNED LOWER THAN            
         BNL   BUFCXY                  CURRENT CONTRA, IF NOT CAN'T             
         PUT   (R3),(R2)               WRITE TO BUFFWK                          
         LA    R1,CONTBLK                                                       
         MVI   TSOFFACT,TSADEL     DELETE THIS RECORD                           
         GOTO1 TSAROFF                                                          
         TM    TSERRS,TSERNF       NOT FOUND?                                   
         BNO   *+6                                                              
         DC    H'0'                                                             
         B     BUFCL3                                                           
*                                                                               
BUFCL5   MVI   TSOFFACT,TSAINI     INIT AGAIN                                   
         MVC   TSAREC,=A(CONLN)    SIZE OF BUFFER                               
         GOTO1 TSAROFF                                                          
         B     BUFCXY                                                           
*                                                                               
BUFCXN   LTR   RB,RB                                                            
         B     *+6                                                              
*                                                                               
BUFCXY   CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R1,R6                                                            
         EJECT                                                                  
LPARM    DS    6F                                                               
LBUFR    DS    CL(CONSLNQ)                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE TOTAL HOURS RECORDS FOR PERSON                              *         
***********************************************************************         
         SPACE 1                                                                
TOTH     DS    0D                                                               
         NMOD1 0,*TOTH*,R9                                                      
         L     RC,BASERC                                                        
         L     R5,AMANWK           CLEAR ARRAY HIT SWITCHES                     
         USING MNWKD,R5                                                         
         LH    R2,MNWKS            NUMBER OF WEEKS                              
         MVI   MNWKHIT,C' '                                                     
         ZAP   MNWKHRS,=P'0'                                                    
         LA    R5,MNWKLNQ(R5)                                                   
         BCT   R2,*-14                                                          
         TM    RQSW,RQFLT          FILTERING BY CONTRA - TIME TOTALS            
         BNO   TOTX                                                             
*                                                                               
         USING CACRECD,R6                                                       
TOTH7    LA    R6,DKEY             GET NON-CLIENT HISTORIES                     
         XC    CACKEY,CACKEY                                                    
         MVC   CACKCULA,ACTACC     ACCOUNT                                      
         MVC   CACKOFF,SPACES                                                   
         MVC   CACKCCPY,CACKCPY    COMPANY(IN CONTRA)                           
         MVC   CACKCUNT(2),NONC    NON-CLIENT LEDGER                            
         GOTO1 ADMGR,HIGH                                                       
*                                                                               
TOTH9    CLC   DKEY(CACKCACT-CACRECD),DIR                                       
         BNE   TOTH25              FINISHED NON-CLIENT                          
         LA    R6,DIR                                                           
         CLC   CACKSPAC,SPACES     TEST CONTRA RECORD                           
         BH    TOTH19                                                           
         CLI   CACKBTYP,C'H'       TEST HOURS                                   
         BNE   TOTH17                                                           
         BAS   RE,NOCLI            GET NON-CLIENT TYPE                          
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         LA    R6,CACRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING BUKELD,R6                                                        
TOTH11   CLI   BUKEL,BUKELQ        TEST HISTORY ELEMENT                         
         BNE   TOTH15                                                           
         MVC   MOA,BUKMOS                                                       
         ZAP   HOURS,BUKCR                                                      
         BAS   RE,TOTP             POST THE TIME TO ACCUM LINES                 
*                                                                               
TOTH15   IC    R0,BUKLN                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   TOTH11                                                           
*                                                                               
TOTH17   GOTO1 ADMGR,RSEQ          READ NEXT CONTRA FOR HOURS                   
         B     TOTH9                                                            
*                                                                               
TOTH19   MVC   DKEY,DIR            SKIP TO NEXT CONTRA ACCOUNT                  
         LA    R6,DKEY                                                          
         USING CACRECD,R6                                                       
         SR    R1,R1                                                            
         IC    R1,CACKCACT+L'CACKCACT-1                                         
         LA    R1,1(R1)                                                         
         STC   R1,CACKCACT+L'CACKCACT-1                                         
         XC    CACKSPAC(6),CACKSPAC                                             
         GOTO1 ADMGR,HIGH                                                       
         B     TOTH9                                                            
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET AND POST PERIOD AND CLIENT TOTALS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TTHRECD,R6                                                       
TOTH25   LA    R6,DKEY                                                          
         XC    TTHKEY,TTHKEY                                                    
         MVI   TTHKTYP,TTHKTYPQ    TOTALS RECORD                                
         MVI   TTHKSUB,TTHKSUBQ                                                 
         MVC   TTHKCULA,ACTACC     ACCOUNT                                      
         MVI   TTHKTIME,TTHKTTOT                                                
         GOTO1 ADMGR,HIGH                                                       
*                                                                               
TOTH27   CLC   DKEY(TTHKEMOA-TTHRECD),DIR                                       
         BNE   TOTX                FINISHED ALL                                 
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         LA    R6,TTHRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING PTHELD,R6                                                        
TOTH31   CLI   PTHEL,PTHELQ        TEST CLIENT/TOTAL LESS ADJUSTED              
         BE    *+8                                                              
         CLI   PTHEL,PTHELQA       TEST CLIENT/TOTAL ADJUSTED                   
         BNE   TOTH35                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PTHLN                                                         
         SH    R1,=Y(PTHBLNQ)                                                   
         SRL   R1,3                DIVIDE BY 8 TO GET # OF SUBELEMS             
         LA    RF,PTHMOA                                                        
*                                                                               
         USING PTHMOA,RF                                                        
         MVC   PRDDATE,PTHEDT                                                   
TOTH32   MVC   MOA,PTHMOA                                                       
         ZAP   HOURS,PTHCHRS       CLIENT HOURS                                 
         MVI   TYPET,CLITQ                                                      
         BAS   RE,TOTP                                                          
         CLI   PTHEL,PTHELQA       TEST CLIENT/TOTAL ADJUSTED                   
         BE    TOTH33                                                           
         ZAP   HOURS,PTHTHRS       TOTAL HOURS                                  
         BAS   RE,TOTM             POST WEEK ARRAY                              
TOTH33   LA    RF,L'PTHSLNQ(RF)                                                 
         BCT   R1,TOTH32                                                        
*                                                                               
TOTH35   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   TOTH31                                                           
         GOTO1 ADMGR,RSEQ                                                       
         B     TOTH27                                                           
*                                                                               
TOTX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*&&DO                                                                           
*---------------------------------------------------------------------          
         USING TTHRECD,R6                                                       
TOTH25   LA    R6,DKEY                                                          
         XC    TTHKEY,TTHKEY                                                    
         MVI   TTHKTYP,TTHKTYPQ    TOTALS RECORD                                
         MVI   TTHKSUB,TTHKSUBQ                                                 
         MVC   TTHKCULA,ACTACC     ACCOUNT                                      
         GOTO1 ADMGR,HIGH                                                       
*                                                                               
TOTH27   CLC   DKEY(TTHKTIME-TTHRECD),DIR                                       
         BNE   TOTX                FINISHED ALL                                 
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         LA    R6,TTHRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING PTHELD,R6                                                        
TOTH31   CLI   PTHEL,PTHELQ        TEST PERIOD HOURS                            
         BNE   TOTH33                                                           
         BAS   RE,TOTM             POST WEEK ARRAY                              
         B     TOTH35                                                           
*                                                                               
         USING MCHELD,R6                                                        
TOTH33   CLI   MCHEL,MCHELQ        TEST CLIENT HOURS                            
         BNE   TOTH35                                                           
         MVI   TYPET,CLITQ                                                      
         MVC   MOA,MCHMOA                                                       
         ZAP   HOURS,MCHHRS                                                     
         BAS   RE,TOTP             POST THE TIME TO ACCUM LINES                 
*                                                                               
TOTH35   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   TOTH31                                                           
         GOTO1 ADMGR,RSEQ                                                       
         B     TOTH27                                                           
*                                                                               
TOTX     XIT1                                                                   
         DROP  R6                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* POST TO ACCUMULATOR LINES                                           *         
***********************************************************************         
         SPACE 1                                                                
TOTP     STM   RE,R6,SVRE                                                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   MOA,ACMHSTR                                                      
         BL    TOTPX                                                            
         CLC   MOA,ACMHEND                                                      
         BH    TOTPX                                                            
         MVC   DUB,=2PL4'0'        GET RELATIVE NUMBER FOR THIS MONTH           
         DROP  RF                                                               
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R0,MOA              YEAR                                         
         SRDL  R0,4                R0=DECADE, R1=YEAR                           
         MH    R0,=H'10'           X'A0' BECOMES 100                            
         SRL   R1,28               YEAR TO LOW ORDER                            
         AR    R0,R1               ADD YEAR                                     
         MH    R0,=H'12'                                                        
         IC    R1,MOA+1            MONTH                                        
         CLI   MOA+1,X'09'         CONVERT MONTH TO BINARY                      
         BNH   *+8                                                              
         SH    R1,=H'6'                                                         
         AR    R0,R1                                                            
         ICM   R1,7,BASMNTH        BASE MONTH NUMBER                            
         SR    R0,R1                                                            
         STC   R0,RELMON           SAVE RELATIVE MONTH NUMBER                   
         LR    R3,R0                                                            
         SLL   R3,3                X WIDTH OF ACCUMULATOR                       
         LA    R2,TOTLNS                                                        
*                                                                               
TOTP3    SR    R1,R1                                                            
         IC    R1,TYPET                                                         
         EX    R1,*+8              TEST HOURS POSTED TO THIS LINE               
         B     *+8                                                              
         TM    1(R2),0                                                          
         BNO   TOTP5                                                            
         ICM   R1,1,0(R2)          DISPLACEMENT TO LINE                         
         LR    RE,R1                                                            
         A     RE,ALNSTL                                                        
         OI    0(RE),LSACT         SET ACTIVITY SWITCH                          
         MH    R1,WLINE                                                         
         A     R1,AZRO             START OF ACCUMS                              
         AR    R1,R3               PLUS DISPLACEMENT TO COLUMN                  
         AP    0(PFL,R1),HOURS     ADD HOURS TO BUCKET                          
*                                                                               
TOTP5    LA    R2,L'TOTLNS(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   TOTP3                                                            
TOTPX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* POST TOTALS TO MANWEEK ARRAYS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING PTHELD,R6                                                        
TOTM     STM   RE,R6,SVRE                                                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   MOA,PEDTE           SKIP INPUT AFTER END DATE                    
         BH    TOTMX                                                            
         L     R5,AMANWK           WEEK DATE ARRAY                              
         USING MNWKD,R5                                                         
         LH    RF,MNWKS            NUMBER OF ENTRIES                            
         CLC   PRDDATE,MNWKSTR     TEST BEFORE START                            
         BL    TOTMX               SKIP IT                                      
*                                                                               
TOTM3    CLC   PRDDATE,MNWKEND     IS TRANSACTION PAST DATE IN ARRAY            
         BH    *+18                DID NOT WORK THIS WEEK                       
         MVI   MNWKHIT,C'H'        MARK A HIT                                   
         AP    MNWKHRS,HOURS       ADD THE HOURS                                
         B     TOTMX                                                            
         LA    R5,MNWKLNQ(R5)      BUMP TO NEXT DATE IN ARRRAY                  
         BCT   RF,TOTM3            AND CHECK TRANSACTION AGAINST IT             
TOTMX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
*&&DO                                                                           
*--------------------------------------------------------------------           
         SPACE 1                                                                
         USING PTHELD,R6                                                        
TOTM     STM   RE,R6,SVRE                                                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   PTHMOA,PEDTE        SKIP INPUT AFTER END DATE                    
         BH    TOTMX                                                            
         L     R5,AMANWK           WEEK DATE ARRAY                              
         USING MNWKD,R5                                                         
         LH    RF,MNWKS            NUMBER OF ENTRIES                            
         CLC   PTHEDT,MNWKSTR      TEST BEFORE START                            
         BL    TOTMX               SKIP IT                                      
*                                                                               
TOTM3    CLC   PTHEDT,MNWKEND      IS TRANSACTION PAST DATE IN ARRAY            
         BH    *+18                DID NOT WORK THIS WEEK                       
         MVI   MNWKHIT,C'H'        MARK A HIT                                   
         AP    MNWKHRS,PTHMHRS     ADD THE HOURS                                
         B     TOTMX                                                            
         LA    R5,MNWKLNQ(R5)      BUMP TO NEXT DATE IN ARRRAY                  
         BCT   RF,TOTM3            AND CHECK TRANSACTION AGAINST IT             
TOTMX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R5,R6                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SET TYPE FOR NON-CLIENT TIME                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CACRECD,R6                                                       
NOCLI    MVI   CURRNT,C'N'                                                      
         TM    RNSW,RNNC           NEW COSTING                                  
         BNO   NOCLI07                                                          
         L     R3,APELVT           TABLE OF PE AND LEAVE ACCOUNTS               
*                                                                               
NOCLI05  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    NOCLIX                                                           
         CLC   CACKULC,0(R3)       MATCH CONTRA                                 
         BNE   *+14                                                             
         MVC   CURRNT,14(R3)       SET TYPE FROM TABLE                          
         B     NOCLIX                                                           
         LA    R3,PETLN(R3)                                                     
         B     NOCLI05                                                          
*                                                                               
NOCLI07  MVC   CURRNT,CACKACT      TYPE IS FROM ACCOUNT CODE                    
NOCLIX   MVI   TYPET,OTHTQ         DEFAULT IS OTHER NON-CLIENT                  
         CLI   CURRNT,C'L'         LEAVE                                        
         BNE   *+8                                                              
         MVI   TYPET,LVETQ                                                      
         CLI   CURRNT,C'P'         PERSONAL                                     
         BNE   *+8                                                              
         MVI   TYPET,PERTQ                                                      
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
NONC     DC    C'1N'                                                            
*                                                                               
PRDDATE  DS    XL3                 PERIOD DATE                                  
MOA      DS    XL2                 YEAR AND MONTH                               
HOURS    DS    PL8                 HOURS                                        
*                                                                               
TYPET    DS    X                       TYPE OF TIME                             
CLITQ    EQU   X'80'                   CLIENT TIME                              
LVETQ    EQU   X'40'                   LEAVE TIME                               
PERTQ    EQU   X'20'                   PERSONAL                                 
OTHTQ    EQU   X'10'                   OTHER NON-CLIENT                         
ALLTQ    EQU   X'FF'                   ALL TIME                                 
ANLVQ    EQU   (ALLTQ-LVETQ)           ALL EXCEPT LEAVE                         
ANLPQ    EQU   (ALLTQ-LVETQ-PERTQ)     ALL EXCEPT LEAVE & PERSONAL              
*                                                                               
TOTLNS   DS    0XL2                                                             
         DC    AL1(LNCLT),AL1(CLITQ)    CLIENT TIME                             
         DC    AL1(LNTLT),AL1(ALLTQ)    TOTAL TIME                              
         DC    AL1(LNTLV),AL1(ANLVQ)    ALL EXCEPT LEAVE                        
         DC    AL1(LNTLP),AL1(ANLPQ)    ALL EXCEPT LEAVE AND PERSONAL           
         DC    AL2(0)                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
DMGR     DS    0D                                                               
         NMOD1 0,*DMGR*,R9                                                      
         L     RC,BASERC                                                        
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     DMGRSEQ                                                          
         B     DMGREAD                                                          
         B     DMGHIGH                                                          
         B     DMGGETR                                                          
*                                                                               
DMGRSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         B     DMXIT                                                            
*                                                                               
DMGREAD  GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         CLC   DIR,DKEY                                                         
         B     DMXIT                                                            
*                                                                               
DMGHIGH  GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         CLC   DIR,DKEY                                                         
         B     DMXIT                                                            
*                                                                               
DMGGETR  GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO1,DMWORK                        
DMXIT    XIT1                                                                   
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'SUBROUTINES - POST SALARY DETAILS'                              
***********************************************************************         
* POST SALARY DETAILS                                                 *         
***********************************************************************         
         SPACE 1                                                                
PSD      DS    0D                                                               
         NMOD1 0,**PSDH**,R9                                                    
         L     RC,BASERC                                                        
         USING SLRD,R5                                                          
         L     R2,ADACC            A(ACCOUNT RECORD)                            
         LA    R3,MONLIST+4        MONTH LIST                                   
         SR    R4,R4               ACCUM DISPLACEMENT                           
         LA    R0,24               ONLY NEED PRIOR AND CURRENT YEAR             
         LA    R5,SALHBLK                                                       
*                                                                               
PSD3     MVC   DUB(2),0(R3)        BUILD YM-YM                                  
         MVC   DUB+2(2),DUB                                                     
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),DUB,(R5),ADCOMFAC                       
         LA    R1,SLRSAL           BASIC                                        
         LH    R6,=Y(LNSAL*WLN)    LINE 21                                      
         LA    RF,LNSAL                                                         
         BAS   RE,PSD99                                                         
*                                                                               
         ZAP   WRK,0(PFL,R6)       BASIC                                        
         SRP   WRK,3,0             SHIFT LEFT 3                                 
         DP    WRK,=PL3'13333'                                                  
         SRP   WRK(12),64-1,5      SHIFT RIGHT 1 AND ROUND                      
         LH    R6,=Y(LNSTR*WLN)    STANDARD RATE                                
         LA    RF,LNSTR                                                         
         A     RF,ALNSTL                                                        
         A     R6,AZRO                                                          
         AR    R6,R4               ADD DISPLACEMENT TO START                    
         ZAP   0(PFL,R6),WRK(12)                                                
         CP    0(PFL,R6),=P'0'     ANY BILLING RATE                             
         BE    *+12                                                             
         OI    ACSW,ACHR           SET HOURLY RATE SWITCH                       
         OI    0(RF),LSACT         SET ACTIVITY IN STATUS LINE                  
*                                                                               
*&&US                                                                           
         LA    R1,SLROVT           OVERTIME                                     
         LH    R6,=Y(LNOVT*WLN)    LINE 22                                      
         LA    RF,LNOVT                                                         
         BAS   RE,PSD99                                                         
         LA    R1,SLRTMP           TEMPORARY                                    
         LH    R6,=Y(LNTMP*WLN)    LINE 23                                      
         LA    RF,LNTMP                                                         
         BAS   RE,PSD99                                                         
         LA    R1,SLRBON           BONUS                                        
         LH    R6,=Y(LNBON*WLN)    LINE 24                                      
         LA    RF,LNBON                                                         
         BAS   RE,PSD99                                                         
*                                                                               
         LA    R1,SLRRTE           BILLING RATE                                 
         LH    R6,=Y(LNBLR*WLN)    LINE 56                                      
         LA    RF,LNBLR                                                         
         BAS   RE,PSD99                                                         
         CP    0(PFL,R6),=P'0'     ANY BILLING RATE                             
         BE    *+8                                                              
         OI    ACSW,ACBR           SET BILLING RATE SWITCH                      
*&&                                                                             
         LA    R3,6(R3)            NEXT DATE                                    
         LA    R4,8(R4)            NEXT COLUMN                                  
         BCT   R0,PSD3                                                          
*                                                                               
PSD10    TM    RNSW,RNNC           NEW COSTING                                  
         BNO   PSDXIT                                                           
                                                                                
*---------------------------------------------------------------------*         
*        LOOP TO READ AND ACCUMULATE SALARY HISTORY FROM $COST                  
*---------------------------------------------------------------------*         
                                                                                
         L     RF,ADLDGHIR                                                      
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
*                                                                               
PSD12    ZIC   R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),12                    LAST LEV HAS MAXLEN FOR ACCT         
         BE    PSD15                                                            
         LA    R2,1(R2)                    ADD TO LEVEL COUNT                   
         ZIC   R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,PSD12                                                         
         DC    H'0'                                                             
PSD15    STC   R2,NUMLEVLS                 ACTUAL NUMBER OF LEVELS              
                                                                                
         USING SALARYD,R5                                                       
         LA    R5,SALHBLK2                                                      
                                                                                
         CLI   INITSW,C'Y'                                                      
         BE    PSD18                                                            
                                                                                
         XC    SALHBLK2,SALHBLK2                                                
         MVC   SALACOVL,COVAIL                                                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVC   SALMETHD,METHOD                                                  
         MVC   SALLEVS,LENLEVLS                                                 
                                                                                
PSD18    MVC   SALOFFC,SPACES                                                   
         L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF OFFICE                             
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*MN      EX    R1,*+4                                                           
         MVC   SALOFFC(0),3(R2)     SAVE OFFICE FOR SALHST                      
         EX    R1,*-6                                                           
*                                                                               
         MVC   SALDEPT,SPACES                                                   
         L     R2,ADHEIRB                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF OFFICE                             
         AR    R2,R1               R2 NOW POINTS AT DEPT                        
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF DEPT                               
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*MN      EX    R1,*+4                                                           
         MVC   SALDEPT(0),0(R2)     SAVE DEPT FOR PERCALL                       
         EX    R1,*-6                                                           
*                                                                               
         MVC   SALSDPT,SPACES                                                   
         L     R2,ADHEIRC                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVB            LENGTH OF OFFICE/DEPT                        
         AR    R2,R1               R2 NOW POINTS AT SUBDEPT                     
         SR    R1,R1                                                            
         IC    R1,LENLEVC          LENGTH OF SUBDEPT                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*MN      EX    R1,*+4                                                           
         MVC   SALSDPT(0),0(R2)     SAVE SUBDEPT                                
         EX    R1,*-6                                                           
*                                                                               
         MVC   SALPRSN,SPACES                                                   
         L     R2,ADHEIRD                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVC            LENGTH OF OFFICE/DEPT/SUBDPT                 
         AR    R2,R1               R2 NOW POINTS AT PERSON                      
         SR    R1,R1                                                            
         IC    R1,LENLEVD          LENGTH OF PERSON                             
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*MN      EX    R1,*+4                                                           
         MVC   SALPRSN(0),0(R2)    SAVE PERSON FOR PERCALL                      
         EX    R1,*-6                                                           
                                                                                
         LA    R3,MONLIST+4        MONTH LIST                                   
         SR    R4,R4               ACCUM DISPLACEMENT                           
         LA    R0,24               ONLY NEED PRIOR AND CURRENT YEAR             
                                                                                
PSD30    LA    R2,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),=P'0'                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R2,*-10                                                          
                                                                                
         MVC   SALSTART(2),0(R3)                                                
         MVI   SALSTART+2,X'01'                                                 
         GOTO1 DATCON,DMCB,(X'31',SALSTART),(1,SALEND),(1,SALEND)               
                                                                                
         LA    R5,SALHBLK2                                                      
         GOTO1 ACSALHST,DMCB,ACWORKD,(R5),ASALHIO                               
         USING SALARYD,R5                                                       
         LA    R5,SALHBLK2                                                      
         CLI   SALSTAT2,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   INITSW,C'Y'                                                      
                                                                                
         CP    SALSALRY,=P'0'                                                   
         BE    PSD40                                                            
         LA    R1,SALSALRY                                                      
                                                                                
         LH    R6,=Y(LNSAL*WLN)                                                 
         LA    RF,LNSAL                                                         
         A     R6,AZRO                                                          
         AR    R6,R4               ADD DISPLACEMENT TO START                    
         ZAP   0(PFL,R6),0(6,R1)                                                
         A     RF,ALNSTL                                                        
         OI    0(RF),LSACT         SET LINE ACTIVITY                            
                                                                                
         LH    R6,=Y(LNHIST*WLN)                                                
         LA    RF,LNHIST                                                        
         A     R6,AZRO                                                          
         AR    R6,R4               ADD DISPLACEMENT TO START                    
         ZAP   0(PFL,R6),0(6,R1)                                                
         A     RF,ALNSTL                                                        
         OI    0(RF),LSACT         SET LINE ACTIVITY                            
                                                                                
PSD40    LA    R3,6(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R0,PSD30                                                         
         B     PSDXIT                                                           
*                                                                               
PSDXIT   XIT1                                                                   
*                                                                               
*                                                                               
PSD99    A     R6,AZRO                                                          
         AR    R6,R4               ADD DISPLACEMENT TO START                    
         ZAP   0(PFL,R6),=P'0'                                                  
         TM    6(R1),X'20'         DONT WANT PERCENTS                           
         BOR   RE                                                               
         ZAP   0(PFL,R6),0(6,R1)                                                
         CP    0(PFL,R6),=P'0'                                                  
         BER   RE                                                               
         A     RF,ALNSTL                                                        
         OI    0(RF),LSACT         SET LINE ACTIVITY                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
ACSALHST DC    V(ACSALHST)                                                      
COVAIL   DC    V(COVAIL)                                                        
ASALHIO  DC    A(SALHIO)                                                        
INITSW   DC    C'N'                                                             
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
NUMLEVLS DS    XL1        NUMBER OF LEVELS IN 1R                                
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1        REAL LENGTH OF LEVEL A                                
LENLEVB  DS    XL1        REAL LENGTH OF LEVEL B                                
LENLEVC  DS    XL1        REAL LENGTH OF LEVEL C                                
LENLEVD  DS    XL1        REAL LENGTH OF LEVEL D                                
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
*                                                                               
         DS    0D                                                               
SALHBLK  DS    CL(SALLNQ)                                                       
*                                                                               
         DS    0D                                                               
SALHBLK2 DS    CL(SALLNQ)                                                       
*                                                                               
         DS    0D                                                               
         DS    CL8'*SALHIO*'                                                    
SALHIO   DS    CL2000                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCB FOR THE WORK FILE                                               *         
***********************************************************************         
         SPACE 1                                                                
BUFFWK   DCB   DDNAME=BUFFWK,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=354,                                              X        
               BLKSIZE=7080,                                           X        
               MACRF=(PM,GM),                                          X        
               BUFNO=2,                                                X        
               EODAD=(BUFC99)                                                   
*                                                                               
ENTRYTAB DC    AL2(CNL-ACM205),AL2(ACNL-MAND)                                   
         DC    AL2(SRCH-ACM205),AL2(ASRCH-MAND)                                 
         DC    AL2(DWNL-ACM205),AL2(ADWNL-MAND)                                 
         DC    AL2(BUFC-ACM205),AL2(ABUFC-MAND)                                 
         DC    AL2(TOTH-ACM205),AL2(ATOTH-MAND)                                 
         DC    AL2(DMGR-ACM205),AL2(ADMGR-MAND)                                 
         DC    AL2(PSD-ACM205),AL2(APSD-MAND)                                   
         DC    AL2(BUFFWK-ACM205),AL2(ABUFFWK-MAND)                             
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DSECT TO COVER ACSALHST BLOCK                               *          
*--------------------------------------------------------------------*          
SALARYD  DSECT                                                                  
SALBLOCK DS    0C                                                               
SALBLOK  DS    0C                                                               
         DS    CL8               SET TO **SALB** FOR DUMPS                      
         DS    XL8               ADDR AND LENGTH OF ACQUIRED STORAGE            
         DS    XL84              RESERVED FOR SALHST                            
SALRESLN EQU   *-SALBLOK         LEN OF RESERVED AREA                           
*                                                                               
SALCMPY  DS    XL1               * COMPANY                        (REQ)         
SALOFFC  DS    CL2               * OFFICE                         (REQ)         
SALDEPT  DS    CL3               * DEPT                           (REQ)         
SALSDPT  DS    CL3               * SUBDEPT                        (REQ)         
SALPRSN  DS    CL8               * PERSON                         (REQ)         
SALMETHD DS    CL1               * METHOD                         (REQ)         
SALSTART DS    PL3               * START - YYMMDD                 (REQ)         
SALEND   DS    PL3               * END   - YYMMDD                 (REQ)         
SALLEVS  DS    0XL4              * 1R LEVEL LENGTHS EX/ 1,2,2,7   (REQ)         
SALLEVA  DS    XL1                                                              
SALLEVB  DS    XL1                                                              
SALLEVC  DS    XL1                                                              
SALLEVD  DS    XL1                                                              
*                                                                               
SALACOVL DS    A                 A(COVAIL) FOR OFFLINE OPTIMIZATION             
*                                                                               
SALACTM1 DS    0PL4                                                             
         DS    12PL(L'SALACTM1)  * ACTUAL HRS BY MONTH FOR STANDARD             
SALACTM# EQU   (*-SALACTM1)/(L'SALACTM1)         NUMBER OF MONTHS               
SALFILT  DS    XL1               * PAYROLL TYPE FILT OR X'00'     (OPT)         
SALSTAT1 DS    XL1               * HOURS STATUS BYTE OR X'00'     (OPT)         
SALSTAND EQU   X'01'               - CALCULATE BASED ON STANDARD HOURS          
SALCMPHR EQU   X'02'               - COMPOSITE HOURS BEING RETURNED             
SALHRRAT EQU   X'04'               - INCLUDE HOURLY RATES                       
SALHRRTO EQU   X'08'               - ONLY HOURLY RATES                          
SALDISTR EQU   X'10'               - REDISTRIBUTE SAL BASED ON PCT              
*                                    OF TIME IN EACH LOCATION FOR MTH           
*                                                                               
SALSTAT2 DS    XL1                 RETURN STATUS FROM ACSALHST                  
SALOK    EQU   X'00'               NO ERROR                                     
SALINVDT EQU   X'01'               INVALID DATE RANGE                           
SALINVMT EQU   X'02'               NO METHOD RECORD                             
SALINV85 EQU   X'04'               NO PAYROLL CODE ELEMENTS X'85'               
SALINVCV EQU   X'08'               A(COVAIL) NOT PASSED                         
SALINVPR EQU   X'10'               MISSING PERSON RECORD                        
SALINV56 EQU   X'20'               MISSING X'56' ELEM - EMPLOYEE HIST           
SALINV1R EQU   X'40'               1R LEVELS NOT PASSED                         
SALINVKY EQU   X'80'               MISSING LEVELS OF KEY                        
*                                                                               
SALINFO  DS    0PL6                SALARY DOLLAR ACCUMULATORS                   
SALSALRY DS    PL(L'SALINFO)       SALARY                                       
SALBENFT DS    PL(L'SALINFO)       BENEFIT                                      
SALPENSN DS    PL(L'SALINFO)       PENSION                                      
SALINDIR DS    PL(L'SALINFO)       INDIRECT                                     
SALOTHER DS    PL(L'SALINFO)       OTHER                                        
SALTOTAL DS    PL(L'SALINFO)       TOTAL FOR PERIOD                             
SALINFO# EQU   (*-SALINFO)/(L'SALINFO)  # ACCUMS                                
SALSTDHR DS    PL(L'SALINFO)       TOTAL YTD STANDARD HOURS                     
SALSTDPC DS    PL(L'SALINFO)       YTD ACTUAL TO YTD STD PERCENTAGE             
SALDPTPC DS    PL(L'SALINFO)       PERCENTAGE OF TIME IN THIS DEPT              
SALACUM# EQU   (*-SALINFO)/(L'SALINFO) TOTAL # ACCUMS                           
SALLNQ   EQU   *-SALARYD                                                        
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE ACAPGGEND                                                      
         PRINT ON                                                               
* DDDLCB                                                                        
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACREPM205 01/28/13'                                      
         END                                                                    
