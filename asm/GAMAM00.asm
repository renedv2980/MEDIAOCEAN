*          DATA SET GAMAM00    AT LEVEL 016 AS OF 05/01/02                      
*PHASE TB0D00A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'GAMAM00-MAMMALS'                                                
         PRINT NOGEN                                                            
MAMMALS  CSECT                                                                  
         NMOD1 0,**MAMM**,R9,RR=R7                                              
         L     RA,4(R1)                                                         
         USING MAMSCRN,RA          TWA                                          
*                                                                               
         L     RF,16(R1)                                                        
         L     RF,4(RF)            RF GETS A(CALLOV)                            
         ST    RF,ACALLOV                                                       
*                                                                               
         GOTO1 ACALLOV,PARM,(1,0),(0,0),0                                       
*                                                                               
         CLI   PARM+4,X'FF'        TEST NOT FOUND                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,PARM                                                          
         LA    R8,0(R8)                                                         
         ST    R8,ATABLES                                                       
*                                                                               
         L     R1,0(R8)                                                         
         LA    R1,0(R1,R8)                                                      
         ST    R1,ATYPH                                                         
*                                                                               
         L     R2,4(R8)                                                         
         LA    R2,0(R2,R8)                                                      
         ST    R2,ARATH                                                         
*                                                                               
         L     RF,=V(RANDOM)                                                    
         AR    RF,R7                                                            
         ST    RF,VRANDOM                                                       
*                                                                               
         ST    RB,ABASE            STORE RB AT ABASE                            
*                                                                               
         BAS   RE,RELOC            RESOLVE TABLE ADDRESSES                      
*                                                                               
         LA    R1,ASKQN                                                         
         ST    R1,AASKQN                                                        
*                                                                               
         LA    R1,FIXSCRN                                                       
         ST    R1,AFIXSCRN                                                      
*                                                                               
         LA    R1,CNAMES                                                        
         ST    R1,ACNAMES                                                       
*                                                                               
         LA    R1,COMPARE                                                       
         ST    R1,ACMPARE                                                       
*                                                                               
         LA    R1,INSERT                                                        
         ST    R1,AINSERT                                                       
*                                                                               
         LA    R1,RELOC                                                         
         ST    R1,ARELOC                                                        
*                                                                               
         B     START                                                            
*                                                                               
         EJECT                                                                  
START    CLI   STAGE,X'00'                                                      
         BNE   ST10                                                             
         CLI   MAMAN01,C'A'                                                     
         BE    ST20                                                             
         CLI   MAMAN01,C'B'                                                     
         BE    ST30                                                             
         XC    MAMAN01,MAMAN01     HE HASN'T SAID WHAT HE WANTS TO DO           
         B     MAMEXIT                                                          
*                                                                               
ST10     CLI   STAGE,X'B0'                                                      
         BNL   ST30                HE IS PLAYING VERSION B                      
*                                                                               
ST20     OI    STAGE,X'A0'                                                      
         CLI   STAGE,X'A0'                                                      
         BE    FIRST                                                            
         CLI   STAGE,X'A1'                                                      
         BE    MIDDLE                                                           
         CLI   STAGE,X'A2'                                                      
         BE    CHECK                                                            
         CLI   STAGE,X'A3'                                                      
         BE    FINAL                                                            
*                                                                               
ST30     DS    0H                                                               
         L     R2,ATABLES                                                       
         L     R1,8(R2)            DISPLACEMENT TO END OF TABLES                
         LA    R2,0(R1,R2)                                                      
*                                                                               
         GOTO1 ACALLOV,PARM,(2,(R2)),(0,0),0                                    
*                                                                               
*                                                                               
         CLI   PARM+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,PARM             RF GETS A(VERSION B)                         
*                                                                               
         OI    STAGE,X'B0'                                                      
*                                                                               
         BASR  RE,RF                                                            
*                                                                               
         B     MAMEXIT                                                          
*                                                                               
         EJECT                                                                  
FIRST    XC    CANDLST,CANDLST                                                  
         XC    GUESSES(23),GUESSES      CLEARS ANS,DKNOWS,NOQNS,QNNUM,          
*                                       & NUMQNS.                               
         XC    IND,IND                                                          
         L     R1,ATYPH                                                         
         MVC   STTABH,0(R1)                                                     
*                                                                               
         BAS   RE,RELOC                                                         
*                                                                               
         MVI   NOQNS,X'14'         SET NO OF QNS LEFT TO TWENTY                 
*                                                                               
         MVI   GUESSES,X'03'       SET UP TO MAKE 3 1ST QNS RANDOM              
         TWAXC MAMTOP2H,MAMBOT4H,PROT=Y                                         
         MVC   MAMBOT1(L'MESSA),MESSA                                           
         MVC   MAMBOT2(L'MESSB),MESSB                                           
         MVC   MAMBOT3(L'MESSC),MESSC                                           
         MVC   MAMBOT4(L'MESSD),MESSD                                           
*                                                                               
         XC    MAMTOP1,MAMTOP1                                                  
         MVC   MAMTOP1(L'MESS1),MESS1                                           
*                                                                               
         MVI   STAGE,X'A1'                                                      
         B     MIDDLE                                                           
         EJECT                                                                  
MIDDLE   XC    MAMTOP2,MAMTOP2     CLEAR ANY OLD MESSAGES                       
*                                                                               
         OC    MAMQN01,MAMQN01                                                  
         BZ    FINDQN                                                           
         OC    IND,IND                                                          
         BNZ   *+12                                                             
         LA    R3,MAMAN01                                                       
         B     *+8                                                              
         LA    R3,MAMAN02                                                       
         XC    IND,IND                                                          
         OC    0(1,R3),0(R3)                                                    
         BZ    MAMAXIT             DONT DO ANYTHING IF HE DOESN'T ANSR          
*                                                                               
         LA    R1,ANSWER                                                        
         ZIC   R2,QNNUM                                                         
         BCTR  R2,0                                                             
         LA    R1,0(R2,R1)         R1 POINTS TO POSN OF QN IN ANSWER            
*                                                                               
         CLI   0(R3),C'Y'                                                       
         BNE   ANSNO                                                            
         MVI   0(R1),Y                                                          
         B     FINDQN                                                           
*                                                                               
ANSNO    CLI   0(R3),C'N'                                                       
         BNE   ANSDKNOW                                                         
         MVI   0(R1),N                                                          
         B     FINDQN                                                           
*                                                                               
ANSDKNOW CLI   0(R3),C'X'                                                       
         BNE   MID10                                                            
         MVI   0(R1),D                                                          
         ZIC   R3,DKNOWS                                                        
         LA    R3,1(R3)                                                         
         STC   R3,DKNOWS                                                        
         CLI   DKNOWS,X'03'                                                     
         BE    HELOSES                                                          
         B     FINDQN                                                           
*                                                                               
MID10    XC    MAMAN01,MAMAN01                                                  
         B     MAMAXIT                                                          
*                                                                               
*   ROUTINE DECIDES WHAT SORT OF QN TO ASK.                                     
*                                                                               
FINDQN   XC    SWITCH,SWITCH                                                    
         OC    NOQNS,NOQNS         EVEN IF I'VE RUN OUT OF QNS I STILL          
         BZ    GOODQN              WANT TO KNOW HOW MANY CANDIDATES             
*                                  THERE ARE.                                   
*                                                                               
         CLI   GUESSES,X'00'                                                    
         BE    GOODQN                                                           
*                                                                               
         EJECT                                                                  
RANDQN   ZIC   R1,NUMQNS           FINDS A RANDOM UNASKED QUESTION              
         L     R2,LEN              PUTS NO QNS IN R2                            
         LA    R3,ANSWER           R3,R4,R5 SET UP FOR BXLE ALONG               
         LA    R4,1                ANSWER                                       
         LR    R5,R2                                                            
         BCTR  R5,0                                                             
         LA    R5,0(R5,R3)         R5 PTS TO LAST POSSIBLE ANSWER ENTRY         
         LR    R6,R2               R6 GETS NO OF QNS REMAINING MINUS 1          
         BCTR  R6,0                                                             
         SR    R6,R1                                                            
         BZ    RAND05                                                           
         BM    RAND30                                                           
         GOTO1 VRANDOM,PARM,(R6)                                                
         L     R6,PARM+4                                                        
*                                                                               
RAND05   SR    R2,R2                                                            
*                                                                               
RAND10   CLI   0(R3),X'00'         HAS THIS QN BEEN ASKED?                      
         BNE   RAND20              YES-GO TO BXLE                               
         CR    R2,R6                                                            
         BE    RAND40              R2=R6 AND R3 MUST BE POINTING TO AN          
         LA    R2,1(R2)            UNASKED QN                                   
RAND20   BXLE  R3,R4,RAND10                                                     
*                                                                               
RAND30   XC    GUESSES,GUESSES     THERE ARE NO QNS LEFT TO ASK.                
         MVI   SWITCH,X'01'                                                     
         B     GOODQN                                                           
*                                                                               
RAND40   LA    R2,ANSWER                                                        
         SR    R3,R2               R3 GETS NO OF QN TO BE ASKED MINUS 1         
*                                                                               
RAND50   ZIC   R2,GUESSES                                                       
         BCTR  R2,0                                                             
         STC   R2,GUESSES                                                       
         LA    R3,1(R3)                                                         
         STC   R3,QNNUM                                                         
         BAS   RE,ASKQN                                                         
*                                                                               
         ZIC   R1,NOQNS                                                         
         BCTR  R1,0                                                             
         STC   R1,NOQNS                                                         
*                                                                               
         ZIC   R1,NUMQNS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NUMQNS                                                        
*                                                                               
         B     MAMAXIT                                                          
         EJECT                                                                  
GOODQN   XC    TEST1,TEST1                                                      
         XC    TEST2,TEST2                                                      
         L     R2,ACHTAB                                                        
*                                                                               
BLDCANDS XC    CANDSAS,CANDSAS                                                  
         LA    R5,CANDSAS                                                       
         SR    R3,R3               R3 COUNTS CANDIDATES                         
*                                                                               
ORTEST   CLI   0(R2),LAST                                                       
         BE    FINDAQN                                                          
         L     R1,LEN                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEST1(0),0(R2)      MOVE IN A ROW TO TEST1                       
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    TEST1(0),ANSWER                                                  
*                                                                               
         LR    R4,R1               R4 GETS LEN MINUS 1                          
*                                                                               
OR10     LA    R6,TEST1                                                         
         AR    R6,R4                                                            
*                                                                               
         CLI   0(R6),WRONG                                                      
         BE    NEXTROW                                                          
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BNM   OR10                                                             
*                                                                               
         ST    R2,0(R5)            BUILD UP CANDSAS                             
         LA    R3,1(R3)            COUNT ENTRIES IN CANDSAS                     
         LA    R5,4(R5)                                                         
*                                                                               
NEXTROW  LA    R2,1(R2,R1)                                                      
         B     ORTEST                                                           
*                                                                               
FINDAQN  STC   R3,NUMCANDS                                                      
*                                                                               
         CLI   NUMCANDS,X'00'                                                   
         BE    BOUNDRY1            THERE ARE NO CANDIDATES                      
*                                                                               
         CLI   NUMCANDS,X'01'                                                   
         BE    NXTPHASE                                                         
*                                                                               
         OC    NOQNS,NOQNS         HAVE USED ALL 20 QNS                         
         BZ    BOUNDRY2                                                         
*                                                                               
         OC    SWITCH,SWITCH       RUN OUT OF MY OWN QNS                        
         BNZ   BOUNDRY3                                                         
*                                                                               
         LR    R6,R3                                                            
         BCTR  R6,0                                                             
         CH    R6,=H'1'                                                         
         BNE   FIND10                                                           
         SR    R4,R4                                                            
         LR    R5,R6                                                            
         B     FIND20                                                           
*                                                                               
FIND10   GOTO1 VRANDOM,PARM,(R6)                                                
*                                                                               
         L     R4,PARM+4                                                        
         BCTR  R6,0                                                             
*                                                                               
         GOTO1 VRANDOM,PARM,(R6)                                                
*                                                                               
         L     R5,PARM+4                                                        
         CR    R5,R4                                                            
         BL    FIND20                                                           
         LA    R5,1(R5)                                                         
*                                                                               
FIND20   SLL   R4,2                                                             
         SLL   R5,2                                                             
         LA    R6,CANDSAS                                                       
         L     R4,0(R4,R6)                                                      
         L     R5,0(R5,R6)                                                      
*                                                                               
* AT THIS POINT R4 AND R5 SHOULD BE POINTED TO TWO DIFFERENT CANDIDATE          
* ROWS IN THE CHARACTERISTICS TABLE.                                            
*                                                                               
         L     R6,LEN                                                           
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TEST1(0),0(R4)                                                   
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TEST2(0),0(R5)                                                   
*                                                                               
         BAS   RE,POSSQN                                                        
*                                                                               
         OC    QNNUM,QNNUM                                                      
         BNZ   GOTOASK                                                          
*                                                                               
         BAS   RE,DIFFICLT                                                      
*                                                                               
         OC    QNNUM,QNNUM                                                      
         BNZ   GOTOASK                                                          
*                                                                               
         B     BOUNDRY3                                                         
GOTOASK  BAS   RE,ASKQN                                                         
*                                                                               
         ZIC   R1,NOQNS                                                         
         BCTR  R1,0                                                             
         STC   R1,NOQNS                                                         
*                                                                               
         ZIC   R1,NUMQNS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NUMQNS                                                        
*                                                                               
         B     MAMAXIT                                                          
         EJECT                                                                  
NXTPHASE L     R1,AROWTAB                                                       
         L     R2,CANDSAS                                                       
         L     R3,ACHTAB                                                        
         L     R4,LEN                                                           
*                                                                               
         SR    R2,R3                                                            
         LA    R5,1                                                             
NXT10    LTR   R2,R2               FINDS THE POSITION OF THE CANDIDATE          
         BZ    NXT20               IN THE TABLES.LOADS IT INTO R5               
         SR    R2,R4                                                            
         LA    R5,1(R5)                                                         
         C     R5,NONTRYS                                                       
         BNH   NXT10                                                            
         DC    X'00'                                                            
*                                                                               
NXT20    CLC   0(5,R1),=C'NAMES'                                                
         BE    COMMIT                                                           
*                                                                               
         OC    NOQNS,NOQNS                                                      
         BZ    BOUNDRY2            HAVE USED ALL 20 QNS                         
*                                                                               
         BCTR  R5,0                                                             
         SLL   R5,2                                                             
         LA    R1,0(R5,R1)                                                      
*                                                                               
NXT40    L     R2,0(R1)                                                         
         L     R3,ATABLES                                                       
         LA    R2,0(R3,R2)                                                      
         MVC   STTABH,0(R2)                                                     
*                                                                               
         BAS   RE,RELOC                                                         
*                                                                               
         XC    ANSWER,ANSWER                                                    
         MVC   GUESSES,=X'02'                                                   
         XC    NUMQNS,NUMQNS                                                    
*                                                                               
         B     FINDQN                                                           
*                                                                               
COMMIT   LA    R1,5(R1)                                                         
         EJECT                                                                  
         SR    R2,R2                                                            
COM10    CLC   0(1,R1),=C'*'                                                    
         BNE   *+14                                                             
         LA    R2,1(R2)                                                         
         CR    R5,R2                                                            
         BE    COM20                                                            
         LA    R1,1(R1)                                                         
         B     COM10                                                            
COM20    LA    R2,1(R1)            R2 IS POINTING TO 1ST C OF MY CHOICE         
*                                                                               
         LR    R4,R2                                                            
COM30    CLI   0(R4),C'*'                                                       
         BE    COM40                                                            
         LA    R4,1(R4)                                                         
         B     COM30                                                            
*                                                                               
COM40    SR    R4,R2                                                            
         STC   R4,LHNAME                                                        
         ST    R2,AHNAME                                                        
*                                                                               
COM50    XC    QNLN1,QNLN1                                                      
         XC    QNLN2,QNLN2                                                      
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
*                                                                               
         MVC   QNLN1(L'MESS5A),MESS5A                                           
*                                                                               
         LA    R1,QNLN1+L'MESS5A                                                
*                                                                               
         BAS   RE,INSERT                                                        
*                                                                               
         L     R1,PARM                                                          
         MVC   0(L'MESS5B,R1),MESS5B                                            
         XC    MAMTOP1+10(13),MAMTOP1+10                                        
         MVI   NOLNS,X'02'                                                      
*                                                                               
         BAS   RE,FIXSCRN                                                       
*                                                                               
         MVI   STAGE,X'A2'                                                      
*                                                                               
         B     MAMEXIT                                                          
         EJECT                                                                  
CHECK    CLI   MAMAN01,C'Y'                                                     
         BE    CH10                                                             
*                                                                               
         CLI   MAMAN01,C'N'                                                     
         BE    CH10                                                             
*                                                                               
         XC    MAMAN01,MAMAN01                                                  
         B     MAMEXIT                                                          
*                                                                               
CH10     XC    MAMTOP1,MAMTOP1                                                  
         XC    MAMTOP2,MAMTOP2                                                  
         CLI   MAMAN01,C'N'                                                     
         BE    CH30                                                             
*                                                                               
CH20     MVC   MAMTOP1(L'MESS7),MESS7                                           
         MVC   MAMTOP2(L'MESS13),MESS13                                         
         XC    QNLN1,QNLN1                                                      
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
*                                                                               
         MVC   QNLN1(L'MESS14),MESS14                                           
         MVI   STAGE,X'00'                                                      
         MVI   NOLNS,X'01'                                                      
*                                                                               
         BAS   RE,FIXSCRN                                                       
*                                                                               
         ZIC   R1,GAMES                                                         
         LA    R1,1(R1)                                                         
         STC   R1,GAMES                                                         
         B     MAMAXIT                                                          
CH30     MVI   STAGE,X'A3'                                                      
         MVC   MAMTOP1(L'MESS6),MESS6                                           
         XC    QNLN1,QNLN1                                                      
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
*                                                                               
         MVC   QNLN1(L'MESS3),MESS3                                             
         MVI   NOLNS,X'01'                                                      
         BAS   RE,FIXSCRN                                                       
*                                                                               
         B     MAMEXIT                                                          
         EJECT                                                                  
FINAL    ZIC   R1,GAMES                                                         
         LA    R1,1(R1)                                                         
         STC   R1,GAMES                                                         
*                                                                               
         ZIC   R1,HISWINS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,HISWINS                                                       
*                                                                               
         XC    MAMTOP1,MAMTOP1                                                  
         XC    MAMTOP2,MAMTOP2                                                  
*                                                                               
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
*                                                                               
         MVC   QNLN1(L'MESS14),MESS14                                           
         MVC   MAMTOP2(L'MESS13),MESS13                                         
*                                                                               
         MVI   NOLNS,X'01'                                                      
         MVI   STAGE,X'00'                                                      
*                                                                               
         LA    R1,MAMAN01                                                       
         LA    R2,1                                                             
         LA    R3,MAMAN01+L'MAMAN01-1                                           
FIN10    CLI   0(R1),C'A'                                                       
         BNL   FIN20                                                            
         BXLE  R1,R2,FIN10                                                      
*                                                                               
         XC    MAMAN01,MAMAN01     HE HASN'T SAID WHAT HE IS                    
         B     MAMEXIT                                                          
*                                                                               
FIN20    CLI   0(R3),C'A'                                                       
         BNL   FIN30                                                            
         BCT   R3,FIN20                                                         
*                                                                               
FIN30    ST    R1,AHNAME                                                        
         LA    R3,1(R3)                                                         
         SR    R3,R1                                                            
         STC   R3,LHNAME                                                        
FIN39    L     R1,AROWTAB                                                       
         CLC   0(5,R1),=C'NAMES'                                                
         BNE   FIN70                                                            
*                                                                               
         XC    #2,#2                                                            
         BAS   RE,CNAMES                                                        
*                                                                               
         CLI   #2,X'FF'                                                         
         BNE   FIN80               HAVE FOUND HIS NAME                          
         B     FIN70               HAVEN'T FOUND IT SO FAR                      
*                                                                               
FIN70    BAS   RE,COMPARE                                                       
*                                                                               
         CLI   #1,X'FF'                                                         
         BE    FIN90               HAVN'T FOUND HIS NAME                        
*                                                                               
FIN80    MVC   MAMTOP1(L'MESS9),MESS9   FOUND HIS NAME                          
         LA    R1,MAMTOP1+L'MESS9                                               
*                                                                               
FIN85    BAS   RE,INSERT                                                        
         B     FINA0                                                            
*                                                                               
FIN90    MVC   MAMTOP1(L'MESS8),MESS8                                           
         LA    R1,MAMTOP1+L'MESS8                                               
         B     FIN85                                                            
*                                                                               
*                                                                               
FINA0    MVI   NOLNS,X'01'                                                      
         BAS   RE,FIXSCRN                                                       
*                                                                               
         B     MAMAXIT                                                          
*                                                                               
*   HE HAS GIVEN ME THREE DON'T KNOW ANSWERS                                    
*                                                                               
HELOSES  ZIC   R1,GAMES                                                         
         LA    R1,1(R1)                                                         
         STC   R1,GAMES                                                         
         XC    MAMTOP1,MAMTOP1                                                  
         XC    MAMTOP2,MAMTOP2                                                  
         MVC   MAMTOP1(L'MESS11),MESS11                                         
         MVC   MAMTOP2(L'MESS13),MESS13                                         
*                                                                               
         XC    QNLN1,QNLN1                                                      
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
         MVC   QNLN1(L'MESS14),MESS14                                           
         MVI   NOLNS,X'01'                                                      
*                                                                               
         BAS   RE,FIXSCRN                                                       
*                                                                               
         MVI   STAGE,X'00'                                                      
         B     MAMAXIT                                                          
*                                                                               
         EJECT                                                                  
*   THERE ARE NO CANDIDATES.SO SET ALL PREVIOUS ANSWERS TO DONT KNOW            
BOUNDRY1 LA    R1,ANSWER                                                        
         L     R2,LEN                                                           
         BCTR  R2,0                                                             
         LA    R3,0(R2,R1)                                                      
         LA    R2,1                                                             
*                                                                               
BOND10   OC    0(1,R1),0(R1)                                                    
         BZ    *+8                                                              
         MVI   0(R1),X'80'                                                      
         BXLE  R1,R2,BOND10                                                     
*                                                                               
         MVI   GUESSES,X'02'                                                    
         CLC   GUESSES,NOQNS                                                    
         BNH   *+10                                                             
         MVC   GUESSES,NOQNS                                                    
*                                                                               
         XC    MAMTOP2,MAMTOP2                                                  
         MVC   MAMTOP2(L'MESS12),MESS12                                         
*                                                                               
         B     FINDQN              NOW PROCEED AS NORMAL                        
*                                                                               
*   HAVE USED ALL 20 QUESTIONS SO CONCEDE                                       
*                                                                               
BOUNDRY2 XC    MAMTOP1,MAMTOP1                                                  
         MVC   MAMTOP1(L'MESS4),MESS4                                           
BOR10    XC    MAMTOP2,MAMTOP2                                                  
         XC    QNLN1,QNLN1                                                      
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
*                                                                               
         MVC   QNLN1(L'MESS14),MESS14                                           
         MVC   MAMTOP2(L'MESS13),MESS13                                         
*                                                                               
         MVI   NOLNS,X'01'                                                      
         BAS   RE,FIXSCRN                                                       
*                                                                               
         ZIC   R1,HISWINS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,HISWINS                                                       
*                                                                               
         ZIC   R1,GAMES                                                         
         LA    R1,1(R1)                                                         
         STC   R1,GAMES                                                         
*                                                                               
         MVI   STAGE,X'00'                                                      
         B     MAMAXIT                                                          
*                                                                               
*   HAVE NO MORE USEFUL QNS LEFT                                                
*                                                                               
BOUNDRY3 L     R1,AROWTAB                                                       
         CLC   0(5,R1),=C'NAMES'                                                
         BE    BOR20                                                            
*                                                                               
         ZIC   R2,NUMCANDS         CHOOSE A RANDOM CANDIDATE AND GO ON          
         BCTR  R2,0                                                             
         GOTO1 VRANDOM,PARM,(R2)                                                
*                                                                               
         L     R2,PARM+4                                                        
         SLL   R2,2                                                             
*                                                                               
         LA    R1,CANDSAS                                                       
         L     R2,0(R2,R1)                                                      
         ST    R2,0(R1)                                                         
*                                                                               
         MVI   NUMCANDS,X'01'                                                   
         B     NXTPHASE                                                         
BOR20    XC    MAMTOP1,MAMTOP1     STUCK AT FINAL STAGE SO CONCEDE              
         MVC   MAMTOP1(L'MESS2),MESS2                                           
         B     BOR10                                                            
*                                                                               
         EJECT                                                                  
*  ROUTINE LOOKS FOR HIS NAME IN MY TABLES IF FOUND PASSES BACK                 
*  PATH TO HIS NAME IN #1,#2,(#3) OTHERWISE PUTS X'FF' IN #1                    
COMPARE  NTR1  BASE=ABASE                                                       
         XC    ADTABH1,ADTABH1                                                  
         XC    ADTABH2,ADTABH2                                                  
         XC    ADTABH3,ADTABH3                                                  
         XC    #1,#1                                                            
         XC    #2,#2                                                            
         XC    #3,#3                                                            
*                                                                               
         L     R1,ATYPH                                                         
         MVC   STTABH,0(R1)                                                     
         BAS   RE,RELOC                                                         
*                                                                               
         MVI   #2,X'FF'                                                         
         L     R2,ATABLES                                                       
         SR    R1,R2                                                            
         ST    R1,ADTABH1                                                       
*                                                                               
C10      CLI   #3,X'FF'                                                         
         BE    C20                                                              
         CLI   #2,X'FF'                                                         
         BE    C30                                                              
         B     EXIT                FOUND IT!                                    
*                                                                               
C20      L     R1,ADTABH2                                                       
         ZIC   R2,#2                                                            
         XC    #3,#3                                                            
         SR    R3,R3                                                            
         B     C40                                                              
*                                                                               
C30      L     R1,ADTABH1                                                       
         ZIC   R2,#1                                                            
         XC    #2,#2                                                            
         XC    #3,#3                                                            
         LA    R3,1                                                             
*                                                                               
C40      L     R4,ATABLES                                                       
         LA    R1,0(R1,R4)                                                      
         MVC   STTABH,0(R1)                                                     
         BAS   RE,RELOC                                                         
         C     R2,NONTRYS                                                       
         BE    D10                                                              
*                                                                               
         LR    R6,R2                                                            
         L     R5,AROWTAB                                                       
         SLL   R6,2                                                             
*                                                                               
         L     R6,0(R6,R5)                                                      
         LA    R2,1(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   C50                                                              
*                                                                               
         ST    R6,ADTABH3                                                       
         STC   R2,#2                                                            
         B     C60                                                              
*                                                                               
C50      ST    R6,ADTABH2                                                       
         STC   R2,#1                                                            
*                                                                               
C60      LA    R6,0(R6,R4)                                                      
         MVC   STTABH,0(R6)                                                     
         BAS   RE,RELOC                                                         
*                                                                               
         L     R1,AROWTAB                                                       
         CLC   0(5,R1),=C'NAMES'                                                
         BE    C70                                                              
         MVI   #2,X'00'                                                         
         MVI   #3,X'FF'                                                         
         B     C10                                                              
*                                                                               
C70      BAS   RE,CNAMES                                                        
         B     C10                                                              
*                                                                               
D10      LTR   R3,R3                                                            
         BNZ   D20                                                              
         XC    #3,#3                                                            
         XC    ADTABH3,ADTABH3                                                  
         MVI   #2,X'FF'                                                         
         B     C10                                                              
*                                                                               
D20      MVI   #1,X'FF'                                                         
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE DOES THE CHECKING FOR 'COMPARE' ONE TABLE AT A TIME                  
*  PASSES BACK PATH OR X'FF' ACCORDINGLY                                        
CNAMES   NTR1  BASE=ABASE                                                       
         L     R1,AROWTAB                                                       
         LA    R1,6(R1)                                                         
         L     R2,AHNAME                                                        
         ZIC   R3,LHNAME                                                        
         BCTR  R3,0                                                             
         LR    R4,R1                                                            
         LA    R5,1                                                             
         L     R6,NONTRYS                                                       
*                                                                               
CN10     CLI   0(R4),C'*'                                                       
         BE    CN20                                                             
         LA    R4,1(R4)                                                         
         B     CN10                                                             
*                                                                               
CN20     LA    R4,1(R4)                                                         
         EX    R3,*+12                                                          
         BE    CN40                                                             
         B     CN30                                                             
         CLC   0(0,R1),0(R2)       COMPARE HIS NAME & MY LISTING                
*                                                                               
CN30     CR    R5,R6                                                            
         BNL   CN60                                                             
         LR    R1,R4                                                            
         LA    R5,1(R5)                                                         
         B     CN10                                                             
*                                                                               
CN40     LA    R7,2(R3)            MAKE SURE BOTH NAMES SAME LENGTH             
         LR    R8,R4                                                            
         SR    R8,R1                                                            
         CR    R7,R8                                                            
         BNE   CN30                                                             
         OC    #2,#2                                                            
         BNZ   CN50                                                             
         STC   R5,#2                                                            
         XC    #3,#3                                                            
         XC    ADTABH3,ADTABH3                                                  
         B     EXIT                                                             
*                                                                               
CN50     STC   R5,#3                                                            
         B     EXIT                                                             
*                                                                               
CN60     OC    #2,#2                                                            
         BNZ   CN70                                                             
         MVI   #2,X'FF'                                                         
         XC    #3,#3                                                            
         B     EXIT                                                             
*                                                                               
CN70     MVI   #3,X'FF'                                                         
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE MOVES HIS NAME TO A LOCATION SPECIFIED BY R1.LOADS                   
*  ADDRESS OF NEXT CHARACTER AFTER NAME INTO PARM.DOESN'T                       
*  CHECK FOR POSSIBLE OVERFLOW IN TWA ON LONG NAMES (NOT NEEDED                 
*  AS PRESENTLY CONFIGURED.)                                                    
INSERT   NTR1  BASE=ABASE                                                       
         LR    R3,R1               R1 BEGINS POINTING TO OUTPUT LINE            
         L     R1,AHNAME                                                        
         ZIC   R2,LHNAME                                                        
*                                                                               
         IF    0(R1),EQ,C'A',OR,C'E',OR,C'I',OR,C'O',OR,C'U',INS10              
*                                                                               
         XC    0(1,R3),0(R3)                                                    
         B     INS20                                                            
*                                                                               
INS10    MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         XC    0(1,R3),0(R3)                                                    
*                                                                               
INS20    LA    R3,1(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
*                                                                               
         LA    R1,1(R2,R3)                                                      
         ST    R1,PARM                                                          
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE FINDS QN IN QNTABLE & ARRANGES FOR FIXSCRN TO                        
*  DISPLAY IT                                                                   
ASKQN    NTR1  BASE=ABASE                                                       
*                                                                               
         XC    QNLN1,QNLN1                                                      
         XC    QNLN2,QNLN2                                                      
         LA    R1,QNLN1                                                         
ASK10    MVI   0(R1),C'.'          FILL QNLN1&2 WITH .S                         
*                                                                               
         MVC   1(L'QNLN1-1,R1),0(R1)                                            
*                                                                               
         CLI   QNLN2,C'.'                                                       
         BE    ASK20                                                            
         LA    R1,QNLN2                                                         
         B     ASK10                                                            
*                                                                               
ASK20    ZIC   R1,QNNUM                                                         
         L     R2,AQNTAB                                                        
         SR    R3,R3                                                            
ASK30    CLI   0(R2),C'*'          FIND 1ST C OF QN TO BE ASKED                 
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         CR    R1,R3                                                            
         BNE   ASK30                                                            
*                                                                               
         LR    R3,R2                                                            
*                                                                               
ASK40    CLI   0(R3),C'*'          FIND LAST C OF QN TO BE ASKED                
         BE    ASK50                                                            
         LA    R3,1(R3)                                                         
         B     ASK40                                                            
*                                                                               
ASK50    LR    R4,R3                                                            
         SR    R4,R2               R4 GETS LENGTH OF QN TO BE ASKED             
         LA    R5,L'QNLN1                                                       
         CR    R4,R5                                                            
         BNH   ASK70               WILL FIT INTO ONE LINE                       
*                                                                               
         LA    R6,0(R5,R2)                                                      
*                                                                               
ASK60    CLI   0(R6),C' '                                                       
         BE    ASK80                                                            
         BCT   R6,ASK60                                                         
*                                                                               
ASK70    BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   QNLN1(0),0(R2)                                                   
         MVI   NOLNS,X'01'                                                      
         XC    IND,IND                                                          
         B     ASK90                                                            
*                                                                               
ASK80    LR    R7,R6                                                            
         SR    R7,R2                                                            
         XC    QNLN1,QNLN1                                                      
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   QNLN1(0),0(R2)                                                   
*                                                                               
         LA    R2,1(R6)                                                         
         LR    R7,R3                                                            
         SR    R7,R2                                                            
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   QNLN2(0),0(R2)                                                   
         MVI   NOLNS,X'02'                                                      
         MVI   IND,X'01'                                                        
*                                                                               
ASK90    BAS   RE,FIXSCRN                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE HANDLES SCREEN ROLL DOWN & INSERTION OF QNS/MESSAGES                 
FIXSCRN  NTR1  BASE=ABASE                                                       
         CLI   NOLNS,X'12'        ONLY 19 LINES ON THE SCREEN                   
         BNL   EXIT                                                             
*                                                                               
         LA    R5,MAMQN19H                                                      
         LA    R4,L'MAMQN01+L'MAMAN01+16    EFFECTIVE NO OF BYTES/LINE          
         LR    R1,R5                                                            
         ZIC   R2,NOLNS                                                         
         SR    R1,R4               MOVE UP R1 BY NOLNS                          
         BCT   R2,*-2                                                           
         LA    R2,L'MAMQN01+8(R1)  POINT R2 AT THE CORRESPONDING ANSWER         
         LA    R6,MAMQN01H                                                      
         LA    R7,L'MAMQN01-1                                                   
         B     MOVEDOWN                                                         
*                                                                               
ANSMOVE  LA    R5,MAMAN19H                                                      
         LA    R6,MAMAN01H                                                      
         LA    R7,L'MAMAN01-1                                                   
         LR    R1,R2                                                            
*                                                                               
MOVEDOWN LR    R3,R1                                                            
         EX    R7,*+8                                                           
         B     *+10                                                             
         XC    8(0,R5),8(R5)                                                    
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(R1)                                                    
         EX    R7,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         SR    R5,R4                                                            
         SR    R1,R4                                                            
         CR    R1,R6                                                            
         BNL   MOVEDOWN+2                                                       
         CR    R3,R2                                                            
         BNE   ANSMOVE                                                          
*                                                                               
         LA    R1,MAMQN19                                                       
         LA    R4,1                                                             
         LA    R5,L'MAMQN19-1(R1)                                               
         CLI   0(R1),C'?'                                                       
         BE    FIXEND                                                           
         BXLE  R1,R4,*-8                                                        
         XC    MAMQN19,MAMQN19                                                  
         XC    MAMAN19,MAMAN19                                                  
*                                                                               
FIXEND   CLI   NOLNS,X'01'                                                      
         BE    *+10                                                             
         MVC   MAMQN02,QNLN2                                                    
         MVC   MAMQN01,QNLN1                                                    
         B     EXIT                                                             
         EJECT                                                                  
* POSSQN GIVEN TWO CANDIDATES WILL FIND A QN TO DISTINGUISH THEM                
POSSQN   NTR1  BASE=ABASE                                                       
         XC    TEST1,TEST2         DONT HAVE TO WORRY ABOUT LENGTHS             
         XC    TEST1,ANSWER                                                     
         XC    QNNUM,QNNUM                                                      
*                                                                               
POS10    LA    R7,TEST1                                                         
         L     R5,LEN                                                           
         LA    R4,1                                                             
         LA    R6,1                                                             
         BCTR  R5,0                                                             
         LA    R5,0(R5,R7)                                                      
*                                                                               
POS20    CLI   0(R7),WRONG                                                      
         BE    POS30                                                            
         LA    R6,1(R6)                                                         
         BXLE  R7,R4,POS20                                                      
         B     POS40                                                            
*                                                                               
POS30    STC   R6,QNNUM                                                         
*                                                                               
POS40    B     EXIT                                                             
         EJECT                                                                  
* EXAMINES PAIRS OF CANDIDATES UNTIL IT FINDS A USEFUL QN SETS CON CODE         
DIFFICLT NTR1  BASE=ABASE                                                       
         L     R1,LEN                                                           
         BCTR  R1,0                                                             
         ZIC   R2,NUMCANDS                                                      
         BCTR  R2,0                                                             
         LA    R3,CANDSAS                                                       
         LA    R4,4(R3)                                                         
         LA    R6,4                                                             
         LA    R7,0(R3)                                                         
         SLL   R2,2                                                             
         LA    R7,0(R2,R7)                                                      
*                                                                               
DIF10    L     R5,0(R3)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEST1(0),0(R5)                                                   
*                                                                               
DIF20    L     R5,0(R4)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEST2(0),0(R5)                                                   
*                                                                               
         BAS   RE,POSSQN                                                        
*                                                                               
         OC    QNNUM,QNNUM                                                      
         BNZ   DIFFXIT                                                          
*                                                                               
         BXLE  R4,R6,DIF10                                                      
         LA    R4,8(R3)                                                         
         CR    R4,R7                                                            
         BH    DIFFXIT                                                          
*                                                                               
         BXLE  R3,R6,DIF10                                                      
*                                                                               
*                                                                               
DIFFXIT  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
RELOC    NTR1  BASE=ABASE                                                       
         L     R1,ATABLES          STORES RESOLVED ADDRESSES OF                 
         L     R2,ADQNTAB          TABLES IN WORKING STORAGE                    
         LA    R2,0(R2,R1)                                                      
         ST    R2,AQNTAB                                                        
         L     R2,ADROWTAB                                                      
         LA    R2,0(R2,R1)                                                      
         ST    R2,AROWTAB                                                       
         L     R2,ADCHTAB                                                       
         LA    R2,0(R2,R1)                                                      
         ST    R2,ACHTAB                                                        
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
MAMAXIT  ZIC   R1,NOQNS                                                         
         CVD   R1,DUB                                                           
         UNPK  MAMBOT3+15(2),DUB                                                
         OI    MAMBOT3+16,X'F0'                                                 
*                                                                               
         ZIC   R1,DKNOWS                                                        
         CVD   R1,DUB                                                           
         UNPK  MAMBOT1+19(1),DUB                                                
         OI    MAMBOT1+19,X'F0'                                                 
*                                                                               
         ZIC   R1,HISWINS                                                       
         CVD   R1,DUB                                                           
         UNPK  MAMBOT4+11(2),DUB                                                
         OI    MAMBOT4+12,X'F0'                                                 
*                                                                               
         ZIC   R1,GAMES                                                         
         CVD   R1,DUB                                                           
         UNPK  MAMBOT4+21(2),DUB                                                
         OI    MAMBOT4+22,X'F0'                                                 
*                                                                               
MAMEXIT  OC    IND,IND                                                          
         BNZ   *+16                                                             
         OI    MAMAN01H+6,X'41'                                                 
         OI    MAMAN02H+6,X'20'    CHANGE TO PROTECTED FOR NEXT INPUT           
         B     *+8                                                              
         OI    MAMAN02H+6,X'41'                                                 
         LA    R1,MAMTOP1H                                                      
         LA    R3,MAMBOT4H                                                      
         SR    R2,R2                                                            
MA10     IC    R2,0(R1)                                                         
         OI    6(R1),X'80'                                                      
         BXLE  R1,R2,MA10                                                       
*                                                                               
         XMOD1 1                                                                
         EJECT                                                                  
*   EQUATES STATEMENTS                                                          
Y        EQU   X'02'                                                            
N        EQU   X'01'                                                            
D        EQU   X'80'                                                            
WRONG    EQU   X'03'                                                            
LAST     EQU   X'FF'                                                            
*                                                                               
*   MESSAGES                                                                    
*                                                                               
MESSA    DC    C'DON''T KNOWS SO FAR=0.............................'            
MESSB    DC    C'YOU ARE ONLY ALLOWED TWO.'                                     
MESSC    DC    C'QUESTIONS LEFT='                                               
MESSD    DC    C'YOU''VE WON 00 OUT OF 00.'                                     
*                                                                               
MESS1    DC    C'Y=YES,N=NO,X=DON''T KNOW'                                      
MESS2    DC    C'YOU WIN I CAN''T GUESS WHAT YOU ARE.'                          
MESS3    DC    C'PLEASE KEY IN YOUR NAME.'                                      
MESS4    DC    C'YOU WIN I''VE RUN OUT OF QUESTIONS.'                           
*                                                                               
MESS5A   DC    C'***ARE YOU A'                                                  
MESS5B   DC    C'?***'                                                          
*                                                                               
MESS6    DC    C'OUTFOXED YOU WIN.'                                             
MESS7    DC    C'HARD LUCK-I WIN.'                                              
MESS8    DC    C'I''M AFRAID I''VE NEVER HEARD OF A'                            
MESS9    DC    C'I SHOULD HAVE KNOWN YOU WERE A'                                
MESS11   DC    C'I WIN,YOU''VE GIVEN ME 3 DON''T KNOW ANSWERS.'                 
MESS12   DC    C'P.S.I THINK YOU''RE LYING,I''LL IGNORE YOUR LAST FEW A*        
               NSWERS'                                                          
MESS13   DC    C'TO PLAY AGAIN ENTER ''A'' OR ''B'''                            
MESS14   DC    C'YOU ARE NOW PLAYING VERSION A'                                 
*                                                                               
         EJECT                                                                  
*    LITERALS/DSECTS                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE GAMAMTW                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016GAMAM00   05/01/02'                                      
         END                                                                    
