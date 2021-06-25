*          DATA SET GACON00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB1100A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'CONNECT FOUR'                                                   
         PRINT NOGEN                                                            
GACON    CSECT                                                                  
         NMOD1 100,**CON4**                                                     
         USING WORKD,RC                                                         
         L     R3,4(R1)                                                         
         USING TB11FFD,R3                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING GACON+4096,RA                                                    
         SPACE 1                                                                
         LA    R0,4                ZERO WORK AREA                               
         LR    R1,RC                                                            
         XC    0(200,R1),0(R1)                                                  
         LA    R1,200(R1)                                                       
         BCT   R0,*-10                                                          
         SPACE 1                                                                
         MVI   HEAVYD,C'N'                                                      
         SPACE 2                                                                
* CONTROL - THIS SECTION OF CODE CONTROLS *                                     
*  THE FLOW OF LOGIC FOR CONNECT FOUR     *                                     
         SPACE 1                                                                
         BAS   RE,GETMOVE                                                       
         CLI   MYMOVESW,1                                                       
         BE    CNTRL                                                            
         CLI   REENTER,1                                                        
         BE    EXIT                                                             
         MVI   WHOMOVED,C'X'                                                    
         BAS   RE,UPDATE1                                                       
         CLI   FLAG,C'X'                                                        
         BE    DISPLAY                                                          
         BAS   RE,QUIKWIN                                                       
         CLI   MYMOVE,0                                                         
         BNE   UPDATE                                                           
CNTRL    BAS   RE,CALCVALU                                                      
         CLI   MYMOVE,0                                                         
         BNE   UPDATE                                                           
         BAS   RE,RESIGN           CHECK TO SEE IF THE CPU SHOULD QUIT          
         B     DISPLAY                                                          
UPDATE   MVI   WHOMOVED,C'O'                                                    
         BAS   RE,UPDATE1                                                       
         BAS   RE,UPDATE2                                                       
DISPLAY  BAS   RE,DSPMOVE                                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
* RESIGN - THIS SECTION OF CODE CHECKS IF THE CPU SHOULD RESIGN *               
         SPACE 1                                                                
RESIGN   NTR1                                                                   
         LA    R4,SVMOVES                                                       
         LA    R5,7                                                             
         SPACE 1                                                                
RES00    CLI   0(R4),42                                                         
         BNH   RES10                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,RES00                                                         
         SPACE 1                                                                
         OI    SWITCH,1            GAME TIED - NO WINNER                        
         SPACE 1                                                                
RES10    XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(42),=CL42'  THE GAME IS OVER - NO WINNER. PLAY AGX        
               AIN? '                                                           
         CLI   SWITCH,1                                                         
         BE    RES20                                                            
         MVC   CONMSG(42),=CL42' NICE GAME - YOU WON. PLAY AGAIN?'              
RES20    FOUT  CONMSGH                                                          
         XC    SVMOVES,SVMOVES                                                  
         B     EXIT                                                             
         EJECT                                                                  
* GETMOVE - THIS SECTION OF CODE GETS THE USER'S LAST MOVE *                    
         SPACE 1                                                                
GETMOVE  NTR1                                                                   
         LA    R2,CONMVXH                                                       
         MVC   CONMSG(57),=CL57'CONNECT FOUR. ENTER YOUR MOVE (1 TO 7) C        
               OR A SPACE TO PASS'                                              
         FOUT  CONMSGH                                                          
         OI    6(R2),OI1C                                                       
         CLC   =C'*D',8(R2)                                                     
         BNE   *+12                                                             
         MVI   HEAVYD,C'Y'                                                      
         B     EXIT2                                                            
         CLC   =C'*ON',8(R2)                                                    
         BNE   *+12                                                             
         MVI   SVDSPSW,C'Y'        DISPLAY EVALUATION OF MOVES                  
         B     EXIT2                                                            
         CLC   =C'*OFF',8(R2)                                                   
         BNE   *+18                                                             
         XC    CONDSP,CONDSP                                                    
         MVI   SVDSPSW,C'N'        DISPLAY OF EVALUATION NOT WANTED             
         B     EXIT2                                                            
         SPACE 1                                                                
         XC    CONDSP,CONDSP                                                    
         CLI   SVDSPSW,C'Y'        IS AN EVALUATION WANTED?                     
         BNE   CONN00               NO                                          
         FOUT  CONDSPH                                                          
CONN00   MVI   MYMOVE,0                                                         
         MVI   HISMOVE,0                                                        
         MVI   MYMOVESW,0                                                       
         MVI   REENTER,0                                                        
         CLI   SVMOVES,0                                                        
         BNE   CONN10                                                           
         SPACE 1                                                                
         MVC   SVMOVES,=X'01020304050607'                                       
         XC    SVBOARD,SVBOARD                                                  
         MVC   SAVETBL(140),WINTAB                                              
         MVC   SAVETBL+140(140),WINTAB+140                                      
         MVC   SAVETBL+280(140),WINTAB+280                                      
         SPACE 1                                                                
         LA    R4,CONROWFH                                                      
         LA    R5,6                                                             
         SPACE 1                                                                
CONN05   DC    0H'0'                                                            
         MVC   14(37,R4),=C'.     .     .     .     .     .     .'              
         FOUT  (R4)                                                             
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   R5,CONN05                                                        
         EJECT                                                                  
         CLI   5(R2),0             TEST IF LENGTH OF INPUT IS NON-ZERO          
         BNE   CONN10                                                           
         OI    MYMOVESW,1                                                       
         B     EXIT                                                             
         SPACE 1                                                                
CONN10   DC    0H'0'                                                            
         CLI   5(R2),1                                                          
         BNE   BADMOVE                                                          
         CLI   8(R2),C'1'                                                       
         BL    BADMOVE                                                          
         CLI   8(R2),C'7'                                                       
         BH    BADMOVE                                                          
         ZIC   R1,8(R2)                                                         
         N     R1,=F'15'                                                        
         IC    R1,SVMOVES-1(R1)                                                 
         SPACE 1                                                                
         STC   R1,HISMOVE                                                       
         CLI   HISMOVE,42                                                       
         BH    BADMOVE                                                          
         SPACE 1                                                                
         LA    R4,SVMOVES                                                       
         LA    R5,7                                                             
CONN15   CLC   HISMOVE,0(R4)                                                    
         BE    CONN20                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,CONN15                                                        
         B     BADMOVE                                                          
CONN20   LA    RE,7(R1)            SET NEXT MOVE THIS COLUMN                    
         STC   RE,0(R4)                                                         
         LA    RE,SVBOARD(R1)                                                   
         MVI   0(RE),C'X'                                                       
         B     EXIT                                                             
         SPACE 3                                                                
BADMOVE  XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(29),=C'INVALID MOVE. PLEASE RE-ENTER'                     
         FOUT  CONMSGH                                                          
         SPACE 1                                                                
EXIT2    DC    0H'0'                                                            
         MVI   REENTER,1                                                        
         B     EXIT                                                             
         EJECT                                                                  
* QUIKWIN - THIS SECTION OF CODE TESTS FOR A QUICK WIN. *                       
*  FAILING THAT, IT LOOKS FOR ANY 'MUST' BLOCKS. UPON   *                       
*  EXIT MYMOVE WILL BE ZERO IF NO MOVE HAS BEEN FOUND.  *                       
         SPACE 1                                                                
QUIKWIN  NTR1                                                                   
         MVI   MYMOVE,0                                                         
         MVI   QWIKSW,C'W'         LOOK FOR WIN POSSIBILITY                     
         LA    R4,SAVETBL                                                       
         LA    R8,SVTBLEND                                                      
QUIK10   CLI   1(R4),1                                                          
         BNE   *+8                                                              
         BAS   RE,QWIKPOSS         CHECK THE POSSIBILITY OF A QUICK WIN         
         LA    R4,6(R4)                                                         
         CR    R4,R8                                                            
         BL    QUIK10              KEEP LOOKING FOR A QUICK WIN                 
         SPACE 1                                                                
         MVI   QWIKSW,C'L'         LOOK FOR LOSS POSSIBILITY                    
         LA    R4,SAVETBL                                                       
QUIK20   CLI   0(R4),1                                                          
         BNE   *+8                                                              
         BAS   RE,QWIKPOSS         CHECK THE PROSPECT OF IMMEDIATE LOSS         
         LA    R4,6(R4)                                                         
         CR    R4,R8                                                            
         BL    QUIK20              KEEP CHECKING FOR IMMEDIATE LOSSES           
         B     EXIT                                                             
         SPACE 1                                                                
QWIKPOSS LA    R6,SVMOVES                                                       
         LA    R7,7                                                             
QUIK30   CLC   2(1,R4),0(R6)       CHECK IF THIS POSSIBLE WIN OR LOSS           
         BE    QUIK40               SITUATION CAN BE REALIZED.                  
         CLC   3(1,R4),0(R6)                                                    
         BE    QUIK40                                                           
         CLC   4(1,R4),0(R6)                                                    
         BE    QUIK40                                                           
         CLC   5(1,R4),0(R6)                                                    
         BE    QUIK40                                                           
         LA    R6,1(R6)                                                         
         BCT   R7,QUIK30                                                        
         BR    RE                  RETURN - NO FORCED MOVES FOUND               
         SPACE 1                                                                
QUIK40   MVC   MYMOVE,0(R6)                                                     
         CLI   QWIKSW,C'W'                                                      
         BNE   QUIK50                                                           
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(42),=CL42' ** I WIN ** LET''S PLAY AGAIN'                 
         FOUT  CONMSGH                                                          
         B     EXIT                                                             
         SPACE 1                                                                
QUIK50   CLI   SVDSPSW,C'Y'                                                     
         BNE   EXIT                                                             
         XC    CONDSP,CONDSP                                                    
         MVC   CONDSP+18(14),=CL14'BLOCK REQUIRED'                              
         FOUT  CONDSPH                                                          
         B     EXIT                                                             
         EJECT                                                                  
* UPDATE1 - THIS SECTION OF CODE UPDATES THE XNEED AND ONEED     *              
*  FIELDS OF SAVETBL. WHOMOVED = 0 IF IT WAS JUST THE CPU'S MOVE *              
         SPACE 1                                                                
UPDATE1  NTR1                                                                   
         XC    FNDSW,FNDSW                                                      
         LA    R5,ALLPOSS                                                       
         CLI   WHOMOVED,C'O'                                                    
         BNE   UPDT50                                                           
         MVC   POSSMOVE,MYMOVE     THE CPU'S (O) MOVE                           
         BAS   RE,FNDENTRY                                                      
UPDT00   CLI   0(R5),X'FF'         IS THIS THE END OF ALLPOSS ARRAY?            
         BE    EXIT                 YES. EXIT                                   
         ZIC   R6,0(R5)            R6 HAS THE DISPLACEMENT INTO SAVETBL         
         MH    R6,=H'6'                                                         
         ZIC   R8,SAVETBL+1(R6)    UPDATE ONEED AND SET XNEED TO X'FF'          
         STC   R8,DUB                                                           
         CLI   DUB,X'FF'                                                        
         BE    UPDT45                                                           
         CLI   DUB,4                                                            
         BE    UPDT30                                                           
         CLI   DUB,3                                                            
         BE    UPDT20                                                           
         CLI   DUB,2                                                            
         BE    UPDT10                                                           
         CLI   DUB,1                                                            
         BNE   *+12                                                             
         MVI   FLAG,C'O'           SET FLAG TO SEND 'CPU WINS' MESSAGE          
         B     EXIT                EXIT                                         
         DC    H'0'                                                             
UPDT10   LH    R8,=X'FF01'                                                      
         B     UPDT40                                                           
UPDT20   LH    R8,=X'FF02'                                                      
         B     UPDT40                                                           
UPDT30   LH    R8,=X'FF03'                                                      
UPDT40   STH   R8,SAVETBL(R6)                                                   
UPDT45   LA    R5,1(R5)                                                         
         B     UPDT00                                                           
         EJECT                                                                  
UPDT50   MVC   POSSMOVE,HISMOVE    THE USER'S (X) MOVE                          
         BAS   RE,FNDENTRY                                                      
UPDT60   CLI   0(R5),X'FF'         IS THIS THE END OF ALLPOSS ARRAY?            
         BE    EXIT                 YES. EXIT                                   
         ZIC   R6,0(R5)            R6 HAS THE DISPLACEMENT INTO SAVETBL         
         MH    R6,=H'6'                                                         
         ZIC   R8,SAVETBL(R6)      UPDATE XNEED AND SET ONEED TO X'FF           
         STC   R8,DUB                                                           
         CLI   DUB,X'FF'                                                        
         BE    UPDT110                                                          
         CLI   DUB,4                                                            
         BE    UPDT90                                                           
         CLI   DUB,3                                                            
         BE    UPDT80                                                           
         CLI   DUB,2                                                            
         BE    UPDT70                                                           
         CLI   DUB,1                                                            
         BNE   *+12                                                             
         MVI   FLAG,C'X'           SET FLAG TO SEND 'USER WINS' MESSAGE         
         B     EXIT                EXIT                                         
         DC    H'0'                                                             
UPDT70   LH    R8,=X'01FF'                                                      
         B     UPDT100                                                          
UPDT80   LH    R8,=X'02FF'                                                      
         B     UPDT100                                                          
UPDT90   LH    R8,=X'03FF'                                                      
UPDT100  STH   R8,SAVETBL(R6)                                                   
UPDT110  LA    R5,1(R5)                                                         
         B     UPDT60                                                           
         EJECT                                                                  
* UPDATE2 - THIS SECTION OF CODE UPDATES SVMOVES *                              
         SPACE 1                                                                
UPDATE2  NTR1                                                                   
         LA    R4,SVMOVES                                                       
         LA    R5,7                                                             
         CLI   WHOMOVED,C'O'                                                    
         BNE   UPDATE2B            ERROR                                        
         MVC   UPMOVE,MYMOVE       CPU MADE THE LAST MOVE                       
UPDATE2A CLC   UPMOVE,0(R4)                                                     
         BE    UPDATE2C                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,UPDATE2A                                                      
UPDATE2B DC    H'0'                                                             
         SPACE 1                                                                
UPDATE2C ZIC   R7,0(R4)                                                         
         LA    RE,SVBOARD(R7)      PUT CPU'S LAST MOVE ON SVBOARD               
         MVI   0(RE),C'O'                                                       
         LA    R7,7(R7)                                                         
         STC   R7,0(R4)                                                         
         SPACE 1                                                                
         LA    R4,SVMOVES                                                       
         LA    R5,7                                                             
UPDATE2D CLI   0(R4),42                                                         
         BNH   EXIT                EXIT                                         
         LA    R4,1(R4)                                                         
         BCT   R5,UPDATE2D                                                      
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(42),=CL42'  THE GAME IS OVER - NO WINNER. PLAY AGX        
               AIN? '                                                           
         FOUT  CONMSGH                                                          
         XC    SVMOVES,SVMOVES                                                  
         B     EXIT                EXIT                                         
         EJECT                                                                  
* FNDENTRY - THIS SECTION OF CODE FINDS ALL THE RELEVANT SAVETBL *              
*  ENTRIES (THE DISPLACEMENTS INTO SAVETBL ARE STORED IN THE     *              
*  ARRAYS WINPOSS AND LOSSPOSS (FOR THE CPU)).                   *              
         SPACE 1                                                                
FNDENTRY NTR1                                                                   
         LA    R4,SAVETBL                                                       
         LA    R5,SVTBLEND                                                      
         XR    R8,R8                                                            
         CLI   FNDSW,0                                                          
         BNE   ENT70                                                            
         LA    R6,ALLPOSS                                                       
         XC    ALLPOSS,ALLPOSS                                                  
ENT00    CLC   2(1,R4),POSSMOVE    DOES THIS COMBINATION INCLUDE THE            
*                                   POSSIBLE MOVE CANDIDATE?                    
         BE    ENT10                                                            
         CLC   3(1,R4),POSSMOVE                                                 
         BE    ENT10                                                            
         CLC   4(1,R4),POSSMOVE                                                 
         BE    ENT10                                                            
         CLC   5(1,R4),POSSMOVE                                                 
         BNE   *+12                                                             
ENT10    STC   R8,0(R6)                                                         
         LA    R6,1(R6)                                                         
         LA    R4,6(R4)                                                         
         CR    R4,R5                                                            
         BNL   ENT20               END OF SAVETBL - EXIT                        
         LA    R8,1(R8)                                                         
         B     ENT00                                                            
ENT20    MVI   0(R6),X'FF'                                                      
         B     EXIT                EXIT                                         
         SPACE 1                                                                
ENT70    LA    R6,LPOSS2                                                        
         XC    LPOSS2,LPOSS2                                                    
ENT80    CLI   0(R4),X'FF'                                                      
         BNE   ENT100                                                           
ENT90    LA    R4,6(R4)                                                         
         LA    R8,1(R8)                                                         
         CR    R4,R5                                                            
         BNL   ENT120              END OF SAVETBL - EXIT                        
         B     ENT80                                                            
ENT100   CLC   2(1,R4),POSSMOVE                                                 
         BE    ENT110                                                           
         CLC   3(1,R4),POSSMOVE                                                 
         BE    ENT110                                                           
         CLC   4(1,R4),POSSMOVE                                                 
         BE    ENT110                                                           
         CLC   5(1,R4),POSSMOVE                                                 
         BNE   ENT90                                                            
ENT110   STC   R8,0(R6)                                                         
         LA    R6,1(R6)                                                         
         B     ENT90                                                            
ENT120   MVI   0(R6),X'FF'                                                      
         B     EXIT                EXIT                                         
         EJECT                                                                  
* CALCVALU - THIS SECTION OF CODE DETERMINES *                                  
*  THE VALUE OF EACH POSSIBLE MOVE CANDIDATE *                                  
         SPACE 1                                                                
CALCVALU NTR1                                                                   
         XC    BESTVALU,BESTVALU                                                
         XC    NEXTBEST,NEXTBEST                                                
         XC    MYMOVE,MYMOVE                                                    
         XC    MY2ND,MY2ND                                                      
         XC    CONDSP,CONDSP                                                    
         XC    VALUE,VALUE                                                      
         XC    SVARRAY,SVARRAY                                                  
         LA    R4,SVMOVES                                                       
         LA    R7,7                                                             
         LA    R8,CONDSP+4                                                      
         SPACE 1                                                                
CALC00   CLI   0(R4),42                                                         
         BH    CALC30              THIS MOVE IS IMPOSSIBLE, VALUE = 0           
         CLI   0(R4),35                                                         
         BH    CALC40              TOP OF THE COLUMN, DON'T WORRY ABOUT         
*                                   THE NEXT MOVE IN THE COLUMN                 
         ZIC   R5,0(R4)                                                         
         LA    R5,7(R5)                                                         
         STC   R5,POSSMOVE                                                      
         OI    FNDSW,1                                                          
         BAS   RE,FNDENTRY                                                      
         LA    R5,LPOSS2                                                        
CALC10   ZIC   R6,0(R5)                                                         
         MH    R6,=H'6'                                                         
         LA    R6,SAVETBL(R6)                                                   
         CLI   0(R6),1                                                          
         BE    CALC30              BAD MOVE, VALUE = 0                          
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    CALC40              END OF LPOSS2 ARRAY, GO CALCULATE            
*                                   THE VALUE OF THIS MOVE                      
         B     CALC10                                                           
         SPACE 1                                                                
CALC30   MVC   0(5,R8),=C'00000'                                                
         B     CALC70                                                           
         SPACE 1                                                                
CALC40   ZIC   R5,0(R4)                                                         
         STC   R5,POSSMOVE                                                      
         XC    FNDSW,FNDSW                                                      
         BAS   RE,FNDENTRY                                                      
         BAS   RE,DEQUALS                                                       
         BAS   RE,OEQUALS                                                       
         L     R9,VALUE                                                         
         SPACE 1                                                                
         CLI   SVDSPSW,C'Y'        IS AN EVALUATION WANTED?                     
         BNE   CALC50               NO.                                         
         CVD   R9,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R8),DUB                                                      
         FOUT  CONDSPH                                                          
         SPACE 1                                                                
CALC50   C     R9,BESTVALU                                                      
         BL    CALC70                                                           
         BNE   CALC60                                                           
         GOTO1 =V(RANDOM),PARAM,9,RR=RB                                         
         TM    PARAM+7,7                                                        
         BZ    CALC60                                                           
         ST    R9,NEXTBEST                                                      
         MVC   MY2ND,POSSMOVE                                                   
         B     CALC70                                                           
         SPACE 1                                                                
CALC60   MVC   NEXTBEST,BESTVALU                                                
         MVC   MY2ND,MYMOVE                                                     
         ST    R9,BESTVALU                                                      
         MVC   MYMOVE,POSSMOVE                                                  
CALC70   LA    R4,1(R4)                                                         
         LA    R8,6(R8)                                                         
         XC    VALUE,VALUE                                                      
         BCT   R7,CALC00                                                        
         SPACE 1                                                                
         CLI   MY2ND,0                                                          
         BE    EXIT                                                             
         GOTO1 =V(RANDOM),PARAM,9,RR=RB                                         
         TM    PARAM+7,8                                                        
         BZ    EXIT                EXIT                                         
         MVC   MYMOVE,MY2ND        USE 2ND BEST MOVE THIS TIME                  
         B     EXIT                EXIT                                         
         EJECT                                                                  
* OEQUALS - THIS SECTION OF CODE COMPUTES THE OFFENSE  *                        
*  FACTOR ASSOCIATED WITH THIS POSSIBLE MOVE CANDIDATE *                        
         SPACE 1                                                                
OEQUALS  NTR1                                                                   
         LA    R1,1                                                             
         LA    R5,ALLPOSS                                                       
         XR    R8,R8                                                            
OEQ00    LA    R7,4                                                             
         CLI   0(R5),X'FF'         IS THIS THE END OF ALLPOSS ARRAY?            
         BE    EXIT                 YES. EXIT                                   
         ZIC   R6,0(R5)            R6 HAS THE DISPLACEMENT INTO SAVETBL         
         MH    R6,=H'6'                                                         
         LA    R6,SAVETBL(R6)      R6 POINTS TO ENTRY IN SAVETBL                
         CLI   1(R6),X'FF'         IS THIS A WORTHLESS MOVE?                    
         BE    OEQ40                YES. TRY NEXT POSSIBILITY                   
         CLI   1(R6),4             HOW MANY SLOTS DO WE HAVE TO FILL            
         BE    OEQ20                IN THIS WIN POSSIBILITY?                    
         CLI   1(R6),3                                                          
         BE    OEQ10                                                            
         CLI   1(R6),2                                                          
         BE    *+20                                                             
         CLI   1(R6),1                                                          
         BE    *+8                                                              
         B     OEQ50               ERROR                                        
         MH    R1,=H'20'                                                        
         MH    R1,=H'20'                                                        
OEQ10    MH    R1,=H'10'                                                        
OEQ20    LA    R8,1(R7,R6)         R8 POINTS TO SLOT IN THIS WIN POSS.          
         ST    R8,SN               SAVE ADDRESS OF ELEMENT OF THIS              
*                                   WIN POSSIBILITY                             
         CLC   POSSMOVE,0(R8)      IS THIS OUR MOVE CANDIDATE?                  
         BE    OEQ30                                                            
         ZIC   R4,0(R8)                                                         
         BCTR  R4,R0                                                            
         ZIC   R9,SVBOARD(R4)                                                   
         STC   R9,DUB                                                           
         CLI   DUB,C'O'            DO WE ALREADY OCCUPY THIS SLOT?              
         BE    OEQ30                                                            
         EJECT                                                                  
* COMPUTE THE MULTIPLIER VALUE OF AN EMPTY SLOT *                               
         SPACE 1                                                                
         L     R4,SN                                                            
         L     R8,0(R4)                                                         
         BCTR  R8,R0                                                            
         SRDA  R8,32                                                            
         D     R8,=F'7'            R8 HAS THE COLUMN NUMBER - 1                 
         ZIC   R9,SVMOVES(R8)                                                   
         STH   R9,M                                                             
         L     R4,SN                                                            
         L     R8,0(R4)                                                         
         SH    R8,M                                                             
         SRDA  R8,32                                                            
         D     R8,=F'7'            R9 HAS THE NUMBER OF SLOTS THAT              
*                                   NEED TO BE FILLED BEFORE WE                 
*                                   CAN FILL THIS SLOT                          
         LA    R8,50                                                            
         SR    R8,R9                                                            
         STH   R8,N                N IS VALUE OF MULTIPLIER                     
         SPACE 1                                                                
         MH    R1,N                                                             
         AH    R1,=H'16384'                                                     
         SRA   R1,15                                                            
         CLI   HEAVYD,C'Y'                                                      
         BNE   *+16                                                             
         A     R1,VALUE                                                         
         ST    R1,VALUE                                                         
         LA    R1,1                                                             
         XC    M,M                                                              
         XC    N,N                                                              
OEQ30    BCT   R7,OEQ20                                                         
         CLI   HEAVYD,C'N'                                                      
         BNE   *+16                                                             
         A     R1,VALUE                                                         
         ST    R1,VALUE                                                         
OEQ40    LA    R1,1                                                             
         LA    R5,1(R5)                                                         
         B     OEQ00                                                            
         SPACE 1                                                                
OEQ50    DC    H'0'                                                             
         EJECT                                                                  
* DEQUALS - THIS SECTION OF CODE COMPUTES THE DEFENSE  *                        
*  FACTOR ASSOCIATED WITH THIS POSSIBLE MOVE CANDIDATE *                        
         SPACE 1                                                                
DEQUALS  NTR1                                                                   
         LA    R5,ALLPOSS                                                       
         XR    R8,R8                                                            
DEQ00    CLI   0(R5),X'FF'         IS THIS THE END OF ALLPOSS ARRAY?            
         BE    DEQ40                YES                                         
         ZIC   R6,0(R5)                                                         
         MH    R6,=H'6'                                                         
         ZIC   R9,SAVETBL(R6)                                                   
         STC   R9,DUB                                                           
         CLI   DUB,X'FF'           HAS THIS POSSIBLE LOSS ALREADY BEEN          
*                                   BLOCKED?                                    
         BE    DEQ30               YES                                          
         CLI   DUB,4               HOW MANY SLOTS DOES X NEED TO FILL           
         BE    DEQ30                IN THIS LOSS POSSIBILITY?                   
         CLI   DUB,3                                                            
         BE    DEQ20                                                            
         CLI   DUB,2                                                            
         BE    DEQ10                                                            
         CLI   DUB,1                                                            
         BNE   ERROR                                                            
         AH    R8,=H'4644'                                                      
DEQ10    AH    R8,=H'256'                                                       
DEQ20    AH    R8,=H'80'                                                        
DEQ30    AH    R8,=H'20'                                                        
         LA    R5,1(R5)                                                         
         B     DEQ00                                                            
         SPACE 1                                                                
DEQ40    A     R8,VALUE                                                         
         ST    R8,VALUE                                                         
         B     EXIT                EXIT                                         
         EJECT                                                                  
* DSPMOVE - THIS SECTION OF CODE UPDATES THE DISPLAY SCREEN *                   
         SPACE 1                                                                
DSPMOVE  NTR1                                                                   
         ZIC   R4,HISMOVE                                                       
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         BAS   RE,DSPMV                                                         
         MVI   0(R4),C'X'                                                       
         CLI   FLAG,C'X'                                                        
         BE    DSP00                                                            
         SPACE 1                                                                
         ZIC   R4,MYMOVE                                                        
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         BAS   RE,DSPMV                                                         
         MVI   0(R4),C'O'                                                       
         SPACE 1                                                                
         CLI   FLAG,0                                                           
         BE    DSP10                                                            
         SPACE 1                                                                
DSP00    XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(42),=CL42' NICE GAME - YOU WON. PLAY AGAIN?'              
         CLI   FLAG,C'X'                                                        
         BE    *+10                                                             
         MVC   CONMSG(42),=CL42' ** I WIN ** LET''S PLAY AGAIN'                 
         FOUT  CONMSGH                                                          
         XC    SVMOVES,SVMOVES                                                  
         B     EXIT                EXIT                                         
         SPACE 1                                                                
DSP10    MVC   CONMOVE+13(1),18(R2)                                             
         UNPK  CONMOVE+14(1),DUB   TELL THE USER WHERE THE CPU MOVED            
         FOUT  CONMOVEH                                                         
         B     EXIT                EXIT                                         
         EJECT                                                                  
DSPMV    DC    0H'0'                                                            
         BCTR  R4,R0                                                            
         SRDL  R4,32                                                            
         D     R4,=F'7'            R5 HAS ROW-1, R4 HAS COLUMN-1                
         LA    R5,1(R5)                                                         
         STC   R5,WORK                                                          
* FIND THE RIGHT ROW                                                            
         LA    R2,CONROWAH                                                      
         CLI   WORK,1                                                           
         BE    DSPMV10                                                          
         LA    R2,CONROWBH                                                      
         CLI   WORK,2                                                           
         BE    DSPMV10                                                          
         LA    R2,CONROWCH                                                      
         CLI   WORK,3                                                           
         BE    DSPMV10                                                          
         LA    R2,CONROWDH                                                      
         CLI   WORK,4                                                           
         BE    DSPMV10                                                          
         LA    R2,CONROWEH                                                      
         CLI   WORK,5                                                           
         BE    DSPMV10                                                          
         LA    R2,CONROWFH                                                      
         CLI   WORK,6                                                           
         BE    DSPMV10                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
DSPMV10  LA    R0,1(R4)            SET COLUMN NUMBER FOR DISPLAY                
         CVD   R0,DUB                                                           
         SPACE 1                                                                
         OI    DUB+7,X'0F'                                                      
         MH    R4,=H'6'            (COLUMN - 1) X 6 GIVES POSITION              
         LA    R4,14(R2,R4)        RETURN INSERT ADDRESS                        
         FOUT  (R2)                                                             
         BR    RE                                                               
         SPACE 3                                                                
ERROR    DC    H'0'                                                             
         EJECT                                                                  
WINTAB   DS    0D                                                               
         DC    AL1(04,04,01,02,03,04)                                           
         DC    AL1(04,04,02,03,04,05)                                           
         DC    AL1(04,04,03,04,05,06)                                           
         DC    AL1(04,04,04,05,06,07)                                           
         DC    AL1(04,04,08,09,10,11)                                           
         DC    AL1(04,04,09,10,11,12)                                           
         DC    AL1(04,04,10,11,12,13)                                           
         DC    AL1(04,04,11,12,13,14)                                           
         DC    AL1(04,04,15,16,17,18)                                           
         DC    AL1(04,04,16,17,18,19)                                           
         DC    AL1(04,04,17,18,19,20)                                           
         DC    AL1(04,04,18,19,20,21)                                           
         DC    AL1(04,04,22,23,24,25)                                           
         DC    AL1(04,04,23,24,25,26)                                           
         DC    AL1(04,04,24,25,26,27)                                           
         DC    AL1(04,04,25,26,27,28)                                           
         DC    AL1(04,04,29,30,31,32)                                           
         DC    AL1(04,04,30,31,32,33)                                           
         DC    AL1(04,04,31,32,33,34)                                           
         DC    AL1(04,04,32,33,34,35)                                           
         DC    AL1(04,04,36,37,38,39)                                           
         DC    AL1(04,04,37,38,39,40)                                           
         DC    AL1(04,04,38,39,40,41)                                           
         DC    AL1(04,04,39,40,41,42)                                           
         DC    AL1(04,04,01,09,17,25)                                           
         DC    AL1(04,04,09,17,25,33)                                           
         DC    AL1(04,04,17,25,33,41)                                           
         DC    AL1(04,04,02,10,18,26)                                           
         DC    AL1(04,04,10,18,26,34)                                           
         DC    AL1(04,04,18,26,34,42)                                           
         DC    AL1(04,04,03,11,19,27)                                           
         DC    AL1(04,04,11,19,27,35)                                           
         DC    AL1(04,04,08,16,24,32)                                           
         DC    AL1(04,04,16,24,32,40)                                           
         DC    AL1(04,04,07,13,19,25)                                           
         DC    AL1(04,04,13,19,25,31)                                           
         DC    AL1(04,04,19,25,31,37)                                           
         DC    AL1(04,04,06,12,18,24)                                           
         DC    AL1(04,04,12,18,24,30)                                           
         DC    AL1(04,04,18,24,30,36)                                           
         DC    AL1(04,04,05,11,17,23)                                           
         DC    AL1(04,04,11,17,23,29)                                           
         DC    AL1(04,04,14,20,26,32)                                           
         DC    AL1(04,04,20,26,32,38)                                           
         DC    AL1(04,04,01,08,15,22)                                           
         DC    AL1(04,04,08,15,22,29)                                           
         DC    AL1(04,04,15,22,29,36)                                           
         DC    AL1(04,04,02,09,16,23)                                           
         DC    AL1(04,04,09,16,23,30)                                           
         DC    AL1(04,04,16,23,30,37)                                           
         DC    AL1(04,04,03,10,17,24)                                           
         DC    AL1(04,04,10,17,24,31)                                           
         DC    AL1(04,04,17,24,31,38)                                           
         DC    AL1(04,04,04,11,18,25)                                           
         DC    AL1(04,04,11,18,25,32)                                           
         DC    AL1(04,04,18,25,32,39)                                           
         DC    AL1(04,04,05,12,19,26)                                           
         DC    AL1(04,04,12,19,26,33)                                           
         DC    AL1(04,04,19,26,33,40)                                           
         DC    AL1(04,04,06,13,20,27)                                           
         DC    AL1(04,04,13,20,27,34)                                           
         DC    AL1(04,04,20,27,34,41)                                           
         DC    AL1(04,04,07,14,21,28)                                           
         DC    AL1(04,04,14,21,28,35)                                           
         DC    AL1(04,04,21,28,35,42)                                           
         DC    AL1(04,04,04,10,16,22)                                           
         DC    AL1(04,04,04,12,20,28)                                           
         DC    AL1(04,04,15,23,31,39)                                           
         DC    AL1(04,04,21,27,33,39)                                           
         DC    6X'FF'                                                           
         SPACE 3                                                                
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GACONFFD                                                       
         EJECT                                                                  
         ORG   TB11FFD+1650                                                     
SVBOARD  DS    CL43                                                             
SVMOVES  DS    CL7                                                              
ALLPOSS  DS    CL30                                                             
LPOSS2   DS    CL15                                                             
SVDSPSW  DS    CL1                                                              
         DS    0D                                                               
SAVETBL  DS    CL420                                                            
SVTBLEND EQU   *                                                                
         SPACE 3                                                                
WORKD    DSECT                                                                  
         DS    0D                                                               
WORK     DS    2D                                                               
DUB      DS    D                                                                
SN       DS    F                                                                
VALUE    DS    F                                                                
BESTVALU DS    F                                                                
NEXTBEST DS    F                                                                
RVALUE   DS    F                                                                
M        DS    H                                                                
N        DS    H                                                                
SVARRAY  DS    7F                                                               
FLAG     DS    CL1                                                              
FNDSW    DS    CL1                                                              
HISMOVE  DS    CL1                                                              
MYMOVE   DS    CL1                                                              
MYMOVESW DS    CL1                                                              
POSSMOVE DS    CL1                                                              
QWIKSW   DS    CL1                                                              
REENTER  DS    CL1                                                              
UPMOVE   DS    CL1                                                              
WHOMOVED DS    CL1                                                              
HEAVYD   DS    CL1                                                              
MY2ND    DS    CL1                                                              
SWITCH   DS    CL1                                                              
         DS    0F                                                               
PARAM    DS    6F                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GACON00   05/01/02'                                      
         END                                                                    
