*          DATA SET GABAK00    AT LEVEL 007 AS OF 05/01/02                      
*PHASE TB0B00A                                                                  
         TITLE 'TB0B00 - BACKGAMMON GAME'                                       
         PRINT NOGEN                                                            
TB0B00   CSECT                                                                  
         NMOD1 BACKDX-BACKD,**BACK**,R9                                         
         USING BACKD,RC                                                         
         L     RA,4(R1)                                                         
         USING TB0BFFD,RA                                                       
         MVC   ACOMFACS,16(R1)                                                  
         EJECT                                                                  
*              MAIN GAME LOGIC                                                  
         SPACE 3                                                                
         CLI   FT,0                FIRST THROW OF GAME                          
         BNE   ML4                                                              
         MVI   FT,1                                                             
         MVC   OFFX(26),START                                                   
         MVC   OFFO(26),START                                                   
         SPACE 2                                                                
ML2      BAS   RE,SHAKE                                                         
         CLC   DICE(1),DICE+1      IF HE IS HIGHER THAN ME, HE STARTS           
         BL    EXITB                                                            
         BE    ML2                 (EQUAL - THROW AGAIN)                        
         MVC   DUB(4),MYDICE       OTHERWISE I START                            
         MVC   MYDICE(4),DICE                                                   
         MVC   DICE(4),DUB                                                      
         BAS   RE,MYMOVE                                                        
         B     EXITA                                                            
         SPACE 2                                                                
ML4      BAS   RE,VALMOVE          CHECK HIS MOVE                               
         BE    ML6                                                              
         OI    BAKMOVEH+6,X'40'    ERRORS                                       
         B     XIT                                                              
         SPACE 2                                                                
ML6      MVC   OFFX(26),HISBOARD                                                
         MVC   OFFO(26),MYBOARD                                                 
         CLI   OFFX,15             OK - DID HE WIN                              
         BE    EXITC                                                            
         BAS   RE,MYMOVE                                                        
         CLI   OFFO,15                                                          
         BE    EXITD                                                            
         B     EXITA                                                            
         EJECT                                                                  
*              MESSAGES AND EXITS                                               
         SPACE 3                                                                
MESSA    DC    CL30'I THREW N-N AND HAVE MOVED'                                 
MESSB    DC    CL30'YOU THREW M-M.  YOUR MOVE'                                  
MESSC    DC    CL30'I THREW N-N AND WON.'                                       
MESSD    DC    CL30'YOU WON - CONGRATULATIONS'                                  
MESSE    DC    CL30'THANK YOU FOR THE GAME'                                     
MESSF    DC    CL30'YOU GET TO START THIS GAME'                                 
         SPACE 2                                                                
EXITA    LA    R2,MESSA            NORMAL EXIT                                  
         LA    R3,MESSB                                                         
         MVC   8(1,R2),MYDICE                                                   
         OI    8(R2),X'F0'                                                      
         MVC   10(1,R2),MYDICE+1                                                
         OI    10(R2),X'F0'                                                     
         SPACE 2                                                                
EXITA2   MVC   10(1,R3),DICE                                                    
         OI    10(R3),X'F0'                                                     
         MVC   12(1,R3),DICE+1                                                  
         OI    12(R3),X'F0'                                                     
         B     EXALL                                                            
         SPACE 2                                                                
EXITB    LA    R2,MESSF            HIM STARTING                                 
         LA    R3,MESSB                                                         
         B     EXITA2                                                           
         SPACE 2                                                                
EXITC    LA    R2,MESSD            HE WINS - RATS                               
         LA    R3,MESSE                                                         
         MVI   FT,0                                                             
         B     EXALL                                                            
         SPACE 2                                                                
EXITD    LA    R2,MESSC            I WON - YEAH                                 
         LA    R3,MESSE                                                         
         MVI   FT,0                                                             
         MVC   8(1,R2),MYDICE                                                   
         OI    8(R2),X'F0'                                                      
         MVC   10(1,R2),MYDICE+1                                                
         OI    10(R2),X'F0'                                                     
         SPACE 2                                                                
EXALL    FOUT  BAKHEADH,SPACES,60                                               
         FOUT  BAKMIDH,SPACES,60                                                
         FOUT  BAKMOVEH,SPACES,27                                               
         MVC   BAKHEAD(30),0(R2)                                                
         MVC   BAKMID(30),0(R3)                                                 
         LA    R2,BAKMOVEH                                                      
         CLC   0(5,R3),=C'THANK'                                                
         BNE   *+8                                                              
         LA    R2,BAKSERVH                                                      
         OI    6(R2),X'40'                                                      
         BAS   RE,OUTBOARD                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MAKE MY MOVE                                          
         SPACE 3                                                                
MYMOVE   NTR1                                                                   
         BAS   RE,SHAKE                                                         
         B     MYMOVED                                                          
         SPACE 2                                                                
MYMOVEB  NTR1                                                                   
         SPACE 2                                                                
MYMOVED  BAS   RE,BILDLIST                                                      
         XC    BESTSCOR,BESTSCOR                                                
         XC    BESTNMOV,BESTNMOV                                                
         LA    R2,225                                                           
         LA    R8,225                                                           
         CLC   MYDICE(1),MYDICE+1                                               
         BNE   MYMOVE2                                                          
         LA    R8,1000                                                          
         MH    R2,=H'225'                                                       
         SPACE 2                                                                
MYMOVE2  MVC   SAVEBX,OFFX                                                      
         MVC   SAVEBO,OFFO                                                      
         MVC   SAVELIST,MYLIST                                                  
         XC    THISNMOV,THISNMOV                                                
         SPACE 2                                                                
MYMOVE4  LA    R3,MYDICE                                                        
         LA    R4,2                                                             
         CLC   MYDICE(1),MYDICE+1                                               
         BNE   *+8                                                              
         LA    R4,4                                                             
         LR    R7,R2                                                            
         SPACE 2                                                                
MYMOVE6  SR    R6,R6                                                            
         D     R6,=F'15'                                                        
         LA    R6,SAVELIST(R6)                                                  
         ST    R6,PARAS                                                         
         ST    R3,PARAS+4                                                       
         BAS   RE,MOVE                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,MYMOVE6                                                       
         XC    THISSCOR,THISSCOR                                                
         BAS   RE,VALUE                                                         
         CLC   THISNMOV(8),BESTNMOV                                             
         BNH   MYMOVE8                                                          
         MVC   BESTNMOV(8),THISNMOV                                             
         MVC   BESTBX,SAVEBX                                                    
         MVC   BESTBO,SAVEBO                                                    
         SPACE 2                                                                
MYMOVE8  BCTR  R2,0                                                             
         BCT   R8,MYMOVE2                                                       
         MVC   OFFX(26),BESTBX                                                  
         MVC   OFFO(26),BESTBO                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOCATE MY MEN                                         
         SPACE 3                                                                
BILDLIST NTR1                                                                   
         LA    R2,OFFO                                                          
         LA    R3,MYLIST                                                        
         LA    R4,26                                                            
         SPACE 2                                                                
BL2      SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    BL6                                                              
         SPACE 2                                                                
BL4      STC   R4,0(R3)                                                         
         LA    R3,1(R3)                                                         
         BCT   R5,BL4                                                           
         SPACE 2                                                                
BL6      LA    R2,1(R2)                                                         
         BCT   R4,BL2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO MAKE ONE OF MY POSSIBLE MOVES                        
         SPACE 3                                                                
MOVE     NTR1                                                                   
         LM    R2,R3,PARAS         A(PIECE NO.) A(DICE NO.)                     
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,0(R3)                                                         
         CH    R4,=H'26'           CANT MOVE A MAN THATS OFF                    
         BE    MOVENO                                                           
         CLI   SAVEBO+25,0         IF I HAVE A MAN ON THE BAR                   
         BE    MOVE2                                                            
         CH    R4,=H'01'              I MUST MOVE HIM FIRST                     
         BNE   MOVENO                                                           
         SPACE 2                                                                
MOVE2    LA    R6,SAVEBO+26        A(PIECE TO BE MOVED)                         
         SR    R6,R4                                                            
         CLI   0(R6),0             DO I HAVE ANY PIECES THERE                   
         BE    MOVENO                                                           
         LA    R1,0(R4,R5)                                                      
         CH    R1,=H'26'           ARE WE MOVING MEN OFF                        
         BL    MOVE4                                                            
         OC    SAVEBO+7(19),SAVEBO+7                                            
         BNZ   MOVENO              NOT ALLOWED UNLESS ALL ARE HOME              
         CH    R1,=H'26'                                                        
         BE    MOVEME                                                           
         OC    1(6,R6),1(R6)       OK TO BEAR OFF WITH HIGHER NUMBER            
         BNZ   MOVENO              IF NO MEN ARE FURTHER OUT                    
         SH    R1,=H'26'                                                        
         SR    R5,R1               (ADJUST NO TO EXACTLY REACH OFF)             
         B     MOVEME                                                           
         SPACE 2                                                                
MOVE4    LA    R7,SAVEBX-1         WORK OUT WHERE THE MOVE WOULD                
         AR    R7,R1               TAKE ME IN HIS BOARD                         
         CLI   0(R7),1             HOW MANY MEN HAS HE THERE                    
         BH    MOVENO              2 OR MORE - NOT ON                           
         BL    MOVEME              0 IS OK                                      
         MVI   0(R7),0             IF 1 I KNOCK HIM OFF                         
         SR    R1,R1                                                            
         IC    R1,SAVEBX+25                                                     
         LA    R1,1(R1)            ADD 1 TO HIS BAR                             
         STC   R1,SAVEBX+25                                                     
         SPACE 2                                                                
MOVEME   SR    R1,R1               -1 FROM POINT                                
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         STC   R1,0(R6)                                                         
         SR    R6,R5               +1 TO POINT                                  
         IC    R1,0(R6)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R6)                                                         
         L     R1,THISNMOV         +1 N'MOVES                                   
         LA    R1,1(R1)                                                         
         ST    R1,THISNMOV                                                      
         LA    R1,0(R4,R5)                                                      
         STC   R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 2                                                                
MOVENO   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALUE THE BOARD                                       
         SPACE 3                                                                
VALUE    NTR1                                                                   
         LA    R2,SAVEBO           LOOK AT THE BOARD FROM MY SIDE               
         LA    R3,SAVEBX                                                        
         BAS   RE,VALUE2                                                        
         L     R4,=F'10000'                                                     
         A     R4,THISSCOR         ADD THIS IN                                  
         LA    R2,SAVEBX                                                        
         LA    R3,SAVEBO                                                        
         BAS   RE,VALUE2           NOW CHECK HIM                                
         S     R4,THISSCOR         AND SUBTRACT THAT                            
         ST    R4,THISSCOR                                                      
         B     XIT                                                              
         SPACE 2                                                                
VALUE2   NTR1                                                                   
         LA    R4,3000             KEEP SCORE IN R4                             
         LA    R5,25(R3)                                                        
         LR    R6,R2                                                            
         SPACE 2                                                                
VALUE4   CLI   0(R5),0             FIND THE ADDRESS OF MOST BACKWARD            
         BNE   VALUE5                   OPPOSING MAN (R5) IN MY BOARD           
         LA    R6,1(R6)                                                         
         BCT   R5,VALUE4                                                        
         SPACE 2                                                                
VALUE5   LR    R5,R2               NOW CHECKING HOW MANY MEN ARE ON             
         LA    R7,26                   EACH POINT                               
         SPACE 2                                                                
VALUE6   SR    R1,R1                                                            
         IC    R1,0(R5)                                                         
         MR    R0,R7               MULTIPLY BY DISTANCE FROM START              
         AR    R4,R1               ADD THIS TO SCORE                            
*                                                                               
         CLI   0(R5),1             IF I HAVE ONLY ONE MAN ON A POINT            
         BNE   VALUE8                                                           
         CR    R5,R6               AND HE HAS A MAN BEHIND ME, THIS             
         BL    VALUE8              IS A BLOT                                    
         SH    R4,=H'20'                                                        
         LR    R0,R5                                                            
         SR    R0,R6                                                            
         CH    R0,=H'6'            AND ITS A WORSE BLOT                         
         BH    VALUE8              IF HE IS WITHIN 6                            
         SH    R4,=H'20'                                                        
         SPACE 2                                                                
VALUE8   CLI   0(R5),2             IF I HAVE AT LEAST TWO ON A POINT            
         BL    VALUE10             THIS IS A GOOD FEATURE                       
         LA    R4,30(R4)                                                        
         CLI   0(R5),3                                                          
         BH    VALUE10                                                          
         LA    R4,10(R4)           I LIKE 2 ON A POINT MORE                     
         BL    VALUE10                                                          
         LA    R4,10(R4)             AND 3 ON A POINT EVEN BETTER               
         SPACE 2                                                                
VALUE10  LA    R5,1(R5)                                                         
         BCT   R7,VALUE6                                                        
         CLI   5(R2),1             I LIKE A BUILDER ON 5 POINT                  
         BL    VALUE12                                                          
         LA    R4,10(R4)                                                        
         BE    VALUE12                                                          
         LA    R4,10(R4)           AND 2 EVEN BETTER                            
         SPACE 2                                                                
VALUE12  CLI   7(R2),1             SAME APPLIES TO MY BAR POINT                 
         BL    VALUE14                                                          
         LA    R4,10(R4)                                                        
         BE    VALUE14                                                          
         LA    R4,10(R4)                                                        
         SPACE 2                                                                
VALUE14  CLI   18(R2),1            AND TO HIS BAR POINT                         
         BL    VALUE16                                                          
         LA    R4,10(R4)                                                        
         BE    VALUE16                                                          
         LA    R4,10(R4)                                                        
         SPACE 2                                                                
VALUE16  CLI   25(R3),0            IF HE HAS A MAN OFF THE BOARD                
         BE    VALUE20                                                          
         LA    R1,1(R2)            COUNT THE NUMBER OF POINTS                   
         LA    R0,6                I HAVE MADE                                  
         SR    RE,RE                                                            
         SPACE 2                                                                
VALUE18  CLI   0(R1),2                                                          
         BL    *+8                                                              
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VALUE18                                                       
         CH    RE,=H'6'            IF I HAVE 6 HE IS SHUT OUT                   
         BNE   *+8                                                              
         LA    R4,200(R4)          SO BIG BONUS                                 
         MH    RE,=H'5'                                                         
         AR    R4,RE                                                            
         SPACE 2                                                                
VALUE20  DS    0H                                                               
         ST    R4,THISSCOR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO VALIDATE HIS MOVE                                    
         SPACE 3                                                                
VALMOVE  NTR1                                                                   
         LA    R2,BAKMOVEH                                                      
         MVC   HISBOARD,OFFX                                                    
         MVC   MYBOARD,OFFO                                                     
         MVC   HISDICE,DICE                                                     
         XC    HISNMOV,HISNMOV                                                  
         CLC   8(4,R2),=C'PASS'                                                 
         BE    VAL32                                                            
         CLC   8(4,R2),=C'NONE'                                                 
         BE    VAL32                                                            
         CLI   5(R2),0                                                          
         BE    VAL32                                                            
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),PARAS,(R2),(4,BLOCK),C',=,-'                                
         LA    R3,BLOCK                                                         
         SR    R4,R4                                                            
         IC    R4,PARAS+4                                                       
         MVI   ERROR,1                                                          
         LTR   R4,R4                                                            
         BZ    VALX                                                             
         ST    R4,HISNMOV                                                       
         SPACE 2                                                                
VAL2     MVI   FROM,25             FROM MUST BE EITHER BAR                      
         SR    R5,R5                                                            
         IC    R5,0(R3)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL10'BAR'                                              
         BE    VAL4                                                             
         MVI   ERROR,3                                                          
         L     R5,4(R3)                                                         
         LTR   R5,R5               OR 1-24                                      
         BZ    VALX                                                             
         CH    R5,=H'24'                                                        
         BH    VALX                                                             
         STC   R5,FROM                                                          
         SPACE 2                                                                
VAL4     MVI   TO,0                TO MUST BE EITHER OFF                        
         SR    R5,R5                                                            
         IC    R5,1(R3)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R3),=CL10'OFF'                                              
         BE    VAL6                                                             
         MVI   ERROR,6                                                          
         L     R5,8(R3)                                                         
         LTR   R5,R5               ALLOW 0 ALSO FOR OFF                         
         BZ    VAL6                                                             
         CH    R5,=H'24'           OR 1-24                                      
         BH    VALX                                                             
         STC   R5,TO                                                            
         SPACE 2                                                                
VAL6     SR    R5,R5                                                            
         IC    R5,FROM                                                          
         SR    R6,R6                                                            
         IC    R6,TO                                                            
         SR    R5,R6               DISTANCE MUST BE 1-6                         
         MVI   ERROR,8                                                          
         BNP   VALX                                                             
         STC   R5,JUMP                                                          
         SR    R0,R0               UNLESS SUM OF 2 DICE=JUMP                    
         SR    R1,R1                                                            
         IC    R0,HISDICE                                                       
         IC    R1,HISDICE+1                                                     
         AR    R1,R0                                                            
         CR    R5,R1                                                            
         BNE   VAL7                                                             
         CLC   HISNMOV,=F'1'                                                    
         BNE   VAL7                                                             
         MVC   HISNMOV,=F'2'                                                    
         LA    R4,2                                                             
         CLC   BLOCK+12(3),=C'BAR'                                              
         BNE   *+10                                                             
         MVC   BLOCK+4(4),=F'25'                                                
         L     R1,BLOCK+4          FUDGE BLOCK TO LOOK LIKE 2 MOVES             
         SR    R1,R0                                                            
*                                  SEE HOW MANY MEN I HAVE ON POINT             
*                                  IF I HAVE 2 OR MORE SWITCH HIS MOVES         
         LA    R6,25                                                            
         SR    R6,R1                                                            
         LA    R6,MYBOARD(R6)                                                   
         CLI   0(R6),2                                                          
         BL    VAL6B                                                            
         L     R1,BLOCK+4                                                       
         ZIC   R0,HISDICE+1                                                     
         SR    R1,R0                                                            
         SPACE 2                                                                
VAL6B    MVC   BLOCK+40(4),BLOCK+8                                              
         ST    R1,BLOCK+8                                                       
         ST    R1,BLOCK+36                                                      
         B     VAL2                                                             
         SPACE 2                                                                
VAL7     CLI   JUMP,6                                                           
         BH    VALX                                                             
         EJECT                                                                  
*              VALIDATION - SPECIAL FROM CHECKS                                 
         SPACE 3                                                                
         CLI   HISBOARD+25,0       ANY MEN ON BAR                               
         BE    VAL8                                                             
         CLI   FROM,25             MUST BE FIRST MOVE                           
         BE    VAL8                                                             
         MVI   ERROR,2                                                          
         B     VALX                                                             
         SPACE 2                                                                
VAL8     SR    R5,R5               DOES HE HAVE A MAN THERE                     
         IC    R5,FROM                                                          
         LA    R5,HISBOARD(R5)                                                  
         MVI   ERROR,4                                                          
         CLI   0(R5),0                                                          
         BE    VALX                                                             
         SR    R6,R6               YES - SUBTRACT 1                             
         IC    R6,0(R5)                                                         
         BCTR  R6,0                                                             
         STC   R6,0(R5)                                                         
         EJECT                                                                  
*              VALIDATION - SPECIAL TO CHECKS                                   
         SPACE 3                                                                
         SR    R5,R5                                                            
         IC    R5,TO                                                            
         LA    R6,25                                                            
         SR    R6,R5                                                            
         LA    R5,HISBOARD(R5)                                                  
         LA    R6,MYBOARD(R6)      SEE HOW MANY I HAVE ON POINT                 
         CLI   TO,0                                                             
         BE    VAL20                                                            
         MVI   ERROR,7                                                          
         CLI   0(R6),1                                                          
         BH    VALX                2 OR MORE IS AN ERROR                        
         BL    VAL10                                                            
         MVI   0(R6),0             1 IS A BLOT SO REMOVE                        
         SR    R1,R1                                                            
         IC    R1,MYBOARD+25                                                    
         LA    R1,1(R1)            AND ADD TO MY BAR                            
         STC   R1,MYBOARD+25                                                    
         SPACE 2                                                                
VAL10    SR    R1,R1                                                            
         IC    R1,0(R5)            ADD 1 TO HIS TO POINT                        
         LA    R1,1(R1)                                                         
         STC   R1,0(R5)                                                         
         LA    R5,HISDICE                                                       
         LA    R6,4                                                             
         MVI   ERROR,8                                                          
         SPACE 2                                                                
VAL12    CLC   JUMP,0(R5)          CHECK HE THREW DICE TO COVER MOVE            
         BE    VAL14                                                            
         LA    R5,1(R5)                                                         
         BCT   R6,VAL12                                                         
         B     VALX                                                             
         SPACE 2                                                                
VAL14    MVI   0(R5),0             (CANT BE USED AGAIN)                         
         B     VAL30                                                            
         EJECT                                                                  
*              SPECIAL ROUTINES FOR BEARING OFF                                 
         SPACE 3                                                                
VAL20    MVI   ERROR,5                       MUST HAVE ALL PIECES               
         OC    HISBOARD+7(19),HISBOARD+7     WITHIN HOME BOARD                  
         BNZ   VALX                                                             
         LA    R1,HISDICE                                                       
         LA    R0,4                                                             
         SPACE 2                                                                
VAL22    CLC   JUMP,0(R1)          CHECK EXACT MATCH                            
         BE    VAL26                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VAL22                                                         
         MVI   ERROR,8                                                          
         SR    R7,R7                                                            
         IC    R7,FROM                                                          
         LA    R7,HISBOARD+1(R7)                                                
         OC    0(6,R7),0(R7)                                                    
         BNZ   VALX                                                             
         LA    R1,HISDICE                                                       
         LA    R0,4                                                             
         SPACE 2                                                                
VAL24    CLC   0(1,R1),JUMP        IF HE HAS NO MEN FURTHER OUT THAN            
         BH    VAL26               FROM HE IS ALLOWED TO BEAR OFF IF            
         LA    R1,1(R1)            HE HAS A DICE HIGHER THAN JUMP               
         BCT   R0,VAL24                                                         
         B     VALX                                                             
         SPACE 2                                                                
VAL26    MVI   0(R1),0             OK TO BEAR OFF                               
         SR    R1,R1                                                            
         IC    R1,HISBOARD                                                      
         LA    R1,1(R1)                                                         
         STC   R1,HISBOARD                                                      
         EJECT                                                                  
*              CHECK WE HAVE USED THE DICE                                      
         SPACE 3                                                                
VAL30    LA    R3,32(R3)                                                        
         BCT   R4,VAL2                                                          
         SPACE 2                                                                
VAL32    MVI   ERROR,0                                                          
         MVC   SAVMYDIC,MYDICE                                                  
         MVC   MYDICE,DICE                                                      
         MVC   HISSAVX,OFFX                                                     
         MVC   HISSAVO,OFFO                                                     
         MVC   OFFO(26),HISSAVX    INVERT THE BOARDS                            
         MVC   OFFX(26),HISSAVO                                                 
         BAS   RE,MYMOVEB          FIND OUT (HIS) BEST MOVE                     
         MVI   ERROR,9                                                          
         MVC   OFFO(26),HISSAVO                                                 
         MVC   OFFX(26),HISSAVX                                                 
         CLC   BESTNMOV,HISNMOV    DID HE MAKE ENOUGH MOVES                     
         MVC   MYDICE,SAVMYDIC                                                  
         BH    VALX                                                             
         MVI   ERROR,0                                                          
         EJECT                                                                  
*              EXITS FROM VALIDATION                                            
         SPACE 2                                                                
VALX     SR    R4,R4                                                            
         IC    R4,ERROR                                                         
         MH    R4,=H'25'                                                        
         LA    R4,ERRLIST(R4)                                                   
         MVC   BAKSUB,0(R4)                                                     
         OI    BAKSUBH+6,X'80'                                                  
         LA    R5,BAKSUB                                                        
         LA    R6,25                                                            
         SPACE 2                                                                
VALX2    CLC   0(2,R5),=C'TO'                                                   
         BNE   *+10                                                             
         MVC   0(3,R5),22(R3)                                                   
         CLC   0(3,R5),=C'FRO'                                                  
         BNE   *+10                                                             
         MVC   0(3,R5),12(R3)                                                   
         LA    R5,1(R5)                                                         
         BCT   R6,VALX2                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),PARAS,0,X'D9000A0D',0                                       
         CLI   PARAS+4,X'FF'       GET A(SQUASHER)                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,PARAS                                                         
         GOTO1 (RF),PARAS,BAKSUB,25                                             
         CLI   ERROR,0                                                          
         B     XIT                                                              
         SPACE 2                                                                
ERRLIST  DS    0H                                                               
         DC    CL25'ENTER YOUR MOVES HERE'        0                             
         DC    CL25'YOUR MOVE IS INVALID'         1                             
         DC    CL25'YOU MUST MOVE BAR FIRST'      2                             
         DC    CL25'FRO IS NOT VALID'             3                             
         DC    CL25'YOU HAVE NO PIECES ON FRO'    4                             
         DC    CL25'YOU CANT BEAR OFF YET'        5                             
         DC    CL25'TO  IS INVALID'               6                             
         DC    CL25'TO  IS OCCUPIED BY ME'        7                             
         DC    CL25'NO DICE MATCH FRO - TO '      8                             
         DC    CL25'INSUFFICIENT MOVES'      9                                  
         EJECT                                                                  
*              ROUTINE TO SHAKE DICE                                            
         SPACE 3                                                                
SHAKE    NTR1                                                                   
SHAKE1   XC    RANDOM,RANDOM                                                    
         TIME  TU                                                               
         STH   R0,RANDOM           GET SEED FROM CLOCK                          
         MVC   DUB(4),RANDOM                                                    
         OC    DUB(4),=X'80000000'                                              
         CLC   DUB(4),=X'80000000'                                              
         BE    SHAKE1              AVOID BAD VALUE                              
         LA    R2,4                                                             
         LA    R3,DICE                                                          
         SPACE 2                                                                
*   PRODUCE A RANDOM NUMBER FROM THE CLOCK                                      
SHAKE2   L     R5,RANDOM                                                        
         M     R4,=F'65541'                                                     
         LPR   R5,R5                                                            
         ST    R5,RANDOM                                                        
         LA    R5,12                                                            
         M     R4,RANDOM                                                        
         LA    R4,1(R4)            R4 IS NOW BETWEEN 1 AND 6                    
*                                                                               
         STC   R4,0(R3)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,SHAKE2                                                        
*                                                                               
         MVC   MYDICE,DICE+2                                                    
         XC    DICE+2(2),DICE+2                                                 
         XC    MYDICE+2(2),MYDICE+2                                             
         CLC   DICE(1),DICE+1      CHECK FOR DOUBLES                            
         BNE   *+10                                                             
         MVC   DICE+2(2),DICE                                                   
         CLC   MYDICE(1),MYDICE+1                                               
         BNE   *+10                                                             
         MVC   MYDICE+2(2),MYDICE                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT THE BOARD                                      
         SPACE 3                                                                
OUTBOARD NTR1                                                                   
         BAS   RE,COPY             PUT SCREEN'S PATTERN TO W.S                  
         LA    R2,OFFS             FIRST CLEAR X'S BOARD                        
         LA    R3,PATTERNS                                                      
         LA    R4,BOARDX                                                        
         BAS   RE,FILL                                                          
         SPACE 1                                                                
         LA    R4,BOARDO           AND O'S BOARD                                
         BAS   RE,FILL                                                          
         SPACE 1                                                                
         LA    R2,OFFC             PUT IN THE DOTS                              
         LA    R3,PATTERNC                                                      
         BAS   RE,FILL                                                          
         SPACE 1                                                                
         LA    R2,OFFO             OUTPUT O'S BOARD                             
         LA    R3,PATTERNO                                                      
         BAS   RE,FILL                                                          
         SPACE 1                                                                
         LA    R2,OFFX             OUTPUT X'S BOARD                             
         LA    R3,PATTERNX                                                      
         LA    R4,BOARDX                                                        
         BAS   RE,FILL                                                          
         SPACE 1                                                                
         LA    R2,WSLINA           OUTPUT LINES TO TWA                          
         LA    R3,BAKLINAH                                                      
         LA    R4,13                                                            
         SPACE 2                                                                
OUT2     CLC   0(57,R2),8(R3)                                                   
         BE    OUT4                                                             
         MVC   8(57,R3),0(R2)                                                   
         OI    6(R3),X'80'                                                      
         SPACE 2                                                                
OUT4     LA    R2,57(R2)                                                        
         LA    R3,65(R3)                                                        
         BCT   R4,OUT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR OUTPUTTING BOARD                         
         SPACE 3                                                                
COPY     NTR1                                                                   
         LA    R2,BAKLINAH         COPY TWA TO WS                               
         LA    R3,WSLINA                                                        
         LA    R4,13                                                            
         SPACE 2                                                                
COPY2    MVC   0(57,R3),8(R2)                                                   
         LA    R2,65(R2)                                                        
         LA    R3,57(R3)                                                        
         BCT   R4,COPY2                                                         
         B     XIT                                                              
         SPACE 2                                                                
FILL     NTR1                                                                   
         LA    R5,26                                                            
         SPACE 2                                                                
FILL2    SR    R6,R6                                                            
         IC    R6,0(R2)       NUMBER TO OUTPUT ON THIS POINT                    
         LTR   R6,R6                                                            
         BZ    FILL8                                                            
         SR    R7,R7                                                            
         IC    R7,0(R4)       LINE NUMBER (1,13)                                
         BCTR  R7,0                                                             
         MH    R7,=H'57'                                                        
         SR    R8,R8                                                            
         IC    R8,1(R4)       COLUMN NUMBER                                     
         SH    R8,=H'7'                                                         
         LA    R7,WSLINA(R7)  DISPLACE INTO WS BLOCK                            
         AR    R7,R8                                                            
         LA    R8,DISPTABA                                                      
         CLI   0(R4),1                                                          
         BE    FILL4                                                            
         LA    R8,DISPTABB                                                      
         SPACE 2                                                                
FILL4    MVC   0(1,R7),0(R3)                                                    
         A     R7,0(R8)                                                         
         LA    R8,4(R8)                                                         
         BCT   R6,FILL4                                                         
         SPACE 2                                                                
FILL8    LA    R2,1(R2)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,FILL2                                                         
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              TABLES                                                           
         SPACE 3                                                                
START    DC    AL1(0)                                                           
         DC    AL1(0,0,0,0,0,5)                                                 
         DC    AL1(0,3,0,0,0,0)                                                 
         DC    AL1(5,0,0,0,0,0)                                                 
         DC    AL1(0,0,0,0,0,2)                                                 
         DC    AL1(0)                                                           
         SPACE 2                                                                
BOARDX   DC    AL1(13,9)                                                        
         DC    AL1(13,20,13,23,13,26,13,29,13,32,13,35)                         
         DC    AL1(13,45,13,48,13,51,13,54,13,57,13,60)                         
         DC    AL1(1,60,1,57,1,54,1,51,1,48,1,45)                               
         DC    AL1(1,35,1,32,1,29,1,26,1,23,1,20)                               
         DC    AL1(1,40)                                                        
         SPACE 2                                                                
BOARDO   DC    AL1(1,9)                                                         
         DC    AL1(1,20,1,23,1,26,1,29,1,32,1,35)                               
         DC    AL1(1,45,1,48,1,51,1,54,1,57,1,60)                               
         DC    AL1(13,60,13,57,13,54,13,51,13,48,13,45)                         
         DC    AL1(13,35,13,32,13,29,13,26,13,23,13,20)                         
         DC    AL1(13,40)                                                       
         SPACE 2                                                                
PATTERNO DC    C'0'                                                             
PATTERNX DC    C'X'                                                             
PATTERNC DC    C'.'                                                             
PATTERNS DC    C' '                                                             
         SPACE 2                                                                
OFFC     DC    AL1(0)                                                           
         DC    24AL1(5)                                                         
         DC    AL1(0)                                                           
OFFS     DC    26AL1(15)                                                        
         SPACE 2                                                                
SPACES   DC    CL60' '                                                          
         SPACE 2                                                                
DISPTABA DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'-286'                                                          
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'57'                                                            
         DC    F'-283'                                                          
         DC    F'57'                                                            
         DC    F'57'                                                            
         SPACE 1                                                                
DISPTABB DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'284'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         DC    F'287'                                                           
         DC    F'-57'                                                           
         DC    F'-57'                                                           
         EJECT                                                                  
       ++INCLUDE GABAKFFD                                                       
         SPACE 3                                                                
OFFO     DS    CL1                                                              
HOMEO    DS    CL6                                                              
         DS    CL6                                                              
         DS    CL6                                                              
BACKO    DS    CL6                                                              
BARO     DS    CL1                                                              
         SPACE 2                                                                
OFFX     DS    CL1                                                              
HOMEX    DS    CL6                                                              
         DS    CL6                                                              
         DS    CL6                                                              
BACKX    DS    CL6                                                              
BARX     DS    CL1                                                              
         SPACE 2                                                                
DICE     DS    CL4                                                              
MYDICE   DS    CL4                                                              
FT       DS    CL1                                                              
         EJECT                                                                  
*              WORKING STORAGE                                                  
         SPACE 3                                                                
BACKD    DSECT                                                                  
DUB      DS    D                                                                
ACOMFACS DS    A                                                                
RANDOM   DS    F                                                                
SAVELIST DS    CL15                                                             
SAVEBX   DS    CL26                                                             
SAVEBO   DS    CL26                                                             
BESTNMOV DS    F                                                                
BESTSCOR DS    F                                                                
THISNMOV DS    F                                                                
THISSCOR DS    F                                                                
MYLIST   DS    CL15                                                             
MYBOARD  DS    CL26                                                             
HISBOARD DS    CL26                                                             
HISNMOV  DS    F                                                                
HISSAVX  DS    CL26                                                             
HISSAVO  DS    CL26                                                             
SAVMYDIC DS    CL4                                                              
BLOCK    DS    10CL32                                                           
PARAS    DS    6F                                                               
ERROR    DS    CL1                                                              
FROM     DS    CL1                                                              
TO       DS    CL1                                                              
JUMP     DS    CL1                                                              
HISDICE  DS    CL4                                                              
BESTBX   DS    CL26                                                             
BESTBO   DS    CL26                                                             
WSLINA   DS    741C                                                             
BACKDX   EQU   *                                                                
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007GABAK00   05/01/02'                                      
         END                                                                    
