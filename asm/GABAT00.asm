*          DATA SET GABAT00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0900A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'BATTLESHIPS AND CRUISERS GAME'                                  
         PRINT NOGEN                                                            
BATTSCRS CSECT                                                                  
         NMOD1 100,GABAT00                                                      
         USING BATTEMP,RC          WORKING STORAGE                              
         L     RA,4(R1)                                                         
         USING BATSAVE,RA          TWA AND SAVE STORAGE                         
         L     RE,16(R1)                                                        
         USING COMFACSD,RE                                                      
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         ST    R1,PARAML           PARAMETER LIST                               
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING BATTSCRS+4096,R8    EXTRA BASE REGISTER                          
         B     RSTART                                                           
         EJECT                                                                  
*                                                                               
*              TO BE USED FOR ADDRESS LOADS FOR ADDRESS CONVERSION              
*                                                                               
PROWS    DS    0F                  FOR PLAYER CONVERSION                        
         LA    RE,BATRW1PH                                                      
         LA    RE,BATRW2PH                                                      
         LA    RE,BATRW3PH                                                      
         LA    RE,BATRW4PH                                                      
         LA    RE,BATRW5PH                                                      
         LA    RE,BATRW6PH                                                      
         LA    RE,BATRW7PH                                                      
         LA    RE,BATRW8PH                                                      
         LA    RE,BATRW9PH                                                      
         LA    RE,BATR10PH                                                      
         SPACE 3                                                                
MROWS    DS    0F                  FOR COMPUTER CONVERSION                      
         LA    RE,BATRW1MH                                                      
         LA    RE,BATRW2MH                                                      
         LA    RE,BATRW3MH                                                      
         LA    RE,BATRW4MH                                                      
         LA    RE,BATRW5MH                                                      
         LA    RE,BATRW6MH                                                      
         LA    RE,BATRW7MH                                                      
         LA    RE,BATRW8MH                                                      
         LA    RE,BATRW9MH                                                      
         LA    RE,BATR10MH                                                      
         EJECT                                                                  
RSTART   MVI   BATHDR,C' '                                                      
         MVC   BATHDR+1(59),BATHDR                                              
         CLI   BATSHOTH+5,X'00'                                                 
         BNE   *+14                                                             
         MVC   BATHDR(L'ERRORH),ERRORH                                          
         B     ERRORXIT                                                         
         CLI   GIP,C'T'            IS GAME IN PROGRESS?                         
         BE    GAMINPRG                                                         
         GOTO1 VSCANNER,PLIST,BATSHOTH,SCANBLCK,C',=,X'                         
         CLI   PLIST+4,0                                                        
         BE    GAMERRA             ERROR IF NIL/INVALID INPUT                   
         TM    SCANBLCK+2,X'80'    IS IT A NUMERIC INPUT                        
         BZ    NEWGAME             IF NOT, ASSUME IT'S A NEW GAME               
         B     GAMESTRT            IF YES, ASSUME IT'S THE QUADRANT SZE         
GAMINPRG GOTO1 VSCANNER,PLIST,BATSHOTH,(5,SCANBLCK)                             
         CLI   PLIST+4,0                                                        
         BNE   *+14                                                             
         MVC   BATHDR(L'ERROR1),ERROR1                                          
         B     ERRORXIT                                                         
         CLC   SCANBLCK+12(3),=C'RES'                                           
         BE    PQUITS              PLAYER HAS RESIGNED                          
         B     MAINGAME                                                         
PQUITS   MVC   BATHDR(L'DRPOUT),DRPOUT                                          
         B     PLOST+6                                                          
ILOST    MVC   BATHDR(L'YOUWON),YOUWON                                          
         LH    R1,PWINS                                                         
         LA    R1,1(R1)                                                         
         STH   R1,PWINS            UPDATE PLAYER WINS AND DISPLAY               
         EDIT  (R1),(2,BATG+2),WRK=WORK+1                                       
         OI    BATGH+6,X'80'                                                    
         TM    WORK,X'60'          WORK = X'60' FOR A DRAW                      
         BC    1,PLOST+6                                                        
         B     ENDGAME                                                          
DRAW     MVC   BATHDR(L'ADRAW),ADRAW                                            
         B     ILOST+6                                                          
PLOST    MVC   BATHDR(L'YOULOST),YOULOST                                        
         LH    R1,MWINS                                                         
         LA    R1,1(R1)                                                         
         STH   R1,MWINS            UPDATE MY WINS AND DISPLAY                   
         EDIT  (R1),(2,BATG+18)                                                 
         OI    BATGH+6,X'80'                                                    
ENDGAME  MVI   PLAYER,C'P'         DISPLAY MY FLEET FROM MFLTABLE               
         XC    PLIST+4(3),PLIST+4                                               
         LA    R1,MFLTABLE                                                      
ENDGDISP CLI   0(R1),X'00'                                                      
         BE    ENDNDLP                                                          
         L     R2,MAXSIZE                                                       
         LA    R3,2                                                             
         MVC   SHIPTYPE,0(R1)                                                   
ENDGLOOP LA    R4,0(R1,R3)                                                      
         CLI   0(R4),X'FF'                                                      
         BE    ENDGLPND                                                         
         MVC   PLIST+7(1),0(R4)                                                 
         BAS   RE,DISPSHT                                                       
         LA    R3,1(R3)                                                         
         BCT   R2,ENDGLOOP                                                      
ENDGLPND LA    R1,L'MFLTABLE(R1)                                                
         B     ENDGDISP                                                         
ENDNDLP  XC    PHITS(4),PHITS                                                   
         XC    NOHITS,NOHITS                                                    
         MVI   GIP,C'E'                                                         
         MVC   BATHDR+9(L'NEXTGAME),NEXTGAME                                    
         OI    BATSHOTH+6,X'40'                                                 
ERRORXIT OI    BATHDRH+6,X'80'                                                  
         OI    BATSHOTH+6,X'40'                                                 
XITXIT   XMOD1 1                                                                
         EJECT                                                                  
NEWGAME  XC    GIP,GIP                                                          
         SR    R1,R1               UNSET PROTECT BIT IN ATTRIBUTE BYTE          
         LA    R2,4                 FOR EACH LINE OF PLAYER'S QUADRANT          
         LA    R3,36                ( USA REQUIREMENT )                         
         SR    R5,R5                                                            
NEWGUNP  LA    R4,PROWS(R1)                                                     
         EX    R5,0(R4)                                                         
         NI    1(RE),X'DF'         CLEAR X'20' BIT                              
         BXLE  R1,R2,NEWGUNP                                                    
         MVI   BATLAST+1,X'01'                                                  
         SR    R3,R3               AND FORCE TRANSMISSION OF ALL FIELDS         
         LA    R2,BATHDRH                                                       
NEWGTRAN CLI   0(R2),0                                                          
         BE    *+20                                                             
         OI    6(R2),X'80'                                                      
         IC    R3,0(R2)                                                         
         LA    R2,0(R2,R3)                                                      
         B     NEWGTRAN                                                         
         CLI   SCANBLCK+12,C'Y'                                                 
         BE    *+16                                                             
         CLI   SCANBLCK+12,C'O'                                                 
         BE    *+8                                                              
         MVI   SCANBLCK+12,C'N'    IF GAME IS NOT IN PROGRESS AND               
         SR    R1,R1                                                            
         CH    R1,MWINS             BATSHOT ISNT NUM, DEFAULT ACTION IS         
         BNZ   NEWGCONT             BATSHOT=NEW.                                
         CH    R1,PWINS                                                         
         BNZ   NEWGCONT            IF THIS IF FIRST GAME OF SESSION,            
         MVC   SHIPTAB(20),SHTBDFLT SET UP DEFAULT RULES                        
         MVC   LIMIT(4),LMTDFLT                                                 
         MVC   NOCOLS(4),COLSDFLT                                               
         MVC   NOROWS(4),ROWSDFLT                                               
         MVC   ADDFAC(8),ADFCDFLT                                               
         MVC   HITSTWIN(2),HTWNDFLT                                             
         B     NEWCLEAR                                                         
NEWGCONT OI    BATHITSH+6,X'80'    INITIALISE HITS, MISSES AND SINKINGS         
         MVC   BATHITS+2(2),SPACES                                              
         MVC   BATHITS+18(2),SPACES                                             
         OI    BATMSSSH+6,X'80'                                                 
         MVC   BATMSSS+2(2),SPACES                                              
         MVC   BATMSSS+18(2),SPACES                                             
         OI    BATSNKSH+6,X'80'                                                 
         MVC   BATSNKS+2(2),SPACES                                              
         MVC   BATSNKS+18(2),SPACES                                             
         LA    R1,2                                                             
         LA    R2,BATYLSH           AND SHOT DESCRIPTIONS                       
         OI    6(R2),X'80'                                                      
         MVC   8(22,R2),SPACES                                                  
         OI    64(R2),X'80'                                                     
         MVC   66(44,R2),SPACES                                                 
         LA    R2,166(R2)                                                       
         BCT   R1,*-24                                                          
NEWCLEAR BAS   RE,CONTENT          RESET REMAINING FLEETS AND                   
         BAS   RE,QUADSZE           QUADRANT SIZE,                              
         MVC   BATSHOT,SPACES                                                   
         CLI   BATMQSZ+18,C' '                                                  
         BE    *+14                                                             
         MVC   BATSHOT(5),BATMQSZ+18                                            
         B     *+10                                                             
         MVC   BATSHOT(4),BATMQSZ+19                                            
         OI    BATSHOTH+6,X'80'                                                 
         MVC   BATENT,STARTENT                                                  
         OI    BATENTH+6,X'80'                                                  
         BAS   RE,QUADSET          AND INITIALISED QUADRANTS                    
         CLI   SCANBLCK+12,C'Y'    GENERATE A PLAYER'S FLEET IF ASKED           
         BNE   NEWGNOTY                                                         
         LH    RF,HITSTWIN         HAVING CHECKED THAT FLEET TO                 
         L     RE,RATIO             QUADRANT SIZE RATIO IS VALID                
         MR    RE,RE                                                            
         C     RF,LIMIT                                                         
         BNH   *+14                                                             
         MVC   BATHDR(L'ERROR3),ERROR3                                          
         B     ERRORXIT                                                         
         BAS   RE,FLEGEN                                                        
         BZ    NEWGERRA                                                         
         BAS   RE,FLESETP                                                       
         B     GAMESAME                                                         
NEWGNOTY MVC   BATHDR,STARTHD                                                   
         CLI   SCANBLCK+12,C'N'                                                 
         BE    ERRORXIT                                                         
         BAS   RE,FLESETP          SET UP PREVIOUS (BATSHOT=OLD) OR             
         B     ERRORXIT             GENERATED (BATSHOT=YOU) P'S FLEET           
NEWGERRA MVC   BATHDR(L'ERRORF),ERRORF                                          
         B     ERRORXIT                                                         
         EJECT                                                                  
GAMESTRT L     R1,SCANBLCK+4       COME HERE IF GAME NOT IN PROGRESS            
         C     R1,COLSDFLT          AND BATSHOT IS NUMERIC                      
         BH    GAMERRA             CHECK THAT QUADRANT IS 1-12X1-10             
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         B     GAMERRA                                                          
         TM    SCANBLCK+3,X'80'                                                 
         BZ    GAMERRA                                                          
         L     R1,SCANBLCK+8                                                    
         C     R1,ROWSDFLT                                                      
         BH    GAMERRA                                                          
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         B     GAMERRA                                                          
         CLC   SCANBLCK+4(4),NOCOLS CHECK WHETHER QUADRANT DIMENSIONS           
         BNE   *+14                 HAVE CHANGED                                
         CLC   SCANBLCK+8(4),NOROWS                                             
         BE    GAMESAME                                                         
         MVC   NOCOLS(4),SCANBLCK+4 IF SO STORE THEM                            
         MVC   NOROWS(4),SCANBLCK+8                                             
         XC    PLIST(1),PLIST                                                   
         L     R5,NOCOLS                                                        
         M     R4,NOROWS                                                        
         ST    R5,LIMIT             AND LIMIT (= COLS X ROWS)                   
         LA    R1,ADDFAC            AND SET UP 2 SETS OF DIRECTION              
         L     R2,NOCOLS             MODIFIERS BASED ON NO OF COLS              
         MVI   0(R1),X'01'                                                      
         STC   R2,1(R1)                                                         
         BCT   R2,*+14             IF NOCOLS=1, PROPAGATE '01' THRUOUT          
         MVC   ADDFAC+2(6),ADDFAC+1 DIRECTION MODIFIERS (ADDFAC(8))             
         B     *+22                                                             
         STC   R2,2(R1)                                                         
         LA    R2,2(R2)                                                         
         STC   R2,3(R1)                                                         
         MVC   ADDFAC+4(4),ADDFAC                                               
         BAS   RE,QUADSZE          AND QUADRANT SZES                            
GAMESAME BAS   RE,FLEETRD                                                       
         BZ    GAMERRB             READ AND VALIDATE PLAYER'S FLEET             
         MVI   GIP,C'T'                                                         
         SR    R1,R1               SET PROTECT BIT IN ATTRIBUTE BYTE            
         LA    R2,4                 FOR EACH LINE OF PLAYER'S QUADRANT          
         LA    R3,36                ( USA REQUIREMENT )                         
         SR    R5,R5                                                            
GAMEPROT LA    R4,PROWS(R1)                                                     
         EX    R5,0(R4)                                                         
         OI    1(RE),X'20'                                                      
         BXLE  R1,R2,GAMEPROT                                                   
         MVI   BATLAST+1,X'01'                                                  
         SR    R3,R3               AND FORCE TRANSMISSION OF ALL FIELDS         
         LA    R2,BATHDRH                                                       
GAMETRAN CLI   0(R2),0                                                          
         BE    *+20                                                             
         OI    6(R2),X'80'                                                      
         IC    R3,0(R2)                                                         
         LA    R2,0(R2,R3)                                                      
         B     GAMETRAN                                                         
         BAS   RE,QUADSET                                                       
         BAS   RE,FLESETP           AND SET IT UP IN TWA                        
         BAS   RE,FLEGEN           GENERATE MY FLEET                            
         BAS   RE,CONTENT          SET UP FLEET CONTENTS ETC IN TWA             
         MVC   BATHDR(L'INITST),INITST                                          
         MVC   BATENT(15),=C'ENTER YOUR SHOT'                                   
         OI    BATENTH+6,X'80'                                                  
         MVC   BATSHOT,SPACES                                                   
         OI    BATSHOTH+6,X'80'                                                 
         MVC   MYPQUADA(121),AVAILSHT                                           
         L     R1,LIMIT                                                         
         STC   R1,MYPQUADA                                                      
         B     ERRORXIT                                                         
GAMERRA  MVC   BATHDR(L'ERRORG),ERRORG                                          
         B     ERRORXIT                                                         
GAMERRB  MVC   BATHDR,WORK                                                      
         OI    BATRW1PH+6,X'40'                                                 
         OI    BATHDRH+6,X'80'                                                  
         B     XITXIT                                                           
         EJECT                                                                  
*                                  COME HERE FOR PLAYER'S SHOTS                 
MAINGAME MVI   UNSCANP,C' '                                                     
         MVC   UNSCANP+1(199),UNSCANP                                           
         SR    R5,R5                R5= NO OF LINES IN P'S UNSCAN BLOCK         
         SR    R2,R2                R2=                MY                       
         MVI   WORKA,X'00'                                                      
         MVC   WORKA+1(1),PLIST+4                                               
         LA    R1,SCANBLCK                                                      
         LA    R3,UNSCANP                                                       
         LA    R4,UNSCANM                                                       
MAINEXT  ST    R3,AUNSCNP                                                       
         ST    R4,AUNSCNM                                                       
         CLI   0(R1),X'00'         ANY MORE SHOTS TO CHECK?                     
         BE    MAINSET                                                          
         MVI   PLAYER,C'P'                                                      
         MVC   0(3,R3),12(R1)      MOVE INPUT COORDINATES TO OUTPUT BLK         
         MVC   PLIST(3),12(R1)     IF YES,                                      
         BAS   RE,VCONADR           VALIDATE AND CONVERT TO 0-119               
         BZ    ADERRA                                                           
         BM    ADERRB                                                           
*                                  PSHTQD CHECK                                 
*              PLIST CONTAINS PLAYER'S SHOT                                     
*              PSHTQD IS A BIT LIST OF UP TO 15 BYTES = 120 BITS                
*                0 BIT = A MISS OR REPEAT SHOT (TREATED AS A MISS)              
*                1 BIT = A HIT                                                  
         SR    R6,R6                                                            
         L     R7,PLIST                                                         
         D     R6,=F'8'            R7=BYTE(0-14),R6=BIT(0-7)                    
         LA    R6,PSHTMASK(R6)                                                  
         LA    R7,PSHTQD(R7)                                                    
         MVC   WORK(1),0(R6)                                                    
         NC    WORK(1),0(R7)                                                    
         BZ    PMISSED                                                          
         XC    0(1,R7),WORK        PLAYER HAS A HIT SO ZEROISE THIS BIT         
         MVC   HITORMSS(4),HIT                                                  
         B     *+10                                                             
PMISSED  MVC   HITORMSS(4),MISS                                                 
         BAS   RE,INCHM            INCREMENT HITS OR MISSES                     
         CLI   HITORMSS,C'M'                                                    
         BE    *+26                IF IT'S A HIT                                
         BAS   RE,POSSNK            CHECK WHETHER IT'S A SINKING                
         MVC   SHIPTYPE,PSHPTYPE    AND IF SO UPDATE SINKINGS AND               
         BNZ   *+16                                                             
         BAS   RE,INCSINK            REMAINING FLEETS                           
         B     *+8                                                              
         MVI   SHIPTYPE,C' '                                                    
         MVC   PLIST+4(4),PLIST                                                 
         BAS   RE,DISPSHT          DISPLAY PLAYER'S SHOT                        
         BAS   RE,MYSHOT                                                        
         LA    R2,1(R2)            UPDATE COUNT AND POINTERS FOR MY             
         L     R4,AUNSCNM           UNSCAN BLOCK                                
         LA    R4,L'UNSCANM(R4)                                                 
         B     *+18                                                             
ADERRA   MVI   10(R3),C'?'         MOVE ? OR ?? TO P'S UNSCANBLOCK              
         B     *+10                                                             
ADERRB   MVC   10(2,R3),=C'??'                                                  
         LA    R5,1(R5)            UPDATE COUNT AND POINTERS FOR MY             
         LA    R1,L'SCANBLCK(R1)    UNSCAN BLOCK                                
         L     R3,AUNSCNP                                                       
         LA    R3,L'UNSCANP(R3)                                                 
         NI    WORK,X'00'          SET BITS IN WORK TO INDICATE GAME            
         LH    R9,HITSTWIN          ENDINGS                                     
         CH    R9,PHITS                                                         
         BNE   *+8                                                              
         OI    WORK,X'40'                                                       
         CH    R9,MHITS                                                         
         BNE   *+8                                                              
         OI    WORK,X'20'                                                       
         OI    WORK,X'00'          IF GAME HAS ENDED                            
         BNZ   MAINSET                                                          
         CH    R5,=H'5'            OR 5 SHOTS HAVE BEEN MADE GO TO              
         BE    MAINSET              SET UP SHOT RESULTS                         
         LH    R9,WORKA                                                         
         SH    R9,=H'1'                                                         
         STH   R9,WORKA                                                         
         BNZ   MAINEXT                                                          
MAINSET  MVC   BATMSHT(44),SPACES                                               
         MVC   BATPHMS(44),SPACES                                               
         OI    BATYLSH+6,X'80'                                                  
         OI    BATPHMSH+6,X'80'                                                 
         OI    BATMLSH+6,X'80'                                                  
         OI    BATMSHTH+6,X'80'                                                 
         CH    R5,=H'1'            IF ONLY ONE SHOT , SET UP LONG SHOT          
         BNE   SEVERAL              RESULT DESCRIPTIONS/ ERROR MESSAGES         
         MVC   BATYLS(22),SPACES   OTHERWISE USE UNSCAN.                        
         MVC   BATMLS(22),SPACES                                                
         CLI   UNSCANP+10,C'?'                                                  
         BNE   NOTERROR                                                         
         CLI   UNSCANP+11,C' '                                                  
         BNE   *+14                                                             
         MVC   BATHDR(L'ERROR1),ERROR1                                          
         B     *+10                                                             
         MVC   BATHDR(L'ERROR2),ERROR2                                          
         B     ERRORXIT                                                         
NOTERROR MVC   BATPHMS(16),=C'YOUR SHOT WAS A '                                 
         MVC   BATPHMS+16(4),UNSCANP+10                                         
         CLI   UNSCANP+10,C'S'                                                  
         BNE   *+10                                                             
         MVC   BATPHMS+20(3),=C'ING'                                            
         MVC   BATMSHT(18),=C'MY SHOT WAS     A '                               
         MVC   BATMSHT+12(3),UNSCANM                                            
         MVC   BATMSHT+18(4),UNSCANM+10                                         
         CLI   UNSCANM+10,C'S'                                                  
         BNE   ENDCHECK                                                         
         MVC   BATMSHT+22(3),=C'ING'                                            
         B     ENDCHECK                                                         
SEVERAL  MVC   BATYLS(22),=C'YOUR LAST SHOTS WERE -'                            
         MVC   BATMLS(22),=C'MY LAST SHOTS WERE   -'                            
         GOTO1 VUNSCAN,PLIST,((R5),UNSCANP),BATPHMSH                            
         LTR   R2,R2                                                            
         BZ    ENDCHECK                                                         
         GOTO1 VUNSCAN,PLIST,((R2),UNSCANM),BATMSHTH                            
ENDCHECK TM    WORK,X'60'                                                       
         BC    8,ERRORXIT                                                       
         BC    1,DRAW                                                           
         TM    WORK,X'40'                                                       
         BC    1,ILOST                                                          
         B     PLOST                                                            
         EJECT                                                                  
*              ADRVCON                                                          
*                                                                               
*              CONVERTS LINEAR COORDINATES (0-119) TO ALPHA OR A/N              
*              INPUTS:                                                          
*                     PLIST+4  1F = LINEAR COORDINATES (0-119)                  
*                     COORDS   1C = A (ALPHA) OR N (ALPHA/NUMERIC)              
*              OUTPUTS:                                                         
*                     PLIST    3C = ALPHA OR A/N COORDS (+ TRAILING SP)         
*                                                                               
ADRVCON  NTR1                                                                   
         MVI   PLIST+2,C' '                                                     
         SR    R4,R4                                                            
         L     R5,PLIST+4                                                       
         D     R4,NOCOLS                                                        
         LA    R6,COLLTRS                                                       
         LA    R6,0(R4,R6)                                                      
         MVC   PLIST(1),0(R6)                                                   
         CLI   COORDS,C'A'                                                      
         BNE   ADRVNUM                                                          
         LA    R6,ROWTRS                                                        
         LA    R6,0(R5,R6)                                                      
         MVC   PLIST+1(1),0(R6)                                                 
         B     ADRVCONX                                                         
ADRVNUM  LA    R5,1(R5)                                                         
         CVD   R5,DUB                                                           
         C     R5,=F'10'                                                        
         BE    ADRVTWO                                                          
         UNPK  PLIST+1(1),DUB                                                   
         OI    PLIST+1,X'F0'                                                    
         B     ADRVCONX                                                         
ADRVTWO  UNPK  PLIST+1(2),DUB                                                   
         OI    PLIST+2,X'F0'                                                    
ADRVCONX XIT1                                                                   
         EJECT                                                                  
*              CONTENT                                                          
*                                                                               
*              SETS UP FLEET CONTENTS AND REMAINING FLEETS IN TWA FROM          
*               SHIPTAB ENTRIES AND SETS P'S  AND MY COUNTS OF SHIPS            
*               AFLOAT IN SHIPTAB.                                              
*              INPUTS:                                                          
*                     SHIPTAB  20C  = TABLE OF SHIP DETAILS                     
*              OUTPUTS:                                                         
*                                                                               
*                                                                               
CONTENT  NTR1                                                                   
         LA    R1,SHIPTAB                                                       
CONTLOOP CLI   0(R1),X'00'                                                      
         BE    CONTENTX                                                         
         MVC   3(2,R1),2(R1)                                                    
         SR    R7,R7                                                            
         IC    R7,2(R1)                                                         
         LTR   R7,R7                                                            
         BZ    *+8                                                              
         IC    R9,1(R1)                                                         
         CLI   0(R1),C'B'                                                       
         BNE   *+16                                                             
         LA    R4,BATBTSPH                                                      
         LA    R5,BATFCBTH                                                      
         B     CONTSIZE                                                         
         CLI   0(R1),C'C'                                                       
         BNE   *+16                                                             
         LA    R4,BATCRSSH                                                      
         LA    R5,BATFCCRH                                                      
         B     CONTSIZE                                                         
         CLI   0(R1),C'F'                                                       
         BNE   *+16                                                             
         LA    R4,BATFGTSH                                                      
         LA    R5,BATFCFGH                                                      
         B     CONTSIZE                                                         
         LA    R4,BATDTRSH                                                      
         LA    R5,BATFCDRH                                                      
CONTSIZE SR    R6,R6                                                            
         IC    R6,2(R1)                                                         
         EDIT  (R6),(2,8(R5))                                                   
         LTR   R6,R6                                                            
         BNZ   *+10                                                             
         MVC   8(2,R5),=C'NO'                                                   
         OI    6(R5),X'80'                                                      
         MVC   10(2,R4),8(R5)                                                   
         MVC   26(2,R4),8(R5)                                                   
         OI    6(R4),X'80'                                                      
         SR    R6,R6                                                            
         IC    R6,1(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   *+20                                                             
         MVC   23(1,R5),0(R1)                                                   
         MVC   24(3,R5),=X'7DE240'     V=  C''S '                               
         B     CONTLPND                                                         
         MVC   23(4,R5),SPACES                                                  
         MVC   23(1,R5),0(R1)                                                   
         LA    R5,1(R5)                                                         
         BCT   R6,*-10                                                          
CONTLPND LA    R1,L'SHIPTAB(R1)                                                 
         B     CONTLOOP                                                         
CONTENTX STC   R9,SMALLEST                                                      
         XIT1                                                                   
         EJECT                                                                  
*              DISPSHT                                                          
*                                                                               
*              DISPLAYS A CHARACTER IN THE PLAYER'S OR MY QUADRANT              
*              INPUTS:                                                          
*                     PLAYER   1C = M (MY SHOT TO P'S QUADRANT) OR P            
*                     SHIPTYPE 1C = CHARACTER TO BE DISPLAYED                   
*                     PLIST+4  1F = ADDRESS IN QUADRANT (0-119)                 
*              OUTPUTS:                                                         
*                     NONE                                                      
*                                                                               
DISPSHT  NTR1                                                                   
         L     R5,PLIST+4                                                       
         SR    R4,R4                                                            
         D     R4,NOCOLS                                                        
         SLL   R5,2                                                             
         LA    R2,PROWS(R5)                                                     
         CLI   PLAYER,C'M'                                                      
         BE    *+8                                                              
         LA    R2,40(R2)           BUMP TO EXECUTABLE LA'S FOR MY QUAD,         
         SR    R3,R3                                                            
         EX    R3,0(R2)                                                         
         SLL   R4,1                                                             
         OI    6(RE),X'80'                                                      
         LA    R3,8(R4,RE)                                                      
         CLI   PLAYER,C'P'                                                      
         BNE   *+12                                                             
         CLI   0(R3),C'+'                                                       
         BNE   DISPSHTX                                                         
         MVC   0(1,R3),SHIPTYPE                                                 
DISPSHTX XIT1                                                                   
         EJECT                                                                  
*              FLEETRD                                                          
*                                                                               
*              READS,VALIDATES AND DISPLAYS A PLAYER'S FLEET, DERIVING          
*               FROM IT DATA TO SET UP SHIPTAB AND DISPLAY FLEET                
*               CONTENTS AND REMAINING FLEETS.                                  
*                                                                               
FLEETRD  NTR1                                                                   
         MVI   PLAYER,C'P'                                                      
         MVC   PREVHIT(16),SPACES                                               
         XC    WORK(240),WORK                                                   
         SR    R1,R1               READ QUADRANT INTO WORK,COUNTING             
         LA    R2,4                 SHIP CHARACTERS IN RF                       
         L     R3,NOROWS                                                        
         BCTR  R3,0                                                             
         SLL   R3,2                R1,2,3= BXLE REGISTERS FOR LOOP TO           
         LA    R4,WORK             READ PLAYER'S QUADRANT,UP TO ROW             
         SR    RF,RF               LIMIT,INTO WORK                              
         LA    R6,2                                                             
         L     R7,NOCOLS                                                        
         BCTR  R7,0                R5,6,7= BXLE REGISTERS FOR INNER             
         SLL   R7,1                LOOP TO READ A ROW,UP TO COLUMN              
FLEEROLP LA    R9,PROWS(R1)        LIMIT                                        
         SR    RE,RE                                                            
         EX    RE,0(R9)                                                         
         LA    RE,6(RE)                                                         
         SR    R5,R5                                                            
FLEECOLP LA    RE,0(R6,RE)                                                      
         CLI   0(RE),0                                                          
         BE    *+30                                                             
         CLI   0(RE),C'+'                                                       
         BE    *+22                                                             
         CLI   0(RE),C' '                                                       
         BE    *+14                                                             
         MVC   0(1,R4),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         BXLE  R5,R6,FLEECOLP                                                   
         BXLE  R1,R2,FLEEROLP                                                   
         LTR   RF,RF                                                            
         BNZ   *+20                                                             
         MVC   WORK(L'ERROR4),ERROR4                                            
         MVC   WORK+L'ERROR4(14),SPACES                                         
         B     FLEERR4             ERROR4 IF NO SHIPS INPUT                     
         STH   RF,HITSTWIN                                                      
         L     RE,RATIO                                                         
         MR    RE,RE               ERRORA IF FLEET-QUADRANT SIZE                
         C     RF,LIMIT             RATIO EXCEEDS LIMIT                         
         BH    FLEERRA                                                          
*                                                                               
*                                                                               
         LA    R1,SHIPTAB          INITIALISE SHIPTAB TO 4X5C ENTRIES           
         LA    R2,4                 CONTAINING:                                 
         LA    R3,SHIPCHAR             TYPE           1C= B,C,D OR F            
FLEESHTB MVC   0(1,R1),0(R3)           SIZE           1C=X'FF'                  
         MVI   1(R1),X'FF'             NO IN FLEET    1C= ZERO                  
         XC    2(3,R1),2(R1)           NO AFLOAT (P'S)1C= ZERO                  
         LA    R1,L'SHIPTAB(R1)        NO AFLOAT (MY )1C= ZERO                  
         LA    R3,1(R3)                                                         
         BCT   R2,FLEESHTB                                                      
*                                                                               
*                                                                               
         L     R1,LIMIT            READ WORK LOOKING FOR SHIPS                  
         SR    R3,R3                                                            
FLEEWRKD LA    R4,WORK(R3)                                                      
         CLI   0(R4),X'00'                                                      
         BE    FLEEWKND                                                         
         LA    R5,WORKA(R3)        IF A SHIP IS IN WORKA IT HAS ALREADY         
         CLC   0(1,R4),0(R5)        BEEN CHECKED                                
         BE    FLEEWKND                                                         
         ST    R3,PLIST                                                         
         LA    R6,SHIPTAB                                                       
         CLI   0(R6),X'00'                                                      
         BE    FLEERRB             ERROR B IF SHIP CHAR NOT IN SHIPTAB          
         CLC   0(1,R4),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,L'SHIPTAB(R6)                                                 
         B     *-22                                                             
         ST    R6,PLIST+8          STORE SHIPTAB ENTRY AD IN PLIST+8            
         XC    PLIST+12(4),PLIST+12      SHIP SIZE IN             +12           
         MVC   PLIST+15(1),1(R6)                                                
         MVC   PLIST+16(1),0(R6)                                                
         ST    R3,PREVHIT          STORE SHIP ADDRESS(0-119) IN PREVHIT         
         LA    R9,1                INCREMENT HITS TO SINK                       
         LA    R5,4                                                             
         SR    RE,RE                                                            
         LR    RF,R3                                                            
         D     RE,NOCOLS                                                        
         SR    RF,RF                                                            
FLEECHDR IC    RF,ADDFAC(R5)       SEARCH FOR AN ADJACENT SHIP CHAR             
         LA    R2,0(R3,RF)          IN 4 POSITIVE DIRECTIONS                    
FLEETHRD C     R2,LIMIT             CHECKING FOR QUADRANT LIMIT                 
         BNL   FLEECHND                                                         
         LR    R7,R2                                                            
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         LR    R7,R6                                                            
         SR    R7,RE                                                            
         LPR   R7,R7                                                            
         CH    R7,=H'1'                                                         
         BH    FLEECHND            AND WRAP-ROUND                               
         LA    R7,WORK(R2)                                                      
         CLI   0(R7),X'00'                                                      
         BNE   FLEESHIP                                                         
FLEECHND BCT   R5,FLEECHDR         NO ADJACENT SHIP CHARACTER                   
         C     R9,PLIST+12                                                      
         BE    FLEECADJ            CHECK SHIP SIZE AND UPDATE SHIPTAB           
         CLI   PLIST+15,X'FF'       ENTRY IF SIZE WAS 'FF'                      
         BNE   FLEERRC             ERROR C IF SHIP SIZES INCONSISTENT           
         L     RF,PLIST+8                                                       
         STC   R9,1(RF)                                                         
         B     FLEECADJ                                                         
FLEESHIP CLC   0(1,R7),PLIST+16                                                 
         BNE   FLEERRE             ERROR E IF SHIP CHAR DIFFERS                 
         C     R9,MAXSIZE                                                       
         BE    FLEERRD             ERROR D IF SHIP SIZE EXCEEDS MAX             
         SLL   R9,2                                                             
         ST    R2,PREVHIT(R9)      STORE ADDRESS OF 2ND AND SUBSEQUENT          
         SRL   R9,2                 SHIP CHARACTERS IN PREVHIT(N) AND           
         LA    R9,1(R9)             INCREMENT HITS TO SINK                      
         AR    R2,RF                                                            
         LA    R5,1                BRANCH BACK TO LOOK FOR A FURTHER            
         LR    RE,R6                SHIP CHARACTER IN THIS DIRECTION.           
         B     FLEETHRD                                                         
*                                  HAVING STORED A COMPLETE SHIP IN             
*                                   PREVHIT(N),COME HERE TO CHECK FOR           
FLEECADJ SR    R2,R2                ADJACENT SHIPS                              
         LA    R4,4                                                             
         LA    R5,12                                                            
FLEEADJL L     R6,PREVHIT(R2)      FOR EACH SHIP CHARACTER                      
         C     R6,SPACES                                                        
         BE    *+20                                                             
         ST    R6,PLIST+4                                                       
         BAS   RE,SHPSRCH          CHECK FOR SURROUNDING ZEROES IN              
         BNZ   FLEERRE              WORKA                                       
         BXLE  R2,R4,FLEEADJL                                                   
         SH    R2,=H'4'            MOVE SHIP TO WORKA                           
         BM    *+22                                                             
         L     R6,PREVHIT(R2)                                                   
         LA    R6,WORKA(R6)                                                     
         MVC   0(1,R6),PLIST+16                                                 
         B     *-22                                                             
         MVC   PREVHIT(16),SPACES  AND CLEAR PREVHIT                            
         L     RF,PLIST+8                                                       
         SR    R6,R6                                                            
         IC    R6,2(RF)                                                         
         LA    R6,1(R6)            INCREMENT SHIP COUNT IN SHIPTAB              
         STC   R6,2(RF)                                                         
FLEEWKND LA    R3,1(R3)                                                         
         BCT   R1,FLEEWRKD                                                      
*                                  SET SHIP SIZE IN SHIPTAB TO ZERO             
*                                   IF IT IS 'FF'                               
         LA    R1,SHIPTAB                                                       
FLESLOOP CLI   0(R1),X'00'                                                      
         BE    FLESORT                                                          
         CLI   1(R1),X'FF'                                                      
         BNE   *+8                                                              
         MVI   1(R1),X'00'                                                      
         LA    R1,L'SHIPTAB(R1)                                                 
         B     FLESLOOP                                                         
*                                                                               
*                                  SORT SHIPTAB ENTRIES TO DESCENDING           
*                                   SEQUENCE OF SHIP SIZE FOR USE BY            
FLESORT  SR    R1,R1                FLEGEN S/R                                  
         LA    R2,4                                                             
         LA    R3,12                                                            
         LA    R4,SHIPTAB                                                       
         ST    R4,PLIST(R1)                                                     
         LA    R4,L'SHIPTAB(R4)                                                 
         BXLE  R1,R2,*-8                                                        
         LA    R4,PLIST                                                         
         LA    R5,PLIST+4                                                       
         BAS   RE,SORTCHK                                                       
         LA    R5,PLIST+8                                                       
         BAS   RE,SORTCHK                                                       
         LA    R5,PLIST+12                                                      
         BAS   RE,SORTCHK                                                       
         LA    R4,PLIST+4                                                       
         LA    R5,PLIST+8                                                       
         BAS   RE,SORTCHK                                                       
         LA    R5,PLIST+12                                                      
         BAS   RE,SORTCHK                                                       
         LA    R4,PLIST+8                                                       
         BAS   RE,SORTCHK                                                       
         LA    R4,WORK                                                          
         SR    R1,R1                                                            
         L     R5,PLIST(R1)                                                     
         MVC   0(5,R4),0(R5)                                                    
         LA    R4,5(R4)                                                         
         BXLE  R1,R2,*-14                                                       
         MVC   SHIPTAB(20),WORK                                                 
         B     FLEETEND                                                         
SORTCHK  L     R6,0(R4)                                                         
         L     R7,0(R5)                                                         
         CLC   1(1,R6),1(R7)                                                    
         BNL   SORTCHKX                                                         
         XC    0(4,R4),0(R5)                                                    
         XC    0(4,R5),0(R4)                                                    
         XC    0(4,R4),0(R5)                                                    
SORTCHKX BR    RE                                                               
FLEETEND MVC   PQDISPA(120),WORKA                                               
         B     FLEERDX                                                          
*              SET UP FLEET INPUT ERROR MESSAGE AND SET CC=0                    
*                                                                               
FLEERRB  MVC   WORK(23),ERRORB                                                  
         B     FLSP                                                             
FLEERRC  MVC   WORK(23),ERRORC                                                  
         B     FLSP                                                             
FLEERRD  MVC   WORK(23),ERRORD                                                  
         B     FLSP                                                             
FLEERRE  MVC   WORK(23),ERRORE                                                  
FLSP     DS    0C                                                               
         MVC   WORK+23(16),=C' - SHIP POSITION'                                 
         MVC   WORK+39(5),SPACES                                                
         MVC   PLIST+4(4),PLIST    CONVERT SHIP ADDRESS IN ERROR TO             
         BAS   RE,ADRVCON           A/N OR ALPHA COORDINATES                    
         MVC   WORK+40(3),PLIST                                                 
         MVC   WORK+44(15),ERROR3+44                                            
         B     FLEERR4                                                          
FLEERRA  MVC   WORK(L'ERROR3),ERROR3                                            
FLEERR4  SR    R4,R4                                                            
FLEERDX  LTR   R4,R4                                                            
         XIT1                                                                   
         EJECT                                                                  
*              FLEGEN                                                           
*                                                                               
*              GENERATES A FLEET FOR PLAYER OR MYSELF                           
*              INPUTS:                                                          
*                     PLAYER   1C = M OR P                                      
*              OUTPUTS:                                                         
*                     IF PLAYER IS M                                            
*                     PSHTQD   15C= BIT LIST WHERE 1=A SHIP CHARACTER           
*                     MFLTABLE 30F= TABLE OF MY FLEET DETAILS                   
*                                                                               
*                     IF PLAYER IS P                                            
*                     PQDISPA  30F= LINEAR REPRESENTATION OF PLAYER'S           
*                                   FLEET (NO SHIP = X'00')                     
*                                                                               
*                     CC OF ZERO  = I CANT GENERATE A VALID FLEET               
*                                                                               
FLEGEN   NTR1                                                                   
         LA    R0,20                                                            
FLEGRTRY MVC   MYPQUADA(121),AVAILSHT                                           
         L     R1,LIMIT                                                         
         STC   R1,MYPQUADA         MYPQUADA CONTAINS COUNT OF NO OF             
         XC    WORK(120),WORK       USEABLE SHOTS + LIST OF WHAT THEY           
         MVI   MFLTABLE,X'FF'       ARE AS ADDRESSES 1-120                      
         MVC   MFLTABLE+1(119),MFLTABLE                                         
         LA    R2,SHIPTAB          SHIPTAB IS USED TO DRIVE FLEET               
         LA    RF,MFLTABLE          GENERATION.                                 
FLEGNTYP ST    RF,PLIST+12                                                      
         CLI   0(R2),X'00'                                                      
         BE    FLEGLPND                                                         
         SR    R3,R3                                                            
         IC    R3,2(R2)            R3=NO OF SHIPS OF THIS TYPE                  
         LTR   R3,R3                                                            
         BZ    FLEGNEWT                                                         
         SR    R4,R4                                                            
         IC    R4,1(R2)                                                         
         BCTR  R4,0                R4=NO OF CHARS IN THIS SHIP MINUS 1          
         STC   R4,HITSTSNK            (USED BY ROOMSHT)                         
FLEGNSHP LA    R7,20                                                            
FLEGRTSH SR    R5,R5               GENERATE A RANDOM SHOT FROM LIST OF          
         IC    R5,MYPQUADA          POSSIBLES                                   
         BCTR  R5,0                                                             
         GOTO1 =V(RANDOM),PLIST,(R5),RR=RB                                      
         L     R5,PLIST+4                                                       
         LA    R6,MYPQUADA+1                                                    
         LA    R6,0(R5,R6)                                                      
         SR    R5,R5                                                            
         IC    R5,0(R6)                                                         
         BCTR  R5,0                                                             
         ST    R5,PREVHIT          STORE IT IN PREVHIT                          
         LTR   R4,R4               IF THIS IS A 1-CHAR SHIP,IT MUST BE          
         BNZ   FLEGTWOM             VALID,SO REMOVE IT FROM THE LIST            
         NI    PLIST+8,X'00'        OF POSSIBLES                                
         ST    R5,PLIST+4                                                       
         BAS   RE,REMSHOT                                                       
         B     FLEGFITS                                                         
FLEGTWOM GOTO1 =V(RANDOM),PLIST,3,RR=RB                                         
         L     R5,PLIST+4          OTHERWISE GENERATE A RANDOM POSITIVE         
         IC    R5,ADDFAC(R5)        DIRECTION AND CHECK WHETHER THERE           
         STC   R5,PLIST             IS ROOM FOR A SHIP OF THIS SIZE IN          
         BAS   RE,ROOMSHT           THIS AND/OR THE OPPOSITE DIRECTION          
         BNZ   FLEGFITS                                                         
         BCT   R7,FLEGRTSH         TRY 20 TIMES,THEN TRY FROM SCRATCH           
         BCT   R0,FLEGRTRY                                                      
         B     FLEGENX              20 TIMES, THEN GIVE UP                      
FLEGFITS BAS   RE,NADJSHT          HAVING PLACED A SHIP,REMOVE ADJACENT         
         L     RF,PLIST+12                                                      
         MVC   0(1,RF),0(R2)        POSITIONS FROM LIST OF POSSIBLES            
         MVC   1(1,RF),1(R2)        AND UPDATE A MFLTABLE ENTRY.                
         LA    R1,2(RF)                                                         
         SR    R5,R5                                                            
         LA    R6,4                                                             
         LA    R7,12                                                            
FLEGPLP  LA    R9,PREVHIT(R5)                                                   
         CLC   0(4,R9),SPACES                                                   
         BE    FLEGPFIN                                                         
         MVC   0(1,R1),3(R9)                                                    
         L     R9,0(R9)                                                         
         LA    R9,WORK(R9)         PLACE SHIP IN WORK                           
         MVC   0(1,R9),0(R2)                                                    
         LA    R1,1(R1)                                                         
         BXLE  R5,R6,FLEGPLP                                                    
FLEGPFIN MVC   PREVHIT(16),SPACES                                               
         LA    RF,L'MFLTABLE(RF)                                                
         ST    RF,PLIST+12                                                      
         BCT   R3,FLEGNSHP         GO TO GENERATE NXT SHIP OF THIS TYPE         
FLEGNEWT LA    R2,L'SHIPTAB(R2)                                                 
         B     FLEGNTYP            GO TO NEXT SHIP TYPE                         
FLEGLPND MVI   0(RF),X'00'         HAVING GENERATED A FLEET                     
         CLI   PLAYER,C'M'                                                      
         BE    *+14                                                             
         MVC   PQDISPA(120),WORK    PUT IT IN PQDISPA IF IT'S PLAYER'S          
         B     FLEGENX                                                          
         XC    PSHTQD(15),PSHTQD    OR SET UP A BIT LIST IN PSHTQD TO           
         SR    R3,R3                 CHECK PLAYER'S SHOTS IF IT'S MINE          
         LA    R4,1                                                             
         L     R5,LIMIT                                                         
         BCTR  R5,0                                                             
FLEGPSHT LA    R6,WORK(R3)                                                      
         CLI   0(R6),X'00'                                                      
         BE    FLEGPSHE                                                         
         SR    R6,R6                                                            
         LR    R7,R3                                                            
         D     R6,=F'8'                                                         
         LA    R6,PSHTMASK(R6)                                                  
         LA    R7,PSHTQD(R7)                                                    
         OC    0(1,R7),0(R6)                                                    
FLEGPSHE BXLE  R3,R4,FLEGPSHT                                                   
FLEGENX  LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*              FLESETP                                                          
*                                                                               
*              SETS UP COMPLETE PLAYER'S FLEET IN TWA                           
*                                                                               
FLESETP  NTR1                                                                   
         MVI   PLAYER,C'M'                                                      
         SR    R1,R1                                                            
         LA    R2,1                                                             
         L     R3,LIMIT                                                         
         BCTR  R3,0                                                             
FLEDISPY LA    R4,PQDISPA(R1)                                                   
         CLI   0(R4),X'00'                                                      
         BE    *+18                                                             
         ST    R1,PLIST+4                                                       
         MVC   SHIPTYPE(1),0(R4)                                                
         BAS   RE,DISPSHT                                                       
         BXLE  R1,R2,FLEDISPY                                                   
FLESETPX XIT1                                                                   
         EJECT                                                                  
*              INCHM                                                            
*                                                                               
*              INCREMENTS PLAYER'S OR MY HITS OR MISSES AND PUTS 'HIT'          
*              OR 'MISS' IN APPROPRIATE SHOT DESCRIPTION                        
*              INPUTS:                                                          
*                    PLAYER    1C = M OR P                                      
*                    HITORMSS  4C = 'HIT ' OR 'MISS'                            
*                                                                               
INCHM    NTR1                                                                   
         CLI   PLAYER,C'M'                                                      
         BNE   INCHPLAY                                                         
         L     R1,AUNSCNM                                                       
         BAS   RE,ADRVCON                                                       
         MVC   0(3,R1),PLIST                                                    
         LA    RF,16               RF= PLAYER MODIFIER                          
         B     INCHGO                                                           
INCHPLAY L     R1,AUNSCNP                                                       
         SR    RF,RF                                                            
INCHGO   MVC   10(4,R1),HITORMSS                                                
         SR    RE,RE                                                            
         CLI   HITORMSS,C'H'                                                    
         BE    *+8                                                              
         LA    RE,110              RE= HIT/MISS MODIFIER                        
         LA    R2,BATHITSH(RE)                                                  
         OI    6(R2),X'80'                                                      
         LA    R2,0(RF,R2)                                                      
         CLI   11(R2),X'40'                                                     
         BNE   *+8                                                              
         MVI   11(R2),X'F0'                                                     
         PACK  DUB,10(2,R2)                                                     
         CVB   R4,DUB                                                           
         LA    R4,1(R4)                                                         
         EDIT  (R4),(2,10(R2))                                                  
         CLI   HITORMSS,C'H'                                                    
         BNE   INCHMX                                                           
         SRL   RF,3                RF= 2 (PLAYER=M) OR 0 (PLAYER=P)             
         LH    R3,PHITS(RF)                                                     
         LA    R3,1(R3)                                                         
         STH   R3,PHITS(RF)                                                     
INCHMX   XIT1                                                                   
         EJECT                                                                  
*              INCSINK                                                          
*                                                                               
*              IN/DE-CREMENTS PLAYER'S OR MY SINKINGS/ REMAINING FLEET,         
*              PUTS 'SINK' IN APPROPRIATE SHOT DESCRIPTION AND, FOR             
*              MY SINKINGS, PREVENTS ADJACENT SHOTS, INITIALISES                
*              PREVHIT AND UPDATES SMALLEST (SHIP SIZE IN HIS FLEET).           
*              INPUTS:                                                          
*                     PLAYER   1C = M OR P                                      
*                     SHIPTYPE 1C = SHIP CHARACTER                              
*                     PREVHIT  4F = (FOR MY SINKINGS) UP TO 4 ADDRESSES         
*              OUTPUTS:                                                         
*                     SMALLEST 1C = SIZE OF SMALLEST PLAYER SHIP AFLOAT         
*                                                                               
INCSINK  NTR1                                                                   
         CLI   PLAYER,C'M'         IF PLAYER IS ME                              
         BNE   INCSPLAY                                                         
         BAS   RE,NADJSHT          PREVENT ADJACENT SHOTS                       
         MVC   PREVHIT(16),SPACES                                               
         L     R1,AUNSCNM                                                       
         SR    RE,RE               RE= MODIFIER FOR SH-T-SNK IN SHIPTAB         
         SR    RF,RF               RF= MODIFIER FOR SCREEN TOTALS               
         B     INCSGO                                                           
INCSPLAY L     R1,AUNSCNP                                                       
         LA    RE,1                                                             
         LA    RF,16                                                            
INCSGO   MVC   10(4,R1),=C'SINK'                                                
         LA    R3,SHIPTAB                                                       
         CLC   0(1,R3),SHIPTYPE                                                 
         BE    *+12                                                             
         LA    R3,L'SHIPTAB(R3)                                                 
         B     *-14                                                             
         LA    R3,0(RE,R3)         SHIPTAB ENTRY +3 OR +4 FOR P'S OR MY         
         SR    R4,R4                SHIPS TO SINK                               
         IC    R4,3(R3)                                                         
         BCT   R4,INCSON           IF NO MORE OF THIS SHIP TYPE TO SINK         
         CLI   PLAYER,C'M'         AND PLAYER IS ME                             
         BNE   INCSON                                                           
         STC   R4,3(R3)                                                         
         LA    R9,SHIPTAB          UPDATE SMALLEST SIZE OF PLAYER SHIP          
         SR    R6,R6               AFLOAT FROM SHIPTAB                          
INCSLOOP CLI   0(R9),X'00'          (NB SHIPTAB IS IN DESCENDING SHIP           
         BE    INCSLPND              SIZE SEQUENCE)                             
         CLI   3(R9),0                                                          
         BE    *+8                                                              
         IC    R7,1(R9)                                                         
         LA    R9,L'SHIPTAB(R9)                                                 
         B     INCSLOOP                                                         
INCSLPND STC   R7,SMALLEST                                                      
INCSON   STC   R4,3(R3)            UPDATE REMAINING FLEET                       
         LA    R2,BATBTSPH                                                      
         CLI   SHIPTYPE,C'B'                                                    
         BE    INCSBAT                                                          
         CLI   SHIPTYPE,C'C'                                                    
         BE    INCSCRU                                                          
         CLI   SHIPTYPE,C'F'                                                    
         BE    *+8                                                              
         LA    R2,110(R2)                                                       
         LA    R2,110(R2)                                                       
INCSCRU  LA    R2,110(R2)                                                       
INCSBAT  OI    6(R2),X'80'                                                      
         LA    R2,0(RF,R2)         RF = 16 (PLAYER = M ) OR  ZERO               
         LTR   R4,R4                                                            
         BZ    INCSNONE                                                         
         EDIT  (R4),(2,10(R2))                                                  
         B     *+10                                                             
INCSNONE MVC   10(2,R2),=C'NO'                                                  
INCSNKCT X     RF,=F'16'           RF= 0 (PLAYER=M) OR 16 (PLAYER=P)            
         LA    R2,BATSNKSH         UPDATE COUNT OF SINKINGS                     
         OI    6(R2),X'80'                                                      
         LA    R2,0(RF,R2)                                                      
         CLI   11(R2),X'40'                                                     
         BNE   *+8                                                              
         MVI   11(R2),X'F0'                                                     
         PACK  DUB,10(2,R2)                                                     
         CVB   R4,DUB                                                           
         LA    R4,1(R4)                                                         
         EDIT  (R4),(2,10(R2))                                                  
         BNE   INCSINKX                                                         
         MVI   10(R2),X'40'                                                     
INCSINKX XIT1                                                                   
*                                                                               
         EJECT                                                                  
MYSHOT   NTR1                                                                   
         MVI   PLAYER,C'M'                                                      
         LA    R3,PREVHIT          R3 = POINTER TO LOCATION TO CONTAIN          
         CLI   NOHITS,X'00'             NEXT HIT ADDRESS (0-119)                
         BNZ   LASTHIT             IF NO PREVIOUS HITS,                         
MYSRAND  SR    R4,R4                                                            
         IC    R4,SMALLEST                                                      
         BCTR  R4,0                                                             
         STC   R4,HITSTSNK                                                      
         SR    R5,R5                SELECT A RANDOM SHOT                        
         IC    R5,MYPQUADA                                                      
         BCT   R5,*+8                                                           
         B     MYONLY              IF ONLY ONE SHOT AVAILABLE, USE IT           
         GOTO1 =V(RANDOM),PLIST,(R5),RR=RB                                      
         L     R5,PLIST+4                                                       
MYONLY   LA    R6,MYPQUADA+1(R5)                                                
         SR    R5,R5                                                            
         IC    R5,0(R6)                                                         
         BCTR  R5,0                                                             
         ST    R5,PREVHIT                                                       
         LTR   R4,R4               IF SMALLEST SHIP AFLOAT IS ONE-CHAR          
         BZ    MYSHOOT              USE THIS SHOT.                              
         LA    R4,4                                                             
         SR    R5,R5               OTHERWISE CHECK THAT THERE IS ROOM           
         IC    R5,PREFDIR           FOR THE SMALLEST SHIP AFLOAT IN THE         
         IC    RE,ADDFAC(R5)       PREFERRED DIRECTION                          
         STC   RE,PLIST                                                         
         BAS   RE,ROOMSHT                                                       
         BNZ   MYSHOOT                                                          
         LA    R5,1(R5)                                                         
         BCT   R4,*-20                                                          
         MVC   PLIST+4(4),PREVHIT   IF THERE ISN'T,REMOVE THIS SHOT             
         NI    PLIST+8,X'00'         FROM THE LIST OF POSSIBLES AND TRY         
         BAS   RE,REMSHOT            ANOTHER                                    
         B     MYSRAND                                                          
*                                                                               
LASTHIT  CLI   NOHITS,X'01'        ONE PREVIOUS HIT                             
         BNE   MYSTWOPL                                                         
         SR    R5,R5                                                            
         IC    R5,PREFDIR                                                       
         IC    RE,ADDFAC(R5)       LOOK FOR AN ADJACENT SHOT THAT               
         STC   RE,PLIST             COULD BE ANOTHER HIT,STARTING IN            
         BAS   RE,ROOMSHT            THE LAST SUCCESSFUL DIRECTION              
         BNZ   *+12                                                             
         LA    R5,1(R5)                                                         
         B     *-20                                                             
         LA    R3,4(R3)             UPDATE PREVHIT POINTER                      
         B     MYSHOOT                                                          
*                                                                               
MYSTWOPL LA    R3,8(R3)            TWO OR MORE PREVIOUS HITS                    
         L     R4,PREVHIT           UPDATE PREVHIT POINTER                      
         L     R5,PREVHIT+4                                                     
         CLI   NOHITS,X'02'                                                     
         BE    MYSTJT                                                           
         LA    R3,4(R3)                                                         
         L     R6,PREVHIT+8                                                     
MYSTLOOP CR    R5,R6                                                            
         BL    *+10                                                             
         XR    R5,R6                                                            
         XR    R6,R5                                                            
         XR    R5,R6                                                            
MYSTJT   CR    R4,R5                                                            
         BL    MYSFIND                                                          
         XR    R4,R5                                                            
         XR    R5,R4                                                            
         XR    R4,R5                                                            
         CLI   NOHITS,X'02'                                                     
         BNE   MYSTLOOP                                                         
MYSFIND  LR    RE,R5               UPDATE PREFERRED ADDFAC MODIFIER             
         SR    RE,R4                                                            
         LPR   RE,RE                                                            
         SR    R9,R9                                                            
         SR    RF,RF                                                            
         IC    RF,ADDFAC(R9)                                                    
         CR    RF,RE                                                            
         BE    *+12                                                             
         LA    R9,1(R9)                                                         
         B     *-14                                                             
         STC   R9,PREFDIR                                                       
         CLI   NOHITS,X'02'                                                     
         BE    *+6                                                              
         LR    R5,R6               R4=LOWEST, R5=HIGHEST HIT ADDRESS            
         LR    R7,R5                                                            
         LR    R9,RF                                                            
         AR    R7,R9               EXTEND SHIP UPWARDS AND CHECK THAT           
         C     R7,LIMIT             ADDRESS IS FEASIBLE FOR A HIT               
         BNL   MYSCHDIR                                                         
         LR    RF,R5                                                            
         LR    R5,R7                                                            
         SR    RE,RE                                                            
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         D     RE,NOCOLS                                                        
         SR    R6,RE                                                            
         LPR   R6,R6                                                            
         CH    R6,=H'1'                                                         
         BH    MYSCHDIR                                                         
         ST    R5,PLIST+4                                                       
         BAS   RE,REMSHOT                                                       
         BZ    MYSCHDIR                                                         
         LR    R4,R5                                                            
         B     *+6                                                              
MYSCHDIR SR    R4,R9                                                            
         ST    R4,0(R3)                                                         
         B     MYSHOOT             AND SHOOT                                    
*                                                                               
MYSHOOT  MVC   PLIST+4(4),0(R3)    REMOVE THE CHOSEN SHOT FROM POSSIBLE         
         NI    PLIST+8,X'00'        LIST                                        
         BAS   RE,REMSHOT                                                       
         LA    R7,PQDISPA          IS IT A HIT ?                                
         L     R6,PLIST+4                                                       
         LA    R7,0(R6,R7)                                                      
         CLI   0(R7),X'00'                                                      
         BE    *+18                                                             
         MVC   HITORMSS(4),HIT                                                  
         MVI   SHIPTYPE,C'H'                                                    
         B     *+14                                                             
         MVC   HITORMSS(4),MISS                                                 
         MVI   SHIPTYPE,C' '                                                    
         BAS   RE,INCHM            UPDATE HITS OR MISSES                        
         BAS   RE,DISPSHT           AND DISPLAY ON HIS QUADRANT                 
         CLI   HITORMSS,C'M'                                                    
         BE    MSHOTX                                                           
         CLI   NOHITS,X'00'        IF NO PREVIOUS HITS                          
         BNE   MYSPREV              FIND HITS-TO-SINK FROM SHIPTAB              
         MVC   MSHPTYPE,0(R7)                                                   
         LA    R9,SHIPTAB                                                       
         CLC   0(1,R9),0(R7)                                                    
         BE    *+12                                                             
         LA    R9,L'SHIPTAB(R9)                                                 
         B     *-14                                                             
         MVC   HITSTSNK(1),1(R9)                                                
MYSPREV  SR    RE,RE               DECREMENT HITS-TO-SINK                       
         IC    RE,HITSTSNK                                                      
         BCTR  RE,0                                                             
         STC   RE,HITSTSNK                                                      
         LTR   RE,RE               IF SHIP NOT SUNK,UPDATE NO OF HITS           
         BZ    MYSINK                                                           
         SR    RE,RE                                                            
         IC    RE,NOHITS                                                        
         LA    RE,1(RE)                                                         
         STC   RE,NOHITS                                                        
         B     MSHOTX                                                           
MYSINK   LA    RE,PREVHIT+16       IF A SINKING                                 
         LA    RF,5(R3)             CLEAR UNUSED PREVHITS TO SPACES             
         SR    RE,RF                (FOR NADJSHT S/R CALLED BY INCSINK)         
         BM    *+8                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),SPACES                                                   
         MVC   SHIPTYPE,MSHPTYPE                                                
         BAS   RE,INCSINK           INCREMENT SINKINGS (ETC)                    
         XC    NOHITS,NOHITS        AND INITIALISE NO OF HITS                   
MSHOTX   XIT1                                                                   
         EJECT                                                                  
*              NADJSHT                                                          
*                                                                               
*              PREVENTS MY SHOTS ADJACENT TO A SHIP                             
*              INPUTS:                                                          
*                     PREVHIT  4F = UP TO 4 ADDRESSES -SPACES IF UNUSED         
*              OUTPUTS:                                                         
*                     NONE -PREVHIT IS NOT INITIALIZED                          
*                                                                               
NADJSHT  NTR1                                                                   
         NI    PLIST+8,X'00'                                                    
         SR    R1,R1               R1= MODIFIER TO ADDRESS PREVHIT (4F)         
         LA    R2,4                R2= INCREMENT TO R1                          
         LA    R3,12               R3= BXLE LIMIT                               
NADJLP   LA    R9,4                R9= DIRECTION MODIFIER AND COUNT             
         L     RF,PREVHIT(R1)                                                   
         C     RF,SPACES           IF SPACES,NO MORE CHARACTERS IN SHIP         
         BE    NADJSHTX                                                         
         LR    R5,RF                                                            
         SR    R4,R4                                                            
         D     R4,NOCOLS           R4= COLUMN NO OF PREVHIT(N)                  
NADJCDIR SR    R5,R5                                                            
         IC    R5,ADDFAC(R9)       R5= A DIR5CTION DISPLACEMENT                 
         LA    R7,0(R5,RF)                                                      
         C     R7,LIMIT            IF > QUADRANT LIMIT MINUS ONE                
         BNL   NADJREV             GO TO REVERSE DIRECTION                      
         ST    R7,PLIST+4                                                       
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         SR    R6,R4                                                            
         LPR   R6,R6               IF WRAP-ROUND                                
         CH    R6,=H'1'                                                         
         BH    NADJREV             GO TO REVERSE DIRECTION                      
         BAS   RE,REMSHOT          OTHERWISE REMOVE SHOT FROM POSSIBLES         
NADJREV  LR    R7,RF               DO SAME FOR REVERSE DIRECTION                
         SR    R7,R5                                                            
         BM    NADJELP                                                          
         ST    R7,PLIST+4                                                       
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         SR    R6,R4                                                            
         LPR   R6,R6                                                            
         CH    R6,=H'1'                                                         
         BH    NADJELP                                                          
         BAS   RE,REMSHOT                                                       
NADJELP  BCT   R9,NADJCDIR         THEN REPEAT FOR OTHER 3 DIRECTIONS           
         BXLE  R1,R2,NADJLP        AND UP TO 3 OTHER PREVHIT'S                  
NADJSHTX XIT1                                                                   
         EJECT                                                                  
*              POSSNK                                                           
*                                                                               
*              CHECKS WHETHER A PLAYER'S HIT IS A SINKING                       
*              INPUTS:                                                          
*                     PLIST    1F = HIT ADDRESS (0-119)                         
*              OUTPUTS:                                                         
*                     CC OF ZERO  = A  SINKING                                  
*                     PSHPTYPE 1C = SHIP CHARACTER IF A SINKING                 
*                                                                               
POSSNK   NTR1                                                                   
         LA    R3,MFLTABLE                                                      
POSSRCH  CLI   1(R3),X'00'                                                      
         BE    POSSLPND                                                         
         LA    R4,4                                                             
         LA    R5,1(R3)                                                         
POSSLOOP LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    POSSLPND                                                         
         CLC   0(1,R5),PLIST+3                                                  
         BE    POSSFND                                                          
         BCT   R4,POSSLOOP                                                      
POSSLPND LA    R3,L'MFLTABLE(R3)                                                
         B     POSSRCH                                                          
POSSFND  SR    R2,R2                                                            
         IC    R2,1(R3)                                                         
         BCTR  R2,0                                                             
         IC    R1,0(R3)                                                         
         STC   R1,PSHPTYPE                                                      
         STC   R2,1(R3)                                                         
         LTR   R2,R2                                                            
POSSNKX  XIT1                                                                   
         EJECT                                                                  
*              QUADSET                                                          
*                                                                               
*              SETS UP PLAYER'S &  MY INITIALIZED QUADRANT WITH PLUSES          
*               TO THE EXTENT OF THE DEFINED QUADRANT SIZE AND SPACES           
*               IN THE REMAINDER                                                
*              IF GAME IS NOT IN PROGRESS A FULL 12X10 QUADRANT IS SET          
*              INPUTS:                                                          
*                     PLAYER   1C = M ( MY QUADRANT ) OR P                      
*                     GIP      1C = T (GAME IN PROGRESS) OR ZERO                
*              OUTPUTS:                                                         
*                     NONE                                                      
*                                                                               
QUADSET  NTR1                                                                   
         SR    R6,R6                                                            
QUADMYQD SR    R1,R1                                                            
         LA    R2,4                                                             
         LA    R3,36               R1,2,3 =BXLE REGISTERS                       
         CLI   GIP,C'T'                                                         
         BE    *+16                                                             
         LA    R4,40                                                            
         LA    R5,22                                                            
         B     QUADLOOP                                                         
         L     R4,NOROWS                                                        
         SLL   R4,2                R4= ROW LIMIT X 4                            
         L     R5,NOCOLS                                                        
         SLL   R5,1                                                             
         BCTR  R5,0                R5= COL LIMIT X 2  -1                        
         BCTR  R5,0                                   -1 FOR MVC LENGTH         
QUADLOOP LA    R9,PROWS(R1)                                                     
         LA    R9,0(R6,R9)                                                      
         SR    RE,RE                                                            
         EX    RE,0(R9)                                                         
         MVC   8(23,RE),SPACES     FILL EACH ROW WITH SPACES                    
         OI    6(RE),X'80'                                                      
         CR    R1,R4                                                            
         BNL   QUADNDLP            JUMP OUT IF THIS ROW IS BEYOND LIMIT         
         EX    R5,*+8               FOR THE GAME                                
         B     *+10                                                             
         MVC   8(0,RE),PLUSN       OTHERWISE FILL ROW WITH PLUSES UP TO         
QUADNDLP BXLE  R1,R2,QUADLOOP       COLUMN LIMIT OF THE GAME                    
         LTR   R6,R6                                                            
         BNZ   QUADSETX                                                         
         LA    R6,40                                                            
         B     QUADMYQD                                                         
QUADSETX XIT1                                                                   
         EJECT                                                                  
*        QUADSZE                                                                
*                                                                               
*        SETS UP QUADRANT SIZE IN TWA FROM NOCOLS AND NOROWS                    
*        INPUTS:                                                                
*               NOCOLS    1F = NUMBER OF COLUMNS                                
*               NOROWS    1F = NUMBER OF ROWS                                   
*                                                                               
QUADSZE  NTR1                                                                   
         MVC   PLIST(5),SPACES                                                  
         LA    R2,PLIST                                                         
         EDIT  (B4,NOCOLS),(2,(R2))                                             
         LA    R2,2(R2)                                                         
         MVI   0(R2),C'X'                                                       
         EDIT  (B4,NOROWS),(2,1(R2)),ALIGN=LEFT                                 
         MVC   BATMQSZ+18(5),PLIST                                              
         MVC   BATPQSZ+17(5),PLIST                                              
         OI    BATMQSZH+6,X'80'                                                 
         OI    BATPQSZH+6,X'80'                                                 
QUADSZEX XIT1                                                                   
         EJECT                                                                  
*              REMSHOT - REMOVES USED/INELIGIBLE SHOTS FROM                     
*                       POSSIBLE LIST.                                          
*                                                                               
*              INPUTS:                                                          
*                     PLIST+4  1F = SHOT TO BE CHECKED (0-119)                  
*                     PLIST+8  1C = X'00' REMOVE SHOT FROM POSSIBLES            
*                                 = X'01' ONLY CHECK IT                         
*              OUTPUTS:                                                         
*                     CC OF ZERO  = SHOT IS NOT USEABLE                         
*                                                                               
REMSHOT  NTR1                                                                   
         L     R3,PLIST+4                                                       
         LA    R3,1(R3)                                                         
         SR    R4,R4                                                            
         IC    R4,MYPQUADA                                                      
         LA    R5,MYPQUADA                                                      
*                                                                               
REMLP    SR    R6,R6                                                            
         LA    R5,1(R5)                                                         
         IC    R6,0(R5)                                                         
         CR    R6,R3                                                            
         BE    REMIT                                                            
         BH    NOHIT                                                            
         BCT   R4,REMLP                                                         
*                                                                               
NOHIT    SR    R4,R4                                                            
         LTR   R4,R4                                                            
         B     REMSHOTX                                                         
*                                                                               
REMIT    OI    PLIST+8,X'00'                                                    
         BNZ   REMSHOTX                                                         
         BCT   R4,*+8                                                           
         B     COUNTONE            IF COUNT WAS ONE NO MOVE REQUIRED            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),1(R5)                                                    
COUNTONE SR    R6,R6                                                            
         IC    R6,MYPQUADA                                                      
         BCTR  R6,0                                                             
         STC   R6,MYPQUADA                                                      
         LTR   R5,R5                                                            
*                                                                               
REMSHOTX XIT1                                                                   
         EJECT                                                                  
*              ROOMSHT                                                          
*                                                                               
*              CHECKS THAT THERE IS ROOM FOR A SHIP OF GIVEN SIZE IN            
*               A GIVEN DIRECTION (+/-),GIVEN A BASE ADDRESS(0-119)             
*              INPUTS:                                                          
*                     PREVHIT  1F = BASE ADDRESS (0-119)                        
*                     HITSTSNK 1F = SHIP SIZE MINUS ONE                         
*                     PLIST    1C = PLUS DIRECTION IE DISPLACEMENT FROM         
*                                   BASE ADDRESS                                
*              OUTPUTS:                                                         
*                     CC OF ZERO  = THERE ISNT ROOM                             
*                     PREVHIT+4,3F= UP TO 3 ADDRESSES IF THERE IS ROOM          
*                                                                               
ROOMSHT  NTR1                                                                   
         OI    PLIST+8,X'01'       USED TO STOP  REMSHOT S/R REMOVING           
         SR    R1,R1                SHOTS FROM POSSIBLES LIST                   
         LA    R2,4                                                             
         SR    R3,R3                                                            
         IC    R3,HITSTSNK         R1,2,3 = BXLE REGISTERS                      
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     RF,PREVHIT                                                       
         LR    R5,RF                                                            
         SR    RE,RE                                                            
         D     RE,NOCOLS                                                        
         LR    R4,RE               R4 & RE= COLUMN NO OF PREVHIT                
         LR    RF,R5               RF= PREVHIT                                  
         SR    R9,R9                                                            
         IC    R9,PLIST                                                         
ROOMLOOP AR    R5,R9               ADD DIRECTION DISPLACEMENT                   
         C     R5,LIMIT            IF RESULT IS >119                            
         BNL   ROOMCDIR                                                         
         LR    R7,R5                                                            
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         SR    RE,R6                                                            
         LPR   RE,RE               OR GIVES WRAP-ROUND                          
         CH    RE,=H'1'                                                         
         BH    ROOMCDIR                                                         
         ST    R5,PLIST+4                                                       
         BAS   RE,REMSHOT          OR IS NOT A POSSIBLE SHOT                    
         BZ    ROOMCDIR            REVERSE DIRECTION                            
         ST    R5,PREVHIT+4(R1)    OTHERWISE STORE ADDRESS                      
         LR    RE,R6                                                            
         BXLE  R1,R2,ROOMLOOP      AND REPEAT UNTIL ROOM HAS BEEN FOUND         
         B     ROOMSHTX             FOR REQUIRED NO OF SHOTS                    
ROOMCDIR LR    R5,RF                                                            
ROOMCDLP SR    R5,R9               IF THIS CANT BE ACHIEVED FORWARDS,           
         BM    ROOMNOT              TRY REVERSING DIRECTION                     
         LR    R7,R5                                                            
         SR    R6,R6                                                            
         D     R6,NOCOLS                                                        
         SR    R4,R6                                                            
         LPR   R4,R4                                                            
         CH    R4,=H'1'                                                         
         BH    ROOMNOT                                                          
         ST    R5,PLIST+4                                                       
         BAS   RE,REMSHOT                                                       
         BZ    ROOMNOT                                                          
         ST    R5,PREVHIT+4(R1)                                                 
         LR    R4,R6                                                            
         BXLE  R1,R2,ROOMCDLP                                                   
         B     ROOMSHTX                                                         
ROOMNOT  SR    R2,R2               SET CC=0 IF NO ROOM                          
         MVC   PREVHIT+4(12),SPACES                                             
ROOMSHTX XIT1                                                                   
         EJECT                                                                  
*              SHPSRCH                                                          
*                                                                               
*              SEARCHES FOR A SHIP CHARACTER ADJACENT TO A GIVEN ADDRSS         
*              INPUTS:                                                          
*                     WORKA    1F = START OF SEARCH AREA                        
*                     PLIST+4  1F = BASE ADDRESS ( 0-119)                       
*              OUTPUTS:                                                         
*                     CC OF ZERO  = NO SHIP CHARACTER  ADJACENT                 
*                     PLIST    1F = ADJACENT ADDRESS CONTAINING SHIP            
*                                                                               
SHPSRCH  NTR1                                                                   
         LA    R9,WORKA                                                         
         LA    R1,4                R1= DIRECTION MODIFIER & COUNT               
         L     RF,PLIST+4                                                       
         LR    R3,RF                                                            
         SR    RE,RE                                                            
         D     RE,NOCOLS           RE= COLUMN NO OF BASE ADDRESS                
         SR    R6,R6                                                            
SHPSLOOP IC    R6,ADDFAC(R1)                                                    
         LA    R5,0(R3,R6)         R5= A (+) ADJACENT ADDRESS                   
         C     R5,LIMIT            IF IT IS >119                                
         BNL   SHPSCDIR                                                         
         LR    R7,R5                                                            
         SR    R4,R4                                                            
         D     R4,NOCOLS                                                        
         SR    R4,RE                                                            
         LPR   R4,R4               OR WRAPS ROUND                               
         CH    R4,=H'1'                                                         
         BH    SHPSCDIR                                                         
         LA    R2,0(R9,R7)                                                      
         CLI   0(R2),X'00'                                                      
         BNE   SHPSFIND                                                         
SHPSCDIR LR    R5,R3               REVERSE DIRECTION AND REPEAT                 
         SR    R5,R6                                                            
         BM    SHPSLPND                                                         
         LR    R7,R5                                                            
         SR    R4,R4                                                            
         D     R4,NOCOLS                                                        
         SR    R4,RE                                                            
         LPR   R4,R4                                                            
         CH    R4,=H'1'                                                         
         BH    SHPSLPND                                                         
         LA    R2,0(R9,R7)                                                      
         CLI   0(R2),X'00'                                                      
         BNE   SHPSFIND                                                         
SHPSLPND BCT   R1,SHPSLOOP         THEN MODIFY DIRECTION & LOOP                 
         SR    R2,R2               SET CC=ZERO IF NO ADJACENT SHIP              
SHPSFIND ST    R7,PLIST            PLIST CONTAINS ADDRESS,IF ANY, OF            
SHPSRCHX XIT1                        ADJACENT SHIP CHARACTER.                   
         EJECT                                                                  
*              VCONADR                                                          
*                                                                               
*              VERIFIES A/N (A1-L10),N/A (1A-10L) OR A/A (AM-LV OR              
*               MA-VL) COORDINATES AND CONVERTS THEM TO LINEAR                  
*               COORDINATES (0-119)                                             
*              INPUTS:                                                          
*                     PLIST    3C = COORDINATES                                 
*                     R1          = POINTER TO CURRENT SCANBLCK LINE            
*              OUTPUTS:                                                         
*                     PLIST    1F = LINEAR COORDINATES (0-119)                  
*                     CC OF ZERO  = INVALID COORDINATES                         
*                     CC OF NEG   = COORDINATES OUTSIDE CURRENT QUADRNT         
*                                                                               
VCONADR  NTR1                                                                   
         SR    R2,R2                                                            
         IC    R2,0(R1)                                                         
         SH    R2,=H'2'                                                         
         BM    VCONBADA            ERROR A IF <2 CHARS ENTERED                  
         CH    R2,=H'2'                                                         
         BL    *+8                                                              
         B     VCONBADA            ERROR A IF >3 CHARS ENTERED                  
         CLI   PLIST,C'M'                                                       
         BL    VCONOKRD            IF 1ST CHAR IS M OR GTR (INC NUM)            
         EX    R2,*+8              TRANSPOSE TWO ELEMENTS                       
         B     *+10                                                             
         MVC   WORK(0),PLIST                                                    
         LA    R3,PLIST(R2)                                                     
         MVC   PLIST(1),1(R3)                                                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PLIST+1(0),WORK                                                  
VCONOKRD CLI   PLIST,C'A'                                                       
         BL    VCONBADA                                                         
         CLI   PLIST,C'L'                                                       
         BH    VCONBADA            ERROR A IF 1ST CHAR NOT A-L                  
         SR    R6,R6               CONVERT A-L TO 0-11                          
         BCTR  R6,0                  A-I = DIGIT NIBBLE -1                      
         CLI   PLIST,C'J'          J-J-L =              +8                      
         BL    *+8                                                              
         LA    R6,8                                                             
         XC    WORK(4),WORK                                                     
         MVN   WORK+3(1),PLIST                                                  
         A     R6,WORK                                                          
         C     R6,NOCOLS                                                        
         BNL   VCONBADB            ERROR B IF 1ST CHAR > COLS LIMIT FOR         
         CLI   PLIST+1,C'1'       THIS GAME                                     
         BL    VCONALPH                                                         
         MVC   WORK(2),=C'00'      2ND CHAR ONE  OR GTR                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),PLIST+1                                                  
         CLC   WORK(2),=C'00'                                                   
         BNE   VCONBADA            ERROR A IF 2ND ELEMENT NOT NUMERIC           
VCONREDO EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PLIST+1(0)                                                   
         CVB   R4,DUB                                                           
         BCTR  R4,0                                                             
         CH    R4,=H'9'            IF 2ND ELEMENT >9                            
         BNH   VCONEND                                                          
         BCTR  R2,0                 IGNORE 2ND CHAR                             
         B     VCONREDO                                                         
VCONALPH CLI   PLIST+1,C'M'      2ND CHAR ALPHA                                 
         BL    VCONBADA                                                         
         CLI   PLIST+1,C'V'                                                     
         BH    VCONBADA            ERROR A IF 2ND CHAR NOT M-V                  
         LA    R4,4                CONVERT M-V TO 0-9                           
         CLI   PLIST+1,C'R'        M-M-R = DIGIT NIBBLE -4                      
         BH    *+6                   S-V =              +4                      
         LNR   R4,R4                                                            
         XC    WORK(4),WORK                                                     
         MVN   WORK+3(1),PLIST+1                                                
         A     R4,WORK                                                          
VCONEND  C     R4,NOROWS           ERROR B IF 2ND ELEMENT > ROWS LIMIT          
         BNL   VCONBADB             FOR THIS GAME                               
         L     R5,NOCOLS                                                        
         MR    R4,R4                                                            
         AR    R6,R5                                                            
         ST    R6,PLIST                                                         
         LPR   R3,R3                                                            
         B     VCONADRX                                                         
VCONBADB LNR   R3,R3               FOR ERROR B  SET CC TO NEG                   
         B     *+6                                                              
VCONBADA SR    R3,R3               FOR ERROR A  SET CC TO ZERO                  
VCONADRX XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MAXSIZE  DC    F'4'                MAX SHIP SIZE                                
RATIO    DC    F'6'                MIN QUADRNAT-FLEET SIZE RATIO                
LMTDFLT  DC    F'120'              DEFAULT QUADRANT SIZE                        
COLSDFLT DC    F'12'                  DO   NO OF COLS                           
ROWSDFLT DC    F'10'                  DO   NO OF ROWS                           
SHTBDFLT DC    X'C204010101C303020202C602020202C401040404' SHIPTAB              
ADFCDFLT DC    X'010B0C0D010B0C0D'    DO   DIRECTION MODIFIERS(X2)              
HTWNDFLT DC    H'18'                                                            
COLLTRS  DC    C'ABCDEFGHIJKL'     COLUMN LETTERS                               
ROWTRS   DC    C'MNOPQRSTUV'       ROW LETTERS (IF ALL ALPHA COORDS)            
PLUSN    DC    C'+ + + + + + + + + + + +'                                       
SHIPCHAR DC    C'BCFD'             SHIP CHARACTERS                              
COORDS   DC    C'N'                COORDS =N FOR A/N, =A FOR ALPHA              
AVAILSHT DS    0CL121                                                           
         DC    X'78'                                                            
         DC    120AL1(*-AVAILSHT)                                               
         DS    0F                                                               
SPACES   DC    CL44' '                                                          
ERROR1   DC    C'INVALID COORDINATES - PLEASE REINPUT'                          
ERROR2   DC    C'COORDINATES OUT OF RANGE - PLEASE REINPUT'                     
ERROR3   DS    0CL59                                                            
         DC    C'FLEET TO QUADRANT SIZE RATIO MORE THAN 1-6  -PLEASE'           
         DC    C' REINPUT'                                                      
ERROR4   DC    C'YOU HAVE NOT ENTERED YOUR FLEET - PLEASE DO SO'                
ERRORB   DC    C'INVALID SHIP CHARACTER '                                       
ERRORC   DC    C'INCONSISTENT SHIP SIZES'                                       
ERRORD   DC    C'SHIP MORE THAN 4 CHARS.'                                       
ERRORE   DC    C'SHIPS TOUCHING         '                                       
ERRORF   DS    0CL58                                                            
         DC    C'SORRY, I CANT GENERATE A VALID FLEET-PLEASE MAKE IT'           
         DC    C' EASIER'                                                       
ERRORG   DS    0CL59                                                            
         DC    C'INVALID QUADRANT SIZE - MUST BE NXM WHERE N=1-12 AND '         
         DC    C'M=1-10'                                                        
ERRORH   DC    C'MISSING INPUT FIELD'                                           
NEXTGAME DC    C'- ENTER OLD, NEW OR YOU TO START A NEW GAME'                   
INITST   DC    C'FLEET ENTERED OK - ENTER FIRST SHOT'                           
STARTHD  DS    0CL60                                                            
         DC    C'BATTLESHIPS * NEW GAME * ENTER YOUR FLEET AND '                
         DC    C'QUADRANT SIZE '                                                
STARTENT DC    C'CURRENT SIZE IS'                                               
YOUWON   DC    C'YOU WON  '                                                     
YOULOST  DC    C'YOU LOST '                                                     
DRPOUT   DC    C'YOU QUIT '                                                     
ADRAW    DC    C'YOU DREW '                                                     
HIT      DC    C'HIT '                                                          
MISS     DC    C'MISS'                                                          
PSHTMASK DC    X'8040201008040201' MASK USED TO ADDRESS BIT LIST PSHTQD         
         EJECT                                                                  
BATTEMP  DSECT                                                                  
VSCANNER DS    V                                                                
VUNSCAN  DS    V                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
PARAML   DS    F                                                                
WORK     DS    CL120                                                            
WORKA    DS    CL120                                                            
PLIST    DS    6F                                                               
PLAYER   DS    C                                                                
SHIPTYPE DS    C                                                                
PSHPTYPE DS    C                                                                
HITORMSS DS    4C                                                               
         DS    0F                                                               
SCANBLCK DS    5CL32               INPUT AREA FOR SCANNER                       
UNSCANP  DS    5CL20               INPUT AREA FOR UNSCAN (P'S SHOTS)            
UNSCANM  DS    5CL20               INPUT AREA FOR UNSCAN (MY SHOTS)             
AUNSCNP  DS    F                                                                
AUNSCNM  DS    F                                                                
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
BATSAVE  DSECT                                                                  
DONOTUSE DS    CL16                                                             
PREFDIR  DS    C                                                                
GIP      DS    C                                                                
PHITS    DS    H                                                                
MHITS    DS    H                                                                
PWINS    DS    H                                                                
MWINS    DS    H                                                                
HITSTWIN DS    H                                                                
NOCOLS   DS    F                                                                
NOROWS   DS    F                                                                
LIMIT    DS    F                                                                
PREVHIT  DS    4F                                                               
ADDFAC   DS    8C                                                               
*                                                                               
       ++INCLUDE GABATFFD                                                       
MSHPTYPE DS    C                                                                
NOHITS   DS    C                                                                
HITSTSNK DS    C                                                                
SMALLEST DS    C                                                                
PQDISPA  DS    CL120                                                            
MYPQUADA DS    CL121                                                            
MFLTABLE DS    20CL6                                                            
         DS    C                                                                
SHIPTAB  DS    4CL5                                                             
         DS    C                                                                
PSHTQD   DS    CL15                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GABAT00   05/01/02'                                      
         END                                                                    
