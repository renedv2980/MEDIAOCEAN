*          DATA SET GAWUM00    AT LEVEL 072 AS OF 05/01/02                      
*PHASE TB1700A                                                                  
WUMPUS   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WUMPWRKX-WUMPWRK,GAWUM00                                         
         USING WUMPWRK,RC          WORKSPACE BASE ADDRESS                       
         L     RA,4(R1)                                                         
         USING TB17FFD,RA          TWA BASE ADDRESS                             
         ST    R1,SYSPARM          SAVE ADDR OF ENTERING PARAMS                 
**********************************************************                      
* MAIN:                                                  *                      
*   MAIN IS THE MAIN (SURPRISED?) ROUTINE WHICH CALLS    *                      
* ALL OTHER SUBROUTINES.                                 *                      
**********************************************************                      
MAIN     MVC   WUMHEAD(60),=C'                                         X        
                                  '                                             
         BAS   RE,GETINPT                                                       
         MVI   BATF,0                                                           
         CLI   ERRF,1              WAS THERE AN INPUT ERROR?                    
         BE    EXIT                IF SO, EXIT                                  
         CLI   16(RA),0            CK NEWGAME FLAG                              
         BNE   M1                  IF NOT NEWGAME, SKIP                         
         MVI   16(RA),1            ELSE SET NEWGAME FLAG                        
         MVI   ARROWS,5                                                         
         BAS   RE,RANDOM           GET ROOM DATA                                
M1       BAS   RE,DISMSG           DISPLAY WARNINGS                             
EXIT     XIT1                                                                   
**********************************************************                      
* GETINPT:                                               *                      
*   GETINPT TAKES CARE OF GETTING THE PLAYERS COMMANDS   *                      
* FROM THE FIRE FIELD.  IT WILL HANDLE VALIDATION        *                      
* OF DATA AND TAKE APPROPRIATE ACTION.                   *                      
**********************************************************                      
GETINPT  NTR1                                                                   
         LA    R3,WUMFIREH         ADDRESS OF TWA HEADER                        
         L     R1,SYSPARM          GET ADDR OF ENTERING PARAMS                  
         L     R2,16(R1)           GET ADDR OF DDCOMFACS                        
         USING COMFACSD,R2         USE BASE R2                                  
         GOTO1 CSCANNER,DMCB,(R3),(6,BLOCK),C',=, '                             
         DROP  R2                  THATS ALL WE NEEDED IT FOR                   
**********************************************************                      
**********************************************************                      
         MVI   ERRF,0              SET ERROR FLAG TO FALSE                      
         NI    BLOCK+2,X'80'       WAS FIRST FIELD NUMERIC?                     
         BNZ   GIN                 IF SO, MOVE TO ROOM                          
         CLI   BLOCK+12,C'C'       CONTINUE?                                    
         BNE   GI1A                                                             
         ZIC   R6,CONTENTS         GET PLAYER LOC IN R6                         
         LA    R7,4                MOVE 4 INTO R7                               
         ZIC   R5,CONTENTS(R7)     GET WUMP LOC IN R5                           
         CR    R6,R5               ARE THEY IN THE SAME ROOM?                   
         BNE   GIX                 IF NOT, CONTINUE                             
GI0      CLI   BATF,1              DID A BAT BRING PLAYER HERE?                 
         BE    GIX                 IF SO, WAKE WUMPUS                           
         MVC   WUMHEAD(59),=C'THE WUMPUS WAS IN YOUR ROOM WHEN HE MOVEDX        
               .  -- S TO RESTART'                                              
GI0A     OI    WUMHEADH+6,X'80'    TRANSMIT MESSAGE FIELD                       
         OI    WUMFIREH+6,X'40'                                                 
         MVI   ERRF,1                                                           
         BAS   RE,NEWGAME                                                       
         B     GIX                                                              
GI1A     CLI   BLOCK+12,C'S'       (RE)START?                                   
         BNE   GI1                 IF NOT, CHECK FIRE                           
         BAS   RE,NEWGAME          ELSE START NEW GAME                          
         B     GIX                 EXIT ROUTINE                                 
GI1      CLI   BLOCK+12,C'F'       FIRE?                                        
         BNE   GIERR               IF NOT, ERROR                                
         BAS   RE,FIRE             ELSE, FIRE ARROW                             
         MVI   ERRF,1              SET FLAG FOR EXIT ON RETURN                  
         BAS   RE,DECARR           DECREASE ON SCREEN                           
         ZIC   R1,ARROWS           GET NUM ARROWS LEFT                          
         SH    R1,=H'1'            DEC                                          
         STC   R1,ARROWS           STORE NEW VALUE                              
         CLI   ARROWS,0            ANY ARROWS LEFT?                             
         BNE   GIX                 YES, NO PROBLEM                              
         CLI   WUMHIT,0            ELSE DID LAST SHOT MISS?                     
         BNE   GIX                 NO, NO PROBLEM                               
         MVC   WUMHEAD(60),=C'YOU RAN OUT OF ARROWS  -- S TO RESTART --X        
                                  '                                             
         BAS   RE,NEWGAME                                                       
         OI    WUMHEADH+6,X'80'    TRANSMIT MESSAGE FIELD                       
         OI    WUMFIREH+6,X'40'                                                 
         B     GIX                 EXIT                                         
GIN      XR    R2,R2               ZERO R2                                      
         L     R0,BLOCK+4                                                       
         ZIC   R1,CONNECT(R2)      GET FIRST CONNECTING ROOM                    
         CR    R0,R1               IS IT ROOM PLAYER WANTS?                     
         BNE   GI2A                IF NOT, CHECK NEXT ROOM                      
         STC   R0,CONTENTS         MOVE TO ROOM R0                              
         B     GIX                 EXIT                                         
GI2A     LA    R2,1(R2)            INC INDEX                                    
         ZIC   R1,CONNECT(R2)      GET SECOND CONN ROOM                         
         CR    R0,R1               IS THIS WHAT PLAYER WANTS?                   
         BNE   GI2B                IF NOT, CHECK THIRD ROOM                     
         STC   R0,CONTENTS         MOVE TO ROOM R0                              
         B     GIX                 EXIT ROUTINE                                 
GI2B     LA    R2,1(R2)            INC INDEX                                    
         ZIC   R1,CONNECT(R2)      GET LAST CONN ROOM                           
         CR    R0,R1               IS THIS ROOM PLAYER WANTS?                   
         BE    GI2C                IF SO, JUMP                                  
GIERR    BAS   RE,ERROR            ELSE PLAYER MADE A MISTAKE                   
         B     GIX                                                              
GI2C     STC   R0,CONTENTS         MOVE TO ROOM R0                              
GIX      XIT1                                                                   
**********************************************************                      
* DECARR:                                                *                      
*   DECARR WILL BE CALLED TO REMOVE AN ARROW FROM THE    *                      
* SCREEN WHEN ONE IS FIRED.  IT TAKES NO OUTSIDE         *                      
* PARAMETERS.                                            *                      
**********************************************************                      
DECARR   NTR1                                                                   
         ZIC   R1,ARROWS           GET NUM OF ARROWS                            
         MH    R1,=H'6'            FIELD LEN = (6*ARROWS)-1                     
         SH    R1,=H'1'                                                         
         LA    R2,6                6 CHARS FROM END TO WIPE OUT                 
         ZIC   R3,=C' '            BLANK SPACE                                  
DA1      STC   R3,WUMAROW(R1)      STORE BLANK ON END                           
         BCTR  R1,0                DEC INDX INTO END WUMAROW                    
         BCT   R2,DA1              REPEAT LP                                    
         OI    WUMAROWH+6,X'80'    TRANSMIT ARROW FIELD                         
         XIT1                                                                   
**********************************************************                      
* ERROR:                                                 *                      
*   ERROR WILL BE CALLED WHEN INVALID INPUT IS           *                      
* ENTERED BY THE PLAYER                                  *                      
**********************************************************                      
ERROR    NTR1                                                                   
         MVC   WUMHEAD(34),=C'*** ERROR ***    --INVALID INPUT--'               
         OI    WUMHEADH+6,X'80'    TRANSMIT MESSAGE FIELD                       
         OI    WUMFIREH+6,X'40'                                                 
         MVI   ERRF,1                                                           
         XIT1                                                                   
**********************************************************                      
* FIRE:                                                  *                      
*   WILL BE CALLED BY GETINPT WHEN PLAYER ENTERS AN 'F'  *                      
* FOLLOWED BY UP TO 5 ROOMS.  NOTE THAT THE ROOMS MAY    *                      
* NOT BE VALID.                                          *                      
**********************************************************                      
FIRE     NTR1                                                                   
         MVI   WUMHIT,0            RESET HIT FLAG                               
         ZIC   R7,CONTENTS         GET CURRENT ARROW ROOM                       
         BAS   RE,GETARRC          GET ARROWS CONN ROOMS                        
         MVI   FIRF,1              SET FIRE FLAG                                
         LA    R2,BLOCK+32         GET NEXT FIELD                               
F1       CLI   0(R2),0             INPUT LENGTH 0?                              
         BE    F3                  DISP MISS MSG & XIT                          
         TM    2(R2),X'80'         IS THERE VALID INPUT?                        
         BZ    FMISS               IF NOT, SCORE MISS                           
         L     R0,4(R2)            GET BIN VALUE                                
         XR    R1,R1               CLEAR R1                                     
         ZIC   R4,ARROWC(R1)       GET FIRST CONN RM                            
         CR    R0,R4               IS ROOM CONNECTED                            
         BE    F2                  YES? TEST FOR HIT                            
         LA    R1,1(R1)            NO, CHECK NEXT ROOM                          
         ZIC   R4,ARROWC(R1)       GET SECOND CONN RM                           
         CR    R0,R4               IS ROOM CONNECTED?                           
         BE    F2                  YES?  TEST FOR HIT                           
         LA    R1,1(R1)            NO, CHECK NEXT ROOM                          
         ZIC   R4,ARROWC(R1)       GET THIRD CONN RM                            
         CR    R0,R4               IS ROOM CONNECTED?                           
         BE    F2                  YES? CHECK FOR HIT                           
* IF WE FALL THRU, THE ROOM IS NOT CONNECTED, SO CHOOSE 1 AT RANDOM             
FERR     TIME  STCK,RAND           GET TIME AS RANDOM NUMBER                    
         ICM   R3,3,RAND+5         GET NIBBLES 11 & 12                          
         SRL   R3,4                DROP LOW BYTE                                
         N     R3,=X'00000003'     GET NUMBER BETWEEN 0-4                       
         CH    R3,=H'3'            NO CONNECT{3}                                
         BE    FERR                IF SO, GET NEW NUMBER                        
         ZIC   R0,ARROWC(R3)       ELSE GET CONN TO MV TO                       
F2       LA    R1,4                IS THIS WUMPUS ROOM?                         
         ZIC   R8,CONTENTS(R1)     WUMPUS STORED IN CONTENTS{4}                 
         CR    R0,R8               WAS IT A HIT?                                
         BE    FHIT                IF SO, SAY SO                                
         ZIC   R8,CONTENTS         GET PLAYER'S ROOM                            
         CR    R0,R8               DID PLAYER SHOOT SELF?                       
         BNE   FMISS               IF NOT, TOTAL MISS                           
         MVC   WUMHEAD(40),=C'YOU SHOT YOURSELF  -- S TO PLAY AGAIN --'         
         B     FX                                                               
FHIT     MVC   WUMHEAD(45),=C'YOU KILLED THE WUMPUS.  -- S TO PLAY AGAIX        
               N --'                                                            
         MVI   WUMHIT,1            SET FLAG FOR WUMPUS HIT                      
         B     FX                  WUMP DEAD, EXIT                              
FMISS    LA    R2,32(R2)           CHECK NEXT FIELD FOR INPT                    
         LR    R7,R0               R7 IS PARAM TO GETARRC                       
         BAS   RE,GETARRC          GET NEW CONNECTING RMS                       
         LA    R3,BLOCK            GET ADDRESS OF INPT BLOCK                    
         A     R3,=F'192'          ARE WE 192 BYTES BEYOND IT?                  
         CR    R2,R3                                                            
         BNH   F1                  BRANCH IF NOT                                
F3       MVC   WUMHEAD(54),=C'YOU MISSED THE WUMPUS.  HE MOVES.  -- C TX        
               O CONTINUE --'                                                   
         BAS   RE,WUMPR            MOVE WUMPUS                                  
FX       OI    WUMHEADH+6,X'80'    TRANSMIT MESSAGE FIELD                       
         OI    WUMFIREH+6,X'40'                                                 
         XIT1                                                                   
**********************************************************                      
* NEWGAME:                                               *                      
*   NEWGAME WILL BE CALLED WHEN THE PLAYER TYPES         *                      
* 'S' AT THE FIRE LINE                                   *                      
**********************************************************                      
NEWGAME  NTR1                                                                   
         MVI   16(RA),0                                                         
         MVC   WUMAROW(29),=C'----> ----> ----> ----> ---->'                    
         OI    WUMAROWH+6,X'80'    TRANSMIT ARROW FIELD                         
         XIT1                                                                   
**********************************************************                      
* DISMSG:                                                *                      
*   THIS ROUTINE WILL CHECK TO SEE IF THE PLAYER SHOULD  *                      
* RECIEVE ANY WARNINGS, OR HAS ENTERED A ROOM WITH ANY   *                      
* HAZARDS                                                *                      
**********************************************************                      
DISMSG   NTR1                                                                   
         MVI   BATF,0              CLEAR BAT FLAG                               
         XR    R5,R5                                                            
         ZIC   R5,CONTENTS         GET PLAYERS CURR LOCATION                    
         EDIT  (R5),(2,WUMCURR),ZERO=NOBLANK                                    
         BAS   RE,GETCON                                                        
         LA    R4,5                5 ROOMS WITH HAZARDS                         
DM1      ZIC   R6,CONTENTS(R4)                                                  
         CR    R5,R6               IS PLAYER IN ROOM WITH OBJECT?               
         BNE   DM2                 IF NOT, CONTINUE                             
* WE CHECK FOR PITS FIRST, SINCE THEY'RE DEADLY                                 
* AND ANY FURTHER MESSAGES ARE USELESS                                          
         CH    R4,=H'2'            CONTENTS{2} IS PIT LOC 1                     
         BNE   DM1A                IF NOT, CHECK PIT 2                          
         BAS   RE,PITR             ELSE WE FELL IN ONE                          
         B     DM13                                                             
DM1A     CH    R4,=H'5'            CONTENTS{5} IS PIT LOC 2                     
         BNE   DM1B                IF NOT, CHECK FOR BATS                       
         BAS   RE,PITR             ELSE WE FELL IN ONE                          
         B     DM13                                                             
* NEXT WE CHECK FOR BATS, SINCE THEY WILL CARRY YOU OFF                         
DM1B     CH    R4,=H'1'            CONTENTS{1} IS BAT LOC 1                     
         BNE   DM1C                IF NOT CHECK BAT LOC 2                       
         BAS   RE,BATR             OOPS, WE FOUND BATS                          
         MVI   BATF,1              SET BAT FLAG                                 
         B     DM2                 STILL HAVE TO SEE IF WE WOKE WUMPY           
DM1C     CH    R4,=H'3'            CONTENTS{3} IS BAT LOC 2                     
         BNE   DM1D                IF NOT, MUST BE WUMPY                        
         BAS   RE,BATR             ELSE FOUND A BAT                             
         MVI   BATF,1              SET BAT FLAG                                 
         B     DM2                 STILL HAVE TO SEE IF WE WOKE WUMPY           
* IF WE REACH THIS POINT, WE MUST BE IN THE WUMPUS' ROOM                        
DM1D     BAS   RE,WUMPR                                                         
         B     DM13                                                             
DM2      BCT   R4,DM1                                                           
         CLI   BATF,1              DID WE GET TAKEN BY A BAT?                   
         BE    DM13                IF SO, NO WARNINGS                           
**********************************************************                      
* ONCE WE ARRIVE HERE, WE ARE SAFE, SO NOW WE CHECK IF                          
* ANY CONNECTING ROOMS HAVE HAZARDS                                             
**********************************************************                      
         XR    R3,R3               ZERO R3                                      
         LA    R4,5                5 HAZARDS TO CHECK                           
DM3      ZIC   R6,CONTENTS(R4)     FIND OUT WHERE HAZARD IS                     
         LA    R2,3                3 ROOMS TO CHECK                             
DM5      BCTR  R2,0                DEC R2                                       
         ZIC   R5,CONNECT(R2)      GET CONNECTING ROOM C(R2)                    
         CR    R5,R6               DOES ROOM CONTAIN HAZARD?                    
         BNE   DM10                IF NOT, CHECK NEXT ROOM                      
         C     R4,=F'4'            IS THERE A WUMPUS?                           
         BNE   DM3A1               IF NOT, CHECK ALL OTHERS                     
         BAS   RE,WUMPW            GIVE WUMPUS WARNING                          
         AH    R3,=H'21'           21 CHARS IN WUMP WARNING                     
DM3A1    C     R4,=F'2'            IS IT PIT 1?                                 
         BNE   DM3A                IF NOT, CHECK PIT 2                          
         BAS   RE,PITW             ELSE PRINT PIT WARNING                       
         AH    R3,=H'19'           19 CHARS IN PIT WARNING                      
         B     DM10                ONLY WUMP & 1 OTHER HAZARD POSS              
DM3A     C     R4,=F'5'            IS IT PIT 2                                  
         BNE   DM3B                IF NOT, CHECK BAT 1                          
         BAS   RE,PITW             ELSE PRINT PIT WARNING                       
         AH    R3,=H'19'           19 CHARS IN PIT WARNING                      
         B     DM10                GO CHECK NEXT ROOM                           
DM3B     C     R4,=F'1'            IS IT BAT 1                                  
         BNE   DM3C                IF NOT CHECK BAT 2                           
         BAS   RE,BATW             ELSE PRINT BAT WARNING                       
         AH    R3,=H'16'           16 CHARS IN BAT WARNING                      
         B     DM10                GO CHECK NEXT ROOM                           
DM3C     C     R4,=F'3'            IS IT BAT 2                                  
         BNE   DM10                IF NOT, NO WARNINGS                          
         BAS   RE,BATW             ELSE PRINT BAT WARNING                       
         AH    R3,=H'16'           16 CHARS IN BAT WARNING                      
DM10     LA    R2,1(R2)            INC R2                                       
         BCT   R2,DM5              INNER LOOPS THRU ROOMS                       
         BCT   R4,DM3              OUTER LOOPS THRU HAZARDS                     
         C     R3,=F'0'            DID WE DISPLAY ANY MESSAGE?                  
         BE    DM13                IF NOT, EXIT                                 
         SH    R3,=H'4'            ELSE, ERASE LAST 'AND '                      
         L     R4,=C'        '                                                  
         ST    R4,WUMHEAD(R3)                                                   
         B     DM13                                                             
DM13     OI    WUMHEADH+6,X'80'    TRANSMIT MESSAGE FIELD                       
         OI    WUMCURRH+6,X'80'    TRANSMIT CURRENT ROOM                        
         OI    WUMCON1H+6,X'80'    TRANSMIT CONNECTING ROOMS                    
         OI    WUMCON2H+6,X'80'                                                 
         OI    WUMCON3H+6,X'80'                                                 
         OI    WUMFIREH+6,X'40'                                                 
DMX      XIT1                                                                   
**********************************************************                      
* GETCON:                                                *                      
*    GETCON WILL GET THE CONNECTING ROOM INFORMATION FOR *                      
* THE CURRENT ROOM WHICH IS PASSED IN R5                 *                      
**********************************************************                      
GETCON   NTR1                                                                   
         MH    R5,=H'3'            MULT BY 3 (0XL3)                             
         LA    R6,CAVE             GET ADDRESS OF CAVE                          
         AR    R6,R5               ADD ADDRESS TO DISPLACEMENT                  
         ZIC   R1,0(R6)            CONNECTING ROOM 1                            
         XR    R2,R2               ZERO R2                                      
         STC   R1,CONNECT(R2)      SAVE ROOM IN TEMP STORAGE                    
         EDIT  (R1),(2,WUMCON1),ZERO=NOBLANK                                    
         LA    R2,1(R2)            INCREMENT INDEX INTO STORAGE                 
         ZIC   R1,1(R6)            CONNECTING ROOM 2                            
         STC   R1,CONNECT(R2)                                                   
         EDIT  (R1),(2,WUMCON2),ZERO=NOBLANK                                    
         LA    R2,1(R2)                                                         
         ZIC   R1,2(R6)            CONNECTING ROOM 3                            
         STC   R1,CONNECT(R2)                                                   
         EDIT  (R1),(2,WUMCON3),ZERO=NOBLANK                                    
         XIT1                                                                   
**********************************************************                      
* GETWUMC:                                               *                      
*   GETWUMC WILL GET THE CONNECTING ROOM INFORMATION FOR *                      
* THE WUMPUS, WHICH IS STORED @CONTENTS{4}               *                      
**********************************************************                      
GETWUMC  NTR1                                                                   
         LA    R1,4                MOVE 4 INTO R1                               
         ZIC   R0,CONTENTS(R1)                                                  
         MH    R0,=H'3'            MULT BY 3 (0XL3)                             
         LA    R6,CAVE             GET ADDRESS OF CAVE                          
         AR    R6,R0               ADD ADDRESS TO DISPLACEMENT                  
         ZIC   R1,0(R6)            CONNECTING ROOM 1                            
         XR    R2,R2               ZERO R2                                      
         STC   R1,WUMPYP(R2)       SAVE ROOM IN TEMP STORAGE                    
         LA    R2,1(R2)            INCREMENT INDEX INTO STORAGE                 
         ZIC   R1,1(R6)            CONNECTING ROOM 2                            
         STC   R1,WUMPYP(R2)                                                    
         LA    R2,1(R2)                                                         
         ZIC   R1,2(R6)            CONNECTING ROOM 3                            
         STC   R1,WUMPYP(R2)                                                    
         XIT1                                                                   
**********************************************************                      
* GETARRC:                                               *                      
*   GETARRC WILL GET THE CONNECTING ROOM INFORMATION FOR *                      
* THE ARROWS.  R7 MUST CONTAIN CURRENT ARROW LOCATION.   *                      
**********************************************************                      
GETARRC  NTR1                                                                   
         MH    R7,=H'3'            MULT BY 3 (0XL3)                             
         LA    R6,CAVE             GET ADDRESS OF CAVE                          
         AR    R6,R7               ADD ADDRESS TO DISPLACEMENT                  
         ZIC   R1,0(R6)            CONNECTING ROOM 1                            
         XR    R2,R2               ZERO R2                                      
         STC   R1,ARROWC(R2)       SAVE ROOM IN TEMP STORAGE                    
         LA    R2,1(R2)            INCREMENT INDEX INTO STORAGE                 
         ZIC   R1,1(R6)            CONNECTING ROOM 2                            
         STC   R1,ARROWC(R2)                                                    
         LA    R2,1(R2)                                                         
         ZIC   R1,2(R6)            CONNECTING ROOM 3                            
         STC   R1,ARROWC(R2)                                                    
         XIT1                                                                   
**********************************************************                      
* PITR:                                                  *                      
*   PITR IS CALLED BY DISMSG WHEN ONE OF THE TWO ROOMS   *                      
* CONTAINING A PIT IS ENTERED.  NOTE THAT IT WILL NOT    *                      
* END THE GAME, DISMSG TAKES CARE OF THAT                *                      
**********************************************************                      
PITR     NTR1                                                                   
         MVC   WUMHEAD(48),=C'YOU FELL INTO A PIT AND DIED.  TYPE S TO X        
               RESTART'                                                         
         MVI   16(RA),0            SET NEWGAME FLAG                             
         XIT1                                                                   
**********************************************************                      
* BATR:                                                  *                      
*   BATR IS CALLED BY DISMSG WHEN ONE OF THE TWO ROOMS   *                      
* CONTAINING BATS IS ENTERED.  IT WILL THEN MOVE THE     *                      
* PLAYER TO A RANDOM LOCATION IN THE MAZE, WHICH MAY     *                      
* OR MAY NOT BE DANGEROUS TO THE PLAYER                  *                      
**********************************************************                      
BATR     NTR1                                                                   
         MVC   WUMHEAD(50),=C'YOU WERE CARRIED OF BY A BAT.  -- C TO COX        
               NTINUE --'                                                       
BR1      TIME  STCK,RAND           GET TIME AS RANDOM NUMBER                    
         ICM   R3,3,RAND+5         GET NIBBLES 11 & 12                          
         SRL   R3,4                DROP LOW BYTE                                
         N     R3,=X'0000001F'     GET NUMBER BETWEEN 0-31                      
         CH    R3,=H'19'           IS NUMBER BETWEEN 0-19?                      
         BH    BR1                 IF NOT, GET NEW NUMBER                       
         STC   R3,CONTENTS         STORE NEW ROOM                               
         XIT1                                                                   
**********************************************************                      
* WUMPR:                                                 *                      
*   WUMPR IS CALLED BY DISMSG WHEN A PLAYER ENTERS THE   *                      
* ROOM CONTAINING THE WUMPUS, OR BY FIRE WHEN THE PLAYER *                      
* FIRES AN ARROW.  THE WUMPUS WILL WAKE AND MAY MOVE TO  *                      
* ANOTHER ROOM (P=.75), OR STAY WHERE HE IS (P=.25).     *                      
* AFTER HE MOVES, IF HE IS IN THE SAME ROOM AS YOU, HE   *                      
* WILL KILL YOU.  THIS WILL NOT BE TAKEN CARE OF BY      *                      
* WUMPR.  THE ROOM WHICH THE WUMPUS MOVES TO MAY NOW     *                      
* CONTAIN ANOTHER HAZARD.                                *                      
**********************************************************                      
WUMPR    NTR1                                                                   
         BAS   RE,GETWUMC          GET ROOMS WUMPUS CAN MOVE TO                 
         CLI   BATF,1              DID A BAT MOVE PLAYER?                       
         BNE   WR1                 IF SO, CHANGE MESSAGE                        
         MVC   WUMHEAD(60),=C'YOU WOKE WUMPY, BUT BATS CARRIED YOU OFF X        
               -- C TO CONTINUE --'                                             
         B     WR2                                                              
WR1      CLI   FIRF,1              ARE WE MOVING WUMPUS BECAUSE OF FIRE         
         BE    WR2                 IF SO, NO MESSAGE                            
         MVC   WUMHEAD(40),=C'YOU WOKE THE WUMPUS. -- C TO CONTINUE --'         
WR2      TIME  STCK,RAND           GET TIME AS RANDOM NUMBER                    
         ICM   R3,3,RAND+5         GET NIBBLES 11 & 12                          
         SRL   R3,4                DROP LOW BYTE                                
         N     R3,=X'00000003'     GET NUMBER BETWEEN 0-4                       
         CH    R3,=H'3'            STAY IN SAME ROOM?                           
         BE    WRX                 IF SO, DO NOTHING                            
         ZIC   R4,WUMPYP(R3)       ELSE GET CONN TO MV TO                       
         LA    R6,4                WUMP IS AT CONTENTS{4}                       
         STC   R4,CONTENTS(R6)     STORE NEW ROOM                               
WRX      XIT1                                                                   
**********************************************************                      
* WUMPW:                                                 *                      
*   WUMPW WILL BE CALLED WHEN A CONNECTING ROOM          *                      
* CONTAINS THE WUMPUS TO DISPLAY AN APPROPRIATE          *                      
* WARNING.  R3 CONTAINS INDEX INTO WUMHEAD TO USE.       *                      
**********************************************************                      
WUMPW    NTR1                                                                   
         LA    R1,21               21 CHARS TO PUT IN HEADER                    
         AR    R3,R1                                                            
         BCTR  R3,0                                                             
WW1      BCTR  R1,0                                                             
         ZIC   R2,WUMPM(R1)        GET CHARS ON RIGHT SIDE                      
         STC   R2,WUMHEAD(R3)      STORE CHAR ON RIGHT                          
         LA    R1,1(R1)                                                         
         BCTR  R3,0                DEC R3                                       
         BCT   R1,WW1              DEC R1 & BRANCH                              
         XIT1                                                                   
**********************************************************                      
* PITW:                                                  *                      
*   PITW WILL BE CALLED WHEN A CONNECTING ROOM CONTAINS  *                      
* ONE OF THE TWO PITS TO DISPLAY AN APPROPRIATE MESSAGE. *                      
* R3 CONTAINS INDEX INTO WUMHEAD.                        *                      
**********************************************************                      
PITW     NTR1                                                                   
         LA    R1,19               19 CHARS TO PUT IN HEADER                    
         AR    R3,R1                                                            
         BCTR  R3,0                                                             
PW1      BCTR  R1,0                                                             
         ZIC   R2,PITM(R1)         GET CHARS ON RIGHT SIDE                      
         STC   R2,WUMHEAD(R3)      STORE CHAR ON RIGHT                          
         LA    R1,1(R1)                                                         
         BCTR  R3,0                DEC R3                                       
         BCT   R1,PW1              DEC R1 & BRANCH                              
         XIT1                                                                   
**********************************************************                      
* BATW:                                                  *                      
*   BATW WILL BE CALLED WHEN A CONNECTING ROOM CONTAINS  *                      
* ONE OF THE TWO BATS TO DISPLAY AN APPROPRIATE MESSAGE. *                      
* R3 CONTAINS INDEX INTO WUMHEAD.                        *                      
**********************************************************                      
BATW     NTR1                                                                   
         LA    R1,16               16 CHARS TO PUT IN HEADER                    
         AR    R3,R1                                                            
         BCTR  R3,0                                                             
BW1      BCTR  R1,0                                                             
         ZIC   R2,BATM(R1)         GET CHARS ON RIGHT SIDE                      
         STC   R2,WUMHEAD(R3)      STORE CHAR ON RIGHT                          
         LA    R1,1(R1)                                                         
         BCTR  R3,0                DEC R3                                       
         BCT   R1,BW1              DEC R1 & BRANCH                              
         XIT1                                                                   
**********************************************************                      
* RANDOM:                                                *                      
*   RANDOM WILL BE CALLED AT THE START OF THE PROGRAM    *                      
* TO RANDOMLY PLACE THE PLAYER, WUMPUS, AND HAZARDS.     *                      
* NO TWO HAZARDS WILL OCCUPY THE SAME ROOM AT START.     *                      
**********************************************************                      
RANDOM   NTR1                                                                   
         XR    R4,R4               ZERO R4                                      
RND1     TIME  STCK,RAND           GET TIME AS RANDOM NUMBER                    
         ICM   R3,3,RAND+5         GET NIBBLES 11 & 12                          
         SRL   R3,4                DROP LOW BYTE                                
         N     R3,=X'0000001F'     GET NUMBER BETWEEN 0-31                      
         CH    R3,=H'19'           IS NUMBER BETWEEN 0-19?                      
         BH    RND1                IF NOT, GET NEW NUMBER                       
         LR    R5,R4               GET LOOP COUNT                               
         C     R5,=F'0'            DON'T CHECK FIRST ROOM                       
         BE    RND3                                                             
RND2     BCTR  R5,0                                                             
         ZIC   R6,CONTENTS(R5)                                                  
         CR    R3,R6               TWO THINGS IN SAME ROOM?                     
         BE    RND1                IF SO, GET NEW ROOM                          
         LA    R5,1(R5)                                                         
         BCT   R5,RND2             AND CHECK NEXT ROOM                          
RND3     STC   R3,CONTENTS(R4)     NOTHING IN THIS ROOM, STORE                  
         LA    R4,1(R4)            INCREMENT INDEX                              
         CH    R4,=H'6'            ARE ALL OBJECTS PLACED?                      
         BL    RND1                IF NOT, STORE NEXT OBJECT                    
         XIT1                                                                   
**********************************************************                      
* CAVE:                                                  *                      
*   CAVE IS A CONSTANT TABLE CONTAINING THE CONNECTING   *                      
* ROOM INFORMATION.  ROOMS ARE NUMBERED 0-19 (0-13H).    *                      
**********************************************************                      
CAVE     DS    0XL3                ROOM NUMBERS 0-19 & CONNECTIONS              
         DC    X'04',X'01',X'07'   0 - 04,01,07                                 
         DC    X'00',X'02',X'09'   1 - 00,02,09                                 
         DC    X'01',X'03',X'0B'   2 - 01,03,11                                 
         DC    X'02',X'04',X'0D'   3 - 02,04,13                                 
         DC    X'03',X'00',X'05'   4 - 03,00,05                                 
         DC    X'0E',X'06',X'04'   5 - 14,06,04                                 
         DC    X'05',X'07',X'10'   6 - 05,07,16                                 
         DC    X'06',X'08',X'00'   7 - 06,08,00                                 
         DC    X'07',X'09',X'11'   8 - 07,09,17                                 
         DC    X'08',X'0A',X'01'   9 - 08,10,01                                 
         DC    X'09',X'0B',X'12'   10- 09,11,18                                 
         DC    X'0A',X'0C',X'02'   11- 10,12,02                                 
         DC    X'0B',X'0D',X'13'   12- 11,13,19                                 
         DC    X'0C',X'0E',X'03'   13- 12,14,03                                 
         DC    X'0D',X'05',X'0F'   14- 13,05,15                                 
         DC    X'13',X'10',X'0E'   15- 19,16,14                                 
         DC    X'0F',X'11',X'06'   16- 15,17,06                                 
         DC    X'10',X'12',X'08'   17- 16,18,08                                 
         DC    X'11',X'13',X'0A'   18- 17,19,10                                 
         DC    X'12',X'0F',X'0C'   19- 18,15,12                                 
**********************************************************                      
*                                                        *                      
*  CONSTANT MESSAGES TO BE DISPLAYED                     *                      
*                                                        *                      
**********************************************************                      
BATM     DS    0CL16                                                            
         DC    C'I HEAR BATS AND '                                              
PITM     DS    0CL19                                                            
         DC    C'I FEEL A DRAFT AND '                                           
WUMPM    DS    0CL21                                                            
         DC    C'I SMELL A WUMPUS AND '                                         
**********************************************************                      
*                                                        *                      
*       WORKING STORAGE AREA                             *                      
*                                                        *                      
**********************************************************                      
WUMPWRK  DSECT                                                                  
TEMP     DS    C                                                                
RAND     DS    D                                                                
ERRF     DS    C                                                                
FIRF     DS    C                                                                
WORK     DS    CL20                                                             
DUB      DS    D                                                                
WUMPYP   DS    CL3                                                              
ARROWC   DS    CL3                                                              
WUMHIT   DS    C                                                                
SYSPARM  DS    F                                                                
DMCB     DS    6F                                                               
BLOCK    DS    6CL32                                                            
WUMPWRKX EQU   *                                                                
**********************************************************                      
*                                                        *                      
*                 D D C O M F A C S                      *                      
*                                                        *                      
**********************************************************                      
       ++INCLUDE DDCOMFACS                                                      
**********************************************************                      
*                                                        *                      
*                       T W A                            *                      
*                                                        *                      
**********************************************************                      
       ++INCLUDE GAWUMFFD                                                       
**********************************************************                      
*                                                        *                      
*    INFORMATION STORED IN TWA THAT MAY CHANGE           *                      
*                                                        *                      
**********************************************************                      
CONTENTS DS    CL6                 6 POSSIBILITIES, EACH HAS RM #:              
*                                  0=PLAYER, 1&3=BATS                           
*                                  2&5=PITS, 4=WUMPUS                           
*                                  IE, CONTENTS+1 HOLDS WUMPUS LOCATION         
ARROWS   DS    C                   NUMBER OF ARROWS PLAYER HAS LEFT             
CONNECT  DS    CL3                                                              
BATF     DS    C                   USED IF PLAYER PRESSED C DUE TO BAT          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072GAWUM00   05/01/02'                                      
         END                                                                    
