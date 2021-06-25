*          DATA SET GAATO00    AT LEVEL 089 AS OF 05/14/02                      
*PHASE TB1600C                                                                  
         TITLE  'GAME ATOMBOX'                                                  
ATOMBOX  CSECT                                                                  
         PRINT  NOGEN                                                           
         NMOD1  ATOMWRKX-ATOMWRK,GAATO00,RR=R2                                  
         USING  ATOMWRK,RC            WORKSPACE BASE ADDRESS                    
         L      RA,4(R1)                                                        
         USING  TB16FFD,RA            TWA BASE ADDRESS                          
         ST     R2,RELO               RELOCATION FACTOR                         
         EJECT                                                                  
*************************************************************                   
*                      MAIN PROGRAM                         *                   
*************************************************************                   
HELLO    CLI    16(RA),0              CHECK NEWGAME FLAG                        
         BNE    GO                                                              
         BAS    RE,NEWGAME                                                      
GO       BAS    RE,ERRCHECK           CHECK FOR INVALID INPUT                   
         CLI    ATOGUES,C'G'          CHECK IF USER WANTS TO GUESS              
         BNE    BOX                                                             
         BAS    RE,GUESS                                                        
         B      OUT                                                             
BOX      BAS    RE,FINDFS             PROCESS FIRING OF RAYS                    
         BAS    RE,DISPLAY            DISPLAY RESULTS                           
OUT      OI     ATOCURSH+6,X'40'                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                        ERRCHECK                           *                   
*************************************************************                   
ERRCHECK NTR1                         CHECKS FOR INVALID INPUT                  
         MVC    ATOHEAD,REGMESS                                                 
         CLI    ATOGUESH+5,0          GUESS FIELD SHOULD BE BLANK OR            
         BE     GUESOK                                                          
         CLI    ATOGUES,C'G'          HAVE A 'G' IN IT                          
         BE     GUESOK                                                          
         MVI    ATOGUES,C' '          BLANK OUT INVALID INPUT                   
         OI     ATOGUESH+6,X'80'                                                
         B      ERROR                                                           
GUESOK   SR     R7,R7                                                           
         LA     R2,ATOCORNH                                                     
ELOOP    ZIC    R3,0(R2)              R3 POINTS TO ID#                          
         AR     R3,R2                                                           
         S      R3,=F'8'                                                        
         CLI    0(R3),1               ID OF ONE MEANS CORNER OF BOX             
         BE     ENEXT                                                           
         CLI    0(R3),2               ID OF TWO MEANS EDGE OF BOX               
         BE     EEDGE                                                           
         CLI    8(R2),C'.'            IF INSIDE THEN MUST BE '.' OR 'X'         
         BE     ENEXT                                                           
         CLI    8(R2),C'X'                                                      
         BE     ENEXT                                                           
         B      ERROR                                                           
EEDGE    LA     R3,MATRIX(R7)         IF MATRIX POSITION ALREADY HAS            
         CLI    0(R3),0               A NON-ZERO VALUE THEN THAT IS             
         BE     ECHECK                WHAT SHOULD BE ON THE SCREEN              
         CLC    0(1,R3),8(R2)                                                   
         BE     ENEXT                                                           
         B      ERROR                                                           
ECHECK   CLI    8(R2),C'.'            IF MATRIX POSITION IS ZERO THEN           
         BE     ENEXT                 SCREEN SHOULD ONLY HAVE A '.'             
         CLI    8(R2),C'F'            OR 'F'                                    
         BE     ENEXT                                                           
         B      ERROR                                                           
ENEXT    LA     R7,1(R7)              DO THIS FOR 100 MATRIX POSITIONS          
         C      R7,=F'100'                                                      
         BE     EOUT                                                            
         BAS    RE,NEXTEF                                                       
         B      ELOOP                                                           
ERROR    MVC    ATOHEAD,ERRMESS       CHANGES HEADER TO ERROR MESSAGE           
EOUT     OI     ATOHEADH+6,X'80'      TRANSMITS HEADER                          
         B      EXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                         FINDFS                            *                   
*************************************************************                   
FINDFS   NTR1                         LOCATES F'S ON THE EDGES                  
         LA     R2,ATOCORNH           CORRESPONDS TO FIRST MATRIX POS.          
         SR     R7,R7                                                           
MLOOP    ZIC    R3,0(R2)              POINTS R3 TO ID#                          
         AR     R3,R2                                                           
         S      R3,=F'8'                                                        
         CLI    0(R3),X'02'           ID OF TWO MEANS EDGE OF BOX               
         BNE    FNEXT                                                           
         CLI    8(R2),C'F'            IF USER PLACED AN 'F' THERE               
         BNE    FNEXT                                                           
         LA     R3,MATRIX(R7)         AND THER ISN'T A CLUE THERE               
         CLI    0(R3),0               ALREADY                                   
         BNE    FNEXT                                                           
         BAS    RE,FIRERAY            THEN CALL FIRE RAY                        
FNEXT    LA     R7,1(R7)              DO FOR 100 MATRIX POSITIONS               
         C      R7,=F'100'                                                      
         BE     EXIT                                                            
         BAS    RE,NEXTEF             BUMPS R2 TO NEXT EXTENDED FIELD           
         B      MLOOP                                                           
         EJECT                                                                  
*************************************************************                   
*                        DISPLAY                            *                   
*************************************************************                   
DISPLAY  NTR1                         DISPLAYS CLUES TO FIRED RAYS              
         LA     R2,ATOCORNH                                                     
         SR     R7,R7                                                           
DLOOP    ZIC    R3,0(R2)              POINTS R3 TO ID#                          
         AR     R3,R2                                                           
         S      R3,=F'8'                                                        
         CLI    0(R3),X'01'           ID OF ONE MEANS CORNER OF BOX             
         BE     DNEXT                                                           
         CLI    0(R3),X'02'           ID OF TWO MEANS EDGE OF BOX               
         BE     EDGE                                                            
         CLI    8(R2),C'X'            MUST BE INSIDE SO BLANK OUT               
         BNE    BLANK                 ALL NON X'S                               
         B      DNEXT                                                           
EDGE     LA     R3,MATRIX(R7)         TRANSFER ALL NON-ZERO MATRIX              
         CLI    0(R3),0               VALUES TO CORRESPONDING EDGE              
         BE     BLANK                 FIELDS AND BLANK ALL OTHERS               
         MVC    8(1,R2),0(R3)                                                   
         B      DNEXT                                                           
BLANK    MVI    8(R2),C'.'                                                      
DNEXT    OI     6(R2),X'80'           TRANSMIT                                  
         LA     R7,1(R7)              DO FOR 100 MATRIX POSITIONS               
         C      R7,=F'100'                                                      
         BE     EXIT                                                            
         BAS    RE,NEXTEF             BUMP R2 TO NEXT EXTENDED FIELD            
         B      DLOOP                                                           
         EJECT                                                                  
*************************************************************                   
*                        FIRERAY                            *                   
*************************************************************                   
FIRERAY  NTR1                         FIRES RAY AND PUTS RESULT IN              
         ST     R7,STARTLOC           MATRIX - R7 HOLDS MATRIX START            
         SR     R4,R4                 POSITION                                  
         LR     R5,R7                                                           
         D      R4,=F'10'                                                       
         C      R4,=F'0'              IF ON LEFT EDGE                           
         BE     FRIGHT                FIRE RIGHT                                
         C      R4,=F'9'              IF ON RIGHT EDGE                          
         BE     FLEFT                 FIRE LEFT                                 
         C      R5,=F'0'              IF ON TOP EDGE                            
         BE     FDOWN                 FIRE DOWN                                 
         MVC    VEC,=F'-10'           ELSE FIRE UP                              
         B      FIRE                                                            
FRIGHT   MVC    VEC,=F'1'                                                       
         B      FIRE                                                            
FLEFT    MVC    VEC,=F'-1'                                                      
         B      FIRE                                                            
FDOWN    MVC    VEC,=F'10'                                                      
*                                                                               
FIRE     LR     R3,R7                 R7 WILL HOLD CURRENT POSITION             
         A      R3,VEC                                                          
         ST     R3,NEW                NEW, LOWDIAG AND HIGHDIAG ARE             
         L      R4,VEC                COMPUTED TO DETERMINE PATH OF             
         LPR    R4,R4                 RAY                                       
         L      R5,=F'11'                                                       
         SR     R5,R4                                                           
         SR     R3,R5                                                           
         ST     R3,LOWDIAG                                                      
         AR     R3,R5                                                           
         AR     R3,R5                                                           
         ST     R3,HIGHDIAG                                                     
CHECKNEW L      R3,NEW                A ONE IN NEW POS. MEANS RAY IS            
         LA     R4,MATRIX(R3)         ABSORBED                                  
         CLI    0(R4),1                                                         
         BE     ABSORB                                                          
         L      R3,LOWDIAG            A ONE IN LOWDIAG MEANS RAY TURNS          
         LA     R4,MATRIX(R3)         RIGHT OR DOWN                             
         CLI    0(R4),1                                                         
         BE     LOWTURN                                                         
         L      R3,HIGHDIAG           A ONE IN HIGHDIAG MEANS RAY TURNS         
         LA     R4,MATRIX(R3)         LEFT OR UP                                
         CLI    0(R4),1                                                         
         BE     HIGHTURN                                                        
         L      R7,NEW                OTHERWISE RAY PROCEEDS FORWARD            
         B      CHECKLOC                                                        
ABSORB   LA     R3,MATRIX             PUT IN CLUE FOR ABSORBTION                
         A      R3,STARTLOC                                                     
         MVI    0(R3),C'H'                                                      
         B      SCOREONE                                                        
LOWTURN  L      R4,VEC                CHANGE VECTOR TO RIGHT OR DOWN            
         LPR    R4,R4                                                           
         L      R5,=F'11'                                                       
         SR     R5,R4                                                           
         ST     R5,VEC                                                          
         B      CHECKLOC                                                        
HIGHTURN L      R4,VEC                CHANGE VECTOR TO LEFT OR UP               
         LPR    R4,R4                                                           
         L      R5,=F'11'                                                       
         SR     R5,R4                                                           
         LNR    R5,R5                                                           
         ST     R5,VEC                                                          
CHECKLOC SR     R4,R4                 CHECK TO SEE IF CURRENT POSITION          
         LR     R5,R7                 IS OUTSIDE OF BOX                         
         D      R4,=F'10'                                                       
         C      R4,=F'0'                                                        
         BE     EMERGE                                                          
         C      R4,=F'9'                                                        
         BE     EMERGE                                                          
         C      R5,=F'0'                                                        
         BE     EMERGE                                                          
         C      R5,=F'9'                                                        
         BE     EMERGE                                                          
         B      FIRE                                                            
EMERGE   C      R7,STARTLOC           CHECK IF ENDING POSITION IS SAME          
         BE     REFLECT               AS STARTING POSITION                      
         IC     R4,ECHAR              ELSE STORE CURRENT CHARACTER IN           
         STC    R4,MATRIX(R7)         STARTING AND ENDING POSITIONS             
         L      R7,STARTLOC                                                     
         STC    R4,MATRIX(R7)                                                   
         CLI    ECHAR,C'9'            JUMP FROM '9' TO 'A' LIKE IN HEX          
         BE     JUMP                                                            
         IC     R4,ECHAR                                                        
         LA     R4,1(R4)                                                        
         STC    R4,ECHAR                                                        
         B      SCORETWO                                                        
JUMP     MVI    ECHAR,C'A'                                                      
         B      SCORETWO                                                        
REFLECT  LA     R4,MATRIX(R7)         STORE 'R' IN START POSITION               
         MVI    0(R4),C'R'                                                      
SCOREONE LA     R3,1                  REFLECTIONS AND ABSORBTIONS SCORE         
         BAS    RE,ADDSCORE           ONE                                       
         B      EXIT                                                            
SCORETWO LA     R3,2                  EMERGING RAYS SCORE TWO                   
         BAS    RE,ADDSCORE                                                     
         B      EXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                        NEWGAME                            *                   
*************************************************************                   
NEWGAME  NTR1                                                                   
         MVI    16(RA),1              FLIP NEWGAME FLAG                         
         XC     MATRIX,MATRIX                                                   
         LA     R7,4                  PLACE FOUR ATOMS                          
CALL     TIME   STCK,TIMADD                                                     
         ICM    R3,3,TIMADD+5         NEED NIBBLES 11-12                        
         SRL    R3,4                                                            
         N      R3,=X'0000003F'       GIVES RANDOM # FROM 0-63                  
         ZIC    R4,INSIDE(R3)         TAKES 0-63 TO 11-88                       
         LA     R5,MATRIX(R4)                                                   
         CLI    0(R5),1               IF ATOM ALREADY PLACED                    
         BE     CALL                  TRY AGAIN                                 
         MVI    0(R5),1               ELSE PLACE ATOM                           
         BCT    R7,CALL                                                         
         SR     R7,R7                 THIS LOOP CLEARS SCREEN FIELDS            
         LA     R2,ATOCORNH                                                     
NLOOP    ZIC    R3,0(R2)              R3 POINTS TO ID#                          
         AR     R3,R2                                                           
         S      R3,=F'8'                                                        
         CLI    0(R3),1               DOESN'T CLEAR CORNERS                     
         BE     NNEXT                                                           
         CLI    8(R2),C'F'            DOESN'T CLEAR F'S                         
         BE     NNEXT                                                           
         MVI    8(R2),C'.'                                                      
         OI     6(R2),X'80'                                                     
NNEXT    LA     R7,1(R7)              DO FOR 100 MATRIX POSITIONS               
         C      R7,=F'100'                                                      
         BE     NOUT                                                            
         BAS    RE,NEXTEF             BUMP TO NEXT EXTENDED FIELD               
         B      NLOOP                                                           
NOUT     MVI    ECHAR,C'0'            CLUES GO FROM '0' TO 'D'                  
         MVI    SCORE,0                                                         
         B      EXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                         GUESS                             *                   
*************************************************************                   
GUESS    NTR1                         SCORES INSIDE OF BOX AND ENDS             
         LA     R2,ATOCORNH           GAME                                      
         SR     R7,R7                 COUNTER FOR MATRIX POSITION               
         SR     R6,R6                 COUNTER FOR NUMBER OF ATOMS FOUND         
GLOOP    ZIC    R3,0(R2)              POINTS R3 TO ID#                          
         AR     R3,R2                                                           
         S      R3,=F'8'                                                        
         CLI    0(R3),3               ID# OF THREE MEANS INSIDE BOX             
         BNE    GNEXT                                                           
         LA     R3,MATRIX(R7)                                                   
         CLI    0(R3),1               IS ATOM REALLY THERE                      
         BNE    GCHECK                                                          
         CLI    8(R2),C'X'            DOES USER THINK ATOM IS THERE             
         BE     GGOOD                                                           
         MVI    8(R2),C'O'            AN ATOM WAS LEFT UNGUESSED                
         B      SCOREFIV                                                        
GGOOD    LA     R6,1(R6)              USER FOUND ANOTHER ATOM                   
         B      GNEXT                                                           
GCHECK   CLI    8(R2),C'X'            DOES USER THINK ATOM IS THERE             
         BNE    GNEXT                                                           
         MVI    8(R2),C'Z'            ATOM ISN'T REALLY THERE                   
SCOREFIV LA     R3,5                                                            
         BAS    RE,ADDSCORE                                                     
GNEXT    OI     6(R2),X'80'           TRANSMIT                                  
         LA     R7,1(R7)              DO FOR 100 MATRIX POSITIONS               
         C      R7,=F'100'                                                      
         BE     GOUT                                                            
         BAS    RE,NEXTEF             BUMPS R2 TO NEXT EXTENDED FIELD           
         B      GLOOP                                                           
GOUT     MVI    ATOGUES,C' '          CLEARS GUESS FIELD                        
         OI     ATOGUESH+6,X'80'                                                
         C      R6,=F'4'              WAS GUESS PERFECT                         
         BNE    NOTGOOD                                                         
         CLC    SCORE,=X'10'          WAS SCORE <= 16                           
         BH     NOTGOOD                                                         
         MVC    ATOHEAD,GOODEND       MOVE IN GOOD MESSAGE                      
         B      GSKIP                                                           
NOTGOOD  MVC    ATOHEAD,BADEND        MOVE IN BAD MESSAGE                       
GSKIP    IC     R3,=C'0'                                                        
         AR     R3,R6                                                           
         STC    R3,ATOHEAD+12         MOVE IN NUMBER FOUND                      
         OI     ATOHEADH+6,X'80'      TRANSMIT MESSAGE FIELD                    
         MVI    16(RA),0              FLIPS NEWGAME FLAG BACK TO TRUE           
         B      EXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                        ADDSCORE                           *                   
*************************************************************                   
ADDSCORE NTR1                         ADDS CONTENTS OF R3 TO SCORE AND          
         ZIC    R4,SCORE              DISPLAYS NEW SCORE ON SCREEN              
         AR     R3,R4                                                           
         STC    R3,SCORE                                                        
         LA     R5,ATOSCOR+6                                                    
         EDIT   SCORE,(2,(R5)),ALIGN=LEFT                                       
         OI     ATOSCORH+6,X'80'                                                
         B      EXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                         NEXTEF                            *                   
*************************************************************                   
NEXTEF   ZIC    R3,0(R2)              BUMPS R2 TO NEXT EXTENDED FILED           
         AR     R2,R3                                                           
         TM     1(R2),X'02'           BYTE ONE OF FIELD HEADER SAYS             
         BZ     NEXTEF                WHETHER FIELD IS EXTENTED OR NOT          
         BR     RE                                                              
         EJECT                                                                  
*************************************************************                   
*                        CONSTANTS                          *                   
*************************************************************                   
RELO     DS     F                     RELOCATION CONSTANT                       
INSIDE   DC     AL1(11,12,13,14,15,16,17,18,21,22,23,24,25,26,27,28)            
         DC     AL1(31,32,33,34,35,36,37,38,41,42,43,44,45,46,47,48)            
         DC     AL1(51,52,53,54,55,56,57,58,61,62,63,64,65,66,67,68)            
         DC     AL1(71,72,73,74,75,76,77,78,81,82,83,84,85,86,87,88)            
REGMESS  DC     CL60'FIRE RAY(F), PLACE ATOM(X), OR GUESS(G)'                   
ERRMESS  DC     CL60'INVALID INPUT HAS BEEN DISREGARDED'                        
GOODEND  DC     CL60'YOU GUESSED 0 OUT OF 4 - EXCELLENT! - FIRE TO BEGIX        
               N AGAIN'                                                         
BADEND   DC     CL60'YOU GUESSED 0 OUT OF 4 - FIRE TO BEGIN AGAIN'              
         EJECT                                                                  
*************************************************************                   
*                        WORKSPACE                          *                   
*************************************************************                   
ATOMWRK  DSECT                                                                  
STARTLOC DS     F                     STARTING RAY POSITION                     
VEC      DS     F                     UP,DOWN,LEFT,RIGHT = -10,10,-1,1          
NEW      DS     F                     CURRENT POSITION + VEC                    
LOWDIAG  DS     F                     LOWER NUMBERED DIAGONAL                   
HIGHDIAG DS     F                     HIGHER NUMBERED DAIGONAL                  
WORK     DS     CL17                  USED FOR EDIT MACRO                       
DUB      DS     D                                                               
TIMADD   DS     D                     USED FOR TIME MACRO                       
ATOMWRKX EQU    *                                                               
         EJECT                                                                  
*************************************************************                   
*                           TWA                             *                   
*************************************************************                   
       ++INCLUDE GAATOFFD                                                       
MATRIX   DS      CL100                THE INSIDE POSITONS HOLD ONES FOR         
*                                     WHERE THE ATOMS ACTUALLY ARE AND          
*                                     THE EDGE POSITIONS HOLD THE CLUES         
ECHAR    DS      CL1                  HOLDS CURRENT CLUE FOR A RAY THAT         
*                                     EMERGES                                   
SCORE    DS      CL1                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089GAATO00   05/14/02'                                      
         END                                                                    
