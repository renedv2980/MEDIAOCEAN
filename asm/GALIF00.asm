*          DATA SET GALIF00    AT LEVEL 062 AS OF 05/01/02                      
*PHASE TB0E00A                                                                  
         TITLE 'GAME OF LIFE'                                                   
         PRINT NOGEN                                                            
LIFECNTL CSECT                                                                  
         NMOD1 LIFEWRKX-LIFEWORK,GALIF00                                        
         L     RA,4(R1)                                                         
         USING LIFED,RA                                                         
         USING LIFEWORK,RC                                                      
         EJECT                                                                  
*************************************************************                   
******         MAIN BODY - PERFORM GENERATIONS **************                   
*************************************************************                   
*                                                                               
*    R4 - FUNCTIONS PASS VALUES BACK IN R4                                      
*    R5 - NUMBER OF GENERATIONS REMAINING TO PERFORM                            
*    R6 - CURRENT ROW.  THE BOTTOMMOST ROW IS ROW 1.                            
*    R7 - CURRENT COLUMN.  THE LEFTMOST COLUMN IS COLUMN 0.                     
*    R8 - TABLE OFFSET (IN WORDS) OF THE CURRENT ROW.                           
*                                                                               
*    TABLE - CONTAINS THE POPULATION                                            
*    PREVLOC - THE LOCATION OF THE PREVIOUS ROW                                 
*    NEXTLOC - THE LOCATION OF THE NEXT ROW                                     
*************************************************************                   
         SPACE 2                                                                
         CLI   36(RA),0            FIRST TIME                                   
         BNE   CONTINU                                                          
         BAS   RE,INITIAL                                                       
         B     ERREND                                                           
CONTINU  BAS   RE,VERIFY                                                        
         LTR   R4,R4               IF DIDN'T VERIFY THEN END                    
         BNZ   ERREND                                                           
*                                                                               
         CVB   R5,GENS             DO FOR EACH GENERATON                        
LOOPR5   LA    R3,KSIZE                                                         
         LA    R4,KWIDTH                                                        
         SR    R3,R4                                                            
         ST    R3,PREVLOC          PREVLOC INITIALLY PTS TO LAST ROW            
         LA    R3,KWIDTH           NEXTLOC INITIALLY PTS TO 2ND ROW             
         ST    R3,NEXTLOC                                                       
         BAS   RE,FILTBL             MOVE POPULATION GRID TO TABLE              
         LA    R6,KLENGTH            DO FOR EACH ROW (TOP TO BOTTOM)            
         SR    R8,R8                                                            
LOOPR6   SR    R7,R7                   DO FOR EACH COL (LEFT TO RIGHT)          
LOOPBEG  EQU   *                                                                
         SR    R3,R3                                                            
*                                                                               
         BAS   RE,NABOR                  R4 CONTAIN NUMBER OF NEIBORS           
*                                                                               
         LA    R3,BMASK(R4)              CHECK FOR BIRTH                        
         CLI   0(R3),X'FF'                                                      
         BNE   NOBIRTH                                                          
         MVI   WORKCHAR,KPERSON                                                 
         B     FILLIN                                                           
*                                                                               
NOBIRTH  EQU   *                                                                
         LA    R3,DMASK(R4)              CHECK FOR DEATH                        
         CLI   0(R3),X'FF'                                                      
         BNE   ENDLOOP                                                          
         MVI   WORKCHAR,KNOPERSN                                                
*                                                                               
FILLIN   EQU   *                                                                
         LA    R4,KLENGTH                CALCULATE CORRSPONDING TWA             
         SR    R4,R6                              POSITION                      
         MH    R4,=H'26'                                                        
         LA    R4,0(R4,R8)                                                      
         LA    R4,LIFFLDH(R4)                                                   
         LA    R3,17(R4,R7)                                                     
         CLC   0(1,R3),WORKCHAR                                                 
         BE    ENDLOOP                                                          
         MVC   0(1,R3),WORKCHAR        FILL TWA POSITION                        
*                                                                               
         LA    R3,15(R4)               SET UP HEADER TO OUTPUT FIELD            
         OI    0(R3),X'80'             SET TRANSMIT FLAG                        
         NI    1(R3),X'80'                                                      
         OI    1(R3),KWIDTH            SET LENGTH OF FIELD                      
*                                                                               
ENDLOOP  EQU   *                                                                
         LA    R7,1(R7)                NEXT COLUMN                              
         LA    R3,KWIDTH                                                        
         CR    R7,R3                                                            
         BL    LOOPBEG                                                          
*                                                                               
         ST    R8,PREVLOC              INCREMENT OFFSETS FOR ROW                
         LA    R8,KWIDTH(R8)                                                    
         LA    R3,KWIDTH(R8)                                                    
         ST    R3,NEXTLOC                                                       
         LA    R3,2                    HANDLE ROW WRAPAROUND FOR                
         CR    R6,R3                        NEXTLOC                             
         BNE   P2                      (R6 WILL DECREMENT SO COMPARE            
         MVC   NEXTLOC,=F'0'             IT TO 2 FOR ROW 1)                     
*                                                                               
P2       BCT   R6,LOOPR6             NEXT ROW                                   
         BCT   R5,LOOPR5           NEXT GENERATION                              
*                                                                               
         OI    LIFENDH+6,X'40'     INSERT CURSOR AT END                         
*                                                                               
         CLC   LIFHEAD(58),BLANKS  CLEAR ERROR FIELD                            
         BE    ERREND                                                           
         FOUT  LIFHEADH,BLANKS,58                                               
*                                                                               
ERREND   XMOD1 1                                                                
*****************************************************                           
         EJECT                                                                  
*****************************************************                           
*   SUBROUTINE FILTBL  - COPIES THE POPULATION GRID INTO TABLE                  
*****************************************************                           
*                                                                               
FILTBL   NTR1                                                                   
         SR    R2,R2                R2=OFFSET OF CURRENT ROW IN TABLE           
         LA    R3,17                R3=OFFSET OF CURRENT ROW IN TWA             
         LA    R6,KLENGTH                                                       
*                                                                               
FILLOOP  EQU   *                    DO FOR EACH ROW                             
         LA    R7,TABLE(R2)                                                     
         LA    R8,LIFFLDH(R3)                                                   
         MVC   0(KWIDTH,R7),0(R8)                                               
         LA    R2,KWIDTH(R2)          COPY THIS ROW                             
         LA    R3,26+KWIDTH(R3)                                                 
         BCT   R6,FILLOOP           NEXT ROW                                    
*                                                                               
         XIT1                                                                   
*******************************************************                         
         EJECT                                                                  
******************************************************                          
*   SUBROUTINE  INITIAL                                                         
*****************************************************                           
*                                                                               
INITIAL  NTR1                                                                   
         MVI   36(RA),X'FF'                                                     
         MVI   LIFBORN,C'3'                                                     
         OI    LIFBORNH+6,X'80'                                                 
         MVC   LIFDIED(7),=7C'0145678'                                          
         OI    LIFDIEDH+6,X'80'                                                 
         MVI   LIFGEN,C'1'                                                      
         OI    LIFGENH+6,X'C0'                                                  
         XIT1                                                                   
*                                                                               
*****************************************************                           
         EJECT                                                                  
*****************************************************                           
*  FUNCTION NABOR  - CALCULATES NUMBER OF NEIGHORS TO THE                       
*                       CURRENT ROW,COLUMN.                                     
*****************************************************                           
* INPUTS -  R7-  CURRENT COLUMN                                                 
*           R8-  OFFSET OF FIRST COLUMN OF CURRENT ROW                          
*           NEXTLOC- OFFSET OF FIRST COLUMN OF NEXT ROW                         
*           PREVLOC- OFFSET OF FIRST COLUMN OF PREVIOUS ROW                     
*           KPERSON - THE CONSTANT USED TO IDENTIFY A PERSON                    
* OUTPUTS-  R4-  NUMBER OF NEIGHBORS TO THE CURRENT COL,ROW                     
******************************************************                          
*                                                                               
NABOR    NTR1                                                                   
         SR    R4,R4                                                            
*                                                                               
         LR    R2,R7                                                            
         BCTR  R2,0                                                             
         LTR   R7,R7               IF CURRENT COLUMN IS LEFTMOST THEN           
         BNZ   LEF1                      RIGHTMOST COL IS LEFT NEIGHBOR         
         LA    R2,KWIDTH-1                                                      
*                                                                               
LEF1     LA    R5,TABLE(R2)                                                     
*                                                                               
         LA    R3,0(R8,R5)                                                      
         CLI   0(R3),KPERSON      CHECK SAME ROW FOR LEFT NEIGHBOR              
         BNE   LEF2                                                             
         LA    R4,1(R4)                                                         
LEF2     L     R6,PREVLOC                                                       
         LA    R3,0(R5,R6)                                                      
         CLI   0(R3),KPERSON      CHECK PREV ROW FOR LEFT NEIGHBOR              
         BNE   LEF3                                                             
         LA    R4,1(R4)                                                         
LEF3     L     R6,NEXTLOC                                                       
         LA    R3,0(R5,R6)                                                      
         CLI   0(R3),KPERSON      CHECK NEXT ROW LEFT NEIGHBOR                  
         BNE   RGT                                                              
         LA    R4,1(R4)                                                         
*                                                                               
RGT      LR    R2,R7                                                            
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R6,KWIDTH-1        IF CURRENT COL  IS RIGHTMOST THEN             
         CR    R7,R6                                                            
         BNZ   RGT1                    LEFTMOST COLUMN IS LEFT NEIGHBOR         
         LA    R2,0                                                             
*                                                                               
RGT1     LA    R5,TABLE(R2)                                                     
*                                                                               
         LA    R3,0(R8,R5)                                                      
         CLI   0(R3),KPERSON     CHECK SAME ROW FOR RIGHT NEIGHBOR              
         BNE   RGT2                                                             
         LA    R4,1(R4)                                                         
RGT2     L     R6,PREVLOC                                                       
         LA    R3,0(R5,R6)       CHECK PREV ROW RIGHT NEIGHBOR                  
         CLI   0(R3),KPERSON                                                    
         BNE   RGT3                                                             
         LA    R4,1(R4)                                                         
RGT3     L     R6,NEXTLOC                                                       
         LA    R3,0(R5,R6)                                                      
         CLI   0(R3),KPERSON      CHECK NEXT ROW RIGHT NEIGHBOR                 
         BNE   MID                                                              
         LA    R4,1(R4)                                                         
*                                                                               
MID      LA    R5,TABLE(R7)                                                     
*                                                                               
         L     R6,PREVLOC                                                       
         LA    R3,0(R5,R6)                                                      
         CLI   0(R3),KPERSON          CK PREVIOUS ROW SAME COL                  
         BNE   MID1                                                             
         LA    R4,1(R4)                                                         
MID1     L     R6,NEXTLOC                                                       
         LA    R3,0(R5,R6)                                                      
         CLI   0(R3),KPERSON          CK NEXT ROW SAME COL                      
         BNE   XITNBR                                                           
         LA    R4,1(R4)                                                         
*                                                                               
XITNBR   XIT1  REGS=(R4)                                                        
******************************************************                          
         EJECT                                                                  
*****************************************************                           
*  FUNCTION VERIFY - VERIFIES USER INPUT                                        
*****************************************************                           
* INPUTS -  TWA                                                                 
* OUTPUTS - R4-  0 IF NO PROBLEMS                                               
*                1 IF BAD DATA                                                  
*           DMASK - MASK OF DIGITS WHICH ARE IN DEATH FIELD                     
*           BMASK - MASK OF DIGITS WHICH ARE IN BIRTH FIELD                     
*****************************************************                           
*                                                                               
VERIFY   NTR1                                                                   
         XC    BMASK,BMASK                                                      
         LA    R2,LIFBORN                                                       
         SR    R3,R3               IF NO BIRTH INPUT THEN                       
         IC    R3,LIFBORNH+5          DONT VALIDATE IT                          
         LTR   R3,R3                                                            
         BZ    VA15                                                             
*                                                                               
VA1      CLI   0(R2),C'0'          FOR EACH DIGIT IN BIRTH ENTRY                
         BL    ERROR1                                                           
         CLI   0(R2),C'8'           VERIFY THEY ARE NUMERIC (0-8)               
         BH    ERROR1                                                           
         PACK  DUB,0(1,R2)                                                      
         CVB   R4,DUB                                                           
         LA    R4,BMASK(R4)                                                     
         CLI   0(R4),X'FF'          AND ARE NOT REPEATED                        
         BE    ERROR3                                                           
         MVI   0(R4),X'FF'          SET MASK OF USED DIGITS                     
         LA    R2,1(R2)                                                         
         BCT   R3,VA1              NEXT DIGIT                                   
*                                                                               
*                                                                               
VA15     XC    DMASK,DMASK                                                      
         LA    R2,LIFDIED                                                       
         SR    R3,R3                                                            
         IC    R3,LIFDIEDH+5                                                    
         LTR   R3,R3                                                            
         BZ    VA3                                                              
*                                                                               
VA2      CLI   0(R2),C'0'          FOR EACH DIGIT IN DEATH ENTRY                
         BL    ERROR2                                                           
         CLI   0(R2),C'8'           VERIFY THEY ARE NUMERIC (0-8)               
         BH    ERROR2                                                           
         PACK  DUB,0(1,R2)                                                      
         CVB   R7,DUB                                                           
         LA    R4,DMASK(R7)                                                     
         CLI   0(R4),X'FF'          AND ARE NOT REPEATED                        
         BE    ERROR4                                                           
         MVI   0(R4),X'FF'          SET MASK OF USED DIGITS                     
         LA    R5,BMASK(R7)                                                     
         CLI   0(R5),X'FF'          VERIFY DIGIT NOT IN BOTH                    
         BE    ERROR6                   BIRTH AND DEATH                         
         LA    R2,1(R2)                                                         
         BCT   R3,VA2              NEXT DIGIT                                   
*                                                                               
*                                                                               
VA3      LA    R2,LIFGEN           INSURE THERE IS SOME INPUT                   
         SR    R3,R3                 IN THE # OF GENERATIONS FIELD              
         IC    R3,LIFGENH+5                                                     
         LTR   R3,R3                                                            
         BZ    ERROR5                                                           
*                                                                               
VA4      BCTR  R3,0                                                             
         MVC   GENS(8),=8C'0'      VERIFY GENERATION IS NUMERIC                 
         EX    R3,ZONES                                                         
         CLC   GENS(8),=8C'0'                                                   
         BNE   ERROR5                                                           
         EX    R3,GENPAK           PACK IT AND SAVE IT                          
         CP    GENS,=P'0'          IF ITS 0 THEN ERROR                          
         BE    ERROR5                                                           
*                                                                               
         LA    R4,0                ALL VERIFIED                                 
         B     XITVAL              EXIT NORMALLY                                
*                                                                               
*                                                                               
ERROR1   LA    R2,0                                                             
         OI    LIFBORNH+6,X'40'                                                 
         B     ALLX                                                             
         SPACE 2                                                                
ERROR2   LA    R2,1                                                             
         OI    LIFDIEDH+6,X'40'                                                 
         B     ALLX                                                             
         SPACE 2                                                                
ERROR3   LA    R2,2                                                             
         OI    LIFBORNH+6,X'40'                                                 
         B     ALLX                                                             
         SPACE 2                                                                
ERROR4   LA    R2,3                                                             
         OI    LIFDIEDH+6,X'40'                                                 
         B     ALLX                                                             
         SPACE 2                                                                
ERROR5   LA    R2,4                                                             
         OI    LIFGENH+6,X'40'                                                  
         B     ALLX                                                             
         SPACE 2                                                                
ERROR6   LA    R2,5                                                             
         OI    LIFBORNH+6,X'40'                                                 
         B     ALLX                                                             
         SPACE 2                                                                
ALLX     EQU   *                                                                
         LA    R4,1               ERROR, SO SET FLAG.                           
         MH    R2,=H'58'                                                        
         LA    R2,MESSAGES(R2)                                                  
         CLC   LIFHEAD(58),0(R2)                                                
         BE    XITVAL                                                           
         FOUT  LIFHEADH,0(R2),58                                                
         SPACE 2                                                                
XITVAL   XIT1  REGS=(R4)                                                        
**************************************************************                  
         EJECT                                                                  
**************************************************************                  
*           PARAMETER CONSTANTS                                                 
*************************************************************                   
*                                                                               
KPERSON  EQU   C'X'                THE CHARACTER REPRESENTING A PERSON          
KNOPERSN EQU   C' '                CHARACTER FOR NO PERSON                      
KLENGTH  EQU   15                  NUMBER OF ROWS IN POPULATION GRID            
KWIDTH   EQU   30                  NUMBER OF COLS IN POPULATION GRID            
KSIZE    EQU   KWIDTH*KLENGTH      TOTAL LOCATIONS IN POPULATION GRID           
**************************************************************                  
         SPACE 5                                                                
************MESSAGES TO OUTPUT LINE 1                                           
*                                                                               
MESSAGES DC    CL58'NOT A VALID BIRTH NUMBER'                                   
         DC    CL58'NOT A VALID DEATH NUMBER'                                   
         DC    CL58'DIGIT HAS BEEN REPEATED IN BIRTH'                           
         DC    CL58'DIGIT HAS BEEN REPEATED IN DEATH'                           
         DC    CL58'GENERATIONS MUST BE NUMERIC AND NON-ZERO'                   
         DC    CL58'DIGIT CANNOT APPEAR IN BOTH BIRTH AND DEATH'                
BLANKS   DC    CL58' '                                                          
         SPACE 5                                                                
*********** INSTRUCTIONS FOR EX *******************                             
***************************************************                             
ZONES    MVZ   GENS(0),LIFGEN                                                   
GENPAK   PACK  GENS,LIFGEN(0)                                                   
***************************************************                             
         EJECT                                                                  
LIFEWORK DSECT                                                                  
*************** SPACE ALLOTTED FOR THIS DSECT IN NMOD MACRO                     
*                                                                               
DUB      DS    D                   WORK                                         
GENS     DS    D                   HOLDS PACKED NUMBER OF GENERATIONS           
TABLE    DS    (KSIZE)CL1          WORKING REPRESENTATION OF                    
*                                      POPULATION GRID                          
BMASK    DS    CL9                 MASK OF NUMBER OF NEIGHBORS                  
*                                      CAUSING BIRTH                            
DMASK    DS    CL9                 MASK OF NUMBER OF NEIGHBORS                  
*                                      CAUSING BIRTH                            
WORKCHAR DS    CL1                                                              
NEXTLOC  DS    F                   OFFSET IN TABLE OF PREVIOS ROW               
PREVLOC  DS    F                   OFFSET IN TABLE OF NEXT ROW                  
WORK     DS    CL32                                                             
LIFEWRKX EQU   *                                                                
         SPACE 5                                                                
LIFED    DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE GALIFFFD                                                       
         SPACE 2                                                                
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062GALIF00   05/01/02'                                      
         END                                                                    
