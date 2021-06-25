*          DATA SET CPINQ04    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TC0304A                                                                  
         TITLE 'COST PER POINT INQUIRY PROGRAM - TREND PHASE'                   
TC0304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 4,**CPQT**                                                       
         LR    R7,RC                                                            
         USING LOCAL,R7            R7 = LOCAL W/S                               
         L     RC,0(R1)                                                         
         USING GWS,RC              RC = GLOBAL W/S                              
         USING TC03TWA,RA          RA = TWA                                     
         SPACE 1                                                                
         CLI   NEXT,0              CONTINUATION SCREEN REQUESTED                
         BNE   T20                                                              
         EJECT                                                                  
*                  GENERATE TABLE TO DRIVE MAIN PROCESSING LOOP                 
         SPACE 3                                                                
T0       MVI   NOHEDLNS,2          2 HEADLINES                                  
         MVC   HEADING1(6),=C'DPT-LN'                                           
         MVC   HEADING2(6),DASHES                                               
         ZIC   R1,RANGEMK          PERIOD SIZE IN MONTHS PER VALUE COL          
         LH    R3,ENDMNTHS         END DATE IN MONTHS                           
         SH    R3,STAMNTHS         WORK OUT NUMBER OF VALUE COLUMNS             
         LA    R3,1(R3)            IN R3                                        
         CLI   RANGEMK,1           MONTHLY ANALYSIS                             
         BE    T1                                                               
         SR    R2,R2                                                            
         DR    R2,R1                                                            
         LTR   R2,R2                                                            
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
T1       LH    RF,=H'72'           WORK OUT INTERVAL SIZE                       
         SR    RE,RE                                                            
         DR    RE,R3                                                            
         STH   RF,INTERVAL                                                      
         SPACE 2                                                                
         BCTR  R1,0                R1 = PERIOD SIZE -1 (ZERO IF MNTHLY)         
         LH    R3,STAMNTHS         R3 = DATE IN PROCESS                         
         SH    RF,=H'4'                                                         
         SRL   RF,1                                                             
         LA    R6,8(RF)            R6 = NEXT START COLUMN NUMBER                
         LA    R9,MAINTAB          R9 = TABLE POINTER                           
         USING MAINTABD,R9                                                      
         SPACE 1                                                                
T2       CH    R3,ENDMNTHS         LOOP TO CREATE A MAINTAB ENTRY AND           
         BH    T6                  HEADLINES FOR A VALUE COLUMN                 
         BAS   RE,DATECONV                                                      
         LA    R4,HEADING1-2(R6)                                                
         MVC   0(5,R4),MMMYY       FIRST HEADLINE                               
         STC   R6,MAINID           MAINTAB ENTRY - ID(USES COLUMN NO.)          
         MVC   MAINMIN(3),YYMM                   - MIN DATE * AS IN             
         MVC   MAINMAX(3),YYMM                   - MAX DATE * CPPERFD           
         STC   R6,MAINSCRC                       - START COLUMN NO.             
         MVI   MAINSCRL,5                        - MAX LENGTH OF VALUE          
         LA    R4,HEADING2-2(R6)                                                
         MVC   0(5,R4),DASHES      DEFAULT 2ND HEADLINE                         
         LTR   R1,R1                                                            
         BZ    T5                  BUMP TO NEXT IF MONTHLY                      
         AR    R3,R1               OTHERWISE WORK OUT MAX DATE FOR THIS         
         LR    R2,R1               VALUE COLUMN                                 
T3       CH    R3,ENDMNTHS                                                      
         BNH   T4                                                               
         BCTR  R3,0                                                             
         BCT   R2,T3                                                            
         MVC   0(5,R4),SPACES      LAST VALUE COLUMN IS FOR ONE MONTH           
         B     T5                                                               
T4       BAS   RE,DATECONV                                                      
         MVC   0(5,R4),MMMYY                                                    
         MVC   MAINMAX(3),YYMM                                                  
         SPACE 1                                                                
T5       LA    R3,1(R3)            BUMP MONTH                                   
         AH    R6,INTERVAL              NEXT START COLUMN NUMBER                
         LA    R9,L'MAINTAB(R9)         TABLE POINTER                           
         B     T2                                                               
         SPACE 1                                                                
T6       MVI   MAINID,X'FF'                                                     
         B     T7                                                               
         SPACE 3                                                                
*                  S/R TO CONVERT A DATE IN MONTHS TO TWO FORMATS               
*                  ON ENTRY R3    = DATE IN MONTHS IN BINARY                    
*                  ON EXIT  MMMYY = DATE IN EBCDIC                              
*                           YYMM  = DATE AS BINARY YEARS AND MONTHS             
*                                   SEPARATED BY X'0B',THE LENGTH OF            
*                                   THE CPFILE PERFORMANCE ELEMENT, FOR         
*                                   EASE OF COMPARISON AT MATRIX                
*                                   UPDATING STAGE                              
         SPACE 3                                                                
DATECONV NTR1                                                                   
         SR    R2,R2                                                            
         D     R2,=F'12'                                                        
         LR    R4,R2                                                            
         MH    R4,=H'3'                                                         
         LA    R4,MONTHTAB(R4)                                                  
         MVC   MMMYY(3),0(R4)      ALPHA MONTH                                  
         CVD   R3,DUB                                                           
         UNPK  MMMYY+3(2),DUB      DECIMAL YEAR                                 
         OI    MMMYY+4,X'F0'                                                    
         LA    R2,1(R2)                                                         
         STC   R2,YYMM+2           BINARY MONTH                                 
         MVI   YYMM+1,X'0B'                                                     
         STC   R3,YYMM             BINARY YEAR                                  
         XIT1                                                                   
         SPACE 1                                                                
MONTHTAB DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         EJECT                                                                  
*                  MAIN PROCESSING LOOP TO UPDATE MATRIX                        
         SPACE 3                                                                
T7       LA    R6,SCANBLK          R6 = A(MATRIX ENTRY BEING BUILT)             
         XC    SCANBLK(L'MATRIX),SCANBLK                                        
         USING MATRIXD,R6                                                       
         MVC   KEY,SAVEKEY                                                      
         SPACE 1                                                                
T8       GOTO1 AHIGH               READ A RECORD                                
         CLC   SAVEKEY(11),IO      CHECK KEY                                    
         BNE   T18                                                              
         LA    R4,IO                                                            
         USING CPKEYD,R4                                                        
         LA    R5,CPRECORD                                                      
         USING CPPERFD,R5                                                       
         GOTO1 AFILTER                                                          
         BZ    T17                 RECORD IS ELIMINATED BY FILTER               
         SR    R0,R0                                                            
         SPACE 1                                                                
T9       CLI   0(R5),0             LOOK FOR A PERFORMANCE ELEMENT               
         BE    T17                 WITHIN OVERALL DATE RANGE                    
         CLC   ENDDAT,CPPYEAR                                                   
         BL    T17                                                              
         CLC   STARTDAT,CPPYEAR                                                 
         BH    T16                                                              
         LA    R9,MAINTAB                                                       
         SPACE 1                                                                
T10      CLC   CPPYEAR(3),MAINMIN  FIND ITS MATCHING MAINTAB ENTRY              
         BL    T16                                                              
         CLC   CPPYEAR(3),MAINMAX                                               
         BH    T15                                                              
         SPACE 1                                                                
         CLC   CPKDAYPT(2),MATDAYPT DIES LAST MATRIX ENTRY MATCH ON             
         BNE   T11                 DAY-PART/LENGTH/ID                           
         CLC   MAINID,MATID                                                     
         BE    T14                                                              
         SPACE 1                                                                
T11      LA    R6,SCANBLK          IF NOT BUILD ONE IN SCANBLK WITH             
         MVI   FIRST,1             KEY AND ZERO VALUES                          
         MVC   MATDAYPT(2),CPKDAYPT                                             
         MVC   MATID,MAINID                                                     
         XC    MATVALA(8),MATVALA                                               
         GOTO1 ASEARCH             ADD IT TO, OR FIND IT IN, THE MATRIX         
         BZ    T18                 MATRIX FULL                                  
         L     R6,DMCB             R6 = A(NEW OR OLD MATRIX ENTRY)              
         SPACE 1                                                                
T14      L     RF,AMATRXSR         CALL MATRIX UPDATE SUBROUTINE FOR            
         GOTO1 (RF)                THIS DATA TYPE                               
         B     T16                                                              
         SPACE 1                                                                
T15      LA    R9,L'MAINTAB(R9)    MAINTAB BUMP                                 
         B     T10                                                              
         SPACE 1                                                                
T16      IC    R0,CPPLEN           ELEMENT BUMP                                 
         AR    R5,R0                                                            
         B     T9                                                               
         SPACE 3                                                                
T17      MVC   KEY,CPKEY           RECORD BUMP (ADD 1 TO LAST WORD OF           
         L     R1,KEY+12           KEY)                                         
         AH    R1,=H'1'                                                         
         ST    R1,KEY+12                                                        
         B     T8                                                               
         SPACE 3                                                                
T18      LA    R3,L'MATRIX         TERMINATE THE MATRIX                         
         M     R2,RECNUM                                                        
         LA    R3,MATRIX(R3)                                                    
         MVI   0(R3),X'FF'                                                      
         ST    R3,AENDMTRX                                                      
         B     T19                                                              
         EJECT                                                                  
*                  SET UP DISPLAY SCREEN FROM MATRIX                            
         SPACE 3                                                                
T19      CLI   FIRST,0                                                          
         BNE   T25                                                              
         MVI   FERN,MINE                                                        
         MVC   CPQHEAD(L'NODSPLAY),NODSPLAY                                     
         B     TEND                                                             
NODSPLAY DC    CL50'NOTHING TO DISPLAY - ENTER FIELDS FOR NEXT INQUIRY'         
         SPACE 3                                                                
T20      GOTO1 ARDIR               IF CONTINUATION SCREEN, RESTORE              
         L     R1,ATIA             SAVED MATRIX VIA TIA                         
         USING TC03SAVX,R1                                                      
         LA    R2,SAVEMTRX                                                      
         LA    R4,L'MATRIX                                                      
         LH    R5,SAVEMXLN                                                      
         LA    R5,0(R2,R5)                                                      
         SR    R5,R4                                                            
         LA    R6,MATRIX-L'MATRIX                                               
         SPACE 1                                                                
T21      LA    R6,L'MATRIX(R6)                                                  
         MVC   0(L'MATRIX,R6),0(R2)                                             
         BXLE  R2,R4,T21                                                        
         ST    R6,AENDMTRX                                                      
         MVC   MAINTAB(241),SAVETAB                                             
         MVI   NEXT,0                                                           
         SPACE 3                                                                
T25      GOTO1 AFORMAT             SET UP DISPLAY                               
TEND     XMOD1 1                                                                
         EJECT                                                                  
*                  CONSTANTS,LITERALS AND LOCAL WORKING STORAGE DSECT           
         SPACE 3                                                                
DASHES   DC    6CL1'-'                                                          
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
LOCAL    DSECT                                                                  
INTERVAL DS    H         X         NO OF POSITIONS BETWEEN VALUE COLS.          
MMMYY    DS    CL5       C         MONTH AND YEAR IN EBCDIC                     
YYMM     DS    CL3       X         YEAR/CPPLEN/MONTH IN BINARY                  
         SPACE 3                                                                
         EJECT                                                                  
*                  NESTED INCLUDES FOR CPINQDSECT & CPGENFILE                   
         PRINT OFF                                                              
       ++INCLUDE CPINQDSECT                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CPINQ04   05/01/02'                                      
         END                                                                    
