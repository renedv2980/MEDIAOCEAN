*          DATA SET SPINF46    AT LEVEL 012 AS OF 03/09/05                      
*PHASE T21A46A                                                                  
*                                                                               
* THIS IS REALLY LEVEL 010. I THOUGHT THERE WAS A BUG, BUT IT'S NOT             
* WORTH FIXING !                                                                
*                                                                               
         TITLE 'SPINFO46 - STATION REC INFO'                                    
T21A46   CSECT                                                                  
         PRINT NOGEN                                                            
LINLEN   EQU   88                                                               
         NMOD1 70,T21A46                                                        
         LR    R9,RC                                                            
         USING DATATBL,R9                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING STARECD,R8                                                       
         MVC   0(256,R9),DATAVALS                                               
         MVC   256(206,R9),DATAVALS+256                                         
         SPACE                                                                  
* FILTER ROUTINE *                                                              
         SPACE                                                                  
         MVI   WRKBT,C'0'          INIT TO NO FILTERS                           
         MVI   WRKBT1,C'0'                                                      
         MVI   WRKBT2,C'0'                                                      
         MVI   WRKBT3,C'0'                                                      
         MVI   WRKBT4,C'0'                                                      
FLTRTN00 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'PAY=')                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT,C'0'          NO PAYREP FILTER                             
         B     FLTRTN05                                                         
         MVI   WRKBT,C'1'          YES - SET WRKBT TO 1                         
         L     R7,4(R1)                                                         
         MVC   WORK(3),4(R7)      MOVE INPUT TO WORK                            
         MVC   WRKBTA,4(R7)       PAYREP CODE TO WRKBTA                         
         B     FLTRTN40                                                         
FLTRTN05 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'CLI=')                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT4,C'0'                                                      
         B     FLTRTN10                                                         
         MVI   WRKBT4,C'1'                                                      
         L     R7,4(R1)                                                         
         MVC   WRKBT4A,4(R7)                                                    
         B     FLTRTN40                                                         
FLTRTN10 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'TIM=')                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT1,C'0'         NO TIME SHEET(CONTRACT) REP                  
         B     FLTRTN20                                                         
         MVI   WRKBT1,C'1'         YES - SET WRKBT1 TO 1                        
         L     R7,4(R1)                                                         
         MVC   WORK(3),4(R7)      MOVE INPUT TO WORK                            
         BAS   RE,NUMTEST                                                       
         MVC   WRKBT1A,4(R7)      TRAFFIC REP CODE TO WRKBT1A                   
         B     FLTRTN40                                                         
FLTRTN20 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'AFF=')                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT2,C'0'         NO NTWRK AFF FLTR                            
         B     FLTRTN30                                                         
         MVI   WRKBT2,C'1'         YES, SET WRKBT2 ON                           
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
         L     R7,4(R1)                                                         
         MVC   WORK(3),4(R7)      INPUT TO WORK                                 
         CLC   WORK(3),=X'C1C1C1'  ALPHPA/NUMERIC TEST                          
         BL    ERRORTN                                                          
         CLC   WORK(3),=X'F9F9F9'                                               
         BH    ERRORTN                                                          
TEST1    CLI   WORK,X'CA'                                                       
         BL    TEST2                                                            
         CLI   WORK,X'D1'                                                       
         BL    ERRORTN                                                          
         CLI   WORK,X'DA'                                                       
         BL    TEST2                                                            
         CLI   WORK,X'E2'                                                       
         BL    ERRORTN                                                          
         CLI   WORK,X'EA'                                                       
         BL    TEST2                                                            
         CLI   WORK,X'F0'                                                       
         BL    ERRORTN                                                          
TEST2    CLI   WORK+1,X'CA'                                                     
         BL    TEST3                                                            
         CLI   WORK+1,X'D1'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+1,X'DA'                                                     
         BL    TEST3                                                            
         CLI   WORK+1,X'E2'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+1,X'EA'                                                     
         BL    TEST3                                                            
         CLI   WORK+1,X'F0'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+1,X'F9'                                                     
         BH    ERRORTN                                                          
TEST3    CLI   WORK+2,X'CA'                                                     
         BL    FLTRTN25                                                         
         CLI   WORK+2,X'D1'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+2,X'DA'                                                     
         BL    FLTRTN25                                                         
         CLI   WORK+2,X'E2'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+2,X'EA'                                                     
         BL    FLTRTN25                                                         
         CLI   WORK+2,X'F0'                                                     
         BL    ERRORTN                                                          
         CLI   WORK+2,X'F9'                                                     
         BH    ERRORTN                                                          
FLTRTN25 MVC   WRKBT2A,4(R7)      NTWRK AFF CODE TO WRKBT2A                     
         OC    WRKBT2A,SPACES                                                   
         B     FLTRTN40                                                         
         EJECT                                                                  
FLTRTN30 GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'TYPE=')                            
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT3,C'0'         NO STATION TYPE FLTR                         
         B     FLTRTN37                                                         
         MVI   WRKBT3,C'1'         YES, SET WRKBT3 TO 1                         
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
         L     R7,4(R1)                                                         
         MVC   WORK(1),5(R7)      MOVE INPUT TO WORK                            
         CLI   WORK,X'C1'          ALPHA /NUMERIC TEST                          
         BL    ERRORTN                                                          
         CLI   WORK,X'CA'                                                       
         BL    FLTRTN35                                                         
         CLI   WORK,X'D1'                                                       
         BL    ERRORTN                                                          
         CLI   WORK,X'DA'                                                       
         BL    FLTRTN35                                                         
         CLI   WORK,X'E2'                                                       
         BL    ERRORTN                                                          
         CLI   WORK,X'EA'                                                       
         BL    FLTRTN35                                                         
         CLI   WORK,X'F0'                                                       
         BL    ERRORTN                                                          
         CLI   WORK,X'F9'                                                       
         BH    ERRORTN                                                          
FLTRTN35 MVC   WRKBT3A,5(R7)      MOVE STATION TYPE CODE TO WRKBT3A             
         B     FLTRTN40                                                         
FLTRTN37 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'MKT=')                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   WRKBT5,C'0'         NO MKT FILTER                                
         B     FLTRTN38                                                         
         MVI   WRKBT5,C'1'         YES, MKT FILTER                              
         L     R7,4(R1)                                                         
         MVC   WORK(4),4(R7)       CHECK INPUT LENGTH                           
         CLI   WORK+3,X'00'                                                     
         BE    FLT37A                                                           
         CLI   WORK+3,C' '                                                      
         BE    FLT37A                                                           
         MVC   WRKBT5A,WORK                                                     
         B     FLTRTN40                                                         
FLT37A   CLI   WORK+2,X'00'                                                     
         BE    FLT37B                                                           
         CLI   WORK+2,C' '                                                      
         BE    FLT37B                                                           
         MVI   WRKBT5A,C'0'                                                     
         MVC   WRKBT5A+1(3),WORK                                                
         B     FLTRTN40                                                         
FLT37B   CLI   WORK+1,X'00'                                                     
         BE    FLT37C                                                           
         CLI   WORK+1,C' '                                                      
         BE    FLT37C                                                           
         MVC   WRKBT5A(2),=2C'0'                                                
         MVC   WRKBT5A+2(2),WORK                                                
         B     FLTRTN40                                                         
FLT37C   MVC   WRKBT5A(3),=3C'0'                                                
         MVC   WRKBT5A+3(1),WORK                                                
         B     FLTRTN40                                                         
FLTRTN38 B     DATA00             NO BYTES ON, GOTO DATA ROUTINE                
         SPACE                                                                  
FLTRTN40 LA    R2,SINHDRH                                                       
         MVC   8(23,R2),=C'STATION/CLIENT/MKT-NAME'                             
         MVC   48(23,R2),=C'STATION/CLIENT/MKT-NAME'                            
         BAS   RE,DASHRTN                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES.                                         
         MVC   KEY,PREVKEY         NO.                                          
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA            READ RECORD                                  
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FLTRTN60                                                         
FLTRTN50 CLI   WRKBT,C'1'          IS PAYREP FILTER ON                          
         BNE   FLT5A               NO. SKIP PAYREP MATCH                        
         CLC   SPAYREP,WRKBTA      PAYREP MATCH                                 
         BNE   FLTRTN60                                                         
FLT5A    CLI   WRKBT1,C'1'         IS TIME SHEET(CONTRACT) FILTER ON            
         BNE   FLT5B               NO. SKIP TIME REP MATCH                      
         CLC   SCONREP,WRKBT1A     TIME SHEET REP MATCH                         
         BNE   FLTRTN60                                                         
FLT5B    CLI   WRKBT2,C'1'         IS NTWRK AFF FLTR ON                         
         BNE   FLT5C               NO.SKIP NTWRK AFF TEST                       
         CLC   SNETWRK,WRKBT2A     NTWRK AFF MATCH                              
         BNE   FLTRTN60                                                         
FLT5C    CLI   WRKBT3,C'1'         IS STATION TYPE FLTR ON                      
         BNE   FLT5C1              NO. SKIP STATION TYPE TEST                   
         CLC   STYPE,WRKBT3A       STATION TYPE MATCH                           
         BNE   FLTRTN60                                                         
FLT5C1   CLI   WRKBT4,C'1'         IS CLIENT EXCEPTION TYPE FLTR ON             
         BNE   FLT5C1A             NO. SKIP CLIENT EXCEP TYPE TEST.             
         CLI   WRKBT4A+2,X'00'     CHK CLI EXCEP INPUT AND PAD -                
         BNE   FLTNXT              WITH BLANKS WHERE                            
         MVI   WRKBT4A+2,C' '      NECESSARY                                    
FLTNXT   CLI   WRKBT4A+1,X'00'                                                  
         BNE   FLTNXT1                                                          
         MVI   WRKBT4A+1,C' '                                                   
FLTNXT1  CLC   WRKBT4A,STAKCLT     CLIENT EXCEP TYPE TEST                       
         BNE   FLTRTN60                                                         
FLT5C1A  CLI   WRKBT5,C'1'         IS MKT FILTER TYPE ON                        
         BNE   FLT5D               NO. SKIP TEST                                
         CLC   WRKBT5A,SMKT        YES.                                         
         BNE   FLTRTN60                                                         
FLT5D    MVC   8(5,R2),STAKCALL   MOVE STATION NAME TO SCREEN                   
         CLC   STAKCLT,=3C'0'     IF ZEROS IN                                   
         BNE   FLT5E              CLIENT CODE                                   
         MVC   15(3,R2),SPACES    MOVE SPACES.                                  
         B     FLT5F                                                            
FLT5E    MVC   15(3,R2),STAKCLT   MOVE CLIENT TO SCREEN                         
FLT5F    BAS   RF,MKT00            GET MARKET NAME                              
         MVC   20(24,R2),MKTNAME  MOVE MKT NAME TO SCREEN                       
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FLTRTN60 BAS   RE,SEQSTA           READ RECORD                                  
         TM    8(R1),X'90'                                                      
         BNZ   EXIT                                                             
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FLTRTN60                                                         
         CLI   0(R2),LINLEN        END OF COLUMN                                
         BE    FLTRTN50            NO.                                          
         LA    R2,SINHDRH          YES. SO RESET R2 -                           
         ZIC   R0,0(R2)            TO POINT TO NEXT COLUMN                      
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            ZIC AGAIN FOR UNDERLINING                    
         AR    R2,R0                                                            
*                                  2ND COLUMN                                   
FLTRTN70 CLI   WRKBT,C'1'          IS PAYREP FILTER ON                          
         BNE   FLT7A               NO. SKIP PAYREP MATCH                        
         CLC   SPAYREP,WRKBTA      PAYREP MATCH                                 
         BNE   FLTRTN80                                                         
FLT7A    CLI   WRKBT1,C'1'         IS TRAFFIC REP FLTR ON                       
         BNE   FLT7B               NO. SKIP TRAFF REP MATCH                     
         CLC   STRFREP,WRKBT1A     TRAFFIC REP MATCH                            
         BNE   FLTRTN80                                                         
FLT7B    CLI   WRKBT2,C'1'         IS NTWRK AFF FLTR ON                         
         BNE   FLT7C               NO.SKIP NTWRK AFF TEST                       
         CLC   SNETWRK,WRKBT2A     NTWRK AFF MATCH                              
         BNE   FLTRTN80                                                         
FLT7C    CLI   WRKBT3,C'1'         IS STATION TYPE FLTR ON                      
         BNE   FLT7C1              NO. SKIP STATION TYPE TEST                   
         CLC   STYPE,WRKBT3A       STATION TYPE MATCH                           
         BNE   FLTRTN80                                                         
FLT7C1   CLI   WRKBT4,C'1'         CLIENT CODE TEST                             
         BNE   FLT7C1A                                                          
         CLI   WRKBT4A+2,X'00'                                                  
         BNE   F7X                                                              
         MVI   WRKBT4A+2,C' '                                                   
F7X      CLI   WRKBT4A+1,X'00'                                                  
         BNE   F7X1                                                             
         MVI   WRKBT4A+1,C' '                                                   
F7X1     CLC   WRKBT4A,STAKCLT                                                  
         BNE   FLTRTN80                                                         
FLT7C1A  CLI   WRKBT5,C'1'                                                      
         BNE   FLT7D                                                            
         CLC   WRKBT5A,SMKT                                                     
         BNE   FLTRTN80                                                         
FLT7D    MVC   49(5,R2),STAKCALL   MOVE STATION NAME TO SCREEN                  
         CLC   STAKCLT,=3C'0'     IF ZEROS IN                                   
         BNE   FLT7E              CLIENT CODE                                   
         MVC   56(3,R2),SPACES    MOVE SPACES.                                  
         B     FLT7F                                                            
FLT7E    MVC   56(3,R2),STAKCLT   MOVE CLIENT TO SCREEN                         
FLT7F    BAS   RF,MKT00            GET MARKET NAME                              
         MVC   61(24,R2),MKTNAME   MOVE MKT NAME TO SCREEN                      
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FLTRTN80 BAS   RE,SEQSTA                                                        
         TM    8(R1),X'90'                                                      
         BNZ   EXIT                                                             
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FLTRTN80                                                         
         CLI   0(R2),LINLEN        END OF SCREEN                                
         BE    FLTRTN70            NO.                                          
         B     PAGEPROC                                                         
         EJECT                                                                  
*  'DATA= ' ROUTINES   *                                                        
         SPACE                                                                  
DATA00   GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'DATA=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    FMT00               NO DATA REQUESTED, GOTO FORMAT               
*                                                                               
* LOAD DATATBL                                                                  
* L' OF DSECT ITEMS, A OF DSECT ITEMS, DISP FOR DSECT ITEMS *                   
*                                                                               
         LA    R4,DATATBL                                                       
         LA    R7,SMKT             ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'04'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'03'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         SET TEST-BYTE TO ZERO.                       
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,SPAYREP          ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'03'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'03'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,SCONREP          ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'03'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'04'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,STRFREP          ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'03'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'04'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,STWIX            ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'14'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'00'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
******** LA    R7,STELE            ADDRS OF-                                    
******** STCM  R7,15,6(R4)         -DSECT ITEM.                                 
******** MVI   5(R4),X'0A'         L' OF DSECT ITEM.                            
******** MVI   3(R4),X'02'         DISP OF DSECT ITEM.                          
******** MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
*                                                                               
         LA    R7,SFAX             ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'0C'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'02'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         SR    R7,R7                                                            
         ICM   R7,3,SNEWTAX                                                     
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'02'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'03'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,SSIZE            ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'01'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'02'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,SNETWRK          ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'03'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'03'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,STYPE            ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'01'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'01'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,SCHNL            ADDRS OF-                                    
         STCM  R7,15,6(R4)         -DSECT ITEM.                                 
         MVI   5(R4),X'04'         L' OF DSECT ITEM.                            
         MVI   3(R4),X'02'         DISP OF DSECT ITEM.                          
         MVI   4(R4),X'00'         TEST-BYTE TO ZERO.                           
         EJECT                                                                  
* TEST FOR 1ST DATANAME OF DATATBL *                                            
*                                                                               
         LA    R4,DATATBL                                                       
         LA    R2,SINHDRH                                                       
         MVC   8(23,R2),=C'STATION/CLIENT/MKT NAME'                             
         LR    RF,R2                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVC   8(23,RF),=23C'-'                                                 
         L     R3,4(R1)           R3 TO 'DATA=' OF USER1                        
         LA    R3,5(R3)           R3 TO AFTER 'DATA='                           
         LA    R6,48              DISP OF 1ST HEADER TO R6                      
DATA10   ZIC   R5,1(R4)           R5 HAS LENGTH OF TEST DATANAME                
         GOTO1 USER1,DMCB,(64,(R3)),((R5),10(R4))                               
         C     R3,4(R1)            USER1 HAS STAYED AT SAME ADDRS               
         BE    DATAMTCH            SO THERE IS A MATCH.                         
         CLI   0(R4),X'FF'                                                      
         BE    INPUTERR                                                         
         ZIC   R0,0(R4)            INCREMENT DATATBL                            
         AR    R4,R0                   "        "                               
         B     DATA10                  "        "                               
DATAMTCH ZIC   R7,2(R4)            L' HEADER  +                                 
         AR    R7,R6               DISP OF HEADER TO R7.                        
         CH    R7,=H'88'           TEST FOR HEADER ROOM.                        
         BH    DATA20              NO MORE ROOM.                                
         MVI   4(R4),C'1'          YES, ROOM. TEST BYTE ON.                     
         ZIC   R7,3(R4)            ITEM DISP TO R7 +                            
         AR    R7,R6               DISP OF HEADER AND                           
         STC   R7,3(R4)            STORE ITEM SCREENDISP.                       
         LA    RE,0(R6,R2)         ADDRS+DISP TO RE                             
         ZIC   R7,2(R4)            L' HEADER TO R7                              
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),30(R4)      HEADER TO SCREEN                             
         LR    RE,R2               UNDERLINING ROUTINE                          
         ZIC   R0,0(RE)                "          "                             
         AR    RE,R0                   "          "                             
         LA    RF,0(R6,RE)             "          "                             
         BCTR  R7,0                REDUCE L' DASH BY 2                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),=26C'-'                                                  
         LA    R7,3(R7)            RESET L' DSECT ITEM AND                      
         AR    R6,R7               ADD IT TO NEXT HEADERDISP.                   
         A     R5,=F'1'            INCREASE R5 BY 1 FOR COMMA                   
         AR    R3,R5               AND ADD TO R3(USER1 POINTER).                
         CLI   0(R3),C' '                                                       
         BE    DATA20                                                           
         CLI   0(R3),X'00'                                                      
         BE    DATA20                                                           
         LA    R4,DATATBL                                                       
         B     DATA10                                                           
DATA20   FOUT  (R2)                FOUT HEADER                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2)                FOUT UNDERLINING                             
         SPACE                                                                  
* GET RECORD, MATCH WITH DATA REQUESTS, PROCESS *                               
         SPACE                                                                  
PROC00   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY        FIRST TIME THROUGH                           
         OC    PREVKEY,PREVKEY     YES                                          
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   PROC50                                                           
PROC10   LA    R4,DATATBL                                                       
PROC20   CLI   4(R4),C'1'          IS TEST BYTE ON                              
         BE    PROC30              YES.                                         
         ZIC   R0,0(R4)            NO. SO INCREMENT DATATBL.                    
         AR    R4,R0                                                            
         CLI   0(R4),X'FF'         END OF DATATBL                               
         BNE   PROC20              NO                                           
         B     PROC40              YES. GO AND PROCESS ITEM.                    
PROC30   CLC   10(3,R4),=C'AFF'    IS IT A SPECIAL CASE                         
         BE    PROC60              YES. GOTO EDITING ROUTINES.                  
         CLC   10(3,R4),=C'MKT'                                                 
         BE    PROC70                                                           
         CLC   10(3,R4),=C'TAX'                                                 
         BE    PROC72                                                           
         CLC   10(3,R4),=C'PAY'                                                 
         BE    PROC74                                                           
         CLC   10(3,R4),=C'CON'                                                 
         BE    PROC76                                                           
         CLC   10(3,R4),=C'TRA'                                                 
         BE    PROC78                                                           
         CLC   10(3,R4),=C'CHA'                                                 
         BE    PROC80                                                           
PROC33   ZIC   RE,3(R4)            SCREEN DISP TO RE                            
         LA    R7,0(RE,R2)         R2(SCRN ADRS)+RE(DISP) TO R7                 
         L     RE,6(R4)            ADDRS OF DSECT ITEM TO RE                    
         ZIC   RF,5(R4)            LENGTH OF DSECT ITEM TO RF                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(RE)       DSECT ITEM TO SCREEN                         
PROC35   ZIC   R0,0(R4)                                                         
         AR    R4,R0               INCREMENT DATATBL                            
         CLI   0(R4),X'FF'         END OF DATATBL                               
         BNE   PROC20              NO                                           
PROC40   MVC   8(5,R2),STAKCALL                                                 
         CLC   STAKCLT,=3C'0'      IF CLIENT CODE ZEROS,                        
         BNE   PROC45              CHANGE TO                                    
         MVC   15(3,R2),SPACES     SPACES.                                      
         B     PROC47                                                           
PROC45   MVC   15(3,R2),STAKCLT                                                 
PROC47   BAS   RF,MKT00                                                         
         MVC   20(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
PROC50   BAS   RE,SEQSTA                                                        
         TM    8(R1),X'90'                                                      
         BNZ   EXIT                                                             
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   PROC50                                                           
         CLI   0(R2),LINLEN        END OF SCREEN                                
         BNE   PAGEPROC            YES.                                         
         B     PROC10                                                           
         EJECT                                                                  
* EDITING PROCS *                                                               
*                                                                               
PROC60   CLC   SCANNTWK,SPACES     IS IT A CANADIAN NTWK                        
         BE    PROC33              NO                                           
         CLC   SCANNTWK,=4X'00'                                                 
         BE    PROC33              NO                                           
         ZIC   RE,3(R4)            YES. GET SCREEN DISP OF AFF                  
         LA    R7,0(RE,R2)         DISP AND SCREEN ADRS TO R7                   
         MVC   0(4,R7),SCANNTWK    MOVE CANADIAN AFF TO SCREEN                  
         B     PROC35              RETURN TO NORMAL PROCESSING                  
PROC70   EDIT  (C4,SMKT),(4,MYWRK)                                              
         ZIC   RE,3(R4)                                                         
         LA    R7,0(RE,R2)                                                      
         MVC   0(4,R7),MYWRK                                                    
         B     PROC35                                                           
PROC72   SR    RF,RF                                                            
         ICM   RF,3,SNEWTAX                                                     
         EDIT  (RF),(6,MYWRK),3                                                 
         ZIC   RE,3(R4)                                                         
         LA    R7,0(RE,R2)                                                      
         MVC   0(6,R7),MYWRK       EDITED TAX RATE TO SCREEN                    
         B     PROC35                                                           
PROC74   EDIT  (C3,SPAYREP),(3,SPAYREP)                                         
         B     PROC33                                                           
PROC76   EDIT  (C3,SCONREP),(3,SCONREP)                                         
         B     PROC33                                                           
PROC78   EDIT  (C3,STRFREP),(3,STRFREP)                                         
         B     PROC33                                                           
PROC80   CLI   STAKMED,C'R'         IS IT RADIO                                 
         BE    PROC82                                                           
         EDIT  (C2,SCHNL),(2,SCHNL)  TV EDIT                                    
         B     PROC33                                                           
*                                                                               
PROC82   DS    0H                                                               
* STUPID EDIT CALL.  ALREADY IN EBCIDIC AND MAY HAVE DECIMAL PLACE              
*        EDIT  (C4,SCHNL),(4,SCHNL)  RADIO EDIT                                 
         B     PROC33                                                           
         EJECT                                                                  
* DEFAULT ROUTINES *                                                            
         SPACE                                                                  
FMT00    CLC   SINIFLT,SPACES     TEST FOR GARBAGE IN INPUT AREA                
         BE    FMT05                                                            
         CLC   SINIFLT,=64X'00'                                                 
         BE    FMT05                                                            
         B     INPUTERR                                                         
FMT05    LA    R2,SINHDRH                                                       
         MVC   8(23,R2),=C'STATION/CLIENT/MKT NAME'                             
         MVC   48(23,R2),=C'STATION/CLIENT/MKT NAME'                            
         BAS   RE,DASHRTN                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES                                          
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FMT20                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FMT10    MVC   8(5,R2),STAKCALL                                                 
         CLC   STAKCLT,=3C'0'      IF CLIENT CODE ZEROS,                        
         BNE   FMT15               MOVE                                         
         MVC   15(3,R2),SPACES     SPACES.                                      
         B     FMT17                                                            
FMT15    MVC   15(3,R2),STAKCLT                                                 
FMT17    BAS   RF,MKT00            GET MARKET NAME                              
         MVC   20(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
FMT20    BAS   RE,SEQSTA                                                        
         TM    8(R1),X'90'                                                      
         BNZ   EXIT                                                             
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FMT20                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN        END OF COLUMN                                
         BE    FMT10               NO                                           
         LA    R2,SINHDRH          YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               EXTRA ZIC FOR UNDERLINING                    
FMT30    MVC   49(5,R2),STAKCALL                                                
         CLC   STAKCLT,=3C'0'      IF CLIENT CODE ZEROS,                        
         BNE   FMT35               MOVE                                         
         MVC   56(3,R2),SPACES     SPACES.                                      
         B     FMT37                                                            
FMT35    MVC   56(3,R2),STAKCLT                                                 
FMT37    BAS   RF,MKT00                                                         
         MVC   61(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
FMT40    BAS   RE,SEQSTA                                                        
         TM    8(R1),X'90'                                                      
         BNZ   EXIT                                                             
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   STAKAGY,AGYALPHA                                                 
         BNE   FMT40                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN        END OF SCREEN                                
         BE    FMT30               NO                                           
         B     PAGEPROC            YES.                                         
         EJECT                                                                  
* VARIOUS BRANCHED-TO ROUTINES *                                                
*                                                                               
NUMTEST  LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
         MVC   WORK+3(3),WORK      STORE INPUT IN WORK+3                        
         MVN   WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'  TEST ZONES FOR F                             
         BNE   ERRORTN                                                          
         MVN   WORK(3),WORK+3      WORK HAS F0F0F0/W+3 HAS INPUT NUMB           
         CLC   WORK(3),=X'F0F0F0'                                               
         BL    ERRORTN                                                          
         CLC   WORK(3),=X'F9F9F9'                                               
         BH    ERRORTN                                                          
         BR    RE                                                               
*                                                                               
DASHRTN  LR    RF,R2                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVC   8(23,RF),=23C'-'                                                 
         MVC   48(23,RF),=23C'-'                                                
         FOUT  (R2)              FOUT HEADER                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2)              FOUT UNDERLINING                               
         BR    RE                                                               
*                                                                               
MKT00    MVC   WORK(117),REC     SAVE STA REC                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'          BUILD KEY                                      
         MVC   KEY+1(1),STAKMED      "                                          
         MVC   KEY+2(4),SMKT         "                                          
         MVC   KEY+6(2),STAKAGY      "                                          
         BAS   RE,HISTA          GET RECORD                                     
         CLC   REC+2(4),WORK+18  MKT CODES THE SAME                             
         BNE   MKT10               NO                                           
         MVC   MKTNAME(24),REC+18  YES                                          
         B     MKT20                                                            
MKT10    MVC   MKTNAME(24),=C'   * * * UNKNOWN * * *  '                         
MKT20    XC    KEY,KEY                                                          
         MVC   KEY(17),WORK                                                     
         BAS   RE,HISTA           TO RESTORE STA SEQ READ                       
         BR    RF                                                               
*                                                                               
HISTA    NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATION ',KEY,AREC                   
         B     EXIT10                                                           
*                                                                               
SEQSTA   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'STATION ',KEY,AREC                   
         B     EXIT10                                                           
*                                                                               
INPUTERR LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
*                                                                               
ERRORTN  GOTO1 ERROR                                                            
*                                                                               
PAGEPROC MVC   PREVKEY,0(8)                                                     
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         B     EXIT10                                                           
*                                                                               
EXIT     XC    PREVKEY,PREVKEY                                                  
         LA    R2,SINIKEYH                                                      
         OI    6(R2),X'C0'                                                      
EXIT10   XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                * DATATBL RECORDS  *                           
DATAVALS DS    0CL462                                                           
         DC    X'2A'             00   LENGTH OF FIELD                           
         DC    X'03'             01   LENGTH OF TEST DATANAME                   
         DC    X'0C'             02   LENGTH OF HEADER                          
         DS    CL1               03   DSECT ITEM DISP LENGTH                    
         DS    CL1               04   TEST-BYTE                                 
         DS    CL1               05   LENGTH OF DSECT ITEM                      
         DS    CL4               06   ADDRESS OF DSECT ITEM                     
         DC    CL20'MKT'         10   TEST DATANAME                             
         DC    C'MKT NUMBER  '   30   HEADER                                    
         DC    X'2A',X'03',X'0C'                                                
         DS    CL7                                                              
         DC    CL20'PAY'                                                        
         DC    C'PAYING REP  '                                                  
         DC    X'2C',X'03',X'0E'                                                
         DS    CL7                                                              
         DC    CL20'TIM'                                                        
         DC    C'TIMESHEET REP '                                                
         DC    X'2B',X'03',X'0D'                                                
         DS    CL7                                                              
         DC    CL20'TRA'                                                        
         DC    C'TRAFFIC REP  '                                                 
         DC    X'34',X'03',X'16'                                                
         DS    CL7                                                              
         DC    CL20'TWX'                                                        
         DC    C'     TWX NUMBER       '                                        
         DC    X'2F',X'03',X'11'                                                
         DS    CL7                                                              
         DC    CL20'FAX'                                                        
         DC    C'  FAX NUMBER     '                                             
         DC    X'28',X'03',X'0C'                                                
         DS    CL7                                                              
         DC    CL20'TAX'                                                        
         DC    C'TAX RATE  '                                                    
         DC    X'24',X'04',X'06'                                                
         DS    CL7                                                              
         DC    CL20'SIZE'                                                       
         DC    C'SIZE  '                                                        
         DC    X'28',X'03',X'0A'                                                
         DS    CL7                                                              
         DC    CL20'AFF'                                                        
         DC    C'NTWK AFF  '                                                    
         DC    X'24',X'04',X'06'                                                
         DS    CL7                                                              
         DC    CL20'TYPE'                                                       
         DC    C'TYPE  '                                                        
         DC    X'27',X'03',X'09'                                                
         DS    CL7                                                              
         DC    CL20'CHA'                                                        
         DC    C'CHANNEL  '                                                     
ENDTBL   DC    X'FF'                                                            
*                                                                               
DATATBL  DSECT                                                                  
         DS    CL462            DATAVALS                                        
WRKBT    DC    C'0'             PAYREP                                          
WRKBTA   DS    CL3                                                              
WRKBT1   DC    C'0'             TRAFFIC REP                                     
WRKBT1A  DS    CL3                                                              
WRKBT2   DC    C'0'             NTWRK AFF                                       
WRKBT2A  DS    CL3                                                              
WRKBT3   DC    C'0'             STATION TYPE                                    
WRKBT3A  DS    CL1                                                              
WRKBT4   DC    C'0'             CLIENT EXCEPTION CODE                           
WRKBT4A  DS    CL3                                                              
WRKBT5   DS    CL1              MKT CODE                                        
WRKBT5A  DS    CL4                                                              
MKTNAME  DS    CL24                                                             
MYWRK    DS    CL5                                                              
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPINF46   03/09/05'                                      
         END                                                                    
