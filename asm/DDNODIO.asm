*          DATA SET DDNODIO    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T00A70A                                                                  
         TITLE 'NODIO- NODAL FILE IO HANDLER'                                   
*                                                                               
*        PARAMETER LIST                                                         
*                                                                               
*                                                                               
*  PARAM 1  BYTE 0-3   A(NODBLKD)      CONTROL BLOCK                            
*  PARAM 2  BYTE 0     A=AFTER,B=BEFORE  *FOR MOVE,ADD,COPY,REPO                
*           BYTE 1-3   A(COMMAND)                                               
*  PARAM 3  BYTE 0-3   A(INPUT)                                                 
*  PARAM 4  BYTE 0     LENGTH OF POSITION OR RENAME CODE                        
*           BYTE 1-3   A(POSITION CODE)  *FOR MOVE,ADD,COPY OR REPO             
*                      CODE X'00'=ADD AT START, X'FF'=ADD AT END                
*                      IF PARAM4 IS NULL, ADD AT END                            
*           BYTE 1-3   A(RENAME CODE) FOR RENAMES                               
*  PARAM 5  BYTE 1-3   A(NEW PARENT 'KEY') FOR REPOS, OR                        
*                      A(NEW 'KEY') FOR COPY                                    
         PRINT NOGEN                                                            
NODIO    CSECT                                                                  
         NMOD1 NDWKL,**NODIO,R7,R8,RR=R3   **THREE BASE REGISTERS               
         SPACE 2                                                                
*                                                                               
         USING NDWKD,RC                                                         
         L     RA,0(R1)                                                         
         USING NODBLKD,RA                                                       
         USING NDLVTABD,R9         R9 USED THROUGHOUT FOR LEV TAB               
         MVC   NDCALLRD,4(RD)      SAVE BACK NMOD LINK                          
         ST    RC,NDSAVWK          SAVE A(WORK AREA)                            
*                                                                               
         L     RF,=A(EXLEV)        RELOCATE                                     
         AR    RF,R3                                                            
         ST    RF,AEXLEV                                                        
         L     RF,=A(LVTRCE)                                                    
         AR    RF,R3                                                            
         ST    RF,ALVTRCE                                                       
         L     RF,=A(PARSUBR)                                                   
         AR    RF,R3                                                            
         ST    RF,APARSUBR                                                      
         MVC   PARAMS(24),0(R1)                                                 
         L     R2,PARAM2                                                        
         MVC   COMMAND,0(R2)                                                    
         MVI   NDERRMSG,C' '                                                    
         MVC   NDERRMSG+1(L'NDERRMSG-1),NDERRMSG                                
         LH    RF,NDKLEN           KEY LENGTH                                   
         BCTR  RF,R0                                                            
         STH   RF,NDKLENM1         KEY LENGTH-1                                 
*                                                                               
         MVI   DMINBTS,0           NO DELETES                                   
         MVI   DMOUTBTS,X'FF'                                                   
         CLI   NDDELRSW,C'Y'       UNLESS REQUESTED                             
         BNE   ND05D                                                            
         CLC   =C'READ',COMMAND    FOR READS                                    
         BE    ND05                                                             
         CLC   =C'SEQ',COMMAND     KEY SEQUENTIALS                              
         BE    ND05                                                             
         CLC   =C'HIGH',COMMAND    AND HIGHS                                    
         BNE   ND05D                                                            
*                                                                               
ND05     DS    0H                                                               
         MVI   DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
ND05D    DS    0H                                                               
         MVC   NDAREC,NDIOA        AREC NORMALLY IOA 1                          
         LA    RF,DMWRK                                                         
         STCM  RF,15,NDDMWORK         GIVE CALLER A(DMWRK)                      
         MVI   NDERR,0                                                          
         MVI   REPOSW,0                                                         
         MVI   NDMODE,0                                                         
*                                                                               
         CLC   =C'MAST',COMMAND    COMMAND TO ADD NEW MASTER                    
         BE    MASTADD             MUST BE DONE HERE                            
*                                                                               
         CLI   NDOPENSW,C'Y'       TEST 'OPENED'                                
         BE    *+8                                                              
         BAS   RE,OPEN                                                          
*                                                                               
         ZIC   RF,NDNDPOS          NODE POSITION                                
         ZIC   RE,NDNODLN          PLUS NODE LENGTH                             
         AR    RF,RE                                                            
         BCTR  RF,R0                                                            
         STH   RF,NDKCLND          SET LENGTH OF KEY THRU NODE END-1            
         ZIC   RE,NDCODLN          PLUS CODE LENGTH                             
         AR    RF,RE                                                            
         STH   RF,NDKCLCD          SET LENGTH OF KEY THRU CODE END-1            
*                                                                               
         CLI   NDREREAD,C'Y'                                                    
         BE    *+8                                                              
         MVI   NDREREAD,C'N'                                                    
         MVC   RERDSW,NDREREAD     SET REREAD =Y OR N                           
         MVI   EXLATTSW,C'N'                                                    
         MVI   ADDSW,C'N'                                                       
         MVI   MOVSW,C'N'                                                       
*                                                                               
         MVI   INPT,C' '           SET INPUT 'KEY' IN INPT                      
         MVC   INPT+1(L'INPT-1),INPT                                            
         L     R3,PARAM3                                                        
         LTR   R3,R3               IS 'KEY' GIVEN AT PARAM3                     
         BZ    ND06                NO                                           
         MVC   INPT,0(R3)          YES- SET DIRECTLY                            
         B     ND07                                                             
*                                                                               
ND06     DS    0H                  ELSE SET INPT FROM LEV TAB                   
         ZIC   R6,NDLEV            FIRST SET CODES                              
         LTR   R6,R6                                                            
         BNP   ND07                NO LEVELS                                    
         LA    R9,NDLVTAB+NDLVTABL START AT LEVEL ONE                           
*                                                                               
ND06B    DS    0H                                                               
         CLC   NDLVCOD,NDLVRCOD    IF ASKED FOR CODE                            
         BE    *+14                NOT = CURRENT                                
         MVC   NDLVCOD,NDLVRCOD    SET IT AND                                   
         OI    NDLVSTAT,X'20'      FORCE READ                                   
         LA    R9,NDLVTABL(R9)                                                  
         BCT   R6,ND06B                                                         
*                                                                               
ND06D    DS    0H                                                               
         BAS   RE,CONCAT           SET INPT                                     
*                                                                               
ND07     DS    0H                                                               
*                                  SET LEVEL 1 NODE                             
         LA    R9,NDLVTAB+NDLVTABL   POINT TO LEVEL 1                           
         MVC   NDLVNOD,NDL1NOD     NORMAL NODE FOR LEV 1                        
         CLC   INPT(1),NDLIBPRE    UNLESS LIBRARY BOOK                          
         BNE   *+10                                                             
         MVC   NDLVNOD,NDLIBNOD    THEN USE LIBRARY NODE                        
*                                                                               
         EJECT                                                                  
*                                     COMMAND PROCESSING                        
*                                     ------------------                        
         SPACE 2                                                                
         CLC   =C'LSEQ',COMMAND     LOGICAL SEQ                                 
         BE    LSEQ                                                             
         CLC   =C'SEQ',COMMAND     SEQ                                          
         BE    SQP                                                              
         CLC   =C'READ',COMMAND    READ                                         
         BE    RDP                                                              
         CLC   =C'HIGH',COMMAND    HIGH                                         
         BE    RDP                    -(SHARES READ ROUTINE)                    
         CLC   =C'BSEQ',COMMAND    BACK SEQ                                     
         BE    BSEQ                                                             
         CLC   =C'ADD',COMMAND     ADD                                          
         BE    ADP                                                              
         CLC   =C'PUT',COMMAND     PUT RECORD                                   
         BE    PUTP                                                             
         CLC   =C'DEL',COMMAND     DELETE (INCLUDES DELALL)                     
         BE    RDP                    -(SHARES READ ROUTINE)                    
         CLC   =C'REN',COMMAND     RENAME                                       
         BE    RDP                    -(SHARES READ ROUTINE)                    
         CLC   =C'RES',COMMAND     RESTORE                                      
         BE    ADP3                   -(SHARES ADD ROUTINE)                     
         CLC   =C'MOVE',COMMAND    MOVE - CHANGE SEQUENCE                       
         BE    RDP                    -(SHARES READ ROUTINE)                    
         CLC   =C'TRACE',COMMAND   TRACE ORIGIN                                 
         BE    TRCE                                                             
         CLC   =C'COPY',COMMAND    COPY A FAMILY OF RECORDS                     
         BE    REPO                   -(SHARES REPO LOGIC)                      
         CLC   =C'REPO',COMMAND    REPOSITION (MOVE TO NEW PARENT)              
         BE    REPO                                                             
*                                                                               
ND08     DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
NDIOX    DS    0H                                                               
         CLI   REPOSW,0            IF PROCESSING A REPO                         
         BNE   REPORET             RETURN TO REPO LOGIC                         
         CLI   NDDUMP,C'F'         ALWAYS DUMP                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
NDIOX4   DS    0H                                                               
         CLI   NDERR,0             TEST ANY ERROR                               
         BE    NDIOXX              NO                                           
         MVI   NDREREAD,C'Y'       YES-FORCE RE-READ NEXT TIME                  
         BAS   RE,SETERM           SET ERRMSG                                   
         CLI   NDDUMP,0            TEST TO DUMP ON ERROR                        
         BE    NDIOXX                                                           
         DC    H'0'                                                             
*                                                                               
NDIOXX   DS    0H                                                               
         MVI   NDMODE,NDEND                                                     
         BAS   RE,HOOK                                                          
*                                                                               
XIT      XIT1                                                                   
*                                                                               
SETERM   DS    0H                   SET ERROR MESSAGES                          
         LA    R6,ERRMSGS                                                       
*                                                                               
SERM4    DS    0H                                                               
         CLI   0(R6),X'FF'         END                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NDERR,0(R6)                                                      
         BNE   *+12                                                             
         MVC   NDERRMSG(EMSGL),1(R6)                                            
         BR    RE                                                               
*                                                                               
         LA    R6,EMSGL+1(R6)                                                   
         B     SERM4                                                            
         SPACE 3                                                                
*        HOOK - CALL USER ROUTINE                                               
         SPACE 2                                                                
HOOK     DS    0H                                                               
         ZIC   RF,NDLEV            SET POINTER TO LEVEL ENTRY                   
         MH    RF,=Y(NDLVTABL)                                                  
         LA    RF,NDLVTAB(RF)                                                   
         ST    RF,NDLEVPTR                                                      
*                                                                               
         OC    NDHOOK,NDHOOK                                                    
         BNZ   HOOKN                                                            
         LTR   RE,RE               NO-USER HOOK- SET CC NOT =                   
         BR    RE                  SO WE CAN CONTINUE                           
*                                                                               
HOOKN    NTR1                                                                   
         L     RF,NDHOOK           A(HOOK)                                      
         L     RE,NDCALLRD         BACK NMOD LINK                               
         LR    R0,RE               SET R0 SO CALLER CAN EXIT                    
         LM    R1,RC,24(RE)        CALLERS R1-RC                                
         BASR  RE,RF               GO TO CALLER                                 
*                                                                               
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)        RESTORE NODIO REGS                           
*                                                                               
         CLI   NDMODE,NDEND        CALLER SAYS WE'RE DONE                       
         BR    RE                                                               
         EJECT                                                                  
*                             READ COMMAND PROCESSING                           
*                             (ALSO USED FOR DEL,HIGH,MOVE,AND PUT)             
*                             -------------------------------------             
         SPACE 2                                                                
RDP      DS    0H                                                               
         LA    R3,INPT                                                          
         MVI   NDLEV,0                                                          
         MVI   NDERR,0                                                          
         MVI   NDSQBACK,0                                                       
         LA    R9,NDLVTAB+NDLVTABL  POINT TO LEVEL 1                            
*                                                                               
         CLI   0(R3),C' '          IF NO KEY GIVEN                              
         BH    RDP4                                                             
         MVC   NDLVCOD,NULLS       CLEAR FIRST CODE                             
         B     RDP8                OK AS START OF GLOBAL SEQUENTIAL             
*                                                                               
RDP4     DS    0H                                                               
         BAS   RE,PARSE                                                         
         CLI   NDERR,0                                                          
         BNE   RDP9                                                             
*                                                                               
         TM    NDLVSTAT,X'40'      TEST NEW THIS TIME                           
         BZ    RDP5                NO- SKIP TRACE AND USER CALL                 
*                                                                               
RDP4D    DS    0H                                                               
         MVI   NDMODE,NDVAL        MODE IS VALIDATE                             
         CLI   0(R3),C' '          UNLESS AT END OF LEVELS                      
         BH    RDP4F                                                            
         MVI   NDMODE,NDPROC       THEN MODE IS PROCESS                         
         CLC   =C'HIGH',COMMAND    FOR HIGHS                                    
         BNE   *+8                                                              
         BAS   RE,SETOUT           SET OUTPUT'KEY' FOR USER                     
*                                                                               
RDP4F    DS    0H                                                               
         BAS   RE,LVTRACE                                                       
         BAS   RE,HOOK             RETURN TO CALLER                             
         BE    RDP9                CALLER SAYS QUIT                             
*                                                                               
RDP5     DS    0H                                                               
         CLI   NDMODE,NDPROC       TEST AT END                                  
         BE    RDP6                YES                                          
         LA    R3,1(R3)            NO- BUMP PAST DELIMITER                      
         LA    R9,NDLVTABL(R9)                                                  
         B     RDP4                                                             
*                                                                               
RDP6     DS    0H                                                               
         MVC   NDSQBACK,NDLEV      SET TO CONTROL SEQ READ                      
*                                                                               
         CLC   =C'READ',COMMAND    FOR READS                                    
         BE    RDP6C                                                            
         CLC   =C'HIGH',COMMAND    AND HIGHS                                    
         BNE   RDP6D                                                            
RDP6C    DS    0H                                                               
         BAS   RE,GETSUBS          GET ANY SUB-RECORDS                          
         B     RDP8                AND DONE                                     
*                                                                               
RDP6D    DS    0H                  FOR OTHER THAN READS                         
         CLI   NDLIBLEV,0          LIB CALL MUST NOT BE IN EFFECT               
         BE    RDP7                                                             
         CLC   NDLEV,NDLIBLEV                                                   
         BNH   RDP7                                                             
         MVI   NDERR,NDLIBERR                                                   
         B     RDP8                                                             
*                                                                               
RDP7     DS    0H                                                               
         CLC   =C'DEL',COMMAND     DELETE                                       
         BE    DELP                                                             
         CLC   =C'MOVE',COMMAND    MOVE                                         
         BE    MOVP                                                             
         CLC   =C'REN',COMMAND     RENAME                                       
         BE    RENP                                                             
         CLC   =C'PUT',COMMAND     PUT                                          
         BE    PUTP10                                                           
*                                                                               
RDP8     DS    0H                                                               
         MVI   NDREREAD,C'N'       NO RE-READ NEXT TIME                         
RDP9     DS    0H                                                               
         B     NDIOX                                                            
         EJECT                                                                  
*                                   DELETE COMMAND PROCESSING                   
*                                   -------------------------                   
         SPACE 2                                                                
DELP     DS    0H                                                               
         MVI   SPMODE,C'D'         SETPOS MODE = DELETE                         
         BAS   RE,SETPOS           REMOVE POSITIONING                           
         BNE   DELP20              ERROR                                        
*                                                                               
         MVC   KEY,NDLVKEY                                                      
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,KEY                                                           
         AH    R3,NDKLEN           POINT TO CONTROL BYTE                        
         OI    0(R3),X'80'         SET DELETED                                  
         BAS   RE,WRITE                                                         
*                                                                               
         CLI   NDSUBKLN,0          TEST ANY POSSIBLE SUB-RECORDS                
         BE    DELP10              NO                                           
*                                                                               
DELP8    DS    0H                  READ AND DELETE ALL SUB-RECORDS              
         BAS   RE,SEQ                                                           
         BAS   RE,KEYCHKC          CHECK KEY THRU CODE                          
         BNE   DELP10                                                           
         OI    0(R3),X'80'         MARK DELETED                                 
         BAS   RE,WRITE                                                         
         B     DELP8                                                            
*                                                                               
DELP10   DS    0H                                                               
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
*                                                                               
         CLC   =C'DELALL',COMMAND  DELETE ALL RECORDS BELOW                     
         BNE   DELP20                                                           
         MVI   DMINBTS,X'08'       PASS DELETES (TO PREVENT                     
         MVI   DMOUTBTS,X'FD'      DELETING ONLY ALTERNATE RECORDS)             
         B     SQP                 USE SEQUENTIAL PROCESSING                    
*                                                                               
DELP20   DS    0H                                                               
         B     NDIOX                                                            
         EJECT                                                                  
*                                MOVE (CHANGE SEQUENCE)                         
*                                ---------------------                          
         SPACE 2                                                                
MOVP     DS    0H                                                               
         CLI   NDUSEQ,C'Y'         ONLY ALLOWED IF USER SEQ SUPPORTED           
         BNE   ND08                                                             
*                                                                               
         MVI   MOVSW,C'Y'          SET 'MOVING'                                 
         MVI   SPMODE,C'D'         SETPOS MODE = DELETE                         
         BAS   RE,SETPOS           REMOVE FROM CURRENT POSITION                 
         BNE   NDIOX               ERROR                                        
*                                                                               
         MVI   SPMODE,C'A'         SETPOS MODE = ADD                            
         BAS   RE,SETPOS           ADD AT NEW POSITION                          
         BE    *+6                                                              
         DC    H'0'                MUST ABEND ON ADD ERROR                      
*                                                                               
         BAS   RE,PUT              NOTE- RECORD READ IN SETPOS                  
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
*                                                                               
         B     NDIOX                                                            
         EJECT                                                                  
*                                       REN (RENAME)                            
*                                       ------------                            
         SPACE 2                                                                
RENP     DS    0H                                                               
         XC    NDLVCOD,NDLVCOD     SET NEW CODE IN NDLVCOD                      
         L     R4,PARAM4           A(NEW CODE)                                  
         ZIC   R5,PARAM4           LENGTH OF CODE                               
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NDLVCOD(0),0(R4)    SET NEW CODE IN NDLVCOD                      
         MVI   RNCNTL,X'A0'        SET RENAME CONTROL BYTE                      
         MVC   SAVKEY,KEY          HOLD ONTO OLD KEY                            
*                                                                               
*                                  DO MAIN RECORD FIRST                         
         BAS   RE,RNSET                                                         
         CLI   NDERR,0                                                          
         BNE   RENP20                                                           
*                                  HANDLE USER-SEQUENCING                       
         MVI   SPMODE,C'R'         SETPOS MODE=RENAME                           
         BAS   RE,SETPOS                                                        
         BE    *+6                                                              
         DC    H'0'                MUST ABEND ON SETPOS ERROR                   
*                                                                               
*                                  NOW READ RECORDS AT NEXT LEVEL               
*                                  TO SET NEW BACK NODE ELEM                    
*                                                                               
*                                  ***ENTRY POINT FOR REPO LOGIC**              
RENP5    DS    0H                                                               
         CLC   NDATTLEV,NDLEV      NOT IF THIS IS ATTACHMENT LEVEL              
         BE    RENP10                                                           
         XC    WKNC,WKNC           CLEAR NODE AND CODE                          
         OC    WKNOD,NDLVNOD2      NODE OF NEXT LEVEL                           
         BZ    RENP10              NONE                                         
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         BAS   RE,SETKEY                                                        
         ZIC   R4,NDSKPOS          POSITION OF SUB KEY                          
         LA    R4,KEY(R4)                                                       
         ZIC   R5,NDSUBKLN         LENGTH OF SUB-KEY                            
         BCTR  R5,R0                                                            
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
RENP7    DS    0H                                                               
         BAS   RE,SEQ                                                           
*                                                                               
         BAS   RE,KEYCHKN          THRU NODE                                    
         BNE   RENP10                                                           
         LTR   R5,R5               TEST ANY SUB-KEYS                            
         BZ    *+12                                                             
         EX    R5,RENCKSUB                                                      
         BNZ   RENP7               SKIP SUBRECORDS                              
*                                                                               
         BAS   RE,GET                                                           
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         CLI   REPOSW,0            IF REPO                                      
         BE    *+10                                                             
         MVC   WKNOD,REPONOD       USE NEW NODE                                 
         BAS   RE,SETBL            ADD BACK LINK ELEM                           
         BE    *+6                                                              
         DC    H'0'                RECORD OVERFLOW-MUST ABEND                   
*                                                                               
         BAS   RE,PUT                                                           
         B     RENP7                                                            
*                                                                               
RENP10   DS    0H                  NOW TRY ANY SUB-RECORDS                      
         CLI   NDSUBKLN,0          ANY POSSIBLE                                 
         BE    RENP13                                                           
         MVC   KEY,SAVKEY          RESTORE ORIGINAL KEY                         
         BAS   RE,HIGH             SKIP ORIGINAL, START WITH FIRST SUB          
         MVI   DMINBTS,0           NO MORE DELETES                              
         MVI   DMOUTBTS,X'FF'                                                   
         BAS   RE,SEQ                                                           
*                                                                               
RENP12   DS    0H                                                               
         BAS   RE,KEYCHKC          TEST THRU CODE                               
         BNE   RENP13                                                           
         MVC   SAVKEY,KEY                                                       
         BAS   RE,GET                                                           
         BAS   RE,RNSET                                                         
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST ABEND ON LATER RENAME ERRORS            
*                                                                               
         MVC   KEY,SAVKEY                                                       
         MVI   DMINBTS,0           NO MORE DELETES                              
         MVI   DMOUTBTS,X'FF'                                                   
         BAS   RE,HIGH                                                          
         B     RENP12                                                           
*                                                                               
RENP13   DS    0H                                                               
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
*                                                                               
RENP20   DS    0H                                                               
         B     NDIOX                                                            
*                                                                               
RENCKSUB OC    0(0,R4),0(R4)       TEST ANY SUBKEY                              
         SPACE 3                                                                
RNSET    NTR1                                                                   
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   ADDLSW,C'N'         RENAME/DELETE SWITCH                         
*                                                                               
         CLI   REPOSW,0            TEST DOING REPOSITION                        
         BE    RNS2                NO                                           
         ZIC   RE,NDNDPOS          YES- SET NEW NODE                            
         LA    RE,KEY(RE)                                                       
         ZIC   RF,NDNODLN                                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),REPONOD                                                  
         B     RNS3                                                             
*                                                                               
RNS2     DS    0H                  FOR RENAME                                   
         ZIC   RE,NDCDPOS          SET NEW KEY                                  
         LA    RE,KEY(RE)          START OF CODE                                
         ZIC   RF,NDCODLN          LENGTH OF CODE                               
         BCTR  RF,R0                                                            
         EX    RF,*+8              SET NEW CODE                                 
         B     *+10                                                             
         MVC   0(0,RE),NDLVCOD                                                  
*                                                                               
RNS3     DS    0H                                                               
         MVC   RNKEY,KEY           HOLD NEW KEY                                 
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         BAS   RE,KEYCHK                                                        
         BNE   RNS6                                                             
*                                  KEY IS ON FILE                               
         LA    R3,KEY                                                           
         AH    R3,NDKLEN           POINT TO CONTROL BYTE                        
         TM    0(R3),X'80'         TEST DELETED                                 
         BZ    RNS20               NO- ERROR                                    
         MVI   ADDLSW,C'Y'         SET RENAMING TO A DELETE                     
         NI    0(R3),X'0F'         SET OFF DELETE RELATED BITS                  
         BAS   RE,WRITE            WRITE UNDELETED POINTER                      
*                                                                               
         MVC   NDAREC,NDIOA2       READ OLD REC INTO IOA2                       
         BAS   RE,GET                                                           
*                                  SAVE ANY B1 ELEM FOR B2                      
*                                  (TO INCLUDE IN NEW RECORD TO                 
*                                  CONTROL PURGES IN DUMP/LOAD)                 
         MVI   WORK,0                                                           
         TM    0(R3),X'30'         DO ONLY FOR PREVIOUS DELETES                 
         BNZ   RNS4                NOT OLD RENAMES OR REPOS                     
         MVI   ELCODE,X'B1'                                                     
         BAS   RE,NEXTELF                                                       
         BNE   RNS4                                                             
         MVC   WORK,0(R2)                                                       
*                                                                               
RNS4     DS    0H                                                               
         MVC   NDAREC,NDIOA        NEW REC IS IN IOA1                           
         L     R2,NDAREC           SET NEW KEY IN FILE RECORD                   
         LH    RF,NDKLENM1                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RNKEY                                                    
*                                  ADD ANY B1 FROM OLD RECORD AS B2             
*                                  FOR PURGES IN DUMP/LOAD                      
         CLI   WORK,0                                                           
         BE    RNS5                                                             
         SR    R2,R2               ADD AT END                                   
         MVI   WORK,X'B2'          MAKE IT A B2 ELEM                            
         BAS   RE,ADDEL                                                         
         BNE   RNSX                RECORD FULL                                  
*                                                                               
RNS5     DS    0H                                                               
         L     RF,NDAREC                                                        
         AH    RF,NDCNTL                                                        
         NI    0(RF),X'0F'         SET OFF DELETE RELATED BITS                  
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
         BAS   RE,PUT              PUT AS NEW RECORD TO OLD DA                  
*                                  NOW DELETE OLD CODE                          
RNS6     DS    0H                                                               
         MVC   KEY,SAVKEY          RESTORE OLD KEY                              
         BAS   RE,READ                                                          
         LA    R3,KEY                                                           
         AH    R3,NDKLEN           POINT TO CONTROL BYTES                       
         OC    0(1,R3),RNCNTL      SET CONTROL BYTE                             
         BAS   RE,WRITE            DELETE CURRENT POINTER                       
         CLI   ADDLSW,C'Y'         HAVE I RENAMED TO A DELETE                   
         BE    RNSX                YES - DONT ADD NEW RECORD                    
*                                                                               
         L     R2,NDAREC           SET NEW KEY IN FILE REC                      
         LH    RF,NDKLENM1                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RNKEY                                                    
*                                                                               
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
         BAS   RE,ADD              ADD NEW RECORD                               
         B     RNSX                                                             
*                                                                               
RNS20    DS    0H                                                               
         MVI   NDERR,NDRENERR                                                   
*                                                                               
RNSX     DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*                                  PUT COMMAND PROCESSING                       
*                                  (CHANGE OF RECORD JUST READ)                 
*                                  ----------------------------                 
         SPACE 2                                                                
PUTP     DS    0H                                                               
         BAS   RE,MOVR1            SAVE CALLERS VERSION IN IOA2                 
         B     RDP                 USE RDP TO READ RECORD                       
*                                                                               
PUTP10   DS    0H                  RETURN POINT FROM RDP                        
         L     R2,NDIOA                                                         
         L     R3,NDIOA2                                                        
         LH    RF,NDKLENM1                                                      
         EX    RF,PUTCLC           COMPARE KEYS OF IOA1 AND IOA2                
         BE    *+6                                                              
         DC    H'0'                KEY CHANGE NOT ALLOWED                       
*                                                                               
*                                  **TEMP- TEST POINTER ELEMS OK                
*                                  **FIRST SEE IF IN ORIGINAL REC               
*X       MVI   BYTE,0              **                                           
*X       MVI   ELCODE,X'B8'        **FWRD                                       
*X       BAS   RE,NEXTELF          **                                           
*X       BE    *+16                **                                           
*X       MVI   ELCODE,X'B9'        **PREV                                       
*X       BAS   RE,NEXTELF          **                                           
*X       BNE   *+8                 **                                           
*X       MVI   BYTE,1              **SET HAD PTRS                               
*                                                                               
*X       BAS   RE,MOVR2            MOVE CALLERS REC BACK TO IOA1                
*                                                                               
*X       CLI   BYTE,1              **TEST HAD PTRS                              
*X       BNE   PUTP12              **                                           
*                                  **                                           
*X       MVI   ELCODE,X'B8'        **FWRD                                       
*X       BAS   RE,NEXTELF          **                                           
*X       BE    PUTP12              **                                           
*X       MVI   ELCODE,X'B9'        **PREV                                       
*X       BAS   RE,NEXTELF          **                                           
*X       BE    PUTP12              **                                           
*X       DC    H'0'                **POINTER ELEMS LOST                         
*                                                                               
PUTP12   DS    0H                                                               
         BAS   RE,MOVR2            MOVE CALLERS REC BACK TO IOA1                
         MVC   SAVLDA,LASTDA       SAVE LAST DISK ADDRESS                       
         BAS   RE,SETATT           SET ANY ATTACHMENT                           
         CLI   NDERR,0                                                          
         BNE   NDIOX                                                            
*                                                                               
         CLC   SAVLDA,LASTDA       TEST OK TO PUT                               
         BE    PUTP13              YES                                          
         LA    RF,KEY              IF NOT, REREAD REC TO BE CHANGED             
         AH    RF,NDDISK           (SETATT MAY HAVE READ FOR LIB CALL)          
         MVC   0(4,RF),SAVLDA                                                   
         MVC   NDAREC,NDIOA2       DO NOT READ OVER CHANGED REC                 
         BAS   RE,GET                                                           
         MVC   NDAREC,NDIOA        RESTORE TO IOA 1                             
*                                                                               
PUTP13   DS    0H                                                               
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
         BAS   RE,PUT                                                           
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
         B     NDIOX               DONE                                         
*                                                                               
PUTCLC   CLC   0(0,R2),0(R3)                                                    
*                                                                               
         EJECT                                                                  
*                                 ADD COMMAND PROCESSING                        
*                                 (ALSO USED FOR RESTORE AND COPY)              
*                                 --------------------------------              
         SPACE 2                                                                
ADP      DS    0H                                                               
         MVI   ADDSW,C'Y'          ADD CONTROL FOR SETPOS                       
         BAS   RE,MOVR1            SAVE RECORD TO BE ADDED IN IOA2              
*                                                                               
ADP3     DS    0H                  ENTRY POINT FOR RESTORE                      
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R3,INPT                                                          
         MVI   NDLEV,0                                                          
         MVI   NDERR,0                                                          
         LA    R9,NDLVTAB+NDLVTABL  POINT TO LEVEL 1                            
*                                                                               
ADP4     DS    0H                                                               
         BAS   RE,PARSE                                                         
*                                                                               
         CLI   0(R3),C' '          TEST AT END                                  
         BNH   ADP5                YES                                          
*                                                                               
         CLI   REPOSW,0            IF REPO (OR COPY)                            
         BE    ADP4C                                                            
         CLC   REPONOD,NDLVNOD     TEST NODE VS SAVED NODE                      
         BNE   ADP4C               TO PREVENT AGAINST MOVE OR COPY              
         MVI   NDERR,NDPMDERR      TO POS WITHIN OWN FAMILY                     
         B     ADP30                                                            
*                                                                               
ADP4C    DS    0H                                                               
         TM    NDLVSTAT,X'40'      NO- TEST NEW THIS TIME                       
         BZ    ADP4D               NO- SKIP TRACE AND RETURN                    
*                                                                               
         LA    RE,KEY              YES- TEST RECORD DELETED                     
         AH    RE,NDKLEN                                                        
         TM    0(RE),X'80'                                                      
         BZ    *+12                                                             
         MVI   NDERR,NDRNFERR                                                   
         B     ADP30                                                            
*                                                                               
         MVI   NDMODE,NDVAL                                                     
         BAS   RE,LVTRACE                                                       
         BAS   RE,HOOK             RETURN TO CALLER                             
         BE    ADP30               CALLER SAYS QUIT                             
*                                                                               
ADP4D    DS    0H                                                               
         CLI   NDERR,0                                                          
         BNE   ADP30                                                            
*                                                                               
         LA    R3,1(R3)            BUMP PAST DELIMITER                          
         LA    R9,NDLVTABL(R9)     NEXT LEVEL                                   
         B     ADP4                                                             
*                                                                               
ADP5     DS    0H                  HAVE END OF 'KEY'                            
         CLI   NDLEV,1             IF ADDING AT LEVEL ONE                       
         BNE   *+8                                                              
         MVI   NDLIBLEV,0          CLEAR LIB LEVEL                              
         CLI   NDLIBLEV,0          LIB CALL CANNOT BE IN EFFECT                 
         BE    ADP6                                                             
         MVI   NDERR,NDAMXERR      MIX OF LIB AND NON-LIB                       
         B     ADP30                                                            
*                                                                               
ADP6     DS    0H                                                               
         CLC   =C'RES',COMMAND     TEST RESTORING                               
         BE    RSTP                                                             
*                                                                               
         MVI   ADDLSW,C'N'          RESET ADDING DELETE SW                      
         CLI   NDERR,NDRNFERR       RECORD NOT FOUND IS OK                      
         BE    ADP7                                                             
         CLI   NDERR,0             OTHER ERRORS NO GOOD                         
         BNE   ADP30                                                            
         MVI   NDERR,NDADDERR      ADD OF KEY ALREADY ON FILE                   
         LA    RE,KEY                                                           
         AH    RE,NDKLEN                                                        
         MVI   ADDLSW,C'N'                                                      
         MVC   SVCNTL,0(RE)        SAVE CONTROL BYTE                            
         TM    0(RE),X'80'         TEST DELETED                                 
         BZ    ADP30               NO                                           
         MVI   ADDLSW,C'Y'         SET RE-ADDING A DELETE                       
         MVC   SAVKEY,KEY                                                       
*                                                                               
ADP7     DS    0H                                                               
         MVC   NDLVNOD2,NULLS      CLEAR NEXT LEVEL NODE                        
         CLC   NDLEV,NDMXLEVS      AND NEXT, UNLESS AT MAX                      
         BNL   *+10                                                             
         MVC   NDLVNOD+NDLVTABL,NULLS                                           
         MVI   NDERR,0             RESET ERROR                                  
         MVC   NDLVCOD,XW          SET CODE (XW SET IN PARSE)                   
         OC    NDLVNOD,NDLVNOD     IS NODE ALREADY ESTABLISHED                  
         BNZ   ADP8                                                             
         BAS   RE,GETNOD           GET NEW NODE                                 
         BAS   RE,SETNODM          SET IN MASTER                                
         BAS   RE,MOVNOD           ALIGN NODE IN XW                             
         MVC   NDLVNOD,XW                                                       
         BAS   RE,SETNODH          SET AT HIGHER LEVEL                          
         CLI   NDERR,0                                                          
         BNE   ADP30                                                            
*                                                                               
ADP8     DS    0H                                                               
         BAS   RE,MOVR2            MOVE RECORD BACK TO IAO1                     
         BAS   RE,SETATT           THEN SET ATTACHMENTS                         
         CLI   NDERR,0                                                          
         BNE   ADP30                                                            
*                                                                               
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
*                                                                               
         L     R2,NDAREC                                                        
         LH    RF,NDKLENM1         KEY LENGTH -1                                
         EX    RF,ADPMVC           SET WHOLE KEY IN RECORD                      
*                                                                               
         CLI   NDUSEQ,C'Y'         IF DOING USER SEQ'S                          
         BNE   ADP9                                                             
         CLC   =C'ADD',COMMAND     THEN ON TRUE ADD                             
         BNE   ADP9                                                             
         MVI   ELCODE,X'B1'        MAKE SURE NO NODE ELEM EXISTS                
         BAS   RE,DELEL                                                         
         MVI   ELCODE,X'B6'        OR FIRST POINTER                             
         BAS   RE,DELEL                                                         
         MVI   ELCODE,X'B7'        OR LAST POINTER                              
         BAS   RE,DELEL                                                         
         B     ADP10                                                            
*                                  IF NO USER SEQS, PERMIT ADD                  
ADP9     DS    0H                  OF CURRENT 'PARENT'                          
         MVI   ELCODE,X'B1'        SEE IF IS A PARENT                           
         BAS   RE,NEXTELF                                                       
         BNE   ADP10               NO, OK                                       
         MVI   ELCODE,X'B5'        YES- CHECK B5 ELEM                           
         BAS   RE,NEXTELF                                                       
         BNE   ADP10                                                            
         USING NDBNODEL,R2                                                      
         TM    NDBCNTL,X'80'       MAY NOT BE LIBRARY MEMBER                    
         BZ    ADP10                                                            
         MVI   NDERR,NDLIBERR                                                   
         B     ADP30                                                            
*                                  ADD BACK-NODE ELEMENT                        
ADP10    DS    0H                                                               
         LR    R3,R9                                                            
         SH    R3,=Y(NDLVTABL)           SET NODE AND CODE                      
         MVC   WKNC,NDLVNOD-NDLVTABD(R3) FROM HIGHER LEVEL                      
         BAS   RE,SETBL            SET BACK LINK ELEM                           
         BNE   ADP30               RECORD OVERFLOW                              
*                                                                               
         MVI   SPMODE,C'A'         SETPOS MODE = ADD                            
         BAS   RE,SETPOS           POSITIONING                                  
         BNE   ADP30                                                            
*                                                                               
         CLC   =C'COPY',COMMAND    SPECIAL FOR COPY'S                           
         BNE   ADP16                                                            
*                                                                               
         XC    SAVNOD,SAVNOD                                                    
         MVI   ELCODE,X'B4'        UNLESS ANY ATTACH ELEM                       
         BAS   RE,NEXTELF                                                       
         BE    ADP16                                                            
*                                                                               
         MVI   ELCODE,X'B1'        FIND ANY CURRENT NODE ELEM                   
         BAS   RE,NEXTELF                                                       
         BNE   ADP16               IF NONE, OK- NO LOWER COPIES                 
*                                                                               
         USING NDNODEL,R2                                                       
         MVC   SAVNOD,NDNODE       SAVE CURRENT NODE FOR SEQ                    
         MVC   NDAREC,NDIOA2       COPIED REC WILL EST NEW NODE                 
         BAS   RE,GETNOD           GET NEW NODE                                 
         MVC   NDAREC,NDIOA                                                     
         BAS   RE,MOVNOD           ALIGN IN XW                                  
         MVC   NDNODE,XW           SET IN NODE ELEM                             
         MVC   NDLVCPYN+NDLVTABL,XW       ALSO SET COPY NODE                    
*                                         AT NEXT LEVEL                         
         L     RF,NDHINOD          RESTORE NDHINOD                              
         BCTR  RF,R0               BECAUSE WONT ACTUALLY USE                    
         ST    RF,NDHINOD          IT UNTIL SEQ FOR COPY                        
*                                                                               
ADP16    DS    0H                                                               
         CLI   ADDLSW,C'Y'         TEST RE-ADDING A DELETE                      
         BE    ADP16D              YES                                          
*                                  NO                                           
         MVC   NDAREC,NDIOA                                                     
         BAS   RE,ADD                                                           
         L     RF,DMCB+8                                                        
         MVC   NDLVDA,0(RF)        SET DISK ADDRESS                             
         MVC   NDLVDA2,0(RF)       ALSO 'ATTACH' DA                             
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
         B     ADP17                                                            
*                                                                               
ADP16D   DS    0H                  RE-ADDING A DELETE                           
         MVC   KEY,SAVKEY          RESTORE KEY                                  
         MVC   NDAREC,NDIOA2                                                    
         BAS   RE,GET              GET OLD RECORD IN IOA2                       
*                                  SAVE ANY B1 ELEM                             
*                                  (TO INCLUDE IN NEW RECORD TO                 
*                                  CONTROL PURGES IN DUMP/LOAD)                 
         MVI   WORK,0                                                           
         TM    SVCNTL,X'30'        DO ONLY FOR RENAMES                          
         BNZ   ADP16F              NOT RENAMES OR REPOS                         
         MVI   ELCODE,X'B1'                                                     
         BAS   RE,NEXTELF                                                       
         BNE   ADP16F                                                           
         MVC   WORK,0(R2)                                                       
*                                                                               
ADP16F   DS    0H                                                               
         MVC   NDAREC,NDIOA        NOW USE IOA1                                 
*                                  ADD ANY B1 FROM OLD RECORD AS B2             
*                                  FOR PURGES IN DUMP/LOAD                      
         CLI   WORK,0                                                           
         BE    ADP16H                                                           
         SR    R2,R2               ADD AT END                                   
         MVI   WORK,X'B2'          MAKE IT A B2 ELEM                            
         BAS   RE,ADDEL                                                         
         BE    *+6                                                              
         DC    H'0'                RECORD FULL- MUST ABEND                      
*                                                                               
ADP16H   DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   NDLVDA,0(RF)        SET DISK ADDRESS                             
         MVC   NDLVDA2,0(RF)       ALSO 'ATTACH' DA                             
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
*                                                                               
         GOTO1 READ                                                             
         LA    RF,KEY                                                           
         AH    RF,NDKLEN           POINT TO CONTROL BYTE                        
         NI    0(RF),X'0F'         SET OFF DELETE RELATED BITS                  
         BAS   RE,WRITE                                                         
*                                                                               
ADP17    DS    0H                                                               
         CLC   =C'COPY',COMMAND    FOR COPY'S                                   
         BNE   ADP20                                                            
         BAS   RE,GETSUBS          DO SUBRECS FOR THIS RECORD                   
         OC    SAVNOD,SAVNOD       DO WE NEED TO COPY ANY LOWER RECS            
         BZ    ADP20               NO                                           
         MVC   NDSQBACK,NDLEV                                                   
         GOTO1 AEXLEV                                                           
         MVC   NDLVNOD+NDLVTABL,SAVNOD   RESTORE CURRENT NODE                   
         MVC   NDLVNOD2,SAVNOD           IN THIS ENTRY ALSO                     
         MVC   NDLVCPYN,NDLVNOD                                                 
         MVI   DMINBTS,0           NO DELETES                                   
         MVI   DMOUTBTS,X'FF'                                                   
         B     SQP                 DO A SEQUENTIAL READ                         
*                                                                               
ADP20    DS    0H                                                               
ADP30    DS    0H                                                               
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
         B     NDIOX                                                            
*                                                                               
ADPMVC   MVC   0(0,R2),KEY                                                      
         EJECT                                                                  
*                                  RESTORE COMMAND                              
*                                  ---------------                              
         SPACE 2                                                                
RSTP     DS    0H                                                               
         CLI   NDERR,0             TEST ERROR                                   
         BNE   ADP30                                                            
*                                                                               
         LA    RF,KEY                                                           
         AH    RF,NDDISK           POINT TO DISK ADDRESS                        
         MVC   NDLVDA,0(RF)        SET DISK ADDRESS                             
         MVC   NDLVDA2,0(RF)       ALSO 'ATTACH' DA                             
*                                                                               
         MVC   SAVKEY,KEY                                                       
         LA    RF,KEY                                                           
         AH    RF,NDKLEN           POINT TO CONTROL BYTES                       
         TM    0(RF),X'80'         MUST BE A DELETE                             
         BNZ   *+12                                                             
         MVI   NDERR,NDRESERR                                                   
         B     NDIOX                                                            
*                                                                               
         TM    0(RF),X'30'         AND NOT A RENAME OR REPO                     
         BZ    *+12                                                             
         MVI   NDERR,NDRNFERR      TREAT AS RECORD NOT FOUND                    
         B     NDIOX                                                            
*                                                                               
RSTP3    DS    0H                                                               
         NI    0(RF),X'0F'         SET OFF DELETE RELATED BITS                  
         BAS   RE,WRITE                                                         
*                      NB - SETPOS DOESNT PRESERVE ORIGINAL SEQUENCE            
         MVI   SPMODE,C'A'         SETPOS MODE = ADD                            
         BAS   RE,SETPOS           POSITIONING                                  
         BE    *+6                                                              
         DC    H'0'                MUST ABEND ON SETPOS ERROR                   
*                                  NOTE-RECORD READ IN SETPOS                   
         L     R2,NDAREC           UNDELETE RECORD AS WELL                      
         AH    R2,NDCNTL                                                        
         NI    0(R2),X'0F'                                                      
         MVI   NDMODE,NDLOOK       LET USER HAVE RECORD                         
         BAS   RE,PUT                                                           
         BAS   RE,HOOK                                                          
*                                  RESTORE ANY SUB-RECORDS                      
         CLI   NDSUBKLN,0          TEST ANY POSSIBLE                            
         BE    RSTP10                                                           
*                                                                               
         MVC   KEY,SAVKEY                                                       
         LA    R3,KEY                                                           
         AH    R3,NDKLEN                                                        
         BAS   RE,HIGH             HIGH TO BASE RECORD                          
*                                                                               
RSTP8    DS    0H                                                               
         BAS   RE,SEQ                                                           
         BAS   RE,KEYCHKC          CHECK THRU CODE                              
         BNE   RSTP10                                                           
         TM    0(R3),X'80'         TEST DELETED                                 
         BZ    RSTP8                                                            
         NI    0(R3),X'0F'         SET OFF DELETED RELATED BITS                 
         BAS   RE,WRITE                                                         
         B     RSTP8                                                            
*                                                                               
RSTP10   DS    0H                                                               
         MVI   NDREREAD,C'Y'       FORCE RE-READ NEXT TIME                      
         B     NDIOX               BACK INTO ADD LOGIC                          
         EJECT                                                                  
*                                  TRACE - TRACE ORIGIN                         
*                                  --------------------                         
         SPACE 2                                                                
TRCE     DS    0H                                                               
         MVI   NDLEV,1                                                          
         MVI   XW,C' '             CLEAR WORK AREA                              
         MVC   XW+1(L'XW-1),XW                                                  
         LA    R6,XW+L'XW          END OF WORK AREA                             
*                                                                               
         L     R2,NDIOA            SET KEY OF RECORD IN KEY                     
         LH    RF,NDKLENM1         KEY LEN -1                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),0(R2)                                                     
*                                                                               
         B     TRC5                                                             
*                                                                               
TRC4     DS    0H                                                               
         MVC   WKNOD,2(R2)         SET NODE                                     
         XC    WKCOD,WKCOD                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WKCOD(0),8(R2)      SET CODE                                     
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BNE   TRC8                NOT FOUND                                    
*                                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IO AREA                              
         BAS   RE,GET                                                           
*                                                                               
TRC5     DS    0H                                                               
         BAS   RE,TRCSET           SET CODE IN WORK AREA                        
*                                                                               
         MVI   ELCODE,X'B5'                                                     
         BAS   RE,NEXTELF                                                       
         BNE   TRC8                NO BACK NODE ELEM                            
*                                                                               
         USING NDBNODEL,R2                                                      
         OC    NDBNODE,NDBNODE     TEST AT END                                  
         BZ    TRC10                                                            
*                                                                               
         ZIC   RF,NDLEV            BUMP LEVEL POINTER                           
         LA    RF,1(RF)                                                         
         STC   RF,NDLEV                                                         
*                                                                               
         IC    RF,1(R2)                                                         
         SH    RF,=Y(NDBCODE-NDBNODEL+1)  RF HAS CODE LEN-1                     
         B     TRC4                                                             
*                                                                               
TRC8     DS    0H                                                               
         MVI   NDERR,NDRNFERR      SET KEY SO FAR EVEN IF ERROR                 
*                                                                               
TRC10    DS    0H                                                               
         MVI   0(R6),C' '          CLEAR FIRST DELIMITER                        
         LA    R6,1(R6)                                                         
         LA    RF,XW+L'XW-1                                                     
         SR    RF,R6                                                            
         L     RE,PARAM3                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R6)                                                    
         LA    RF,1(RF)                                                         
         AR    RE,RF                                                            
         MVI   0(RE),C' '                                                       
         STC   RF,NDCKEYL          SET LENGTH                                   
         B     TRCX                                                             
*                                                                               
TRCSET   DS    0H                  MOVE CODE FROM KEY TO WORK AREA              
         ZIC   RF,NDCDPOS                                                       
         LA    RF,KEY(RF)          START OF CODE                                
         LA    R3,WORK                                                          
         MVC   WORK,NULLS                                                       
         ZIC   R0,NDCODLN                                                       
         SR    R1,R1                                                            
*                                                                               
TRCS14   DS    0H                                                               
         CLI   0(RF),0                                                          
         BNE   TRCS14B                                                          
*                                                                               
         LTR   R1,R1               0 IS END IF HAVE                             
         BP    TRCS16              FOUND SIGNIFICANT BYTES                      
         B     TRCS15              ELSE IGNORE (FOR RIGHT ALIGNED)              
*                                                                               
TRCS14B  DS    0H                                                               
         MVC   0(1,R3),0(RF)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
TRCS15   DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   R0,TRCS14                                                        
*                                                                               
TRCS16   DS    0H                                                               
         LTR   R1,R1               R1 HAS NUMBER OF BYTES                       
         BNPR  RE                                                               
         SR    R6,R1                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),WORK                                                     
         BCTR  R6,R0                                                            
         MVC   0(1,R6),NDDELIM                                                  
         BR    RE                                                               
*                                                                               
TRCX     DS    0H                                                               
         B     NDIOX                                                            
         EJECT                                                                  
*                                  REPOSITION COMMAND PROCESSING                
*                                  -----------------------------                
*                                  (ALSO USED FOR COPY)                         
*                                                                               
*                      NB- REPO INVOLVES FIVE PHASES                            
*                          1- INITIAL READ OF REC TO BE MOVED                   
*                          2- 'ADDING' RECORD WITH NODE OF NEW PARENT           
*                          3- RE-READ OF ORIGINAL RECORD                        
*                          4- DELETE OF ORIGINAL REC (MARKED REPO)              
*                          5- 'RENAME' ROUTINE TO FIX ANY SUB-RECS              
*                                                                               
*                        THIS SEQUENCE IS FOLLOWED TO GIVE A GOOD               
*                        CHANCE OF FINDING ERRORS BEFORE IT IS TOO              
*                        LATE AND WE HAVE TO ABEND.                             
         SPACE 2                                                                
REPO     DS    0H                                                               
         MVI   REPOSW,C'1'         FIRST READ                                   
         B     RDP                                                              
*                                                                               
REPO4    DS    0H                                                               
         XC    REPONOD,REPONOD     SET REPONOD TO NODE                          
         CLC   NDLEV,NDMXLEVS      ESTABLISHED BY REC                           
         BE    *+10                BEING MOVED (IF ANY)                         
         MVC   REPONOD,NDLVNOD+NDLVTABL                                         
*                                                                               
         OC    PARAM5,PARAM5       TEST HAVE PARAM5                             
         BNZ   REPO4B                                                           
         CLI   COMMAND,C'R'        IF REPO                                      
         BE    MOVP                JUST DO MOVE WITHIN SAME FAMILY              
         DC    H'0'                NEED PARAM5 FOR COPY                         
*                                                                               
REPO4B   DS    0H                                                               
         MVC   SVINPT,INPT         SAVE ORIGINAL 'KEY'                          
         L     RF,PARAM5           NEW PARENT OR NEW CODE                       
         MVC   INPT,0(RF)                                                       
         CLI   COMMAND,C'C'        FOR COPY, JUST DO ADDING                     
         BE    REPO5D                                                           
*                                                                               
         LA    RF,INPT             FIND END TO ADD NEW CODE                     
         CLI   0(RF),C' '          NO INPT= ADD AT LEVEL 1                      
         BH    *+10                                                             
         BCTR  RF,R0                                                            
         B     REPO4D                                                           
*                                                                               
         CLI   0(RF),C' '          FIRST BLANK                                  
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         MVC   0(1,RF),NDDELIM     SET NEW CODE ON END                          
REPO4D   DS    0H                                                               
         MVC   1(NDCODL,RF),NDLVCOD                                             
*                                                                               
         LA    R0,INPT+1           GET LENGTH OF NEW PARENT                     
         SR    RF,R0                                                            
         BNM   REPO5                                                            
*                                  IF ZERO THEN REPO'ING TO LEVEL 1             
         CLI   NDLEV,1             IF ORIG RECORD AT LEVEL 1 ALSO               
         BE    MOVP                THEN JUST DO NORMAL MOVE                     
         B     REPO5D                                                           
*                                                                               
REPO5    DS    0H                  TEST SAME PARENT                             
*                                  NOTE- RF HAS LEN-1 OF NEW PARENT             
         LA    RE,SVINPT+L'SVINPT-1   GET LENGTH OF OLD PARENT                  
         LA    R0,SVINPT                                                        
REPO5B   DS    0H                                                               
         CLC   0(1,RE),NDDELIM        LOOK FOR DELIMITER                        
         BE    REPO5C                                                           
         CR    RE,R0                                                            
         BNH   REPO5D                                                           
         BCT   RE,REPO5B                                                        
REPO5C   DS    0H                                                               
         BCTR  RE,R0                                                            
         SR    RE,R0                                                            
         CR    RE,RF               LENGTHS MUST BE EQUAL                        
         BNE   REPO5D                                                           
         LR    RF,RE                                                            
*                                                                               
         EX    RF,*+8              TEST SAME PARENT                             
         B     *+10                                                             
         CLC   INPT(0),SVINPT                                                   
         BE    MOVP                YES- JUST DO NORMAL MOVE                     
*                                                                               
REPO5D   DS    0H                                                               
         MVI   REPOSW,C'A'         ADD TO NEW PARENT                            
         B     ADP                                                              
*                                                                               
REPO8    DS    0H                                                               
         CLC   =C'COPY',COMMAND    FOR COPIES                                   
         BE    NDIOX               DONE                                         
*                                                                               
         MVC   REPONOD,NDLVNOD     SAVE NEW NODE                                
         MVI   REPOSW,C'2'         READ FOR 2ND TIME                            
         MVC   INPT,SVINPT                                                      
         B     RDP                                                              
*                                                                               
REPO10   DS    0H                                                               
         MVI   SPMODE,C'D'         SETPOS MODE = DELETE                         
         XC    PARAM4,PARAM4       REMOVE POSITIONING BACAUSE IT                
         BAS   RE,SETPOS           REFERS TO NEW FAMILY                         
         BE    *+6                                                              
         DC    H'0'                MUST ABEND ON ERROR                          
*                                                                               
         MVC   KEY,NDLVKEY                                                      
         MVC   SAVKEY,KEY                                                       
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,KEY                                                           
         AH    R3,NDKLEN           POINT TO CONTROL BYTE                        
         OI    0(R3),X'90'         SET DELETED AND REPO'D                       
         BAS   RE,WRITE                                                         
*                                                                               
         MVI   REPOSW,C'R'         'RENAME' PHASE                               
         MVI   RNCNTL,X'90'        REPO CONTROL BYTE                            
         B     RENP5                                                            
         SPACE 3                                                                
*                                  REPO RETURN CONTROLS                         
*                                  --------------------                         
*                                                                               
REPORET  DS    0H                                                               
         CLI   REPOSW,C'1'         FROM FIRST READ                              
         BNE   RPR6                                                             
         MVI   REPOSW,0                                                         
         CLI   NDERR,0                                                          
         BNE   NDIOX                                                            
         B     REPO4                                                            
*                                                                               
RPR6     DS    0H                                                               
         CLI   REPOSW,C'A'         FROM ADD                                     
         BNE   RPR10                                                            
         MVI   REPOSW,0                                                         
         CLI   NDERR,0                                                          
         BE    REPO8                                                            
         MVI   NDERR,NDPMDERR      TREAT ALL ERRORS                             
         B     NDIOX               AS POSITIONING ERRORS                        
*                                                                               
RPR10    DS    0H                                                               
         CLI   REPOSW,C'2'         FROM 2ND READ                                
         BNE   RPR14                                                            
         MVI   REPOSW,0                                                         
         CLI   NDERR,0                                                          
         BE    REPO10                                                           
         DC    H'0'                MUST ABEND                                   
*                                                                               
RPR14    DS    0H                                                               
         CLI   REPOSW,C'R'         FROM RENAME                                  
         BNE   RPR18                                                            
         MVI   REPOSW,0                                                         
         CLI   NDERR,0                                                          
         BE    NDIOX                                                            
         DC    H'0'                MUST ABEND                                   
*                                                                               
RPR18    DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                  SEQ COMMAND PROCESSING                       
*                                  ----------------------                       
         SPACE 2                                                                
SQP      DS    0H                                                               
*                                 INITIALIZE FOR SEQUENTIAL READING             
         ZIC   R9,NDLEV                                                         
         MH    R9,=Y(NDLVTABL)                                                  
         LA    R9,NDLVTAB(R9)                                                   
         MVI   SQFRST,C'Y'                                                      
*                                                                               
         CLC   NDSQBACK,NDLEV      IF NOT BACKING UP BEYOND                     
         BL    SQP4                THIS LEVEL                                   
         BAS   RE,TSTLEV           THEN TEST ANY LOWER LEVEL                    
         BZ    SQP30               NO- DONE,GIVEN KEY IS END OF LINE            
         B     SQP5                                                             
*                                                                               
SQP4     DS    0H                                                               
         BAS   RE,TSTLEV           TEST ANY LOWER LEVEL                         
         BZ    SQP10               NO- GET NEXT AT THIS LEVEL                   
*                                                                               
SQP5     DS    0H                                                               
         MVI   NDMODE,NDFRST       FIRST FOR THIS LEVEL                         
         BAS   RE,SQPGO                                                         
         XC    NDLVSQC,NDLVSQC     CLEAR COUNT OF SEQ'S                         
         CLI   NDSKIP,C'Y'         TEST TO SKIP LOWER RECORDS                   
         BNE   *+12                                                             
         MVI   NDSKIP,C'N'                                                      
         B     SQP24               YES- SET LAST AND GET NEXT                   
*                                  AT THIS LEVEL                                
         LA    R9,NDLVTABL(R9)     ELSE BUMP TO NEXT LEVEL                      
         ZIC   R6,NDLEV            BUMP LEVEL COUNT                             
         LA    R6,1(R6)                                                         
         STC   R6,NDLEV                                                         
         CLC   NDLEV,NDMXLEVS      TEST VS MAXIMUM LEVELS                       
         BNH   SQP6                                                             
         MVI   NDERR,NDLEVERR                                                   
         BAS   RE,SQPGO                                                         
         MVI   NDERR,0                                                          
         B     SQP20                                                            
*                                                                               
SQP6     DS    0H                                                               
         CLC   =C'COPY',COMMAND    FOR COPY'S                                   
         BNE   SQP6D                                                            
         L     RF,NDHINOD          BUMP HIGH NODE                               
         LA    RF,1(RF)                                                         
         ST    RF,NDHINOD                                                       
         BAS   RE,MOVNOD           ALIGN IN XW                                  
         MVC   NDLVCPYN,XW         AND SET IN LEVTAB                            
*                                                                               
SQP6D    DS    0H                                                               
         MVI   SQFRST,C'Y'                                                      
*                                                                               
SQP10    DS    0H                                                               
         CLI   SQFRST,C'Y'         TEST FIRST TIME AT THIS LEVEL                
         BNE   SQP18                                                            
         MVI   SQFRST,C'N'                                                      
*                                  INITIALIZE AT THIS LEVEL                     
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
*                                                                               
         CLC   NDLVCOD,NULLS       IF HAD SEQ START CODE                        
         BNH   SQP10D                                                           
*                                                                               
SQP10C   DS    0H                                                               
         ZIC   RE,NDSKPOS          BYTE AFTER CODE('SUB-KEY')                   
         LA    RE,KEY(RE)          LAST BYTE OF CODE                            
         MVI   0(RE),X'FF'                                                      
*                                                                               
SQP10D   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     SQP12B                                                           
*                                                                               
SQP12    DS    0H                                                               
         BAS   RE,SEQ                                                           
*                                                                               
SQP12B   DS    0H                                                               
         BAS   RE,KEYCHKN          COMPARE THRU NODE                            
         BNE   SQP20               DONE                                         
*                                                                               
         CLC   =C'DELALL',COMMAND  TEST DELETING                                
         BNE   SQP13                                                            
         LA    RF,KEY                                                           
         AH    RF,NDKLEN           POINT TO CONTROL BYTE                        
         OI    0(RF),X'80'         SET DELETED                                  
         BAS   RE,WRITE                                                         
*                                                                               
SQP13    DS    0H                                                               
         CLI   NDSUBKLN,0          TEST ANY POSSIBLE SUBKEYS                    
         BE    SQP13A              NO                                           
         ZIC   RE,NDSKPOS          START OF 'SUBKEY'                            
         LA    RE,KEY(RE)                                                       
         ZIC   RF,NDSUBKLN         LENGTH                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       SKIP IF SUBKEY PRESENT                       
         BNZ   SQP12               BUT NO NODIO REC WITH THAT CODE              
*                                                                               
SQP13A   DS    0H                                                               
         ZIC   RF,NDCDPOS          CODE POSITION                                
         LA    R2,KEY(RF)                                                       
         MVC   NDLVCOD,NULLS       MOVE CODE FROM KEY TO NDLVCOD                
         CLI   NDCDALIN,C'R'                                                    
         BE    SQP13B                                                           
         ZIC   RF,NDCODLN          FOR LEFT ALIGNED JUST MOVE                   
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     SQP16                                                            
         MVC   NDLVCOD(0),0(R2)                                                 
*                                                                               
SQP13B   DS    0H                                                               
         LA    R3,NDLVCOD          RIGHT ALIGNMENT                              
         ZIC   R0,NDCODLN                                                       
         SR    R1,R1                                                            
*                                                                               
SQP14    DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   SQP14B                                                           
*                                                                               
         LTR   R1,R1               0 IS END IF HAVE                             
         BP    SQP16               FOUND SIGNIFICANT BYTES                      
         B     SQP15               ELSE IGNORE                                  
*                                                                               
SQP14B   DS    0H                                                               
         MVC   0(1,R3),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
SQP15    DS    0H                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,SQP14                                                         
*                                                                               
SQP16    DS    0H                                                               
         BAS   RE,GET                                                           
         GOTO1 AEXLEV              EXTRACT DATA                                 
         BAS   RE,PARSUB           PARAMETER SUBSTITUTION                       
         BNE   SQP18               OMIT THIS RECORD                             
*                                                                               
         MVI   NDMODE,NDPROC       PROCESS                                      
         BAS   RE,SQPGO                                                         
         CLC   =C'COPY',COMMAND                                                 
         BNE   *+8                                                              
         BAS   RE,CPYREC                                                        
         BAS   RE,GETSUBS          GET ANY SUB-RECORDS                          
         BE    SQP30               CALLER SAYS TO QUIT                          
         B     SQP4                                                             
*                                                                               
*                                  CONTINUED SEQUENTIAL READING                 
SQP18    DS    0H                                                               
         MVC   KEY,NDLVKEY         SET KEY                                      
         B     SQP10C              BUMP TO NEXT CODE                            
*                                                                               
SQP20    DS    0H                                                               
         ZIC   R6,NDLEV            BACK UP 1 LEVEL                              
         BCTR  R6,R0                                                            
         STC   R6,NDLEV                                                         
         SH    R9,=Y(NDLVTABL)                                                  
*                                                                               
SQP24    DS    0H                                                               
         MVI   NDMODE,NDLAST       LAST FOR LEVEL                               
         BAS   RE,SQPGO                                                         
         CLC   NDLEV,NDSQBACK      IF HAVE NOT REACHED START LEV                
         BH    SQP10               CONTINUE AT NOW CURRENT LEVEL                
*                                  ELSE DONE                                    
SQP30    DS    0H                                                               
         CLC   =C'COPY',COMMAND    FOR COPY'S                                   
         BNE   SQP32                                                            
         LA    R2,KEY              READ MASTER RECORD                           
         AH    R2,NDDISK                                                        
         LA    R3,NDLVTAB                                                       
         MVC   0(4,R2),NDLVDA-NDLVTABD(R3)  DA OF MASTER RECORD                 
         BAS   RE,GET                                                           
         BAS   RE,SETNODM          SET HIGHEST NODE USED                        
         MVI   NDREREAD,C'Y'                                                    
*                                                                               
SQP32    DS    0H                                                               
         B     NDIOX                                                            
         SPACE 2                                                                
SQPGO    DS    0H                  RETURN TO CALLER                             
         LR    R0,RE               SAVE RE                                      
         BAS   RE,SETOUT           SET OUTPUT 'KEY' IN USER AREA                
         BAS   RE,LVTRACE                                                       
         BAS   RE,HOOK             RETURN TO CALLER                             
         BE    SQP30               CALLER SAYS QUIT                             
         LR    RE,R0               RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*                                LOGICAL SEQUENTIAL READING                     
*                                --------------------------                     
         SPACE 2                                                                
LSEQ     DS    0H                                                               
         CLI   NDUSEQ,C'Y'                                                      
         BNE   LSEQ30              USER SEQS NOT SUPPORTED                      
*                                 INITIALIZE FOR SEQUENTIAL READING             
         ZIC   R9,NDLEV                                                         
         MH    R9,=Y(NDLVTABL)                                                  
         LA    R9,NDLVTAB(R9)                                                   
*                                                                               
         LA    RF,NDLVTAB          CLEAR COUNT OF SEQ'S AT                      
LSEQ2    DS    0H                  THIS LEVEL AND ALL HIGHER                    
         CR    RF,R9                                                            
         BH    LSEQ3                                                            
         LA    RE,NDLVSQC-NDLVTABD(RF)                                          
         XC    0(2,RE),0(RE)                                                    
         LA    RF,NDLVTABL(RF)                                                  
         B     LSEQ2                                                            
*                                                                               
LSEQ3    DS    0H                                                               
         MVI   SQFRST,C'Y'                                                      
*                                                                               
         CLC   NDSQBACK,NDLEV      IF NOT BACKING UP BEYOND                     
         BL    LSEQ4               THIS LEVEL                                   
         BAS   RE,TSTLEV           THEN TEST ANY LOWER LEVEL                    
         BZ    LSEQ30              NO- DONE,GIVEN KEY IS END OF LINE            
         B     LSEQ5                                                            
*                                                                               
LSEQ4    DS    0H                                                               
         BAS   RE,TSTLEV           TEST ANY LOWER LEVEL                         
         BZ    LSEQ10              NO- GET NEXT AT THIS LEVEL                   
*                                                                               
LSEQ5    DS    0H                                                               
         MVI   NDMODE,NDFRST       FIRST FOR THIS LEVEL                         
         BAS   RE,SQPGO                                                         
         CLI   NDSKIP,C'Y'         TEST TO SKIP LOWER RECORDS                   
         BNE   *+12                                                             
         MVI   NDSKIP,C'N'                                                      
         B     LSEQ24              YES- SET LAST AND GET NEXT                   
*                                  AT THIS LEVEL                                
         LA    R9,NDLVTABL(R9)     ELSE BUMP TO NEXT LEVEL                      
         XC    NDLVSQC,NDLVSQC     CLEAR COUNT OF SEQ'S                         
         ZIC   R6,NDLEV            BUMP LEVEL COUNT                             
         LA    R6,1(R6)                                                         
         STC   R6,NDLEV                                                         
         CLC   NDLEV,NDMXLEVS      TEST VS MAXIMUM LEVELS                       
         BNH   LSEQ6                                                            
         MVI   NDERR,NDLEVERR                                                   
         BAS   RE,SQPGO                                                         
         MVI   NDERR,0                                                          
         B     LSEQ20                                                           
*                                                                               
LSEQ6    DS    0H                                                               
         MVI   SQFRST,C'Y'                                                      
*                                                                               
LSEQ10   DS    0H                                                               
         CLI   SQFRST,C'Y'         TEST FIRST TIME AT THIS LEVEL                
         BNE   LSEQ18                                                           
         MVI   SQFRST,C'N'                                                      
*                                  INITIALIZE AT THIS LEVEL                     
         CLC   NDLVCOD,NULLS       TEST HAVE START CODE                         
         BH    LSEQ18                                                           
*                                                                               
         MVC   NDLVCOD,NDLVFRST    START WITH FIRST                             
         CLC   NDLVCOD,NULLS                                                    
         BH    *+6                                                              
         DC    H'0'                FIRST SHOULD ALWAYS BE THERE                 
*                                                                               
LSEQ11   DS    0H                                                               
         LH    RE,NDLVSQC          BUMP COUNT OF SEQ'S                          
         LA    RE,1(RE)                                                         
         STH   RE,NDLVSQC                                                       
         LH    RF,NDLVCNT          SUBTRACT TRUE COUNT                          
         LA    RF,20(RF)           **TEMPORARY SLACK                            
         SR    RF,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                CATCH LSEQ READ LOOP                         
*                                                                               
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                NEXT RECORD IS MISSING                       
*                                                                               
         ZIC   RF,NDNDPOS          NODE POS                                     
         ZIC   RE,NDNODLN          PLUS NODE LENGTH                             
         AR    RF,RE                                                            
*                                                                               
         LA    R2,KEY(RF)          START OF CODE IN KEY                         
         MVC   NDLVCOD,NULLS       MOVE CODE FROM KEY TO NDLVCOD                
         CLI   NDCDALIN,C'R'                                                    
         BE    LSEQ13B                                                          
         ZIC   RF,NDCODLN          FOR LEFT ALIGNED JUST MOVE                   
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     LSEQ16                                                           
         MVC   NDLVCOD(0),0(R2)                                                 
*                                                                               
LSEQ13B  DS    0H                                                               
         LA    R3,NDLVCOD          RIGHT ALIGNMENT                              
         ZIC   R0,NDCODLN                                                       
         SR    R1,R1                                                            
*                                                                               
LSEQ14   DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   LSEQ14B                                                          
*                                                                               
         LTR   R1,R1               0 IS END IF HAVE                             
         BP    LSEQ16              FOUND SIGNIFICANT BYTES                      
         B     LSEQ15              ELSE IGNORE                                  
*                                                                               
LSEQ14B  DS    0H                                                               
         MVC   0(1,R3),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
LSEQ15   DS    0H                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,LSEQ14                                                        
*                                                                               
LSEQ16   DS    0H                                                               
         BAS   RE,GET                                                           
         GOTO1 AEXLEV              EXTRACT DATA                                 
         BAS   RE,PARSUB           PARAMETER SUBSTITUTION                       
         BNE   LSEQ18              OMIT THIS RECORD                             
*                                                                               
         MVI   NDMODE,NDPROC       PROCESS                                      
         BAS   RE,SQPGO                                                         
         BAS   RE,GETSUBS          GET ANY SUB-RECORDS                          
         B     LSEQ4                                                            
*                                                                               
*                                  CONTINUED SEQUENTIAL READING                 
LSEQ18   DS    0H                                                               
         CLC   NDLVFWRD,NULLS      TEST HAVE FWRD LINK                          
         BH    LSEQ18D             YES - CONTINUE                               
         CLC   NDLVCOD,NDLVLAST    IF NOT, CURRENT CODE SHOULD = LAST           
         BE    LSEQ20                                                           
         DC    H'0'                                                             
*                                                                               
LSEQ18D  DS    0H                                                               
         MVC   NDLVCOD,NDLVFWRD    SET KEY AND                                  
         B     LSEQ11              GET NEXT RECORD                              
*                                                                               
LSEQ20   DS    0H                                                               
         ZIC   R6,NDLEV            BACK UP 1 LEVEL                              
         BCTR  R6,R0                                                            
         STC   R6,NDLEV                                                         
         SH    R9,=Y(NDLVTABL)                                                  
*                                                                               
LSEQ24   DS    0H                                                               
         MVI   NDMODE,NDLAST       LAST FOR LEVEL                               
         BAS   RE,SQPGO                                                         
         CLC   NDLEV,NDSQBACK      IF HAVE NOT REACHED START LEV                
         BH    LSEQ10              CONTINUE AT NOW CURRENT LEVEL                
*                                  ELSE DONE                                    
LSEQ30   DS    0H                                                               
         B     NDIOX                                                            
         EJECT                                                                  
*                                  BACKWARD SEQUENTIAL READING                  
*                                  ---------------------------                  
         SPACE 2                                                                
BSEQ     DS    0H                                                               
         CLI   NDUSEQ,C'Y'                                                      
         BNE   BSEQ30              USER SEQS NOT SUPPORTED                      
*                                 INITIALIZE FOR SEQUENTIAL READING             
         ZIC   R9,NDLEV                                                         
         MH    R9,=Y(NDLVTABL)                                                  
         LA    R9,NDLVTAB(R9)                                                   
*                                                                               
         LA    RF,NDLVTAB          CLEAR COUNT OF SEQ'S AT                      
BSEQ2    DS    0H                  THIS LEVEL AND ALL HIGHER                    
         CR    RF,R9                                                            
         BH    BSEQ3                                                            
         LA    RE,NDLVSQC-NDLVTABD(RF)                                          
         XC    0(2,RE),0(RE)                                                    
         LA    RF,NDLVTABL(RF)                                                  
         B     BSEQ2                                                            
*                                                                               
BSEQ3    DS    0H                                                               
         MVI   SQFRST,C'Y'                                                      
*                                                                               
         CLC   NDSQBACK,NDLEV      IF NOT BACKING UP BEYOND                     
         BL    BSEQ4               THIS LEVEL                                   
         BAS   RE,TSTLEV           THEN TEST ANY LOWER LEVEL                    
         BZ    BSEQ30              NO- DONE,GIVEN KEY IS END OF LINE            
*                                                                               
BSEQ4    DS    0H                                                               
         CLI   NDSKIP,C'Y'         TEST TO SKIP FIRST                           
         BNE   *+12                (E.G. ON CONTINUATIONS)                      
         MVI   NDSKIP,C'N'                                                      
         B     BSEQ10              GET NEXT AT THIS LEVEL                       
*                                                                               
         BAS   RE,TSTLEV           TEST ANY LOWER LEVEL                         
         BNZ   BSEQ5               YES - GET THEM                               
         CLI   NDLEV,0             DONE IF AT LEVEL 0                           
         BE    BSEQ30                                                           
         MVI   NDMODE,NDPROC       NO SET PROC FOR THIS RECORD                  
         BAS   RE,CHKREC           SEE IF NEED TO READ RECORD                   
         BAS   RE,SQPGO                                                         
         B     BSEQ10              GET NEXT AT THIS LEVEL                       
*                                                                               
BSEQ5    DS    0H                                                               
         MVI   NDMODE,NDFRST       FIRST FOR THIS LEVEL                         
         BAS   RE,SQPGO                                                         
         CLI   NDSKIP,C'Y'         TEST TO SKIP LOWER RECORDS                   
         BNE   *+12                                                             
         MVI   NDSKIP,C'N'                                                      
         B     BSEQ24              YES- SET LAST AND GET NEXT AT                
*                                  THIS LEVEL                                   
         LA    R9,NDLVTABL(R9)     ELSE BUMP TO NEXT LEVEL                      
         XC    NDLVSQC,NDLVSQC     CLEAR COUNT OF SEQ'S                         
         ZIC   R6,NDLEV            BUMP LEVEL COUNT                             
         LA    R6,1(R6)                                                         
         STC   R6,NDLEV                                                         
         CLC   NDLEV,NDMXLEVS      TEST VS MAXIMUM LEVELS                       
         BNH   BSEQ6                                                            
         MVI   NDERR,NDLEVERR                                                   
         BAS   RE,SQPGO                                                         
         MVI   NDERR,0                                                          
         B     BSEQ20                                                           
*                                                                               
BSEQ6    DS    0H                                                               
         MVI   SQFRST,C'Y'                                                      
*                                                                               
BSEQ10   DS    0H                                                               
         CLI   NDLEV,0             DONE IF AT LEVEL 0                           
         BE    BSEQ30                                                           
         CLI   SQFRST,C'Y'         TEST FIRST TIME AT THIS LEVEL                
         BNE   BSEQ18                                                           
         MVI   SQFRST,C'N'                                                      
*                                  INITIALIZE AT THIS LEVEL                     
         CLC   NDLVCOD,NULLS       TEST HAVE START CODE                         
         BH    BSEQ18              YES - GET NEXT                               
*                                                                               
         MVC   NDLVCOD,NDLVLAST    START WITH LAST                              
         CLC   NDLVCOD,NULLS                                                    
         BH    *+6                                                              
         DC    H'0'                LAST SHOULD ALWAYS BE THERE                  
*                                                                               
BSEQ11   DS    0H                                                               
         LH    RE,NDLVSQC          BUMP COUNT OF SEQ'S                          
         LA    RE,1(RE)                                                         
         STH   RE,NDLVSQC                                                       
         LH    RF,NDLVCNT          SUBTRACT TRUE COUNT                          
         LA    RF,20(RF)           **TEMPORARY SLACK                            
         SR    RF,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                CATCH BSEQ READ LOOP                         
*                                                                               
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                NEXT RECORD IS MISSING                       
*                                                                               
         ZIC   RF,NDNDPOS          NODE POS                                     
         ZIC   RE,NDNODLN          PLUS NODE LENGTH                             
         AR    RF,RE                                                            
*                                                                               
         LA    R2,KEY(RF)          START OF CODE IN KEY                         
         MVC   NDLVCOD,NULLS       MOVE CODE FROM KEY TO NDLVCOD                
         CLI   NDCDALIN,C'R'                                                    
         BE    BSEQ13B                                                          
         ZIC   RF,NDCODLN          FOR LEFT ALIGNED JUST MOVE                   
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     BSEQ16                                                           
         MVC   NDLVCOD(0),0(R2)                                                 
*                                                                               
BSEQ13B  DS    0H                                                               
         LA    R3,NDLVCOD          RIGHT ALIGNMENT                              
         ZIC   R0,NDCODLN                                                       
         SR    R1,R1                                                            
*                                                                               
BSEQ14   DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   BSEQ14B                                                          
*                                                                               
         LTR   R1,R1               0 IS END IF HAVE                             
         BP    BSEQ16              FOUND SIGNIFICANT BYTES                      
         B     BSEQ15              ELSE IGNORE                                  
*                                                                               
BSEQ14B  DS    0H                                                               
         MVC   0(1,R3),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
BSEQ15   DS    0H                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,BSEQ14                                                        
*                                                                               
BSEQ16   DS    0H                                                               
         BAS   RE,GET                                                           
         GOTO1 AEXLEV              EXTRACT DATA                                 
         BAS   RE,PARSUB           PARAMETER SUBSTITUTION                       
         BNE   BSEQ18              OMIT THIS RECORD                             
*                                                                               
         B     BSEQ4                                                            
*                                                                               
*                                  CONTINUED SEQUENTIAL READING                 
BSEQ18   DS    0H                                                               
         CLC   NDLVBACK,NULLS      TEST HAVE BACKWARD LINK                      
         BH    BSEQ18D             YES - CONTINUE                               
         CLC   NDLVCOD,NDLVFRST    IF NOT, CURRENT CODE SHOULD = FRST           
         BE    BSEQ20                                                           
         DC    H'0'                                                             
*                                                                               
BSEQ18D  DS    0H                                                               
         MVC   NDLVCOD,NDLVBACK    SET KEY AND                                  
         B     BSEQ11              GET 'NEXT' RECORD                            
*                                                                               
BSEQ20   DS    0H                                                               
         ZIC   R6,NDLEV            BACK UP 1 LEVEL                              
         BCTR  R6,R0                                                            
         STC   R6,NDLEV                                                         
         SH    R9,=Y(NDLVTABL)                                                  
*                                                                               
BSEQ24   DS    0H                                                               
         MVI   NDMODE,NDLAST       LAST FOR LEVEL                               
         BAS   RE,SQPGO                                                         
         BAS   RE,GETSUBS          GET ANY SUB-RECORDS                          
         CLI   NDLEV,0             DONE IF BACK TO LEV 0                        
         BE    BSEQ30                                                           
         MVI   NDMODE,NDPROC       PROCESS (AFTER LAST)                         
         BAS   RE,CHKREC           SEE IF NEED TO READ RECORD                   
         BAS   RE,SQPGO                                                         
         CLC   NDLEV,NDSQBACK      IF HAVE NOT REACHED START LEV                
         BH    BSEQ10              CONTINUE AT NOW CURRENT LEVEL                
*                                  ELSE DONE                                    
*                                                                               
BSEQ30   DS    0H                                                               
         B     NDIOX                                                            
         SPACE 3                                                                
*        TSTLEV-   TEST ANY LOWER LEVEL                                         
         SPACE 2                                                                
TSTLEV   DS    0H                                                               
         CLC   NDLVACD,NULLS       TEST THIS LEVEL HAS ATTACHMENT               
         BNH   TL4                                                              
*                                                                               
         CLC   =C'COPY',COMMAND    SKIP ANY ATTACHED IF COPYING                 
         BE    TLNO                (DONT COPY ATTACHED RECORDS)                 
*                                  READ THE ATTACHED RECORD                     
         LR    R0,RE               SAVE RE                                      
         BAS   RE,SETAFL                                                        
         LR    RE,R0                                                            
*                                                                               
TL4      DS    0H                                                               
         CLC   NDLVNOD2,NULLS       TEST NODE AT NEXT LEVEL                     
         BZ    TLNO                     NONE- NO LOWER RECORDS                  
         CLC   =C'COPY',COMMAND     IF COPYING, NEED TO HANDLE NODE             
         BE    TLYES               EVEN IF NO ACTUAL LOWER RECORDS              
         CLI   NDUSEQ,C'Y'              ARE THERE USER SEQS                     
         BNE   TLYES                    IF NOT, NO FURTHER TEST                 
*                     **NOTE- COULD TRY HIGH WITH NODE, ELSE                    
*                             WILL PASS FRST/LAST EVEN IF NODE EMPTY            
*                                                                               
*                                        IF HAVE USER SEQS                      
TL6      DS    0H                        THERE IS A BETTER TEST                 
         CLC   NDLVFRST+NDLVTABL,NULLS   WILL HAVE LOWER RECORDS                
         BNH   TLNO                      IF FRST IS STATED                      
*                                                                               
TLYES    DS    0H                                                               
         LTR   RE,RE               CC NOT 0, HAVE NEXT LEVEL                    
         BR    RE                                                               
*                                                                               
TLNO     DS    0H                                                               
         CR    RE,RE               SET CC = 0, NO NEXT LEVEL                    
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
*        SETAFL-   SET FIRST AND LAST FROM ATTACHED REC                         
*                                                                               
SETAFL   DS    0H                  GET RECORD ATTACHED                          
         CLC   NDATTLEV,NDLEV      TEST AT THIS LEVEL                           
         BNER  RE                                                               
         CLI   NDUSEQ,C'Y'         ONLY NEED TO READ ATTAHCED REC               
         BNER  RE                  IF HAVE USER SEQS                            
         CLC   =C'LSEQ',COMMAND    NEED ONLY FOR LSEQ                           
         BE    SETAFLN                                                          
         CLC   =C'SEQ',COMMAND     SEQ                                          
         BE    SETAFLN                                                          
         CLC   =C'BSEQ',COMMAND    BSEQ                                         
         BE    SETAFLN                                                          
         CLC   =C'READ',COMMAND    READ                                         
         BE    SETAFLN                                                          
         CLC   =C'HIGH',COMMAND    AND HIGH                                     
         BNER  RE                                                               
*                                                                               
SETAFLN  NTR1                                                                   
*                                                                               
SETA2    DS    0H                                                               
         MVC   WKNC,NDATTNOD       ATTACHED NODE AND CODE                       
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BNE   SETAFLX                                                          
*                                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IO AREA                              
         BAS   RE,GET                                                           
         MVI   EXLATTSW,C'Y'       SET EXTRACTING FOR ATTACH                    
         GOTO1 AEXLEV              EXTRACT DATA                                 
         MVI   EXLATTSW,C'N'       SET OFF                                      
         CLI   EXLNEWA,C'Y'        TEST ANOTHER ATTACH                          
         BE    SETA2               READ IT                                      
         MVC   NDAREC,NDIOA        RESTORE TO  NORMAL IO AREA                   
*                                                                               
SETAFLX  DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
CHKREC   DS    0H                  MAY NEED TO READ OR RE-READ RECORD           
         L     RF,NDAREC           TEST KEY OF REC IN NDAREC                    
         LH    R1,NDKLENM1         VS KEY IN LEVEL TABLE                        
         EX    R1,CHKRCLC                                                       
         BER   RE                                                               
         MVC   KEY,NDLVKEY         READ RECORD                                  
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      SET DISK ADDR                                
         LR    R0,RE                                                            
         BAS   RE,GET                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CHKRCLC  CLC   NDLVKEY(0),0(RF)                                                 
         EJECT                                                                  
*               CPYREC-  COPY RECORD JUST READ WITH NEW NODES                   
         SPACE 2                                                                
CPYREC   NTR1                                                                   
         BAS   RE,MOVR1            MOVE TO IOA2                                 
         MVC   NDAREC,NDIOA2                                                    
         MVI   ELCODE,X'B4'        IF HAVE ATTACH ELEM                          
         BAS   RE,NEXTELF                                                       
         BE    CPYR6               DONT SET NEW NODE                            
*                                                                               
         MVI   ELCODE,X'B1'        FIRST FIND CURRENT NODE ELEM                 
         BAS   RE,NEXTELF                                                       
         BNE   CPYR6               NONE- OK                                     
*                                                                               
         USING NDNODEL,R2                                                       
         L     RF,NDHINOD          BUMP HI NODE                                 
         LR    R5,RF               SAVE IT                                      
         LA    RF,1(RF)                                                         
         ST    RF,NDHINOD                                                       
         BAS   RE,MOVNOD           ALIGN IN XW                                  
         MVC   NDNODE,XW           SET IN NDNODEL                               
*                                                                               
         CLI   NDUPMAST,C'Y'       TEST TO UPDATE MASTER                        
         BNE   CPYR4               FOR EACH NEW NODE USED                       
         MVC   NDAREC,NDIOA        IOAREA 1 OK TO USE                           
         MVC   SAVNOD,NDHINOD      DON'T LET GETNOD DISTURB MY NODE             
         BAS   RE,GETNOD                                                        
         MVC   NDHINOD,SAVNOD                                                   
         BAS   RE,SETNODM                                                       
         MVC   NDAREC,NDIOA2       POINT BACK TO IOA 2                          
*                                                                               
CPYR4    DS    0H                                                               
         ST    R5,NDHINOD          RESTORE HIGH NODE                            
*                                                                               
CPYR6    DS    0H                  SET NEW NODE IN KEY                          
         ZIC   R2,NDNDPOS          START OF NODE                                
         A     R2,NDAREC                                                        
         ZIC   RF,NDNODLN                                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NDLVCPYN                                                 
*                                  SET NEW BACK ELEM                            
         LR    R3,R9                                                            
         SH    R3,=Y(NDLVTABL)             SET NODE AND CODE                    
         MVC   WKCOD,NDLVCOD-NDLVTABD(R3)  FROM HIGHER LEVEL                    
         MVC   WKNOD,NDLVCPYN-NDLVTABD(R3) (USE 'COPY' NODE)                    
         BAS   RE,SETBL                                                         
         BE    *+6                                                              
         DC    H'0'                RECORD OVERFLOW - MUST ABEND                 
*                                                                               
         BAS   RE,ADD              ADD NEW RECORD                               
         MVC   NDAREC,NDIOA        RESTORE IOREA                                
         L     RF,DMCB+8                                                        
         MVC   NDLVDA,0(RF)        SET DISK ADDRESS                             
         MVC   NDLVDA2,0(RF)       ALSO 'ATTACH' DA                             
         MVI   NDMODE,NDLOOK       LET USER LOOK AT RECORD                      
         BAS   RE,HOOK                                                          
*                                                                               
*                                                                               
CPYRX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*        GETSUBS- GET ANY SUB-RECORDS                                           
         SPACE 2                                                                
GETSUBS  DS    0H                                                               
         CLI   NDSUBRSW,C'Y'       TEST NEEDED                                  
         BNE   GSUB2                                                            
         CLI   NDSUBKLN,0          MUST HAVE SUB-KEY LENGTH                     
         BNE   GSUBN                                                            
*                                                                               
GSUB2    DS    0H                                                               
         LTR   RE,RE               SET CC NOT ZERO                              
         BR    RE                  TO CONTINUE                                  
*                                                                               
GSUBN    NTR1                                                                   
         MVC   KEY,NDLVKEY         RESET SEQUENTIAL                             
         BAS   RE,HIGH                                                          
*                                                                               
GSUB4    DS    0H                                                               
         BAS   RE,SEQ                                                           
         BAS   RE,KEYCHKC          CHECK THRU CODE                              
         BNE   GSUBX                                                            
         ZIC   RE,NDSKPOS          START OF SUB-KEY                             
         LA    RE,KEY(RE)                                                       
         ZIC   RF,NDSUBKLN         LENGTH OF SUB-KEY                            
         BCTR  RF,R0                                                            
         EX    RF,GSUBCLO          TEST VS LO FILTER                            
         BL    GSUB4                                                            
         OC    NDSUBRHI,NDSUBRHI   AND HI                                       
         BZ    GSUB8                                                            
         EX    RF,GSUBCHI                                                       
         BH    GSUB4                                                            
*                                                                               
GSUB8    DS    0H                                                               
         BAS   RE,GET                                                           
         CLC   =C'COPY',COMMAND    TEST COPYING                                 
         BNE   GSUB12                                                           
*                                                                               
         MVC   SAVKEY,KEY                                                       
         ZIC   RE,NDNDPOS          START OF NODE                                
         A     RE,NDIOA                                                         
         ZIC   RF,NDNODLN          LENGTH OF NODE                               
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),NDLVCPYN    MOVE IN COPY NODE                            
         BAS   RE,ADD                                                           
         MVC   KEY,SAVKEY                                                       
         BAS   RE,HIGH                                                          
         B     GSUB4                                                            
*                                                                               
GSUB12   DS    0H                                                               
         BAS   RE,PARSUB           DO ANY PARAMATER SUBSTITUTION                
         MVI   NDMODE,NDPROCSR                                                  
         BAS   RE,LVTRACE                                                       
         BAS   RE,HOOK             NB- HOOK SETS CC                             
         BNE   GSUB4               CONTINUE                                     
*                                                                               
GSUBX    DS    0H                  NB- CC ZERO MEANS CALLER                     
         B     XIT                     SAYS TO QUIT                             
         SPACE 2                                                                
GSUBCLO  CLC   0(0,RE),NDSUBRLO                                                 
GSUBCHI  CLC   0(0,RE),NDSUBRHI                                                 
         EJECT                                                                  
*        SETPOS POSITIONING LOGIC                                               
         SPACE 2                                                                
SETPOS   NTR1                                                                   
         CLC   NDLIBPRE,INPT       SKIP SETPOS FOR                              
         BNE   SP06                                                             
         CLI   NDLEV,1             LIBRARY STARTS                               
         BE    SPX                                                              
*                                                                               
SP06     DS    0H                                                               
         CLI   NDUSEQ,C'Y'         IF USER SEQS NOT SUPPORTED                   
         BE    SP08                                                             
         OC    PARAM4,PARAM4       NO POSITIONING REQ ALLOWED                   
         BZ    SPX                                                              
         CLI   SPMODE,C'R'         BUT RENAMES USE PARAM4 ALSO                  
         BE    SPX                                                              
         MVI   NDERR,NDPMDERR                                                   
         B     SPX                                                              
*                                  SET POSITIONING CODE                         
SP08     DS    0H                                                               
         CLI   SPMODE,C'R'         FOR RENAMES SKIP PARAM4 EDIT                 
         BE    SPRENM                                                           
         OC    PARAM4,PARAM4       IF NO POSITION CODE GIVEN                    
         BNZ   *+12                                                             
         MVI   POS,X'FF'           ADD AT END                                   
         B     SP08F                                                            
*                                                                               
         MVC   BASW,PARAM2         BEFORE/AFTER SW                              
         L     R4,PARAM4           A(POSITION CODE)                             
         MVC   POS,NULLS                                                        
         ZIC   R5,PARAM4           LENGTH OF CODE                               
         LTR   R5,R5                                                            
         BNP   SP08F                                                            
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   POS(0),0(R4)        SET CODE IN POS                              
         CLC   POS,NULLS                                                        
         BE    SP08F                                                            
*                                                                               
         MVC   WKCOD,POS           MAKE SURE POSITIONING CODE                   
         MVC   WKNOD,NDLVNOD       EXISTS                                       
         MVC   SVDMBTS,DMINBTS     SAVE DMINBTS AND DMOUTBTS                    
         MVI   DMINBTS,0           NO DELETES                                   
         MVI   DMOUTBTS,X'FF'                                                   
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         MVC   DMINBTS(2),SVDMBTS  RESTORE BITS                                 
         BAS   RE,KEYCHK                                                        
         BE    *+12                                                             
         MVI   NDERR,NDPMDERR      POSITION CODE NOT FOUND                      
         B     SPX                                                              
*                                                                               
*                                                                               
SP08F    DS    0H                  SPECIAL CHECKS FOR SEQ CHANGE                
         CLI   MOVSW,C'Y'          FOR SEQ CHANGE                               
         BNE   SP09                                                             
         CLC   NDLVCOD,POS         CODE MUST NOT = POS                          
         BE    SP08P                                                            
*                                                                               
         CLI   POS,X'FF'           IF ADDING AT END                             
         BNE   *+18                                                             
         CLC   NDLVCOD,NDLVLAST    THEN CODE SHOULD NOT = LAST                  
         BE    SP08P                                                            
         B     SP09                                                             
*                                                                               
         CLC   POS,NULLS           IF ADDING AT START                           
         BH    *+18                                                             
         CLC   NDLVCOD,NDLVFRST    THEN CODE SHOULD NOT = FIRST                 
         BE    SP08P                                                            
         B     SP09                                                             
*                                                                               
         CLI   BASW,C'A'           IF ADDING AFTER                              
         BNE   *+18                                                             
         CLC   NDLVBACK,POS        THEN BACK SHOULD NOT = POS                   
         BE    SP08P                                                            
         B     SP09                                                             
*                                                                               
         CLI   BASW,C'B'           IF ADDING BEFORE                             
         BNE   SP09                                                             
         CLC   NDLVFWRD,POS        THEN FWRD SHOULD NOT = POS                   
         BNE   SP09                                                             
*                                                                               
SP08P    DS    0H                                                               
         MVI   NDERR,NDPMDERR      INVALID POSITIONING                          
         B     SPX                                                              
*                                                                               
SP09     DS    0H                                                               
         CLI   SPMODE,C'D'                                                      
         BE    SPDEL                                                            
         EJECT                                                                  
*        SPADD - ADD POSITIONING                                                
         SPACE 2                                                                
SPADD    DS    0H                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IOAREA                               
         CLC   POS,NULLS           ADDING AT START                              
         BNH   SP20                                                             
         CLI   POS,X'FF'                                                        
         BE    SP30                ADDING AT END                                
         CLI   BASW,C'A'           ADDING AFTER                                 
         BE    SP40                                                             
         CLI   BASW,C'B'           ADDING BEFORE                                
         BE    SP50                                                             
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*                                  ADD AT START                                 
*                                  ------------                                 
SP20     DS    0H                                                               
*                                  FIND CURRENT FRST AND SET BACK LINK          
         MVC   WKNOD,NDLVNOD                                                    
         MVC   WKCOD,NDLVFRST                                                   
         CLC   WKCOD,NULLS                                                      
         BNH   SP22                NO FRST PRESENT                              
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                FRST NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,FNDBACK                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT FIND BACK                         
*                                                                               
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETBACK          SET NEW BACK                                 
         BAS   RE,PUT                                                           
*                                                                               
SP22     DS    0H                  SET NEW FRST CODE IN HIGHER LEVEL            
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,INCNDC           INCREMENT COUNT                              
         BAS   RE,DELFRST                                                       
         MVC   WKCOD,NDLVCOD+NDLVTABL                                           
         BAS   RE,SETFRST                                                       
*                                                                               
         BAS   RE,FNDLAST          SEE IF HAVE LAST                             
         BE    SP23                YES                                          
         CLC   NDLVFRST+NDLVTABL,NULLS  NO- MUST NOT HAVE HAD FRST              
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   WKCOD,NDLVCOD+NDLVTABL   SET LAST = NEW                          
         BAS   RE,SETLAST                                                       
         MVC   NDLVLAST+NDLVTABL,WKCOD                                          
         B     SP23D                                                            
*                                                                               
SP23     DS    0H                                                               
         CLC   NDLVFRST+NDLVTABL,NULLS    IF HAVE LAST MUST                     
         BH    *+6                        HAVE HAD FRST                         
         DC    H'0'                                                             
*                                                                               
SP23D    DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RESET LEVEL POINTER                          
*                                                                               
         MVC   NDAREC,NDIOA        RESET TO USE IOAREA 1                        
         CLI   ADDSW,C'Y'          UNLESS ADDING                                
         BE    SP23F               MUST READ RECORD NOW                         
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      SET DISK ADDRESS                             
         BAS   RE,GET                                                           
*                                                                               
SP23F    DS    0H                  SET FWRD LINK ELEM IN NEW RECORD             
         BAS   RE,DELFWRD          FIRST DELETE ANY FWRD                        
         BAS   RE,DELBACK          AND BACK                                     
         CLC   NDLVFRST,NULLS      ADD FWRD ONLY IF HAD FRST                    
         BNH   SP24                                                             
         MVC   WKCOD,NDLVFRST                                                   
         BAS   RE,SETFWRD                                                       
*                                                                               
SP24     DS    0H                                                               
         MVC   NDLVFRST,NDLVCOD    NEW IS FRST                                  
         B     SPX                                                              
         EJECT                                                                  
*                                  ADD AT END                                   
*                                  ----------                                   
SP30     DS    0H                                                               
*                                  FIND CURRENT LAST AND SET FWRD LINK          
         MVC   WKNOD,NDLVNOD                                                    
         MVC   WKCOD,NDLVLAST                                                   
         CLC   WKCOD,NULLS                                                      
         BNH   SP32                NO LAST PRESENT                              
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                LAST NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,FNDFWRD                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT FIND FWRD                         
*                                                                               
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETFWRD          SET NEW FWRD                                 
         BAS   RE,PUT                                                           
*                                                                               
SP32     DS    0H                  SET NEW LAST CODE IN HIGHER LEVEL            
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,INCNDC           INCREMENT COUNT                              
         BAS   RE,DELLAST                                                       
         MVC   WKCOD,NDLVCOD+NDLVTABL                                           
         BAS   RE,SETLAST                                                       
*                                                                               
         BAS   RE,FNDFRST          SEE IF HAVE FRST                             
         BE    SP33                YES                                          
         CLC   NDLVLAST+NDLVTABL,NULLS NO- MUST NOT HAVE HAD LAST               
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   WKCOD,NDLVCOD+NDLVTABL   SET FRST = NEW                          
         BAS   RE,SETFRST                                                       
         MVC   NDLVFRST+NDLVTABL,WKCOD                                          
         B     SP33D                                                            
*                                                                               
SP33     DS    0H                                                               
         CLC   NDLVLAST+NDLVTABL,NULLS   IF HAVE FRST MUST                      
         BH    *+6                       HAVE HAD LAST                          
         DC    H'0'                                                             
*                                                                               
SP33D    DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RESET LEVEL POINTER                          
*                                                                               
         MVC   NDAREC,NDIOA        RESET TO USE IOAREA 1                        
         CLI   ADDSW,C'Y'          UNLESS ADDING                                
         BE    SP33F               MUST READ RECORD NOW                         
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      SET DISK ADDRESS                             
         BAS   RE,GET                                                           
*                                                                               
SP33F    DS    0H                  SET BACK LINK ELEM IN NEW RECORD             
         BAS   RE,DELBACK          FIRST DELETE ANY BACK                        
         BAS   RE,DELFWRD          AND FWRD                                     
         CLC   NDLVLAST,NULLS      AND BACK ONLY IF HAD LAST                    
         BNH   SP34                                                             
         MVC   WKCOD,NDLVLAST                                                   
         BAS   RE,SETBACK                                                       
*                                                                               
SP34     DS    0H                                                               
         MVC   NDLVLAST,NDLVCOD    NEW IS LAST                                  
         B     SPX                                                              
         EJECT                                                                  
*                                  ADD AFTER                                    
*                                  ---------                                    
SP40     DS    0H                                                               
         CLC   POS,NDLVLAST        IF POS=LAST, ADD AT END                      
         BE    SP30                                                             
*                                                                               
         CLC   =C'MOVE',COMMAND     UNLESS CHANGE OF SEQUENCE                   
         BE    SP42                                                             
         BAS   RE,SPHILV           READ HIGHER LEVEL REC                        
         BAS   RE,INCNDC           INCREMENT COUNT                              
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
*                                                                               
SP42     DS    0H                                                               
         MVC   WKCOD,POS           READ POS TO SET NEW FWRD                     
         MVC   WKNOD,NDLVNOD       EXISTS                                       
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+12                                                             
         MVI   NDERR,NDPMDERR      POSITION CODE NOT FOUND                      
         B     SPX                                                              
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,FNDFWRD          OLD FRWD                                     
         MVC   NDLVFWRD,WKCOD      SAVE OLD FWRD                                
         BAS   RE,DELFWRD                                                       
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETFWRD                                                       
         BAS   RE,PUT                                                           
*                                                                               
         MVC   WKCOD,NDLVFWRD     READ OLD FWRD TO SET BACK LINK                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                OLD FWRD NOT FOUND                           
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELBACK                                                       
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETBACK                                                       
         BAS   RE,PUT                                                           
*                                                                               
*                                  NOW ADD LINKS IN NEW RECORD                  
         MVC   NDAREC,NDIOA        RESTORE TO IOAREA 1                          
         CLI   ADDSW,C'Y'          UNLESS ADDING                                
         BE    SP43F               MUST READ RECORD NOW                         
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      SET DISK ADDRESS                             
         BAS   RE,GET                                                           
*                                                                               
SP43F    DS    0H                                                               
         MVC   WKCOD,POS           BACK = POS                                   
         BAS   RE,DELBACK                                                       
         BAS   RE,SETBACK                                                       
*                                                                               
         MVC   WKCOD,NDLVFWRD      OLD FWRD IS NEW FWRD                         
         BAS   RE,DELFWRD                                                       
         BAS   RE,SETFWRD                                                       
*                                                                               
         B     SPX                                                              
         EJECT                                                                  
*                                  ADD BEFORE                                   
*                                  ----------                                   
SP50     DS    0H                                                               
         CLC   POS,NDLVFRST        IF POS=FRST, ADD AT START                    
         BE    SP20                                                             
*                                                                               
         CLC   =C'MOVE',COMMAND     UNLESS CHANGE OF SEQUENCE                   
         BE    SP52                                                             
         BAS   RE,SPHILV           READ HIGHER LEVEL REC                        
         BAS   RE,INCNDC           INCREMENT COUNT                              
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
*                                                                               
SP52     DS    0H                                                               
         MVC   WKCOD,POS           READ POS TO SET NEW BACK                     
         MVC   WKNOD,NDLVNOD                                                    
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+12                                                             
         MVI   NDERR,NDPMDERR      POSITION CODE NOT FOUND                      
         B     SPX                                                              
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,FNDBACK          OLD BACK                                     
         MVC   NDLVBACK,WKCOD      SAVE OLD BACK                                
         BAS   RE,DELBACK                                                       
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETBACK                                                       
         BAS   RE,PUT                                                           
*                                                                               
         MVC   WKCOD,NDLVBACK     READ OLD BACK TO SET FWRD LINK                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                OLD BACK NOT FOUND                           
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELFWRD                                                       
         MVC   WKCOD,NDLVCOD                                                    
         BAS   RE,SETFWRD                                                       
         BAS   RE,PUT                                                           
*                                                                               
*                                  NOW ADD LINKS IN NEW RECORD                  
         MVC   NDAREC,NDIOA        RESTORE TO IOAREA 1                          
         CLI   ADDSW,C'Y'          UNLESS ADDING                                
         BE    SP53F               MUST READ RECORD NOW                         
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      SET DISK ADDRESS                             
         BAS   RE,GET                                                           
*                                                                               
SP53F    DS    0H                                                               
         MVC   WKCOD,POS           FWRD = POS                                   
         BAS   RE,DELFWRD                                                       
         BAS   RE,SETFWRD                                                       
*                                                                               
         MVC   WKCOD,NDLVBACK      OLD BACK IS NEW BACK                         
         BAS   RE,DELBACK                                                       
         BAS   RE,SETBACK                                                       
*                                                                               
SPX      DS    0H                                                               
         CLI   NDERR,0                                                          
         B     XIT                                                              
         EJECT                                                                  
*        SPDEL - DELETE POSITIONING                                             
         SPACE 2                                                                
SPDEL    DS    0H                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IOAREA                               
*                                                                               
*                                  TEST CODE=FRST                               
*                                  --------------                               
         CLC   NDLVFRST,NDLVCOD                                                 
         BNE   DP10                                                             
*                                  READ HIGHER LEVEL TO SET NEW FRST            
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,DECNDC           DECREMENT COUNT                              
         BAS   RE,DELFRST          GET RID OF CURRENT FRST                      
         XC    WKCOD,WKCOD                                                      
         CLC   NDLVFWRD+NDLVTABL,NULLS  TEST ANY FWRD                           
         BH    *+12                                                             
         BAS   RE,DELLAST          IF NOT-GET RID OF LAST ALSO                  
         B     DP6                 (NO LOWER RECORDS LEFT)                      
*                                                                               
         MVC   WKCOD,NDLVFWRD+NDLVTABL   SET NEW FRST                           
         BAS   RE,SETFRST                = CURRENT FWRD                         
*                                                                               
DP6      DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
         MVC   NDLVFRST,WKCOD      SET NEW FRST (OR CLEAR)                      
         MVC   WKNOD,NDLVNOD                                                    
         CLC   NDLVFWRD,NULLS      IF NOT AT END                                
         BNH   *+8                                                              
         BAS   RE,DPFWRD           REMOVE BACK LINK FROM FWRD REC               
         B     DP80                                                             
*                                                                               
*                                  TEST CODE=LAST                               
*                                  --------------                               
DP10     DS    0H                                                               
         CLC   NDLVLAST,NDLVCOD                                                 
         BNE   DP20                                                             
*                                  READ HIGHER LEVEL TO SET NEW LAST            
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,DECNDC           DECREMENT COUNT                              
         BAS   RE,DELLAST          GET RID OF CURRENT LAST                      
         XC    WKCOD,WKCOD                                                      
         CLC   NDLVBACK+NDLVTABL,NULLS  TEST ANY BACK                           
         BH    *+12                                                             
         BAS   RE,DELFRST          IF NOT-GET RID OF FRST ALSO                  
         B     DP6                 (NO LOWER RECORDS LEFT)                      
*                                                                               
         MVC   WKCOD,NDLVBACK+NDLVTABL   SET NEW LAST                           
         BAS   RE,SETLAST                = CURRENT BACK                         
*                                                                               
DP16     DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
         MVC   NDLVLAST,WKCOD      SET NEW LAST (OR CLEAR)                      
         MVC   WKNOD,NDLVNOD                                                    
         CLC   NDLVBACK,NULLS      IF NOT AT START                              
         BNH   *+8                                                              
         BAS   RE,DPBACK           REMOVE FWRD LINK FROM BACK REC               
         B     DP80                                                             
*                                                                               
*                                  CODE NEITHER FRST NOR LAST                   
*                                  --------------------------                   
DP20     DS    0H                                                               
*                                  REMOVE THIS LINK FROM CHAIN                  
*                                                                               
         BAS   RE,DPFWRD           REPLACE BACK LINK IN FWRD REC                
         BAS   RE,DPBACK           REPLACE FWRD LINK IN BACK REC                
*                                                                               
         CLC   =C'MOVE',COMMAND     UNLESS CHANGE OF SEQUENCE                   
         BE    DP80                                                             
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,DECNDC           DECREMENT COUNT                              
         BAS   RE,PUT                                                           
*                                                                               
DP80     DS    0H                                                               
         MVC   NDAREC,NDIOA        RESTORE IOAREA                               
         B     SPX                                                              
         SPACE 3                                                                
DPBACK   NTR1                      REPLACE FWRD LINK IN BACK RECORD             
         MVC   WKNOD,NDLVNOD                                                    
         MVC   WKCOD,NDLVBACK      READ BACK TO SET NEW FWRD                    
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                BACK NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELFWRD          GET RID OF CURRENT FWRD                      
         MVC   WKCOD,NDLVFWRD      AND SET NEW FWRD                             
         CLC   NDLVFWRD,NULLS      IF NEEDED                                    
         BNH   *+8                                                              
         BAS   RE,SETFWRD                                                       
         BAS   RE,PUT                                                           
         B     XIT                                                              
         SPACE 3                                                                
DPFWRD   NTR1                      REPLACE BACK LINK IN FWRD REC                
         MVC   WKNOD,NDLVNOD                                                    
         MVC   WKCOD,NDLVFWRD      READ FWRD TO SET NEW BACK                    
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                FWRD NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELBACK          GET RID OF CURRENT BACK                      
         MVC   WKCOD,NDLVBACK      AND SET NEW BACK                             
         CLC   NDLVBACK,NULLS      IF NEEDED                                    
         BNH   *+8                                                              
         BAS   RE,SETBACK                                                       
         BAS   RE,PUT                                                           
         B     XIT                                                              
         EJECT                                                                  
*        SPRENM - RENAME POSITIONING                                            
         SPACE 2                                                                
SPRENM   DS    0H                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IOAREA                               
         CLC   NDLVBACK,NULLS      TEST ANY BACK LINK                           
         BH    RP10                YES                                          
*                                  NO- THIS MUST BE FRST                        
*                                  RESET FRST IN HIGHER LEVEL                   
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,DELFRST          GET RID OF CURRENT FRST                      
         MVC   WKCOD,NDLVCOD+NDLVTABL   SET NEW FRST                            
         BAS   RE,SETFRST                = NEW RENAME CODE                      
         MVC   NDLVFRST+NDLVTABL,WKCOD      SET NEW FRST                        
*                                                                               
         CLC   NDLVFWRD+NDLVTABL,NULLS  TEST ANY FWRD                           
         BH    RP6                 YES                                          
         BAS   RE,DELLAST          IF NOT-SET NEW LAST ALSO                     
         BAS   RE,SETLAST          (RENAMED RECORD IS ONLY ONE)                 
         MVC   NDLVLAST+NDLVTABL,WKCOD      SET NEW LAST                        
         BAS   RE,PUT                                                           
         B     SPX                                                              
*                                                                               
RP6      DS    0H                                                               
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
         B     RP20                                                             
*                                                                               
RP10     DS    0H                  HAVE BACK LINK                               
         MVC   WKCOD,NDLVBACK      READ BACK TO SET NEW FWRD                    
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                BACK NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELFWRD          GET RID OF CURRENT FWRD                      
         MVC   WKCOD,NDLVCOD       AND SET NEW FWRD=RENAME CODE                 
         BAS   RE,SETFWRD                                                       
         BAS   RE,PUT                                                           
*                                                                               
*                                                                               
         CLC   NDLVFWRD,NULLS      TEST HAVE FWRD LINK                          
         BH    RP20                YES                                          
*                                  NO- THIS MUST BE LAST                        
*                                  RESET LAST IN HIGHER LEVEL                   
         BAS   RE,SPHILV           READ HIGHER LEVEL RECORD                     
         BAS   RE,DELLAST          GET RID OF CURRENT LAST                      
         MVC   WKCOD,NDLVCOD+NDLVTABL   SET NEW LAST                            
         BAS   RE,SETLAST                = NEW RENAME CODE                      
         BAS   RE,PUT                                                           
         LA    R9,NDLVTABL(R9)     RETURN TO CURRENT LEVEL                      
         MVC   NDLVLAST,WKCOD      SET NEW LAST                                 
         B     SPX                                                              
*                                                                               
RP20     DS    0H                  HAVE FWRD LINK                               
         MVC   WKNOD,NDLVNOD                                                    
         MVC   WKCOD,NDLVFWRD      READ FWRD TO SET NEW BACK                    
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+6                                                              
         DC    H'0'                FWRD NOT FOUND                               
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,DELBACK          GET RID OF CURRENT BACK                      
         MVC   WKCOD,NDLVCOD       AND SET NEW BACK=RENAME CODE                 
         BAS   RE,SETBACK                                                       
         BAS   RE,PUT                                                           
         B     SPX                                                              
*                                                                               
         EJECT                                                                  
*                                  VARIOUS POSIITONING ROUTINES                 
         SPACE 2                                                                
FNDFRST  DS    0H                                                               
         MVI   ELCODE,X'B6'        FRST ELEM                                    
         B     FNDALL                                                           
         SPACE 2                                                                
FNDLAST  DS    0H                                                               
         MVI   ELCODE,X'B7'        LAST ELEM                                    
         B     FNDALL                                                           
         SPACE 2                                                                
FNDFWRD  DS    0H                                                               
         MVI   ELCODE,X'B8'        FWRD ELEM                                    
         B     FNDALL                                                           
         SPACE 2                                                                
FNDBACK  DS    0H                                                               
         MVI   ELCODE,X'B9'        BACK ELEM                                    
         SPACE 2                                                                
FNDALL   DS    0H                                                               
         MVC   WKCOD,NULLS                                                      
         LR    RF,RE                                                            
         BAS   RE,NEXTELF                                                       
         BNE   FNDALL4                                                          
*                                                                               
         ZIC   R1,1(R2)            MOVE TO WKCOD                                
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WKCOD(0),2(R2)                                                   
         SR    R1,R1               SET CC                                       
*                                                                               
FNDALL4  DS    0H                                                               
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
DELFRST  DS    0H                                                               
         MVI   ELCODE,X'B6'        FRST ELEM                                    
         B     DELALL                                                           
         SPACE 2                                                                
DELLAST  DS    0H                                                               
         MVI   ELCODE,X'B7'        LAST ELEM                                    
         B     DELALL                                                           
         SPACE 2                                                                
DELFWRD  DS    0H                                                               
         MVI   ELCODE,X'B8'        FWRD ELEM                                    
         B     DELALL                                                           
         SPACE 2                                                                
DELBACK  DS    0H                                                               
         MVI   ELCODE,X'B9'        BACK ELEM                                    
         SPACE 2                                                                
DELALL   DS    0H                                                               
         LR    RF,RE                                                            
         BAS   RE,DELEL                                                         
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
SETFRST  DS    0H                                                               
         MVI   WORK,X'B6'          FRST ELEM                                    
         B     SETALL                                                           
         SPACE 2                                                                
SETLAST  DS    0H                                                               
         MVI   WORK,X'B7'          LAST ELEM                                    
         B     SETALL                                                           
         SPACE 2                                                                
SETFWRD  DS    0H                                                               
         MVI   WORK,X'B8'          FWRD ELEM                                    
         B     SETALL                                                           
         SPACE 2                                                                
SETBACK  DS    0H                                                               
         MVI   WORK,X'B9'          BACK ELEM                                    
         SPACE 2                                                                
SETALL   NTR1                                                                   
         MVC   WORK+2(NDCODL),WKCOD                                             
         LA    RF,WORK+1+NDCODL    GET LENGTH                                   
         CLI   0(RF),0                                                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,WORK-1                                                        
         SR    RF,R0                                                            
         STC   RF,WORK+1           SET ELEM LENGTH                              
         SR    R2,R2               ADD AT END                                   
         BAS   RE,ADDEL                                                         
         B     XIT                                                              
         SPACE 2                                                                
*        READ HIGHER LEVEL RECORD                                               
         SPACE 2                                                                
SPHILV   DS    0H                                                               
         LR    R0,RE                                                            
         SH    R9,=Y(NDLVTABL)     BACK UP ONE LEVEL                            
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA2     DISK ADDRESS OF HIGHER LEVEL                 
         BAS   RE,GET                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
INCNDC   DS    0H                  INCREMENT NODE COUNT                         
         LA    R1,1                                                             
         B     FIXNDC                                                           
*                                                                               
DECNDC   DS    0H                  DECREMENT NODE COUNT                         
         LA    R1,1                                                             
         LCR   R1,R1                                                            
*                                                                               
FIXNDC   DS    0H                  ADJUST COUNT AT HIGHER LEVEL                 
         CLC   =C'MOVE',COMMAND    SKIP IF CHANGE OF SEQ                        
         BER   RE                                                               
         MVI   ELCODE,X'B1'        FIND NODE ELEM                               
         LR    RF,RE                                                            
         BAS   RE,NEXTELF                                                       
         BNE   FIXNDC2                                                          
*                                                                               
         USING NDNODEL,R2                                                       
         SR    RE,RE               INC/DEC COUNT                                
         ICM   RE,3,NDNCOUNT                                                    
         AR    RE,R1               +/- 1                                        
         STCM  RE,3,NDNCOUNT                                                    
*                                                                               
FIXNDC2  DS    0H                                                               
         LR    RE,RF                                                            
         BR    RE                  RETURN WITH CC SET                           
         EJECT                                                                  
*        PARSE - SEPARATE KEY INTO LEVEL TABLE COMPONENTS                       
*                (R3 AT CURRENT POSITION IN 'KEY')                              
*                                                                               
*                ONE LEVEL FOR EACH CALL                                        
*                FILE IS READ AND CONTROL DATA EXTRACTED                        
         SPACE 2                                                                
PARSE    NTR1                                                                   
         SPACE 1                                                                
         NI    NDLVSTAT,X'BF'      SET OFF NEW STAT                             
*                                                                               
         SR    R5,R5               FOR BYTE COUNTER                             
         LR    R2,R3               SAVE START OF LEVEL                          
*                                                                               
PRS4     DS    0H                                                               
         CLI   0(R3),C' '          END                                          
         BH    PRS5                                                             
*                                                                               
         LA    R0,INPT             AT END SET LENGTH                            
         LR    RF,R3                                                            
         SR    RF,R0                                                            
         STC   RF,NDCKEYL                                                       
         B     PRS8                                                             
*                                                                               
PRS5     DS    0H                                                               
         CLC   0(1,R3),NDDELIM     DELIMITER                                    
         BE    PRS8                                                             
*                                                                               
         LA    R3,1(R3)            BUMP POSITION                                
         B     PRS4                                                             
*                                                                               
PRS8     DS    0H                                                               
         ZIC   R6,NDLEV                                                         
         LA    R6,1(R6)            BUMP LEVEL COUNTER                           
         STC   R6,NDLEV                                                         
         CLC   NDLEV,NDMXLEVS      TEST VS MAX                                  
         BNH   *+12                                                             
         MVI   NDERR,NDLEVERR                                                   
         B     PRSX                                                             
*                                                                               
         LR    R5,R3                                                            
         SR    R5,R2               LENGTH                                       
         BP    *+12                                                             
*                                                                               
PRS8B    DS    0H                                                               
         MVI   NDERR,NDCDLERR      CODE LENGTH ERROR                            
         B     PRSX                                                             
*                                                                               
         ZIC   RE,NDCODLN          TEST VS MAX                                  
         CR    R5,RE                                                            
         BH    PRS8B                                                            
*                                                                               
         STC   R6,NDLEV            SET LEVEL                                    
         MVC   XW(NDCODL),NULLS                                                 
         BCTR  R5,R0                                                            
         EX    R5,PRSMVC          MOVE CODE TO XW                               
*                                                                               
         CLC   XW(1),NDLIBPRE    TEST LIBRARY START                             
         BNE   PRS8B2                                                           
         CLI   NDLEV,1             MUST BE AT LEVEL ONE                         
         BE    PRS8B2                                                           
         MVI   NDERR,NDLIBERR                                                   
         B     PRSX                                                             
*                                                                               
PRS8B2   DS    0H                                                               
         TM    NDLVSTAT,X'20'      TEST FORCE READ                              
         BNZ   PRS8C                                                            
*                                                                               
         CLI   0(R3),C' '          ALWAYS TREAT LAST AS NEW                     
         BNH   PRS8C                                                            
*                                                                               
         CLI   RERDSW,C'Y'         TEST FORCE REREAD                            
         BE    PRS8C                                                            
*                                                                               
         OC    NDLVNOD,NDLVNOD     IF HAVE NODE                                 
         BZ    PRS8C                                                            
         CLC   NDLVCOD,XW          TEST VS CURRENT CODE                         
         BE    PRS12                                                            
*                                                                               
PRS8C    DS    0H                  MUST GET RECORD FOR THIS LEVEL               
         MVI   RERDSW,C'Y'         FORCE SUBSEQUENT READS                       
         OI    NDLVSTAT,X'40'      SET NEW THIS TIME                            
         NI    NDLVSTAT,X'DF'      SET OFF FORCE READ                           
         MVC   NDLVCOD,XW                                                       
*                                                                               
         OC    NDLVNOD,NDLVNOD     MUST HAVE NODE                               
         BZ    PRS9                                                             
*                                                                               
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    PRS10               RECORD FOUND                                 
*                                                                               
         CLI   0(R3),C' '          IF AT END                                    
         BH    PRS9                                                             
         CLC   =C'HIGH',COMMAND    FOR HIGH KEY NEED MATCH                      
         BNE   PRS9                                                             
         BAS   RE,KEYCHKN          ONLY THRU NODE                               
         BNE   PRS9                                                             
*                                  SET ACTUAL KEY FOUND IN NDLVCOD              
         ZIC   RF,NDNDPOS                                                       
         ZIC   R0,NDNODLN                                                       
         AR    RF,R0                                                            
         LA    R2,KEY(RF)          START OF CODE                                
         MVC   NDLVCOD,NULLS                                                    
         CLI   NDCDALIN,C'R'                                                    
         BE    PRS8E                                                            
         ZIC   RF,NDCODLN          FOR LEFT ALIGNED JUST MOVE                   
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     PRS8L                                                            
         MVC   NDLVCOD(0),0(R2)                                                 
*                                                                               
PRS8E    DS    0H                                                               
         LA    RE,NDLVCOD          RIGHT ALIGNMENT                              
         ZIC   R0,NDCODLN                                                       
         SR    R1,R1                                                            
*                                                                               
PRS8F    DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   PRS8H                                                            
*                                                                               
         LTR   R1,R1               0 IS END IF HAVE                             
         BP    PRS8L               FOUND SIGNIFICANT BYTES                      
         B     PRS8J               ELSE IGNORE (FOR RIGHT ALIGNED)              
*                                                                               
PRS8H    DS    0H                                                               
         MVC   0(1,RE),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
PRS8J    DS    0H                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,PRS8F                                                         
*                                                                               
PRS8L    DS    0H                                                               
         B     PRS10                                                            
*                                                                               
PRS9     DS    0H                                                               
         MVI   NDERR,NDRNFERR      RECORD NOT FOUND                             
         MVC   NDLVCOD,NULLS       CLEAR LEVEL ENTRY                            
         B     PRSX                                                             
*                                                                               
PRS10    DS    0H                                                               
         LA    RF,KEY              DO NOT GET RENAMED/REPO'D RECORDS            
         AH    RF,NDKLEN                                                        
         TM    0(RF),X'30'         NB- WAS X'80' (WHY?) GEP 2/14/86             
         BNZ   PRS12                                                            
         BAS   RE,GET                                                           
         GOTO1 AEXLEV              EXTRACT DATA                                 
         CLC   =C'PUT',COMMAND     FOR PUT, ATTACHES NOT NEEDED                 
         BE    PRS12               (ALSO THEY CLOBBER IOA2)                     
         BAS   RE,SETAFL           SET ATTACHMENT INFORMATION                   
         BAS   RE,PARSUB           PARAMETER SUBSTITUTION                       
         BNE   PRS9                OMIT THIS RECORD                             
*                                                                               
PRS12    DS    0H                                                               
PRSX     DS    0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
PRSMVC   MVC   XW(0),0(R2)                                                      
*                                                                               
         EJECT                                                                  
*                           CONCAT - CONCATENATE KEY COMPONENTS FROM            
*                                    LEVTAB INTO INPT                           
         SPACE 2                                                                
CONCAT   NTR1                                                                   
         SPACE 2                                                                
         LA    R9,NDLVTAB+NDLVTABL  POINT TO LEVEL 1                            
         MVI   INPT,C' '           PUT KEY BACK TOGETHER                        
         MVC   INPT+1(L'INPT-1),INPT                                            
         LA    R3,INPT                                                          
         MVI   NDCKEYL,0                                                        
         SR    R6,R6                                                            
         ICM   R6,1,NDLEV            NO. OF LEVELS                              
         BZ    CONCX                                                            
         LR    R4,R3               SAVE OUTPUT START                            
*                                                                               
CONC4    DS    0H                                                               
         CLC   NDLVCOD,NULLS       NO CODE- END                                 
         BE    CONC6                                                            
         LA    RF,NDLVCOD+NDCODL-1                                              
         CLI   0(RF),0                                                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R0,NDLVCOD                                                       
         SR    RF,R0                                                            
         EX    RF,CONCMVC          SET CODE IN OUTPUT                           
*                                                                               
         LA    R3,1(RF,R3)                                                      
         MVC   0(1,R3),NDDELIM     SET DELIMITER                                
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R9,NDLVTABL(R9)     NEXT LEVEL                                   
         BCT   R6,CONC4                                                         
*                                                                               
CONC6    DS    0H                                                               
         BCTR  R3,R0                                                            
         MVI   0(R3),C' '          ERASE LAST DELIMITER                         
         SR    R3,R4                                                            
         STC   R3,NDCKEYL          SET OUTPUT KEY LENGTH                        
*                                                                               
CONCX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
CONCMVC  MVC   0(0,R3),NDLVCOD                                                  
         SPACE 3                                                                
*        SETOUT- SET OUTPUT 'KEY' IN USER AREA                                  
         SPACE 2                                                                
SETOUT   DS    0H                                                               
         OC    PARAM3,PARAM3       ONLY IF 'KEY' USED                           
         BZR   RE                                                               
*                                                                               
         NTR1                                                                   
         BAS   RE,CONCAT           CONCATENATE LEVELS                           
         L     R3,PARAM3                                                        
         ZIC   R1,NDCKEYL                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),INPT        MOVE TO USER KEY                             
         B     XIT                                                              
         EJECT                                                                  
*        GETNOD - GET NEXT AVAILABLE NODE                                       
         SPACE 2                                                                
GETNOD   NTR1                                                                   
         SPACE 1                                                                
         LA    R2,KEY              READ MASTER RECORD                           
         AH    R2,NDDISK                                                        
         LA    R3,NDLVTAB                                                       
         MVC   0(4,R2),NDLVDA-NDLVTABD(R3)  DA OF MASTER RECORD                 
         BAS   RE,GET                                                           
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT                                                      
*                                                                               
GN8      DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING MASTER CONTROL ELEM                  
*                                                                               
         CLI   0(R2),X'BF'                                                      
         BE    GN10                                                             
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GN8                                                              
*                                                                               
GN10     DS    0H                                                               
         USING NDMASTEL,R2                                                      
         MVC   NDHINOD,NDMHINOD                                                 
*                                                                               
         CLC   NDHINOD,=X'FFFFFFFF'  TEST END OF NODES                          
         BL    *+6                                                              
         DC    H'0'                                                             
*                                   INCREMENT NODE                              
         L     R5,NDHINOD                                                       
         LA    R5,1(R5)                                                         
         ST    R5,NDHINOD                                                       
*                                                                               
GNX      DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
*        SETNODM - SET NEW NODE IN MASTER                                       
         SPACE 2                                                                
SETNODM  NTR1                      RECORD MUST BE IN NDAREC                     
         SPACE 1                                                                
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT                                                      
*                                                                               
SNM8     DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING MASTER CONTROL ELEM                  
*                                                                               
         CLI   0(R2),X'BF'                                                      
         BE    SNM10                                                            
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SNM8                                                             
*                                                                               
SNM10    DS    0H                                                               
         USING NDMASTEL,R2                                                      
         MVC   NDMHINOD,NDHINOD                                                 
         BAS   RE,PUT              RE-WRITE MASTER WITH NEW NODE                
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*        SETNODH - SET NODE IN HIGHER LEVEL RECORD                              
         SPACE 2                                                                
SETNODH  NTR1                                                                   
         SPACE 1                                                                
         SH    R9,=Y(NDLVTABL)     SET NODE IN HIGHER LEVEL RECORD              
         LA    R2,KEY                                                           
         AH    R2,NDDISK                                                        
         MVC   0(4,R2),NDLVDA2                                                  
         LA    R9,NDLVTABL(R9)                                                  
         BAS   RE,GET                                                           
*                                                                               
         MVI   ELCODE,X'B1'        DELETE ANY EXISTING NODE ELEM                
         BAS   RE,DELEL                                                         
*                                                                               
         XC    WORK(10),WORK                                                    
         MVI   WORK,X'B1'                                                       
         MVI   WORK+1,10                                                        
         MVC   WORK+2(NDNODL),NDLVNOD NODE                                      
*                                                                               
         SR    R2,R2               ADD AT END                                   
         BAS   RE,ADDEL                                                         
         BNE   SNHX                ERROR                                        
         BAS   RE,PUT              RE-WRITE WITH NEW NODE                       
*                                                                               
SNHX     DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
*        MOVNOD- ALIGN NDHINOD IN XW                                            
         SPACE 2                                                                
MOVNOD   DS    0H                                                               
         XC    XW(8),XW            ALIGN NODE IN XW                             
         MVC   XW+NDNODL(NDNODL),NDHINOD                                        
         LA    RF,XW+NDNODL+NDNODL   END  OF NODE                               
         ZIC   R1,NDNODLN          NODE LENGTH                                  
         SR    RF,R1               RF IS FROM ADDRESS                           
         MVC   XW(NDNODL),0(RF)                                                 
         BR    RE                                                               
         EJECT                                                                  
*        OPEN - INITIALIZATION                                                  
         SPACE 2                                                                
OPEN     NTR1                                                                   
*                                                                               
         CLI   NDDELIM,0           SET DELIMETER                                
         BNE   *+8                                                              
         MVI   NDDELIM,C'.'                                                     
*                                  READ MASTER RECORD                           
         MVC   KEY,NDKEY           USE GIVEN MASTER KEY                         
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK           WHOLE KEY                                    
         BE    *+6                                                              
         DC    H'0'                MASTER NOT FOUND                             
*                                                                               
         MVC   NDAREC,NDIOA2       USE 2ND IOAREA                               
         BAS   RE,GET                                                           
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT         POINT TO FIRST ELEM                          
         XC    XW,XW                                                            
*                                                                               
INIT8    DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    INIT50                                                           
         CLI   0(R2),X'BF'         MASTER CONTROL ELEM                          
         BE    INIT10                                                           
*                                                                               
INIT9    DS    0H                                                               
         ZIC   R0,1(R2)            NEXT ELEM                                    
         AR    R2,R0                                                            
         B     INIT8                                                            
         SPACE 2                                                                
INIT10   DS    0H                  MASTER CONTROL ELEM                          
         USING NDMASTEL,R2                                                      
         MVC   NDHINOD,NDMHINOD    SET HIGH NODE                                
         MVC   NDNODLN,NDMNODLN    LENGTH OF NODE                               
         MVC   NDNDPOS,NDMNPOS     NODE POS                                     
         MVC   NDLIBNOD,NDMLIBND   LIBRARY NODE                                 
         MVC   NDLIBPRE,NDMLIBPR   LIBRARY PREFIX                               
         MVC   NDUSEQ,NDMUSEQ      USER SEQUENCING SUPPORTED                    
         MVC   NDCODLN,NDMCDLN     CODE LENGTH                                  
         MVC   NDCDALIN,NDMCDALN   CODE ALIGNMENT (R,L)                         
         ZIC   RF,NDNDPOS          NODE POS                                     
         ZIC   RE,NDNODLN          PLUS NOD LENGTH                              
         AR    RF,RE                                                            
         STC   RF,NDCDPOS          =CODE POSITION                               
         ZIC   RE,NDCODLN          PLUS CODE LENGTH                             
         AR    RF,RE                                                            
         STC   RF,NDSKPOS          =POSITION OF SUB-REC 'SUB-KEY'               
         LH    RE,NDKLEN           TOTAL KEY LENGTH                             
         SR    RE,RF               LESS START =                                 
         STC   RE,NDSUBKLN         SUB-REC 'SUB-KEY' LENGTH                     
*                                                                               
         LA    R9,NDLVTAB          CLEAR LEVEL TABLE                            
         SR    R0,R0                                                            
         ICM   R0,1,NDMXLEVS                                                    
         BNZ   *+6                                                              
         DC    H'0'                ZERO LEVELS                                  
*                                                                               
INIT10B  DS    0H                                                               
         XC    0(NDLVTABL,R9),0(R9)                                             
         LA    R9,NDLVTABL(R9)                                                  
         BCT   R0,INIT10B                                                       
*                                                                               
         B     INIT9                                                            
         SPACE 2                                                                
INIT50   DS    0H                                                               
*                              SET DEFAULTS NOT SPECIFICIED                     
*                              TO DEFAULT DEFAULTS                              
         LA    R9,NDLVTAB                                                       
         SR    R5,R5               FOR LEVEL COUNT                              
         ZIC   R6,NDMXLEVS                                                      
*                                                                               
INIT52   DS    0H                                                               
         MVC   NDLVKEY,NDKEY       SET KEY                                      
*                                                                               
         EDIT  (R5),(3,NDLVLOC),FILL=0  FOR DUMP READABILITY                    
         MVI   NDLVLOC,C'L'                                                     
         MVI   NDLVLOC+1,C'='                                                   
*                                                                               
         LA    R9,NDLVTABL(R9)      NEXT LEVEL                                  
         LA    R5,1(R5)                                                         
         CR    R5,R6                                                            
         BNH   INIT52                                                           
*                                                                               
         LA    R9,NDLVTAB          RESET TO START                               
         GOTO1 AEXLEV              EXTRACT DATA                                 
*                                                                               
         LA    R9,NDLVTABL(R9)                                                  
         MVC   NDL1NOD,NDLVNOD     SAVE LEVEL 1 NODE                            
         SH    R9,=Y(NDLVTABL)                                                  
*                                                                               
INITX    DS    0H                                                               
         MVC   NDAREC,NDIOA        RESTORE TO NORMAL IOA                        
         MVI   NDOPENSW,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                  SETKEY                                       
*                                  ------                                       
         SPACE 2                                                                
SETKEY   DS    0H                                                               
         MVC   KEY,NDLVKEY         START WITH TEMPLATE                          
         ZIC   RF,NDNDPOS          POSITION FOR NODE                            
         LA    RF,KEY(RF)                                                       
         ZIC   R1,NDNODLN          NODE LENGTH                                  
         ZIC   R0,NDCODLN          PLUS CODE LENGTH                             
         AR    R1,R0                                                            
         BCTR  R1,R0                                                            
         EX    R1,SKCLR            CLEAR CODE AREA                              
*                                                                               
         ZIC   R1,NDNODLN                                                       
         BCTR  R1,R0                                                            
         EX    R1,SKMVC            MOVE IN NODE                                 
*                                                                               
         OC    WKCOD,WKCOD         NO CODE                                      
         BZ    SKX                                                              
         LA    R1,WKCOD+NDCODL-1   GET LENGTH                                   
         CLI   0(R1),0                                                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,WKCOD-1                                                       
         SR    R1,R0                                                            
         CLI   NDCDALIN,C'L'       LEFT ALIGN - SIMPLY MOVE                     
         BNE   SK7                                                              
         ZIC   R0,NDNODLN                                                       
         AR    RF,R0               START OF CODE                                
         B     SK8                                                              
*                                  RIGHT ALIGN-                                 
SK7      DS    0H                                                               
         ZIC   R0,NDCODLN          MAXIMUM LENGTH                               
         AR    RF,R0               PLUS START                                   
         ZIC   R0,NDNODLN          PLUS NODE LENGTH                             
         AR    RF,R0                                                            
         SR    RF,R1               MINUS ACTUAL LEN = TO ADDRESS                
*                                                                               
SK8      DS    0H                                                               
         BCTR  R1,R0                                                            
         EX    R1,SKMVC2           MOVE CODE                                    
*                                                                               
SKX      DS    0H                                                               
         BR    RE                                                               
*                                                                               
SKCLR    XC    0(0,RF),0(RF)                                                    
SKMVC    MVC   0(0,RF),WKNOD                                                    
SKMVC2   MVC   0(0,RF),WKCOD                                                    
         SPACE 3                                                                
*                                                                               
KEYCHK   DS    0H                  KEY VS KEYSAVE                               
         LH    RF,NDKLENM1         WHOLE KEY                                    
         B     KEYCHK2                                                          
*                                                                               
KEYCHKN  DS    0H                                                               
         LH    RF,NDKCLND          KEY THROUGH NODE                             
         B     KEYCHK2                                                          
*                                                                               
KEYCHKC  DS    0H                                                               
         LH    RF,NDKCLCD          KEY THROUGH CODE                             
*                                                                               
KEYCHK2  DS    0H                                                               
         EX    RF,KCCLC                                                         
         BR    RE                                                               
*                                                                               
KCCLC    CLC   KEY(0),KEYSAVE                                                   
         SPACE 3                                                                
NEXTELF  DS    0H                  NEXTEL FIRST - START OF RECORD               
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT                                                      
         B     NEXTEL2                                                          
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ZERO LENGTH ELEM                             
         AR    R2,R0                                                            
*                                                                               
NEXTEL2  CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                  DELETE ALL ELCODE ELEMS                      
*                                  -----------------------                      
DELEL    NTR1                                                                   
         SPACE 1                                                                
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT                                                      
*                                                                               
DELEL2   DS    0H                                                               
         BAS   RE,NEXTEL2                                                       
         BNE   DELELX                                                           
*                             NOTE-4TH PARM IS ADDR OF ELEM START(2),           
*                             LENGTH DISP(2), AND MAX REC SIZE(2)               
         GOTO1 NDRECUP,DMCB,(X'FE',NDAREC),(R2),,NDELSTRT                       
         B     DELEL2                                                           
*                                                                               
DELELX   DS    0H                                                               
         B     XIT                 (= MEANS NO CHANGE)                          
         SPACE 3                                                                
*                                  ADD ELEM IN WORK AT (R2)                     
*                                  ------------------------                     
ADDEL    DS    0H                                                               
         LR    R0,RE                                                            
         LTR   R2,R2               IF R2 NOT SET                                
         BNZ   ADDEL4                                                           
         L     R2,NDAREC           ADD AT END                                   
         LR    RE,R2                                                            
         AH    RE,NDRLEN                                                        
         AH    R2,0(RE)            POINT TO EOR                                 
*                             NOTE-4TH PARM IS ADDR OF ELEM START(2),           
*                             LENGTH DISP(2), AND MAX REC SIZE(2)               
ADDEL4   DS    0H                                                               
         GOTO1 NDRECUP,DMCB,(X'FE',NDAREC),WORK,(C'R',(R2)),NDELSTRT            
         CLI   DMCB+8,C'R'                                                      
         BE    *+10                OK, RETURN WITH CC=                          
         MVI   NDERR,NDOVFERR      RECORD TOO BIG                               
         LTR   RE,RE               CC NOT =                                     
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                  MOVE FROM ONE IOA TO ANOTHER                 
*                                  ----------------------------                 
*                                                                               
MOVR1    DS    0H                  MOVE RECORD IN IOA1 TO IOA2                  
         L     RF,NDIOA2           RECIEVING                                    
         L     R0,NDIOA            SENDING                                      
         B     MOVR4                                                            
*                                                                               
MOVR2    DS    0H                  MOVE RECORD IN IOA2 TO IOA1                  
         L     RF,NDIOA                                                         
         L     R0,NDIOA2                                                        
*                                                                               
MOVR4    DS    0H                                                               
         XC    0(64,RF),0(RF)      CLEAR KEY OF RECIEVING AREA                  
         LR    R1,R0                                                            
         AH    R1,NDRLEN                                                        
         OC    0(2,R1),0(R1)       TEST ANY LENGTH                              
         BNZ   *+10                                                             
         MVC   0(2,R1),NDELSTRT    NO-SET TO ELEM START                         
         LH    R1,0(R1)            RECORD LENGTH                                
         LA    R1,1(R1)            ONE EXTRA BYTE                               
         XR    R0,RE            SAVE RE AND POINT RE TO SENDING REC             
         XR    RE,R0                                                            
         XR    R0,RE                                                            
*                                                                               
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
*        SETBL  - SET BACK LINK (TO PARENT)                                     
         SPACE 2                                                                
SETBL    NTR1                                                                   
         SPACE 1                                                                
         MVI   ELCODE,X'B5'        DELETE ANY THERE                             
         BAS   RE,DELEL                                                         
         LA    R2,WORK                                                          
         USING NDBNODEL,R2                                                      
         XC    WORK,WORK                                                        
         MVI   WORK,X'B5'                                                       
         LA    RF,NDLVTAB+NDLVTABL     POINT TO LEVEL 1                         
         CLC   NDLVCOD-NDLVTABD(1,RF),NDLIBPRE                                  
         BNE   *+8                                                              
         OI    NDBCNTL,X'80'           SET IS LIBRARY MEMBER                    
         LA    RF,NDBCODE-NDBNODEL                                              
         OC    NDBNODE,WKNOD           SET THIS NODE                            
         BZ    SETBL4              IF NONE- SKIP CODE                           
         MVC   NDBCODE(NDCODL),WKCOD                                            
         LA    RF,NDBCODE+NDCODL                                                
         CLI   0(RF),0                                                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,WORK-1                                                        
         SR    RF,R0                                                            
*                                                                               
SETBL4   DS    0H                                                               
         STC   RF,WORK+1                                                        
         SR    R2,R2               ADD ELEM AT END                              
         BAS   RE,ADDEL                                                         
         B     XIT                 EXIT WITH CC SET                             
         EJECT                                                                  
*        SETATT - SET ATTACHMENTS                                               
         SPACE 2                                                                
SETATT   NTR1                                                                   
         SPACE 1                                                                
         CLC   NDSETACD,NULLS      TEST ATTACHMENT                              
         BNH   STA30               NO                                           
         CLC   NDLIBPRE,INPT       MUST NOT BE TO A POINT                       
         BNE   *+12                WITHIN A LIBRARY GROUP                       
         MVI   NDERR,NDLIBERR      NO ATTACH WITHIN LIBRARY                     
         B     SETATTX                                                          
*                                                                               
         CLC   NDSETACD(1),NDLIBPRE   FOR LIBRARY GROUPS                        
         BNE   *+10                                                             
         MVC   NDSETAND,NDLIBNOD      SET LIB NODE                              
*                                                                               
         CLC   NDSETAND(NDNODL+NDCODL),NDATTNOD    SAME AS NOW                  
         BE    STA30               YES-DONE                                     
         CLI   NDSETACD,X'FF'      FF = NONE (REMOVE)                           
         BNE   *+14                                                             
         XC    NDSETAND,NDSETAND                                                
         B     STA21H                                                           
*                                                                               
         CLC   NDATTCOD,NULLS      WAS IT PREVIOUSLY AN ATTACH                  
         BH    STA21H              YES - OK                                     
*                                  IF NOT MUST REMOVE NODE                      
         LA    R9,NDLVTABL(R9)                                                  
         OC    NDLVNOD,NDLVNOD     TEST ANY NODE                                
         BZ    STA21                                                            
         MVC   NDLVCOD,NULLS                                                    
         MVC   WKNC,NDLVNOD        NODE AND CODE                                
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHKN          THRU NODE                                    
         BNE   STA21               OK TO REMOVE NODE                            
*                                                                               
         MVI   NDERR,NDAMXERR      MIXING ATTACH AND REG                        
         B     SETATTX                                                          
*                                                                               
STA21    DS    0H                                                               
         SH    R9,=Y(NDLVTABL)     RETURN TO RIGHT LEVEL                        
*                                                                               
STA21H   DS    0H                                                               
         MVC   NDATTNOD(NDNODL+NDCODL),NDSETAND   SET ATT NODE + CODE           
         CLI   NDSETACD,X'FF'      BUT IF NONE                                  
         BNE   *+10                                                             
         MVC   NDATTCOD,NULLS      THEN CLEAR                                   
         MVI   ELCODE,X'B4'        DELETE ANY CURRENT ATTACH ELEMS              
         BAS   RE,DELEL                                                         
         MVI   ELCODE,X'B1'        DELETE ANY NODE ELEMS                        
         BAS   RE,DELEL                                                         
         CLC   =C'COPY',COMMAND    FOR COPY DONT ADD ATTACH ELEM                
         BE    STA21J                                                           
         CLI   NDSETACD,X'FF'      FF = NONE                                    
         BE    STA30                                                            
         XC    WORK,WORK           BUILD NEW ELEM IN WORK                       
         MVI   WORK,X'B4'                                                       
         MVC   WORK+2(NDNODL),NDSETAND    ATTACHMENT NODE                       
         MVC   WORK+6(L'NDSETACD),NDSETACD                                      
         LA    R4,WORK+6+L'NDSETACD                                             
         CLI   0(R4),0                                                          
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         LA    R0,WORK-1                                                        
         SR    R4,R0                                                            
         STC   R4,WORK+1           ELEM LENGTH                                  
*                                                                               
         SR    R2,R2               ADD AT END                                   
         BAS   RE,ADDEL                                                         
         BNE   SETATTX                                                          
*                                  READ ATTACHMENT FOR NODE                     
STA21J   DS    0H                                                               
         MVC   WKNC,NDATTNOD       ATTACHED NODE AND CODE                       
         BAS   RE,SETKEY                                                        
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCHK                                                        
         BE    *+12                                                             
*                                                                               
STA21L   DS    0H                                                               
         MVI   NDERR,NDANFERR      ATTACH NOT FOUND                             
         B     SETATTX                                                          
*                                                                               
         MVC   NDAREC,NDIOA2       USE IOA2                                     
         BAS   RE,GET                                                           
*                                                                               
         CLC   NDSETACD(1),NDLIBPRE  TEST ATTACH IS LIBRARY HEAD                
         BE    STA23                 YES - OK                                   
         MVI   NDERR,NDAMXERR      **DISALLOW NON-LIBRARY ATTACHES              
         B     SETATTX                                                          
*                                                                               
STA21P   DS    0H                  BUT CANNOT BE RECORD WITHIN LIBRARY          
         MVI   ELCODE,X'B5'          FIND BACK NODE ELEM                        
         BAS   RE,NEXTELF                                                       
         BNE   STA23                 NONE                                       
         USING NDBNODEL,R2                                                      
         TM    NDBCNTL,X'80'         TEST REC WITHIN LIB                        
         BZ    STA23                 NO- OK                                     
         MVI   NDERR,NDLIBERR                                                   
         B     STA30                                                            
*                                                                               
*                                                                               
STA23    DS    0H                                                               
         MVI   ELCODE,X'B1'        LOOK FOR NODE ELEM                           
         BAS   RE,NEXTELF                                                       
         BE    STA25               HAVE LOWER NODE                              
         CLC   =C'COPY',COMMAND     NO NODE IS OK FOR COPIES ONLY               
         BNE   STA21L                                                           
*                                                                               
STA25    DS    0H                                                               
         USING NDNODEL,R2                                                       
         MVC   DUB(NDNODL+2),NDNODE  NODE AND COUNT FROM ATTACHMENT             
*                                  NOW ADD NODE ELEM THIS CURRENT REC           
         MVC   NDAREC,NDIOA        RESTORE TO IOA1                              
         XC    WORK(10),WORK                                                    
         MVI   WORK,X'B1'                                                       
         MVI   WORK+1,10                                                        
         MVC   WORK+2(NDNODL+2),DUB  NODE AND COUNT                             
*                                                                               
         SR    R2,R2               ADD AT END                                   
         BAS   RE,ADDEL                                                         
*                                                                               
STA30    DS    0H                                                               
SETATTX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                **DIRECTORY CALLS**                            
         SPACE 3                                                                
READ     LA    RF,=C'DMREAD'                                                    
         B     DIRCTRR                                                          
         SPACE 2                                                                
SEQ      LA    RF,=C'DMRSEQ'                                                    
         B     DIRCTRR                                                          
         SPACE 2                                                                
HIGH     LA    RF,=C'DMRDHI'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRR                                                          
         SPACE 2                                                                
ADDDIR   LA    RF,=C'DMADD '                                                    
         B     DIRCTRW                                                          
         SPACE 2                                                                
WRITE    LA    RF,=C'DMWRT '                                                    
         B     DIRCTRW                                                          
         SPACE 2                                                                
DIRCTRR  MVI   DMGW,C'N'           NOT A WRITE                                  
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRW  MVI   DMGW,C'Y'           IS A WRITE                                   
         SPACE 2                                                                
DIRCTRY  NTR1                                                                   
         MVI   TRCFTYP,C'D'                                                     
         MVC   TRCKEY,KEY                                                       
         ST    RF,DMCB                                                          
         LA    RF,NDDIRNAM                                                      
         ST    RF,DMCB+4                                                        
         MVC   DMCB(1),DMINBTS                                                  
         OI    DMCB,X'80'          SET TO READ FOR UPDATE                       
         MVI   DMCB+8,0                                                         
         CLI   DMGW,C'N'                                                        
         BE    DIRCTRY2                                                         
         CLI   NDWRITE,C'N'        NO WRITES                                    
         BE    DMCHECK                                                          
DIRCTRY2 GOTO1 NDDMGR,DMCB,,,KEY,KEY                                            
         B     DMCHECK                                                          
         SPACE 3                                                                
*                                        **FILE CALLS**                         
         SPACE 3                                                                
GET      LA    RF,=C'GETREC'                                                    
         B     FILER                                                            
         SPACE 2                                                                
PUT      LA    RF,=C'PUTREC'                                                    
         B     FILEW                                                            
         SPACE 2                                                                
ADD      LA    RF,=C'ADDREC'                                                    
         B     FILEW                                                            
         SPACE 2                                                                
FILER    MVI   DMGW,C'N'           NOT A WRITE                                  
         B     FILE                                                             
         SPACE 2                                                                
FILEW    MVI   DMGW,C'Y'           IS A WRITE                                   
         SPACE 2                                                                
FILE     NTR1                                                                   
         MVI   TRCFTYP,C'F'                                                     
         ST    RF,DMCB                                                          
         LA    RF,NDFILNAM                                                      
         ST    RF,DMCB+4                                                        
         MVC   DMCB(1),DMINBTS                                                  
         CLI   NDUPDTSW,C'N'       TEST TO READ FOR UPDATE                      
         BE    *+8                                                              
         OI    DMCB,X'80'                                                       
         LA    RE,KEY                                                           
         AH    RE,NDDISK                                                        
         ST    RE,DMCB+8                                                        
         MVC   TRCDISK,0(RE)                                                    
         MVC   LASTDA,0(RE)        SAVE LAST DISK ADDRESS                       
         MVI   DMCB+8,0                                                         
         CLI   DMGW,C'N'                                                        
         BE    FILE2                                                            
         CLI   NDWRITE,C'N'        NO WRITES                                    
         BE    DMCHECK                                                          
*                                                                               
FILE2    GOTO1 NDDMGR,DMCB,,,,NDAREC,DMWRK                                      
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  DS    0H                                                               
         CLI   NDTRACE,C'Y'        TEST DOING TRACE                             
         BNE   DMCK20                                                           
*                                                                               
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         MVC   P(3),=C'ND*'                                                     
         L     RF,DMCB                                                          
         MVC   P+4(6),0(RF)                                                     
         L     RF,DMCB+4                                                        
         MVC   P+11(8),0(RF)                                                    
         GOTO1 NDHEXOUT,TRCDMCB,DMCB+8,P+29,1,=C'N'                             
         LH    R4,NDDISK                                                        
         LA    R4,4(R4)            LENGTH INCLUDING DISK ADDR                   
         CLI   TRCFTYP,C'D'                                                     
         BNE   DMCK8                                                            
*                                  DIRECTORY TRACE                              
         L     RF,DMCB                                                          
         CLC   3(3,RF),=C'SEQ'     FOR SEQ PRINT ONLY KEY FOUND                 
         BE    DMCK4D                                                           
         LH    R1,NDKLENM1                                                      
         EX    R1,*+8          PRINT TRCKEY ONLY IF NOT = TO KEY FOUND          
         B     *+10                                                             
         CLC   TRCKEY(0),KEY                                                    
         BNE   DMCK4                                                            
         MVI   P+41,C'='                                                        
         B     DMCK4D                                                           
*                                                                               
DMCK4    DS    0H                                                               
         GOTO1 NDHEXOUT,TRCDMCB,TRCKEY,P+42,(R4)                                
         GOTO1 NDPRINT,TRCDMCB,P,=C'BL01'                                       
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
*                                                                               
DMCK4D   DS    0H                                                               
         GOTO1 NDHEXOUT,TRCDMCB,KEY,P+42,(R4),=C'N'                             
         GOTO1 NDPRINT,TRCDMCB,P,=C'BL01'                                       
         B     DMCK20                                                           
*                                                                               
DMCK8    DS    0H                                                               
         GOTO1 (RF),(R1),TRCDISK,P+20,4                                         
         GOTO1 (RF),(R1),NDAREC,P+42,(R4)                                       
         GOTO1 NDPRINT,TRCDMCB,P,=C'BL01'                                       
*                                                                               
*                                                                               
DMCK20   DS    0H                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    XIT                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
*        LVTRACE - TRACE OF LEVEL TAB AND GO'S                                  
         SPACE 2                                                                
LVTRACE  DS    0H                                                               
         CLI   NDTRACE,C'Y'        IF NO TRACE                                  
         BNER  RE                  RETURN                                       
         LR    R0,RE                                                            
         GOTO1 ALVTRCE                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*        PARAMETER SUBSTITUTION ROUTINE                                         
         SPACE 2                                                                
PARSUB   DS    0H                                                               
         CLI   NDLIBLEV,0          TEST HAVE LIBRARY CALL                       
         BER   RE                                                               
         CLC   NDLIBLEV,NDLEV      TEST THERE YET                               
         BNH   PARSUBN             NO- DONE                                     
         CR    RE,RE               SET CC OK                                    
         BR    RE                                                               
*                                                                               
PARSUBN  NTR1                                                                   
         CLC   NDLIBLEV,NDLEV      TEST LIB CALL AT THIS LEVEL                  
         BNE   *+14                                                             
         BAS   RE,MOVR1            YES - JUST SAVE CALLING REC                  
         CR    RE,RE               SET CC OK                                    
         B     PSX                 AND EXIT                                     
*                                                                               
*                                  IF BELOW THIS LEVEL TRY PARAM SUBS           
*                                  FIRST MAKE SURE STILL HAVE                   
*                                  RECORD OF LEVEL DOING CALL                   
*                                                                               
         ZIC   R9,NDLIBLEV         POINT TO LEVEL OF ATTACH                     
         MH    R9,=Y(NDLVTABL)                                                  
         LA    R9,NDLVTAB(R9)                                                   
*                                                                               
         L     R2,NDIOA2           POINT TO WHERE REC SHOULD BE                 
         LH    RF,NDKLENM1                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   NDLVKEY(0),0(R2)    TEST KEY OK                                  
         BE    PS2                                                              
*                                  NO- RE-READ RECORD                           
         LA    RF,KEY                                                           
         AH    RF,NDDISK                                                        
         MVC   0(4,RF),NDLVDA      DISK ADDRESS                                 
         MVC   NDAREC,NDIOA2       READ INTO 2ND IOA                            
         BAS   RE,GET                                                           
         MVC   NDAREC,NDIOA        RESTORE TO NORMAL IO AREA                    
*                                                                               
PS2      DS    0H                                                               
         GOTO1 APARSUBR            DO ACTUAL PARAM SUBSTITUTION                 
PSX      XIT1                      NB- CC SET IN PARSUBR FOR RETURN             
         EJECT                                                                  
*        MASTADD- ADD NEW MASTER                                                
         SPACE 2                                                                
MASTADD  DS    0H                                                               
*                                  ADD MASTER CONTROL ELEMENT                   
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING NDMASTEL,R2                                                      
*                                                                               
         MVI   WORK,X'BF'                                                       
         MVI   WORK+1,20                                                        
*                                                                               
         MVI   NDMHINOD+3,FNODE    SET HIGHEST NODE USED                        
         OC    NDMNODLN,NDNODLN    NODE LENGTH                                  
         BNZ   *+8                                                              
         MVI   NDMNODLN,4          DEFAULTS TO 4                                
*                                                                               
         MVC   NDMNPOS,NDNDPOS     NODE POSITION                                
*                                                                               
         OC    NDMLIBND,NDLIBNOD   LIB NODE                                     
         BNZ   *+8                                                              
         MVI   NDMLIBND+3,2        DEFAULTS TO 2                                
*                                                                               
         OC    NDMLIBPR,NDLIBPRE   LIB PREFIX                                   
         BNZ   *+8                                                              
         MVI   NDMLIBPR,C'$'       DEFAULTS TO $                                
*                                                                               
         OC    NDMUSEQ,NDUSEQ      USER SEQUENCE OPTION                         
         BNZ   *+8                                                              
         MVI   NDMUSEQ,C'Y'        DEFAULTS TO Y (SUPPORT USER SEQ)             
*                                                                               
         OC    NDMCDLN,NDCODLN     CODE LENGTH                                  
         BNZ   *+8                                                              
         MVI   NDMCDLN,8           DEFAULTS TO 8                                
*                                                                               
         OC    NDMCDALN,NDCDALIN   CODE ALIGNMENT (R,L)                         
         BNZ   *+8                                                              
         MVI   NDMCDALN,C'L'       DEFAULTS TO L                                
*                                                                               
         SR    R2,R2               ADD ELEM AT END                              
         BAS   RE,ADDEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO ROOM                                      
*                                                                               
*                                  ADD INITIAL LEVEL ELEMENT                    
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING NDNODEL,R2                                                       
*                                                                               
         MVI   WORK,X'B1'                                                       
         MVI   WORK+1,10                                                        
*                                                                               
         MVI   NDNODE+3,FNODE      SET FIRST NODE                               
*                                                                               
         SR    R2,R2               ADD ELEM AT END                              
         BAS   RE,ADDEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO ROOM                                      
*                                                                               
         BAS   RE,ADD                                                           
         B     NDIOX                                                            
*                                                                               
FNODE    EQU   101                 RESERVE FIRT 100 NODES                       
         SPACE 2                                                                
NULLS    DC    XL(NDCODL)'00'                                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*                                  ERROR MESSAGES                               
*                                  --------------                               
EMSGL    EQU   40                                                               
ERRMSGS  DS    0C                                                               
         DC    AL1(NDLEVERR),CL40'TOO MANY LEVELS'                              
         DC    AL1(NDCDLERR),CL40'CODE LENGTH ERROR'                            
         DC    AL1(NDRNFERR),CL40'RECORD NOT FOUND'                             
         DC    AL1(NDADDERR),CL40'RECORD ALREADY ON FILE'                       
         DC    AL1(NDRENERR),CL40'RENAME CODE ALREADY IN USE'                   
         DC    AL1(NDPMDERR),CL40'INVALID POSITIONING REQUEST'                  
         DC    AL1(NDOVFERR),CL40'RECORD IS TOO BIG'                            
         DC    AL1(NDRESERR),CL40'INVALID RESTORE- RECORD NOT DELETED'          
         DC    AL1(NDLIBERR),CL40'LIBRARY CALL INVALID '                        
         DC    AL1(NDANFERR),CL40'ATTACHMENT NOT FOUND'                         
         DC    AL1(NDAMXERR),CL40'INVALID ATTACHMENT'                           
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*        EXTRACT LEVEL DATA FROM CURRENT RECORD                                 
         SPACE 2                                                                
EXLEV    NMOD1 0,EXLEV                                                          
         L     RC,NDSAVWK          A(WORK AREA)                                 
         SPACE 1                                                                
         CLI   EXLATTSW,C'Y'       IF READING AN ATTACHED NODE                  
         BE    EXLEV3              DONT CLEAR ATTACH CONTROLS                   
*                                                                               
         CLI   NDATTLEV,0                                                       
         BE    EXLEV1                                                           
         CLC   NDATTLEV,NDLEV      IF NOT CURRENTLY WITHIN                      
         BL    EXLEV2              ATTACHMENT                                   
         MVI   NDATTLEV,0          CLEAR ATTACHMENT LEVEL                       
EXLEV1   DS    0H                                                               
         XC    NDATTNOD,NDATTNOD                                                
         MVC   NDATTCOD,NULLS                                                   
*                                                                               
EXLEV2   DS    0H                                                               
         CLC   NDLIBLEV,NDLEV      UNLESS CURRENTLY WITHIN                      
         BL    *+8                 LIBRARY CALL                                 
         MVI   NDLIBLEV,0          CLEAR LIBRARY LEVEL                          
*                                                                               
EXLEV3   DS    0H                                                               
         MVI   EXLNEWA,C'N'        SET NO NEW ATTACH                            
         XC    NDLVAND,NDLVAND     CLEAR ATTACHMENT NODE                        
         MVC   NDLVACD,NULLS       AND CODE                                     
         MVC   NDLVNOD2,NULLS      AND NEXT LEVEL NODE                          
*                                                                               
         LA    RF,KEY              POINT TO DISK ADDRESS                        
         AH    RF,NDDISK                                                        
         MVC   NDLVDA2,0(RF)       SET ATTACH DISK ADDRESS                      
*                                                                               
         CLI   EXLATTSW,C'Y'       IF READING AN ATTACHED NODE                  
         BE    EXLEV4              DONT DO FWRD,BACK,KEY, OR DA                 
*                                                                               
         MVC   NDLVDA,0(RF)        SET NORMAL DISK ADDRESS                      
         MVC   NDLVFWRD,NULLS      CLEAR FWRD LINK                              
         MVC   NDLVBACK,NULLS      CLEAR BACK LINK                              
*                                                                               
         XC    NDLVKEY,NDLVKEY     SET KEY IN LEV TAB                           
         LH    R1,NDKLENM1                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDLVKEY(0),KEY                                                   
*                                                                               
EXLEV4   DS    0H                                                               
         LA    R9,NDLVTABL(R9)     **NOTE-MOST DATA FOUND AT GIVEN              
*                                         LEVEL APPLIES TO NEXT LEVEL           
         CLC   NDLEV,NDMXLEVS      IF NOW AT LOWEST LEVEL                       
         BE    EXLEV5              SKIP NEXT LEVEL CLEAR                        
*                                                                               
         XC    NDLVNOD,NDLVNOD     CLEAR NODE                                   
         MVI   NDLVSTAT,0                                                       
         MVC   NDLVKEY,NDKEY       SET KEY                                      
         MVC   NDLVFRST,NULLS      CLEAR FIRST                                  
         MVC   NDLVLAST,NULLS      CLEAR LAST                                   
         MVC   NDLVCOD,NULLS       CLEAR CODE                                   
         MVC   NDLVCNT,NULLS       CLEAR COUNT OF RECORDS                       
*                                                                               
EXLEV5   DS    0H                                                               
         L     R2,NDAREC                                                        
         AH    R2,NDELSTRT                                                      
*                                                                               
EXLEV8   DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    EXLEV50                                                          
         CLC   NDLEV,NDMXLEVS      IF NOW AT LOWEST LEVEL                       
         BE    EXLEV8F             SKIP NEXT LEVEL SETS                         
*                                                                               
         CLI   0(R2),X'B1'         NODE ELEM                                    
         BE    EXLEV16                                                          
         CLI   0(R2),X'B4'         ATTACHMENT ELEM                              
         BE    EXLEV18                                                          
         CLI   0(R2),X'B6'         POINTER TO FIRST FOR NEXT LEVEL              
         BE    EXLEV20                                                          
         CLI   0(R2),X'B7'         POINTER TO LAST FOR NEXT LEVEL               
         BE    EXLEV22                                                          
*                                                                               
EXLEV8F  DS    0H                                                               
         CLI   0(R2),X'B8'         POINTER TO NEXT FOR THIS LEVEL               
         BE    EXLEV24                                                          
         CLI   0(R2),X'B9'         POINTER TO BACK FOR THIS LEVEL               
         BE    EXLEV26                                                          
*                                                                               
*                                                                               
EXLEV9   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     EXLEV8                                                           
*                                                                               
EXLEV16  DS    0H                  NODE ELEM                                    
         USING NDNODEL,R2                                                       
         SH    R9,=Y(NDLVTABL)     SET NODE AT CURRENT LEVEL                    
         MVC   NDLVNOD2,NDNODE                                                  
         LA    R9,NDLVTABL(R9)                                                  
         OC    NDLVCNT,NDNCOUNT    SET COUNT AT NEXT LEVEL                      
         BZ    *+10                IF ANY RECORDS                               
         MVC   NDLVNOD,NDNODE      THEN SET NODE                                
         CLI   NDDELRSW,C'Y'       BUT IF PASSING DELETES                       
         BNE   *+10                                                             
         MVC   NDLVNOD,NDNODE      ALWAYS SET NODE                              
         B     EXLEV9                                                           
*                                                                               
EXLEV18  DS    0H                  ATTACHMENT  ELEM                             
         CLC   =C'DELALL',COMMAND    FOR DELALL COMMAND                         
         BE    EXLEV9              DO NOT SET ATTACHMENT                        
         USING NDATTEL,R2                                                       
         MVC   NDATTNOD,NDAATTND                                                
         ZIC   RF,1(R2)            ELEM LENGTH                                  
         SH    RF,=H'7'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NDATTCOD(0),NDAATTCD                                             
         MVC   NDATTLEV,NDLEV      SET LEVEL OF CALL                            
         CLC   NDATTCOD(1),NDLIBPRE   IF THIS ATTACH IS LIBRARY                 
         BNE   *+10                                                             
         MVC   NDLIBLEV,NDLEV      SET LEVEL OF LIBRARY CALL                    
         SH    R9,=Y(NDLVTABL)     BACK UP                                      
         MVC   NDLVAND(NDNODL+NDCODL),NDATTNOD  NODE + CODE                     
         LA    R9,NDLVTABL(R9)     RESTORE                                      
         MVI   EXLNEWA,C'Y'        SET HAVE NEW ATTACH                          
         B     EXLEV9                                                           
*                                                                               
EXLEV20  DS    0H                  SET FIRST FOR NEXT LEVEL                     
         USING NDFRSTEL,R2                                                      
         ZIC   RF,1(R2)            ELEM LENGTH                                  
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NDLVFRST(0),NDFRSTCD                                             
         B     EXLEV9                                                           
*                                                                               
EXLEV22  DS    0H                  SET LAST FOR NEXT LEVEL                      
         USING NDLASTEL,R2                                                      
         ZIC   RF,1(R2)            ELEM LENGTH                                  
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NDLVLAST(0),NDLASTCD                                             
         B     EXLEV9                                                           
*                                                                               
EXLEV24  DS    0H                  SET NEXT FOR THIS LEVEL                      
         CLI   EXLATTSW,C'Y'       IF DOING ATTACHMENT                          
         BE    EXLEV9              IGNORE FWRD LINK                             
         USING NDFWRDEL,R2                                                      
         SH    R9,=Y(NDLVTABL)     BACK UP                                      
         ZIC   RF,1(R2)            ELEM LENGTH                                  
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NDLVFWRD(0),NDFWRDCD                                             
         LA    R9,NDLVTABL(R9)     RESTORE                                      
         B     EXLEV9                                                           
*                                                                               
EXLEV26  DS    0H                  SET BACK FOR THIS LEVEL                      
         CLI   EXLATTSW,C'Y'       IF DOING ATTACHMENT                          
         BE    EXLEV9              IGNORE BACK LINK                             
         USING NDBACKEL,R2                                                      
         SH    R9,=Y(NDLVTABL)     BACK UP                                      
         ZIC   RF,1(R2)            ELEM LENGTH                                  
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NDLVBACK(0),NDBACKCD                                             
         LA    R9,NDLVTABL(R9)     RESTORE                                      
         B     EXLEV9                                                           
         SPACE 2                                                                
EXLEV50  DS    0H                                                               
*                                                                               
EXLEVX   DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        PARSUBR- LIBRARY PARAMETER SUBSTITUTION                                
         SPACE 2                                                                
PARSUBR  NMOD1 0,PARSUBR                                                        
         L     RC,NDSAVWK                                                       
         SPACE 1                                                                
*                                                                               
         BAS   RE,PSOMCK           FIRST CHECK FOR OMITS                        
         BNE   PSNOX               THIS RECORD TO BE OMITTED                    
*                                                                               
*                                  LOOK FOR PARAM ID AND REPLACEMENT            
*                                  ELEMENT PAIRS                                
         SPACE 1                                                                
*        SUBSTITUTION NOW RELIES ON A TRAILING SPACE OR END OF ELEMENT          
*        TO DELIMIT A SUBSTITUTION PARAM.                                       
*                                                                               
         L     R2,NDIOA2           IOAREA 2 HAS LIB CALL LEVEL REC              
         AH    R2,NDELSTRT                                                      
*                                                                               
PS4      DS    0H                                                               
         CLI   0(R2),0             END OF PARAM/SUB ELEMS                       
         BE    PSR20                                                            
         CLI   0(R2),X'BA'         PAR ID ELEM                                  
         BE    PS5                                                              
*                                                                               
PS4B     DS    0H                  NEXT ELEM                                    
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PS4                                                              
*                                                                               
PS5      DS    0H                                                               
         CLI   1(R2),L'PARID+1                                                  
         BNH   *+6                                                              
         DC    H'0'                BAD PARAM ID ELEM                            
*                                                                               
         ZIC   R5,1(R2)                                                         
         SH    R5,=H'4'                                                         
         MVI   PARID,C' '          SPACE FILL TO FORCE TRAILING SPACE           
         MVC   PARID+1(L'PARID+L'REPL-1),PARID                                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PARID+2(0),3(R2)                                                 
         MVC   PARID(2),=C'&&&&'                                                
         LA    R5,4(R5)            R5 HAS LENGTH (INC TRAIL SPACE)              
*                                                                               
         LA    R3,0(R2,R5)         POINT TO NEXT ELEM                           
         CLI   0(R3),X'BB'                                                      
         BE    *+6                                                              
         DC    H'0'                MISSING REPLACEMENT ELEM                     
         ZIC   R6,1(R3)                                                         
         SH    R6,=H'4'                                                         
         BNM   *+6                                                              
         DC    H'0'                BAD REPLACEMENT ELEM                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   REPL(0),3(R3)                                                    
         LA    R6,2(R6)            R6=L'REPLACEMENT TEXT PLUS SPACE             
*                                                                               
*                                  SEARCH LIB MEMBER RECORD FOR                 
*                                  PARAMS AND DO REPLACMENT                     
         L     R4,NDIOA            RECORD IS IN IOAREA 1                        
         AH    R4,NDELSTRT                                                      
*                                                                               
PSR4     DS    0H                                                               
         CLI   0(R4),0             EOR                                          
         BE    PS4B                NEXT PARAM ID ELEM                           
         CLI   0(R4),X'B0'         IGNORE NODIO CTL ELEMS B0-BF                 
         BL    PSR6                                                             
         CLI   0(R4),X'BF'                                                      
         BH    PSR6                                                             
*                                                                               
PSR4B    DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PSR4                                                             
*                                                                               
PSR6     DS    0H                                                               
         MVI   ELWRK,C' '                                                       
         MVC   ELWRK+1(L'ELWRK-1),ELWRK                                         
         ZIC   RF,1(R4)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELWRK(0),0(R4)      SET WHOLE ELEM IN ELWRK                      
         ST    RF,DMCB+4           SET 2ND PARAM L'TEXT TO BE MODIFIED          
         BCTR  RF,0                                                             
         ST    RF,FULL             SAVE CURRENT LEN -NB EXL TRAIL SPACE         
*                                                                               
         GOTO1 NDTSCAN,DMCB,ELWRK+2,,((R5),PARID),((R6),REPL),0                 
*                                                                               
         OC    DMCB+16(4),DMCB+16  TEST PARAM FOUND                             
         BZ    PSR4B               NO-NEXT MEMBER ELEM                          
*                                                                               
         L     RF,DMCB+4           ADJUST ELEM LENGTH IF TRAILING SPACE         
         CLI   0(RF),C' '          IGNORE TRAILING SPACE IF ANY                 
         BH    *+10                                                             
         BCTR  RF,0                                                             
         ST    RF,DMCB+4                                                        
*                                                                               
         CLI   1(R3),3             IF NO REPLACEMENT VALUE                      
         BNH   PSR6A                                                            
         CLC   =C'DELETE',3(R3)    OR 'DELETE'                                  
         BNE   PSR6B               REMOVE ELEMENT                               
*                                                                               
PSR6A    DS    0H                  REOVE ELEMENT                                
         GOTO1 NDRECUP,DMCB,(X'FE',NDIOA),(R4),,NDELSTRT                        
         B     PSR4                NEXT LIB MEMBER ELEM                         
*                                                                               
PSR6B    DS    0H                                                               
         CLC   FULL,DMCB+4         TEST LENGTH CHANGE                           
         BE    PSR8                NO                                           
         CLC   DMCB+4(4),=F'253'   YES TEST TOO LARGE                           
         BNH   *+12                                                             
*                                                                               
PSR7     DS    0H                                                               
         MVI   NDERR,NDOVFERR      ELEM/RECORD OVERFLOW                         
         B     PSRX                                                             
*                                                                               
         L     RF,DMCB+4           SET NEW LENGTH                               
*                                                                               
         LA    R1,ELWRK+1(RF)                                                   
         LA    RF,2(RF)                                                         
         STC   RF,ELWRK+1                                                       
*                                  DELETE OLD ELEM                              
         GOTO1 NDRECUP,DMCB,(X'FE',NDIOA),(R4),,NDELSTRT                        
*                                                                               
         GOTO1 (RF),(R1),,ELWRK,(C'R',(R4)),NDELSTRT                            
         CLI   DMCB+8,C'R'         TEST RECORD TOO BIG                          
         BNE   PSR7                                                             
*                                                                               
         B     PSR4B               NEXT LIB MEMBER ELEM                         
*                                                                               
PSR8     DS    0H                  NO LENGTH CHANGE                             
         ZIC   RF,ELWRK+1                                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),ELWRK                                                    
         B     PSR4B               NEXT LIB MEMBER ELEM                         
*                                                                               
*                                  DONE WITH SUBS- NOW DELETE ELEMS             
*                                  WITH UNREPLACED PARAMS                       
PSR20    DS    0H                                                               
         L     R4,NDIOA            LIB MEMEBER RECORD IN IOA 1                  
         AH    R4,NDELSTRT                                                      
         B     PSR24                                                            
*                                                                               
PSR22    DS    0H                  BUMP TO NEXT ELEM                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
PSR24    DS    0H                                                               
         CLI   0(R4),0             EOR                                          
         BE    PSRX                                                             
         ZIC   R1,1(R4)                                                         
         ST    R1,DMCB+4           LENGTH OF TEXT                               
         LA    R3,=C'&&&&'        LOOK FOR &&                                   
         ST    R3,DMCB+8          SEARCH ARG                                    
         MVI   DMCB+8,2           LENGTH                                        
         MVC   DMCB+12(4),DMCB+8  REPLACEMENT ARG THE SAME (NO REPLACE)         
         GOTO1 NDTSCAN,DMCB,(R4),,,0                                            
         OC    DMCB+16(4),DMCB+16  TEST ANY FOUND                               
         BZ    PSR22               NO- NEXT ELEM                                
*                                  YES- DELETE ELEM                             
         GOTO1 NDRECUP,DMCB,(X'FE',NDIOA),(R4),,NDELSTRT                        
         B     PSR24                                                            
*                                                                               
PSNOX    DS    0H                                                               
         LTR   RE,RE               CC - OMIT RECORD                             
         B     PSRXX                                                            
*                                                                               
PSRX     DS    0H                                                               
         CR    RE,RE               CC - DO NOT OMIT                             
PSRXX    DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*        TEST ATTACHED MEMBER TO BE OMITTED                                     
*                                                                               
PSOMCK   NTR1                                                                   
         XC    XW,XW                                                            
*                                  LOOK FOR OMIT ELEMS                          
         L     R2,NDIOA2           IOAREA 2 HAS LIB CALL LEVEL REC              
         AH    R2,NDELSTRT                                                      
*                                                                               
PSO4     DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    PSOX                                                             
         CLI   0(R2),X'BC'         OMIT CODE ELEM                               
         BE    PSO5                                                             
*                                                                               
PSO4B    DS    0H                  NEXT ELEM                                    
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSO4                                                             
*                                                                               
PSO5     DS    0H                                                               
         USING NDLBOMEL,R2                                                      
         CLI   XW,0                TEST HAVE BUILT KEY TO COMPARE               
         BNZ   *+8                                                              
         BAS   RE,PSOSET           SET MEMBER PART OF 'KEY'                     
*                                                                               
         ZIC   RF,XW               LENGTH OF 'KEY'                              
         ZIC   R5,1(R2)                                                         
         SH    R5,=H'3'            LENGTH OF OMIT CODE                          
         CR    R5,RF               COMPARE LENGTHS                              
         BH    PSO4B               IF OMIT CODE LENGTH GREATER                  
*                                  DO NOT OMIT-TRY ANOTHER ELEM                 
         BCTR  R5,R0               ELSE COMPARE CODES                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   NDLBOMCD(0),XW+1    IF NOT EQUAL - DO NOT OMIT                   
         BNE   PSO4B               GET NEXT OMIT ELEM                           
*                                                                               
         LA    R5,1(R5)            CODES ARE EQUAL                              
         CR    R5,RF               IF LENGTHS ALSO EQUAL                        
         BE    PSONO               OMIT THIS RECORD                             
*                                                                               
         LA    R5,XW+1(R5)         OMIT CODE LENGTH SHORTER                     
         CLC   0(1,R5),NDDELIM     NEXT POS IN 'KEY' MUST BE                    
*                                  A DELIMETER (OMIT ALL AT THIS                
*                                  LEVEL OR LOWER)                              
         BNE   PSO4B               DO NOT OMIT-GET ANOTHER ELEM                 
*                                                                               
PSONO    DS    0H                                                               
         LTR   RE,RE               SET CC- OMIT                                 
         B     PSOXX                                                            
*                                                                               
PSOX     DS    0H                                                               
         CR    RE,RE               SET CC                                       
*                                                                               
PSOXX    DS    0H                                                               
         B     PSRXX                                                            
         SPACE 2                                                                
PSOSET   NTR1                                                                   
         ZIC   R9,NDLIBLEV         START AT ONE BEYOND ATTACH LEV               
         MH    R9,=Y(NDLVTABL)                                                  
         LA    R9,NDLVTAB+NDLVTABL(R9)                                          
*                                                                               
         LA    R3,XW+1                                                          
         ZIC   R6,NDLEV            NUMBER OF LEVELS                             
         ZIC   RE,NDLIBLEV         LESS LEVEL AFTER ATTACH                      
         SR    R6,RE               IS NUMBER TO DO                              
         BP    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R3               SAVE OUTPUT START                            
*                                                                               
PSOS4    DS    0H                                                               
         LA    RF,NDLVCOD+NDCODL-1                                              
         CLI   0(RF),0                                                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R0,NDLVCOD                                                       
         SR    RF,R0                                                            
         EX    RF,PSOMVC           SET CODE IN OUTPUT                           
*                                                                               
         LA    R3,1(RF,R3)                                                      
         MVC   0(1,R3),NDDELIM     SET DELIMITER                                
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R9,NDLVTABL(R9)     NEXT LEVEL                                   
         BCT   R6,PSOS4                                                         
*                                                                               
         BCTR  R3,R0                                                            
         MVI   0(R3),C' '          ERASE LAST DELIMITER                         
         SR    R3,R4                                                            
         STC   R3,XW               SET OUTPUT KEY LENGTH                        
*                                                                               
         B     PSRX                                                             
*                                                                               
PSOMVC   MVC   0(0,R3),NDLVCOD                                                  
         B     PSOXX                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        LVTRACE- TRACE ROUTINE                                                 
         SPACE 2                                                                
LVTRCE   NMOD1 0,LVTRCE                                                         
         L     RC,NDSAVWK                                                       
         SPACE 1                                                                
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
*                                                                               
         LA    R2,P                FIRST LINE                                   
         MVC   0(3,R2),=C'ND*'                                                  
*                                                                               
         LA    R2,4(R2)                                                         
         MVC   0(2,R2),=C'L='                                                   
         EDIT  (B1,NDLEV),(2,2(R2)),FILL=0                                      
*                                                                               
         LA    R2,5(R2)                                                         
         MVC   0(5,R2),=C'MODE='                                                
         GOTO1 NDHEXOUT,DMCB,NDMODE,5(R2),1,=C'N'                               
         MVI   7(R2),C'('                                                       
         MVI   12(R2),C')'                                                      
         LA    RF,MODLST                                                        
         LA    R0,MODLSTN                                                       
*                                                                               
LVTR4    DS    0H                                                               
         CLC   NDMODE,0(RF)                                                     
         BE    LVTR6                                                            
         LA    RF,5(RF)                                                         
         BCT   R0,LVTR4                                                         
*                                                                               
LVTR6    DS    0H                                                               
         MVC   8(4,R2),1(RF)       MODE WORD                                    
*                                                                               
         LA    R2,14(R2)                                                        
         MVC   0(4,R2),=C'ERR='                                                 
         GOTO1 NDHEXOUT,DMCB,NDERR,4(R2)                                        
*                                                                               
         LA    R2,7(R2)                                                         
         MVC   0(5,R2),=C'STAT='                                                
         GOTO1 (RF),(R1),NDLVSTAT,5(R2)                                         
*                                                                               
         LA    R2,8(R2)                                                         
         MVC   0(5,R2),=C'CODE='                                                
         MVC   5(12,R2),NDLVCOD                                                 
*                                                                               
         LA    R2,21(R2)                                                        
         MVC   0(5,R2),=C'NODE='                                                
         GOTO1 NDHEXOUT,DMCB,NDLVNOD,5(R2),NDNODL,=C'N'                         
*                                                                               
         GOTO1 NDPRINT,DMCB,P,=C'BL01'                                          
*                                          SECOND LINE                          
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
*                                                                               
         LA    R2,P+38                                                          
*                                                                               
         MVC   0(5,R2),=C'FRST='                                                
         MVC   5(12,R2),NDLVFRST                                                
         LA    R2,18(R2)                                                        
*                                                                               
         MVC   0(5,R2),=C'LAST='                                                
         MVC   5(12,R2),NDLVLAST                                                
         LA    R2,18(R2)                                                        
*                                                                               
         MVC   0(5,R2),=C'FWRD='                                                
         MVC   5(12,R2),NDLVFWRD                                                
         LA    R2,18(R2)                                                        
*                                                                               
         MVC   0(5,R2),=C'BACK='                                                
         MVC   5(12,R2),NDLVBACK                                                
         LA    R2,18(R2)                                                        
*                                                                               
         GOTO1 NDPRINT,DMCB,P,=C'BL01'                                          
*                                                                               
LVTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
MODLST   DS    0C                                                               
         DC    X'01',C'INIT'                                                    
         DC    X'11',C'FRST'                                                    
         DC    X'12',C'PROC'                                                    
         DC    X'13',C'LAST'                                                    
         DC    X'21',C'VAL '                                                    
         DC    X'FF',C'END '                                                    
MODLSTN  EQU   (*-MODLST)/5                                                     
         DC    X'00',C'****'                                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*        NODIO WORK DSECT                                                       
         SPACE 2                                                                
NDWKD    DSECT                                                                  
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
         DS    0F                                                               
PARAMS   DS    0XL24                                                            
PARAM1   DS    F                                                                
PARAM2   DS    F                                                                
PARAM3   DS    F                                                                
PARAM4   DS    F                                                                
PARAM5   DS    F                                                                
PARAM6   DS    F                                                                
*                                                                               
AEXLEV   DS    A                                                                
ALVTRCE  DS    A                                                                
APARSUBR DS    A                                                                
WORK     DS    XL80                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
XW       DS    XL80                                                             
P        DS    CL132                                                            
WKNC     DS    0CL(NDNODL+NDCODL)                                               
WKNOD    DS    XL(NDNODL)                                                       
WKCOD    DS    CL(NDCODL)                                                       
*                                                                               
POS      DS    CL(NDCODL)                                                       
BASW     DS    CL1                                                              
SPMODE   DS    CL1                                                              
*                                                                               
         DS    0D                                                               
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
TRCKEY   DS    XL64                                                             
SAVKEY   DS    XL64                                                             
RNKEY    DS    XL64                                                             
TRCDMCB  DS    6F                                                               
TRCDISK  DS    XL4                                                              
TRCFTYP  DS    C                                                                
LASTDA   DS    XL4                                                              
SAVLDA   DS    XL4                                                              
SAVNOD   DS    XL4                                                              
DMGW     DS    C                                                                
ELCODE   DS    X                                                                
ADDLSW   DS    C                                                                
EXLATTSW DS    C                                                                
EXLNEWA  DS    C                                                                
ADDSW    DS    C                                                                
MOVSW    DS    C                                                                
SVCNTL   DS    X                                                                
REPOSW   DS    C                                                                
REPONOD  DS    XL4                                                              
RNCNTL   DS    X                                                                
*                                                                               
ELWRK    DS    XL256                                                            
PARID    DS    CL20                SUBSTITUTION PARAMETER                       
REPL     DS    CL20                SUBSTITUTE VALUE                             
*                                                                               
DMWRK    DS    12D                                                              
NDAREC   DS    A                                                                
INPT     DS    CL80                                                             
SVINPT   DS    CL80                                                             
COMMAND  DS    CL8                                                              
DMINBTS  DS    X                                                                
DMOUTBTS DS    X                                                                
SVDMBTS  DS    XL2                                                              
RERDSW   DS    C                                                                
SQFRST   DS    C                                                                
*                                                                               
NDCALLRD DS    F         SAVE BACK NMOD LINK                                    
NDKLENM1 DS    H         KEY LENGTH-1                                           
NDKCLND  DS    H         KEY LENGTH THRU NODE END-1                             
NDKCLCD  DS    H         KEY LENGTH THRU CODE END-1                             
*                                                                               
NDCODL   EQU   12                                                               
NDNODL   EQU   4                                                                
*                                                                               
NDWKL    EQU   *-NDWKD                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDNODIOELS                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDNODBLKD                                                      
*                                                                               
NODBLKD  DSECT                                                                  
         ORG   NDWORK              NODIO FIELDS NOT IN USER DSECT               
NDSAVWK  DS    A                   SAVE A(WORK AREA)                            
*                                                                               
NDLVTABD DSECT                                                                  
         ORG   NDLVWORK            NODIO FIELDS NOT IN USER DSECT               
NDLVCNT  DS    XL2       COUNT OF RECORDS AT THIS LEVEL                         
NDLVSQC  DS    XL2       COUNTER TO CATCH READ LOOPS                            
NDLVSTAT DS    XL1       STATUS (40=NEW,20=FORCE READ)                          
NDLVDA2  DS    XL4       DA OF ATTACHMENT RECORD                                
NDLVAND  DS    XL4       ATTACHMENT NODE                                        
NDLVACD  DS    CL12      ATTACHMENT CODE                                        
NDLVDDSC DS    CL20      DEFAULT DESCRIPTION                                    
NDLVCPYN DS    XL4       NODE FOR COPY ROUTINE                                  
         DS    XL11      SPARE                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DDNODIO   05/01/02'                                      
         END                                                                    
