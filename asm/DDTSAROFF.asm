*          DATA SET DDTSAROFF  AT LEVEL 018 AS OF 11/26/12                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00A7DA                                                                  
*INCLUDE BINSR31                                                                
         TITLE 'TSAROFF - OFFLINE 31-BIT BUFFERING ROUTINE'                     
*==========================================================*                    
* TAKE NOTE ---                                            *                    
* THE ACTION FIELD IS MOVED FROM THE ONLINE VERSION        *                    
* TO ACCOMMODATE 31-BIT BUFFER ADDRESSES                   *                    
*----------------------------------------------------------*                    
* THIS VERSION SUPPORTS MORE THAN 65535 RECORDS BY TURNING *                    
* ON TSIND2 (TSI2MANY). ALL FIELDS CONTAINING NUMBER OF    *                    
* RECORDS ARE THEN TREATED AS FULLWORDS, NOT HALFWORDS.    *                    
*----------------------------------------------------------*                    
* 15APR94:TSACOM NO LONGER REQUIRED                        *                    
* 06JAN98:TSACOM HOLDS EXTRA PARAMTERS (TSI2XPRM SET)      *                    
*        :CODE ACCESS REGISTER COMPLIANT - YOU MUST CALL   *                    
*        :USING 'SAFE' MACRO THOUGH                        *                    
* 26AUG98:FIX BUG WITH MAX # RECORDS IN TABLE              *                    
*==========================================================*                    
         SPACE 1                                                                
TSAR     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TSAWRKX-TSAWRK,*TSAR*,RA,RR=RE,CLEAR=YES                         
         USING TSAWRK,RC           RC=A(W/S)                                    
         ST    RE,RELO                                                          
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'     CLEAN UP                                     
*                                                                               
         SAM31 ,                   GET INTO 31-BIT MODE                         
*                                                                               
         LA    R2,0(R1)            R1 POINTS TO TSAR BLOCK                      
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
*                                                                               
         MVI   TSERRS,0            CLEAR ERROR FLAG                             
**NOP    SR    R1,R1                                                            
**NOP    ICM   R1,7,TSACOM+1                                                    
**NOP    MVC   TDATAMGR,CDATAMGR-COMFACSD(R1)                                   
**NOP    MVC   TCALLOV,CCALLOV-COMFACSD(R1)                                     
*                                                                               
         XC    ALET,ALET                                                        
         TM    TSIND2,TSI2XPRM     EXTRA PARAMETERS SET?                        
         JZ    TS0                                                              
         ICM   RF,15,TSACOM                                                     
         USING TSXTRAD,RF                                                       
         ICM   R3,15,TSXALET       PASSING ALET?                                
         JZ    TS0                 NO                                           
         STCM  R3,15,ALET                                                       
         LAM   AR3,AR3,ALET                                                     
         SAC   512                                                              
*                                                                               
TS0      ICM   R3,15,TSABUF                                                     
         USING TSBUFFD,R3                                                       
*                                                                               
*        NOTE: FROM THIS POINT ONWARDS, ASSUME ACCESS REGISTERS ON              
*                                                                               
BUFFREG  EQU   R3,,,,GR32                                                       
         EJECT                                                                  
TS1      MVI   ADDFLAG,C'N'                                                     
         LA    RE,ACTNTAB                                                       
TS2      CLI   0(RE),0             TEST E-O-T                                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),TSOFFACT    MATCH ACTION TO TABLE                        
         JE    *+12                                                             
         LA    RE,L'ACTNTAB(RE)                                                 
         J     TS2                                                              
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)                                                       
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
* RETURN NUMBER OF RECORDS ON EXIT                                              
*                                                                               
         SAC   512                                                              
         SR    R0,R0                                                            
         ICM   R0,3,TSBRECS                                                     
         JZ    *+6                                                              
         BCTR  R0,0                                                             
         STH   R0,TSPRECN          SET NUMBER OF RECORDS                        
*                                                                               
         TM    TSIND2,TSI2MANY                                                  
         JZ    TS4                                                              
         ICM   R0,15,TSBRECS                                                    
         JZ    *+6                                                              
         BCTR  R0,0                                                             
         STCM  R0,15,TSPRECN       SET NUMBER OF RECORDS                        
*                                                                               
TS4      NI    TSINDS,X'FF'-TSIANYAD                                            
         LTR   R0,R0                                                            
         JZ    *+8                                                              
         OI    TSINDS,TSIANYAD                                                  
*                                                                               
TSX      DS    0H                                                               
*                                                                               
TSXX     SAC   0                   SET CONDITION CODE FOR CALLER                
         CLI   TSERRS,0            SET CONDITION CODE FOR CALLER                
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BSM   0,RE                RETURN IN CORRECT ADDRESSING MODE            
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO INITIALISE TSARD                                         *         
*=====================================================================*         
         SPACE 1                                                                
TSINI    NTR1  ,                                                                
         MVC   0(8,BUFFREG),=C'**TSAR**'  RESTORE EYECATCHER                    
         MVC   8(24,BUFFREG),0(BUFFREG)                                         
*                                                                               
         XC    TSBPGTAB(TSBXCLEN),TSBPGTAB                                      
         MVC   TSBKKK1,=C'KLNS'                                                 
         MVC   TSBKKK2,=C'RECS'                                                 
         MVC   TSBRECI,TSRECI      SAVE RECI                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,TSKEYL                                                      
         JNZ   *+6                                                              
         DC    H'0'                KEY LENGTH OF ZERO SPECIFIED                 
         STH   R0,TSBKEYL                                                       
         BCTR  R0,0                                                             
         STH   R0,TSBKEYL1                                                      
*                                                                               
         LH    R0,TSBKEYL                                                       
         AHI   R0,3                                                             
         STH   R0,TSBKEYL3         SAVE KEYLEN + 3                              
*                                                                               
         MVC   TSBRECL,TSRECL                                                   
         OC    TSBRECL,TSBRECL                                                  
         JNZ   *+6                                                              
         DC    H'0'                RECORD LENGTH NOT SPECIFIED                  
*                                                                               
         LHI   R1,4096                                                          
         TM    TSIND2,TSI2OBIG                                                  
         JZ    *+8                                                              
         LHI   R1,8192                                                          
         CH    R1,TSBRECL                                                       
         JNL   *+6                                                              
         DC    H'0'                RECORD LENGTH TOO LONG                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1       GET BUFFER SIZE                              
         ST    R1,TSBMAX           SET MAX BYTES                                
         ST    R1,TSBNXDTA         SET DSPL TO NEXT DATA RECORD END             
* BUILD A HIGH KEY IN THE KEY AREA                                              
         XC    TSBKEYS(3),TSBKEYS  CLEAR DSPL FIELD                             
         MVI   TSBKEYS+3,X'FF'                                                  
         LH    RE,TSBKEYL                                                       
         SHI   RE,2                                                             
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         MVC   TSBKEYS+4(0),TSBKEYS+3  *EXECUTED*                               
*                                                                               
         LA    RE,5(RE)                GET KEY BYTES USED                       
         LA    RE,TSBKEYS-TSBUFFD(RE)  ADD BUFFER OVERHEAD                      
         ST    RE,TSBACT               SET ACTUAL COUNT                         
         ST    RE,TSBNXKEY             SET DSPL FOR NEXT KEY                    
*                                                                               
         LA    R0,1                SET 1 RECORD PRESENT                         
         STH   R0,TSBRECS                                                       
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         ST    R0,TSBRECS                                                       
*                                                                               
TSINIX   OI    TSINDS,TSIINIOK                                                  
         J     TSX                                                              
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO ADD A RECORD BY KEY                                      *         
*=====================================================================*         
         SPACE 1                                                                
TSADD    NTR1  ,                                                                
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(RECORD TO BE ADDED)                     
         JNZ   *+6                                                              
         DC    H'0'                RECORD ADDRESS NOT PASSED                    
         ST    RE,AREC             SAVE A(RECORD TO BE ADDED)                   
*                                                                               
         LH    R1,TSBRECL                                                       
         TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         JZ    *+8                                                              
         ICM   R1,3,0(RE)                                                       
         STH   R1,RECL             SAVE RECORD LENGTH                           
         CLC   RECL,TSBRECL        COMPARE ACTUAL LEN TO MAX                    
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         A     R1,TSBACT                                                        
         LA    R1,3(R1)            ADD 3 BYTES/KEY FOR DSPL                     
         C     R1,TSBMAX                                                        
         JL    *+12                                                             
         MVI   TSERRS,TSEEOF       YES - RECORD WILL NOT FIT                    
         J     TSX                                                              
*                                                                               
         MVI   ADDFLAG,C'Y'                                                     
         BAS   RE,TSRDH            READ HIGH FOR KEY                            
         SAC   512                                                              
         JNE   *+12                                                             
         MVI   TSERRS,TSEDUP       IF FOUND SET DUPLICATE KEY                   
         J     TSX                                                              
*                                                                               
         TM    TSERRS,TSEEOF       DO NOT ALLOW ADD OF X'FF' RECORD             
         JO    TSX                                                              
*                                                                               
         MVI   TSERRS,0            RESET ERR IND                                
         ST    R1,TSBACT           SAVE NEW ACTUAL BYTE COUNT                   
*                                                                               
         L     R4,KEYADDR          GET ADDR OF ADDED KEY                        
         LH    R0,RECL                                                          
         SH    R0,TSBKEYL          LESS KEY LENGTH                              
         L     R1,TSBNXDTA         GET DATA DSPL                                
         SR    R1,R0               GIVES MOVE 'TO' DSPL                         
         LAM   AR4,AR4,ALET                                                     
         STCM  R1,7,0(R4)          SET DSPL IN KEY TABLE                        
         ST    R1,TSBNXDTA         AND SET NEW DATA DSPL                        
         EJECT                                                                  
* UPDATE NEXT KEY DSPL                                                          
         L     R0,TSBNXKEY                                                      
         AH    R0,TSBKEYL3                                                      
         ST    R0,TSBNXKEY                                                      
* NOW SET UP TO MOVE RECORD TO BUFFER                                           
         ICM   R4,15,TSBNXDTA                                                   
         AR    R4,BUFFREG          POINT TO 'TO' DATA ADDRESS                   
         L     RE,AREC             POINT TO USER RECORD                         
         TM    TSBRECI,TSRVAR                                                   
         JZ    TSADD10                                                          
         SPACE 1                                                                
*====================================================*                          
* VARIABLE LENGTH RECORDS                            *                          
*====================================================*                          
         SPACE 1                                                                
         MVC   0(2,R4),0(RE)       MOVE RECORD LENGTH TO DATA AREA              
         LA    R4,2(R4)            SET 'TO' ADDRESS FOR RECORD DATA             
         LH    R5,RECL             RECLEN                                       
         SH    R5,TSBKEYL          LESS KEY LEN                                 
         SHI   R5,2                LESS 2 FOR RECLEN ALREADY MOVED              
         JP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(RE)            POINT PAST RECLEN                            
         AH    RE,TSBKEYL          AND KEY                                      
         LR    RF,R5               SET 'FROM' LEN = 'TO' LEN                    
         MVCL  R4,RE                                                            
         J     TSADDX                                                           
         SPACE 1                                                                
*====================================================*                          
* FIXED LENGTH RECORDS                               *                          
*====================================================*                          
         SPACE 1                                                                
TSADD10  LH    R5,TSBRECL                                                       
         SH    R5,TSBKEYL                                                       
         AH    RE,TSBKEYL          POINT PAST KEY                               
         LR    RF,R5                                                            
         MVCL  R4,RE                                                            
*                                                                               
TSADDX   LAM   AR4,AR4,=F'0'                                                    
         TM    TSIND2,TSI2XPRM     EXTRA PARAMETERS?                            
         JZ    TSADDX1             NO                                           
         L     RE,TSACOM           RETURN LENGTH OF BUFFER USED                 
         MVC   TSXADD-TSXTRAD(L'TSXADD,RE),TSBACT                               
*                                                                               
TSADDX1  TM    TSIND2,TSI2MANY                                                  
         JO    TSADDX2                                                          
         LH    RE,TSBRECS                                                       
         LA    RE,1(RE)                                                         
         STH   RE,TSBRECS                                                       
         J     TSX                                                              
*                                                                               
TSADDX2  L     RE,TSBRECS                                                       
         LA    RE,1(RE)                                                         
         ST    RE,TSBRECS                                                       
         J     TSX                                                              
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO PUT A RECORD BY NUMBER                                   *         
*=====================================================================*         
         SPACE 1                                                                
TSPUT    LH    R0,TSRNUM                                                        
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         ICM   R0,15,TSRNUM        TEST RECORD NUMBER SET                       
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TSGET            GET RECORD                                   
         SAC   512                                                              
         JNE   *+8                                                              
         BAS   RE,TSUPD            UPDATE RECORD IF FOUND                       
TSPUTX   J     TSX                                                              
         SPACE 2                                                                
*=====================================================================*         
* ROUTINE TO PUT A RECORD BY KEY                                      *         
*=====================================================================*         
         SPACE 1                                                                
TSWRT    MVI   ADDFLAG,C'N'                                                     
         BAS   RE,TSRDH            READ HIGH FOR KEY                            
         SAC   512                                                              
         JNE   *+8                                                              
         BAS   RE,TSUPD            UPDATE RECORD IF FOUND                       
TSWRTX   J     TSX                                                              
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO UPDATE A RECORD                                          *         
*=====================================================================*         
         SPACE 1                                                                
TSUPD    NTR1  ,                                                                
         XR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(NEW RECORD)                             
         TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         JZ    *+8                                                              
         LA    RE,2(RE)            POINT TO KEY (AFTER LENGTH)                  
         LH    R1,TSBKEYL1                                                      
         L     R4,KEYADDR          R4=A(OLD KEY)                                
         LAM   AR4,AR4,ALET                                                     
         EXRL  R1,TSUPCMP          MATCH OLD/NEW KEY VALUES                     
         LAM   AR4,AR4,=F'0'                                                    
         JE    TSUPD02                                                          
         MVI   TSERRS,TSERNF       IF KEY CHANGED SET RECORD NOT FOUND          
         J     TSUPDX                                                           
*                                                                               
TSUPCMP  CLC   0(0,RE),3(R4)                                                    
*                                                                               
TSUPD02  TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         JO    TSUPD10                                                          
         SPACE 1                                                                
*=======================================================*                       
* FIXED LENGTH RECORDS - MOVE NEW OVER OLD              *                       
*=======================================================*                       
         SPACE 1                                                                
         XR    R0,R0                                                            
         ICM   R0,7,0(R4)          GET DSPL TO DATA                             
         AR    R0,BUFFREG          POINT TO DATA                                
         LR    R4,R0                                                            
         LAM   AR4,AR4,ALET                                                     
         LH    R5,TSBRECL          GET RECORD LENGTH                            
         SH    R5,TSBKEYL          LESS KEY LENGTH                              
*                                                                               
         AH    RE,TSBKEYL          POINT TO NEW DATA                            
         LR    RF,R5               'FROM' LEN = 'TO' LEN                        
         MVCL  R4,RE               MOVE RECORD                                  
         LAM   AR4,AR4,=F'0'                                                    
         J     TSUPDX                                                           
         EJECT                                                                  
*=======================================================*                       
* VARIABLE LENGTH RECORDS - TEST LENGTH CHANGED         *                       
*=======================================================*                       
         SPACE 1                                                                
TSUPD10  L     R4,KEYADDR                                                       
         LAM   AR4,AR4,ALET                                                     
         XR    RF,RF                                                            
         ICM   RF,7,0(R4)                                                       
         AR    RF,BUFFREG                                                       
         LR    R4,RF               R4 = OLD RECORD                              
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE = NEW RECORD                              
         CLC   0(2,RE),0(R4)       TEST SAME LENGTH                             
         JNE   TSUPD20                                                          
         SPACE 1                                                                
*=======================================================*                       
* LENGTH NOT CHANGED - MOVE NEW DATA OVER OLD           *                       
*=======================================================*                       
         SPACE 1                                                                
         XR    R5,R5                                                            
         ICM   R5,3,0(R4)          GET RECORD LENGTH                            
         SH    R5,TSBKEYL          LESS KEY LENGTH                              
         SHI   R5,2                LESS RECLEN OVERHEAD                         
         LA    R4,2(R4)            R0--> OLD DATA                               
*                                                                               
         LA    RE,2(RE)            RE--> NEW KEY                                
         AH    RE,TSBKEYL          RE--> NEW DATA                               
         LR    RF,R5                                                            
         MVCL  R4,RE                                                            
         LAM   AR4,AR4,=F'0'                                                    
         J     TSUPDX                                                           
         SPACE 1                                                                
*=======================================================*                       
* LENGTH CHANGED - DELETE RECORD AND ADD                *                       
*=======================================================*                       
         SPACE 1                                                                
TSUPD20  LAM   AR4,AR4,=F'0'                                                    
         BAS   RE,TSDEL            DELETE THIS RECORD                           
         SAC   512                                                              
         BAS   RE,TSADD            ADD RECORD WITH NEW LENGTH                   
*                                                                               
TSUPDX   J     TSX                                                              
         EJECT                                                                  
*======================================================*                        
* ROUTINE TO DELETE A RECORD BY NUMBER                 *                        
*  (IF RECORD NUMBER SET) OR BY KEY                    *                        
*======================================================*                        
         SPACE 1                                                                
TSDEL    NTR1  ,                                                                
         ICM   R4,15,KEYADDR       TEST GET PREVIOUSLY DONE                     
         JNZ   TSDEL2                                                           
         LA    RF,TSGET                                                         
         LH    R0,TSRNUM                                                        
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         ICM   R0,15,TSRNUM                                                     
         LTR   R0,R0               TEST RECORD NUMBER SET                       
         JNZ   *+8                                                              
         LA    RF,TSRDH            NO - DO READ FOR KEY                         
         MVI   ADDFLAG,C'N'                                                     
         BASR  RE,RF               GET RECORD ADDRESS                           
         JNE   TSDELX              EXIT IF NOT FOUND                            
         SAC   512                                                              
*                                                                               
TSDEL2   L     R4,KEYADDR                                                       
         LAM   AR4,AR4,ALET                                                     
         L     R0,TSBNXDTA         GET NEXT DATA DSPL                           
         AR    R0,BUFFREG          POINT TO IT                                  
         ST    R0,SAVE             SAVE START OF DATA ADDR                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,0(R4)          GET DSPL TO DATA                             
         ST    R0,RECDSPL                                                       
         AR    R0,BUFFREG          POINT TO RECORD BEING DELETED                
         LR    R4,R0                                                            
*                                                                               
         LH    R5,TSBRECL                                                       
         TM    TSBRECI,TSRVAR                                                   
         JZ    *+8                                                              
         ICM   R5,3,0(R4)          GET RECORD LENGTH                            
         STH   R5,RECL             SAVE RECORD LENGTH                           
         SH    R5,TSBKEYL          LESS KEY LEN GIVES DELETE LEN                
         C     R4,SAVE             TEST RECORD IS AT START OF BUFFER            
         JE    TSDEL10             YES - NO MOVE REQUIRED                       
*                                                                               
TSDEL4   LR    R0,R4               'TO' ADDR = ADDR OF DELETED RECORD           
         LR    R1,R5               'TO' LEN  = DELETE LENGTH                    
         LR    RE,R4               'FROM' ADDR = START                          
         SR    RE,R5                  - DELETE LENGTH                           
         LAM   ARE,ARE,ALET                                                     
         LR    RF,R5               'FROM' LEN = 'TO' LEN                        
         MVCL  R4,RE                                                            
         LAM   ARE,ARE,=F'0'                                                    
         LR    R4,R0               R0 UNUSABLE FOR AN AR MOVE !                 
         LR    R5,R1                                                            
         C     R4,SAVE             TEST REACHED START OF DATA YET               
         JE    TSDEL10                                                          
*                                                                               
         SR    R4,R5               BACK UP FOR NEXT                             
         C     R4,SAVE             TEST BEFORE START OF DATA                    
         JNL   TSDEL4                                                           
* SET TO MOVE FROM START OF DATA                                                
         L     R0,SAVE                                                          
         SR    R0,R4                                                            
         AR    R4,R0               ADVANCE START POINTER                        
         SR    R5,R0               DECREASE MOVE LENGTH                         
         J     TSDEL4                                                           
         EJECT                                                                  
*=======================================================*                       
* NEED TO ADJUST DISPLACEMENTS IN KEY AREA              *                       
* FOR RECORDS THAT WERE MOVED (DSPL LESS THAN THAT OF   *                       
* THE RECORD BEING DELETED)                             *                       
*=======================================================*                       
         SPACE 1                                                                
TSDEL10  LA    R4,TSBKEYS                                                       
         LH    R5,RECL                                                          
         SH    R5,TSBKEYL          THIS IS LENGTH OF RECORD REMOVED             
         SR    R0,R0                                                            
*                                                                               
TSDEL12  ICM   R0,7,0(R4)          TEST REACHED HIGH KEY                        
         JZ    TSDEL20                                                          
         C     R0,RECDSPL                                                       
         JH    TSDEL14                                                          
         AR    R0,R5                                                            
         STCM  R0,7,0(R4)                                                       
TSDEL14  AH    R4,TSBKEYL3         POINT TO NEXT KEY                            
         J     TSDEL12                                                          
*                                                                               
TSDEL20  L     R4,KEYADDR          'TO' ADDR = ADDRESS OF DELETED KEY           
         L     R5,TSBNXKEY         GET LENGTH OF KEY AREA                       
         AR    R5,BUFFREG          POINT TO END OF KEY AREA                     
         SR    R5,R4               SUBTRACT ADDR OF DELETED KEY                 
         SH    R5,TSBKEYL3         LESS KEYLEN GIVES LEN TO MOVE                
         JZ    TSDEL22                                                          
*                                                                               
         LR    RE,R4                                                            
         AH    RE,TSBKEYL3         ADD KEYLEN TO GET 'FROM' ADDR                
         LR    RF,R5               'FROM' LEN = 'TO' LEN                        
         LAM   ARE,ARE,ALET                                                     
         MVCL  R4,RE                                                            
         LAM   ARE,ARE,=F'0'                                                    
*                                                                               
TSDEL22  L     R0,TSBNXKEY         UPDATE NEXT KEY DSPL                         
         SH    R0,TSBKEYL3                                                      
         ST    R0,TSBNXKEY                                                      
*                                                                               
         L     R0,TSBNXDTA         UPDATE NEXT DATA DSPL                        
         AH    R0,RECL             RECOVER RECORD LENGTH                        
         SH    R0,TSBKEYL          LESS KEY LENGTH                              
         ST    R0,TSBNXDTA                                                      
*                                                                               
         L     R0,TSBACT           UPDATE ACTUAL BYTE COUNT                     
         SH    R0,RECL                                                          
         SHI   R0,3                                                             
         ST    R0,TSBACT                                                        
*                                                                               
         TM    TSIND2,TSI2MANY                                                  
         JO    TSDEL24                                                          
         LH    R0,TSBRECS                                                       
         BCTR  R0,0                                                             
         STH   R0,TSBRECS                                                       
         J     TSDELX                                                           
*                                                                               
TSDEL24  L     R0,TSBRECS                                                       
         BCTR  R0,0                                                             
         ST    R0,TSBRECS                                                       
*                                                                               
TSDELX   LAM   AR4,AR4,=F'0'                                                    
         J     TSX                                                              
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO GET A RECORD BY NUMBER OR NEXT RECORD BY NUMBER          *         
*=====================================================================*         
         SPACE 2                                                                
TSNXT    TM    TSIND2,TSI2MANY                                                  
         JO    TSNXT2                                                           
         LH    RF,TSRNUM                                                        
         LA    RF,1(RF)                                                         
         STH   RF,TSRNUM                                                        
         J     TSGET                                                            
*                                                                               
TSNXT2   ICM   RF,15,TSRNUM                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,15,TSRNUM                                                     
         SPACE 2                                                                
TSGET    NTR1                                                                   
         LA    RE,2-1              SET DEFAULT COMPARE LEN                      
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         LA    RE,4-1                                                           
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   TSRNUM(0),TSBRECS    TEST GREATER THAN ACTUAL                    
         JL    *+12                                                             
         OI    TSERRS,TSEEOF+TSERNF  SET EOF + NOT FOUND                        
         J     TSX                                                              
*                                                                               
         LH    R1,TSRNUM                                                        
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         ICM   R1,15,TSRNUM                                                     
         LTR   R1,R1                                                            
         JP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,TSBKEYL3         X KEYLEN+3 GIVES DSPL TO KEY                 
         LA    R4,TSBKEYS(R1)      POINT TO THE KEY                             
         ST    R4,KEYADDR                                                       
         J     TSRDH22                                                          
*                                                                               
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO READ HIGH FOR A KEY - IF KEY IS NOT FOUND TSERNF IS SET  *         
*=====================================================================*         
         SPACE 1                                                                
TSRDH    NTR1  ,                                                                
         LA    RF,DMCB                                                          
         USING BSPARA,RF                                                        
*                                                                               
         MVI   TSERRS,0            NB: TSERRS IS IN HOB OF TSAREC               
         XR    R1,R1                                                            
         ICM   R1,7,TSAREC+1       GET USER REC ADDRESS                         
         TM    TSBRECI,TSRVAR                                                   
         JZ    *+8                                                              
         LA    R1,2(R1)            POINT TO USER KEY                            
         ST    R1,SAVEKEY                                                       
*                                                                               
         LR    RE,R1                                                            
         SHI   RE,3                NEED TO SET USER KEY ADDRESS-3               
         ST    RE,BSPAREC                                                       
         LA    RE,TSBKEYS                                                       
         ST    RE,BSPSTRT                                                       
*                                                                               
         LH    RE,TSBRECS                                                       
         TM    TSIND2,TSI2MANY                                                  
         JZ    *+8                                                              
         L     RE,TSBRECS                                                       
         ST    RE,BSPNOR                                                        
         LA    RE,1(,RE)           TAB MAX 1 BIGGER TO ALLOW FOR ADD            
         ST    RE,BSPEND           SET MAX COUNT                                
*                                                                               
         LH    RE,TSBKEYL3         RECORD LENGTH = KEYLEN+3                     
         ST    RE,BSPLENR                                                       
         MVI   BSPLENR,X'02'           SET FOR READ HIGH                        
         CLI   ADDFLAG,C'Y'        IS THIS RDHI FOR AN ADD                      
         JNE   *+8                 NO                                           
         MVI   BSPLENR,X'01'           SET INSERT IF NOT FOUND                  
         OI    BSPLENR+1,X'80'     SET AR IN P7                                 
*                                                                               
         L     RE,=X'03000000'     SET DISPLACEMENT OF KEY IN HOB               
         ICM   RE,3,TSBKEYL        AND KEYLEN IN LOW BITS                       
         ST    RE,BSPLENK                                                       
*        MVC   BSPEND,=X'00FFFFFF'    SET LARGE MAX COUNT                       
         STAM  AR3,AR3,BSPARS                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RR=RELO                                         
         SAFE  CLEAR=Y                                                          
         L     RE,DM1              POINT TO WHATEVER HAS BEEN FOUND             
         LA    R1,0(RE)            CLEAR HOB                                    
         LTR   R1,R1                                                            
         JZ    TSRDH19                                                          
         ST    R1,KEYADDR          AND SAVE ITS ADDRESS                         
         EJECT                                                                  
* NEED TO COMPUTE RECORD NUMBER                                                 
*                                                                               
TSRDH12  LA    R0,TSBKEYS          POINT TO START OF DATA                       
         SR    R1,R0               GIVES DSPL TO KEY                            
         SR    R0,R0               CLEAR FOR DIVIDE                             
         LH    RE,TSBKEYL3         GET KEYLEN+3                                 
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         JZ    *+6                 CANNOT POSSIBLY HAVE REMAINDER               
         DC    H'0'                                                             
*                                                                               
         TM    TSIND2,TSI2MANY                                                  
         JO    TSRDH14                                                          
         CH    R1,TSBRECS                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
         AHI   R1,1                LOW RECORD NUMBER IS 1                       
         STH   R1,TSRNUM           SAVE RECORD NUMBER                           
         J     TSRDH16                                                          
*                                                                               
TSRDH14  C     R1,TSBRECS                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
         AHI   R1,1                                                             
         STCM  R1,15,TSRNUM                                                     
*                                                                               
TSRDH16  TM    DM1,X'80'           TEST RECORD NOT FOUND                        
         JZ    TSRDH18                                                          
*                                                                               
         OI    TSERRS,TSERNF       SET RECORD NOT FOUND                         
         CLI   TSOFFACT,TSARDH     NB -- ALWAYS RETURN RECORD IF                
         JNE   TSRDHX                 RDHI FROM CALLER                          
*                                                                               
TSRDH18  L     R4,KEYADDR          KEY COULD BE IN DATASPACE                    
         LAM   AR4,AR4,ALET                                                     
         OC    0(3,R4),0(R4)       TEST VALID DATA DSPL                         
         LAM   AR4,AR4,=F'0'       FYI: LAM DOES NOT AFFECT CC -                
         JNZ   TSRDH20                  BUT YOU BETTER RESET IT!!!              
*                                                                               
TSRDH19  OI    TSERRS,TSEEOF+TSERNF  SET EOF + NOT FOUND                        
         J     TSRDHX                HOPEFULLY THAT'S ENOUGH                    
         EJECT                                                                  
         SPACE 1                                                                
TSRDH20  CLI   TSOFFACT,TSARDH     TEST READ HIGH                               
         JNE   TSRDH22             NO                                           
* MAY NEED TO SET RECORD NOT FOUND FOR IDIOTIC DROOL                            
         LH    R1,TSBKEYL1                                                      
         L     RE,SAVEKEY          GET ADDR OF KEYARG                           
         L     R4,KEYADDR          RECORD BACK FROM BINSRCH                     
         LAM   AR4,AR4,ALET                                                     
         EXRL  R1,TSRDHCMP         COMPARE KEYS                                 
         LAM   AR4,AR4,=F'0'                                                    
         JE    *+8                                                              
         OI    TSERRS,TSERNF                                                    
         J     TSRDH30                                                          
*                                                                               
TSRDHCMP CLC   0(0,RE),3(R4)       *EXECUTED*                                   
*                                                                               
TSRDH22  CLI   TSOFFACT,TSAGET     TEST GET                                     
         JE    TSRDH30             YES - PASS RECORD                            
         CLI   TSOFFACT,TSANXT     TEST NEXT                                    
         JE    TSRDH30             YES - PASS RECORD                            
         J     TSRDHX              ELSE EXIT                                    
*                                                                               
TSRDH30  L     R4,KEYADDR          ADDRESS KEY IN DATASPACE                     
         LAM   AR4,AR4,ALET                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TSAREC+1       GET USER REC ADDRESS                         
         TM    TSBRECI,TSRVAR                                                   
         JZ    *+8                                                              
         LA    R1,2(R1)            POINT TO USER KEY                            
         LH    RF,TSBKEYL1                                                      
         EXRL  RF,*+10             MOVE KEY TO USER RECORD AREA                 
         J     *+10                                                             
         MVC   0(0,R1),3(R4) *EXECUTED*                                         
*                                                                               
TSRDH32  XR    RE,RE                                                            
         ICM   RE,7,0(R4)          GET DSPL TO DATA                             
         JZ    TSRDHX              EXIT IF NO DATA                              
         AR    RE,BUFFREG          ADD BUFFER ADDR TO POINT TO RECORD           
         LR    R4,RE                                                            
         EJECT                                                                  
*=========================================================*                     
*     KEY AREA HAS    - DSPL-  -- KEY --                  *                     
*                      0  1  2  3  4  5  6  ...           *                     
*                                                         *                     
*     DATA AREA HAS  (LN LN) DATA ... *                   *                     
*                    ( 0  1) 2  3  4  5 ...               *                     
*     THIS LENGTH INCLUDES KEY.                           *                     
*                                                         *                     
*     USER AREA GETS  (LN LN) KEY      ...   DATA         *                     
*                     (0  1 ) 2  3  4  ...                *                     
*=========================================================*                     
         SPACE 1                                                                
         TM    TSBRECI,TSRVAR                                                   
         JZ    TSRDH34                                                          
         SPACE 1                                                                
*====================================================*                          
* VARIABLE LENGTH RECORDS                            *                          
*====================================================*                          
         SPACE 1                                                                
         SHI   R1,2                                                             
         MVC   0(2,R1),0(R4)       MOVE RECORD LENGTH IN FRONT OF KEY           
*                                                                               
         LA    R0,2(R1)            POINT TO START OF KEY                        
         AH    R0,TSBKEYL          AND ADD KEY LENGTH                           
         SR    R1,R1                                                            
         ICM   R1,3,0(R4)          GET RECORD LENGTH                            
         SH    R1,TSBKEYL          LESS KEY LENGTH                              
         SHI   R1,2                ADJUST FOR RECORD LEN                        
         LA    R4,2(R4)            POINT TO START OF RECORD DATA                
         LR    R5,R1               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,R4                                                            
         J     TSRDHX                                                           
         SPACE 1                                                                
*====================================================*                          
* FIXED LENGTH RECORDS                               *                          
*====================================================*                          
         SPACE 1                                                                
TSRDH34  LR    R0,R1                                                            
         AH    R0,TSBKEYL          POINT TO START OF DATA                       
         LH    R1,TSBRECL                                                       
         SH    R1,TSBKEYL                                                       
         LR    R5,R1                                                            
         MVCL  R0,R4               MOVE RECORD TO CALLER'S I/O AREA             
*                                                                               
TSRDHX   LAM   AR4,AR4,=F'0'                                                    
         J     TSX                                                              
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMRSRV   DC    C'DMRSRV '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
TEMPEST  DC    C'TEMPEST'                                                       
         SPACE 1                                                                
ACTNTAB  DS    0XL4                                                             
         DC    AL1(TSAADD),AL3(TSADD)                                           
         DC    AL1(TSARDH),AL3(TSRDH)                                           
         DC    AL1(TSAGET),AL3(TSGET)                                           
         DC    AL1(TSANXT),AL3(TSNXT)                                           
         DC    AL1(TSAPUT),AL3(TSPUT)                                           
         DC    AL1(TSAWRT),AL3(TSWRT)                                           
         DC    AL1(TSADEL),AL3(TSDEL)                                           
         DC    AL1(TSAINI),AL3(TSINI)                                           
         DC    AL1(0)                                                           
         SPACE 1                                                                
ARZERO   DC    16F'0'                                                           
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         EJECT                                                                  
TSBUFFD  DSECT                     = TSAR BUFFER HEADER =                       
         DS    XL32                TSAR EYECATCHER                              
TSBPGTAB DS    XL(4*32)            3 BYTES DSPL/1 BYTE INDS                     
*                                  NO IND BITS DEFINED                          
*                                                                               
TSBNXKEY DS    F                   DSPL TO NEXT KEY                             
TSBNXDTA DS    F                   DSPL TO NEXT DATA                            
TSBACT   DS    F                   NUMBER OF BYTES IN USE                       
TSBMAX   DS    F                   TOTAL BYTES AVAILABLE                        
TSBSTAT  DS    X                   STATUS BYTE                                  
TSBRECI  DS    X                   SAVED TSRECI AT INIT                         
         DS    XL2                                                              
*                                                                               
TSBKKK1  DS    F                   C'KLNS'                                      
TSBKEYL  DS    H                   KEY LENGTH                                   
TSBKEYL1 DS    H                   KEYLEN -1                                    
TSBKEYL3 DS    H                   KEYLEN +3                                    
TSBRECL  DS    H                   RECORD LENGTH (IF VARIABLE=MAX LEN)          
TSBKKK2  DS    F                   C'RECS'                                      
TSBRECS  DS    H                   NUMBER OF RECORDS IN BUFFER                  
TSBRECSX DS    H                   USED FOR EXTENDED NUMBER OF RECORDS          
         DS    0D                                                               
TSBXCLEN EQU   *-TSBPGTAB                                                       
*                                                                               
TSBKEYS  DS    0D                  START OF KEY DATA                            
*                                  3 BYTE DSPL FOLLOWED BY KEY                  
*                                  X'80' IN DSPL FOR DELETED RECORD             
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         SPACE 1                                                                
TSAWRK   DSECT                     = TSAR WORKING STORAGE =                     
RELO     DS    A                                                                
TDATAMGR DS    V                                                                
TCALLOV  DS    V                                                                
ALET     DS    A                                                                
DMCB     DS    0XL24                                                            
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
DM7      DS    F                                                                
DM8      DS    F                                                                
AREC     DS    A                                                                
KEYADDR  DS    A                                                                
SAVE     DS    A                                                                
SAVEKEY  DS    A                                                                
SAVERE   DS    A                                                                
RECDSPL  DS    F                                                                
PAGESIZE DS    H                                                                
TERM     DS    H                                                                
RECL     DS    H                                                                
FLAG     DS    X                                                                
ERRS     DS    X                                                                
ADDFLAG  DS    X                                                                
FILE     DS    CL7                                                              
TSAWRKX  EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018DDTSAROFF 11/26/12'                                      
         END                                                                    
