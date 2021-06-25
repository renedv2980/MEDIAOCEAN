*          DATA SET SPREPTF02  AT LEVEL 025 AS OF 05/12/98                      
*PHASE SPTF02A                                                                  
         TITLE 'SPTF02 - RECOMPUTE GOALS FOR TALENT FACTOR CHANGE'              
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*-DATE---------BY------------------CHANGE-----------------------------*         
*                                                                     *         
* 03/03/98     NRK                 INITIAL RELEASE                    *         
*                                                                     *         
* 04/08/98     NRK                 ADD DATE TO CSO RECORD KEY READ IF *         
*                                  INPUT IN REQUEST.              NRK *         
***********************************************************************         
SPTF02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPTF02,R8                                                      
*                                                                               
         L     RA,0(R1)            STANDARD                                     
         USING SPWORKD,RA,R9         SPONSOR                                    
         LA    R9,2048(RA)             OPENING                                  
         LA    R9,2048(R9)               STUFF                                  
*                                                                               
* MAINLINE                                                                      
*                                                                               
         CLI   MODE,REQFRST        FIRST TIME THRU?                             
         BNE   MAIN0050            NO - SO CONTINUE                             
*                                                                               
         BAS   RE,INITIAL          DO WHATEVER INITIAL STUFF NEEDED             
         B     MAINEXIT            AND LEAVE                                    
*                                                                               
MAIN0050 EQU   *                                                                
*                                                                               
         CLI   MODE,CLTFRST        FIRST TIME FOR CLIENT?                       
         BNE   MAIN0100            NO - SO CONTINUE                             
*                                                                               
         BAS   RE,PROCESS          ELSE - GO PROCESS DATA                       
         GOTO1 AENDREQ             AND ALL DONE                                 
*                                                                               
MAIN0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
MAINEXIT EQU   *                                                                
*                                                                               
         XMOD1                                                                  
*                                                                               
MAINERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     MAINEXIT            AND EXIT                                     
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DOES THE INITIALIZATION FOR THE REPORT                           
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE VARIOUS WORK AREA VARIABLES                                        
*                                                                               
         LA    RE,MESTTBL          SET UP TO INITIALIZE THE                     
         LA    RF,LMESTTBL         MASTER/SUB ESTIMATE TABLE                    
         XCEF                                                                   
         XC    FLAG,FLAG           INITIALIZE PROGRAM FLAGS                     
*                                                                               
         LA    R0,MYREC            GET A(MY IO AREA)                            
         ST    R0,AMYREC           AND STORE IT                                 
*                                                                               
* DO OTHER STUFF HERE                                                           
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE DOES THE MAIN PROCESSING                                         
*                                                                               
PROCESS  NTR1                                                                   
         BAS   RE,READCSO          READ CSO MASTER EST LIST REC                 
         BNZ   PROCERR             ERROR FOUND                                  
*                                                                               
* NOW LOOP THROUGH THE MASTER/ESTIMATE LIST TABLE FILLING IN THE                
* NECESSARY DATA FOR THE MASTER AND SUB ESTIMATES.                              
*                                                                               
         LA    R5,MESTTBL          R5 = A(MASTER/SUB EST TABLE)                 
         USING MESTD,R5                                                         
*                                                                               
PROC0100 EQU   *                                                                
*                                                                               
         CLI   MEST,0              ANY MASTER ESTS LEFT IN TABLE?               
         BE    PROC0200            NO - SO CONTINUE                             
*                                                                               
         BAS   RE,READMEST         ELSE - READ THE MASTER ESTIMATE              
         BNZ   PROCERR             ERROR FOUND                                  
*                                                                               
         BAS   RE,READSEST         READ THE SUB ESTIMATE(S)                     
         BNZ   PROCERR             ERROR FOUND                                  
*                                                                               
         LA    R5,LMESTD(R5)       INC TO NEXT TABLE ENTRY                      
         B     PROC0100            AND LOOP BACK                                
         DROP R5                                                                
*                                                                               
PROC0200 EQU   *                                                                
*                                                                               
         BAS   RE,READGOAL         READ THE GOALS (MAIN PROCESSING)             
         BNZ   PROCERR             ERROR FOUND                                  
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
PROCEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
PROCERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     PROCEXIT            AND EXIT                                     
*                                                                               
* THIS ROUTINE BUILDS THE LIST OF MASTER AND SUB ESTIMATES                      
*                                                                               
READCSO  NTR1                                                                   
*                                                                               
* FIND THE LAST CSO MASTER ESTIMATE LIST RECORD FOR A-M/CLT ON FILE             
*                                                                               
* BUILD FOR 1ST KEY                                                             
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0D'           CSO KEY TYPE                                 
         MVI   KEY+1,X'6F'         SUB-TYPE FOR MASTER EST LIST                 
         L     R6,ADCLT            POINT TO CLIENT RECORD                       
         MVC   KEY+2(3),1(R6)      MOVE A-M/CLT                                 
*                                                                   NRK         
         LA    R2,4                SET L(KEY) THRU CLIENT FOR CLC   NRK         
         CLC   QSTART,SPACES       ANY START DATE?                  NRK         
         BNH   RCSO0100            NO - SO CONTINUE                 NRK         
*                                                                   NRK         
         MVC   KEY+5(4),QSTART     ELSE - MOVE IN YYMM              NRK         
         LA    R2,4(R2)            INC L(KEY) FOR DATE              NRK         
*                                                                   NRK         
RCSO0100 EQU   *                                                    NRK         
*                                                                   NRK         
         GOTO1 HIGH                READ THE KEY                                 
*                                                                               
         EX    R2,RCSO0200         EXECUTE THE CLC                  NRK         
         B     RCSO0250            AND CONTINUE                     NRK         
*                                                                   NRK         
RCSO0200 EQU   *                                                    NRK         
*                                                                   NRK         
         CLC   KEY(0),KEYSAVE      MATCH?                           NRK         
*                                                                   NRK         
RCSO0250 EQU   *                                                    NRK         
*                                                                   NRK         
         BE    RCSO0300            YES - SO CONTINUE                            
*                                                                               
         DC    H'0'                NO MASTER EST LIST REC FOUND                 
*                                                                               
RCSO0300 EQU   *                                                                
*                                                                               
         MVC   KEYSAVE(13),KEY     SAVE KEY THRU SEQ READ                       
         GOTO1 SEQ                 GET NEXT SEQUENTIAL KEY                      
*                                                                               
         EX    R2,RCSO0400         EXECUTE THE CLC                  NRK         
         B     RCSO0450            AND CONTINUE                     NRK         
*                                                                   NRK         
RCSO0400 EQU   *                                                    NRK         
*                                                                   NRK         
         CLC   KEY(0),KEYSAVE      MATCH?                           NRK         
*                                                                   NRK         
RCSO0450 EQU   *                                                    NRK         
*                                                                   NRK         
         BE    RCSO0300            YES - SO GET NEXT KEY                        
*                                                                               
         MVC   KEY(13),KEYSAVE     ELSE - RESTORE LAST KEY FOUND                
         GOTO1 HIGH                AND RE-READ IT                               
*                                                                               
         GOTO1 GETBUY              READ MASTER EST INTO BUY IO AREA             
*                                                                               
* NOW BUILD TABLE OF MASTER EST, SUB ESTIMATES                                  
*                                                                               
         L     R6,ADBUY            R6 = A(MASTER EST LIST RECORD)               
         USING MASRECD,R6                                                       
*                                                                   NRK         
         CLC   QSTART,SPACES       ANY START DATE?                  NRK         
         BH    RCSO0500            YES - SO CONTINUE                NRK         
*                                                                   NRK         
         MVC   QSTART(4),MASKDATE  ELSE - MOVE IN DATE FROM KEY                 
*                                                                   NRK         
RCSO0500 EQU   *                                                    NRK         
*                                                                   NRK         
         LA    R6,MELLIST          NOW R6 = A(MASTER EST LIST)                  
         USING MESTLSTD,R6                                                      
         LA    R5,MESTTBL          R5 = A(MASTER EST LIST TABLE)                
         USING MESTD,R5                                                         
*                                                                               
RCSO0600 EQU   *                                                                
*                                                                               
         CLI   MESTNUM,0           END OF LIST?                                 
         BE    RCSO0800            YES - SO CONTINUE                            
*                                                                               
         MVC   MEST,MESTNUM        MOVE MASTER EST # TO TABLE                   
         LA    R2,L'MESTSUBS       # OF POSSIBLE SUB ESTS                       
         LA    R7,MESTSUBS         R7 = A(SUB EST LIST)                         
         LA    R4,SUBEST           R4 = A(SUB EST IN TABLE)                     
*                                                                               
RCSO0700 EQU   *                                                                
*                                                                               
         MVC   0(1,R4),0(R7)       MOVE IN THE SUB ESTIMATE #                   
         LA    R7,1(R7)            INC TO NEXT EST IN LIST                      
         LA    R4,LSUBEST(R4)      INC TO NEXT EST IN TABLE                     
         BCT   R2,RCSO0700         AND GET NEXT SUB ENTRY                       
*                                                                               
         LA    R5,LMESTD(R5)       INC TO NEXT TABLE ENTRY                      
         LA    R6,MESTLSTL(R6)     INC TO NEXT LIST ENTRY                       
         B     RCSO0600            AND CONTINUE                                 
*                                                                               
RCSO0800 EQU   *                                                                
*                                                                               
*                                                                               
RCSODONE EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RCSOEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RCSOERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RCSOEXIT            AND RETURN                                   
         DROP  R5,R6                                                            
*                                                                               
* THIS ROUTINE READS THE GOALS FOR THE LIST OF MASTER AND SUB                   
* ESTIMATES AND PROCESSES THE DATA.                                             
*                                                                               
READGOAL NTR1                                                                   
         LA    R5,MESTTBL          A(MASTER/SUB ESTIMATES)                      
         USING MESTD,R5                                                         
         LA    R1,P                GET A(PRINT LINE)                            
         ST    R1,APRNTPOS         AND STORE IT                                 
         OI    FLAG,FIRST          SET 'FIRST PASS' FLAG BIT                    
*                                                                               
RGOL0100 EQU   *                                                                
*                                                                               
         XR    R7,R7               INITIALIZE TEMP RECORD COUNTER               
*                                                                               
         CLI   MEST,0              END OF LIST?                                 
         BE    RGOLDONE            YES - SO ALL DONE                            
*                                                                               
         MVC   REQEST,MEST         STORE THE REQUESTED MASTER EST               
*                                                                               
* BUILD THE BASE KEY (A/M, CLIENT)                                              
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'02'           GOAL KEY ID                                  
         L     R6,ADCLT            POINT TO CLIENT RECORD                       
         MVC   KEY+1(3),1(R6)      MOVE A-M/CLT                                 
*                                                                               
RGOL0200 EQU   *                                                                
*                                                                               
         BAS   RE,SKIPREAD         AND GO GET IT                                
         BNZ   RGOL1300            NOT FOUND - SO CONTINUE                      
*                                                                               
* ELSE - PROCESS THE MASTER EST GOAL RECORD                                     
*                                                                               
         LA    R7,1(R7)            INC TEMP REC COUNTER                         
*                                                                               
         L     R6,ADGOAL           R6 = A(MASTER EST GOAL RECORD)               
         USING GOALD,R6                                                         
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RGOL0300            NO - SO CONTINUE                             
*                                                                               
         GOTO1 REPORT              BLANK LINE                                   
         MVC   P(23),=C'MASTER EST GOAL RECORD '                                
         GOTO1 HEXOUT,DMCB,GKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
RGOL0300 EQU   *                                                                
*                                                                               
         MVC   MYKEY(13),KEY       SAVE THE MASTER EST GOAL KEY                 
         MVC   SAVPRDNO,GKEYPRD    SAVE THE PRODUCT NUMBER                      
*                                                                               
         BAS   RE,READPRD          READ THE PRODUCT                             
         BNZ   RGOL1200            ERROR - GET NEXT MASTER                      
*                                                                               
         BAS   RE,READTAL          READ THE TALENT FACTOR                       
         BNZ   RGOL1200            ERROR - GET NEXT MASTER                      
*                                                                               
* GETEL LOOP TO READ THE PREBUY BUDGET (X'40') ELEMENTS                         
*                                                                               
         XC    GLBUDTBL(LGLBDTBL),GLBUDTBL INITIALIZE THE TABLE                 
         LA    R3,GLBUDTBL                                                      
         USING GLBUDD,R3                                                        
*                                                                               
         L     R2,ADGOAL           SET A(GOAL RECORD)                           
         MVI   ELCODE,X'40'        SET THE ELEMET CODE                          
         BAS   RE,GETEL            GET THE ELEMENT                              
         BZ    RGOL0500            1ST ELEMENT FOUND - CONTINUE                 
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(32),=C'***NO BUDGET FOUND FOR PRODUCT: '                       
         MVC   P+32(3),SAVPRDCD    PRODUCT CODE                                 
         MVC   P+35(9),=C' MARKET: '                                            
         EDIT  GKEYMKT,(4,P+44)    MARKET NUMBER                                
         MVC   P+48(18),=C' MASTER ESTIMATE: '                                  
         EDIT  GKEYEST,(3,P+66)    MASTER ESTIMATE                              
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RGOL1200            AND GET NEXT MASTER                          
*                                                                               
RGOL0400 EQU   *                                                                
*                                                                               
         BAS   RE,NEXTEL           GET THE NEXT ELEMENT                         
         BNZ   RGOL0700            NONE LEFT - CONTINUE                         
*                                                                               
RGOL0500 EQU   *                                                                
*                                                                               
         USING GLBUDEL,R2                                                       
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RGOL0600            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(18),=C'OLD BUDGET ELEMENT '                                  
         GOTO1 HEXOUT,DMCB,GLBUDCOD,P+21,16,0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
RGOL0600 EQU   *                                                                
*                                                                               
* NOTE: THIS CODE IS STOLEN FROM SPGOL23 AT LABLE 'GLBUD10'.  ANY               
* BUGS FOUND HERE SHOULD BE INVESTIGATED THERE AS WELL.                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVTLNT+2       GET MULT FACTOR (10000)                      
         L     R1,GLBUDACT                                                      
         AR    R1,R1               X 2                                          
         MR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,3,SVTLNT                                                      
         DR    R0,RE                                                            
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
         ST    R1,GLBUDDOL                                                      
*                                                                               
* END OF STOLEN CODE                                                            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RGOL0650            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(18),=C'NEW BUDGET ELEMENT '                                  
         GOTO1 HEXOUT,DMCB,GLBUDCOD,P+21,16,0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
RGOL0650 EQU   *                                                                
*                                                                               
* FILL THE BUDGET ELEMENT DATA INTO THE TABLE                                   
*                                                                               
         MVC   GLBDAT,GLBUDDAT     START DATE TO TABLE                          
         MVC   GLBEND,GLBUDEND     END DATE                                     
         PACK  DUB,GLBUDTR         PACK THE TIER                                
         CVB   R1,DUB              CONVERT TO BINARY                            
         STC   R1,GLBTR            STORE IN TABLE                               
         MVC   GLBDOL,GLBUDDOL     BUDGET DOLLARS                               
*                                                                               
         LA    R3,LGLBUDD(R3)      INC TO NEXT TABLE ENTRY                      
         B     RGOL0400            AND GET NEXT BUDGET ELEMENT                  
         DROP  R2                                                               
*                                                                               
RGOL0700 EQU   *                                                                
*                                                                               
* RE-WRITE THE MASTER GOAL                                                      
*                                                                               
         MVC   GACTDATE,TODAYP     SET ACTIVITY DATE                            
*                                                                               
         MVC   KEY(13),GKEY        RE-SET GOAL KEY                              
         GOTO1 HIGH                RE-READ THE KEY                              
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES - SO CONTINUE                            
*                                                                               
         DC    H'0'                ELSE - CRITICAL ERROR                        
*                                                                               
         MVC   AREC,AMYREC         SET A(TEMP I/O AREA)                         
         GOTO1 GET                 GET THE RECORD                               
*                                                                               
         CLI   QOPT4,C'Y'          TEST MODE?                                   
         BE    RGOL0750            YES - SO CONTINUE                            
*                                                                               
         GOTO1 PUTGOAL             ELSE - RE-WRITE THE GOAL                     
*                                                                               
RGOL0750 EQU   *                                                                
*                                                                               
         BAS   RE,BLDBKTS          BUILD THE WEEKLY BUCKETS                     
         BNZ   RGOL1200            ERROR - GO GET NEXT MASTER                   
*                                                                               
         BAS   RE,READTIER         READ TIERS AND STORE $ IN BKTS               
         BNZ   RGOL1200            ERROR - GO GET NEXT MASTER                   
*                                                                               
         BAS   RE,EXPLODE          EXPLODE THE DATA                             
*                                                                               
* PRINT THE DATA                                                                
*                                                                               
***      CLI   QOPT5,C'Y'          TRACE MODE?                                  
***      BE    RGOL1200            YES - SO SKIP PRINTING THE LINE              
*                                                                               
         TM    FLAG,FIRST          FIRST PASS?                                  
         BZ    RGOL0800            NO - SO CONTINUE                             
*                                                                               
         NI    FLAG,X'FF'-FIRST    ELSE - RESET 'FIRST PASS' FLAG               
         MVC   OLDEST(1),MYKEY+7   SET 'PREVIOUS EST'                           
         MVC   OLDPROD(3),SAVPRDCD SET 'PREVIOUS PRODUCT CODE'                  
         B     RGOL1100            AND CONTINUE                                 
*                                                                               
RGOL0800 EQU   *                                                                
*                                                                               
         CLC   OLDEST(1),MYKEY+7   MASTER ESTIMATE CHANGE?                      
         BE    RGOL0900            NO - SO CONTINUE                             
*                                                                               
         GOTO1 REPORT              ELSE - PRINT THE OLD LINE                    
         MVI   FORCEHED,C'Y'       FORCE A PAGE BREAK                           
         MVC   OLDEST(1),MYKEY+7   SET 'PREVIOUS EST'                           
         B     RGOL1000            AND CONTINUE                                 
*                                                                               
RGOL0900 EQU   *                                                                
*                                                                               
         CLC   OLDPROD(3),SAVPRDCD PRODUCT CODE CHANGE?                         
         BE    RGOL1100            NO - SO CONTINUE                             
*                                                                               
         GOTO1 REPORT              ELSE - PRINT THE OLD LINE                    
         MVI   P,0                 SET UP TO SKIP A LINE                        
         GOTO1 REPORT              AND SKIP A LINE                              
*                                                                               
RGOL1000 EQU   *                                                                
*                                                                               
         MVC   OLDPROD(3),SAVPRDCD SET 'PREVIOUS PRODUCT CODE'                  
         LA    R1,P                RE-SET PRINT POSITION                        
         ST    R1,APRNTPOS         AND STORE IT                                 
*                                                                               
RGOL1100 EQU   *                                                                
*                                                                               
         L     R2,APRNTPOS         R1 = A(CURRENT PRINT POSITION)               
         USING PRINTD,R2                                                        
*                                                                               
         MVC   PPRODUCT(3),SAVPRDCD PRODUCT CODE                                
         GOTO1 CENTER,DMCB,PPRODUCT,L'PPRODUCT                                  
         EDIT  (2,MYKEY+5),(4,PMARKET) MARKET NUMBER                            
         GOTO1 CENTER,DMCB,PMARKET,L'PMARKET                                    
         EDIT  (1,MYKEY+7),(3,PMASTER) MASTER ESTIMATE                          
         GOTO1 CENTER,DMCB,PMASTER,L'PMASTER                                    
*                                                                               
         LA    R2,LPRINTD(R2)      INC TO NEXT PRINT POSITION                   
         ST    R2,APRNTPOS         AND STORE IT                                 
*                                                                               
         LA    R0,P+(4*LPRINTD)    R0 = A(LAST PRINT POSITION)                  
         CL    R0,APRNTPOS         ARE WE PAST END OF THE PRINT LINE?           
         BH    RGOL1200            NO - SO DON'T PRINT YET                      
*                                                                               
         GOTO1 REPORT              ELSE - PRINT THE LINE                        
         LA    R1,P                RE-SET PRINT POSITION                        
         ST    R1,APRNTPOS         AND STORE IT                                 
         DROP  R2                                                               
*                                                                               
RGOL1200 EQU   *                                                                
*                                                                               
* SET UP TO GET THE NEXT MARKET FOR THIS MASTER ESTIMATE GOAL RECORD            
*                                                                               
         MVC   KEY(13),MYKEY       RESTORE THE MASTER EST GOAL KEY              
*                                                                               
         C     R7,=F'10'           10TH RECORD?                                 
***      BNL   RGOL1300            YES - SKIP TO NEXT MASTER ESTIMATE           
         BNE   RGOL1250            NO - SO CONTINUE                             
*                                                                               
         MVI   QOPT5,C'N'          ELSE - TURN OFF TRACE BIT                    
*                                                                               
RGOL1250 EQU   *                                                                
*                                                                               
*                                                                               
         ZICM  R6,KEY+5,(3)        R6 = MKT #                                   
         LA    R6,1(R6)            ADD 1                                        
         STCM  R6,3,KEY+5          PUT NEW MKT # IN KEY                         
         XC    KEY+7(6),KEY+7      CLEAR THE REST OF THE KEY                    
*                                                                               
         B     RGOL0200            LOOP BACK TO PROCESS IT                      
*                                                                               
RGOL1300 EQU   *                                                                
*                                                                               
         LA    R5,LMESTD(R5)       INC TO NEXT TABLE ENTRY                      
         B     RGOL0100            AND GET NEXT MASTER EST GOAL                 
*                                                                               
RGOLDONE EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RGOLEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RGOLERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RGOLEXIT            AND RETURN                                   
         DROP  R3,R5,R6                                                         
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE WILL SKIP-READ THROUGH GOALS UNTIL IF FINDS A GOAL               
* FOR THE REQUESTED ESTIMATE.                                                   
*                                                                               
* I/P: REQEST = ESTIMATE CODE      OP: A(GOAL REC) IN ADGOAL                    
*                                                                               
* CC .EQ. 0 = GOAL FOUND                                                        
*    .NE. 0 = NO GOAL FOUND FOR ESTIMATE                                        
*                                                                               
SKIPREAD NTR1                                                                   
*                                                                               
SKIP0100 EQU   *                                                                
*                                                                               
         GOTO1 HIGH                READ THE KEY                                 
         B     SKIP0200            BRANCH OVER SEQ READ                         
*                                                                               
SKIP0150 EQU   *                                                                
*                                                                               
         GOTO1 SEQ                 GET NEXT SEQUENTIAL KEY                      
*                                                                               
SKIP0200 EQU   *                                                                
*                                                                               
         CLC   KEY(4),KEYSAVE      MATCH THRU CLIENT?                           
         BNE   SKIPNONE            NO - SO NONE FOUND FOR THIS EST              
*                                                                               
         CLC   KEY+7(1),REQEST     KEY FOR REQUESTED ESTIMATE?                  
         BE    SKIP0400            YES - SO CONTINUE                            
*                                                                               
* ELSE - SKIP READ KEYS FOR CORRECT ESTIMATE                                    
*                                                                               
         BL    SKIP0300            KEY IS BEFORE REQUESTED EST                  
*                                                                               
* KEY IS AFTER REQUESTED EST, SO ADD 1 TO MARKET, ZERO END OF KEY,              
* AND READ HIGH TO GET FIRST KEY FOR NEXT MARKET                                
*                                                                               
         ZICM  R6,KEY+5,(3)        R6 = MKT #                                   
         LA    R6,1(R6)            ADD 1                                        
         STCM  R6,3,KEY+5          PUT NEW MKT # IN KEY                         
         XC    KEY+7(6),KEY+7      CLEAR THE REST OF THE KEY                    
         B     SKIP0100            AND GO READ THE KEY                          
*                                                                               
SKIP0300 EQU   *                                                                
*                                                                               
* KEY IS BEFORE REQUESTED EST, SO PLUG INTO KEY, ZERO THE REST OF               
* THE KEY AND GO READ HIGH FOR THE SPECIFIC EST.                                
*                                                                               
         MVC   KEY+7(1),REQEST     PUT THE ESTIMATE INTO THE KEY                
         XC    KEY+8(5),KEY+8      CLEAR THE REST OF THE KEY                    
         B     SKIP0100            AND GO READ THE KEY                          
*                                                                               
SKIP0400 EQU   *                                                                
*                                                                               
         CLI   KEY+11,X'40'        'TIER' TYPE GOAL?                            
         BE    SKIP0150            YES - GO GET NEXT SEQUENTIAL KEY             
*                                                                               
         GOTO1 GETGOAL             ELSE - READ REQUESTED EST GOAL               
*                                                                               
         XR    R0,R0               SET 'GOAL FOUND' CC                          
*                                                                               
SKIPEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
SKIPNONE EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET 'NO GOALS FOUND' CC                      
         B     SKIPEXIT            AND RETURN                                   
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE SCANS THE CLIENT RECORD FOR THE PRODUCT CODE, THEN               
* BUILDS THE PRODUCT RECORD TO GET THE TALEN FACTOR DATA.                       
*                                                                               
READPRD  NTR1                                                                   
         L     R6,ADCLT            GET A(CLIENT RECORD)                         
         USING CLIENTD,R6                                                       
         LA    R5,CLIST            R5 = A(PROD CODE LIST)                       
         USING CLISTD,R5                                                        
         LA    R1,CCLTIFC          R1 = A(1ST FIELD AFTER CLIST)                
*                                                                               
RPRD0100 EQU   *                                                                
*                                                                               
         CLC   SAVPRDNO,PRDNUM     PRODUCT MATCH?                               
         BE    RPRD0200            YES - SO CONTINUE                            
*                                                                               
         LA    R5,LCLISTD(R5)      ELSE - INC TO NEXT PROD NAME/# PAIR          
         LA    R0,PRDNUM           R0 = A(PRODUCT NUMBER FIELD)                 
         CR    R0,R1               PAST END OF LIST?                            
         BL    RPRD0100            NO - SO PROCESS NEXT ENTRY                   
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                PROD NOT IN CLIENT LIST                      
*                                                                               
         MVC   P(13),=C'M**PRODUCT X'''                                         
         GOTO1 HEXOUT,DMCB,SAVPRDNO,P+13,1,0                                    
         MVC   P+15(20),=C''' NOT IN CLIENT LIST'                               
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RPRDERR             AND GO TO ERROR                              
*                                                                               
RPRD0200 EQU   *                                                                
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(3),CKEYAM     MOVE IN AGY/MED, CLIENT                      
         MVI   KEY,X'00'           SET UP PRODUCT REC KEY ID                    
         MVC   KEY+4(3),PRDCODE    MOVE IN ALPHA CODE                           
         XC    KEY+7(6),KEY+7      CLEAR END OF KEY                             
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    RPRD0300            YES - SO CONTINUE                            
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                PRODUCT REC NOT FOUND                        
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(22),=C'***PRODUCT RECORD FOR '                                 
         MVC   P+22(3),PRDCODE                                                  
         MVC   P+25(17),=C' NOT FOUND'                                          
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RPRDERR             AND GO TO ERROR                              
         DROP  R5                                                               
*                                                                               
RPRD0300 EQU   *                                                                
*                                                                               
         GOTO1 GETPRD              READ PRODUCT RECORD                          
         L     R6,ADPRD            R6 = A(PRODUCT RECORD)                       
         USING PRODUCTD,R6                                                      
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RPRD0350            NO - SO CONTINUE                             
*                                                                               
         MVC   P(15),=C'PRODUCT RECORD '                                        
         GOTO1 HEXOUT,DMCB,PKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT                                                           
*                                                                               
RPRD0350 EQU   *                                                                
*                                                                               
         MVC   SAVTAL,PTAL         SAVE THE TALENT FACTOR GROUP                 
         MVC   SAVPRDCD,PKEYPRD    SAVE THE PROD CODE J.I.C.                    
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RPRDEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RPRDERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RPRDEXIT            AND RETURN                                   
         DROP  R6                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS THE MASTER ESTIMATE RECORD AND SAVES THE                   
* ESTIMATE START AND END DATES.                                                 
*                                                                               
* NOTE: R5 = A(CURRENT MESTTBL ENTRY) - DON'T MUCK WITH IT!                     
*                                                                               
READMEST NTR1                                                                   
         USING MESTD,R5                                                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'00'           MOVE IN ESTIMATE KEY ID                      
         L     R6,ADCLT            POINT TO CLIENT RECORD                       
         MVC   KEY+1(3),1(R6)      MOVE A-M/CLT                                 
         MVC   KEY+4(3),=C'POL'    MOVE IN 'POL' PRODUCT CODE                   
         MVC   KEY+7(1),MEST       MOVE IN ESTIMATE NUMBER                      
*                                                                               
         GOTO1 HIGH                READ THE ESTIMATE                            
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE RMES0100               YES - SO CONTINUE                            
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                MASTER EST NOT FOUND                         
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(19),=C'***MASTER ESTIMATE '                                    
         EDIT  MEST,(3,P+19)       EDIT OUT THE MASTER EST                      
         MVC   P+22(28),=C' NOT ON FILE FOR PRODUCT POL'                        
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RMESERR             AND GO TO ERROR                              
*                                                                               
RMES0100 EQU   *                                                                
*                                                                               
         GOTO1 GETEST              READ THE ESTIMATE                            
         L     R6,ADEST                                                         
         USING ESTD,R6                                                          
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RMES0150            NO - SO CONTINUE                             
*                                                                               
         MVC   P(23),=C'MASTER ESTIMATE RECORD '                                
         GOTO1 HEXOUT,DMCB,EKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT                                                           
*                                                                               
RMES0150 EQU   *                                                                
*                                                                               
         MVC   MESTART,ESTART      STORE THE MASTER EST START                   
         MVC   MEEND,EEND             AND END DATES IN THE TABLE                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RMESEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RMESERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RMESEXIT            AND RETURN                                   
         DROP  R5,R6                                                            
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS THE SUB ESTIMATES FOR THE MASTER AND STORES                
* THE START AND END DATES IN 2-BYTE COMPRESSED FORMAT INTO THE TABLE.           
*                                                                               
* NOTE: R5 = A(CURRENT MESTTBL ENTRY) - DON'T MUCK WITH IT!                     
*                                                                               
READSEST NTR1                                                                   
         USING MESTD,R5                                                         
         LA    R3,L'MESTSUBS       MAX # OF SUB ESTS (LOOP COUNTER)             
         LA    R4,SUBEST           R4 = A(SUB EST ENTRY IN TABLE)               
         USING SESTD,R4                                                         
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'00'           MOVE IN ESTIMATE KEY ID                      
         L     R6,ADCLT            POINT TO CLIENT RECORD                       
         MVC   KEY+1(3),1(R6)      MOVE A-M/CLT                                 
         MVC   KEY+4(3),=C'POL'    MOVE IN 'POL' PRODUCT CODE                   
*                                                                               
RSES0100 EQU   *                                                                
*                                                                               
         CLI   SEST,0              ANY ENTRIES LEFT IN TABLE?                   
         BE    RSES0300            NO - SO CONTINUE                             
*                                                                               
         MVC   KEY+7(1),SEST       ELSE - PUT SUB EST INTO EST KEY              
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    RSES0200            YES - SO CONTINUE                            
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                SUB ESTIMATE NOT FOUND                       
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(16),=C'***SUB ESTIMATE '                                       
         EDIT  SEST,(3,P+16)       EDIT OUT THE SUB EST                         
         MVC   P+19(28),=C' NOT ON FILE FOR PRODUCT POL'                        
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RSESERR             AND GO TO ERROR                              
*                                                                               
RSES0200 EQU   *                                                                
*                                                                               
         GOTO1 GETEST              READ THE ESTIMATE                            
         L     R6,ADEST            R6 = A(SUB ESTIMATE)                         
         USING ESTD,R6                                                          
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RSES0250            NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'SUB ESTIMATE RECORD '                                   
         GOTO1 HEXOUT,DMCB,EKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
RSES0250 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,ESTART,(2,SESTART)  CNVT START DATE                  
         GOTO1 DATCON,DMCB,EEND,(2,SEEND)  CNVT END DATE                        
*                                                                               
         LA    R4,LSUBEST(R4)      INC TO NEXT SUB ESTIMATE                     
         B     RSES0100            AND LOOP BACK                                
*                                                                               
RSES0300 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RSESEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RSESERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RSESEXIT            AND RETURN                                   
         DROP  R4,R5,R6                                                         
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS THE TALENT FACTOR FOR THE MASTER ESTIMATE                  
* END DATE AND PRODUCT RECORD TALENT FACTOR GROUP CODE                          
*                                                                               
* NOTE: R5 = A(CURRENT MESTTBL ENTRY) - DON'T MUCK WITH IT!                     
*                                                                               
READTAL  NTR1                                                                   
         USING MESTD,R5                                                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'0D27'     KEYID/REC TYPE                               
         L     R6,ADCLT            POINT TO CLIENT RECORD                       
         MVC   KEY+2(3),1(R6)      MOVE A-M/CLT                                 
         MVC   KEY+5(6),MEEND      EST END DATE                                 
         MVC   KEY+11(1),SAVTAL    TALENT FACTOR GROUP CODE                     
*                                                                               
         GOTO1 HIGH                GET THE KEY                                  
         CLC   KEY(13),KEYSAVE     FOUND IT?                                    
         BE    RTAL0100            YES - SO CONTINUE                            
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                TALENT FACTOR REC NOT FOUND                  
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(28),=C'***TALENT FACTOR GROUP CODE '                           
         EDIT  SAVTAL,(3,P+28),ZERO=NOBLANK EDIT OUT TALENT FACTOR              
         MVC   P+31(17),=C' NOT ON FILE FOR '                                   
         MVC   P+48(6),MEEND       MOVE OUT END DATE                            
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RTALERR             AND GO TO ERROR                              
*                                                                               
RTAL0100 EQU   *                                                                
*                                                                               
         L     R6,AMYREC           GET A(MYREC)                                 
         ST    R6,AREC             SET UP TO READ TALENT FACTOR                 
         GOTO1 GET                     INTO 'MYREC'                             
         USING TALENTD,R6                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RTAL0150            NO - SO CONTINUE                             
*                                                                               
         MVC   P(21),=C'TALENT FACTOR RECORD '                                  
         GOTO1 HEXOUT,DMCB,TALKTYP,P+24,13,0                                    
         GOTO1 REPORT                                                           
         MVC   P+3(17),=C'MULT/DIV FACTORS '                                    
         GOTO1 HEXOUT,DMCB,TAL05MUL,P+21,8,0                                    
         GOTO1 REPORT                                                           
*                                                                               
RTAL0150 EQU   *                                                                
*                                                                               
* NOTE: THIS CODE IS STOLEN FROM SGOL23, WHICH CLAIMS THE MULTIP-               
* LICATION AND DIVISION FACTORS ARE BOTH A) REVERSED AND B) ONLY                
* 2-BYTES LONG.                                                                 
*                                                                               
         MVC   SVTLNT(2),TAL05MUL+2                                             
****NRK  MVC   SVTLNT(2),=H'5000'  FORCE IT TO DOUBLE TO TEST IT                
         MVC   SVTLNT+2(2),TAL05DIV+2   (10000)                                 
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RTALEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RTALERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RTALEXIT            AND RETURN                                   
         SPACE 3                                                                
         DROP  R5,R6                                                            
*                                                                               
* THIS ROUTINE BUILDS THE WEEKLY BUCKETS FOR THE MASTER ESTIMATE                
* (MAX 53).                                                                     
*                                                                               
* NOTE: R5 = A(CURRENT MESTTBL ENTRY) - DON'T MUCK WITH IT!                     
*                                                                               
BLDBKTS  NTR1                                                                   
         USING MESTD,R5                                                         
         LA    RE,BUCKETS          A(BUCKETS) FOR XCEF                          
         LA    RF,LBUCKETS         L(BUCKETS) FOR XCEF                          
         XCEF                                                                   
         LA    R4,BUCKETS          R4 = A(BUCKETS)                              
         USING BUCKD,R4                                                         
*                                                                               
         MVC   DATE,MESTART        STORE EST START DATE                         
*                                                                               
BBKT0100 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,DATE,(2,BUCKDATE)  CNVT CURRENT DATE                 
*                                                                               
         GOTO1 ADDAY,DMCB,(C'D',DATE),DATE,7 INC TO NEXT WEEK                   
         CLC   DATE,MEEND          CURRENT DATE .GT. END DATE?                  
         BH    BBKT0200            YES - SO CONTINUE                            
*                                                                               
         LA    R4,LBUCKD(R4)       ELSE - INC TO NEXT BUCKET                    
         LA    R2,BUCKDOLR         A(LAST FIELD IN BUCKET)                      
         LA    R3,TOTBUCK          A(TOTALS BUCKET)                             
         CR    R2,R3               ARE WE PAST THE TOTALS BUCKET?               
         BL    BBKT0100            NO - SO LOOP BACK AND CONTINUE               
*                                                                               
         DC    H'0'                ELSE - OOPS!                                 
*                                                                               
BBKT0200 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
BBKTEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
BBKTERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     BBKTEXIT            AND RETURN                                   
         DROP  R4,R5                                                            
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS THE TIER RECORD FOR THE MASTER ESTIMATE                    
* GOAL, MULTIPLIES THE BUDGET DOLLARS BY THE TIER PERCENT FOR THE               
* WEEK, AND STORES THE DOLLARS IN THE APPROPRIATE WEEKLY BUCKET.                
*                                                                               
READTIER NTR1                                                                   
         LA    R6,GLBUDTBL         A(BUDGET ELEMENT TABLE)                      
         USING GLBUDD,R6                                                        
*                                                                               
RDTR0100 EQU   *                                                                
*                                                                               
         CLI   0(R6),0             END OF LIST?                                 
         BE    RDTR0700            YES - SO CONTINUE                            
*                                                                               
         MVC   KEY(13),MYKEY       RESTORE THE MASTER GOAL KEY                  
         MVI   KEY+8,C'Z'          FORCE DAYPART                                
         MVI   KEY+11,X'40'        NO PRODUCT2 FOR TIER                         
         MVC   KEY+12(1),GLBTR     TIER CODE                                    
*                                                                               
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    RDTR0200            YES - SO CONTINUE                            
*                                                                               
         MVC   KEY(13),KEYSAVE     ELSE - RESTORE THE KEY                       
         MVC   KEY+5(2),=H'9999'   READ FOR DFLT MARKET                         
*                                                                               
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    RDTR0200            YES - SO CONTINUE                            
*                                                                               
***      CLI   QOPT4,C'Y'          TEST MODE?                                   
***      BE    *+6                 YES - PRINT OUT MSG                          
*                                                                               
***      DC    H'0'                TIER RECORD NOT FOUND                        
*                                                                               
         GOTO1 REPORT              PRINT THE LAST LINE                          
*                                                                               
         MVC   P(37),=C'***TIER RECORD NOT FOUND FOR PRODUCT '                  
         MVC   P+37(3),SAVPRDCD                                                 
         MVC   P+40(28),=C', MARKET 9999, AND ESTIMATE '                        
         EDIT  (1,KEYSAVE+7),(3,P+68) EDIT OUT THE MASTER EST                   
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RDTRERR             AND GO TO ERROR                              
*                                                                               
RDTR0200 EQU   *                                                                
*                                                                               
         GOTO1 GETGOAL             READ THE RECORD                              
         L     R5,ADGOAL                                                        
         USING GOALD,R5                                                         
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RDTR0250            NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'TIER RECORD '                                           
         GOTO1 HEXOUT,DMCB,GKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT                                                           
*                                                                               
RDTR0250 EQU   *                                                                
*                                                                               
         XC    TOTPCT,TOTPCT       INITIALIZE TOTAL PCT                         
         L     R2,ADGOAL           SET A(GOAL RECORD)                           
         MVI   ELCODE,X'41'        SET THE ELEMET CODE                          
         BAS   RE,GETEL            GET THE ELEMENT                              
         BZ    RDTR0350            1ST ELEMENT FOUND - CONTINUE                 
*                                                                               
         MVC   P+1(22),=C'NO PERCENTS FOUND FOR '                               
         GOTO1 HEXOUT,DMCB,GKEYTYPE,P+24,13,0                                   
         GOTO1 REPORT              PRINT THE ERROR MSG                          
         B     RDTR0500            AND GET NEXT TIER                            
*                                                                               
RDTR0300 EQU   *                                                                
*                                                                               
         BAS   RE,NEXTEL           GET THE NEXT ELEMENT                         
         BNZ   RDTR0500            NONE LEFT - CONTINUE                         
*                                                                               
RDTR0350 EQU   *                                                                
*                                                                               
         USING GLPCTEL,R2                                                       
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RDTR0360            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(21),=C'TIER PERCENT ELEMENT '                                
         GOTO1 HEXOUT,DMCB,GLPCTCOD,P+24,12,0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
RDTR0360 EQU   *                                                                
*                                                                               
         L     R3,TOTPCT           LOAD TOTAL PCT FOR THIS TIER                 
         A     R3,GLPCTPCT         ADD THIS WEEK'S PERCENT                      
         ST    R3,TOTPCT           STORE NEW TOTAL                              
*                                                                               
* FIND THE PROPER BUCKET TO ADD THE WEEK'S DOLLARS                              
*                                                                               
         LA    R3,BUCKETS          R3 = A(WEEKLY BUCKETS)                       
         USING BUCKD,R3                                                         
*                                                                               
RDTR0400 EQU   *                                                                
*                                                                               
         CLC   BUCKDATE,GLPCTDAT   RIGHT BUCKET (DATES MATCH)                   
         BE    RDTR0450            YES - SO CONTINUE                            
*                                                                               
         LA    R3,LBUCKD(R3)       ELSE - INC TO NEXT BUCKET                    
         B     RDTR0400            AND LOOP BACK TO CHECK IT                    
*                                                                               
RDTR0450 EQU   *                                                                
*                                                                               
         L     R1,GLPCTPCT         LOAD THE PERCENT                             
         ICM   R0,15,GLBDOL        LOAD THE DOLLARS                             
*                                                                               
* NOTE: THIS CODE IS STOLEN FROM SPGOL23 NEAR LABEL 'GLX22'.  ANY               
* BUGS FOUND HERE SHOULD BE INVESTIGATED THERE AS WELL.                         
*                                                                               
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
         D     R0,=F'1000000'                                                   
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         MH    R1,=H'100'          GIVES ROUNDED DOLLARS IN PENNIES             
*                                                                               
* END OF STOLEN CODE                                                            
*                                                                               
         STCM  R1,15,BUCKDOLR      STORE $ IN WEEKLY BUCKET                     
         ICM   R0,15,TOTBUCK       GET TOTAL $                                  
         AR    R0,R1               ADD CURRENT $                                
         STCM  R0,15,TOTBUCK       STORE THE NEW TOTAL $                        
*                                                                               
         B     RDTR0300            LOOP TO GET NEXT ELEMENT                     
         DROP  R2,R3                                                            
*                                                                               
RDTR0500 EQU   *                                                                
*                                                                               
         L     R3,TOTPCT           GET TOTAL PCT FOR THIS TIER                  
         CH    R3,=H'10000'        100.00 PERCENT?                              
         BE    RDTR0600            YES - SO CONTINUE                            
*                                                                               
         MVC   P+3(25),=C'TIER ONLY TOTALS        %'                            
         EDIT  (R3),(6,P+21),2     EDIT OUT THE PERCENT                         
         GOTO1 HEXOUT,DMCB,TOTPCT,P+29,4,0                                      
*        GOTO1 HEXOUT,DMCB,GKEY,P+29,13,0                                       
         GOTO1 REPORT              PRINT THE ERROR MESSAGE                      
         B     RDTRERR             GO TO ERROR                                  
*                                                                               
RDTR0600 EQU   *                                                                
*                                                                               
         LA    R6,LGLBUDD(R6)      INC TO NEXT TABLE ENTRY                      
         B     RDTR0100            AND LOOP BACK                                
*                                                                               
RDTR0700 EQU   *                                                                
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   RDTR0750            NO - SO CONTINUE                             
*                                                                               
         MVC   P+3(16),=C'TOTALS BUCKET = '                                     
         GOTO1 HEXOUT,DMCB,TOTBUCK,P+20,4,0                                     
         GOTO1 REPORT                                                           
*                                                                               
RDTR0750 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
RDTREXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
RDTRERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     RDTREXIT            AND RETURN                                   
         DROP R5,R6                                                             
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE READS/CREATES THE SUB-ESTIMATES FOR EACH MASTER                  
* ESTIMATE, DELETES ALL THE EXISTING WEEKLY GOAL (X'21') ELEMENTS,              
* CREATES THE NEW WEEKLY GOAL ELEMENTS BASED ON THE WEEKLY BUCKETS,             
* AND EITHER RE-WRITES THE RECORD IF THERE WAS DATA OR DELETES THE              
* THE EXISTING RECORD IF NO NEW DATA WAS CREATED.                               
*                                                                               
* NOTE: R5 = A(CURRENT MESTTBL ENTRY) - DON'T MUCK WITH IT!                     
*                                                                               
EXPLODE  NTR1                                                                   
         USING MESTD,R5                                                         
         LA    R6,L'MESTSUBS       MAX # OF SUB ESTS (LOOP COUNTER)             
         LA    R7,SUBEST           R7 = A(1ST SUB ESTIMATE)                     
         USING SESTD,R7                                                         
         L     R4,ADGOAL           A(GOAL IO AREA)                              
         USING GOALD,R4                                                         
*                                                                               
         XC    OLDELTOT,OLDELTOT   INITIALIZE OLD ELEMENT TOTALS                
*                                                                               
EXPL0100 EQU   *                                                                
*                                                                               
         NI    FLAG,X'FF'-NEWREC   RESET 'NEW RECORD' FLAG BIT                  
         CLI   SEST,0              MORE SUB ESTS IN LIST?                       
         BE    EXPL2400            NO - SO DONE                                 
*                                                                               
         MVC   KEY(13),MYKEY       ELSE - RESTORE MASTER EST GOAL KEY           
         MVC   KEY+7(1),SEST       MOVE SUB ESTIMATE TO KEY                     
         GOTO1 HIGH                READ THE KEY                                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL0200            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(20),=C'SUB EST GOAL RECORD '                                 
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+24,13,0                                    
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL0200 EQU   *                                                                
*                                                                               
         CLC   KEY(13),KEYSAVE     SAME KEY?                                    
         BNE   EXPL0500            NO - SO CONTINUE                             
*                                                                               
         GOTO1 GETGOAL             READ THE GOAL                                
*                                                                               
* DELETE ALL X'21' ELEMENTS                                                     
*                                                                               
         MVI   ELCODE,X'21'        SET ELEMENT CODE FOR GETEL                   
*                                                                               
EXPL0300 EQU   *                                                                
*                                                                               
         LA    R2,GOALREC          A(GOAL RECORD)                               
         BAS   RE,GETEL            GET THE ELEMENT                              
         BNZ   EXPL0700            NONE LEFT - CONTINUE                         
*                                                                               
         USING GLEMENT,R2                                                       
         ICM   R0,15,OLDELTOT      R0 = TOTAL FOR OLD ELEMENTS                  
         A     R0,GLBUDGET         ADD THIS EL $ TO TOTAL                       
         STCM  R0,15,OLDELTOT      STORE OLD EL TOTAL $                         
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL0400            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(17),=C'OLD GOAL ELEMENT '                                    
         ZICM  R0,GLEN,(1)         R0 = L(ELEMENT)                              
         GOTO1 HEXOUT,DMCB,GLCODE,P+20,(R0),0                                   
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL0400 EQU   *                                                                
*                                                                               
         GOTO1 RECUP,DMCB,GOALREC,(R2),0,0 DELETE THE ELEMENT                   
         B     EXPL0300            LOOP BACK TO GET NEXT ELEMENT                
         DROP  R2                                                               
*                                                                               
EXPL0500 EQU   *                                                                
*                                                                               
* NO KEY WAS FOUND ON FILE, SO BUILD THE RECORD TO ADD TO THE FILE.             
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL0600            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(23),=C'**ADDING NEW RECORD 1**'                              
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL0600 EQU   *                                                                
*                                                                               
         OI    FLAG,NEWREC         SET 'NEW RECORD' FLAG BIT                    
*                                                                               
         MVC   GKEY(13),KEYSAVE    MOVE IN THE KEY                              
         XC    GCNTRLS,GCNTRLS     CLEAR THE CONTROL BYTE                       
         MVC   GLENGTH,=H'100'     MOVE IN THE RECORD LENGTH                    
         XC    GLINKS,GLINKS       CLEAR THE LINK FIELD                         
         MVC   GAGYALPH,AGY        MOVE IN AGY CODE                             
         XC    GAGYALPH+2(2),GAGYALPH+2 CLEAR THE SPARE BYTE                    
*                                                                               
         XC    GDELEM(GDLENQ),GDELEM CLEAR THE DESC ELEMENT                     
         MVC   GDELEM(2),=X'204C'  DESC EL CODE/LENGTH                          
         MVC   GBUYNAME(4),=C'TF02'                                             
         MVC   GREDATE,TODAYP      SET CREATE DATE                              
*                                                                               
EXPL0700 EQU   *                                                                
*                                                                               
         MVC   GACTDATE,TODAYP     SET LAST ACTIVITY DATE                       
*                                                                               
* NOW BUILD THE NEW ELEMENTS FOR THE SUB EST GOAL RECORD                        
*                                                                               
         XC    TOTBUCK,TOTBUCK     CLEAR THE TOTALS BUCKET                      
         LA    R3,BUCKETS          GET A(1ST BUCKET)                            
         USING BUCKD,R3                                                         
*                                                                               
EXPL0800 EQU   *                                                                
*                                                                               
         CLC   BUCKDATE,SESTART    BUCKET EARLIER THAN EST START?               
         BL    EXPL1000            YES - SO GO INC TO NEXT BUCKET               
*                                                                               
         CLC   BUCKDATE,SEEND      BUCKET LATER THAN EST END?                   
         BH    EXPL1100            YES - SO ALL DONE W/THIS EST                 
*                                                                               
         CLC   BUCKDOLR(4),=F'0'   ANY DOLLARS FOR WEEK?                        
         BE    EXPL1000            NO - SO GO INC TO NEXT BUCKET                
*                                                                               
         XC    ELBUILD,ELBUILD     ELSE - INIT EL BUILD AREA                    
         LA    R2,ELBUILD          GET A(ELEMENT BUILD AREA)                    
         USING GLEMENT,R2                                                       
         MVC   GLCODE(2),=X'210C'  EL CODE/LENGTH                               
         MVC   GLWEEK,BUCKDATE     ELEMENT DATE                                 
         MVC   GLBUDGET,BUCKDOLR                                                
*                                                                               
         STCM  R1,15,BUCKDOLR      STORE $ IN TOTALS BUCKET                     
         ICM   R0,15,TOTBUCK       GET TOTAL $                                  
         AR    R0,R1               ADD CURRENT $                                
         STCM  R0,15,TOTBUCK       STORE THE NEW TOTAL $                        
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL0900            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(17),=C'NEW GOAL ELEMENT '                                    
         ZICM  R0,GLEN,(1)         R0 = L(ELEMENT)                              
         GOTO1 HEXOUT,DMCB,GLCODE,P+20,(R0),0                                   
         GOTO1 REPORT              PRINT THE LINE                               
         DROP  R2                                                               
*                                                                               
EXPL0900 EQU   *                                                                
*                                                                               
         LA    R2,GOALREC          GET A(RECORD)                                
         ZICM  R1,GLENGTH,(3)      R1 = L(GOALREC)                              
         AR    R2,R1               INC TO END OF RECORD                         
         GOTO1 RECUP,DMCB,GOALREC,ELBUILD,(R2),0 ADD THE ELEMENT                
*                                                                               
EXPL1000 EQU   *                                                                
*                                                                               
         LA    R3,LBUCKD(R3)       INC TO NEXT BUCKET                           
         LA    R0,BUCKDOLR         A(LAST FIELD IN BUCKET)                      
         LA    R1,TOTBUCK          A(TOTALS BUCKET)                             
         CR    R0,R1               ARE WE PAST THE TOTALS BUCKET?               
         BL    EXPL0800            NO - SO LOOP BACK AND CONTINUE               
*                                                                               
EXPL1100 EQU   *                                                                
*                                                                               
* RE-WRITE, DELETE, OR ADD THE GOAL ELEMENT BACK TO THE FILE                    
*                                                                               
         CLC   TOTBUCK(4),=F'0'    ANY DATA FOR THIS SUB EST?                   
         BNE   EXPL1500            YES - SO GO ADD/RE-WRITE                     
*                                                                               
* DELETE THE RECORD, IF NECESSARY                                               
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL1200            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(25),=C'**NO DATA FOUND FOR REC**'                            
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL1200 EQU   *                                                                
*                                                                               
         TM    FLAG,NEWREC         'NEW RECORD' FLAG SET?                       
         BO    EXPL2300            YES - SO NO RECORD TO DELETE                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL1300            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(25),=C'***DELETING OLD RECORD***'                            
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL1300 EQU   *                                                                
*                                                                               
         OI    KEY+13,X'80'        SET THE DELETE BIT IN THE KEY                
*                                                                               
         CLI   QOPT4,C'Y'          TEST MODE?                                   
         BE    EXPL1400            YES - SO CONTINUE                            
*                                                                               
         GOTO1 WRITE               RE-WRITE THE DELETED KEY                     
*                                                                               
EXPL1400 EQU   *                                                                
*                                                                               
         OI    GCNTRLS,X'80'       SET DELETE BIT IN THE RECORD                 
*                                                                               
EXPL1500 EQU   *                                                                
*                                                                               
         TM    FLAG,NEWREC         'NEW RECORD' FLAG SET?                       
         BO    EXPL1800            YES - SO GO ADD IT                           
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL1600            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(26),=C'***REWRITING THE RECORD***'                           
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL1600 EQU   *                                                                
*                                                                               
         CLI   QOPT4,C'Y'          TEST MODE?                                   
         BE    EXPL1700            YES - SO CONTINUE                            
*                                                                               
         GOTO1 PUTGOAL             ELSE - RE-WRITE THE GOAL RECORD              
*                                                                               
EXPL1700 EQU   *                                                                
*                                                                               
         B     EXPL2300            AND CONTINUE                                 
*                                                                               
EXPL1800 EQU   *                                                                
*                                                                               
* NO RECORD EXISTS CURRENTLY ON THE FILE, SO CHECK FOR ALREADY                  
* DELETED KEY AND ADD THE RECORD.                                               
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL1900            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(23),=C'**ADDING NEW RECORD 2**'                              
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL1900 EQU   *                                                                
*                                                                               
         MVC   KEY(13),GKEY        SET KEY TO ONE WE WANT TO ADD                
         OI    DMINBTS,X'08'       SET 'GIVE ME DELETED KEYS' FLAG              
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     SAME KEY?                                    
         BNE   EXPL2200            NO - SO CONITINUE                            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL2000            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(20),=C'**RE-ADDING RECORD**'                                 
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+24,13,0                                    
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL2000 EQU   *                                                                
*                                                                               
         TM    KEY+13,X'80'        KEY DELETED?                                 
         BO    *+6                 YES - SO CONTINUE                            
*                                                                               
         DC    H'0'                ELSE - HUH?                                  
*                                                                               
* 'RE-ADD' DELETED RECORD AND KEY TO THE FILE                                   
*                                                                               
         NI    KEY+13,X'7F'        TURN OFF DELETE BIT IN KEY                   
         MVC   AREC,AMYREC         SET DUMMY IO AREA                            
         GOTO1 GET                 GET THE DELETED RECORD                       
*                                                                               
         CLI   QOPT4,C'Y'          TEST MODE?                                   
         BE    EXPL2100            YES - SO CONTINUE                            
*                                                                               
         GOTO1 PUTGOAL             RE-WRITE THE NEW RECORD                      
         GOTO1 WRITE               RE-WRITE THE UNDELETED KEY                   
*                                                                               
EXPL2100 EQU   *                                                                
*                                                                               
         B     EXPL2300            AND CONTINUE                                 
*                                                                               
EXPL2200 EQU   *                                                                
*                                                                               
* ADD NEW RECORD (AND KEY) TO THE FILE                                          
*                                                                               
         MVC   AREC,ADGOAL         SET A(GOAL) J.I.C.                           
*                                                                               
         CLI   QOPT4,C'Y'          TEST MODE?                                   
         BE    EXPL2300            YES - SO CONTINUE                            
*                                                                               
         GOTO1 ADD                 ELSE - ADD THE NEW GOAL                      
*                                                                               
EXPL2300 EQU   *                                                                
*                                                                               
         LA    R7,LSUBEST(R7)      INC TO NEXT SUB-ESTIMATE                     
         BCT   R6,EXPL0100         AND LOOP BACK                                
*                                                                               
EXPL2400 EQU   *                                                                
*                                                                               
         CLI   QOPT5,C'Y'          TRACE MODE?                                  
         BNE   EXPL2500            NO - SO CONTINUE                             
*                                                                               
         MVC   P+2(24),=C'OLD GOAL ELEMENT TOTALS '                             
         GOTO1 HEXOUT,DMCB,OLDELTOT,P+27,4,0                                    
         GOTO1 REPORT              PRINT THE LINE                               
*                                                                               
EXPL2500 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
EXPLEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
EXPLERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     EXPLEXIT            AND RETURN                                   
         DROP  R3,R4,R5                                                         
         SPACE 3                                                                
         PRINT GEN                                                              
         GETEL R2,DATADISP,ELCODE  SET UP GETEL                                 
         PRINT NOGEN                                                            
         TITLE 'WORK AREAS AND LITERAL POOL'                                    
AMYREC   DS    A                   A(MY 2K IO AREA)                             
APRNTPOS DS    A                   A(CURRENT PRINT POSITION)                    
*                                                                               
MESTTBL  DS    28XL(LMESTD)        TABLE OF MASTER AND SUB ESTS                 
*                                                                               
         DS    X                   E.O.T.                                       
LMESTTBL EQU   *-MESTTBL           L(TABLE AND E.O.T. FLAG)                     
*                                                                               
GLBUDTBL DS    6XL(LGLBUDD)        TABLE OF GOAL BUDGET DATA                    
*                                                                               
         DS    X                   E.O.T.                                       
LGLBDTBL EQU   *-GLBUDTBL          L(TABLE AND E.O.T. FLAG)                     
*                                                                               
BUCKETS  DS    53XL(LBUCKD)        WEEKLY BUCKETS FOR THE PERIOD                
TOTBUCK  DS    XL4                 TOTALS BUCKET FOR THE PERIOD                 
LBUCKETS EQU   *-BUCKETS           L(BUCKETS + TOTBUCK)                         
*                                                                               
ELBUILD  DS    XL256               ELEMENT BUILD AREA                           
*                                                                               
MYKEY    DS    XL13                TEMP KEY SAVE AREA                           
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
REQEST   DS    X                   EST CODE FOR SKIPREAD ROUTINE                
FLAG     DS    X                   FLAGS FOR THIS RUN                           
*                                                                               
* EQUATES FOR FLAG BITS                                                         
*                                                                               
NEWREC   EQU   X'80'               NEED TO ADD NEW RECORD                       
FIRST    EQU   X'40'               FIRST PASS THRU GOAL DATA                    
*                                                                               
SAVTAL   DS    X                   TALENT FACTOR GROUP CODE                     
SAVPRDNO DS    X                   PRODUCT NUMBER                               
SAVPRDCD DS    CL3                 PRODUCT CODE                                 
DATE     DS    CL6                 TEMPORARY DATE STORAGE AREA                  
SVTLNT   DS    XL4                 DIV/MULT FACTORS                             
OLDELTOT DS    XL4                 TOTAL $ FOR OLD GOALS                        
OLDPROD  DS    CL3                 PRIOR PRODUCT CODE                           
OLDEST   DS    X                   PRIOR MASTER ESTIMATE NUMBER                 
*                                                                               
         DS    0D                  FORCE DOUBLE WORD ALIGNMENT                  
TOTPCT   DS    XL4                 TOTAL PCT FOR EACH TIER                      
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                  ALIGN IT                                     
MYREC    DS    XL2048              MY 2K IO AREA                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
       ++INCLUDE SPGENCSO                                                       
CLIENTD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRODUCTD DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
TALENTD  DSECT                                                                  
       ++INCLUDE SPGENTAL                                                       
GOALD    DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
ESTD     DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
MESTD    DSECT                     TABLE OF MASTER AND SUB ESTIMATES            
MEST     DS    X                   MASTER ESTIMATE NUMBER                       
MESTART  DS    CL6                 MASTER EST START DATE                        
MEEND    DS    CL6                 MASTER EST END DATE                          
*                                                                               
SUBEST   DS    XL(LSESTD)          FIRST SUB ESTIMATE IN LIST                   
*                                                                               
* OTHER DATA FOR THIS SUB ESTIMATE HERE, IF NEEDED                              
*                                                                               
LSUBEST  EQU   *-SUBEST            L(ONE SUB EST ENTRY)                         
*                                                                               
         DS    (L'MESTSUBS-1)XL(LSUBEST) UP TO 5 MORE SUB ESTS                  
*                                                                               
LMESTD   EQU   *-MESTD             L(TABLE ENTRY)                               
*                                                                               
SESTD    DSECT                     SUB ESTIMATE DSECT                           
SEST     DS    X                   ESTIMATE NUMBER                              
SESTART  DS    XL2                 COMPRESSED START DATE                        
SEEND    DS    XL2                 COMPRESSED END DATE                          
LSESTD   EQU   *-SESTD                                                          
*                                                                               
GLBUDD   DSECT                     TABLE OF GOAL BUDGET EL DATA                 
GLBDAT   DS    XL2                 PERIOD START                                 
GLBEND   DS    XL2                 PERIOD END                                   
GLBTR    DS    X                   TIER (IN BINARY)                             
GLBDOL   DS    XL4                 DOLLARS FOR PERIOD                           
*                                                                               
LGLBUDD  EQU   *-GLBUDD            L(TABLE ENTRY)                               
*                                                                               
BUCKD    DSECT                     WEEKLY BUCKETS LAYOUT                        
BUCKDATE DS    XL2                 COMPRESSED DATE                              
BUCKDOLR DS    XL4                 DOLLARS (MAX=9999.00)                        
*                                                                               
LBUCKD   EQU   *-BUCKD                                                          
*                                                                               
CLISTD   DSECT                     LAYOUT FOR PRD CODES IN CLIENT REC           
PRDCODE  DS    XL3                 PRODUCT CODE                                 
PRDNUM   DS    X                   PRODUCT NUMBER                               
*                                                                               
LCLISTD  EQU   *-CLISTD                                                         
*                                                                               
PRINTD   DSECT                     PRINTLINE LAYOUT                             
PPRODUCT DS    CL7                 PRODUCT CODE                                 
         DS    CL2                 SPACE                                        
PMARKET  DS    CL6                 MARKET NUMBER                                
         DS    CL2                 SPACE                                        
PMASTER  DS    CL10                MASTER ESTIMATE NUMBER                       
         DS    CL4                 SPACE                                        
*                                                                               
LPRINTD  EQU   *-PRINTD                                                         
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPTF02 05/12/98'                                      
         END                                                                    
