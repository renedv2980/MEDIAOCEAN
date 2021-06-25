*          DATA SET SPBUY12    AT LEVEL 042 AS OF 11/21/19                      
*PHASE T21112C  <====                                                           
*==================================================================*            
* 14NOV13 HWON FIX PROPER 208 MAX SPOT BOUNDARY                                 
* 29MAY18 MHER ON CHANGE OF DAY OR TIME, SET COMSCORE DEMO LOOKUP REQD          
* 01NOV17 HWON SUPPORT FOR SBU UPLOADS                                          
* 21MAR12 AKAT FIX CHGCOST BUG FOR CANADIAN PAID SPOTS                          
* 29JUL04 MHER ALLOW STARTING STATION FOR PCT DISPLAY                           
* 27MAY03 MHER ALLOW CANAD EXPLODED BUYS IN DOLLARS                             
* 11FEB03 MHER DO NOT ALLOW NPW CHANGE IF SPODS IN BUY                          
* 12DEC99 MHER FIX SKED LOGIC TO INCLUDE OTO'S IN SKED COUNTS                   
* 17SEP99 MHER ALLOW SKED TO ADD SPOTS FOR PAID/MATCHED POL DATES               
*==================================================================*            
         TITLE 'T21112 - SPOTPAK BUY - CHANGE LOGIC I'                          
T21112   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21112,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21112+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO MVI   DEMLKSW,0           RESET DEMO LOOK-UP REQ'D SW                  
         XC    BUEXPKEY,BUEXPKEY                                                
*                                                                               
         BRAS  RE,FIXREC           FIX ELEMENT SEQUENCE !                       
*                                                                               
         CLI   EDTVAL,PEREDT       SPECIAL TEST FOR PERIOD CHANGES              
         BE    CHG100                                                           
*                                                                               
         L     R5,=A(BRTAB)                                                     
         A     R5,RELO                                                          
         LA    R6,4                                                             
         LA    R7,BRTABX-BRTAB(R5)                                              
*                                                                               
         CLC   0(1,R5),EDTVAL                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         L     RF,0(R5)                                                         
         A     RF,RELO                                                          
         ST    RF,CHGADDR                                                       
         B     CHG2                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   CLC   =C'CM',BUTRCODE                                                  
         BNE   BUYERRX                                                          
         CLI   CMPASS,1            TEST FOR FIRST PASS                          
         BE    NEQXIT              YES-EXIT MODULE WITH CC=NEQ                  
         DC    H'0'                FORCE A DUMP IF SECOND PASS                  
*                                                                               
BUYERRX  GOTO1 ERROR                                                            
RELO     DC    A(0)                                                             
         SPACE 2                                                                
****************************************************************                
*                                                              *                
* WHEN CALLED BY SPBUY07 FOR CHANGE MULTIPLE, MODULE WILL      *                
* EXIT SETTING CC=EQ IF NO ERRORS AND CC=NEQ IF ERROR FOUND.   *                
* SPBUY07 WILL HANDLE THE ERROR CONDITION.                     *                
*                                                              *                
****************************************************************                
         EJECT                                                                  
CHG2     DS    0H                                                               
         MVC   BUSVELDT,BUELDT     SAVE ORIGINAL ELEMENT DATE                   
         MVI   CHGADDR,0                                                        
         L     RF,CHGADDR                                                       
         BASR  RE,RF                                                            
         BNE   BUYERR                                                           
         CLI   EDTVAL,SDTEDT       TEST FOR CAN/NET DAY/TIME                    
         BE    EQXIT               YES - DONE                                   
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHG4                                                             
         CLI   EDTVAL,SKEDEDT                                                   
         BE    EQXIT               PUTREC DONE IN OVLY07                        
         CLI   DEMLKSW,0           TEST DEMO LOOK-UP REQ'D                      
         BNE   EQXIT                                                            
*                                                                               
CHG4     GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT                                                         
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHG10                                                            
         B     EQXIT                                                            
         EJECT                                                                  
* CANAD NTWK                                                                    
*                                                                               
CHG10    CLI   EDTVAL,COSTEDT                                                   
         BE    CHG11                                                            
         CLI   EDTVAL,COS2EDT                                                   
         BE    CHG11                                                            
         CLI   EDTVAL,TAXEDT                                                    
         BE    CHG11                                                            
         CLI   EDTVAL,SPCTEDT                                                   
         BE    CHG11                                                            
* MOVE REC1 TO REC3                                                             
         MVC   BUSVSTB(6),BDSTART  SAVE NETWORK START/END DATES                 
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
CHG11    L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CHG12    MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CHG16                                                            
         CLI   EDTVAL,TAXEDT                                                    
         BNZ   CHG13                                                            
         OC    7(4,R6),7(R6)       TEST TAX DOLLARS = 0                         
         BNZ   CHG13                                                            
         OC    BUCOST(3),BUCOST    TEST NEW TAX = 0                             
         BNZ   CHG12               NO - SKIP                                    
*                                                                               
* READ EXPLODED KEY/REC                                                         
*                                                                               
CHG13    MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
         BRAS  RE,FIXREC           MAKE SURE SF ELEMS IN RIGHT SEQNCE           
*                                                                               
         XC    BUEXPDAY,BUEXPDAY                                                
         MVC   BUELDT,BUSVELDT     RESTORE ELEMENT DATE                         
         OC    BUELDT,BUELDT       TEST SOME KIND OF DATE CHANGE                
         BZ    CHG13X              NO                                           
*                                                                               
         CLI   BDPROGRM,C'='       TEST SIMULCAST                               
         BE    CHG13X                                                           
         BAS   RE,GETRELST         SET RELATIVE START DAY                       
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUELDT),WORK                                     
         LH    R0,BUEXPDAY                                                      
         GOTO1 VADDAY,(R1),WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,(R1),WORK+6,(2,BUELDT)                                   
*                                                                               
CHG13X   DS    0H                                                               
         GOTO1 CHGADDR                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DEMLKSW,0                                                        
         BE    *+10                                                             
         L     RF,DEMLKUP                                                       
         BASR  RE,RF                                                            
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT                                                         
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
         B     CHG12                                                            
*                                                                               
CHG16    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE FUCKING NETWORK BUY                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         BRAS  RE,FIXREC                                                        
         B     CHG151                                                           
         EJECT                                                                  
* CHANGE OF PERIOD                                                              
*                                                                               
CHG100   DS    0H                                                               
         XC    BUEXPDAY,BUEXPDAY                                                
         MVC   BUSVSTB(6),BDSTART                                               
         MVC   BUSVWKIN,BDWKIND                                                 
         MVC   BUSVDAYS,BDDAY                                                   
         MVC   BUSVSEDY,BDSEDAY                                                 
*                                                                               
CHG105   MVC   BDSTART(6),BUSTARTB                                              
         MVC   BDWKIND,BUWKIND                                                  
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDSEDAY,BUDAYNUM                                                 
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDWKS,BUWKS                                                      
*                                                                               
         BRAS  RE,CHGPER                                                        
         BNE   BUYERR                                                           
         MVI   BUWHY,X'42'         SET BUY DESC/DEMO LOOKUP                     
         GOTO1 SETCHGDT            SET THE REASON NOW YSFI                      
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BZ    CHG107                                                           
         BAS   RE,CHKINF           MAKE SURE NO UNHUNG RESPONSES                
         BNE   BUYERR                                                           
*                                                                               
CHG107   OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    EQXIT               NO - DONE                                    
         B     CHG130                                                           
*                                                                               
* MASTER                                                                        
*                                                                               
* CANAD NTWK                                                                    
*                                                                               
CHG130   DS    0H                                                               
*                                                                               
         MVI   BUWHY,X'42'                                                      
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT                                                         
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
* MOVE REC TO REC3                                                              
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         B     CHG144                                                           
*                                                                               
CHG132   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CHG150                                                           
* READ EXPLODED KEY/REC                                                         
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUEXPKEY,KEY        SAVE EXPL KEY                                
         GOTO1 GETREC                                                           
         BRAS  RE,FIXREC                                                        
*                                                                               
         MVC   BUSVDAYS,BDDAY                                                   
         MVC   BUSVSEDY,BDSEDAY                                                 
* WORK OUT REL START DAY                                                        
         XC    BUEXPDAY,BUEXPDAY                                                
         CLI   BDPROGRM,C'='       TEST SIMULCAST                               
         BE    CHG140              YES                                          
*                                                                               
         BAS   RE,GETRELST         GO SET REL START DAY                         
*                                                                               
         L     R4,AREC3                                                         
         LA    R4,BDSTART-BUYREC(R4)                                            
         MVC   BDSTART(6),0(R4)                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,BUDAYS         PUT NETWORK DAYS IN REG                      
         LH    R0,BUEXPDAY                                                      
         LTR   R0,R0                                                            
         BP    CHG135                                                           
         BZ    CHG136                                                           
* REL DAY IS NEGATIVE                                                           
         SLL   RE,24               SHIFT DAYS TO LEFT HAND SIDE                 
         LPR   R0,R0               AND MAKE COUNTER POSITIVE !                  
CHG134   SLL   RE,1                                                             
         LTR   RE,RE               TEST REGISTER NEG                            
         BM    CHG139              UH OH (MONDAY IS X40 SO X80 IS BAD)          
         BCT   R0,CHG134                                                        
         STCM  RE,8,BDDAY          SET DAY IN BUYREC                            
         B     CHG137                                                           
*                                                                               
CHG135   SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BM    CHG139              UH OH                                        
         BCT   R0,CHG135                                                        
CHG136   STCM  RE,1,BDDAY          SET DAY IN BUYREC                            
*                                                                               
CHG137   LH    R0,BUEXPDAY                                                      
         LA    R4,BDEND                                                         
         BAS   RE,GETDATE                                                       
         LA    R4,BDSTART                                                       
         BAS   RE,GETDATE                                                       
*                                                                               
         CLC   SVSTARTB,BDSTART                                                 
         BH    CHG138                                                           
         CLC   SVENDB,BDEND                                                     
         BL    CHG138                                                           
         B     CHG142                                                           
*                                                                               
CHG138   MVC   NERRCD,=Y(OUTOFEST)                                              
         B     CHG139X                                                          
CHG139   MVC   NERRCD,=Y(OUTOFWK)                                               
*                                                                               
CHG139X  MVI   ERRAREA,X'FE'       DO DC H'0',$ABEND                            
         MVI   ERRCD,NEWERRS                                                    
         LA    R2,BUYINP1H                                                      
         B     BUYERR                                                           
*                                                                               
CHG140   MVC   BDSTART(6),BUSTARTB                                              
         MVC   BDDAY,BUDAYS                                                     
*                                                                               
CHG142   MVC   BDWKIND,BUWKIND                                                  
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDWKS,BUWKS                                                      
         MVC   BDSEDAY,BUDAYNUM                                                 
*                                                                               
         BRAS  RE,CHGPER                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DEMLKUP                                                          
         MVI   BUWHY,X'42'                                                      
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT                                                         
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
*                                                                               
CHG144   B     CHG132                                                           
         SPACE 1                                                                
* RESTORE REC3 TO REC *                                                         
         SPACE 1                                                                
CHG150   MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
CHG151   MVI   DEMLKSW,0           RESET DEMO LOOKUP SWITCH                     
         B     EQXIT                                                            
         EJECT                                                                  
*============================================================*                  
* ON CHANGE OF DATE FOR INFOMERCIAL CLIENT, MAKE SURE THERE  *                  
* ARE NO RESPONSES PRIOR TO NEW BROADCAST DATE               *                  
*============================================================*                  
         SPACE 1                                                                
CHKINF   NTR1                                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R7,R6               SAVE 0B ELEMENT ADDRESS                      
*                                                                               
         MVI   ELCDLO,X'73'                                                     
         MVI   ELCDHI,X'73'                                                     
         LA    R6,BDELEM                                                        
CHKINF2  BRAS  RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         CLC   2(2,R7),2(R6)       EL DATE TO RESPONSE DATE                     
         BNH   CHKINF2             IF EL DATE PRIOR, OK                         
         MVI   ERRCD,INFRESP       SET 'RESPONSES PRIOR TO SPOT'                
         B     NEQXIT                                                           
         EJECT                                                                  
*======================================================*                        
* SUBROUTINE COMPUTES RELATIVE START DATE FOR EXPLODED *                        
* CANADIAN NETWORK BUYS                                *                        
*======================================================*                        
         SPACE 1                                                                
GETRELST NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK+6  EXPLODED BUY START              
         LA    R4,BUSVSTB                       NETWORK BUY START               
         GOTO1 (RF),(R1),(3,(R4)),WORK                                          
*                                                                               
         SR    R4,R4               CLEAR COUNTER                                
         LA    R0,1                SET TO ADVANCE DAYS                          
         CLC   WORK(6),WORK+6                                                   
         BE    GETRELS4                                                         
         BL    *+6                                                              
         LCR   R0,R0               UNLESS HIGH ALREADY                          
*                                                                               
GETRELS2 LA    R4,1(R4)            BUMP COUNTER                                 
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         MVC   WORK(6),WORK+12                                                  
         CLC   WORK(6),WORK+6                                                   
         BNE   GETRELS2                                                         
*                                                                               
GETRELS4 LTR   R0,R0                                                            
         BNM   *+6                                                              
         LCR   R4,R4                                                            
         STH   R4,BUEXPDAY         SET SIGNED RELATIVE DAY                      
         XIT1                                                                   
*                                                                               
GETDATE  NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,(R4)),WORK                                       
         GOTO1 VADDAY,(R1),WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,(R1),WORK+6,(3,(R4))                                     
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* C,NPW AND C,SKED= FUNCTIONS DONE HERE                            *            
*==================================================================*            
         SPACE 1                                                                
CHGNPW   NTR1                                                                   
*                                                                               
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
*                                                                               
CNPW1    MVI   BUWHY3,X'80'        SET SKED CHANGE IND                          
         MVI   RCLOPT,0                                                         
         TM    SVOPTS,X'02'        TEST NO ROT REQ                              
         BO    *+8                                                              
         MVI   RCLOPT,RCLROT                                                    
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         NI    BDSTAT,X'7F'                                                     
*                                                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    CNPW90                                                           
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BE    CNPW20                                                           
*                                                                               
         CLI   EDTVAL,SKEDEDT                                                   
         BE    CNPW10                                                           
         SPACE 1                                                                
*=============================================================*                 
* FOR NON-POL C,NPW REGELS MUST NOT BE PAID OR MATCHED        *                 
*=============================================================*                 
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
CNPW2    MVI   ELCDLO,6                                                         
         MVI   ELCDHI,6                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   CNPWX                                                            
         BRAS  RE,TESTPD                                                        
         BNE   NEQXIT                                                           
         BRAS  RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
*                                                                               
         CLC   7(1,R6),BUNPW       COMPARE OLD V NEW NPW                        
         BH    CNPW3               IF OLD HI, CHECK FURTHER                     
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CLTFRZN)                                               
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    NEQXIT                                                           
         MVC   7(1,R6),BUNPW       ELSE SET NEW NPW                             
         B     CNPW2               AND CONTINUE                                 
*                                                                               
CNPW3    MVC   7(1,R6),BUNPW       SET NEW NPW                                  
* DECREASE IN NPW, MAKE SURE SPOTS ON DAY REMAIN POSITIVE.                      
         MVI   ERRCD,NEGSPOTS                                                   
         SR    R8,R8                                                            
         IC    R8,7(R6)            SAVE NUMBER OF SPOTS                         
         LR    R7,R6               SAVE ELEM ADDRESS                            
         MVI   ELCDHI,8                                                         
*                                                                               
CNPW4    BRAS  RE,NEXTEL                                                        
         BNE   CNPW6                                                            
         CLC   2(2,R7),2(R6)       TEST SAME DATE                               
         BNE   CNPW6                                                            
         SR    R0,R0                                                            
         IC    R0,7(R6)            EXTRACT SPOTS                                
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BZ    *+6                 NO                                           
         LCR   R0,R0                                                            
         AR    R8,R0                                                            
         B     CNPW4                                                            
* CHANGE OF DATE                                                                
CNPW6    LTR   R8,R8               TEST NUMBER OF SPOTS                         
         BM    NEQXIT              OK IF NON-NEG                                
         LR    R6,R7               RESTORE ELEM POINTER                         
         B     CNPW2                                                            
         EJECT                                                                  
*================================================================*              
* NON-POL SKED= LOGIC                                            *              
* FIRST FIND NET SPOTS IN WEEK                                   *              
*================================================================*              
         SPACE 1                                                                
CNPW10   MVC   ELEMDT,BUELDT       CALCULATE WEEK END DATE                      
         GOTO1 VDATCON,DMCB,(2,ELEMDT),WORK                                     
         GOTO1 VADDAY,(R1),WORK,WORK+6,F'6'                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,ELEMDTX)                                  
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,7                                                         
         MVI   ELEMNO,0            CLEAR SPOT COUNTER                           
         SR    R7,R7               CLEAR ELEMENT FOUND FLAG                     
*                                                                               
CNPW11   BRAS  RE,NEXTEL                                                        
         BNE   CNPW11A                                                          
*                                                                               
         CLC   2(2,R6),ELEMDT      TEST WITHIN WEEK                             
         BL    CNPW11              IF LOW, CONTINUE                             
         CLC   2(2,R6),ELEMDTX                                                  
         BH    CNPW11A             IF HIGH, NO ELEM THIS DATE                   
*                                                                               
         LTR   R7,R7               TEST FIRST ELEM THIS WEEK                    
         BNZ   *+6                                                              
         LR    R7,R6               SAVE ELEM ADDR                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ELEMNO           BUMP SPOT COUNT                              
         AR    RE,R0                                                            
         STC   RE,ELEMNO                                                        
         B     CNPW11                                                           
*                                                                               
CNPW11A  L     RE,ASVDARE          TEST ELEM IN LOCKED DARE FLIGHT              
         ST    R7,SVDRELEM-SVDARED(RE)                                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ELEMNO                                                        
         SR    RE,RE                                                            
         IC    RE,BUNPW                                                         
         SR    RE,R0               NEW - OLD SPOTS                              
         STC   RE,ELEMNO           SAVE NET CHANGE                              
         BM    CNPW12              PROCESS DECREASE                             
         BZ    EQXIT               NO CHANGE                                    
         EJECT                                                                  
* NUMBER OF SPOTS HAS INCREASED TRY TO CHANGE REGEL AT R7                       
         LR    R6,R7               POINT TO FIRST SPOT THIS WEEK                
         CLI   0(R6),6             TEST REGEL                                   
         BE    CNPW11B                                                          
         MVC   BUNPW,ELEMNO        MOVE NUMBER OF SPOTS TO ADD                  
         B     CNPW15              AND ADD NEW ELEMENT                          
* CHANGE THE REGEL OR RETURN AN ERROR                                           
CNPW11B  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(REGELPD)                                             
         BRAS  RE,TESTPD           TEST PAID                                    
         BNE   NEQXIT                                                           
         MVC   NERRCD,=AL2(REGELMAT)                                            
         BRAS  RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
         IC    R0,7(R6)            GET CURRENT NUMBER OF SPOTS                  
         SR    RE,RE                                                            
         IC    RE,ELEMNO           GET NUMBER TO ADD                            
         AR    R0,RE                                                            
         STC   R0,7(R6)                                                         
         B     EQXIT               AND EXIT                                     
         EJECT                                                                  
*===============================================================*               
* NUMBER OF SPOTS HAS DECREASED                                 *               
* FIRST TRY TO CHANGE REGEL AND THEN ADJUST OTO'S               *               
* NEED TO MAKE SURE NET SPOTS ON EACH DAY NOT NEGATIVE          *               
* ON ARRIVAL, R7 POINTS TO FIRST SPOT THIS WEEK                 *               
*===============================================================*               
         SPACE 1                                                                
CNPW12   LPR   RE,RE                                                            
         STC   RE,ELEMNO           SAVE NET CHANGE IN SPOTS                     
*                                                                               
         BRAS  RE,FIXOTOS          FIX OTOS SO ALL + PRECEDE ALL -              
         LR    R6,R7               POINT TO FIRST SPOT THIS WEEK                
         SR    R8,R8               CLEAR COUNTER                                
*                                                                               
* COUNT NUMBER OF CHANGEABLE SPOTS                                              
*                                                                               
CNPW12A  BRAS  RE,TESTPD                                                        
         BNE   CNPW12B                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R8,R0               ADJUST COUNT                                 
*                                                                               
CNPW12B  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R6),7                                                          
         BNE   CNPW12C                                                          
         CLC   2(2,R6),2(R7)       TEST SAME DATE                               
         BE    CNPW12A                                                          
*                                                                               
CNPW12C  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTENUF)                                             
         CLM   R8,1,ELEMNO         CAN WE CHANGE ENOUGH SPOTS                   
         BL    NEQXIT              NO                                           
*                                                                               
         LR    R6,R7               POINT TO FIRST SPOT THIS DATE                
*                                                                               
CNPW12D  TM    6(R6),X'80'         TEST MINUS                                   
         BO    CNPW12E             SKIP                                         
*                                                                               
         BRAS  RE,TESTPD                                                        
         BNE   CNPW12E                                                          
         SR    RF,RF                                                            
         IC    RF,6(R6)            GET NUMBER OF SPOTS                          
         CLM   RF,1,ELEMNO         SPOTS >= NUM TO REMOVE                       
         BH    *+8                                                              
         IC    RF,ELEMNO           ELSE JUST REMOVE ELEMNO                      
         SR    R0,R0                                                            
         IC    R0,ELEMNO                                                        
         SR    R0,RF                                                            
         STC   R0,ELEMNO           SET REMAINING NUMBER TO REMOVE               
*                                                                               
         IC    R0,7(R6)                                                         
         SR    R0,RF               SET REMAINING SPOTS THIS ELEM                
         STC   R0,7(R6)                                                         
         BNZ   CNPW12E                                                          
* DELETE ELEMENT IF NO SPOTS REMAIN                                             
         BRAS  RE,DELEL                                                         
         CLI   ELEMNO,0            ANY MORE TO REMOVE                           
         BE    CNPW12X                                                          
         CLI   0(R6),7             OTO MUST FOLLOW !                            
         BE    CNPW12D                                                          
         DC    H'0'                                                             
*                                                                               
CNPW12E  CLI   ELEMNO,0            ANY MORE TO REMOVE                           
         BE    CNPW12X                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),7                                                          
         BE    CNPW12D                                                          
         DC    H'0'                                                             
*                                                                               
CNPW12X  B     EQXIT                                                            
*                                                                               
CNPW15   GOTO1 VBLDEL                                                           
         MVC   ELEM+2(2),BUELDT                                                 
         MVC   ELEM+7(1),BUNPW                                                  
         CLI   BUNPW,0                                                          
         BE    CNPW80                                                           
* FIND INSERTION POINT                                                          
         MVI   ELCDHI,8                                                         
*                                                                               
CNPW16   LA    R6,BDELEM                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   *+14                                                             
         CLC   2(2,R6),BUELDT                                                   
         BL    *-14                                                             
         GOTO1 TESTGLS,DMCB,ELEM                                                
         BRAS  RE,ADDEL                                                         
         B     CNPW80                                                           
         EJECT                                                                  
*===================================================================*           
* POL - SKED AND C,NPW                                              *           
*===================================================================*           
         SPACE 1                                                                
CNPW20   DS    0H                                                               
         MVI   ERRCD,BADMGPER                                                   
         OC    BDMGDATE,BDMGDATE   TEST THIS LINE IS A MAKEGOOD                 
         BZ    CNPW22              NO                                           
         CLI   BDMGDATE,C'A'       TEST FOR NEW MAKEGOOD                        
         BL    NEQXIT              NO - THEY CAN'T DO IT                        
*                                                                               
CNPW22   CLI   EDTVAL,SKEDEDT                                                   
         BNE   CNPW30                                                           
         SPACE 1                                                                
*================================================================*              
* FOR SKED, COUNT SPOTS THIS WEEK (MAY BE 0) INCLUDING ALL OTO'S *              
* ELEMNO = NET SPOTS THIS WEEK (INCLUDING OTOS)                  *              
*          NET SPOTS FOLLOWED BY ? IF OTO'S PRESENT IN WEEK      *              
*================================================================*              
         SPACE 1                                                                
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,BUELDT                                                    
         GOTO1 VDATCON,DMCB,(2,ELEMDT),WORK                                     
         GOTO1 VADDAY,(R1),WORK,WORK+6,F'6'                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,ELEMDTX)                                  
*                                                                               
CNPW24   BRAS  RE,NEXTEL                                                        
         BNE   CNPW40                                                           
*                                                                               
CNPW26   CLC   2(2,R6),ELEMDT      TEST WITHIN WEEK                             
         BL    CNPW24                                                           
         CLC   2(2,R6),ELEMDTX                                                  
         BH    CNPW40                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ELEMNO                                                        
*                                                                               
         LA    R0,1                                                             
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    RE,R0                                                            
         STC   RE,ELEMNO           UPDATE NET SPOT COUNT                        
         B     CNPW24                                                           
         EJECT                                                                  
*===============================================================*               
* FOR NPW, FIND HIGHEST REGEL NUMBER FOR NEXT DATE IN RECORD    *               
* IF A WEEK HAS 0 SPOTS, IT WILL STILL HAVE 0 SPOTS AFTER C,NPW *               
* BUELDT IS INITIALLY ZERO                                      *               
* COUNT OF NET SPOTS THIS WEEK IS NOT RELEVANT                  *               
*===============================================================*               
*                                                                               
CNPW30   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NONPWCHG)                                              
         TM    BDSTAT3,BDST3_SPODS   TEST SPODS IN BUY                          
         BO    BUYERR                                                           
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CNPW32   BRAS  RE,NEXTEL                                                        
         BNE   CNPWX                                                            
         CLC   2(2,R6),BUELDT      TEST GREATER THAN PREV                       
         BNH   CNPW32                                                           
         MVC   BUELDT,2(R6)        SAVE NEXT REGEL DATE                         
         MVC   ELEMDT,2(R6)                                                     
         MVC   ELEMDTX,2(R6)       FORCE END DATE=START DATE                    
         MVI   ELEMNO,1                                                         
*                                                                               
CNPW34   BRAS  RE,NEXTEL                                                        
         BNE   CNPW40                                                           
*                                                                               
         CLC   2(2,R6),ELEMDT                                                   
         BL    CNPW34                                                           
         CLC   2(2,R6),ELEMDTX                                                  
         BH    CNPW40                                                           
         IC    R0,ELEMNO                                                        
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         B     CNPW34                                                           
         EJECT                                                                  
CNPW40   CLC   ELEMNO,BUNPW        TEST OLD V NEW NPW                           
         BE    CNPW80                                                           
         BL    CNPW60                                                           
         SPACE 1                                                                
*===============================================================*               
* DECREASE IN NPW - SEE IF CAN FIND UNPAID/UNMATCHED/UN-MADEGOOD*               
* REGELS TO DELETE                                              *               
*===============================================================*               
         SPACE 1                                                                
         SR    R0,R0                                                            
         IC    R0,ELEMNO                                                        
         SR    R1,R1                                                            
         IC    R1,BUNPW                                                         
         SR    R0,R1                                                            
         STC   R0,ELEMNO           SET NUMBER OF REGELS TO BE REMOVED           
*                                                                               
         LA    R6,BDELEM                                                        
*                                                                               
CNPW42   BRAS  RE,NEXTEL                                                        
         BE    CNPW44                                                           
CNPW43   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CANTRMVE)                                              
         GOTO1 VDATCON,DMCB,(2,BUELDT),(4,ERRTEXT)                              
         GOTO1 ERROR                                                            
*                                                                               
CNPW44   CLC   2(2,R6),ELEMDT      TEST IN WEEK                                 
         BL    CNPW42                                                           
         CLC   2(2,R6),ELEMDTX                                                  
         BH    CNPW42                                                           
*                                                                               
         TM    6(R6),X'D0'         TEST MINUS/MINUSSED/MG PNDG                  
         BNZ   CNPW42              YES, IGNORE                                  
*                                                                               
         BRAS  RE,TESTPD           IF PAID, IGNORE                              
         BNE   CNPW42                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'10'         TEST AFFID FOLLOWS                           
         BNE   CNPW45              NO - THAT'S FINE                             
         OC    SVNDEF(16),SVNDEF   TEST CAN NTWK                                
         BZ    CNPW42              NO - JUST IGNORE MATCHED SPOTS               
         B     CNPW43              ELSE ERROR IS FATAL                          
*                                                                               
CNPW45   L     RE,ASVDARE             TEST ELEM IN LOCKED DARE FLIGHT           
         ST    R6,SVDRELEM-SVDARED(RE)                                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         BRAS  RE,DELEL            DELETE ELEM AND POINT TO NEXT                
*                                                                               
CNPW46   CLI   0(R6),0                                                          
         BE    CNPW48                                                           
         CLI   0(R6),X'11'         REMOVE NEXT ELEMENT AS NEEDED                
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   CNPW47                                                           
         CLI   0(R6),X'0C'                                                      
         BNE   CNPW48                                                           
         TM    6(R6),X'80'                                                      
         BZ    CNPW48                                                           
CNPW47   BRAS  RE,DELEL            DELETE -OTO                                  
         B     CNPW46                                                           
*                                                                               
CNPW48   IC    R0,ELEMNO           DECREMENT REMAINING COUNT                    
         BCTR  R0,0                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         LTR   R0,R0                                                            
         BZ    CNPW50                                                           
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         BNE   CNPW43              ERROR - SHOULD BE MORE ELEMENTS              
         B     CNPW44                                                           
*                                                                               
CNPW50   CLI   EDTVAL,SKEDEDT                                                   
         BNE   CNPW30              FOR NPW, GO DO NEXT WEEK                     
         B     CNPWX                                                            
         EJECT                                                                  
*==================================================================*            
* INCREASE IN NUMBER OF SPOTS - OK UNLESS FROZEN                   *            
*==================================================================*            
         SPACE 1                                                                
CNPW60   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CLTFRZN)                                               
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    NEQXIT                                                           
         SPACE 1                                                                
*================================================================*              
* SET INSERTION POINT ADDRESS                                    *              
* REGELS ALWAYS GO BEFORE +OTO'S WITH SAME DATE                  *              
*================================================================*              
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
*                                                                               
CNPW62   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    CNPW70                                                           
*                                                                               
         CLI   0(R6),X'0B'                                                      
         BL    CNPW62                                                           
         BE    CNPW64                                                           
*                                                                               
         CLI   0(R6),X'0C'                                                      
         BE    CNPW64                                                           
*                                                                               
         CLI   0(R6),X'24'         INSERT BEFORE ANY EL > X'23'                 
         BL    CNPW62                                                           
         B     CNPW70                                                           
*                                                                               
CNPW64   CLC   BUELDT,2(R6)        INSERT DATE TO CURRENT ELEM                  
         BH    CNPW62              IF HIGH, KEEP GOING                          
         BL    CNPW70              IF LOW, INSERT HERE                          
         CLI   0(R6),X'0C'         EQUAL - TEST FOR +OTO                        
         BNE   CNPW62                                                           
         TM    6(R6),X'80'         IGNORE -OTO                                  
         BO    CNPW62              ELSE HAVE INSERTION POINT                    
         EJECT                                                                  
* ADD NEW ELEMENTS                                                              
*                                                                               
CNPW70   MVC   BUELPRD(2),BDMASPRD                                              
         GOTO1 VBLDEL                                                           
         MVC   ELEM+2(2),ELEMDT                                                 
         ZIC   R0,BUNPW            NEW NPW                                      
         ZIC   RE,ELEMNO           OLD NPW                                      
         SR    R0,RE                                                            
         BNP   CNPW80                                                           
*                                                                               
CNPW78   GOTO1 TESTGLS,DMCB,ELEM                                                
         L     RE,ASVDARE             TEST ELEM IN LOCKED DARE FLIGHT           
         LA    RF,ELEM                POINT TO NEW ELEMENT                      
         ST    RF,SVDRELEM-SVDARED(RE)                                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         BRAS  RE,ADDEL                                                         
         BCT   R0,CNPW78                                                        
*                                                                               
CNPW80   CLI   EDTVAL,SKEDEDT                                                   
         BNE   CNPW30              FOR NPW EDIT GO DO NEXT WEEK                 
         OI    BDSTAT,X'40'        SET SKED DATA                                
* CHECK EXCEED MAXIMUM REGELS                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         IC    R7,SVMAXSPT         GET MAX SPOTS/BUY                            
         LA    R7,1(R7)                                                         
*                                                                               
CNPW81   BRAS  RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         BCT   R7,CNPW81                                                        
         CLC   BUYREC+13(2),OLDBUYLN  TEST BUYREC HAS GOTTEN SMALLER            
         BNH   EQXIT                  YES - LET IT GO                           
         MVI   ERRCD,MAXELEMS                                                   
         B     NEQXIT                                                           
         EJECT                                                                  
CNPWX    L     RE,ASVDARE                                                       
         CLI   EDTVAL,SKEDEDT                                                   
         BNE   *+8                                                              
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN SUPPRESS LATER TESTS           
         LA    RE,SVDRELEM-SVDARED(RE)                                          
         XC    0(4,RE),0(RE)       CLEAR DARE ELEMENT ADDRESS                   
         CLI   EDTVAL,SKEDEDT      DON'T RESET NPW ON SKED                      
         BE    *+10                                                             
         MVC   BDNOWK,BUNPW                                                     
         OC    BDSTAT,BUSTAT                                                    
         B     EQXIT                                                            
         EJECT                                                                  
* NPW CHANGE FOR POL NPW BUYS                                                   
*                                                                               
CNPW90   MVI   ERRCD,BADMGPER                                                   
         OC    BDMGDATE,BDMGDATE   TEST THIS LINE IS A MAKEGOOD                 
         BZ    CNPW91              NO                                           
         CLI   BDMGDATE,C'A'       TEST FOR NEW MAKEGOOD                        
         BL    NEQXIT              NO - THEY CAN'T DO IT                        
*                                                                               
CNPW91   MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CNPW92   BRAS  RE,NEXTEL                                                        
         BNE   CNPW100                                                          
         CLI   EDTVAL,SKEDEDT                                                   
         BNE   CNPW94                                                           
         CLC   2(2,R6),BUELDT                                                   
         BL    CNPW92                                                           
         BH    CNPW100                                                          
CNPW94   BRAS  RE,TESTPD                                                        
         BNE   NEQXIT                                                           
         BRAS  RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
*                                                                               
         ZIC   R0,7(R6)            GET OLD NPW                                  
         SRL   R0,2                                                             
         STC   R0,DUB                                                           
         CLC   DUB(1),BUNPW        OLD V. NEW                                   
         BNH   CNPW98              IF OLD LOW OR EQ, OK                         
* DECREASE IN NPW - SPOTS MUST REMAIN POSITIVE                                  
         LR    R7,R6                                                            
         SR    R0,R0                                                            
         MVI   ERRCD,NEGSPOTS                                                   
         ZIC   R8,BUNPW            GET NEW + SPOTS                              
CNPW95   IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    CNPW96                                                           
         CLI   0(R7),X'1F'                                                      
         BNH   CNPW95                                                           
CNPW96   CLI   0(R7),X'0C'         TEST OTO                                     
         BNE   CNPW98                                                           
         TM    6(R7),X'80'         TEST MINUS                                   
         BZ    CNPW98                                                           
         IC    R0,7(R7)                                                         
         SRL   R0,2                                                             
         SR    R8,R0                                                            
         BM    NEQXIT                                                           
         B     CNPW95                                                           
*                                                                               
CNPW98   NI    7(R6),X'03'         DROP OLD NPW                                 
         IC    R0,BUNPW                                                         
         SLL   R0,2                                                             
         STC   R0,DUB                                                           
         OC    7(1,R6),DUB                                                      
         LTR   R0,R0               TEST NEW NPW=0                               
         BNZ   *+8                                                              
         BRAS  RE,DELEL            YES - DELETE ELEMENT                         
         CLI   EDTVAL,SKEDEDT      TEST SKED=                                   
         BE    EQXIT               YES - EXIT                                   
         B     CNPW92              ELSE NEXT ELEMENT                            
*                                                                               
CNPW100  CLI   EDTVAL,SKEDEDT                                                   
         BNE   CNPWX                                                            
         CLI   BUNPW,0             TEST NEW NPW=0                               
         BE    CNPW80                                                           
* MUST BE SKED = WITH 0 SPOTS ON DATE                                           
         MVC   BUELPRD(2),BDMASPRD                                              
         GOTO1 VBLDEL                                                           
         MVC   ELEM+2(2),BUELDT                                                 
         IC    R0,BUNPW                                                         
         SLL   R0,2                                                             
         STC   R0,ELEM+7           SET NPW IN ELEM                              
         MVI   ELCDHI,X'0D'                                                     
         B     CNPW16                                                           
         EJECT                                                                  
*=============================================================*                 
* TIME CHANGES ALLOWED IF UNMATCHED BUY                       *                 
* OR TO BROADER TIME PERIOD IF MATCHED                        *                 
*                                                             *                 
* FOR CANADIAN NETWORK, AFTER NETWORK LEVEL BUY IS PROCESSED, *                 
* BUTIME FIELD WILL CONTAIN THE TIME DIFFERENCES              *                 
*=============================================================*                 
         SPACE 1                                                                
CHGTIM   NTR1                                                                   
         OC    SVNDEF(16),SVNDEF   TEST CANADIAN NTWK                           
         BZ    CTIM1                                                            
         OC    BUEXPKEY,BUEXPKEY   TEST EXP BUY                                 
         BNZ   CTIM20              YES                                          
         B     CTIM1A                                                           
*                                                                               
CTIM1    MVI   DEMLKSW,C'Y'        INDICATE DEMO LOOK-UPS REQ'D                 
         BRAS  RE,SET50EL          FIX COMSCORE DEMOS                           
*                                                                               
CTIM1A   MVC   DUB(4),BDTIMST                                                   
         LA    R1,DUB                                                           
         BAS   RE,FIXTIME                                                       
         MVC   DUB+4(4),BUTIME                                                  
         LA    R1,DUB+4                                                         
         BAS   RE,FIXTIME                                                       
*                                                                               
         CLC   DUB(2),DUB+4        START TIME ADVANCED                          
         BL    CTIM2               YES                                          
         CLC   DUB+2(2),DUB+6      END TIME CUT BACK                            
         BNH   CTIM10              NO - OK                                      
* CHECK MATCHED                                                                 
*                                                                               
CTIM2    MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CTIM10                                                           
         MVI   ERRCD,MATCHED                                                    
         B     NEQXIT                                                           
*                                                                               
CTIM10   MVC   BDTIMST(4),BUTIME   SET TIME IN BUY                              
*                                                                               
CTIM12   OC    SVNDEF(16),SVNDEF   TEST CANADIAN NTWK                           
         BZ    EQXIT                                                            
* NOTE THAT TIME DIFFERENCES ARE SAVED IN >>MINUTES<<                           
         LA    R1,DUB              POINT TO OLD NORMALIZED TIMES                
         BAS   RE,CNVTIM1                                                       
*                                                                               
         LA    R1,DUB+2                                                         
         BAS   RE,CNVTIM1                                                       
*                                                                               
         LA    R1,DUB+4            POINT TO NEW NORMALIZED TIMES                
         BAS   RE,CNVTIM1                                                       
*                                                                               
         LA    R1,DUB+6                                                         
         BAS   RE,CNVTIM1                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DUB+4          GET NEW START TIME                           
         SR    R1,R1                                                            
         ICM   R1,3,DUB            LESS OLD START TIME                          
         SR    R0,R1                                                            
         STCM  R0,3,BUTIME         SAVE START TIME DIFF IN MINUTES              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DUB+6          GET NEW END TIME                             
         ICM   R1,3,DUB+2          GET OLD END TIME                             
         SR    R0,R1                                                            
         STCM  R0,3,BUTIME+2       SAVE END TIME DIFF IN MINUTES                
         B     EQXIT                                                            
*                                                                               
FIXTIME  OC    2(2,R1),2(R1)       TEST END TIME PRESENT                        
         BNZ   *+10                YES                                          
         MVC   2(2,R1),0(R1)       ELSE SET EQUAL TO START                      
         LH    R0,0(R1)                                                         
         CHI   R0,600                                                           
         BNL   *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,0(R1)                                                         
         LH    R0,2(R1)                                                         
         CHI   R0,600                                                           
         BNL   *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,2(R1)                                                         
         BR    RE                                                               
         EJECT                                                                  
*=============================================================*                 
* CHANGE TIME FOR CANADIAN NETWORK PROGRAM                    *                 
* EXPLODED BUY IS IN AREC1                                    *                 
* BUTIME HAS TIME DIFFERENCES IN MINUTES                      *                 
*=============================================================*                 
         SPACE 1                                                                
CTIM20   DS    0H                                                               
         MVC   DUB(4),BDTIMST      MOVE OLD EXP TIMES                           
         LA    R1,DUB                                                           
         BAS   RE,FIXTIME          GET NORMALIZED TIMES                         
*                                                                               
         LA    R1,DUB                                                           
         BAS   RE,CNVTIM1          CONVERT TO MINUTES                           
         LA    R1,DUB+2                                                         
         BAS   RE,CNVTIM1                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DUB                                                         
         MVC   HALF,BUTIME         ALIGN                                        
         AH    R0,HALF                                                          
         STCM  R0,3,DUB+4          SAVE NEW START TIME                          
*                                                                               
         LTR   R0,R0                                                            
         BM    CNTIMERR                                                         
         CHI   R0,2400                                                          
         BNH   *+8                                                              
         SHI   R0,2400                                                          
         STCM  R0,3,BDTIMST                                                     
         LA    R1,BDTIMST                                                       
         BAS   RE,CNVTIM2          CONVERT TO MILITARY                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DUB+2                                                       
         MVC   HALF,BUTIME+2       ALIGN                                        
         AH    R0,HALF                                                          
         STCM  R0,3,DUB+6          SAVE NEW END TIME                            
*                                                                               
         LTR   R0,R0                                                            
         BM    CNTIMERR                                                         
         CHI   R0,2400                                                          
         BL    *+8                                                              
         SHI   R0,2400                                                          
         STCM  R0,3,BDTIMST+2                                                   
         LA    R1,BDTIMST+2                                                     
         BAS   RE,CNVTIM2          CONVERT TO MILITARY                          
         B     EQXIT                                                            
*                                                                               
CNTIMERR MVC   NERRCD,=Y(OUTOFTIM)                                              
         MVI   ERRCD,NEWERRS                                                    
         MVI   ERRAREA,X'FE'                                                    
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR                                                            
*                                                                               
*                                                                               
* CONVERT MILITARY TIME TO MINUTES                                              
*                                                                               
CNVTIM1  SR    R4,R4                                                            
         ICM   R4,3,0(R1)                                                       
         SRDL  R4,32                                                            
         D     R4,=F'100'          HOURS IN R1, MIN IN R0                       
         MHI   R5,60                                                            
         AR    R4,R5                                                            
         STCM  R4,3,0(R1)                                                       
         BR    RE                                                               
*                                                                               
* CONVERT MINUTES TO MILITARY TIME                                              
*                                                                               
CNVTIM2  SR    R4,R4                                                            
         ICM   R4,3,0(R1)                                                       
         SRDL  R4,32                                                            
         D     R4,=F'60'           HOURS IN R1, MIN IN R0                       
         MHI   R5,100                                                           
         AR    R4,R5                                                            
         STCM  R4,3,0(R1)                                                       
         BR    RE                                                               
         EJECT                                                                  
CHGSLN   NTR1                                                                   
         MVI   BUWHY,X'40'         SET BUY DESC CHANGE                          
* TEST BUY MATCHED                                                              
         SPACE 1                                                                
         MVI   ERRCD,MATCHED                                                    
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    NEQXIT                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOSLNCHG)                                              
         TM    BDSTAT3,BDST3_SPODS                                              
         BO    NEQXIT                                                           
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BE    CSLN10              YES                                          
         CLI   BDTIME,0            TEST P/B                                     
         BE    CSLNX               NO                                           
*                                                                               
* NON POL P/B - PRESERVE PARTNER SHARES                                         
*                                                                               
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R7,1(R6)            GET ELEM LEN                                 
         SRL   R7,3                DIVIDE BY 8 TO SET BCT REG                   
         MVI   ERRCD,PTNRLEN                                                    
CSLN5    ZIC   R0,4(R6)                                                         
         BAS   RE,SETSLN           GET NEW SHARE IN SECONDS                     
         BNE   NEQXIT                                                           
         MVC   4(1,R6),BYTE        SET PRTNR TIME                               
         MVC   5(1,R6),BYTE        AND COST SHARES                              
*                                                                               
         LA    R6,2(R6)                                                         
         BCT   R7,CSLN5                                                         
* DO ACTIVE PARTNER                                                             
         ZIC   R0,BDTIME                                                        
         BAS   RE,SETSLN                                                        
         BNE   BUYERR                                                           
         MVC   BDTIME,BYTE                                                      
         MVC   BDCOSTP,BYTE                                                     
*                                                                               
         B     CSLNX                                                            
         EJECT                                                                  
* POL                                                                           
*                                                                               
CSLN10   MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
CSLN12   BRAS  RE,NEXTEL                                                        
         BNE   CSLNX                                                            
         CLI   1(R6),14                                                         
         BL    CSLN12                                                           
         BH    CSLN14                                                           
* SINGLE PRD ALLOC                                                              
         MVC   11(1,R6),BUSLN                                                   
         B     CSLN12                                                           
* MULTI-PRD ALLOC                                                               
CSLN14   MVI   ERRCD,PTNRLEN                                                    
         ZIC   R8,1(R6)                                                         
         SHI   R8,10                                                            
         SRL   R8,2                                                             
         LA    R7,10(R6)                                                        
CSLN16   ZIC   R0,1(R7)                                                         
         BAS   RE,SETSLN                                                        
         MVC   1(1,R7),BYTE                                                     
         LA    R7,4(R7)                                                         
         BCT   R8,CSLN16                                                        
         B     CSLN12                                                           
*                                                                               
CSLNX    MVC   BDSEC,BUSLN         SET NEW SPTLEN IN BUYREC                     
*                                                                               
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
CSLNX2   BRAS  RE,NEXTEL                                                        
         BNE   CSLNX4                                                           
         GOTO1 TESTGLS,DMCB,(R6)                                                
         B     CSLNX2                                                           
         SPACE 1                                                                
* DELETE FILM ELEMENTS                                                          
         SPACE 1                                                                
CSLNX4   MVI   ELCDLO,X'12'                                                     
         MVI   ELCDHI,X'12'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EQXIT                                                            
CSLNX6   BRAS  RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    CSLNX6                                                           
         B     EQXIT                                                            
         EJECT                                                                  
* R0 HAS OLD PARTNER SECONDS                                                    
* BDSEC IS OLD TOTAL SECS, BUSEC IS NEW TOTAL SECS                              
* CALC (PTNR SECS/TOTAL SECS) * NEW TOTAL SECS                                  
*                                                                               
SETSLN   NTR1                                                                   
         ZIC   R1,BUSLN                                                         
         MR    R0,R0                                                            
         ZIC   RF,BDSEC                                                         
         DR    R0,RF                                                            
         LTR   R0,R0               TEST REMAINDER                               
         BNZ   BUYERR                                                           
         STC   R1,BYTE                                                          
         GOTO1 VCHKSLN                                                          
         XIT1                                                                   
         EJECT                                                                  
CHGTAX   NTR1                                                                   
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+12                                                             
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BNE   CTAX30                                                           
*                                                                               
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
CTAX2    BRAS  RE,NEXTEL                                                        
         BNE   CTAX4                                                            
         BRAS  RE,TESTPD                                                        
         BNE   NEQXIT                                                           
         BRAS  RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
         B     CTAX2                                                            
CTAX4    OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CTAX10                                                           
         MVC   BDNTAX,BUNTAX                                                    
         B     EQXIT                                                            
*                                                                               
CTAX10   MVI   ELCDLO,X'69'        FIND TAX ELEM                                
         MVI   ELCDHI,X'69'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+14                                                             
         MVC   3(3,R6),BUCOST                                                   
         B     CTAX11                                                           
* CREATE NEW ELEM                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,X'69'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+3(3),BUCOST                                                  
         GOTO1 VRECUP,DMCB,BUYREC,DUB,(R6)                                      
* SAVE REC IN REC3                                                              
CTAX11   L     R0,AREC3            SET 'TO' ADDR                                
         LHI   R1,REC2-REC         SET 'TO' LEN                                 
         L     RE,AREC1            SET 'FROM' ADDR                              
         LR    RF,R0               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         EJECT                                                                  
* PROCESS NTWK DEFN REC TO FIND STATIONS WITH TAX                               
* UNLESS CXTRA+1 = 1 WHICH MEANS ALLOC TAX TO ALL STATIONS                      
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         SR    R8,R8                                                            
CTAX12   CLI   SVCXTRA+1,C'1'      TEST APPORTION TO ALL                        
         BE    *+14                                                             
         OC    SVNDTAX,SVNDTAX                                                  
         BZ    CTAX14                                                           
         MVC   FULL,SVNDPCT        ALIGN PCTG                                   
         A     R8,FULL                                                          
CTAX14   LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   CTAX12                                                           
* CLEAR X'68' ELEMS IN NTWK BUY                                                 
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+14                                                             
         XC    7(4,R6),7(R6)                                                    
         B     *-14                                                             
* NOW APPORTION TAX DOLLARS                                                     
         L     R5,BUCOST                                                        
         SRL   R5,8                                                             
         LA    R7,SVNDEF                                                        
CTAX16   CLI   SVCXTRA+1,C'1'                                                   
         BE    CTAX17                                                           
         OC    SVNDTAX,SVNDTAX                                                  
         BNE   CTAX17                                                           
         LTR   R5,R5               TEST NEW TAX = 0                             
         BE    CTAX17              YES                                          
         LTR   R8,R8               TEST NO STATIONS WITH TAX                    
         BNZ   CTAX20              TAX ALLOCATED - SKIP                         
CTAX17   ICM   R1,15,SVNDPCT       ELSE GET PCTG                                
         AR    R1,R1               X 2                                          
         MR    R0,R5               X TAX DOLLARS                                
         LTR   RE,R8                                                            
         BNZ   *+8                                                              
         L     RE,=F'100000'                                                    
         DR    R0,RE               DIVIDE BY TOTAL PCTG                         
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,FULL                                                          
         EJECT                                                                  
* NOW FIND 68 ELEM FOR THIS STATINN                                             
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
CTAX18   BRAS  RE,NEXTEL                                                        
         BE    CTAX18X                                                          
* NO 68 ELEM IN BUY SO NTWK HAS CHANGED - OK IF STA TAX = 0 *                   
         LTR   R5,R5                                                            
         BE    CTAX20                                                           
         MVI   ERRCD,NOTSAME       TELL USER TO GET RIGHT NTWK DEF              
         B     BUYERR                                                           
CTAX18X  CLC   2(5,R6),SVNDMKST                                                 
         BNE   CTAX18                                                           
         MVC   7(4,R6),FULL                                                     
*                                                                               
CTAX20   LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   CTAX16                                                           
         DROP  R7                                                               
* TEST SUM = BUCOST                                                             
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         LR    R7,R6                                                            
         SR    R8,R8                                                            
         B     *+12                                                             
*                                                                               
CTAX22   BRAS  RE,NEXTEL                                                        
         BNE   CTAX24                                                           
         ICM   R0,15,7(R6)                                                      
         AR    R8,R0                                                            
         CLC   7(4,R6),7(R7)                                                    
         BNH   *+6                                                              
         LR    R7,R6               SAVE A(LARGEST)                              
         B     CTAX22                                                           
* ADJUST LARGEST AMOUNT                                                         
CTAX24   SR    R8,R5                                                            
         ICM   R0,15,7(R7)                                                      
         SR    R0,R8                                                            
         STCM  R0,15,7(R7)                                                      
         B     EQXIT                                                            
         EJECT                                                                  
CTAX30   XC    BDNTAX,BDNTAX       RESET OLD TAX RATE                           
         MVI   ELCDLO,X'69'                                                     
         MVI   ELCDHI,X'69'                                                     
         LR    R7,R6               SAVE X'68' POINTER                           
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CTAX32                                                           
         MVC   2(4,R6),7(R7)                                                    
         OC    7(4,R7),7(R7)       TEST NEW TAX = 0                             
         BNZ   EQXIT                                                            
         BRAS  RE,DELEL                                                         
         B     EQXIT                                                            
*                                                                               
* CREATE NEW ELEM                                                               
*                                                                               
CTAX32   OC    7(4,R7),7(R7)       TEST NEW TAX = 0                             
         BZ    EQXIT                                                            
         XC    DUB,DUB                                                          
         MVI   DUB,X'69'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+2(4),7(R7)                                                   
         GOTO1 VRECUP,DMCB,BUYREC,DUB,(R6)                                      
         B     EQXIT                                                            
         EJECT                                                                  
CHGREP   NTR1                                                                   
*                                                                               
         CLC   BDREP,BUREP         TEST NEW=OLD                                 
         BNE   CREP005                                                          
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN SUPPRESS LATER TESTS           
         B     EQXIT                                                            
*                                                                               
CREP005  MVI   ELCDLO,6                                                         
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM           DON'T ALLOW REP CHANGE IF PAID               
CREP010  BRAS  RE,NEXTEL                                                        
         BNE   CREP020                                                          
         BRAS  RE,TESTPD                                                        
         BNE   EQXIT                                                            
         B     CREP010                                                          
*                                                                               
CREP020  CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   CREP030              NO                                          
         CLI   BUYMD,C'N'          NETWORK?                                     
         BNE   CREP030              NO                                          
         MVC   BDREP,BUREP         MOVE NEW SPECIAL REP CODE                    
         B     EQXIT                                                            
*                                                                               
CREP030  CLI   CHGADDR,0           TEST SPECIAL REP-PASS 1                      
         BNE   EQXIT               YES - DON'T CHANGE NOW                       
         CLC   =C'CM',BUTRCODE     TEST FOR CHANGE MULTIPLE                     
         BNE   *+12                                                             
         CLI   CMPASS,2            TEST FOR SECOND PASS                         
         BNE   EQXIT               NO-EXIT NOW SKIPPING POINTER CHANGES         
*&&DO                                                                           
* BUILD OLD REP KEY                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,X'03'           TYPE                                         
         MVC   KEY+1(2),BDREP      REP                                          
         MVC   BYTE,SVKEY          MOVE AGY/MD                                  
         NI    BYTE,X'F0'          DROP MD                                      
         OC    KEY+1(1),BYTE       'OR' IN AGY                                  
         MVC   KEY+3(5),SVKEY+4    MKT/STA                                      
         MVC   KEY+8(3),SVKEY+1    CLT/PRD                                      
         MVC   KEY+11(1),SVKEY+9   EST                                          
         MVC   KEY+12(1),SVKEY+11  LINE                                         
         OC    BDREP,BDREP                                                      
         BZ    CREP050                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CREP040                                                          
         MVC   KEY,KEYSAVE         RESTORE KEY, WE MAY HAVE A BAD KEY           
         B     CREP050             MUST BE DELETED ALREADY                      
CREP040  OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
CREP050  OC    BUREP,BUREP                                                      
         BZ    CREP080                                                          
*                                                                               
         NC    KEY+1(2),=X'F000'                                                
         OC    KEY+1(2),BUREP                                                   
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CREP060                                                          
         NI    KEY+13,X'3F'        UNDELETE                                     
         OC    KEY+14(4),KEY+14    TEST THERE IS A DISK ADDR                    
         BNZ   *+10                                                             
         MVC   KEY+14(4),SVKEY+14  ELSE SET IT NOW                              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     CREP080                                                          
         EJECT                                                                  
CREP060  XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   KEY+14(4),SVKEY+14                                               
*                                                                               
CREP070  MVC   COMMAND,=C'DMADD'                                                
         GOTO1 DIR                                                              
*&&                                                                             
CREP080  DS    0H                                                               
         MVC   HALF,BDREP                                                       
         MVI   BYTE,C'C'                                                        
         BRAS  RE,CHKTRD           ORIGINAL REP IS TRADE?                       
         BE    *+8                 NO, CASH                                     
         MVI   BYTE,C'T'                                                        
*                                                                               
         MVC   HALF,BUREP                                                       
         MVI   BYTE2,C'C'                                                       
         BRAS  RE,CHKTRD           CHANGE REP IS TRADE?                         
         BE    *+8                 NO, CASH                                     
         MVI   BYTE2,C'T'                                                       
*                                                                               
         CLC   BYTE,BYTE2          DOES ORDER TYPE (C/T) MATCH?                 
         BE    CREP090             YES                                          
         GOTO1 ATESTDAR,DMCB,(RC)  NO, TEST ORDER LOCK FOR ORIG BUY             
         B     CREPX                                                            
*                                                                               
CREP090  L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN SUPPRESS LATER TESTS           
*                                                                               
CREPX    MVC   BDREP,BUREP         SET REP IN BUYLINE                           
         B     EQXIT                                                            
*                                                                               
         DS    F                                                                
CHKTRD   OC    HALF,HALF           HAVE REP CODE?                               
         BZR   RE                   NO                                          
         ST    RE,CHKTRD-4                                                      
         LHI   RE,VRCPACK-BUYSAVE  ONLY CHECK THAT FIRST 2 DIGITS MATCH         
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',HALF),FULL                                       
         LA    RE,2                DEFAULT - 3 CHAR MUST MATCH                  
         CLI   SVDARPRF+14,C'Y'    MULTIPLE TRADE CODES?                        
         BNE   *+6                 NO , ALL 3 CHAR REP CODE MUST MATCH          
         BCTR  RE,0                YES, ONLY FIRST 2 CHAR MUST MATCH            
         CLC   SVDARPRF+6(0),FULL                                               
         EX    RE,*-6                                                           
         BE    *+6                                                              
         SR    RE,RE                                                            
         LTR   RE,RE                                                            
CHKTRDX  L     RE,CHKTRD-4                                                      
         BR    RE                                                               
         EJECT                                                                  
*======================================================*                        
* CHANGE EXCHANGE RATE ELEMENT VALUES                  *                        
*======================================================*                        
         SPACE 1                                                                
CHGXRT   NTR1                                                                   
         MVI   ELCDLO,6            CHECK FOR BILLED/PAID SPOTS                  
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
CHGXRT2  BRAS  RE,NEXTEL                                                        
         BNE   CHGXRT4                                                          
         BRAS  RE,TESTPD                                                        
         JNZ   NEQXIT              BILLED/PAID - CAN'T CHANGE XCHG              
         B     CHGXRT2                                                          
*                                                                               
CHGXRT4  MVI   ELCDLO,XCHCODEQ     LOCATE OLD EXCHANGE RATE ELEMENT             
         MVI   ELCDHI,XCHCODEQ     (IF ANY)                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CHGXRT6                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM             POINT TO SAVED ELEMENT                       
         LA    RE,XCHLENQ-1                                                     
         EX    RE,*+8              SAVE THE OLD ELEMENT                         
         B     *+10                                                             
         MVC   0(0,R7),0(R6) *EXECUTED*                                         
*                                                                               
         BRAS  RE,DELEL            DELETE THE OLD ELEMENT                       
         NI    BDCIND2,X'FF'-X'40' RESET EXCHANGE RATE USED FLAG                
         B     CHGXRT10                                                         
*                                                                               
CHGXRT6  XC    ELEM,ELEM           BUILD NEW EXCHANGE ELEMENT                   
         LA    R7,ELEM                                                          
         USING XCHELEM,R7                                                       
*                                                                               
         MVI   XCHCODE,XCHCODEQ                                                 
         MVI   XCHLEN,XCHLENQ                                                   
         MVC   XCHCTYP,SVCXTRA+9                                                
         MVC   XCHSTYP,SVSTACNT                                                 
         MVC   XCHDTYP,SVSTADOL                                                 
         MVC   XCHC58,SVSTACTX     C58 TAX                                      
*                                                                               
         MVI   ERRCD,XRTRERR                                                    
         OC    SVXRATE,SVXRATE     TEST EXCHANGE RATE EXISTS                    
         JZ    NEQXIT              NO-ERROR                                     
         MVC   XCHRATE,SVXRATE     CURRENT EXCHANGE RATE                        
*                                                                               
CHGXRT10 CLI   EDTVAL,CTYPEDT      TEST OVERRIDE THIS TIME                      
         BNE   *+10                                                             
         MVC   XCHCTYP,BUCTYPE                                                  
*                                                                               
         CLI   EDTVAL,STYPEDT                                                   
         BNE   *+10                                                             
         MVC   XCHSTYP,BUSTYPE                                                  
*                                                                               
         CLI   EDTVAL,C58EDT                                                    
         BNE   *+10                                                             
         MVC   XCHC58,BUC58TAX                                                  
*                                                                               
         CLI   EDTVAL,XRTEDT                                                    
         BNE   *+10                                                             
         MVC   XCHRATE,BUXRATE                                                  
*                                                                               
         CLI   XCHCTYP,C'C'        TEST CANADIAN CLIENT                         
         BNE   CHGXRT12                                                         
         CLI   XCHSTYP,C'C'        TEST CANADIAN STATION                        
         BNE   CHGXRT12                                                         
         CLI   XCHDTYP,C'C'        TEST CANADIAN DOLLARS                        
         JE    EQXIT               ALL CANADIAN - NO EXCHANGE NEEDED            
*                                                                               
CHGXRT12 BRAS  RE,ADDEL            ADD THE ELEMENT                              
         OI    BDCIND2,X'40'       INDICATE IN BUY DESC                         
         J     EQXIT                                                            
         DROP  R7                                                               
         EJECT                                                                  
*======================================================*                        
* CHANGE CANADIAN NETWORK STATION PERCENTAGES          *                        
* AFTER NEW 68 ELEMENTS CREATED, CONTINUE AS IF THERE  *                        
* HAD BEEN A CHANGE OF COST                            *                        
*======================================================*                        
         SPACE 1                                                                
CHGSPCT  NTR1                                                                   
         MVI   ERRCD,NOSTADIS                                                   
         CLI   SVRCLOPT,RCLSTA                                                  
         BNE   BUYERR                                                           
*                                                                               
         MVI   ERRCD,BLLDPAID                                                   
         MVI   ELCDLO,6            CHECK FOR BILLED/PAID SPOTS                  
         MVI   ELCDHI,13           AND NON-ZERO COST OVERRIDES                  
         LA    R6,BDELEM                                                        
*                                                                               
SPCT1    BRAS  RE,NEXTEL                                                        
         BNE   SPCT1X                                                           
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   BUYERR                                                           
         TM    6(R6),X'20'         TEST COST OVRD                               
         BZ    SPCT1                                                            
         OC    7(3,R6),7(R6)       TEST NON-ZERO                                
         BZ    SPCT1                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPCTCHG)                                              
         B     BUYERR                                                           
*                                                                               
SPCT1X   LA    R2,BUYINP2H         POINT TO F1 SCREEN OUTPUT AREA               
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
*                                                                               
SPCT2    BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVRCLSTA,SVRCLSTA   TEST STARTING STATION                        
         BZ    SPCT10              NO                                           
*                                                                               
SPCT4    GOTO1 STAPACK,DMCB,(C'U',2(R6)),DUB,(X'80',WORK)                       
         CLC   SVRCLSTA,WORK       REACHED START YET ?                          
         BNE   SPCT2               NO                                           
*                                                                               
SPCT10   ZIC   R0,0(R2)            BUMP TO NEXT UNP FIELD                       
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    SPCT10                                                           
         CLI   0(R2),9                                                          
         BNH   SPCT12                                                           
*                                                                               
         MVI   ERRCD,BADPCTG                                                    
         ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,8(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         MVC   7(4,R6),4(R1)       SET NEW PCTG                                 
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    SPCT10                                                           
                                                                                
*=========================================================                      
* CHECK PCTS SUM TO 100                                                         
*=========================================================                      
                                                                                
SPCT12   SR    R8,R8                                                            
         LA    R6,BDELEM                                                        
*                                                                               
SPCT14   BRAS  RE,NEXTEL                                                        
         BNE   SPCT20                                                           
         ICM   R0,15,7(R6)                                                      
         AR    R8,R0                                                            
         B     SPCT14                                                           
*                                                                               
SPCT20   LA    R2,BUYINP2H         FIND 'TOT=' AND SHOW CURRENT TOTAL           
*                                                                               
SPCT22   ZIC   R0,0(R2)                                                         
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BZ    SPCT22                                                           
         CLC   =C'TOT=',8(R2)                                                   
         BNE   SPCT22                                                           
*                                                                               
         LA    R4,13(R2)                                                        
         ZIC   RE,0(R2)                                                         
         SHI   RE,9                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         EDIT  (R8),(7,(R4)),3,ALIGN=LEFT                                       
         OI    6(R2),X'80'         SET XMT FLAG                                 
*                                                                               
         MVI   ERRCD,STPCTERR                                                   
         C     R8,=F'100000'                                                    
         BNE   BUYERR                                                           
*                                                                               
* NOW UPDATE CORE TABLE OF BUYREC PCTGS                                         
*                                                                               
         L     R7,ASVNOLD                                                       
         USING SVNDEFD,R7                                                       
*                                                                               
         LA    R6,BDELEM                                                        
SPCT26   BRAS  RE,NEXTEL                                                        
         BNE   SPCT30                                                           
         MVC   SVNDPCT,7(R6)                                                    
         LA    R7,L'SVOLDDEF(R7)                                                
         B     SPCT26                                                           
*                                                                               
SPCT30   MVC   BUCIND,BDCIND       SET FIELDS AS IF COSTEDT                     
         MVC   BUCIND2,BDCIND2                                                  
         MVC   BUCOST,BDCOST                                                    
         BRAS  RE,CHGCOST          PRETEND COST CHANGE AT NTWK LEVEL            
         BNE   BUYERR              CAN'T DO IT - TROUBLE                        
* NEED TO UPDATE SUBROUTINE ADDRESS IN CHGADDR TO CHGCOST                       
         L     RF,=A(CHGCOST)                                                   
         A     RF,RELO                                                          
         ST    RF,CHGADDR                                                       
         B     EQXIT                                                            
         EJECT                                                                  
*======================================================*                        
* CHANGE CANADIAN NETWORK EXPLODED DAYS/TIMES          *                        
* INPUT FORMAT IS +3/10P OR -2/11A                     *                        
*              OR =/10P FOR SAME DAY/DIFFERENT TIME    *                        
* BUILD TABLE OF CHANGED STATIONS IN AREC4             *                        
* TABLE FORMAT IS 0000/STA(5)  REL DAY (2)  TIME (2)   *                        
*======================================================*                        
         SPACE 1                                                                
CHGSDT   NTR1                                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,6            CHECK FOR BILLED/PAID SPOTS                  
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
SDT1     BRAS  RE,NEXTEL                                                        
         BNE   SDT1X                                                            
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    SDT1                                                             
         MVI   ERRCD,BLLDPAID                                                   
         B     NEQXIT                                                           
*                                                                               
SDT1X    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
*                                                                               
         L     R7,AREC4                                                         
         XC    0(256,R7),0(R7)                                                  
         XC    256(256,R7),256(R7)                                              
*                                                                               
         LA    R2,BUYOUTH                                                       
*                                                                               
SDT2     BRAS  RE,NEXTEL                                                        
         BNE   SDT20                                                            
* NOTE - ALWAYS BUILD COMPLETE TABLE                                            
         MVC   0(5,R7),2(R6)       SET MKT/STA IN TABLE                         
         MVC   5(4,R7),=X'EEEEEEEE'   SET 'SAME AS NETWORK' FLAG                
*                                                                               
SDT4     ZIC   R0,0(R2)            BUMP TO NEXT UNP FIELD                       
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    SDT4                YES - IGNORE                                 
*                                                                               
         CLI   5(R2),0             TEST INPUT                                   
         BE    SDT14               NO                                           
*                                                                               
         MVI   ERRCD,DAYERR                                                     
         LA    R4,8(R2)            POINT TO START OF INPUT                      
         CLI   8(R2),C'='          TEST SAME DAY AS NTWK                        
         BNE   SDT6                                                             
         LA    R4,1(R4)                                                         
         XC    5(2,R7),5(R7)       SET TABLE VALUE TO 0                         
         B     SDT10                                                            
*                                                                               
SDT6     CLI   0(R4),C'+'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'-'                                                       
         BNE   BUYERR                                                           
*                                                                               
         CLI   1(R4),C'0'                                                       
         BL    BUYERR                                                           
         CLI   1(R4),C'7'                                                       
         BH    BUYERR                                                           
*                                                                               
         ZIC   R0,1(R4)                                                         
         SLL   R0,28                                                            
         SRL   R0,28                                                            
         CLI   0(R4),C'-'                                                       
         BNE   *+6                                                              
         LNR   R0,R0                                                            
         STCM  R0,3,5(R7)          SET SIGNED REL DAY                           
         LA    R4,2(R4)                                                         
*                                                                               
SDT10    MVI   ERRCD,TIMERR                                                     
         CLI   0(R4),C'/'          TEST FOR SEPARATOR                           
         BE    SDT12                                                            
         CLC   1(3,R4),SPACES                                                   
         BH    BUYERR                                                           
         B     SDT14                                                            
*                                                                               
SDT12    LA    R4,1(R4)                      POINT BEYOND                       
         GOTO1 VCALLOV,DMCB,0,X'D9000A0E'    GET ADDRESS OF TIMVAL              
         L     RF,0(R1)                                                         
         LA    RE,8(R2)            POINT TO START OF INPUT                      
         ZIC   R0,5(R2)            GET INPUT LENGTH                             
         AR    R0,RE               POINT BEYOND END                             
         SR    R0,R4               GIVES LENGTH OF TIME INPUT                   
         GOTO1 (RF),(R1),((R0),(R4)),FULL                                       
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         MVC   7(2,R7),FULL        MOVE START TIME ONLY                         
*                                                                               
SDT14    LA    R7,9(R7)            NEXT TABLE ENTRY                             
         B     SDT2                                                             
         EJECT                                                                  
* NOW UPDATE THE EXPLODED BUYS *                                                
         SPACE 1                                                                
SDT20    DS    0H                                                               
         MVC   DUB(4),AREC1                                                     
         MVC   DUB+4(4),AREC3                                                   
         GOTO1 MOVEREC             SAVE NTWK BUY IN REC3                        
*                                                                               
         L     R7,AREC4                                                         
*                                                                               
SDT22    OC    0(5,R7),0(R7)       TEST FOR MORE STATIONS                       
         BZ    SDTX                                                             
         CLC   5(4,R7),=X'EEEEEEEE'   TEST 'SAME AS NETWORK'                    
         BNE   SDT22A                                                           
         XC    5(4,R7),5(R7)       FIX ENTRY FOR DISPLAY LOGIC                  
         L     RE,AREC3            DIG TIME OUT OF SAVED RECORD                 
         LA    RE,BDTIMST-BUYREC(RE)                                            
         MVC   7(2,R7),0(RE)       SET TIME TO NTWK START TIME                  
         B     SDT32                                                            
*                                                                               
SDT22A   MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),0(R7)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
         BRAS  RE,FIXREC                                                        
         MVC   KEY+4(5),0(R7)                                                   
*                                                                               
         L     R4,AREC3            DIG NTWK VALUES OUT OF SAVED RECORD          
         LA    R4,BDSTART-BUYREC(R4)                                            
         MVC   BUSTARTB(6),0(R4)                                                
         GOTO1 VDATCON,DMCB,(3,BUSTARTB),(2,BUSTARTP)                           
         GOTO1 (RF),(R1),(3,BUENDB),(2,BUENDP)                                  
*                                                                               
         GOTO1 (RF),(R1),(3,BUSTARTB),(0,WORK)    FROM NETWK                    
         GOTO1 (RF),(R1),(3,BDSTART),(0,WORK+6)        EX BUY                   
         L     RF,VCOMFACS                 GET THE RELATIVE DIFF OF             
         L     RF,CPERVERT-COMFACSD(RF)    BDSTART IN NETWORK &                 
         GOTO1 (RF),DMCB,WORK,WORK+6       EXPLODED BUYS                        
         LH    R2,8(R1)                                                         
         BNP   SDT23                                                            
         BCTR  R2,0                FIX FENCE POST                               
         B     *+8                                                              
SDT23    LA    R2,1(R2)                                                         
         LH    R1,5(R7)            GET SIGNED REL DAY FROM TABLE                
         SR    R1,R2               SUBTRACT OLD FROM NEW                        
         STH   R1,BUEXPDAY         STORE IN BUEXPDAY                            
*                                                                               
         MVC   HALF,5(R7)          MOVE SIGNED REL DAY                          
         LH    R0,HALF             AND SET IN R0                                
         MVC   BDSTART(6),BUSTARTB                                              
         LA    R4,BDEND                                                         
         BAS   RE,GETDATE                                                       
         LA    R4,BDSTART                                                       
         BAS   RE,GETDATE                                                       
         EJECT                                                                  
* NEED TO SET BIT VALUES FOR DAYS                                               
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 VGETDAY,(R1),WORK,DUB                                            
*                                                                               
         LA    R1,DAYTAB                                                        
         LA    R0,7                                                             
*                                                                               
SDT24    CLC   DUB(2),0(R1)                                                     
         BE    SDT26                                                            
         LA    R1,4(R1)                                                         
         BCT   R0,SDT24                                                         
         DC    H'0'                                                             
         EJECT                                                                  
SDT26    MVC   BUSVSEDY,BDSEDAY    SAVE OLD VALUE                               
         MVC   BDSEDAY,2(R1)       AND SET NEW                                  
         MVC   BUSVDAYS,BDDAY                                                   
         MVC   BDDAY,3(R1)                                                      
*                                                                               
         CLC   SVSTARTB,BDSTART    TEST STILL IN ESTIMATE PERIOD                
         BH    SDT26ERR                                                         
         CLC   SVENDB,BDEND                                                     
         BL    SDT26ERR                                                         
         B     SDT26X                                                           
*                                                                               
                                                                                
SDT26ERR MVC   NERRCD,=Y(OUTOFEST)                                              
         MVI   ERRCD,NEWERRS                                                    
         MVI   ERRAREA,X'FE'                                                    
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR                                                            
*                                                                               
SDT26X   DS    0H                                                               
         BRAS  RE,CHGPER                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         CLC   7(2,R7),=X'EEEE'    TEST 'SAME AS NETWORK'                       
         BNE   SDT27                                                            
         L     RE,AREC3                                                         
         MVC   7(2,R7),BDTIMST-BUYREC(RE)                                       
*                                                                               
SDT27    CLC   BDTIMST,7(R7)       TEST CHANGE OF TIME                          
         BE    SDT30                                                            
         XC    BUTIME,BUTIME                                                    
         MVC   BUTIME(2),7(R7)                                                  
         SPACE 1                                                                
* NEED TO CALCULATE DURATION                                                    
         SPACE 1                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMST        GET OLD START TIME                           
         D     R0,=F'100'          GIVES HOURS IN R1, MINUTES IN R0             
         MHI   R1,60               CONVERT HOURS TO MINUTES                     
         AR    R1,R0               ADD MINUTES                                  
         ST    R1,FULL             SAVE START TIME IN MINUTES                   
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND       GET OLD END TIME                             
         BZ    SDT30                                                            
         CLC   BDTIMST,BDTIMEND                                                 
         BL    *+8                                                              
         AHI   R1,2400                                                          
         D     R0,=F'100'                                                       
         MHI   R1,60                                                            
         AR    R1,R0                                                            
*                                                                               
         S     R1,FULL             END - START GIVES DURATION                   
         ST    R1,FULL             SAVE IT                                      
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BUTIME         GET NEW START TIME                           
         STCM  R1,3,BDTIMST        AND SET IN SF BUY RECORD                     
         D     R0,=F'100'                                                       
         MHI   R1,60               GET HRS X 60                                 
         AR    R1,R0               ADD MINUTES                                  
         A     R1,FULL             +DURATION GIVES NEW END TIME                 
*                                                                               
         CHI   R1,2400                                                          
         BL    *+8                                                              
         SHI   R1,2400                                                          
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'60'          GET HOURS IN R1, MINUTES IN R0                
         MHI   R1,100                                                           
         AR    R1,R0                                                            
         STCM  R1,3,BDTIMEND                                                    
*                                                                               
SDT30    DS    0H                                                               
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT                                                         
*                                                                               
SDT32    LA    R7,9(R7)            NEXT TABLE ENTRY                             
         B     SDT22                                                            
*                                                                               
SDTX     MVC   DUB(4),AREC3        SET 'FROM' ADDRESS                           
         MVC   DUB+4(4),AREC1      SET 'TO' ADDRESS                             
         GOTO1 MOVEREC             MOVE NETWORK RECORD TO REC1                  
         B     EQXIT                                                            
*                                                                               
DAYTAB   DC    C'MO',X'1040'       DAY NUMBER/SPOT DAY BIT VALUE                
         DC    C'TU',X'2020'                                                    
         DC    C'WE',X'3010'                                                    
         DC    C'TH',X'4008'                                                    
         DC    C'FR',X'5004'                                                    
         DC    C'SA',X'6002'                                                    
         DC    C'SU',X'7001'                                                    
         EJECT                                                                  
PUTIT    NTR1                                                                   
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+12                                                             
         CLI   CMPASS,1            YES-SKIP PUTREC ON FIRST PASS                
         BE    PUTITX                                                           
*                                                                               
PUTIT1   DS    0H                                                               
         TM    SVAFLAG1,X'22'      TEST CTA OR TRADE ACTIVE                     
         BZ    PUTIT2                                                           
         GOTO1 GOGETCTA,DMCB,('CIBCHGQ',AREC)                                   
*                                                                               
PUTIT2   DS    0H                                                               
         TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    PUTIT3              YES-DON'T WRITE THE RECORD                   
         TM    WRKRUPSW,WRKRUPSW_NOIO OR WRKR UPLOAD                            
         BO    PUTIT3                                                           
         MVI   SVMGINIT,0          RESET MG TABLE REBUILD                       
         GOTO1 PUTREC                                                           
*                                                                               
PUTIT3   TM    BUWHY,X'70'         TEST TYPE OF CHANGE                          
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
         TM    BUWHY3,X'80'        TEST TYPE OF CHANGE                          
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
*                                                                               
PUTITX   B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE CHANGES BUY RECORD PERIOD                   *                      
*========================================================*                      
         SPACE 1                                                                
CHGPER   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VBLDQLST            BUILD REQUEST PRD LIST                       
         SPACE 1                                                                
* TEST NO +OTO'S OUTSIDE BUY DESC PERIOD *                                      
         SPACE 1                                                                
         CLI   BDNOWK,0            TEST NPW=0                                   
         BE    *+12                YES - NO OTO'S OUTSIDE BUY DESC              
         CLI   SVAPROF+5,C'1'                                                   
         BE    CPER30                                                           
         MVI   ERRCD,NOTINPER                                                   
         MVI   ELCDLO,X'0C'                                                     
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         MVI   ELCDLO,7                                                         
         MVC   ELCDHI,ELCDLO                                                    
         CLI   BDNOWK,0                                                         
         BNE   *+8                                                              
         MVI   ELCDLO,X'0B'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CPER10   BRAS  RE,NEXTEL                                                        
         BNE   CPER30                                                           
         TM    6(R6),X'80'                                                      
         BO    CPER10                                                           
         CLC   2(2,R6),BUSTARTP                                                 
         JL    NEQXIT                                                           
         CLC   2(2,R6),BUENDP                                                   
         JH    NEQXIT                                                           
         B     CPER10                                                           
*                                                                               
CPER30   DS    0H                                                               
         MVI   DEMLKSW,C'Y'        SET DEMO LOOK-UP REQ'D                       
         BRAS  RE,SET50EL          SET COMSCORE FLAGS AS NEEDED                 
         MVI   RCLOPT,0                                                         
         TM    SVOPTS,X'02'        TEST NO ROTATION REQ                         
         BO    *+8                                                              
         MVI   RCLOPT,RCLROT                                                    
* BUILD LIST OF WEEKS IN NEW BUY PERIOD                                         
         LA    R4,BDSTART                                                       
         L     R5,AREC2                                                         
         BRAS  RE,BLDWKS                                                        
*                                                                               
         MVI   ERRCD,BADMGPER                                                   
         OC    BDMGDATE,BDMGDATE   TEST THIS LINE IS A MAKEGOOD                 
         BZ    CPER35              NO - THEY CAN DO IT                          
         CLI   BDMGDATE,C'A'       TEST THIS IS A NEW MAKEGOOD LINE             
         BNL   CPER35              YES                                          
         OC    6(6,R5),6(R5)       NO - ALLOW ONE WEEK ONLY                     
         BNZ   CPERERR                                                          
*                                                                               
CPER35   XC    BUELDATA,BUELDATA                                                
         MVC   BUELPRD(2),BDMASPRD                                              
         GOTO1 VBLDEL              BUILD MODEL ELEMENT                          
         SPACE 1                                                                
* TEST DAYS EXPANDED *                                                          
         SPACE 1                                                                
CPER40   DS    0H                                                               
         ZIC   R0,BDSEDAY          PICK UP NEW FIELD                            
         SRDL  R0,4                                                             
         STC   R0,BUDAY1IN         SET START DAY NUMBER                         
         SRL   R1,28                                                            
         STC   R1,BUDAYXIN         SET END DAY NUMBER                           
*                                                                               
         CLI   BDSEDAY,0           TEST DATA PRESENT                            
         BNE   CPER50              YES                                          
*                                                                               
         ZIC   R0,BDDAY            GET NEW START DAY                            
         LA    RE,7                                                             
         SRL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         STC   RE,BUDAY1IN                                                      
*                                                                               
         ZIC   R0,BDDAY            GET NEW END DAY                              
         SLL   R0,24                                                            
         SR    RE,RE                                                            
         SLL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STC   RE,BUDAYXIN                                                      
*                                                                               
CPER50   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,BUSVSEDY                                                      
         SRDL  R0,4                                                             
         STC   R0,BUDAY1                                                        
         SRL   R1,28                                                            
         STC   R1,BUDAYX                                                        
         CLI   BUSVSEDY,0          TEST DATA PRESENT                            
         BNE   CPER60                                                           
*                                                                               
         ZIC   R0,BUSVDAYS         GET OLD START DAY                            
         LA    RE,7                                                             
         SRL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         STC   RE,BUDAY1                                                        
*                                                                               
         ZIC   R0,BUSVDAYS         GET OLD END DAY                              
         SLL   R0,24                                                            
         SR    RE,RE                                                            
         SLL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STC   RE,BUDAYX                                                        
*                                                                               
CPER60   MVI   ERRCD,NODAYCHG                                                   
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    CPER70              NO                                           
         OC    BUEXPKEY,BUEXPKEY   TEST EXPLODED BUYS                           
         BNZ   CPER70              YES - ALLOW CHANGES                          
         CLC   HALF(2),HALF2       TEST SAME DAYS                               
**NOP**  BNE   CPERERR                                                          
*                                                                               
CPER70   CLC   BUDAY1IN,BUDAY1     TEST SAME START DAY                          
         BE    CPER90                                                           
* SINCE ELEMENT DATES WILL CHANGE MUST CHECK FOR MAKEGOODS *                    
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BNE   CPER90                                                           
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
CPER80   BRAS  RE,NEXTEL                                                        
         BNE   CPER90                                                           
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         JO    NEQXIT                                                           
         MVI   ERRCD,DARMGPND                                                   
         TM    6(R6),X'10'                                                      
         JO    NEQXIT                                                           
         B     CPER80                                                           
*                                                                               
CPER90   CLC   BUDAY1IN,BUDAY1     TEST LATER START DAY                         
         BH    CPER100             YES-NOT EXPANDED                             
         CLC   BUDAYXIN,BUDAYX     TEST EARLIER END DAY                         
         BL    CPER100             YES-NOT EXPANDED                             
         B     CPER110             DAYS EXPANDED - KEEP AFFIDS                  
         SPACE 1                                                                
* TEST AFFIDS PRESENT *                                                         
         SPACE 1                                                                
CPER100  MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CPER110                                                          
*                                                                               
         MVI   ERRCD,MATCHED                                                    
         B     CPERERR                                                          
         EJECT                                                                  
* SET SEARCH ARGUMENTS *                                                        
         SPACE 1                                                                
CPER110  LA    RE,X'0B'                                                         
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         LA    RE,X'06'                                                         
         STC   RE,HALF             SET REGEL SEARCH ARGS IN HALF                
         STC   RE,HALF+1                                                        
         LA    RE,1(RE)                                                         
         STC   RE,HALF2            AND OTO SEARCH ARGS IN HALF2                 
         STC   RE,HALF2+1                                                       
*                                                                               
         CLC   BUDAY1IN,BUDAY1     TEST CHANGE OF START DAY                     
         BE    CPER130             NO                                           
* MAKE SURE NO BLLD/PAID/MADE-GOODS                                             
         IC    RE,HALF             REGEL CODE                                   
         STC   RE,ELCDLO                                                        
         LA    RE,2(RE)                                                         
         STC   RE,ELCDHI                                                        
         LA    R6,BDELEM                                                        
CPER120  BRAS  RE,NEXTEL                                                        
         BNE   CPER130                                                          
         BRAS  RE,TESTPD                                                        
         JNE   NEQXIT                                                           
         MVI   ERRCD,MADEGOOD                                                   
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   *+12                                                             
         TM    6(R6),X'02'                                                      
         JO    NEQXIT                                                           
         B     CPER120                                                          
*                                                                               
CPER130  MVC   ELCDLO(2),HALF      SET TO SEARCH FOR REGELS ONLY                
         EJECT                                                                  
* DELETE ALL REGELS NOT IN NEW PERIOD                                           
         SPACE 1                                                                
         CLI   EDTVAL,SDTEDT       TEST CANAD NTWK DAY/TIME OVERRIDE            
         BE    CPER310             YES - NEVER DELETE ELEMENT !                 
         LA    R6,BDELEM                                                        
*                                                                               
CPER140  BRAS  RE,NEXTEL                                                        
         BNE   CPER180                                                          
*                                                                               
CPER150  L     R5,AREC2                                                         
*                                                                               
CPER160  CLC   2(2,R6),0(R5)       EL PRIOR TO MONDAY                           
         BL    *+14                                                             
         CLC   2(2,R6),4(R5)        OR AFTER SUNDAY                             
         BNH   CPER140             EL IS IN WEEK - NEXT REGEL                   
         LA    R5,6(R5)                                                         
         OC    0(2,R5),0(R5)       TEST E-O-L                                   
         BNZ   CPER160                                                          
* CHECK OK TO DELETE REGEL                                                      
         BRAS  RE,TESTPD                                                        
         JNE   NEQXIT                                                           
         BRAS  RE,TESTMTCH                                                      
         JNE   NEQXIT                                                           
         MVI   ERRCD,MADEGOOD                                                   
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   *+12                                                             
         TM    6(R6),X'02'                                                      
         JO    NEQXIT                                                           
* DELETE ELEMENT                                                                
CPER170  DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)   DELETE THE ELEMENT                     
* TEST NEXT ELEM A MINUS OTO OR AFFID                                           
         CLC   0(1,R6),HALF2                                                    
         BNE   *+12                                                             
         TM    6(R6),X'80'                                                      
         BO    CPER170                                                          
         CLI   0(R6),X'10'         TEST AFFID                                   
         BL    *+12                                                             
         CLI   0(R6),X'1F'         DELETE ELEMS IN RANGE 10-1F                  
         BNH   CPER170                                                          
         BRAS  RE,NEXTEL2                                                       
         BE    CPER150                                                          
         EJECT                                                                  
* ADD REGELS FOR ALL WEEKS IN NEW PERIOD BUT NOT IN RECORD                      
* AND CHANGE ELEMENT DATES AS NEEDED                                            
         SPACE 1                                                                
CPER180  L     R5,AREC2            POINT TO WEEK LIST                           
         CLI   BDNOWK,0            IF NPW=0, WHY BOTHER                         
         BE    CPER300                                                          
*                                                                               
CPER190  MVC   ELCDLO(2),HALF      SET REGEL SRCH ARGS                          
         LA    R6,BDELEM                                                        
CPER200  BRAS  RE,NEXTEL                                                        
         BNE   CPER240             WEEK NOT IN REC - GO ADD                     
         CLC   2(2,R6),0(R5)       ELEM PRIOR TO MONDAY                         
         BL    CPER200             YES - SKIP                                   
         CLC   2(2,R6),4(R5)       ELEM AFTER SUNDAY                            
         BH    CPER200             YES - SKIP                                   
* ELEMENT IS IN WEEK                                                            
         MVC   ELEMDT,2(R6)        SAVE REGEL DATE                              
*                                                                               
CPER210  MVC   2(2,R6),2(R5)       SET NEW ELEMENT DATE                         
*                                                                               
CPER220  ZIC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO NEXT ELEM                           
         CLI   0(R6),0             TEST E-O-R                                   
         BE    CPER280             YES - NEXT WEEK                              
         CLI   0(R6),X'10'         TEST AFFID                                   
         BL    *+12                                                             
         CLI   0(R6),X'1F'         SKIP ELEMS IN RANGE 10-1F                    
         BNH   CPER220                                                          
         CLC   0(1,R6),ELCDLO      TEST REGEL                                   
         BNE   CPER230             NO                                           
         CLC   2(2,R6),4(R5)       TEST ELEMENT IN SAME WEEK                    
         BNH   CPER210             YES - CHANGE DATE                            
         B     CPER280             NO - DO NEXT WEEK                            
*                                                                               
CPER230  CLC   0(1,R6),HALF2       TEST OTO                                     
         BH    CPER280             NO                                           
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    CPER210             YES - CHANGE DATE                            
         B     CPER280             ELSE NO MORE REGELS THIS WEEK                
         EJECT                                                                  
* FIND INSERTION POINT                                                          
*                                                                               
CPER240  CLI   EDTVAL,SDTEDT       TEST CANAD NTWK DAY/TIME OVERRIDE            
         BE    CPER280             YES - NEVER ADD AN ELEMENT !                 
*                                                                               
         MVC   ELEM+2(2),2(R5)     SET ELEM DATE                                
*                                                                               
         IC    RE,ELCDLO                                                        
         LA    RE,2(RE)            EXPAND SRCH ARGS TO INCLUDE OTO'S            
         STC   RE,ELCDHI                                                        
         LA    R6,BDELEM                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CPER250                                                          
         CLC   ELEM+2(2),2(R6)                                                  
         BH    *-14                                                             
*                                                                               
CPER250  LA    R0,1                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    CPER270                                                          
         CLI   BUYKEY+3,X'FF'      NON-POL IS 1 ELEM/WEEK                       
         BNE   *+8                                                              
         IC    R0,BDNOWK           POL GETS 1 ELEM/SPOT                         
         B     CPER270                                                          
*                                                                               
CPER260  DS    0H                                                               
         CLI   0(R6),0             TEST E-O-R                                   
         BE    CPER270                                                          
         ZIC   RE,1(R6)            ADD NEXT EL AFTER THIS ONE                   
         AR    R6,RE                                                            
CPER270  GOTO1 TESTGLS,DMCB,ELEM                                                
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         BCT   R0,CPER260                                                       
*                                                                               
CPER280  LA    R5,6(R5)                                                         
         OC    0(2,R5),0(R5)       TEST E-O-L                                   
         BNZ   CPER190                                                          
*                                                                               
         CLC   BUDAY1IN,BUDAY1     TEST CHANGE OF START DAY                     
         JE    EQXIT               NO- DONE                                     
* WATCH OUT FOR NON-POL CATASTROPHES                                            
         CLI   BUYKEY+3,X'FF'                                                   
         BE    CPER300                                                          
* LOOK FOR -OTO'S THAT HAVE MORE THAN NPW SPOTS                                 
         MVC   ELCDLO(2),HALF2     SEARCH FOR OTO'S ONLY                        
         LA    R6,BDELEM                                                        
CPER290  BRAS  RE,NEXTEL                                                        
         BNE   CPER300                                                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    CPER290             NO                                           
         CLC   BDNOWK,7(R6)                                                     
         BNL   CPER290                                                          
         MVI   ERRCD,NEGSPOTS                                                   
         B     CPERERR                                                          
*                                                                               
CPER300  DS    0H                                                               
         EJECT                                                                  
*============================================================*                  
* DELETE ALL +OTO'S AND ASSOCIATED ELEMENTS                  *                  
* THEN RE-ADD TO INSURE PROPER DATE SEQUENCE                 *                  
*============================================================*                  
         SPACE 1                                                                
CPER310  MVC   DUB(4),AREC1                                                     
         MVC   DUB+4(4),AREC2                                                   
         GOTO1 MOVEREC             SAVE REC IN REC2                             
*                                                                               
         MVC   ELCDLO(2),HALF2     SET OTO SRCH ARGS                            
         LA    R6,BDELEM                                                        
CPER320  BRAS  RE,NEXTEL                                                        
         BNE   CPER370                                                          
CPER330  TM    6(R6),X'80'         TEST MINUS OTO                               
         BO    CPER320             YES-SKIP                                     
CPER340  DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  DELETE THE ELEMENT                      
* REMEMBER R6 POINTS TO NEXT ELEMENT                                            
         CLC   0(1,R6),HALF2       TEST OTO                                     
         BE    CPER340             YES DELETE (WHETHER + OR -)                  
         CLI   0(R6),X'10'         TEST AFFID                                   
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   CPER340                                                          
         BRAS  RE,NEXTEL2           ELSE CONTINUE                               
         BE    CPER330              IF MORE ELEMS                               
         B     CPER370                                                          
         EJECT                                                                  
CPER350  DS    0H                                                               
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'0B'        SET SRCH ARGS                                
         MVI   ELCDHI,X'0C'                                                     
         LH    R2,BUEXPDAY                                                      
*                                                                               
CPER355  BRAS  RE,NEXTEL                                                        
         BNE   CPER310                                                          
         CLI   0(R6),X'0B'                                                      
         BE    CPER360                                                          
         TM    6(R6),X'80'         TEST -OTO                                    
         BNO   CPER355             NO  - SKIP                                   
*                                                                               
CPER360  GOTO1 VDATCON,DMCB,(2,2(R6)),(0,WORK)    GET EBCDIC START              
         GOTO1 VADDAY,(R1),WORK,WORK+6,(R2)                                     
         GOTO1 VDATCON,(R1),(0,WORK+6),(2,2(R6))  SET COMPRESSED DATE           
         B     CPER355                                                          
         EJECT                                                                  
* NOW RE-ADD + OTOS AND OTHER DATA                                              
*                                                                               
CPER370  L     R6,AREC2                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CPER380  MVC   ELCDLO(2),HALF2     SET OTO SRCH ARGS                            
         BRAS  RE,NEXTEL                                                        
         JNE   EQXIT                                                            
         TM    6(R6),X'80'         TEST -OTO                                    
         BO    CPER380             YES - SKIP                                   
* FIND INSERTION POINT                                                          
         LR    R7,R6               SAVE OTO ADDRESS                             
         ST    R6,WORK             SAVE IT HERE TOO                             
         MVC   ELCDLO(2),HALF      SET REGEL SRCH ARGS                          
         LA    R6,BDELEM                                                        
CPER390  BRAS  RE,NEXTEL           SEARCH FOR REGELS                            
         BNE   CPER400                                                          
         CLC   2(2,R6),2(R7)       REGEL DATE TO OTO DATE                       
         BNH   CPER390             LOW OR EQUAL - CONTINUE                      
* ADD ELEMENT                                                                   
CPER400  GOTO1 VRECUP,DMCB,BUYREC,(R7),(R6)                                     
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               SET NEXT INSERT ADDRESS                      
* NOW CHECK FOR MORE ELEMS AFTER OTO                                            
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'         TEST AFFID                                   
         BL    *+12                                                             
         CLI   0(R7),X'1F'                                                      
         BNH   CPER400                                                          
         CLC   0(1,R7),HALF2       TEST OTO                                     
         BNE   *+12                NO - SEARCH FOR MORE                         
         TM    6(R7),X'80'         ELSE TEST MINUS OTO                          
         BO    CPER400             YES - ADD IT                                 
         L     R6,WORK             RESTORE OTO ADDR IN REC2                     
         B     CPER380             AND GET NEXT ELEMENT                         
*                                                                               
CPERERR  DS    0H                                                               
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
CHGCOST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BUWHY,X'40'                                                      
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    CCOS1                                                            
* CANADIAN NETWORK                                                              
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),BUCOST    MOVE NETWORK BUY COST                        
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BE    *+10                                                             
         MVC   FULL(4),7(R6)       MOVE EXPLODED COST FROM 68 ELEM              
*                                                                               
         MVC   BDCOST,FULL+1                                                    
         MVC   BDCIND,BUCIND       AND SET RATE TYPE                            
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
         CLI   FULL,0              DOES COST FIT IN PENNIES                     
         JE    CCOS0X              YES                                          
         L     R1,FULL                                                          
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,7,BDCOST                                                      
         NI    BDCIND2,X'FF'-X'01'                                              
         OI    BDCIND2,X'10'       SET COST IN DOLLARS                          
*                                                                               
CCOS0X   CLI   BUEXPKEY,0          EXPLODED BUY?                                
         JNE   EQXIT               YES - DONE                                   
*                                                                               
CCOS1    CLI   EDTVAL,COS2EDT      IS THIS JUST COST2 EDIT                      
         BNE   CCOS1X              NO                                           
         L     RE,=A(SVB0PROF-BUYSAVE)                                          
         AR    RE,RA                                                            
         CLI   6(RE),C'N'       TEST CHANGE NOT ALLOWED IF PAID/MATCHED         
         BNE   CCOS4                                                            
*                                                                               
CCOS1X   MVI   ELCDLO,6                                                         
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CCOS2    BRAS  RE,NEXTEL                                                        
         BNE   CCOS4                                                            
         BRAS  RE,TESTPD                                                        
         JNE   NEQXIT                                                           
         BRAS  RE,TESTMTCH                                                      
         JNE   NEQXIT                                                           
         B     CCOS2                                                            
                                                                                
* WORK+0   COST FLAG             X01=MINUS + RATE TYPE                          
* WORK+1   COST FLAG2            X10=DOLLARS, X01=PENNIES IF CANNET             
* WORK+2   COST2 FLAG                                                           
* WORK+3   COST2 IN DOLLARS FLAG                                                
*                                                                               
* WORK+4   BUYREC COST IN PENNIES                                               
* WORK+8   COST2 IN PENNIES                                                     
* WORK+12  C'Y' IF COST INPUT THIS TIME                                         
* WORK+13  C'Y' IF COST2 INPUT THIS TIME                                        
* WORK+14  C'Y' IF COST2 IS A FACTOR, NOT DOLLARS                               
                                                                                
*                                                                               
CCOS4    OC    SVNDEF(16),SVNDEF   CANADIAN NETWORK?                            
         BNZ   CCOS20              YES - GO ALLOCATE COSTS                      
         XC    WORK,WORK           BUILD A TABLE OF VALUES                      
         MVC   WORK+0(1),BDCIND                                                 
         MVC   WORK+1(1),BDCIND2                                                
         MVC   WORK+5(3),BDCOST                                                 
*                                                                               
         OC    WORK+5(3),WORK+5    TEST COST IS 0                               
         BNZ   *+8                                                              
         NI    WORK+0,X'FF'-X'01'  TURN OFF -COST IND                           
*                                                                               
         MVI   WORK+12,C'N'        SET COST NOT INPUT                           
         MVI   WORK+13,C'N'        SET COST2 NOT INPUT                          
         MVI   WORK+14,C'N'        SET COST2 NOT A FACTOR                       
*                                                                               
         OC    SVCCOST2,SVCCOST2                                                
         BNZ   *+14                                                             
         OC    SVECOST2,SVECOST2                                                
         BZ    *+8                                                              
         MVI   WORK+14,C'Y'        SET COST2 IS A FACTOR                        
*                                                                               
         TM    BUCOS2IND,X'80'     TEST COST2 INPUT THIS TIME                   
         BZ    *+8                                                              
         MVI   WORK+13,C'Y'        SET COST2 INPUT THIS TIME                    
         CLI   EDTVAL,COS2EDT      TEST COST2 ONLY                              
         BE    CCOS6               YES                                          
*                                                                               
CCOS5    MVI   WORK+12,C'Y'        SET COST INPUT FLAG                          
         MVC   WORK+0(1),BUCIND    ELSE MOVE NEW COST VALUES                    
         MVC   WORK+1(1),BUCIND2                                                
         MVC   WORK+5(3),BUCOST                                                 
*                                                                               
CCOS6    LA    R6,BDELEM           SEE IF THERE IS A COST2 $ ELEM               
         MVI   ELCDLO,X'71'                                                     
         MVI   ELCDHI,X'71'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CCOS8                                                            
*                                                                               
         MVC   WORK+2(1),BDCIND                                                 
         MVC   WORK+3(1),BDCIND2                                                
         OI    WORK+3,X'40'        SET FLAG THERE IS A COST2                    
         MVC   WORK+8(4),2(R6)     MOVE BUYREC COST2                            
*                                                                               
CCOS8    CLI   WORK+13,C'Y'        TEST COST2 INPUT THIS TIME                   
         BE    CCOS8B              YES                                          
         CLI   WORK+14,C'Y'        NO - TEST IT'S A FACTOR                      
         BE    CCOS8B                                                           
         ICM   R0,15,SVCCOST2                                                   
         BNZ   *+12                                                             
         ICM   R0,15,SVECOST2                                                   
         BZ    CCOS8B              NO                                           
*                                                                               
CCOS8A   STCM  R0,15,BUCOST2                                                    
*                                                                               
         LA    R6,BDELEM           BUT LOOK FOR AN EXISTING VALUE               
         MVI   ELCDLO,X'73'                                                     
         MVI   ELCDHI,X'73'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   BUCOST2,2(R6)                                                    
         B     CCOS10                                                           
*                                                                               
CCOS8B   CLI   WORK+13,C'Y'        TEST COST2 INPUT THIS TIME                   
         BNE   CCOS10              NO                                           
*                                                                               
         CLI   BUCOST2,X'FF'       TEST 'DELETE COST2'                          
         BE    CCOS12                                                           
*                                                                               
         MVC   WORK+8(4),BUCOST2   MOVE NEW COST2 AMOUNT                        
         NI    WORK+2,X'FF'-X'01'  RESET - COST IND                             
         TM    BUCOS2IND,X'01'     TEST COST2 NEGATIVE                          
         BZ    *+8                                                              
         OI    WORK+2,X'01'                                                     
*                                                                               
         MVI   WORK+3,X'40'        SET FLAG FOR COST2 PRESENT                   
         TM    BUCOS2IND,X'10'     TEST COST2 IN DOLLARS                        
         BZ    *+8                                                              
         OI    WORK+3,X'10'        SET FLAG FOR COST2 IN DOLLARS                
*                                                                               
CCOS10   BRAS  RE,SETSCALE         SCALE COSTS/CHECK SIGNS                      
*                                                                               
         MVC   BDCOST(3),BUCOST    MOVE COST                                    
         MVC   BDCIND,BUCIND                                                    
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
CCOS12   BAS   RE,SETCOST2         YES - SET COST2                              
         J     EQXIT                                                            
         EJECT                                                                  
* MOVE REC TO REC3 SO CAN WRITE REC *                                           
                                                                                
CCOS20   MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CCOS22   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* REPLACE SHARE IN EACH 68 ELEM WITH DOLLARS                                    
                                                                                
CCOS24   ICM   R0,15,7(R6)          ALIGN                                       
         SR    R1,R1                                                            
         ICM   R1,7,BUCOST                                                      
         BZ    CCOS26                                                           
         AR    R1,R1               X 2                                          
         MR    R0,R0               X SHARE                                      
         MVC   FULL,=F'1000'                                                    
         TM    BUCIND2,X'01'       TEST COST IN PENNIES                         
         BZ    *+10                NO                                           
         MVC   FULL,=F'100000'     YES                                          
         D     R0,FULL                                                          
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
CCOS26   STCM  R1,15,7(R6)                                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    CCOS24                                                           
* TEST SUM = BUCOST                                                             
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         LR    R7,R6                                                            
         SR    R8,R8                                                            
         B     *+8                                                              
CCOS30   BRAS  RE,NEXTEL                                                        
         BNE   CCOS34                                                           
*                                                                               
         ICM   R1,15,7(R6)           GET COST IN PENNIES                        
         CLC   7(4,R6),=X'00FFFFFF'  TEST AMOUNT FITS IN 3 BYTES                
         BL    CCOS32                YES CONSIDER ADJUSTING                     
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                GIVES COST IN DOLLARS                        
         MHI   R1,100              NOW CONVERT BACK TO PENNIES                  
         AR    R8,R1               AND ADD TO TOTAL                             
         B     CCOS30              AND DO NOT CONSIDER ADJUSTING                
*                                                                               
CCOS32   ICM   R1,15,7(R6)                                                      
         AR    R8,R1                                                            
         CLC   7(4,R6),7(R7)                                                    
         BNH   *+6                 SAVE A(LARGEST AMT)                          
         LR    R7,R6                                                            
         B     CCOS30                                                           
*                                                                               
* ADJUST LARGEST AMOUNT NOT IN DOLLARS                                          
*                                                                               
CCOS34   L     R1,BUCOST                                                        
         SRL   R1,8                                                             
         TM    BUCIND2,X'01'       TEST COST IN PENNIES                         
         BO    *+8                 YES                                          
         M     R0,=F'100'          NO-GET PENNIES                               
         SR    R8,R1                                                            
         ICM   R0,15,7(R7)                                                      
         SR    R0,R8                                                            
         STCM  R0,15,7(R7)                                                      
         J     EQXIT                                                            
         EJECT                                                                  
*===============================================================                
* CHECK THAT COST AND COST2 HAVE SAME SIGNS                                     
* IF EITHER COST IS IN DOLLARS, SCALE BOTH TO DOLLARS                           
*===============================================================                
* WORK+0   COST FLAG             X01=MINUS + RATE TYPE                          
* WORK+1   COST FLAG2            X10=DOLLARS, X01=PENNIES IF CANNET             
* WORK+2   COST2 MINUS FLAG      X01                                            
* WORK+3   COST2 IN DOLLARS FLAG X10 + X40=COST2 PRESENT                        
*                                                                               
* WORK+4   BUYREC COST IN PENNIES                                               
* WORK+8   COST2 IN PENNIES                                                     
*===============================================================                
                                                                                
SETSCALE NTR1                                                                   
         CLI   WORK+14,C'Y'        TEST COST2 IS A FACTOR                       
         BE    SETSCL14                                                         
*                                                                               
         L     R0,WORK+4                                                        
         TM    WORK+1,X'10'        TEST COST IN $                               
         BZ    *+8                                                              
         MHI   R0,100              SCALE TO PENNIES                             
         ST    R0,WORK+4                                                        
         NI    WORK+1,X'FF'-X'10'                                               
*                                                                               
SETSCL2  TM    WORK+3,X'40'        COST2 PRESENT?                               
         BZ    SETSCL4                                                          
         TM    WORK+3,X'01'        TEST COST2 IN PENNIES                        
         BO    SETSCL4                                                          
         L     R0,WORK+8                                                        
         TM    WORK+3,X'10'        TEST COST2 IN $                              
         BZ    *+8                                                              
         MHI   R0,100              SCALE TO PENNIES                             
         ST    R0,WORK+8                                                        
         NI    WORK+3,X'FF'-X'10'                                               
                                                                                
* IF EITHER COST OR COST2 IS 0 OR IS A FACTOR, NO PROBLEM                       
                                                                                
SETSCL4  OC    WORK+4(4),WORK+4    TEST COST 0                                  
         BZ    SETSCL10                                                         
         OC    WORK+8(4),WORK+8                                                 
         BZ    SETSCL10                                                         
* BOTH VALUES NOT ZERO - SO BOTH MUST BE + OR BOTH BE -                         
         TM    WORK+0,X'01'        TEST COST IS MINUS                           
         BZ    SETSCL6             NO                                           
         TM    WORK+2,X'01'        YES, TEST COST2 IS MINUS ALSO                
         BO    SETSCL10            YES - ALL IS GOOD                            
         B     SETSCERR                                                         
*                                                                               
SETSCL6  TM    WORK+2,X'01'        COST POS, TEST COST2 IS MINUS                
         BO    SETSCERR                                                         
*                                                                               
SETSCL10 CLI   WORK+4,0            TEST COST FITS IN PENNIES                    
         BNE   SETSCL12            NO - SCALE TO DOLLARS                        
         CLI   WORK+8,0            TEST COST2 FITS IN PENNIES                   
         BE    SETSCL14            YES - ALL IS GOOD                            
*                                                                               
SETSCL12 SR    R0,R0                                                            
         L     R1,WORK+4                                                        
         D     R0,=F'100'                                                       
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BNZ   SETSCER2                                                         
         ST    R1,WORK+4           SET COST IN DOLLARS                          
         OI    WORK+1,X'10'                                                     
*                                                                               
         TM    WORK+3,X'40'        COST2 PRESENT?                               
         BZ    SETSCL14                                                         
         MVI   WORK+13,C'Y'        AND SET FLAG TO REBUILD ELEMENT              
         SR    R0,R0                                                            
         L     R1,WORK+8                                                        
         D     R0,=F'100'                                                       
         ST    R1,WORK+8           SET COST2 IN DOLLARS                         
         OI    WORK+3,X'10'                                                     
*                                                                               
SETSCL14 MVC   BUCOST,WORK+5                                                    
         MVC   BUCOST2,WORK+8                                                   
         MVC   BUCIND,WORK+0                                                    
         TM    WORK+2,X'01'        TEST COST 2 IS MINUS                         
         BZ    *+8                                                              
         OI    BUCIND,X'01'        THEN SET - COST FLAG                         
         MVC   BUCIND2,WORK+1                                                   
*                                                                               
SETSCLX  XIT1                                                                   
*                                                                               
SETSCERR MVC   NERRCD,=Y(COSTSIGN)                                              
         B     SETSCERX                                                         
*                                                                               
SETSCER2 MVC   NERRCD,=Y(LOSTCNTS)                                              
*                                                                               
SETSCERX MVI   ERRCD,NEWERRS                                                    
         J     BUYERRX                                                          
         EJECT                                                                  
SETCOST2 NTR1                                                                   
*                                                                               
         MVI   ELCDLO,X'73'                                                     
         CLI   WORK+14,C'Y'        TEST COST2 FACTOR                            
         BE    SETCOS2A                                                         
*                                                                               
         MVI   ELCDLO,X'71'                                                     
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BO    SETCOS2A            YES - ALWAYS ADD COST2 ELEMENT               
         CLI   WORK+13,C'Y'        WAS COST 2 INPUT                             
         JNE   EQXIT                                                            
*                                                                               
         TM    SVCOPT1,COP1COSQ    TEST COS2 DOLLARS REQD                       
         BO    SETCOS2A                                                         
         TM    SVCOPT3,COP3COSQ    TEST COS2 DOLLARS OR OPTIONAL                
         BO    SETCOS2A                                                         
         TM    SVCOPT4,COP4TRD     TEST TRADE                                   
         BO    SETCOS2A                                                         
*                                                                               
         CLI   WORK+13,C'Y'        TEST COST2 INPUT                             
         JNE   EQXIT               NO - IT SHOULDN'T BE                         
         MVI   ERRCD,BADKEYWD      ELSE ERROR                                   
         J     BUYERRX                                                          
*                                                                               
SETCOS2A MVC   ELEM(1),ELCDLO      SET CODE IN ELEM                             
*                                                                               
         MVI   ELCDLO,X'71'        ALWAYS DELETE 71 AND 73                      
         MVI   ELCDHI,X'73'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
SETCOS2B BRAS  RE,NEXTEL                                                        
         BNE   *+12                                                             
         BRAS  RE,DELEL                                                         
         B     SETCOS2B                                                         
*                                                                               
         CLI   BUCOST2,X'FF'       TEST 'DELETE COST 2'                         
         JE    EQXIT                                                            
*                                                                               
SETCOS2C MVI   ELEM+1,6                                                         
         MVC   ELEM+2(4),BUCOST2                                                
         BRAS  RE,ADDEL                                                         
*                                                                               
         TM    SVCOPT4,COP4MIDS    TEST MIDAS CLIENT                            
         BZ    SETCOS2X                                                         
         CLI   SVMIDAS,C'C'        TEST MIDAS STATION                           
         BE    *+12                                                             
         CLI   SVMIDAS,C'M'                                                     
         BNE   SETCOS2X                                                         
         OI    BDSTAT3,BDST3_COS2RT  SET COST2 RATE TYPE FLAG                   
SETCOS2X J     EQXIT                                                            
         EJECT                                                                  
*==============================================================*                
* MAKE SURE SPOT ELEMENTS ELEMS IN RECORD                                       
*                                                                               
FIXREC   NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC             SAVE REC IN REC2                             
*                                                                               
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                NO COMMENTS                                  
*                                                                               
FIXR2    BRAS  RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    FIXR2                                                            
* R6 POINTS TO E-O-R                                                            
         LR    R7,R6               SAVE E-O-R ADDRESS                           
         L     R6,AREC2                                                         
         LA    R6,24(R6)                                                        
*                                                                               
         SR    R0,R0                                                            
FIXR4    BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6),(R7)                                     
         IC    R0,1(R7)            ADD NEXT ELEM AFTER THIS ONE                 
         AR    R7,R0                                                            
         B     FIXR4                                                            
         LTORG                                                                  
         EJECT                                                                  
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         J     EXIT                                                             
*                                                                               
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         J     EXIT                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* TEST SPOT AT 0(R6) PAID *                                                     
         SPACE 1                                                                
TESTPD   MVI   ERRCD,BLLDPAID                                                   
         OC    4(2,R6),4(R6)                                                    
         BNZR  RE                                                               
*                                                                               
         MVI   ERRCD,DARMGPND                                                   
         TM    6(R6),X'10'         MAKEGOOD PENDING?                            
         BR    RE                  RETURN WITH CC SET                           
         SPACE 2                                                                
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
*                                                                               
TESTMTCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'MC',AGYALPHA                                                  
         BNE   TMTCH0                                                           
         CLC   BDEND,=X'660401'                                                 
         JL    EQXIT                                                            
*                                                                               
TMTCH0   MVI   ERRCD,MATCHED                                                    
         LR    R5,R6               SAVE EL ADDRESS                              
*                                                                               
TMTCH2   BRAS  RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
*                                                                               
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
*                                                                               
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         JE    NEQXIT              EXIT WITH CC NOT EQ IF MATCHED               
         ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         J     EQXIT                                                            
         EJECT                                                                  
*=================================================================*             
* MAKE SURE ALL +OTO'S PRECEDE ALL -OTO'S FOR EACH DATE IN BUYREC *             
* ON ENTRY BUYREC HAS RECORD TO BE FIXED                          *             
*=================================================================*             
         SPACE 1                                                                
FIXOTOS  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,7                                                         
         MVI   ELCDHI,7                                                         
*                                                                               
FIXOT2   BRAS  RE,NEXTEL           FIND FIRST OTO                               
         JNE   EXIT                                                             
*                                                                               
FIXOT4   TM    6(R6),X'80'         TEST -OTO                                    
         BZ    FIXOT2                                                           
         LR    R7,R6               SAVE FIRST -OTO ADDRESS                      
*                                                                               
FIXOT6   BRAS  RE,NEXTEL           FIND NEXT OTO                                
         JNE   EXIT                                                             
*                                                                               
         CLC   2(2,R6),2(R7)       TEST SAME DATE                               
         BNE   FIXOT4                                                           
*                                                                               
         TM    6(R6),X'80'         IS IT A -OTO                                 
         BO    FIXOT6              YES - KEEP GOING                             
* THIS IS A +OTO FOLLOWING A -OTO. SWAP THEM                                    
         XC    0(10,R6),0(R7)                                                   
         XC    0(10,R7),0(R6)                                                   
         XC    0(10,R6),0(R7)                                                   
         LR    R6,R7               POINT R6 BACK TO -OTO SLOT                   
         B     FIXOT2              R7 GETS RESET ON NEXT -OTO                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* BUILD WEEK LIST AT R5 FOR ST/END DATES AT R4.           *                     
* FORMAT IS MONDAY DATE/ELEMENT DATE/SUNDAY DATE          *                     
*=========================================================*                     
         SPACE 1                                                                
BLDWKS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(256,R5),0(R5)                                                  
         XC    256(256,R5),256(R5)                                              
         GOTO1 VDATCON,DMCB,(3,0(R4)),WORK+6      GET EBCDIC START              
         GOTO1 (RF),(R1),(3,3(R4)),WORK+18        GET EBCDIC END                
*                                                                               
* OUT-OF-WEEK DATA USES BUY START DATE DAY FOR WEEK STARTS                      
         GOTO1 (RF),(R1),(3,BDSTART),WORK                                       
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK ESTIMATE                    
         BNE   BLDWK8              YES                                          
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK+6,DUB  ELSE USE MONDAY WEEKS                   
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0               BACK UP TO MONDAY                            
         GOTO1 VADDAY,DMCB,WORK+6,WORK,(R0)                                     
*                                                                               
BLDWK8   LA    R0,6                ADVANCE TO SUNDAY                            
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         SPACE 1                                                                
* GET INTERVAL BETWEEN WEEKS *                                                  
         SPACE 1                                                                
         LA    R0,7                                                             
         CLI   BDWKIND,C'O'                                                     
         BE    BLDWK10                                                          
         LA    R0,14                                                            
         CLI   BDWKIND,C'A'                                                     
         BE    BLDWK10                                                          
         LA    R0,21                                                            
         CLI   BDWKIND,C'T'                                                     
         BE    BLDWK10                                                          
         LA    R0,28                                                            
         CLI   BDWKIND,C'F'                                                     
         BE    BLDWK10                                                          
         DC    H'0'                                                             
*                                                                               
BLDWK10  DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,(R5))    CNVRT MONDAY DATE                  
         GOTO1 (RF),(R1),WORK+6,(2,2(R5))    CNVRT ELEMENT DATE                 
*                                                                               
         GOTO1 (RF),(R1),WORK+12,(2,4(R5))    CNVRT SUNDAY DATE                 
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+24,(R0)           NEXT MONDAY              
         MVC   WORK(6),WORK+24                                                  
         GOTO1 (RF),(R1),WORK+6,WORK+24,(R0)           NEXT ELEM                
         MVC   WORK+6(6),WORK+24                                                
         GOTO1 (RF),(R1),WORK+12,WORK+24,(R0)          NEXT SUNDAY              
         MVC   WORK+12(6),WORK+24                                               
*                                                                               
         CLC   WORK(6),WORK+18     TEST PAST END                                
         BH    BLDWKX                                                           
         LA    R5,6(R5)                                                         
         B     BLDWK10                                                          
*                                                                               
BLDWKX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* ON CHANGE OF DAY OR TIME, SET LOOKUP REQD FLAG FOR COMSCORE DEMOS             
* THAT DO NOT HAVE OVERRIDES                                                    
*==================================================================             
                                                                                
SET50EL  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         LR    R7,R6               SAVE A(50EL)                                 
*                                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
*                                                                               
SET50EL2 LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                DIVIDE BY 8                                  
         LA    R1,24(R6)                                                        
*                                                                               
SET50EL4 CLI   2(R1),0             TEST COMSCORE DEMO                           
         JNE   SET50EL6            NO                                           
         TM    4(R1),X'80'         TEST HAS OVRD                                
         JO    SET50EL6                                                         
*                                                                               
         LLC   RE,1(R1)            GET INDEX                                    
         BCTR  RE,0                                                             
         MHI   RE,9                                                             
         LA    RE,2(RE,R7)         POINT TO DEMO IN 50 EL                       
         MVI   BYTE,X'FF'-X'80'    FLAG TO RESET IF ORIG LKUP REQD              
         CLI   0(R6),2                                                          
         JE    *+8                                                              
         MVI   BYTE,X'FF'-X'40'    FLAG TO RESET IF SPL LKUP REQD               
         NC    8(1,RE),BYTE        TURN OFF APPROPRIATE FLAG                    
*                                                                               
SET50EL6 LA    R1,8(R1)            NEXT DEMO                                    
         JCT   R0,SET50EL4                                                      
*                                                                               
         BRAS  RE,NEXTEL           NEXT DEMO ELEMENT                            
         JE    SET50EL2                                                         
         J     EXIT                                                             
                                                                                
         EJECT                                                                  
BRTAB    DS    0F                                                               
         DC    AL1(NPWEDT),AL3(CHGNPW)                                          
         DC    AL1(SKEDEDT),AL3(CHGNPW)                                         
         DC    AL1(TIMEDT),AL3(CHGTIM)                                          
         DC    AL1(SLNEDT),AL3(CHGSLN)                                          
         DC    AL1(COSTEDT),AL3(CHGCOST)                                        
         DC    AL1(COS2EDT),AL3(CHGCOST)                                        
         DC    AL1(TAXEDT),AL3(CHGTAX)                                          
         DC    AL1(REPEDT),AL3(CHGREP)                                          
         DC    AL1(CTYPEDT),AL3(CHGXRT)                                         
         DC    AL1(STYPEDT),AL3(CHGXRT)                                         
         DC    AL1(C58EDT),AL3(CHGXRT)                                          
         DC    AL1(XRTEDT),AL3(CHGXRT)                                          
         DC    AL1(SPCTEDT),AL3(CHGSPCT)                                        
         DC    AL1(SDTEDT),AL3(CHGSDT)                                          
BRTABX   EQU   *-1                                                              
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042SPBUY12   11/21/19'                                      
         END                                                                    
