*          DATA SET SPINF45    AT LEVEL 005 AS OF 05/01/02                      
         TITLE 'MARKET RECORD INFO  T21A45'                                     
*PHASE T21A45A                                                                  
T21A45   CSECT                                                                  
         PRINT NOGEN                                                            
LINLEN   EQU   88                                                               
         NMOD1 55,T21A45                                                        
         LR    R9,RC                                                            
         USING DATATBL,R9                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
         USING MKTRECD,R6                                                       
         MVC   0(256,R9),DATAVALS                                               
         MVC   256(184,R9),DATAVALS+256                                         
*                                                                               
* FILTER ROUTINE                                                                
*                                                                               
FLTRTN00 GOTO1 USER1,DMCB,(64,SINIFLT),(3,=C'TZ=')                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   FLTRTN05                                                         
         MVI   WRKBT,C'0'          NO TIMEZONE, SET BYTE TO ZER0.               
         B     FLTRTN10                                                         
FLTRTN05 MVI   WRKBT,C'1'                                                       
         L     RF,4(R1)                                                         
         CLI   3(RF),C'1'         TIMEZONE N(1-4) TEST                          
         BL    INPUTERR                "                                        
         CLI   3(RF),C'4'              "                                        
         BH    INPUTERR                "                                        
         MVC   TMZNWRK,3(RF)      TIMEZONE CODE TO TMZNWRK                      
FLTRTN10 GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'SWP=')                             
         OC    4(4,R1),4(R1)       IS THERE A SWEEP CLASS                       
         BNZ   FLTRTN15                                                         
         MVI   WRKBT2,C'0'         NO SWEEPCLASS, SET WRKBT2 TO ZERO            
         CLI   WRKBT,C'0'          IF TIMEZONE ALSO EQUALS ZERO THEN            
         BE    DATA00              GO TO DATA PROC.                             
         B     FLTRTN20            NO SWEEP, TIMEZONE ON, DO FLTRTN             
FLTRTN15 MVI   WRKBT2,C'1'         YES, SWEEPCLASS, SO SET BYTE ON.             
         L     RF,4(R1)                                                         
         CLI   4(RF),C'1'         SWEEPCLASS N(1-8) TEST                        
         BL    INPUTERR                 "             "                         
         CLI   4(RF),C'8'               "             "                         
         BH    INPUTERR                 "             "                         
         MVC   SWPWRK,4(RF)       SWEEPCLASS TO SWPWRK                          
*                                                                               
* READ RECORD,CHECK FILTERS, DISPLAY                                            
*                                                                               
FLTRTN20 LA    R2,SINHDRH                                                       
         MVC   9(14,R2),=C'MKTCODE / NAME'                                      
         MVC   45(14,R2),=C'MKTCODE / NAME'                                     
         BAS   RE,DASHRTN                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(6),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES.                                         
         MVC   KEY,PREVKEY         NO.                                          
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FLTRTN40                                                         
FLTRTN30 CLI   WRKBT,C'1'          IS TIMEZONE FILTER ON                        
         BNE   *+14                NO.SKIP TIMEZONE TEST.                       
         CLC   MKTZONE,TMZNWRK     TIMEZONE MATCH                               
         BNE   FLTRTN40            NO.                                          
         CLI   WRKBT2,C'1'         IS SWEEPCLASS FILTER ON                      
         BNE   *+14                NO.SKIP SWEEPCLASS TEST.                     
         CLC   MKTCLASS,SWPWRK     SWEEPCLASS MATCH                             
         BNE   FLTRTN40            NO.                                          
         MVC   12(4,R2),MKTKMKT    MARKET CODE                                  
         MVC   19(24,R2),MKTNAME   MARKET NAME                                  
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FLTRTN40 BAS   RE,SEQSTA                                                        
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FLTRTN40                                                         
         CLI   0(R2),LINLEN        END OF COLUMN                                
         BE    FLTRTN30            NO.                                          
         LA    R2,SINHDRH          YES.SO RESET R2 TO                           
         ZIC   R0,0(R2)            POINT TO                                     
         AR    R2,R0               TOP OF SCREEN.                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               EXTRA ZIC FOR UNDERLIN                       
FLTRTN50 CLI   WRKBT,C'1'          IS TIMEZONE FILTER ON                        
         BNE   *+14                 NO.                                         
         CLC   MKTZONE,TMZNWRK      YES. - TIMEZONE MATCH                       
         BNE   FLTRTN60             NO.                                         
         CLI   WRKBT2,C'1'          SWEEPCLASS FILTER ON                        
         BNE   *+14                 NO.                                         
         CLC   MKTCLASS,SWPWRK      YES. - SWEEPCLASS MATCH                     
         BNE   FLTRTN60             NO.                                         
         MVC   47(4,R2),MKTKMKT                                                 
         MVC   55(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FLTRTN60 BAS   RE,SEQSTA                                                        
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FLTRTN60                                                         
         CLI   0(R2),LINLEN        END OF SCREEN                                
         BE    FLTRTN50            NO.                                          
         B     PAGEPROC            YES.                                         
*                                                                               
* DATA ROUTINES *                                                               
*                                                                               
DATA00   GOTO1 USER1,DMCB,(64,SINIFLT),(5,=C'DATA=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    FMT00               NO DATA REQUESTED, GOTO DEFAULT RTN.         
         SPACE                                                                  
* LOAD DATATBL WITH DSECT ADDRS, SET TEST BYTES TO ZERO *                       
         SPACE                                                                  
         LA    R4,DATATBL                                                       
         LA    R7,MKTZONE          ADDRS OF DSECT ITEM -                        
         STCM  R7,15,26(R4)        TO RESPECTIVE AREA OF DATATBL                
         MVI   4(R4),C'0'          SET TEST BYTE TO ZERO                        
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTCLASS                                                      
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTRANK                                                       
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTHOMES                                                      
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTNTA                                                        
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTWT                                                         
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         LA    R7,MKTSHR                                                        
         STCM  R7,15,26(R4)                                                     
         MVI   4(R4),C'0'                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   4(R4),C'0'          NSI TEST BYTE TO ZERO                        
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   4(R4),C'0'          CSI TEST BYTE                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   4(R4),C'0'          ARB TEST BYTE                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   4(R4),C'0'          BBM TEST BYTE                                
*                                                                               
* TEST FOR DATA NAME OF DATATBL *                                               
*                                                                               
         LA    R4,DATATBL          RESET R4 TO DATATBL                          
         LA    R2,SINHDRH          R2 TO HEADER                                 
         MVC   9(14,R2),=C'MKTCODE / NAME'                                      
         LR    RF,R2                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVC   9(14,RF),=14C'-'                                                 
         L     R3,4(R1)            R3 TO 'DATA=' OF USER1                       
         A     R3,=F'5'            R3 TO INPUT AFTER 'DATA='                    
         LA    R8,45               DISP OF 1ST HEADER INTO R8                   
DATA10   ZIC   R5,0(R4)            R5 HAS LENGTH OF TESTDATA                    
         GOTO1 USER1,DMCB,(64,(R3)),((R5),5(R4))                                
         C     R3,4(R1)            USER1 HAS STAYED AT ADDRS OF                 
         BE    DATAMTCH            LIST ITEM, SO A MATCH.                       
         CLI   0(R4),X'FF'         END OF DATATBL                               
         BE    INPUTERR            GOTO INPUTERR                                
         ZIC   R0,1(R4)            INCREMENT DATATBL                            
         AR    R4,R0                   "       "                                
         B     DATA10                                                           
DATAMTCH CH    R8,=H'75'           TEST FOR SCREEN HEADER SPACE                 
         BH    DATA20                                                           
         MVI   4(R4),C'1'          TURN ON TEST BYTE                            
         ZIC   R7,3(R4)            ADD DISP OF ITEM TO-                         
         AR    R7,R8               DISP OF HEADER TO-                           
         STC   R7,3(R4)            GIVE SCREEN DISP OF ITEM.                    
         ZIC   R7,2(R4)            L' DATA HEADER TO R7                         
         LA    RE,0(R8,R2)         R2(ADDRS)+R8(DISP) TO RE                     
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),30(R4)      HEADER TO SCREEN                             
         LR    RE,R2               UNDERLINE HEADER                             
         ZIC   R0,0(RE)               "        "                                
         AR    RE,R0                  "        "                                
         LA    RF,0(R8,RE)         ADDRS+DISP TO RF                             
         BCTR  R7,0                REDUCE HEADER DASHES-                        
         BCTR  R7,0                BY 2                                         
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),=14C'-'     MOVE UNDERLINE TO HEADER                     
         A     R7,=F'3'            DISP OF NEXT-                                
         AR    R8,R7               HEADER TO R8                                 
         A     R5,=F'1'            INCREMENT "DATA=" BY                         
         AR    R3,R5               L'TESTDATA + 1 FOR COMMA                     
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
* GET RECORD,MATCH WITH DATA REQUESTS,PROCESS  *                                
         SPACE                                                                  
PROC00   XC    KEY,KEY                                                          
         MVC   KEY(6),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES.                                         
         MVC   KEY,PREVKEY         NO.                                          
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   PROC45                                                           
         ZIC   R0,0(R2)            INCREMENT R2 LINE IF HISTA                   
         AR    R2,R0               FINDS RECORD TO DISPLAY                      
PROC10   LA    R4,DATATBL                                                       
PROC20   CLI   4(R4),C'1'          IS TEST BYTE ON                              
         BE    PROC30              YES.                                         
PROC25   ZIC   R0,1(R4)            NO. SO INCREMENT DATATBL.                    
         AR    R4,R0                "         "                                 
         CLI   0(R4),X'FF'         END OF DATATBL                               
         BNE   PROC20              NO.                                          
         B     PROC40              YES.                                         
PROC30   CLC   5(3,R4),=C'NSI'     RATING SERVICE REQUESTS                      
         BE    PROC50                                                           
         CLC   5(3,R4),=C'CSI'                                                  
         BE    PROC50                                                           
         CLC   5(3,R4),=C'BBM'                                                  
         BE    PROC60                                                           
         CLC   5(3,R4),=C'ARB'                                                  
         BE    PROC60                                                           
         CLC   5(3,R4),=C'HOMES'                                                
         BE    PROC70                                                           
         CLC   5(3,R4),=C'RANK'                                                 
         BE    PROC72                                                           
         CLC   5(3,R4),=C'NTA'                                                  
         BE    PROC74                                                           
         CLC   5(3,R4),=C'WEIGHT'                                               
         BE    PROC76                                                           
         CLC   5(3,R4),=C'SHARE'                                                
         BE    PROC78                                                           
PROC33   ZIC   RE,3(R4)            SCREEN DISP TO RE                            
         LA    R7,0(RE,R2)         R2(ADDRS)+RE(DISP) TO R7                     
         L     RE,26(R4)           ADDRS OF DSECT ITEM TO RE                    
         ZIC   RF,25(R4)           LENGTH OF DSECT ITEM TO RF                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(RE)       DSECT ITEM TO SCREEN                         
PROC35   ZIC   R0,1(R4)            INCREMENT DATATBL                            
         AR    R4,R0                   "       "                                
         CLI   0(R4),X'FF'         END OF DATATBL                               
         BNE   PROC20              NO.                                          
PROC40   MVC   11(4,R2),MKTKMKT    YES.                                         
         MVC   19(24,R2),MKTNAME   "                                            
         FOUT  (R2)                "                                            
PROC45   BAS   RE,SEQSTA                                                        
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   PROC45                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN      END OF SCREEN                                  
         BNE   PAGEPROC          YES.                                           
         B     PROC10            NO.                                            
*                                                                               
* SPECIAL PROCESSING FOR RATING SERVICE REQUESTS *                              
*                                                                               
PROC50   CLI   MKTRS1,C'0'                                                      
         BNE   PROC50B                                                          
PROC50A  ZIC   RE,3(R4)           DISP TO RE                                    
         LA    R7,0(RE,R2)        ADDRS + DISP TO R7                            
         EDIT  (B2,MKTRSM1),(5,EDWRK)   EDIT,MOVE TO SCREEN                     
         MVC   0(5,R7),EDWRK                                                    
         B     PROC35             RETURN TO GENERAL PROCESSING                  
PROC50B  CLI   MKTRS2,C'0'                                                      
         BNE   PROC35             NO MATCH,MOVE NOTHING,RETURN                  
PROC50C  ZIC   RE,3(R4)                                                         
         LA    R7,0(RE,R2)                                                      
         EDIT  (B2,MKTRSM2),(5,EDWRK)    EDIT,MOVE TO SCREEN                    
         MVC   0(5,R7),EDWRK                                                    
         B     PROC35             RETURN TO GENERAL PROCESSING                  
PROC60   CLI   MKTRS1,C'1'                                                      
         BE    PROC50A                                                          
         CLI   MKTRS2,C'1'                                                      
         BE    PROC50C                                                          
         B     PROC35             NO MATCH,MOVE NOTHING,RETURN                  
*                                                                               
* EDIT ROUTINE / NUMB IS EDITED INTO ITS OWN                                    
* FIELD / PROG RETURNS TO NORMAL PROCESSING                                     
*                                                                               
PROC70   EDIT  (C8,MKTHOMES),(8,MKTHOMES)                                       
         B     PROC33                                                           
PROC72   EDIT  (C3,MKTRANK),(3,MKTRANK)                                         
         B     PROC33                                                           
PROC74   EDIT  (C2,MKTNTA),(2,MKTNTA)                                           
         B     PROC33                                                           
PROC76   EDIT  (C4,MKTWT),(5,EDWRK),2       SPECIAL EDITING SINCE               
         B     PROC80                       DECIMAL POINT MAKES                 
PROC78   EDIT  (C4,MKTSHR),(5,EDWRK),2      FIELD TOO LARGE TO                  
PROC80   ZIC   RE,3(R4)                     EDIT INTO ITSELF.                   
         LA    R7,0(RE,R2)                                                      
         MVC   0(5,R7),EDWRK                                                    
         B     PROC35                                                           
*                                                                               
* DEFAULT ROUTINES *                                                            
*                                                                               
FMT00    CLC   SINIFLT,SPACES      TEST FOR GARBAGE IN INPUT AREA               
         BE    FMT05                                                            
         CLC   SINIFLT,=64X'00'                                                 
         BE    FMT05                                                            
         B     INPUTERR                                                         
FMT05    LA    R2,SINHDRH                                                       
         MVC   9(14,R2),=C'MKTCODE / NAME'                                      
         MVC   45(14,R2),=C'MKTCODE / NAME'                                     
         BAS   RE,DASHRTN                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(6),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES                                          
         MVC   KEY,PREVKEY         NO                                           
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FMT15                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
FMT10    MVC   11(4,R2),MKTKMKT                                                 
         MVC   19(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
FMT15    BAS   RE,SEQSTA                                                        
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FMT15                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN        END OF COLUMN                                
         BE    FMT10               NO.                                          
         LA    R2,SINHDRH          YES.                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               AN EXTRA ZIC FOR UNDERLIN                    
FMT20    MVC   47(4,R2),MKTKMKT                                                 
         MVC   55(24,R2),MKTNAME                                                
         FOUT  (R2)                                                             
FMT25    BAS   RE,SEQSTA                                                        
         CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   MKTKAGY,AGYALPHA                                                 
         BNE   FMT25                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN        END OF SCREEN                                
         BE    FMT20               NO.                                          
         B     PAGEPROC            YES.                                         
*                                                                               
HISTA    NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATI',KEY,AREC                      
         B     EXIT10                                                           
*                                                                               
SEQSTA   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'STATI',KEY,AREC                      
         B     EXIT10                                                           
*                                                                               
DASHRTN  LR    RF,R2                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVC   9(14,RF),=14C'-'                                                 
         MVC   45(14,RF),=14C'-'                                                
         FOUT  (R2)                FOUT MKTCODE/NAME                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2)                FOUT UNDERLINING                             
         BR    RE                                                               
*                                                                               
INPUTERR LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
         GOTO1 ERROR                                                            
*                                                                               
PAGEPROC MVC   PREVKEY,0(R6)                                                    
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         B     EXIT10                                                           
*                                                                               
EXIT     XC    PREVKEY,PREVKEY                                                  
         LA    R2,SINIKEYH                                                      
         OI    6(R2),X'C0'                                                      
EXIT10   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
DATAVALS DS    0CL424              * DATATBL RECORDS  *                         
         DC    X'02'               LENGTH OF TEST DATANAME                      
         DC    X'29'               LENGTH OF FIELD                              
         DC    X'0B'               LENGTH OF NAME TO BE MOVED                   
         DC    X'05'               DISP(LACEMENT) LENGTH                        
         DS    CL1                 TEST-BYTE                                    
         DC    CL20'TZ'            TEST DATANAME,'DATA='                        
         DC    X'01'               LENGTH OF DSECT ITEM                         
         DS    CL4                 ADDRS OF DSECT ITEM                          
         DC    CL11'TIME ZONE  '   HEADER                                       
         DC    X'03',X'2B',X'0D',X'06'                                          
         DS    CL1                                                              
         DC    CL20'SWP'                                                        
         DC    X'01'                                                            
         DS    CL4                                                              
         DC    C'SWEEP CLASS  '                                                 
         DC    X'04',X'24',X'06',X'01'                                          
         DS    CL1                                                              
         DC    CL20'RANK'                                                       
         DC    X'03'                                                            
         DS    CL4                                                              
         DC    C'RANK  '                                                        
         DC    X'05',X'28',X'0A',X'00'                                          
         DS    CL1                                                              
         DC    CL20'HOMES'                                                      
         DC    X'08'                                                            
         DS    CL4                                                              
         DC    C'  HOMES   '                                                    
         DC    X'03',X'23',X'05',X'01'                                          
         DS    CL1                                                              
         DC    CL20'NTA'                                                        
         DC    X'02'                                                            
         DS    CL4                                                              
         DC    C'NTA  '                                                         
         DC    X'06',X'26',X'08',X'00'                                          
         DS    CL1                                                              
         DC    CL20'WEIGHT'                                                     
         DS    CL5                      SPECIAL EDIT,NOT USED                   
         DC    C'WEIGHT  '                                                      
         DC    X'05',X'25',X'07',X'00'                                          
         DS    CL1                                                              
         DC    CL20'SHARE'                                                      
         DS    CL5                      SPECIAL EDIT, NOT USED                  
         DC    C'SHARE  '                                                       
         DC    X'03',X'25',X'07',X'00'                                          
         DS    CL1                                                              
         DC    CL20'NSI'                                                        
         DS    CL5                   NOT USED FOR RSM RECORDS                   
         DC    C' RSM   '                                                       
         DC    X'03',X'25',X'07',X'00'                                          
         DS    CL1                                                              
         DC    CL20'CSI'                                                        
         DS    CL5                   NOT USED FOR RSM RECORDS                   
         DC    C' RSM   '                                                       
         DC    X'03',X'25',X'07',X'00'                                          
         DS    CL1                                                              
         DC    CL20'ARB'                                                        
         DS    CL5                   NOT USED FOR RSM RECORDS                   
         DC    C' RSM   '                                                       
         DC    X'03',X'25',X'07',X'00'                                          
         DS    CL1                                                              
         DC    CL20'BBM'                                                        
         DS    CL5                   NOT USED FOR RSM RECORDS                   
         DC    C' RSM   '                                                       
         DC    X'FF'                                                            
*                                                                               
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL1                                                              
DATATBL  DSECT                                                                  
         DS    CL420                                                            
WRKBT    DS    CL1                                                              
WRKBT2   DS    CL1                                                              
TMZNWRK  DS    CL1                                                              
SWPWRK   DS    CL1                                                              
EDWRK    DS    CL5                                                              
         DS    CL11                                                             
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPINF45   05/01/02'                                      
         END                                                                    
