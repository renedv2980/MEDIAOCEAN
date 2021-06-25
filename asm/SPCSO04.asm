*          DATA SET SPCSO04    AT LEVEL 045 AS OF 07/01/03                      
*PHASE T21804A,*                                                                
         TITLE 'T21804 - CHILD SPOT ALLOCATION RECAP'                           
T21804   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21804                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'04'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    RECMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'04'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE AND DISPLAY REQUESTS                
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   KEYCHANG,C'N'                                                    
*                                                                               
         LA    R2,RECMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    RECCLTH+4,X'DF'                                                  
         NI    RECSTAH+4,X'DF'                                                  
         NI    RECESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,RECCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    RECSTAH+4,X'DF'                                                  
         NI    RECESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,RECSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    RECESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,RECESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKESTX                                                           
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
VKESTX   GOTO1 CMPNTP                                                           
         GOTO1 CLRACC                                                           
         EJECT                                                                  
VKOPT    LA    R2,RECOPTH          VALIDATE OPTION FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEND                                                            
*                                                                               
         MVI   KEYCHANG,C'Y'                                                    
         MVI   COSTOPT,C'N'                                                     
         MVI   TRADEOPT,C'N'                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKOPTX                                                           
*                                                                               
         CLI   5(R2),4             IF OPTION COST REQUESTED                     
         BNE   VKOPT10                                                          
         CLC   8(4,R2),=C'COST'                                                 
         BNE   VKOPT10                                                          
         MVI   COSTOPT,C'Y'        THEN SET COST OPTION FLAG                    
         B     VKOPTX                                                           
*                                                                               
VKOPT10  CLC   5(R2),5             ELSE IF OPTION TRADE REQUESTED               
         BNE   VKOPT20                                                          
         CLC   8(5,R2),=C'TRADE'                                                
         BNE   VKOPT20                                                          
         MVI   TRADEOPT,C'Y'       THEN SET OPTION TRADE                        
         B     VKOPTX                                                           
*                                  ELSE IF OPTION TRADE AND COST REQ            
VKOPT20  CLC   8(10,R2),=C'TRADE,COST'                                          
         BE    VKOPT30                                                          
         CLC   8(10,R2),=C'COST,TRADE'                                          
         BNE   INVERR                                                           
*                                                                               
VKOPT30  MVI   COSTOPT,C'Y'        THEN SET BOTH OPTIONS                        
         MVI   TRADEOPT,C'Y'                                                    
         B     VKOPTX                                                           
*                                                                               
VKOPTX   OI    4(R2),X'20'                                                      
*                                                                               
VKEND    CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED BUILD REFTAB              
         BNE   VKQUA                                                            
         EJECT                                                                  
VKREF    LA    R4,KEY              READ RECORDS AND BUILD REFERNCE TAB          
         USING CSOKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         LA    R5,REFTAB                                                        
         USING REFTABD,R5                                                       
         XC    DOLTOTAL,DOLTOTAL                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SR    R8,R8               COUNT REFERENCE NUMBERS                      
*                                                                               
VKREF10  CLC   KEY(11),KEYSAVE     LOOP THROUGH REFERENCE NUMBERS               
         BNE   VKREFX                                                           
         MVC   REFREF,CSOKREF      SAVE REFERENCE NUMBER                        
         LR    R1,R4                                                            
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   REFDISKA,0(R1)      DISK ADDRESS                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              R6 = A(FIRST WEEKLY ELEMENT)                 
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
VKREF12  CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   VKREF13                                                          
         OC    WKTSPOTS,WKTSPOTS   THEN SKIP RECS WITH NO TRADE SPOTS           
         BNZ   VKREF13                                                          
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE WEEKLY ELEMENTS         
         BE    VKREF12                                                          
         B     VKREF40                                                          
         DROP  R6                                                               
*                                                                               
VKREF13  L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ     USE                                          
         BAS   RE,GETEL                DESCRIPTION                              
         BE    *+6                          ELEMENT                             
         DC    H'0'                              FOR ...                        
         USING CSODSCEL,R6                                                      
         MVC   REFPROG,DSCPROG     PROGRAM NAME,                                
         MVC   REFDAY,DSCDAY           DAY CODE,                                
         MVC   REFTIME,DSCTIME         TIME CODE                                
*                                                                               
         CLI   TRADEOPT,C'Y'       IF NOT TRADE OPTION                          
         BE    *+12                                                             
         CLI   DSCWGT,0            THEN SKIP RECS WITH ZERO WEIGHT              
         BE    VKREF40                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,WKCODEQ      USE WEEKLY ELEMENTS FOR                      
         BAS   RE,GETEL                SPOTS,                                   
         BE    *+6                     DOLLARS,                                 
         DC    H'0'                    AND DATE/RATE TABLE                      
         USING CSOWKEL,R6                                                       
         LA    R2,REFCASH                                                       
         LA    R3,DOLTOTAL                                                      
         LA    R7,REFDRTAB                                                      
         USING DRTABD,R7                                                        
*                                                                               
VKREF15  MVC   DRDATE,WKDATE       MAKE DRTAB ENTRY                             
         MVC   DRRATE,WKCOST                                                    
         MVC   FULL,WKCOST                                                      
         LA    R7,DRTABL(R7)       BUMP TO NEXT ENTRY                           
*                                                                               
VKREF20  CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   *+14                                                             
         MVC   0(1,R2),WKTSPOTS+1  THEN MOVE IN TRADE SPOTS                     
         B     *+10                                                             
         MVC   0(1,R2),WKCSPOTS+1  ELSE MOVE IN CASH SPOTS                      
         LA    R2,1(R2)                                                         
*                                                                               
         SR    RF,RF               ADD DOLLARS                                  
*                                                                               
         CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   *+12                                                             
         ICM   RF,3,WKTSPOTS       THEN USE TRADE SPOTS                         
         B     *+8                                                              
         ICM   RF,3,WKCSPOTS       ELSE USE CASH SPOTS                          
*                                                                               
         ICM   R0,15,WKCOST        MULTIPLY BY COST                             
         MR    RE,R0                                                            
*                                                                               
         CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   *+16                                                             
         M     RE,NTPPERC          THEN APPLY NTP% TRADE VALUE                  
         A     RF,=F'5000'                                                      
         D     RE,=F'10000'                                                     
*                                                                               
         A     RF,0(R3)            ADD DOLLARS TO TABLE                         
         ST    RF,0(R3)                                                         
         LA    R3,4(R3)            BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT WEEK ELEMENT                    
         BNE   VKREF30                                                          
*                                                                               
         CLC   WKCOST,FULL         TEST CHANGE IN RATE                          
         BE    VKREF20                                                          
         B     VKREF15                                                          
         DROP  R6                                                               
*                                                                               
VKREF30  MVC   DRDATE,=X'FFFF'     SET LAST DATE TO INFINITY                    
         DROP  R7                                                               
*                                                                               
         LA    R5,REFTABL(R5)      BUMP TO NEXT REFTAB ENTRY                    
         LA    R8,1(R8)                                                         
*                                                                               
VKREF40  MVI   RDUPDATE,C'N'       READ NEXT KEY                                
         GOTO1 SEQ                                                              
         B     VKREF10                                                          
*                                                                               
VKREFX   MVI   0(R5),0             MARK END OF TABLE                            
         C     R8,=F'0'                                                         
         BE    ERRPNF              ERROR IF NO PROGRAMS OR SPOTS                
*                                                                               
         ST    R8,NUMREFS          SAVE NUMBER OF REFERENCE NUMBERS             
         SR    R0,R0                                                            
         LR    R1,R8               CALCULATE THE NUMBER OF SCREENS              
         BCTR  R1,0                    GENERATED BY ALL OF THE PROGRAMS         
         D     R0,=F'12'                                                        
         LA    R1,1(R1)                                                         
         ST    R1,NUMPROGS                                                      
*                                                                               
         MVC   PROGS,=F'1'         RESET PROGS TO ONE                           
         MVC   QUARTER,=F'1'       RESET QUARTER TO ONE                         
         MVC   STARTWK,=F'0'       RESET STARTING WEEK NUMBER                   
         EJECT                                                                  
VKQUA    LA    R2,RECQUAH          VALIDATE QUARTER FIELD                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         MVI   KEYCHANG,C'Y'                                                    
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         C     R1,=F'1'            MUST BE ONE TO FOUR                          
         BL    INVERR                                                           
         C     R1,=F'4'                                                         
         BH    INVERR                                                           
         ST    R1,QUARTER                                                       
         XC    RECQUA,RECQUA                                                    
         OI    RECQUAH+6,X'80'                                                  
*                                                                               
VKX      BAS   RE,CALCPTRS         CALCULATE POINTERS                           
*                                                                               
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BE    DR                  THEN SKIP VALREC                             
         B     VR                  ELSE GO TO VALREC                            
         EJECT                                                                  
VR       LA    R2,RECCASHH         VALIDATE CASH SPOT FIELDS                    
         MVI   REFCHANG,C'N'                                                    
         L     R5,REFPTR                                                        
*                                                                               
VR10     TM    4(R2),X'20'         TEST FIELD NEEDS TO BE VALIDATED             
         BNZ   VR12                                                             
         GOTO1 VALCASH                                                          
         MVI   REFCHANG,C'Y'       SET FLAG FOR REFERENCE CHANGED               
*                                                                               
VR12     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         LA    R3,RECTOTSH                                                      
         CR    R2,R3               TEST LAST FIELD                              
         BE    VR15                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT CASH FIELD                      
         AR    R2,R0                                                            
         LA    R5,REFTABL(R5)      BUMP TO NEXT REFTAB ENTRY                    
         CLI   0(R5),0             TEST END OF TABLE                            
         BE    VR15                                                             
         B     VR10                                                             
*                                                                               
VR15     CLI   REFCHANG,C'Y'       TEST REFERENCE CHANGED                       
         BE    DR                                                               
*                                                                               
VR20     CLI   PFKEY,0             TEST ENTER PRESSED                           
         BE    VR30                                                             
         CLI   PFKEY,6             TEST PF6 PRESSED                             
         BE    VR40                                                             
         CLI   PFKEY,7             TEST PF7 PRESSED                             
         BE    VR50                                                             
         CLI   PFKEY,8             TEST PF8 PRESSED                             
         BE    VR60                                                             
         B     INVPFK              ERROR INVALID PFKEY                          
*                                                                               
VR30     L     R1,QUARTER          GO TO NEXT QUARTER                           
         LA    R1,1(R1)                                                         
         C     R1,=F'4'                                                         
         BNH   *+8                                                              
         LA    R1,1                                                             
         ST    R1,QUARTER                                                       
         B     VR100                                                            
*                                                                               
VR40     L     R1,QUARTER          GO TO PREVIOUS QUARTER                       
         BCTR  R1,0                                                             
         C     R1,=F'0'                                                         
         BH    *+8                                                              
         LA    R1,4                                                             
         ST    R1,QUARTER                                                       
         B     VR100                                                            
*                                                                               
VR50     L     R1,PROGS            GO TO NEXT SET OF PROGRAMS                   
         LA    R1,1(R1)                                                         
         C     R1,NUMPROGS                                                      
         BNH   *+8                                                              
         LA    R1,1                                                             
         ST    R1,PROGS                                                         
         B     VR100                                                            
*                                                                               
VR60     L     R1,PROGS            GO TO PREVIOUS SET OF PROGRAMS               
         BCTR  R1,0                                                             
         C     R1,=F'0'                                                         
         BH    *+8                                                              
         L     R1,NUMPROGS                                                      
         ST    R1,PROGS                                                         
         B     VR100                                                            
*                                                                               
VR100    BAS   RE,CALCPTRS         CALCULATE NEW POINTERS AND DISPLAY           
         B     DR                                                               
         EJECT                                                                  
VALCASH  NTR1                                                                   
         GOTO1 VALSKD                                                           
         USING REFTABD,R5          DON'T READ RECORD IF SPOTS HAVE              
         L     R4,STARTWK              NOT CHANGED                              
         LA    R4,REFCASH(R4)      R4 = A(REFTAB SPOTS PER WEEK)                
         LA    R6,SPPW             R6 = A(VALIDATED SPOTS PER WEEK)             
         L     R3,NUMWEEKS                                                      
*                                                                               
VCSH10   CLI   0(R6),X'FF'         TEST IGNORE THIS WEEK                        
         BE    VCSH20                                                           
         CLC   0(1,R4),0(R6)       TEST SAME FOR THIS WEEK                      
         BNE   VCSH30                                                           
*                                                                               
VCSH20   LA    R4,1(R4)            BUMP TO NEXT WEEK                            
         LA    R6,1(R6)                                                         
         BCT   R3,VCSH10                                                        
         B     XIT                 EXIT IF NO WEEK HAS CHANGED                  
*                                  ELSE READ RECORD AND ADD CHANGES             
VCSH30   GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',REFDISKA,AIO,X        
               DMWORK                                                           
*                                                                               
VCSH40   L     R6,AIO              POINT EVERYTHING TO THIS WEEK                
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6          R6 = A(PROGRAM RECORD WEEK ELEMENT)          
         LA    R8,DOLTOTAL         R8 = A(TOTAL DOLLARS)                        
         LA    R4,REFCASH          R4 = A(CURRENT CASH SPOTS                    
         LA    R2,SPPW             R2 = A(NEW CASH SPOTS)                       
         OC    STARTWK,STARTWK                                                  
         BZ    VCSH60                                                           
         L     R3,STARTWK                                                       
*                                                                               
VCSH50   BAS   RE,NEXTEL           REPEAT UNTIL STARTING WEEK                   
         LA    R8,4(R8)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,VCSH50                                                        
*                                                                               
VCSH60   L     R3,NUMWEEKS         UPDATE ELEMENTS FOR THIS QUARTER             
*                                                                               
VCSH70   CLI   0(R2),X'FF'         TEST IGNORE THIS WEEK                        
         BE    VCSH80                                                           
         CLC   0(1,R4),0(R2)       TEST CHANGE THIS WEEK                        
         BE    VCSH80                                                           
*                                                                               
         CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   *+14                                                             
         MVC   WKTSPOTS+1(1),0(R2) THEN UPDATE TRADE SPOTS IN RECORD            
         B     *+10                                                             
         MVC   WKCSPOTS+1(1),0(R2) ELSE UPDATE CASH SPOTS IN RECORD             
*                                                                               
         ZIC   R1,0(R2)            R1 = DIFFERENCE IN COST BETWEEN OLD          
         ZIC   R0,0(R4)                NUMBER OF SPOTS AND NEW NUMBER           
         SR    R1,R0                   OF SPOTS                                 
         ICM   RF,15,WKCOST                                                     
         MR    R0,RF                                                            
*                                                                               
         CLI   TRADEOPT,C'Y'       IF TRADE OPTION                              
         BNE   *+16                                                             
         M     R0,NTPPERC          THEN APPLY NTP% TRADE VALUE                  
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
*                                                                               
         A     R1,0(R8)            UPDATE TOTAL DOLLARS                         
         ST    R1,0(R8)                                                         
*                                                                               
         MVC   0(1,R4),0(R2)       UPDATE REFTAB WITH NEW # OF SPOTS            
*                                                                               
VCSH80   BAS   RE,NEXTEL           BUMP TO NEXT WEEK                            
         LA    R8,4(R8)                                                         
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,VCSH70                                                        
*                                  WRITE BACK UPDATED RECORD                    
VCSH90   GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',REFDISKA,AIO,DMWORK           
         B     XIT                                                              
         EJECT                                                                  
VALSKD   NTR1                                                                   
         MVC   PRIORWKS,NUMWEEKS                                                
         MVC   SPPW,=16X'FF'       INITIALIZE TO IGNORE ALL SPOTS               
         LHI   RE,TRTAB-SYSSPARE                                                
         LA    RE,SYSSPARE(RE)                                                  
         XC    0(256,RE),0(RE)                                                  
         LA    RF,C'/'                                                          
         STC   RF,0(RF,RE)                                                      
         LA    RF,C'+'                                                          
         STC   RF,0(RF,RE)                                                      
         LA    RF,C'A'             SET UP TRANSLATION TABLE TO SEARCH           
         STC   RF,0(RF,RE)             FOR /,+,A,T, OR F                        
         LA    RF,C'T'                                                          
         STC   RF,0(RF,RE)                                                      
         LA    RF,C'F'                                                          
         STC   RF,0(RF,RE)                                                      
         LR    R4,R2                                                            
         TRT   8(41,R2),0(RE)      SEARCH FIELD DATA FOR SPECIAL CHAR           
         LR    R2,R4                                                            
         BZ    VSKD200             NO SHORT HAND SCHEDULE                       
         LA    RF,7(R2)                                                         
         L     RE,=F'-1'           SET UP BXH TO SCAN BACKWARDS                 
*                                                                               
VSKD10   BXH   R1,RE,*+8           SCAN BACK TO BEGINNING OF SHORT SCH          
         B     VSKD20                                                           
         CLI   0(R1),C' '                                                       
         BNH   VSKD20                                                           
         CLI   0(R1),C'0'                                                       
         BL    INVERR                                                           
         CLI   0(R1),C'9'                                                       
         BH    INVERR                                                           
         B     VSKD10                                                           
*                                                                               
VSKD20   LA    R4,1(R1)            R4 = A(BEGINNING OF SHORT SCHEDULE)          
         SR    R1,R2                                                            
         SH    R1,=H'6'                                                         
         SR    R0,R0                                                            
         D     R0,=F'3'            R1 = NUMBER OF WEEKS BEFORE THIS ONE         
         ST    R1,PRIORWKS                                                      
         LA    RF,SPPW(R1)         RF = A(SPOTS FOR THIS WEEK)                  
         L     R8,NUMWEEKS                                                      
         SR    R8,R1               R8 = WEEKS LEFT IN QUARTER                   
*                                                                               
VSKD30   CLI   0(R4),C'/'          TEST SKIP THIS WEEK                          
         BE    VSKD40                                                           
         CLI   0(R4),C'0'          MUST BE AT LEAST ONE DIGIT                   
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         SR    RE,RE                                                            
         CLI   1(R4),C'0'          TEST TWO DIGITS                              
         BL    *+16                                                             
         CLI   1(R4),C'9'                                                       
         BH    *+8                                                              
         LA    RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         PACK AND CONVERT NUMBER                      
         CVB   R0,DUB                                                           
         STC   R0,0(RF)                                                         
         LA    R4,1(R4,RE)         BUMP TO SPECIAL CHAR                         
         CLI   0(R4),C' '          TEST END OF SHORT SCHEDULE                   
         BNH   VSKD200                                                          
         CLI   0(R4),C'/'          TEST ANOTHER WEEK SEPARATOR                  
         BNE   VSKD100                                                          
*                                                                               
VSKD40   LA    R4,1(R4)            BUMP TO NEXT WEEK                            
         LA    RF,1(RF)                                                         
         BCT   R8,VSKD30                                                        
         CLI   0(R4),C' '          MUST BE END OF SHORT SCHEDULE                
         BH    INVERR                                                           
         B     VSKD200                                                          
*                                                                               
VSKD100  LA    R6,=C'FTA+'         SEARCH FOR FREQUENCY                         
         LA    R5,4                                                             
*                                                                               
VSKD110  CLC   0(1,R4),0(R6)                                                    
         BE    VSKD120                                                          
         LA    R6,1(R6)            BUMP TO NEXT FREQUENCY                       
         BCT   R5,VSKD110                                                       
         B     INVERR              MUST BE ONE OF THEM                          
*                                                                               
VSKD120  CR    R8,R5               TEST ENOUGH WEEKS LEFT                       
         BNH   VSKD200                                                          
         SR    R8,R5                                                            
         LA    RF,0(RF,R5)         BUMP TO NEXT WEEK                            
         STC   R0,0(RF)            SAVE VALUE THERE                             
         B     VSKD120                                                          
         EJECT                                                                  
VSKD200  OC    PRIORWKS,PRIORWKS   VALIDATE LONG SCHEDULE                       
         BZ    VSKDX                                                            
         L     R8,PRIORWKS         R8 = NUMBER OF WEEKS LEFT                    
         LA    R4,8(R2)            R4 = A(FIELD DATA)                           
         LA    RF,SPPW             RF = A(SPOTS PER WEEK)                       
*                                                                               
VSKD210  CLI   0(R4),C' '          TEST ONE DIGIT NUMBER                        
         BNH   VSKD220                                                          
         CLI   0(R4),C'0'          MUST BE TWO DIGITS                           
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         CLI   1(R4),C'0'                                                       
         BL    INVERR                                                           
         CLI   1(R4),C'9'                                                       
         BH    INVERR                                                           
         LA    RE,1                PACK TWO DIGITS                              
         B     VSKD230                                                          
*                                                                               
VSKD220  LA    R4,1(R4)            BUMP TO NEXT CHAR                            
         CLI   0(R4),C'.'          SKIP DOTS                                    
         BE    VSKD240                                                          
         CLI   0(R4),C'0'          MUST BE A DIGIT THEN                         
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         SR    RE,RE               PACK ONLY ONE DIGIT                          
*                                                                               
VSKD230  EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         PACK AND CONVERT NUMBER                      
         CVB   R0,DUB                                                           
         STC   R0,0(RF)                                                         
         LA    R4,1(R4,RE)         BUMP PAST NUMBER                             
         B     VSKD250                                                          
*                                                                               
VSKD240  LA    R4,1(R4)            BUMP PAST DOT                                
*                                                                               
VSKD250  BCT   R8,*+8              TEST NO MORE WEEKS                           
         B     VSKDX                                                            
         CLI   0(R4),C' '          MUST BE BLANK BETWEEN WEEKS                  
         BH    INVERR                                                           
         LA    R4,1(R4)            BUMP TO NEXT WEEK                            
         LA    RF,1(RF)                                                         
         B     VSKD210                                                          
*                                                                               
VSKDX    XIT1                                                                   
         EJECT                                                                  
* DISPLAY CALENDER HEADING *                                                    
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,RECL1H),RECLAST                                   
         GOTO1 CLEARF,DMCB,(1,RECL1H),RECLAST                                   
         LA    R7,RECL1H           POINT TO FIRST DISPLAY FIELD                 
         MVC   8(L'HEADMESS,R7),HEADMESS                                        
         CLI   COSTOPT,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8(L'HEADCOST,R7),HEADCOST                                        
         B     DR5                                                              
*                                                                               
HEADMESS DC    C'REF PROGRAM NAME  DAY     TIME'                                
HEADCOST DC    C'REF PROGRAM NAME  RATES       '                                
*                                                                               
DR5      LA    R2,38+8(R7)         R2 POINTS TO MONTH HEADINGS                  
         ZIC   R0,0(R7)                                                         
         AR    R7,R0                                                            
         LA    R3,38+8(R7)         R3 POINTS TO DATES                           
         L     R5,MONPTR           R5 POINTS TO MONTH NAMES                     
         L     R6,WPMPTR           R6 POINTS TO WEEKS PER MONTH TABLE           
*                                                                               
DR10     MVC   THISDATE,QSTART     SET DATE TO FIRST DATE OF QUARTER            
         OC    STARTWK,STARTWK                                                  
         BZ    DR30                                                             
         L     R4,STARTWK          BUMP THROUGH WEEKS BEFORE THIS QTR           
*                                                                               
DR20     GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         BCT   R4,DR20                                                          
*                                                                               
DR30     LA    R4,3                MAKE CALENDER FOR THREE MONTHS               
         XC    NUMWEEKS,NUMWEEKS                                                
*                                                                               
DR50     ZIC   R8,0(R6)            GET WEEKS PER MONTH                          
         LA    R6,1(R6)                                                         
         C     R8,=F'4'                                                         
         BNE   DR60                                                             
         MVC   0(11,R2),DASHES     MOVE IN FOUR WEEK HEADING                    
         MVC   4(3,R2),0(R5)                                                    
         LA    R2,12(R2)                                                        
         B     DR70                                                             
*                                                                               
DR60     MVC   0(14,R2),DASHES     MOVE IN FIVE WEEK HEADING                    
         MVC   6(3,R2),0(R5)                                                    
         LA    R2,15(R2)                                                        
*                                                                               
DR70     LA    R5,3(R5)            BUMP MONTH NAMES POINTER                     
*                                                                               
DR80     MVC   0(2,R3),THISDATE+4     MOVE IN DATES FOR EACH WEEK               
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         LA    R3,3(R3)                                                         
         L     R1,NUMWEEKS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,NUMWEEKS         ACCUMULATE # OF WEEKS FOR THIS QTR           
         BCT   R8,DR80             REPEAT UNTIL END OF MONTH                    
*                                                                               
DR90     BCT   R4,DR50             REPEAT UNTIL END OF QUARTER                  
         L     R8,ASPOOLD          RESTORE R8                                   
         B     DR100                                                            
*                                                                               
DASHES   DC    C'--------------------------'                                    
         EJECT                                                                  
* DISPLAY ONE LINE FOR EACH PROGRAM                                             
*                                                                               
DR100    L     R5,REFPTR           DISPLAY UP TO 12 REFERENCES OR UNTIL         
         USING REFTABD,R5              END OF REFTAB                            
         LA    R6,12                                                            
*                                                                               
DR110    CLI   0(R5),0             TEST END OF REFTAB                           
         BE    DR300                                                            
*                                                                               
         ZIC   R0,0(R7)            FILL DESCRIPTION FIELD                       
         AR    R7,R0                                                            
         LA    R2,8(R7)                                                         
         USING DISPLIND,R2                                                      
         EDIT  (1,REFREF),(2,DISPREF),FILL=0                                    
         MVC   DISPPROG,REFPROG    ALWAYS DISPLAY PROGRAM NAME                  
         CLI   COSTOPT,C'Y'                                                     
         BE    DR120               IF COST OPT NOT SELECTED                     
*                                                                               
         GOTO1 UNDAY,DMCB,REFDAY,DISPDAY       THEN DISPLAY DAYS                
         GOTO1 UNTIME,DMCB,REFTIME,DISPTIME        AND TIMES                    
         B     DR190                                                            
*                                                                               
DR120    LA    R3,DISPDAY          ELSE DISPLAY RATES IN DAY FIELD              
         DROP  R2                                                               
*                                                                               
         LA    R4,REFDRTAB         FIGURE OUT THE RATE THAT THE QUARTER         
         USING DRTABD,R4               STARTS WITH AND POINT TO THE             
         MVC   RATE,DRRATE             NEXT DRTAB ENTRY AFTER IT                
         LA    R4,DRTABL(R4)                                                    
*                                                                               
         MVC   THISDATE,QSTART                                                  
         OC    STARTWK,STARTWK                                                  
         BZ    DR140                                                            
         L     R2,STARTWK          BUMP THROUGH THE WEEKS LEADING UP            
*                                      TO THE QUARTER                           
DR130    GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,NEXTDATE)                            
         CLC   NEXTDATE,DRDATE                                                  
         BL    DR135                                                            
         MVC   RATE,DRRATE         SAVE LAST RATE AND POINT TO                  
         LA    R4,DRTABL(R4)           NEXT DATE                                
*                                                                               
DR135    BCT   R2,DR130            REPEAT UNTIL REACHED QUARTER                 
*                                                                               
DR140    L     R2,NUMWEEKS         R2 = NUMBER OF WEEKS IN THIS QUARTER         
         SR    R8,R8                                                            
*                                                                               
DR150    C     R8,=F'0'            IF NOT FIRST RATE                            
         BE    DR160                                                            
         MVI   0(R3),C'/'          DISPLAY # OF WEEKS FOR LAST RATE             
         LA    R3,1(R3)                                                         
         EDIT  (R8),(2,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         MVI   0(R3),C','          SEPARATE FROM NEXT RATE WITH A ','           
         LA    R3,1(R3)                                                         
*                                  DISPLAY NEW RATE                             
DR160    EDIT  (4,RATE),(4,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R3,R0                                                            
         SR    R8,R8                                                            
*                                                                               
DR170    BCT   R2,*+8              IF NO MORE WEEKS THIS QUARTER                
         B     DR190                   THEN EXIT                                
*                                                                               
         LA    R8,1(R8)            INCREMENT WEEK COUNTER                       
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,NEXTDATE)                            
         CLC   NEXTDATE,DRDATE                                                  
         BL    DR170               IF REACHED NEXT RATE CHANGE                  
*                                                                               
         MVC   RATE,DRRATE         THEN BUMP TO NEXT DRTAB ENTRY                
         LA    R4,DRTABL(R4)           AND DISPLAY NEW RATE                     
         B     DR150                                                            
*                                                                               
DR190    B     DR200                                                            
         EJECT                                                                  
DR200    ZIC   R0,0(R7)            FILL CASH SPOT FIELD                         
         AR    R7,R0                                                            
         OI    4(R7),X'20'                                                      
         LA    R2,8(R7)                                                         
         L     R4,STARTWK                                                       
         LA    R4,REFCASH(R4)                                                   
         L     R3,NUMWEEKS                                                      
*                                                                               
DR210    CLI   0(R4),X'00'         DISPLAY DOT FOR ZERO SPOTS                   
         BNE   DR220                                                            
         MVI   1(R2),C'.'                                                       
         B     DR230                                                            
*                                  ELSE DISPLAY TWO DIGIT NUMBER                
DR220    EDIT  (1,0(R4)),(2,0(R2)),ZERO=NOBLANK                                 
*                                                                               
DR230    LA    R4,1(R4)                                                         
         LA    R2,3(R2)                                                         
         BCT   R3,DR210            LOOP UNTIL END OF QUARTER                    
*                                                                               
         LA    R5,REFTABL(R5)                                                   
         BCT   R6,DR110            LOOP UNTIL LAST PROGRAM DISPLAYED            
         EJECT                                                                  
DR300    LA    R2,RECTOTSH         DISPLAY WEEKLY DOLLAR TOTALS                 
         ZIC   R1,0(R2)                                                         
         LA    R3,0(R2,R1)                                                      
         MVC   8(14,R2),=C'WEEKLY DOLLARS'                                      
         LA    R2,35+8(R2)         R2 POINTS TO FIRST LINE                      
         LA    R3,38+8(R3)         R3 POINTS TO SECOND LINE                     
         L     R5,STARTWK                                                       
         SLL   R5,2                                                             
         LA    R5,DOLTOTAL(R5)                                                  
         L     R4,NUMWEEKS                                                      
*                                      MOVE FIRST WEEK INTO FIRST LINE          
DR310    EDIT  (4,0(R5)),(5,0(R2)),ZERO=NOBLANK                                 
         LA    R2,6(R2)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,DR320                                                         
         B     DR350                                                            
*                                      MOVE SECOND INTO SECOND LINE             
DR320    EDIT  (4,0(R5)),(5,0(R3)),ZERO=NOBLANK                                 
         LA    R3,6(R3)                                                         
         LA    R5,4(R5)            LOOP UNTIL END OF QUARTER                    
         BCT   R4,DR310                                                         
*                                  DISPLAY PFKEY HELP                           
DR350    MVC   RECPFKS(L'TAILMESS),TAILMESS                                     
*                                  DISPLAY TOTAL DOLLARS                        
         MVC   RECPFKS+L'TAILMESS+6(16),=C'TOTAL DOLLARS  $'                    
         GOTO1 TOTFULL,DMCB,DOLTOTAL,4                                          
         LA    R2,RECPFKS+L'TAILMESS+6+16                                       
         EDIT  (4,0(R1)),(11,0(R2)),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK          
*                                                                               
DRX      OI    CONSERVH+6,X'81'    SET MODIFIED AND XMIT                        
         B     EXIT                                                             
*                                                                               
TAILMESS DC    C'ENTER=NXTQTR PF6=PRVQTR PF7=NXTPRG PF8=PRVPRG'                 
         EJECT                                                                  
CALCPTRS NTR1                      CALCULATE POINTERS FOR THIS QUARTER          
         SR    R2,R2                                                            
         LA    R4,WPMTAB                                                        
         LA    R6,MONAMES                                                       
         CLC   QUARTER,=F'1'                                                    
         BE    CP20                                                             
         L     R1,QUARTER          LOOP FOR 3*(QUARTER-1) WEEKS                 
         BCTR  R1,0                                                             
         M     R0,=F'3'                                                         
*                                                                               
CP10     ZIC   R0,0(R4)                                                         
         AR    R2,R0               ACCUMULATE STARTING WEEK NUMBER              
         LA    R4,1(R4)            BUMP WEEKS PER MONTH POINTER                 
         LA    R6,3(R6)            BUMP MONTH NAMES POINTER                     
         BCT   R1,CP10                                                          
*                                                                               
CP20     ST    R2,STARTWK          SAVE TOTAL IN STARTWK                        
         ST    R4,WPMPTR           SAVE WEEKS PER MONTH POINTER                 
         ST    R6,MONPTR           SAVE MONTH NAMES POINTER                     
*                                                                               
CP30     LA    R5,REFTAB           CALCULATE POINTER TO FIRST REF TO BE         
         CLC   PROGS,=F'1'             DISPLAYED                                
         BE    CP60                                                             
         CLC   PROGS,NUMPROGS                                                   
         BE    CP40                                                             
         L     R3,PROGS            SET R3 TO NUMBER OF REFS BEFORE              
         BCTR  R3,0                    FIRST REF                                
         M     R2,=F'12'                                                        
         B     CP50                                                             
*                                                                               
CP40     L     R3,NUMREFS          FOR LAST PAGE SHOW 12 REFS                   
         S     R3,=F'12'                                                        
*                                                                               
CP50     LA    R5,REFTABL(R5)      LOOP AND BUMP R5                             
         BCT   R3,CP50                                                          
*                                                                               
CP60     ST    R5,REFPTR           SAVE POINTER TO FIRST REF                    
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
INVPFK   OI    RECCASHH+6,X'40'    SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(19,R2),=C'** INVALID PFKEY **'                                 
         OI    CONSERVH+6,X'81'    SET MODIFIED AND XMIT                        
         GOTO1 ERREX2                                                           
*                                                                               
ERRPNF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'** ERROR NO PROGRAMS OR CASH SPOTS'               
         LA    R2,RECMEDH                                                       
         NI    RECMEDH+4,X'DF'     FORCE KEY VALIDATION                         
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  NI    RECMEDH+4,X'DF'     FORCE KEY VALIDATION                         
         GOTO1 ERREX                                                            
*                                                                               
EXIT     OI    RECCASHH+6,X'40'    SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(14,R2),=C'DATA DISPLAYED'                                      
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF4D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
COSTOPT  DS    C                                                                
TRADEOPT DS    C                                                                
KEYCHANG DS    C                                                                
REFCHANG DS    C                                                                
PROGS    DS    F                                                                
NUMPROGS DS    F                                                                
NUMREFS  DS    F                                                                
NUMWEEKS DS    F                                                                
QUARTER  DS    F                                                                
STARTWK  DS    F                                                                
REFPTR   DS    F                                                                
WPMPTR   DS    F                                                                
MONPTR   DS    F                                                                
DOLTOTAL DS    CL(53*4)                                                         
SPPW     DS    XL14                                                             
PRIORWKS DS    F                                                                
RATE     DS    XL4                                                              
REFTAB   DS    30CL(REFTABL)                                                    
TRTAB    DS    CL256                                                            
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
REFTABD  DSECT                                                                  
REFREF   DS    XL1                 REFERENCE NUMBER                             
REFDISKA DS    CL4                 DISK ADDRESS                                 
REFPROG  DS    CL14                TRUNCATED PROGRAM NAME                       
REFDAY   DS    XL1                 DAY CODE                                     
REFTIME  DS    XL4                 TIME CODE                                    
REFCASH  DS    53X                 # OF CASH SPOTS FOR 53 WEEKS                 
REFDRTAB DS    XL(6*DRTABL)        DATE/RATE TABLE                              
REFTABL  EQU   *-REFTABD                                                        
*                                                                               
DRTABD   DSECT                                                                  
DRDATE   DS    XL2                 DATE                                         
DRRATE   DS    XL4                 RATE                                         
DRTABL   EQU   *-DRTABD                                                         
*                                                                               
DISPLIND DSECT                                                                  
DISPREF  DS    CL2                 REFERENCE NUMBER                             
         DS    C                                                                
DISPPROG DS    CL14                TRUNCATED PROGRAM NAME                       
         DS    C                                                                
DISPDAY  DS    CL7                 DAY CODE                                     
         DS    C                                                                
DISPTIME DS    CL11                TIME CODE                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPCSO04   07/01/03'                                      
         END                                                                    
