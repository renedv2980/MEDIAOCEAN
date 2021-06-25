*          DATA SET SPDEMUP    AT LEVEL 117 AS OF 02/26/21                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T00A22C                                                                  
*INCLUDE DEINMKT                                                                
*                                                                               
********************************************************************            
* LEVEL 108  BPOO  SUPPORT PROJECTIONS FOR LIVE+1.  LIVE+1 BOOKTYPE             
*                  IN THE PROJECTION SHOULD RESULT IN LIVE+7 STANARD            
*                  HUT/PUTS                                                     
********************************************************************            
* NOTE * IF WE HAVE INSTANCES WHERE THE DEMOS LOOK TOO BIG                      
* DEPENDING ON A BROAD DAY TIME AFTER A CERTAIN FACTOR                          
* THEN MOST LIKELY WE HAVE OVERFLOWED A SINGLE REGISTER INSTRUCTION             
* WE WILL CONTINUE TO MIGRATE TO PACK DECIMAL INSTRUCTIONS AND                  
* USE DOUBLE WORD EXTENDED BUFFERS.  SAME APPLIES IF WE ABEND DUE TO            
* AN INVALID DIVIDE                                                             
* BPOO                                                                          
********************************************************************            
* BPOO LVL50  1- SUPPORT MULTIBOOK AVERAGE FOR WEEKLY TIME PERIOD               
*             2- CHANGE MULTIBOOK AVERAGE TO IMPRESSIONS BASED                  
*             3- CREATE LARGER BUFFERS FOR WEIGHT AND UNWEIGHT SO               
*                WE DONT OVERFLOW BUFFERS FOR IMPRESSIONS BASED MBK             
*             4- ADJUST HPT, RTG AND IMP WHEN CROSSING UNIVERSES                
*                THEREFORE WE HAVE TO FORCE SPGETIUN NOT TO ACCUMULATE          
*                DEMOS FOR MULTIBOOK AVERAGE - ACCUMULATE LOCALLY               
*             5- MOVED ROUTINES ( UPEND,ADDOVER,GETINDEX, AND SETOVER)          
*                TO SUBROUTINE POOL TO CREATE ROOM                              
*             6- MBK SHARES ARE CALCULATED OFF IMPRESSIONS BASED RATING         
*                AND PUTS UNLESS IMPRESSIONS BASED PUTS ARE NOT                 
*                AVAILABLE, THEN WE TAKE ORIGINAL SHARES                        
********************************************************************            
* -----------> REMEMBER TO LINK LMSPDEMUP <-----------                          
*                                                                               
* -----------> DISABLED OPTIONS <-------------------                            
* 1. CANADIAN NSI UPGRADES                                                      
* 2. TAPE BASED DEMOS                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* PARAM1 BYTE(S) 1-3 = A(SPOT UPGRADE BLOCK) (SPDEMUPD)               *         
* PARAM2         1-3 = A(INPUT DEMO LIST) OR ZERO                     *         
* PARAM3         1-3 = A(OUTPUT DEMO AREA) OR ZERO                    *         
***********************************************************************         
         TITLE 'SPDEMUP - SPOT SYSTEM DEMO UPGRADE ROUTINES'                    
SPDEMUP  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPDEMUP,RA,R5,CLEAR=YES,RR=R8                        
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RD,SAVERD           SAVE RD FOR ERROR EXIT                       
         ST    R1,APARM                                                         
         MVI   0(R1),0             RESET ERROR BYTE                             
         L     R9,0(R1)                                                         
         USING SPDEMUPD,R9         R9=A(LOOK-UP/UPG   E EXPRESSION)             
         MVC   ADEMLST,4(R1)       SAVE A(DEMO LIST) & A(OUTPUT)                
         MVC   ADEMOUT,8(R1)                                                    
         ST    R8,RELO                                                          
         L     RF,=V(SUBR01)                                                    
         A     RF,RELO                                                          
         ST    RF,VSUBR01                                                       
         L     RF,=V(SUBR02)                                                    
         A     RF,RELO                                                          
         ST    RF,VSUBR02                                                       
         LH    RF,=Y(IOAREA2-WORKD)                                             
         AR    RF,RC                                                            
         ST    RF,AIO2             GET ADDRESSABILITY TO IO2                    
         LH    RF,=Y(IOAREA1-WORKD)                                             
         AR    RF,RC                                                            
         ST    RF,AIO1             GET ADDRESSABILITY TO IO2                    
         GOTO1 VSUBR02,DMCB,('INITQ',(RC))                                      
         CLI   SPUPSRC,0           TEST SOURCE INVALID                          
         BE    UPX                                                              
                                                                                
         LA    RF,TPTVALS                                                       
         ST    RF,ATPTVALS                                                      
         LA    RF,PAVVALS                                                       
         ST    RF,APAVVALS                                                      
         LA    RF,OFORMAT                                                       
         ST    RF,AOFORMAT                                                      
*                                                                               
UP1      MVC   LOCALMED,SPUPMED            SETUP FOR CSI UPGRADES               
         CLI   LOCALMED,MEDCAN                                                  
         BNE   *+8                                                              
         CLI   SPUPSRC,C'N'                                                     
         BNE   *+8                                                              
         MVI   LOCALMED,C'T'                                                    
         MVC   LOCALMED,SPUPMED            DISABLE OPTION FOR NOW               
         SPACE 1                                                                
         L     R0,=V(DEINMKT)                                                   
         A     R0,RELO                                                          
         ST    R0,VDEINMKT                                                      
         SPACE 1                                                                
**********************************************************                      
* MOVED TO INIT ROUTINE                                                         
*&&DO                                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A26' DEFINE                                
         MVC   VDEFINE,0(R1)                                                    
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A24' SPGETIUN                              
         MVC   VGETIUN,0(R1)                                                    
*&&                                                                             
*                                                                               
         CLI   SPUPSTA,C'Z'        NO DEMOS FOR CABLE (TEST ON NTWK)            
         BH    UPX                                                              
         CLI   LOCALMED,MEDCAN                                                  
         BE    UP8                                                              
         LA    R7,DBLOCK1          GET US AGENCY CONTROL VALUES                 
         USING DBLOCKD,R7                                                       
         BAS   RE,BLDBLK                                                        
         GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
         MVC   DBFILE,TPTVALS+1                                                 
                                                                                
         MVI   DBFUNCT,DBGETCTL                                                 
         MVC   DBSELSYC,SPUPSYSC                                                
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         TM    DBCSHR,DBOTOSHR     TEST COMPUTE HOMES SHARES USING R/P          
         BZ    *+8                                                              
         MVI   PERSHR,YES                                                       
                                                                                
         L     R1,VCOMFACS                                                      
         L     R1,CT00AD0-COMFACSD(R1)  LOCATE IUN DISP. TABLE ENTRY            
         LA    R1,16(R1)           GO PAST HEADER                               
         USING DSPHDRD,R1          R1=A(MASTER DISP. TABLE)                     
UP6      CLC   0(2,R1),=X'0000'    TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DSPFILE,IUNVALS+1   MATCH ON INTERNAL FILE CODE                  
         BE    *+16                                                             
         ICM   RE,7,DSPAET                                                      
         LA    R1,1(RE,R1)         BUMP TO NEXT TABLE HEADER                    
         B     UP6                                                              
                                                                                
         CLI   TAPEOPT,C'Y'        TAPE BASED OPTION                            
         BE    UP7                                                              
         CLC   DSPSBOOK,=X'B0F4'   BOOK BASE DISP TABLE                         
         BE    UP7                                                              
         ICM   RE,7,DSPAET         NO - USE NEXT DISPLACEMENT TABLE             
         LA    R1,1(RE,R1)                                                      
                                                                                
UP7      LA    R1,DSPHDRLN(R1)     BUMP PAST HEADER                             
         ST    R1,ADISPDTA         SAVE A(FIRST DEMO ENTRY)                     
         DROP  R1                                                               
                                                                                
UP8      LA    R7,DBLOCK1                                                       
         BAS   RE,BLDBLK           BUILD DBLOCK FOR RATING LOOK-UPS             
         GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
         CLC   SPUPFIL,PAVVALS     TEST FOR PAV FILE LOOK-UP                    
         BNE   UP10                                                             
         XC    DBSELBK,DBSELBK     YES - GET LATEST T/P BOOK                    
         MVC   DBFILE,TPTVALS+1                                                 
                                                                                
                                                                                
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBSELSYC,SPUPSYSC                                                
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
*                                                                               
         GOTO1 VDEMAND,DMCB,DBLOCK,0,0                                          
         L     R1,APARM                                                         
         CLI   DBERROR,0           TEST FOR ERRORS                              
         BE    *+14                                                             
         MVC   0(1,R1),DBERROR     YES - RETURN DEMAND ERROR NUMBER             
         B     UPX                                                              
         OC    SPUPFBK,SPUPFBK     TEST BOOK PASSED                             
         BZ    *+14                                                             
         CLC   SPUPFBK,DBACTBK     YES - TEST GR LATEST BOOK                    
         BNH   *+10                                                             
         MVC   SPUPFBK,DBACTBK     SET BOOK FROM LATEST T/P BOOK                
         LA    R1,SPUPFBKL                                                      
         LA    RF,L'SPUPFBKL/2     DO THE SAME FOR BOOK LIST                    
                                                                                
UP9      CLC   0(2,R1),=X'0000'                                                 
         BE    UP9A                                                             
         CLC   0(2,R1),DBACTBK                                                  
         BNH   *+10                                                             
         MVC   0(2,R1),DBACTBK                                                  
         LA    R1,2(R1)                                                         
         BCT   RF,UP9                                                           
                                                                                
UP9A     BAS   RE,BLDBLK           RE-BUILD DBLOCK WITH BOOK VALUE              
         GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
         B     UP10                                                             
         EJECT                                                                  
***********************************************************************         
* GET ORIGINAL DEMO VALUES                                            *         
***********************************************************************         
         SPACE 1                                                                
UP10     MVI   DBFUNCT,DBTSTACS    TEST IF USER HAS ACCESS TO FILE              
         MVC   DBSELSYC,SPUPSYSC                                                
         GOTO1 VDEMAND,DMCB,DBLOCK,0,0                                          
         L     R1,APARM                                                         
         CLI   DBERROR,0           TEST ACCESS IS VALID                         
         BE    *+14                                                             
         MVC   0(1,R1),DBERROR     NO - RETURN WITH ERROR                       
         B     UPX                                                              
         MVI   DBFUNCT,DBGETDEM    SET TO LOOK-UP DEMOS                         
         XC    TOTHMSHR,TOTHMSHR                                                
         XC    MBKHMSHR,MBKHMSHR                                                
                                                                                
         CLI   LOCALMED,MEDCAN     BUILD DUMMY RECORD AT IO2 FOR CANADA         
         BNE   UP11                                                             
         L     R1,AIO2                                                          
         XC    0(50,R1),0(R1)                                                   
         MVI   0(R1),C'R'                                                       
         LA    R0,PRFRSTEL+1-PRKEY                                              
         STCM  R0,3,PRRLEN-PRKEY(R1)                                            
                                                                                
UP11     MVC   DBTAPEP,TAPEOPT                                                  
         CLC   =X'9999',SPUPFLD3  ASSUME ALWAYS LPM BOOKTYPE FOR LPS            
         BNE   *+8                                                              
         MVI   DBBTYPE,C'P'                                                     
*                                                                               
*  CALL ROUTINE TO CHECK IF ANY OF THE BOOKS ARE LIVE ZEROCELL                  
*  IF ONE OF THE BOOKS IS LIVE ZERO CELL THEN THE CURRENT BOOK WILL             
*  BE ADJUSTED TO THE CORRECT PARALLEL ZEROCELL BOOKTYPE                        
*                                                                               
         MVI   ZCELLBTY,0                                                       
         L     RF,=A(CHKLIVEZ)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         L     RF,=A(CHKLIVEM)     CHECK LIVE MONTHLY BOOK                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
**       L     RF,=A(CHKNHUT)      FOR NEW HUT BOOKS                            
**       A     RF,RELO                                                          
**       BASR  RE,RF                                                            
*&&DO                                                                           
* WE SHOULD NOT LOOK AT IMPACT BOOKTYPES FOR SHARE BOOK                         
* AS PER MARIA                                                                  
         L     RF,=A(CHKLEXPB)     FOR NEW HUT BOOKS                            
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*&&                                                                             
         MVC   DBSELSYC,SPUPSYSC                                                
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
UP11A    GOTO1 VDEMAND,DMCB,DBLOCK,DEMHK                                        
*                                                                               
*                                                                               
         L     R1,APARM                                                         
         OC    DBDIVSOR,DBDIVSOR   TEST DEMOS FOUND                             
         BNZ   UP12                                                             
* NOT FOUND !                                                                   
* NEW HUT CATEGORIES ONLY AVAILABLE FOE LPM MARKETS                             
* IF NOT FOUND RELOOK UP USING ORIGINAL BOOKTYPE                                
*                                                                               
**       CLI   NHUTFLAG,C'Y'                                                    
**       BNE   UP11B                                                            
**       MVI   NHUTFLAG,C'N'                                                    
         CLI   NEXPFLAG,C'Y'                                                    
         BNE   UP11B                                                            
         MVI   NEXPFLAG,C'N'                                                    
         MVC   DBBTYPE,ZSVBKTYP                                                 
* RESTORE TIME BECAUSE GETTP COULD HAVE CHANGED IT.                             
* EX, CROSSING THE START OF DAY WHERE GETTP SPLITS THE TIMES                    
         OC    SVSELTIM,SVSELTIM                                                
         BZ    *+10                                                             
         MVC   DBSELTIM,SVSELTIM                                                
         B     UP11A                                                            
*                                                                               
*&&DO                                                                           
* NOT FOUND !                                                                   
* CHECK IF WE ARE ASKED FOR ZERO CELL DATA                                      
* IF SO IT WILL SET THE BOOKTYPE TO NON ZEROCELL LOOKUP                         
*                                                                               
         CLI   ZCELLBTY,0                                                       
         BNE   UP11B                                                            
         L     RF,=A(NONZEROC)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
         BNE   UP11A              ZERO CELL - REREAD FOR NON ZEROCELL           
*&&                                                                             
*                                 X'FF'- MEANT NOT A ZEROCELL LOOKUP            
* RESTORE FROM SAVED BOOKTYPE AT CHKLIVEZ                                       
UP11B    MVC   DBBTYPE,ZSVBKTYP                                                 
         MVI   0(R1),X'FF'         RETURN NO DEMOS FOUND ERROR                  
         B     UPX                                                              
*                                                                               
UP12     CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BE    *+14                                                             
         MVC   0(1,R1),DBERROR     RETURN DEMAND ERROR NUMBER                   
         B     UPX                                                              
                                                                                
*                                                                               
         GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
*  RESET THE BOOKTYPE IF IT HAS BEEN CHANGED BY CHKLIVEZ ROUTINE                
         MVC   DBBTYPE,ZSVBKTYP                                                 
*                                                                               
                                                                                
         MVC   SPUPACTS,DBACTSRC   SET ACTUAL RATING SERVICE                    
         MVC   DBSELSRC,DBACTSRC                                                
                                                                                
         CLI   LOCALMED,MEDCAN     CANADIAN LOOK-UPS                            
         BE    UP14                                                             
         OC    DBSELBK,DBSELBK     LATEST IS AFTER X'5801'                      
         BZ    *+14                                                             
         CLC   DBSELBK,=X'5801'    IF AFTER DEC/87                              
         BL    *+8                                                              
         MVI   SBKCTRL,1           SET FOR NEW PRECISION                        
         B     UP16                                                             
UP14     L     R1,AIO2                                                          
         STCM  R1,15,DBAREC                                                     
         LA    R1,PRFRSTEL-PRKEY(R1)                                            
         STCM  R1,15,DBAQUART                                                   
         XC    WORK,WORK                                                        
         LA    R0,DBLOCKD                                                       
         STCM  R0,15,WORK                                                       
         MVC   WORK+06(2),DBDIVSOR                                              
         MVC   WORK+08(3),DBFILE                                                
         MVC   WORK+11(3),DBFILE                                                
         GOTO1 VDEMOMTH,DMCB,DIV,AIO2,AIO2,WORK                                 
         CLI   SPUPTYPE,X'0F'      WE TREAT MBKAVG AS SPUPTNDX                  
         BE    *+8                 BUT WEEKLY HAS TO ACCOUNTED FOR              
         CLI   SPUPTYPE,X'0A'      IMS WAS ALSO 04 BEFORE NOW 0A                
         BE    *+8                                                              
*                                  DUE TO SPUPEXTEND BEING USED NOW             
         CLI   SPUPTYPE,SPUPTNDX   TEST INDEX UPGRADE                           
         BNE   UP72                NO - OTHER UPGRADES NOT SUPPORTED            
         MVC   WORK+6(2),SPUPFLD1                                               
*                                  MULTIPLY RECORD BY UPGRADE INDEX             
         GOTO1 (RF),(R1),MUL,AIO2,AIO2,WORK                                     
         LA    R0,100                                                           
         STCM  R0,3,WORK+6                                                      
*                                  DIVIDE RECORD BY 100                         
         GOTO1 (RF),(R1),DIV,AIO2,AIO2,WORK                                     
         B     UP72                                                             
                                                                                
UP16     CLI   SPUP2YRR,C'Y'       TEST 2 YEAR RATINGS REQUIRED                 
         BNE   UP18                                                             
         MVC   DBSELBK,SPUPFBK     YES - DECREMENT ACTUAL BOOK                  
         NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
         ZIC   R1,DBSELBK                                                       
         BCTR  R1,0                                                             
         STC   R1,DBSELBK                                                       
         PACK  DBSELWKN,SPUPFBK+1(1)                                            
         NI    DBSELWKN,X'0F'      AND OFF MONTH BITS                           
         XC    DBACTBK,DBACTBK                                                  
         MVC   SVDIVSOR,DBDIVSOR   SAVE DBDIVSOR VALUE                          
         XC    DBDIVSOR,DBDIVSOR                                                
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBSELSYC,SPUPSYSC                                                
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
         GOTO1 VDEMAND,DMCB,DBLOCK,DEMHK                                        
         SR    R0,R0                                                            
         ICM   R0,3,DBDIVSOR       ADD DBDIVSORS FOR CALLS TOGETHER             
         AH    R0,SVDIVSOR                                                      
         STCM  R0,3,DBDIVSOR                                                    
*                                  CLEAR WORK AREA VALUES                       
UP18     XC    OLDRTG(LENVALS*2),OLDRTG                                         
         XC    OLDHPT(LENVALS*2),OLDHPT                                         
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
                                                                                
         CLI   EXTBUFF,C'Y'        USE EXTENDED BUFF?                           
         BE    UP24                                                             
         CLI   MBKIFLAG,C'Y'       MULTIBK IMPRESSIONS BASED                    
         BE    UP24                USE EXPANDED BUFFERS                         
                                                                                
UP20     LA    R0,3                UNWEIGHT VUTS                                
         LA    R1,HOMEVUTS                                                      
         ST    R1,DMCB+8                                                        
         SR    R2,R2                                                            
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
         B     UP28                                                             
                                                                                
UP24     LA    R0,3                UNWEIGHT VUTS                                
         LA    R1,MBKHVUTS         EXPANDED BUFFER                              
         ST    R1,DMCB+8                                                        
         LA    R2,HOMEVUTS         NORMAL BUFFER                                
         GOTO1 VSUBR01,DMCB,('MBKUNWQ',(RC)),(R0),,(R2)                         
                                                                                
UP28     CLI   EXTBUFF,C'Y'        USE EXTENDED BUFFERS?                        
         BE    UP32                                                             
         CLI   MBKIFLAG,C'Y'                                                    
         BE    UP32                                                             
         LA    R0,NUMVALS*4        UNWEIGHT DEMO VALUES                         
         LA    R1,NEWRTG                                                        
         ST    R1,DMCB+8                                                        
         LA    R2,OLDRTG                                                        
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
*        BAS   RE,UNWGHT                                                        
         B     UP38                                                             
UP32     LA    R0,NUMVALS*4        UNWEIGHT DEMO VALUES                         
         L     R1,AIO2                                                          
         LA    R1,MBKRTG-IOAREA2(R1)                                            
         ST    R1,DMCB+8                                                        
         LA    R2,OLDRTG                                                        
         GOTO1 VSUBR01,DMCB,('MBKUNWQ',(RC)),(R0),,(R2)                         
                                                                                
                                                                                
UP38     LA    RF,3                RF=N'HOMES SHARE VALUES                      
         SR    RE,RE               RE=INDEX REGISTER                            
UP40     L     R1,UORHOMES(RE)     HOMES SHARE=RATING/VUT                       
         M     R0,=F'2000'         RATING + PUT LINES ARE 1 DECIMAL             
         L     R2,HOMEVUTS(RE)                                                  
         CLI   PERSHR,YES          TEST FOR HOMES SHARE=R/P                     
         BNE   *+8                                                              
         L     R2,UOPHOMES(RE)                                                  
                                                                                
                                                                                
         CLI   MBKIFLAG,C'Y'       MBK DMA=I USE UOPHOMES                       
         BNE   UP46                                                             
         L     R2,UOPHOMES(RE)                                                  
                                                                                
UP46     LTR   R2,R2               TEST FOR ZERO PUT                            
         BNZ   *+10                                                             
         SR    R1,R1               YES-SET SHARE TO ZERO                        
         B     *+14                                                             
         DR    R0,R2                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,HOMSHR(RE)                                                    
         LA    RE,4(RE)                                                         
         BCT   RF,UP40                                                          
                                                                                
         DS    0H                                                               
                                                                                
         CLI   TAPEOPT,C'Y'        USE ORIG SHARES IF IMP-BASED                 
         BNE   UP64                                                             
*                                                                               
*                                                                               
         CLI   MBKIFLAG,C'Y'        DMA=I MULTIBOOK AVERAGE                     
         BNE   UP50                 IF WE COULDNT GET UOPHOMES FROM             
         OC    UOPHOMES(4),UOPHOMES GETIUN, WE MUST USE ORIG SHARES             
         BZ    UP58                ELSE                                         
         B     UP64                USE RECALCULATED SHARES                      
*                                                                               
UP50     CLI   EXTBUFF,C'Y'                                                     
         BE    UP58                 USE EXTENDED BUFFERS ?                      
                                                                                
         LHI   R0,3                # OF HOME SHARES TO UNWEIGH                  
         LA    R1,TOTHMSHR         ACCUMULATED ORIGINAL SHARES,                 
         ST    R1,DMCB+8                                                        
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         LA    R2,HOMSHR            UNWEIGH AND OUTPUT THEM HERE                
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
*&&DO                                                                           
* TAKE OUT FOR NOW - DONT CLEAR OUT HOMSHRS                                     
* IT IS RECALCULATED IN HOOK FOR WIRED CABLE                                    
*                                                                               
*RECALCULATE THE HOME SHARES FOR NSI WIRED - ORIGINAL SHARES FROM FILE          
*WERE NOT BASED ON THE DMA PUTS                                                 
         CLI   DBSELSRC,C'N'                                                    
         BNE   UP64                                                             
         CLI   DBSELMED,C'T'                                                    
         BNE   UP64                                                             
         CLI   DBACTBTY,C'3'         ZERO CELL CABLE                            
         BE    *+8                   GETTP SETS BOOTKYPE C                      
         CLI   DBACTBTY,C'C'         FOR LOCAL CABLE                            
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'          ZERO CELL WIRED                            
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   *+10                                                             
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
*&&                                                                             
         B     UP64                                                             
UP58     LHI   R0,3                # OF HOME SHARES TO UNWEIGH                  
         LA    R1,MBKHMSHR         ACCUMULATED ORIGINAL SHARES,                 
         ST    R1,DMCB+8           IN EXPANDED AREA                             
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         LA    R2,HOMSHR            UNWEIGH AND OUTPUT THEM HERE                
         GOTO1 VSUBR01,DMCB,('MBKUNWQ',(RC)),(R0),,(R2)                         
*&&DO                                                                           
* TAKE OUT FOR NOW - DONT CLEAR OUT HOMSHRS                                     
* IT IS RECALCULATED IN HOOK FOR WIRED CABLE                                    
*                                                                               
*RECALCULATE THE HOME SHARES FOR NSI WIRED - ORIGINAL SHARES FROM FILE          
*WERE NOT BASED ON THE DMA PUTS                                                 
         CLI   DBSELSRC,C'N'                                                    
         BNE   UP64                                                             
         CLI   DBSELMED,C'T'                                                    
         BNE   UP64                                                             
         CLI   DBACTBTY,C'3'         ZERO CELL CABLE                            
         BE    *+8                   GETTP SETS BOOTKYPE C                      
         CLI   DBACTBTY,C'C'         FOR LOCAL CABLE                            
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'          ZERO CELL WIRED CABLE                      
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   *+10                                                             
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
*&&                                                                             
UP64     EQU   *                                                                
                                                                                
         LA    RF,NUMVALS*2        RF=N'SHARE VALUES                            
         SR    RE,RE               RE=INDEX REGISTER                            
UP70     L     R1,OLDRTG(RE)       SHARE=RATING/PUT                             
         M     R0,=F'2000'         RATING + PUT LINES ARE 1 DECIMAL             
         L     R2,OLDHPT(RE)                                                    
         LTR   R2,R2               TEST FOR ZERO PUT                            
         BNZ   *+10                                                             
         SR    R1,R1               YES-SET SHARE TO ZERO                        
         B     *+14                                                             
         DR    R0,R2                                                            
         AH    R1,=H'1'            ROUNDED DIVIDE                               
         SRA   R1,1                SHARE PRECISION IS XX.X                      
         ST    R1,OLDSHR(RE)                                                    
         LA    RE,4(RE)                                                         
         BCT   RF,UP70                                                          
         CLI   PERSHR,YES          TEST HOMES SHARES=R/P                        
         BE    *+10                YES-USE COMPUTED R/P IN UPGRADE              
         MVC   OLDSHR+(UORHOMES-OLDRTG)(HOMSHRLN),HOMSHR                        
                                                                                
         MVC   NEWRTG(LENVALS*2),OLDRTG                                         
         MVC   NEWHPT(LENVALS*2),OLDHPT                                         
         BAS   RE,UPGRADE          CALL DEMO UPGRADE ROUTINE                    
                                                                                
** NOTE - VALUES BELOW ARE SET HERE BECAUSE OF ADDRESSABILITY PROBLEMS          
** IN GODEMOUT                                                                  
                                                                                
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR LOOK-UPS                    
         MVC   DBFILE,PAVVALS+1    FORCE OVERRIDE ELEMENT LOOK-UP               
         LA    RF,DEXTRA1          SET FOR 1 DEC AND HUNDREDS                   
         XC    0(L'DEXTRA1,RF),0(RF)                                            
         MVC   0(4,RF),=C'SPOT'                                                 
         ST    RF,DBEXTEND                                                      
UP72     TM    SPUPOPTS,X'81'      1 OR 2 DECIMAL PRECISION                     
         BZ    UP80                NEITHER                                      
*                                                                               
*                                                                               
         TM    SPUPOPTS,X'01'      TEST 2 DECIMAL REQUEST                       
         BO    UP76                                                             
         LA    RF,DEXTRA1          SET FOR 1 DEC AND HUNDREDS                   
         USING DBXTTID,RF                                                       
         MVI   DBXTTRP,X'01'       SPECIAL 1-DECIMAL REQUEST                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
         B     UP80                                                             
*                                                                               
UP76     MVI   DBXTSCTL,C'2'       REQUEST 2-DEC DEMOS                          
         DROP  RF                                                               
*                                                                               
UP80     GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
* NEED THIS FLAG SO WE DEMOUT KNOWS WE ARE REQUESTING 2 DEC IMPS                
* SO IT CAN ADJUST THE DEMOS FOR TSA SHARE TO 10 TIMES SMALLER                  
*                                                                               
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         L     RF,=A(GODEMOUT)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         OC    SPUPSYSE,SPUPSYSE   IF THE SPOT 4 CHAR SYSCODE IS SET            
         BZ    *+10                THEN CLEAR OUT THE BINARY SYSCODE            
         XC    SPUPSYSC,SPUPSYSC   ON EXIT                                      
*                                                                               
UPERR    L     RD,SAVERD           ENTER HERE ON ERROR                          
UPX      XIT1  ,                   EXIT FROM MODULE                             
XIT      EQU   UPX                                                              
         EJECT                                                                  
***********************************************************************         
* HOOK ROUTINE TO PROCESS DEMO ELEMENTS FOR OLD DATA LOOK-UPS         *         
***********************************************************************         
         SPACE 1                                                                
DEMHK    ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                  EXTRACT ACTUAL BOOK VALUE                    
*                                                                               
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+10                                                             
         MVC   SPUPFBK,DBACTBK                                                  
*                                  EXTRACT PROGRAM NAMES                        
         GOTO1 VDEFINE,DMCB,PROGPLUS,DBLOCKD,NDPROG                             
         OC    STPROG,STPROG                                                    
         BNZ   *+10                                                             
         MVC   STPROG,NDPROG                                                    
*                                  EXTRACT MARKET NUMBER                        
         OC    MARKET,MARKET       TEST MARKET NUMBER SET                       
         BNZ   DEMHK1                                                           
         L     R1,DBAREC           POINT TO FIRST ELEMENT                       
         LA    R1,DRFRSTEL-DRKEY(R1)                                            
         CLI   0(R1),MARCODEQ      EXTRACT MARKET NUMBER                        
         BNE   DEMHK1                                                           
         MVC   MARKET,MARNO-MARELEM(R1)                                         
*                                                                               
DEMHK1   CLI   LOCALMED,MEDCAN     TEST CANADIAN LOOK-UPS                       
         BNE   DEMHK2                                                           
         XC    WORK,WORK           MAD RECORD INTO IOAREA2                      
         LA    R0,DBLOCKD                                                       
         ST    R0,WORK                                                          
         MVC   WORK+6(2),DBFACTOR                                               
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),DBFILE                                                
*                                                                               
*FOLLOWING CODE HAS BEEN DISABLED.  WAS ORIGINALLY PUT IN                       
*BECAUSE WE COULD NOT AVERAGE NEW BBM FORMAT RECORDS WITH OLD FORMAT            
*SO WE WOULD PASS IN A BOOKTYPE "U" AND DEMEL WILL SET THE OFORMAT              
*TO THE OLD FORMAT AND THIS WILL DETERMINE THE DEMDISPS TABLE                   
*SINCE THE INPUT FORMAT AND OUTPUT FORMATS ARE DIFFERENT - DEMEL                
*CALLS DEMOUT TO EXTRACT VALUES USING THE OFORMAT ASSIGNED                      
*                                                                               
* IM LEAVING THIS CODE IN BECAUSE IT IS EXTREMELY HELPFUL IN                    
* UNDERSTANDING A PART OF THE DEMO SYSTEM.                                      
*                                                                               
*&&DO                                                                           
         CLI   DBSELMED,C'C'       BBM DIARY WILL BE FORCED TO                  
         BNE   BBMFMTX             USE OLD BBM FORMAT IN DEMEL                  
         CLI   DBSELSRC,C'A'       IF WE FORCE THE BOOKTYPE                     
         BNE   BBMFMTX                                                          
         CLI   DBBTYPE,C'W'                                                     
         BE    BBMFMTX                                                          
         MVI   DBBTYPE,C'U'        FORCE DBBTYPE TO U FOR DEMEL                 
*&&                                                                             
BBMFMTX  DS    0H                                                               
*                                                                               
         GOTO1 VDEMOMTH,DMCB,MAD,DBAREC,AIO2,WORK                               
         CLI   DBBTYPE,C'U'        TAKE OUT FORCED BOOKTYPE                     
         BNE   *+8                 FOR BBM DIARY                                
         MVI   DBBTYPE,0                                                        
         B     DEMHKX                                                           
*                                                                               
DEMHK2   CLI   PUREFLAG,YES        TEST PURE PAV RECORD                         
         BNE   DEMHK3                                                           
         L     R1,DBAREC           EXTRACT PROGRAM DAY/WEEK                     
         MVC   PURESTIM,PRSTIM-PRKEY(R1)                                        
         MVC   PUREDW,PRDW-PRKEY(R1)                                            
         L     R1,DBAQUART         EXTRACT PROGRAM DURATION                     
         ICM   R1,1,PHDUR-PHELEM(R1)                                            
         BNZ   *+8                                                              
         LA    R1,2                                                             
         SRL   R1,1                ROUND TO NEXT LOWEST EVEN QTR HOUR           
         SLL   R1,1                                                             
         STC   R1,PUREDUR                                                       
*                                                                               
DEMHK3   DS    0H                                                               
         SPACE 1                                                                
* EXTRACT OLD UNVS/RTGS/IMPS/PUTS/TOTS                                          
         SPACE 1                                                                
         CLI   EXTBUFF,C'Y'                                                     
         BE    DEMHK8               USE EXTENDED BUFFERS?                       
*                                                                               
         CLI   MBKIFLAG,C'Y'                                                    
         BE    DEMHK8                                                           
                                                                                
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),NEWUNV                                   
                                                                                
         OC    OLDUNV(LENVALS),OLDUNV   TEST OLD UNVS SET YET                   
         BNZ   *+10                                                             
         MVC   OLDUNV(LENVALS),NEWUNV                                           
                                                                                
         DS    0H                  EXTRACT HOME SHARES                          
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         OC    DBAQUART,DBAQUART                                                
         BZ    DEMHK3SHRX                                                       
         L     R0,=A(DEMOSHR)                                                   
         A     R0,RELO                                                          
         MVC   SVDBACBK,DBACTBK                                                 
         GOTO1 VDEMOUT,DMCB,(C'P',(R0)),DBLOCK,HOMSHR,0                         
         MVC   DBACTBK,SVDBACBK                                                 
*                                                                               
                                                                                
DEMHK3D  LA    R0,3                                                             
         LA    R1,HOMSHR                                                        
         ST    R1,DMCB+8                                                        
         LA    R2,TOTHMSHR                                                      
         GOTO1 VSUBR01,DMCB,('WGHTUPQ',(RC)),(R0),,(R2)                         
         B     DEMHK3SHRX                                                       
                                                                                
DEMHK3SHRX EQU *                                                                
*                                                                               
*                                                                               
* FOR WIRED LOOKUPS RECALCULATE THE HOME SHARES                                 
*                                                                               
         CLI   DBSELMED,C'T'          NSI USTV                                  
         BE    DEMHK3A                ALWAYS RELOOK HOMESHARE                   
         CLI   DBSELSRC,C'F'          FOR LPM MKTS CABLE                        
         BE    DEMHK3A                WHICH READ WIRED                          
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   DEMHK3D                                                          
         CLI   DBSELSRC,C'N'                                                    
         BNE   DEMHK3SHRXX                                                      
DEMHK3A  LHI   R0,3                                                             
         LA    R3,HOMSHR                                                        
         LA    RF,NEWUNV                                                        
         LA    R1,NEWHPT-NEWUNV(RF)                                             
         AHI   R1,(DISPHOM*4)                                                   
         LA    R6,NEWRTG-NEWUNV(RF)                                             
         AHI   R6,(DISPHOM*4)                                                   
DEMHK3B  L     R2,0(R1)               HUT                                       
         L     RF,0(R6)               HOMES                                     
         OR    R2,R2                                                            
         BZ    DEMHK3C                                                          
         XC    WORK,WORK                                                        
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                HOMES                                     
         OI    DUB2+7,X'0C'                                                     
         MVC   WORK(3),=X'02000C'     *2000                                     
         MP    DUB2(8),WORK(3)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB2                                                   
         XC    DUB2,DUB2                                                        
         CVD   R2,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         DP    WORK(16),DUB2(8)                                                 
         MVC   DUB2(8),WORK                                                     
         CVB   RF,DUB2                                                          
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         SR    RE,RE                                                            
*                                                                               
         ZICM  R2,DBDIVSOR,(3)                                                  
         AH    R2,SVDIVSOR            PREVIOUS DIVSOR                           
         MR    RE,R2                                                            
***      MH    RF,DBDIVSOR                                                      
         LR    RE,RF                                                            
         ST    RE,0(R4)               HOME SHARES                               
         ST    RE,0(R3)               GETIUN ACCUMULATED VALUES                 
DEMHK3C  AHI   R1,4                   BUMP-HUT                                  
         AHI   R6,4                   BUMP-DMA IMP                              
         AHI   R3,4                   BUMP-HOMSHR                               
         BCT   R0,DEMHK3B                                                       
DEMHK3SHRXX EQU *                                                               
*                                                                               
*                                  EXTRACT VUTS (FOR HOMES SHARES)              
         CLI   PERSHR,YES          TAKE OUT FOR NOW BPOO                        
         BE    DEMHKX                                                           
         XC    OVERDEMS(12),OVERDEMS                                            
         CLI   DBSELMED,C'N'                                                    
         BNE   DEMHK4                                                           
         MVC   DEMOLST(10),NETPUTS                                              
         GOTO1 VDEMOUT,DMCB,(C'L',DEMOLST),DBLOCK,OVERDEMS                      
         B     DEMHK5                                                           
*                                                                               
DEMHK4   GOTO1 VDEMOUT,(R1),(C'L',HOMEVUT),DBLOCK,OVERDEMS                      
         OC    OVERDEMS(4),OVERDEMS   ANY VUTS (RTG/SHR)                        
         BNZ   DEMHK5                                                           
         GOTO1 (RF),(R1),(C'L',HOMEPUT)  NO - USE THE PUT                       
*                                                                               
                                                                                
DEMHK5   LA    R0,3                WEIGHT & ACCUMULATE VUTS                     
         LA    R1,OVERDEMS                                                      
         ST    R1,DMCB+8                                                        
         LA    R2,HOMEVUTS                                                      
         GOTO1 VSUBR01,DMCB,('WGHTUPQ',(RC)),(R0),,(R2)                         
         B     DEMHKX                                                           
                                                                                
* THIS PART OF THE HOOK IS ONLY FOR IMPRESSIONS BASED                           
* MULTIBOOK AVERAGE SO FAR                                                      
* PASS GETIUN A BLANK RECORD JUST TO EXPLODE THE RECORD  ONE                    
* RECORD AT A TIME. DONT WANT THE WEIGHTING FROM GETIUN                         
* BECAUSE GETIUN ACCUMULATES AND WEIGHTS TO VALUE IT MESSES UP                  
* OUR CALULATION                                                                
*                                                                               
DEMHK8   LA    R0,NEWUNV           CLEAR DOWN SAVE AREA                         
         LHI   R1,MBKIUNL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SVFACTOR,DBFACTOR        NO WEIGHTING IN SPGETIUN                
         MVC   DBFACTOR,=H'1'                                                   
                                                                                
DEMHK8A  TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),NEWUNV                                   
         MVC   DBFACTOR,SVFACTOR                                                
                                                                                
* SINCE NOT ONLY MULTIBOOK AVG GOES THROUGH THIS PART                           
* OF THE HOOK IN ORDER TO USE THE EXTENDED BUFFERS - OVERLOW FULLWORD           
* CHECK TO SEE IF WE ARE PROCESSING MULTIBOOK AVG                               
* MULTIBOOK AVG IS THE ONLY UPGRADE WE NORMALIZE IMP,RATINGS AND HPT            
* BASED ON THE UNIVERSE                                                         
*                                                                               
         CLI   MBKIFLAG,C'Y'                                                    
         BNE   DEMHK8C                                                          
         OC    OLDUNV(LENVALS),OLDUNV                                           
         BZ    *+18                                                             
         CLC   NEWUNV(LENVALS),OLDUNV   DID UNIVERSE CHANGE?                    
         BE    *+8                                                              
         BAS   RE,NORMIMP                                                       
DEMHK8C  EQU   *                                                                
*                                                                               
                                                                                
         OC    OLDUNV(LENVALS),OLDUNV  TEST OLD UNVS SET YET                    
         BNZ   *+10                                                             
         MVC   OLDUNV(LENVALS),NEWUNV                                           
                                                                                
         LA    R0,NUMVALS*4                                                     
         LA    R1,NEWRTG                                                        
         ST    R1,DMCB+8                                                        
         L     R2,AIO2                                                          
         LA    R2,MBKRTG-IOAREA2(R2)                                            
         GOTO1 VSUBR01,DMCB,('MBKWGTQ',(RC)),(R0),,(R2)                         
                                                                                
         DS    0H                  EXTRACT HOME SHARES                          
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         OC    DBAQUART,DBAQUART                                                
         BZ    DEMHK9SHRX                                                       
         L     R0,=A(DEMOSHR)                                                   
         A     R0,RELO                                                          
         MVC   SVDBACBK,DBACTBK                                                 
         GOTO1 VDEMOUT,DMCB,(C'P',(R0)),DBLOCK,HOMSHR,0                         
         MVC   DBACTBK,SVDBACBK                                                 
                                                                                
         LA    R0,3                                                             
         LA    R1,HOMSHR                                                        
         ST    R1,DMCB+8                                                        
         LA    R2,MBKHMSHR                                                      
         GOTO1 VSUBR01,DMCB,('MBKWGTQ',(RC)),(R0),,(R2)                         
                                                                                
DEMHK9SHRX EQU *                                                                
*                                  EXTRACT VUTS (FOR HOMES SHARES)              
         XC    OVERDEMS(12),OVERDEMS                                            
         CLI   DBSELMED,C'N'                                                    
         BNE   DEMHK14                                                          
         MVC   DEMOLST(10),NETPUTS                                              
         GOTO1 VDEMOUT,DMCB,(C'L',DEMOLST),DBLOCK,OVERDEMS                      
         B     DEMHK18                                                          
                                                                                
DEMHK14  GOTO1 VDEMOUT,(R1),(C'L',HOMEVUT),DBLOCK,OVERDEMS                      
         OC    OVERDEMS(4),OVERDEMS   ANY VUTS (RTG/SHR)                        
         BNZ   DEMHK18                                                          
         GOTO1 (RF),(R1),(C'L',HOMEPUT)  NO - USE THE PUT                       
                                                                                
                                                                                
DEMHK18  LA    R0,3                WEIGHT & ACCUMULATE VUTS                     
         LA    R1,OVERDEMS                                                      
         ST    R1,DMCB+8                                                        
         LA    R2,MBKHVUTS         EXPANDED OUTPUT AREA                         
         GOTO1 VSUBR01,DMCB,('MBKWGTQ',(RC)),(R0),,(R2)                         
*                                                                               
* RECALCULATE HOME SHARES FOR WIRED BOOK                                        
*                                                                               
         CLI   DBSELMED,C'T'       NSI USTV                                     
         BE    DEMHKAA             ALWAYS RELOOK HOMESHARE                      
         CLI   DBSELSRC,C'F'                                                    
         BE    DEMHKAA                                                          
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   DEMHKAD                                                          
         CLI   DBSELSRC,C'N'                                                    
         BNE   DEMHKAD                                                          
*                                                                               
DEMHKAA  LHI   R0,3                                                             
         LA    R3,MBKHMSHR                                                      
         L     RF,AIO2                                                          
         LA    R1,MBKHPT-IOAREA2(RF)                                            
         AHI   R1,(DISPHOM*8)                                                   
         LA    R6,MBKRTG-IOAREA2(RF)                                            
         AHI   R6,(DISPHOM*8)                                                   
DEMHKAB  CLC   0(8,R1),=X'000000000000000C'                                     
         BE    DEMHKAC                                                          
*&&DO                                                                           
         XC    WORK,WORK                                                        
         MVC   DUB2(8),0(R6)                                                    
         MVC   WORK(3),=X'02000C'     *2000                                     
         MP    DUB2(8),WORK(3)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB2                                                   
         XC    DUB2,DUB2                                                        
         MVC   DUB2(8),0(R1)                                                    
         DP    WORK(16),DUB2(8)                                                 
         MVC   DUB2(8),WORK                                                     
*&&                                                                             
** NEW CODE                                                                     
         XC    WORK,WORK                                                        
         XC    DUB2,DUB2                                                        
         MVC   WORK+8(8),0(R6)                                                  
         MVC   DUB2(3),=X'02000C'     *2000                                     
         MP    WORK(16),DUB2(3)                                                 
         XC    DUB2,DUB2                                                        
         MVC   DUB2(8),0(R1)                                                    
         DP    WORK(16),DUB2(8)                                                 
         MVC   DUB2(8),WORK                                                     
**                                                                              
         CVB   RF,DUB2                                                          
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         SR    RE,RE                                                            
****     MH    RF,DBDIVSOR                                                      
         ZICM  R2,DBDIVSOR,(3)                                                  
         AH    R2,SVDIVSOR            PREVIOUS DIVSOR                           
         MR    RE,R2                                                            
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         MVC   0(8,R3),DUB2                                                     
DEMHKAC  AHI   R6,8                   BUMP-HUT                                  
         AHI   R1,8                   BUMP-DMA IMP                              
         AHI   R3,8                   BUMP-MBKHMSHR                             
         BCT   R0,DEMHKAB                                                       
DEMHKAD  DS    0C                                                               
*                                                                               
                                                                                
DEMHKX   L     RE,SAVERE           RETURN TO DEMAND FOR NEXT RECORD             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DO UPGRADES                                             *         
***********************************************************************         
         SPACE 1                                                                
UPGRADE  NTR1  ,                                                                
         MVI   PUTUPGD,NO                                                       
         L     R1,SPUPAREC                                                      
         LA    RE,DRFRSTEL-DRKEY(R1)                                            
         ST    RE,AFRSTEL                                                       
         MVC   FILEVALS,TPTVALS                                                 
                                                                                
         CLC   SPUPFIL,TPTVALS                                                  
         BE    *+10                                                             
         MVC   FILEVALS,PAVVALS                                                 
                                                                                
UPG06    LA    R7,DBLOCK2                                                       
         XC    DBLOCK,DBLOCK       EXPLODE RECORD INTO IUNREC                   
         MVC   DBFILE,IUNVALS+1    BUILD DBLOCK FOR LOOKUP/EXPLODE              
         MVC   DBAREC,SPUPAREC                                                  
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBTAPEP,TAPEOPT                                                  
                                                                                
         MVI   HPT,OLD             SET TO GET OLD H/P/T LINE                    
         LA    R1,BOOK                                                          
                                                                                
                                                                                
         CLI   SPUPTYPE,X'0F'      DMA=I                                        
         BE    *+8                 MULTIBOOK DONT GET NEW HPT                   
         CLI   SPUPTYPE,X'04'      DMA=I                                        
         BNE   UPG10               MULTIBOOK DONT GET NEW HPT                   
         CLI   MBKIFLAG,C'Y'                                                    
         BE    UPG18                                                            
                                                                                
UPG10    OC    UOQHOMES,UOQHOMES   ONLY IF QHOMES NOT PRESENT                   
         BNZ   *+8                                                              
         BAS   RE,GETHPT           GET OLD H/P/T VALUES                         
UPG18    MVI   HPT,NEW             SET TO GET NEW H/P/T VALUES                  
UPG19    B     UPRTG                                                            
         EJECT                                                                  
UPRTG    CLI   SPUPTYPE,SPUPTRTG                                                
         BNE   UPHPT                                                            
         OC    SPUPFLD2,SPUPFLD2   TEST IF SHARE PRESENT                        
         BZ    *+14                                                             
         CLC   SPUPFLD2,BOOK       TEST SHARE VALUE PRESENT                     
         BL    UPRTGSHR            HIGH IS A BOOK -- NOT A SHARE                
         SPACE 1                                                                
***********************************************************************         
* RATING ONLY UPGRADE                                                 *         
* ADJRTG = RTGINX * OLDRTG                                            *         
* OUTPUT = OLDRTG / OLDHPT / OLDRTG / NEWHPT                          *         
* OVERRIDE RATING - CALCULATE SHARE OVERRIDE                          *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 GETHPT,SPUPFLD1                                                  
*                                                                               
         SR    R1,R1               CREATE HOMES OVERRIDE                        
         ICM   R1,3,SPUPFLD1       GET NEW RHOMES                               
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,UORHOMES         COMPUTE HOMES INDEX                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPRTG2                                                           
                                                                                
         LR    RF,R1               R1 CONTAINS NEW RHOMES PERCENT               
         SR    RE,RE                                                            
         M     RE,UOUHOMES                                                      
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         LR    R1,RF                                                            
***UPRTG2   BAS   RE,GETINDEX                                                   
UPRTG2   ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
***      GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
* CALL NEW GET INDEX ROUTINE WHICH USES PACK INSTRUCTIONS                       
         GOTO1 VSUBR01,DMCB,('GETIDX2Q',(RC))                                   
*                                                                               
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
*                                                                               
         SR    R1,R1               CALCULATE SHARE OVERRIDE                     
         ICM   R1,3,SPUPFLD1       GET NEW RATING HOMES                         
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALING FOR DIVISION TO DERIVE SHARE         
         L     R0,UOPHOMES         OLD HUT                                      
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPRTG3                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
UPRTG3   BAS   RE,GETVALUE                                                      
         MVC   DUB(2),SHOMES                                                    
         MH    R1,=H'10'           SCALING FOR OVERRIDE ELEMENT                 
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER                                                       
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         MVI   INDEXUPG,C'Y'                                                    
         B     UPEND                                                            
         EJECT                                                                  
***********************************************************************         
* RATING/SHARE UPGRADE                                                *         
* ADJRTG = RTGINX * OLDRTG                                            *         
* OUTPUT = ADJRTG / ADJHPT / ADJRTG / NEWHPT                          *         
* OVERRIDE RATING/SHARE - CALCULATE HUT OVERRIDE                      *         
***********************************************************************         
         SPACE 1                                                                
UPRTGSHR GOTO1 GETHPT,SPUPFLD1                                                  
*                                                                               
         SR    R1,R1               CREATE HOMES OVERRIDE                        
         ICM   R1,3,SPUPFLD1       GET NEW RHOMES                               
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,UORHOMES         COMPUTE HOMES INDEX                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPRTSH2                                                          
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
**UPRTSH2  BAS   RE,GETINDEX                                                    
UPRTSH2  ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
*                                                                               
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
*                                                                               
         SR    R1,R1               CREATE SHARE OVERRIDE                        
         ICM   R1,3,SPUPFLD2       GET NEW SHARE                                
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         SR    R1,R1               CALCULATE NEW HUT OVERRIDE                   
         ICM   R1,3,SPUPFLD1       NEW RATING                                   
         CLI   PRECUPG,C'Y'                                                     
         BNE   *+12                                                             
         MH    R1,=H'10'                                                        
         B     *+8                                                              
         MH    R1,=H'100'                                                       
         LH    R0,DUB+2            NEW SHARE                                    
         BAS   RE,GETVALUE                                                      
         MVC   DUB(2),PHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
* ADJUST THE PUTS/TOTS                                                          
         LH    R1,DUB+2            NEW HUT                                      
         MVC   DUB(4),INDEX        SAVE CURRENT INDEX                           
         SR    R0,R0                                                            
         CLI   TAPEOPT,C'Y'        IMP BASED                                    
         BNE   UPRTSH3                                                          
         L     R1,UOPHOMES         GETS ADJUSTED BY UNIVERSE                    
         MH    R1,=H'100'                                                       
         D     R0,UOUHOMES                                                      
         SR    R0,R0                                                            
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         LR    R0,R1               SLOT PROPERLY                                
         L     R1,DUB+4            RESTORE NEW                                  
         B     *+8                                                              
UPRTSH3  L     R0,UOPHOMES                                                      
*        BAS   RE,GETINDEX                                                      
         ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDHPT),0,NEWHPT                                 
         GOTO1 CALC,DMCB,((R0),OLDTOT),0,NEWTOT                                 
         MVC   INDEX(4),DUB                                                     
         MVI   INDEXUPG,C'Y'                                                    
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
UPHPT    CLI   SPUPTYPE,SPUPTHPT   TEST HPT UPGRADE                             
         BE    UPHPT2                                                           
         CLI   SPUPTYPE,SPUPTHUT                                                
         BNE   UPNDX                                                            
         CLI   SPUPSTYP,C'P'       TEST PUT UPGRADE                             
         BNE   UPHUT                                                            
         SPACE 1                                                                
***********************************************************************         
* PUT UPGRADE                                                         *         
* NEWHPT = BOOK VALUES                                                *         
* ADJRTG = OLDSHR * NEWHPT                                            *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
***********************************************************************         
         SPACE 1                                                                
UPHPT2   GOTO1 GETHPT,SPUPFLD1                                                  
*                                                                               
         CLI   SPUPTYPE,SPUPTHPT   TEST HPT UPGRADE                             
         BNE   UPPUT                                                            
*                                                                               
         SR    R0,R0               INDEX HPTS BY VALUE IN RAVLNOP3              
         ICM   R0,3,SPUPFLD3                                                    
         CLI   PRECUPG,C'Y'                                                     
         BNE   *+12                                                             
         MH    R0,=H'10'                                                        
         B     *+8                                                              
         MH    R0,=H'100'          SCALE TO 2 DEC PLACES                        
         ST    R0,INDEX                                                         
         LA    R0,NUMVALS*2                                                     
         GOTO1 CALC,DMCB,((R0),NEWHPT),0,NEWHPT                                 
*                                                                               
UPPUT    BAS   RE,NORMU                                                         
*                                                                               
UPPUTXX  LA    R0,NUMVALS*2        CALCULATE NEW RATINGS                        
*                                                                               
         CLC   SPUPFLD2,BOOK       IF IT'S A BOOK IGNORE                        
         BL    *+10                THAT'S WHAT HAPPENED WITH ANY VALUE          
         XC    SPUPFLD2,SPUPFLD2   BEFORE CODE BELOW                            
*                                                                               
         OC    SPUPFLD2,SPUPFLD2   ANY NEW SHARE                                
         BZ    UPPSH8              NO - ADJUST RATINGS/IMPS ONLY                
*                                                                               
         OC    HOMSHR,HOMSHR       ANY OLD SHARE                                
         BZ    UPPSH8              NO - ADJUST RATINGS/IMPS ONLY                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,SPUPFLD2       GET NEW SHARE                                
         SR    RE,RE                                                            
         M     RE,=F'2000'                                                      
         D     RE,HOMSHR           INDEX TO OLD SHARE                           
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         LR    R1,RF                                                            
         LA    R0,NUMVALS*2        INDEX ALL OLD SHARES                         
         LA    R4,OLDSHR                                                        
UPPSH    SR    RE,RE                                                            
         L     RF,0(R4)            OLD SHARE                                    
         SLDA  RE,1                                                             
         MR    RE,R1               TIMES INDEX                                  
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R4)            EQUAL NEW SHARE                              
         LA    R4,4(R4)                                                         
         BCT   R0,UPPSH                                                         
         LA    R0,3                INDEX ALL OLD HOMES SHARES                   
         LA    R4,HOMSHR                                                        
UPPSH2   SR    RE,RE                                                            
         L     RF,0(R4)            OLD SHARE                                    
         SLDA  RE,1                                                             
         MR    RE,R1               TIMES INDEX                                  
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         ST    RF,0(R4)            EQUAL NEW SHARE                              
         LA    R4,4(R4)                                                         
         BCT   R0,UPPSH2                                                        
*                                                                               
         ICM   RF,3,SPUPFLD2       NOW REPLACE OVERRIDEN SHARE                  
         ST    RF,HOMSHR                                                        
         LA    R0,NUMVALS*2        RESET FOR CALCS                              
*                                                                               
UPPSH8   DS    0H                                                               
         GOTO1 CALC,DMCB,((R0),NEWHPT),OLDSHR,NEWRTG                            
         XC    INDEX,INDEX                                                      
         MVI   PUTUPGD,YES                                                      
         OC    SPUPFLD2,SPUPFLD2   ANY NEW SHARE                                
         BZ    UPEND               NO - EXIT                                    
*                                                                               
UPPSH9   MVC   OLDRTG(NUMVALS*4*2),NEWRTG    UPDATE THE SHARES                  
         MVC   OLDHPT(NUMVALS*4*2),NEWHPT                                       
*                                                                               
         B     UPEND                                                            
         EJECT                                                                  
UPHUT    OC    SPUPFLD2,SPUPFLD2   TEST IF SHARE PRESENT                        
         BZ    *+14                                                             
         CLC   SPUPFLD2,BOOK       TEST SHARE GIVEN                             
         BL    UPHUTSHR            YES                                          
         SPACE 1                                                                
***********************************************************************         
* HUT ONLY UPGRADE                                                    *         
* NEWRTG(HMS) = OLDSHR * NEWHUT                                       *         
* ADJRTG      = RTGINX * OLDRTG                                       *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / OLDHPT                          *         
* OVERRIDE HUT - CALCULATE RATING OVERRIDE                            *         
***********************************************************************         
         SPACE 1                                                                
         SR    R1,R1                                                            
         ICM   R1,3,SPUPFLD1       GET HUT INDEX                                
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         CLC   SPUPFLD1,BOOK       TEST HUT = INDEX OR BOOK                     
         BL    UPHUT2                                                           
         GOTO1 GETHPT,SPUPFLD1     BOOK - GET NEW HUT VALUE                     
*                                                                               
         BAS   RE,NORMU                                                         
*                                                                               
         L     R1,UNPHOMES                                                      
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPHUT2                                                           
         SR    R0,R0                                                            
         MH    R1,=H'100'                                                       
*&&DO                                                                           
*------------------------------     THIS CODE GETS THE NEW HUT                  
         D     R0,UOUHOMES    |     VALUE AND DOES THE CALCULATION              
         AH    R1,=H'5'       |     TO PRODUCE PUTS TO EQUAL TO                 
         SR    R0,R0          |     PUTS PRODUCED BY PUT UPGRADE                
         D     R0,=F'10'      |                                                 
*------------------------------                                                 
*&&                                                                             
         L     RE,UOUHOMES          THIS CODE GETS THE NEW HUT                  
         AHI   RE,5                 VALUE AND DOES THE CALCULATION              
         DR    R0,RE                TO PRODUCE PUTS TO EQUAL TO                 
         AH    R1,=H'5'             PUTS PRODUCED BY PUT UPGRADE                
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
UPHUT2   MVC   DUB(2),PHOMES       CREATE OVERRIDE ELEMENT                      
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  CALCULATE NEW RTGHMS AND OVRD ELEM           
         L     R0,HOMSHR           GET OLD HOMES SHARE                          
         MR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DUB+2            STORE NEW RATING HOMES                       
         MVC   DUB(2),RHOMES                                                    
*        BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,UORHOMES         GET OLD RATING HOMES                         
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPHUT3                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
*---------------------------                                                    
*****    D     RE,UOUHOMES  |      ROUND UP ALSO                                
*---------------------------                                                    
         L     R3,UOUHOMES         ROUND UP TO GET THE SAME RESULTS AS          
         AHI   R3,5                PUT UPGRADES                                 
         SR    R2,R2                                                            
         DR    RE,R3                                                            
*                                                                               
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
***UPHUT3   BAS   RE,GETINDEX                                                   
UPHUT3   ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
*                                                                               
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         GOTO1 CALC,DMCB,((R0),OLDHPT),0,NEWHPT                                 
         GOTO1 CALC,DMCB,((R0),OLDTOT),0,NEWTOT                                 
         MVI   INDEXUPG,C'Y'                                                    
         CLI   TAPEOPT,C'Y'             IF IMP-BASED, KEEP                      
         BE    *+10                      ORIG HOME SHRS FOR HUT UPGD            
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
UPHUTSHR OC    SPUPFLD1,SPUPFLD1   TEST HUT PRESENT                             
         BZ    UPSHR                                                            
         SPACE 1                                                                
***********************************************************************         
* HUT/SHARE UPGRADE                                                   *         
* NEWRTG = NEWHUT * NEWSHR(HMS)                                       *         
* ADJRTG = RTGINX * OLDRTG                                            *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
* OVERRIDE HUT/SHARE - CALCULATE RATING OVERRIDE                      *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 GETHPT,SPUPFLD1                                                  
         BAS   RE,NORMU                                                         
*                                                                               
         SR    R1,R1               CREATE HUT OVERRIDE ELEMENT                  
         ICM   R1,3,SPUPFLD1       GET HUT INDEX                                
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         CLC   SPUPFLD1,BOOK       TEST HUT = INDEX OR BOOK                     
         BL    UPHUT7                                                           
         L     R1,UNPHOMES         BOOK - GET NEW HUT VALUE                     
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPHUT7                                                           
         SR    R0,R0                                                            
         MH    R1,=H'100'                                                       
*---------------------------------                                              
*&&DO                                                                           
         D     R0,UOUHOMES       |                                              
         AH    R1,=H'5'          |  CHUNK OF CODE REPLACED                      
         SR    R0,R0             |  BECAUSE IT DOESNT PRODUCE                   
         D     R0,=F'10'         |  THE SAME PUTS AS THE PUT                    
*&&                                 UPGRADES                                    
*---------------------------------                                              
*                                                                               
         L     RE,UOUHOMES          THIS CODE GETS THE NEW HUT                  
         AHI   RE,5                 VALUE AND DOES THE CALCULATION              
         DR    R0,RE                TO PRODUCE PUTS TO EQUAL TO                 
         AH    R1,=H'5'             PUTS PRODUCED BY PUT UPGRADE                
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
UPHUT7   MVC   DUB(2),PHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
         ST    R1,DUB+4            SAVE NEW HUT                                 
*                                                                               
         SR    R1,R1               CREATE SHARE OVERRIDE ELEMENT                
         ICM   R1,3,SPUPFLD2       GET NEW SHARE                                
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,DUB+4            RETRIEVE NEW HUT                             
         MR    R0,R0               COMPUTE NEW RATING HOMES                     
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,UORHOMES                                                      
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPHUT8                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
**UPHUT8   BAS   RE,GETINDEX                                                    
UPHUT8   ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
* ADJUST THE PUTS/TOTS                                                          
         MVC   DUB(4),INDEX        SAVE CURRENT INDEX                           
         SR    R0,R0                                                            
         L     R1,DUB+4            NEW HUT                                      
         CLI   TAPEOPT,C'Y'        IMP BASED                                    
         BNE   UPHUT9                                                           
         L     R1,UOPHOMES         GETS ADJUSTED BY UNIVERSE                    
         MH    R1,=H'100'                                                       
******   D     R0,UOUHOMES                                                      
         L     RF,UOUHOMES                                                      
         SR    RE,RE                                                            
         AHI   RF,5                                                             
         DR    R0,RF                                                            
*                                                                               
         SR    R0,R0                                                            
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         LR    R0,R1               SLOT PROPERLY                                
         L     R1,DUB+4            RESTORE NEW                                  
         B     *+8                                                              
UPHUT9   L     R0,UOPHOMES                                                      
**       BAS   RE,GETINDEX                                                      
         ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDHPT),0,NEWHPT                                 
         GOTO1 CALC,DMCB,((R0),OLDTOT),0,NEWTOT                                 
         MVC   INDEX(4),DUB                                                     
         MVI   INDEXUPG,C'Y'                                                    
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
***********************************************************************         
* SHARE ONLY UPGRADE                                                  *         
* NEWRTG = NEWSHR * OLDHUT                                            *         
* ADJRTG = RTGINX * OLDRTGS                                           *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
* OVERRIDE SHARE - CALCULATE RATING OVERRIDE                          *         
***********************************************************************         
         SPACE 1                                                                
UPSHR    GOTO1 GETHPT,SPUPFLD1                                                  
*                                                                               
         SR    R1,R1               CREATE SHARE OVERRIDE                        
         ICM   R1,3,SPUPFLD2       GET NEW SHARE                                
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  CALCULATE RATING HOMES OVERRIDE              
         L     R0,UOPHOMES         OLD HUT                                      
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPSHR2                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
UPSHR2   MR    R0,R0               DEVELOP NEW RATING HOMES                     
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2            NEW RATING HOMES                             
         BAS   RE,ADDOVER                                                       
*                                                                               
         L     R0,UORHOMES         OLD RATING                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPSHR4                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
***UPSHR4   BAS   RE,GETINDEX         RATING INDEX                              
UPSHR4   ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVI   INDEXUPG,C'Y'                                                    
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
UPNDX    CLI   SPUPTYPE,X'0F'          WEEKLY MULITBOOK AVG - NOTE              
         BE    *+8                     WE USE X'0F' INSTEAD OF SPUPTNDX         
         CLI   SPUPTYPE,X'0A'          NEW IMS TYPE                             
         BE    *+8                                                              
         CLI   SPUPTYPE,SPUPTNDX       BECAUSE A LOT OF PROGRAMS DONT           
         BNE   UPDEM                   ACCOUNT FOR SPUPEXTEND                   
         SPACE 1                                                                
***********************************************************************         
* INDEX UPGRADE                                                       *         
* ADJRTG = INDEX * OLDRTG                                             *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
* CALCULATE SHARE OVERRIDE                                            *         
***********************************************************************         
         SPACE 1                                                                
         CLI   MBKIFLAG,C'Y'       MULTIBOOK AVERAGE IMPRESSIONS                
         BE    UPNDX1              BASED DONT GET HPT AGAIN                     
         GOTO1 GETHPT,SPUPFLD1                                                  
UPNDX1   CLC   SPUPFLD1,=H'100'    TEST INDEX=100                               
         BNE   UPNDX2                                                           
         MVC   NEWRTG(LENVALS*2),OLDRTG                                         
         XC    INDEX,INDEX                                                      
         B     UPEND                                                            
*                                                                               
UPNDX2   SR    R1,R1                                                            
         ICM   R1,3,SPUPFLD1       GET INDEX VALUE                              
         MH    R1,=H'100'          INDEX VALUE MUST BE TO 2 DEC. PLCS.          
         ST    R1,INDEX                                                         
*                                                                               
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVI   INDEXUPG,C'Y'                                                    
*                                                                               
         L     R1,UNRHOMES         NEW RATING HOMES                             
         MH    R1,=H'10'           SCALING                                      
         L     R0,UOPHOMES         OLD HUT                                      
         BAS   RE,GETVALUE         CALCULATE NEW HOMES SHARE                    
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER                                                       
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
UPDEM    CLI   SPUPTYPE,X'09'                                                   
         BE    UPMMU                                                            
         CLI   SPUPTYPE,X'0B'                                                   
         BE    UPVIX                                                            
         CLI   SPUPTYPE,X'0C'                                                   
         BE    UPVIX                                                            
         CLI   SPUPTYPE,X'0D'                                                   
         BE    UPVAX                                                            
         CLI   SPUPTYPE,X'0E'                                                   
         BE    UPVAX                                                            
         CLI   SPUPTYPE,C'A'                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   SPUPTYPE,C'P'       TEST FOR PUT                                 
         BNE   UPDEM2              NO                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SPUPFLD1       R0=PUT                                       
         SR    R1,R1                                                            
         ICM   R1,3,SPUPFLD2       R1=SHARE                                     
         MR    R0,R0               RATING=PUT*SHARE                             
         CLI   PRECUPG,C'Y'                                                     
         BE    *+16                                                             
         AH    R1,=H'50'                                                        
         D     R0,=F'100'          SCALE THE RATING                             
         B     *+12                                                             
         AH    R1,=F'500'                                                       
         D     R0,=F'1000'         SCALE THE RATING                             
         STCM  R1,3,SPUPFLD1       PROCEED AS THOUGH RATING                     
         MVI   SPUPTYPE,C'R'       WERE INPUT FOR CELL                          
         SPACE 1                                                                
***********************************************************************         
* DEMO UPGRADE                                                        *         
* INDEX  = NEWDEMO / OLDDEMO                                          *         
* ADJRTG = INDEX * OLDRTG                                             *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
* OVERRIDE NEWDEMO - CALCULATE SHARE/HOMES RATING OVERRIDE            *         
***********************************************************************         
         SPACE 1                                                                
UPDEM2   XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR DEMAINT CALL                
         MVC   DBFILE,IUNVALS+1                                                 
         MVC   DBTAPEP,TAPEOPT                                                  
******   LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
         XC    0(50,R1),0(R1)                                                   
         LA    R0,DRFRSTEL+1-DRKEY                                              
         STCM  R0,3,DRRLEN-DRKEY(R1)                                            
         LA    R1,DRFRSTEL-DRKEY(R1)                                            
         ST    R1,DBAQUART                                                      
         MVC   DBCOMFCS,VCOMFACS                                                
         LA    R1,(UPRECX-UPREC)/4                                              
         STCM  R1,3,DBNUMVLS       SET DBNUMVLS TO MAXIMUM                      
         SLL   R1,2                                                             
         L     RF,AIO2                                                          
         MOVE  ((RF),(R1)),UPREC                                                
*                                                                               
         MVC   WORK(10),OFORMAT    BUILD DEMAINT OUTPUT FORMAT BLOCK            
         L     RE,=A(BOOK9011)                                                  
         A     RE,RELO                                                          
         CLI   TAPEOPT,C'Y'        TAPE BASED ?                                 
         BNE   *+10                                                             
****     MVC   WORK+7(2),BOOK9011  SWITCH FORMULA                               
         MVC   WORK+7(2),0(RE)     SWITCH FORMULA                               
*                                                                               
         GOTO1 VDEMAINT,DMCB,REP,DBLOCK,AIO2,WORK                               
         CLI   DBERROR,0                                                        
         BNE   UPERR                                                            
*                                  GET OLD DEMO VALUE INTO INDEX                
         GOTO1 VDEMOUT,DMCB,(C'D',SPUPTYPE-1),DBLOCK,INDEX                      
         CLI   DBERROR,0                                                        
         BNE   UPERR                                                            
*                                                                               
         GOTO1 GETHPT,BOOKZ                                                     
*                                                                               
         L     R0,INDEX            GET INDEX VALUE                              
         SR    R1,R1                                                            
         ICM   R1,3,SPUPFLD1                                                    
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'                                                        
***      BAS   RE,GETINDEX                                                      
         ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
*                                                                               
         SR    R1,R1               CREATE DEMO OVERRIDE ELEMENT                 
         ICM   R1,3,SPUPFLD1                                                    
         CLI   PRECUPG,C'Y'                                                     
         BE    *+8                                                              
         MH    R1,=H'10'                                                        
         MVC   DUB(2),SPUPTYPE                                                  
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         LA    R0,NUMVALS          CALCULATE NEW IMPRESSIONS                    
         GOTO1 CALC,DMCB,((R0),OLDIMP),0,NEWIMP                                 
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVI   INDEXUPG,C'Y'                                                    
*                                                                               
         L     R1,HOMSHR           CREATE SHARE OVERRIDE                        
         M     R0,INDEX                                                         
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER                                                       
*                                                                               
         CLC   SPUPTYPE(2),RHOMES  CREATE RATING OVERRIDE                       
         BE    UPEND               UNLESS RHOMES WAS INPUT DEMO                 
         L     RF,UOPHOMES                                                      
         CLI   TAPEOPT,C'Y'                                                     
         BNE   UPDEM3                                                           
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         D     RE,UOUHOMES                                                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
UPDEM3   MR    R0,RF                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         B     UPEND                                                            
         EJECT                                                                  
UPVIX    DS    0C                  VARIOUS PUT/SHARE INDEX TRENDS               
         MVI   Y4SSW,C'S'                                                       
         CLI   SPUPTYPE,12                                                      
         BE    *+8                                                              
         MVI   Y4SSW,C'P'                                                       
         B     UPVX                                                             
         SPACE 1                                                                
UPVAX    DS    0C                  VARIOUS PUT/SHARE AVERAGES                   
         MVI   Y4SSW,C'S'                                                       
         MVI   Y4ASW,C'Y'                                                       
         CLI   SPUPTYPE,14                                                      
         BE    *+8                                                              
         MVI   Y4SSW,C'P'                                                       
         SPACE 1                                                                
UPVX     GOTO1 GETHPT,BOOKZ                                                     
                                                                                
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         B     UPEND                                                            
*                                                                               
* METERED MARKET UPGRADE                                                        
* PUTS FROM A BOOK(UPFLD1) MULTIPLIED BY FACTOR FROM                            
* INDEX OF UPFLD2 AND THE PRIOR YEAR                                            
UPMMU    CLC   =X'9999',SPUPFLD3      DDUPVAL USED 9999 AS IDENTIFIER           
         BE    UPLPS                                                            
         GOTO1 GETHPT,SPUPFLD1                                                  
*                                                                               
*  CHECK IF THIS IS LPM UPGRADE                                                 
         CLI   SPUPFLD2,X'C1'                                                   
         BL    *+12                                                             
         CLI   SPUPFLD2,X'E9'                                                   
         BNH   UPLPM                                                            
*                                                                               
         BAS   RE,NORMU                                                         
         MVC   SVUPFLD(6),SPUPFLD1                                              
         MVI   Y4ASW,C'N'                                                       
         MVI   Y4SSW,C'P'                                                       
         MVC   SPUPFLD1,SPUPFLD2                                                
         MVC   SPUPFLD2,=H'2'                                                   
         MVI   SPUPSTYP,C'Y'                                                    
         MVC   MMUBOOK,SPUPFLD1     SET INDEX BASE BOOK                         
         MVI   MMUPASS,1            SET PASS FOR PARALLEL BOOK                  
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         MVC   SPUPFLD1(6),SVUPFLD                                              
         B     UPEND                                                            
         EJECT                                                                  
UPLPS    DS    0C                                                               
         MVI   PUTUPGD,YES                                                      
         GOTO1 VSUBR01,DMCB,('LPSIDXQ',(RC))                                    
         B     UPEND                                                            
***********************************************************************         
* LPM UPGRADE                                                                   
***********************************************************************         
UPLPM    DS    0C                                                               
UPLPM04  BAS   RE,NORMU                                                         
         LA    R0,NUMVALS*2        CALCULATE NEW RATINGS                        
UPLPM08  DS    0H                                                               
         GOTO1 CALC,DMCB,((R0),NEWHPT),OLDSHR,NEWRTG                            
         XC    INDEX,INDEX                                                      
         MVI   PUTUPGD,YES                                                      
         GOTO1 VSUBR01,DMCB,('LPMIDXQ',(RC))                                    
         B     UPEND                                                            
***********************************************************************         
* IUN RECORD VALUES HAVE NOW BEEN ADJUSTED BY UPGRADE ROUTINES.       *         
* REPLACE OLD DEMO ELEMENTS WITH IUN FORMAT DEMO ELEMENTS.            *         
* CREATE OVERRIDE ELEMENTS FOR ANY VALUE THAT WAS PASSED FROM CALLER. *         
* BOOK ELEMENT IS FORCED TO JUN/83 IF PUT UPGRADE TOOK PLACE.         *         
*=== CODE HAS BEEN MOVED DOWN IN SUBROUTINE POOL TO CREATE ROOM ======*         
***********************************************************************         
                                                                                
UPEND    GOTO1 VSUBR01,DMCB,('UPENDQ',(RC))                                     
         B     UPX                                                              
*&&DO                                                                           
***********************************************************************         
* IUN RECORD VALUES HAVE NOW BEEN ADJUSTED BY UPGRADE ROUTINES.       *         
* REPLACE OLD DEMO ELEMENTS WITH IUN FORMAT DEMO ELEMENTS.            *         
* CREATE OVERRIDE ELEMENTS FOR ANY VALUE THAT WAS PASSED FROM CALLER. *         
* BOOK ELEMENT IS FORCED TO JUN/83 IF PUT UPGRADE TOOK PLACE.         *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
UPEND    OC    SPUPSPL,SPUPSPL     CLEAR IMPS/TOTS IF SPILL                     
         BZ    UPEND4                                                           
         XC    OLDIMP(LENVALS),OLDIMP                                           
         XC    OLDTOT(LENVALS),OLDTOT                                           
         XC    NEWIMP(LENVALS),NEWIMP                                           
         XC    NEWTOT(LENVALS),NEWTOT                                           
*                                                                               
UPEND4   XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR DEMAINT CALL                
         MVC   DBFILE,PAVVALS+1                                                 
         MVC   DBAREC,SPUPAREC                                                  
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBCOMFCS,VCOMFACS                                                
         LA    R1,(UPRECX-UPREC)/4                                              
         STCM  R1,3,DBNUMVLS       SET DBNUMVLS TO MAXIMUM                      
         MVC   WORK(10),OFORMAT    BUILD DEMAINT OUTPUT FORMAT BLOCK            
                                                                                
UPEND13  CLI   INDEXUPG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK8212  YES - SET BOOK TO DEC/82                     
*                                                                               
         CLI   PUTUPGD,YES         TEST IF PUT UPGRADE DONE                     
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK8306  YES - SET BOOK TO JUN/83                     
                                                                                
         CLI   TAPEOPT,C'Y'        TAPE BASED ?                                 
         BNE   UPEND14                                                          
         MVC   WORK+7(2),BOOK9011  SWITCH FORMULA                               
                                                                                
         CLI   INDEXUPG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK9012  SWITCH TO INDEX FORMULA                      
*                                                                               
         CLI   PUTUPGD,YES         TEST IF PUT UPGRADE DONE                     
         BNE   UPEND15                                                          
         MVC   WORK+7(2),BOOK9106  YES - SET BOOK TO JUN/91                     
         B     UPEND15                                                          
*                                                                               
UPEND14  ZIC   RE,WORK+7                                                        
         ZIC   RF,SBKCTRL                                                       
         AR    RE,RF                                                            
         STC   RE,WORK+7                                                        
*                                                                               
*        UPGRADES DO A REP ON A TPT RECORD WHICH IS TOTALLY ILLEGAL             
*        THIS SECTION DEALS WITH THAT                                           
UPEND15  L     R1,DBAREC                                                        
         CLI   0(R1),C'R'          POSSIBLE TPT RECORD                          
         BNE   UPEND30             NO - LEAVE IT ALONE                          
         L     R1,DBAQUART         FORCE DEMEL TO INSERT DEMOS AT               
*                                  AT THIS QH                                   
UPEND16  CLI   0(R1),0                                                          
         BE    UPEND30                                                          
         ZIC   RE,1(R1)                                                         
         LA    RF,0(RE,R1)                                                      
         CLI   0(RF),X'5F'         60 AND ABOVE WILL STOP DEMAINT               
         BH    UPEND30             AT THE CORRECT SPOT                          
         CLC   0(1,RF),0(R1)       NEW SECTION                                  
         BL    UPEND20                                                          
         LR    R1,RF                                                            
         B     UPEND16                                                          
*                                                                               
UPEND20  CLI   0(RF),X'20'         ENSURE I HAVE ANOTHER QH                     
         BNE   UPEND30              NO-DON'T ZAP RECORD                         
UPEND24  MVI   0(RF),X'FF'         DELETE REMAINING ELEMENTS                    
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         BNE   UPEND24                                                          
*                                                                               
UPEND30  GOTO1 VDEMAINT,DMCB,REP,DBLOCK,UPREC,WORK                              
         CLI   DBERROR,0                                                        
         BNE   UPERR                                                            
*                                                                               
         MVC   DUB(2),NDXDEMO      ADD INDEX VALUE OVERRIDE ELEMENT             
         MVC   DUB+2(2),INDEX+2                                                 
         OC    INDEX,INDEX                                                      
         BZ    *+8                                                              
         BAS   RE,ADDOVER                                                       
*                                                                               
         SR    R0,R0               CREATE DEMO OVERRIDE ELEMENTS                
         ICM   R0,1,OVERLIST       R0=NUMBER OF OVERRIDE VALUES                 
         BZ    UPX                                                              
         LA    R2,OVERLIST+1       R2=A(OVERRIDE LIST)                          
         MVI   WORK+0,OVERELEM     DELETE OVERRIDE ELEMENTS                     
         MVI   WORK+1,6                                                         
*                                                                               
UPEND34  MVC   WORK+2(4),0(R2)     ADD OVERRIDE ELEMENTS                        
         GOTO1 VHELLO,DMCB,(C'P',DEMFILE),SPUPAREC,WORK,0                       
         LA    R2,4(R2)                                                         
         BCT   R0,UPEND34                                                       
         B     UPX                                                              
         EJECT                                                                  
*&&                                                                             
                                                                                
NORMU    NTR1                                                                   
         GOTO1 VSUBR01,DMCB,('NORMUQ',(RC))                                     
         J     XIT                                                              
NORMIMP  NTR1                                                                   
         GOTO1 VSUBR01,DMCB,('NORMIMPQ',(RC))                                   
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OLD OR NEW H/P/T VALUES FROM TIME PERIOD FILE        *         
* ON ENTRY HPT=OLD FOR OLD H/P/T OR NEW FOR NEW H/P/T WITH            *         
* R1=A(BOOK OPTION)                                                   *         
*                                                                     *         
* EXIT WITH H/P/T VALUES AT OLDHPT OR NEWHPT                          *         
***********************************************************************         
         SPACE 1                                                                
GETHPT   NTR1  WORK=(R8,GETHPTLN)                                               
         USING GETHPTWK,R8         R8=A(LOCAL W/S)                              
         CLI   DBSELSRC,C'F'       EXIT IF FUSION                               
         BE    GETHPTHX                                                         
         TM    SPUPOPTS,SPOPEXT                                                 
         BZ    GETHPT0             EXTEND BLOCK NOT IN USE --NO IMS             
         ICM   RE,15,SPUPEXTN      IN MKT SHARE REQUEST?                        
GETHPIMS LTR   RE,RE                                                            
         BZ    GETHPT0             NO, REGULAR HUTS/PUT HANDLING                
         USING SPIMSD,RE                                                        
*                                                                               
         MVI   IMSFLAG,C'A'        SET FLAG TO 'A' = AFFILS (DEFAULT)           
         CLC   SPIMSID,=C'ISS '    IN STA SHARE REQUESTED?                      
         BNE   *+12                                                             
         MVI   IMSFLAG,C'S'        SET FLAG TO 'S' = STATIONS                   
         B     *+10                                                             
*                                                                               
         CLC   SPIMSID,=C'IMS '    IN MKT SHARE REQUESTED?                      
         BE    *+12                                                             
         L     RE,SPIMSNXT         BUMP TO NEXT EXTN BLOCK                      
**       B     *-20                                                             
         B     GETHPIMS                                                         
*                                                                               
         LA    RE,SPIMSAFF                                                      
         ST    RE,STNAFFL          SAVE ADDR OF STATION AFFL LIST               
         DROP  RE                                                               
         LR    R2,R1               PASS GETHPT PARM LIST        Z               
****     GOTO1 VSUBR01,DMCB,('STNHPTS',(RC)),(R2)                               
         GOTO1 VSUBR02,DMCB,('STNHPTS',(RC)),(R2)                               
         CLI   DBERROR,0           IF NO ERRORS, EXIT ELSE DEFAULT              
         BE    UPX                  TO REG LK UP W/OUT AN IMS                   
         LR    R1,R2               RESTORE PARM LIST                            
*                                                                               
GETHPT0  MVI   GETHIND,0           SET INDICATOR BYTE                           
         CLI   HPT,OLD             CLEAR REQUIRED H/P/T VALUES                  
         BNE   *+14                                                             
         XC    OLDHPT(LENVALS*2),OLDHPT                                         
         B     *+10                                                             
         XC    NEWHPT(LENVALS*2),NEWHPT                                         
*                                                                               
GETHPT1  XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         MVC   DBFILE,TPTVALS+1                                                 
*                                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION TO GET MARKET               
         CLC   0(2,R1),BOOK        TEST IF BOOK PASSED                          
         BNH   *+10                                                             
         MVC   DBSELBK,0(R1)       YES - SET SELECTED BOOK                      
         NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
*****    LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
         MVC   DBCOMFCS,VCOMFACS   **MUST BE DONE FOR MKT OVERRIDES**           
         MVC   DBSELAGY,SPUPAGY    RESTORE AGENCY                               
         MVC   DBSELUMK,SPUPMKT    SET AGENCY MKT CODE                          
         MVC   DBSELCLI,SPUPCLI    SET AGENCY CLIENT CODE                       
         MVC   DBSELMED,SPUPMED                                                 
         TM    SPUPOPTS,SPOPEXT                                                 
*&&DO                                                                           
         BZ    *+10                EXTEND BLOCK NOT IN USE                      
         MVC   DBEXTEND,SPUPEXTN    IN USE - SET IT                             
*&&                                                                             
         BZ    *+8                 EXTEND BLOCK NOT IN USE                      
         BAS   RE,INXTND            IN USE - INSERT IT                          
         TM    SPUPOPTS,SPOANGFR   CANADIAN ANGLO/FRANCO OPTION                 
         BZ    *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
         TM    SPUPOPT2,SPO2CMON   CANADIAN WEEKLIES AS MONTH                   
         BZ    *+8                                                              
         MVI   DBBEST,C'M'                                                      
         TM    SPUPOPT2,SPO2UBBM   CANADIAN USE BBM WEEKLIES                    
         BZ    *+8                                                              
         MVI   DBUSEBBM,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTY   SPORTS ONLY OPTION                           
         BZ    *+8                                                              
         MVI   DBSELSPO,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTN   EXCLUDE SPORTS OPTION                        
         BZ    *+8                                                              
         MVI   DBSELSPO,C'N'                                                    
*                                                                               
         OC    SPUPPUR,SPUPPUR     TEST DAY/TIME LOOK-UP                        
         BNZ   GETHPT2                                                          
         LA    R1,DBLOCK1          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DUB(2),SPUPFBK                                                   
         NI    DUB+1,X'0F'         AND OFF WEEK BITS                            
         B     GETHPT4                                                          
*                                                                               
GETHPT2  L     R1,SPUPAREC                                                      
         MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
         MVC   DBSELSRC,PRSRC-PRKEY(R1)                                         
         CLI   DBSELMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   DBSELSRC,SPUPSRC                                                 
         MVC   DBBTYPE,PRBTYP-PRKEY(R1)                                         
         MVC   DUB(2),PRBOOK-PRKEY(R1)                                          
*                                                                               
GETHPT4  MVC   SVBTYPE,DBBTYPE     SAVE INITIAL BOOK TYPE                       
         CLI   DBBTYPE,C'A'        PARENT ONLY OF P+S COMBO                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'O'        OLYMPIC BOOK                                 
         BNE   *+8                                                              
         CLI   HPT,NEW             AND NEW HPT                                  
         BNE   *+8                                                              
         MVI   DBBTYPE,0           KILL BOOK TYPE                               
*                                                                               
* FOR WIRED USTV NSI - GRAB NEW HPT FROM STANDARD                               
         CLI   DBSELMED,C'T'                                                    
         BNE   *+8                                                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   *+8                                                              
         CLI   HPT,NEW                                                          
         BNE   *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,C'4'        WIRED ZERO CELL GRAB FROM STANDARD           
         BNE   *+8                 ZERO CELL                                    
         MVI   DBBTYPE,C'1'                                                     
*                                                                               
         CLI   DBBTYPE,C'U'        CABLE LIVE ONLY BOOKS GO TO                  
         BE    *+8                 STANDARD LIVE ONLY                           
         CLI   DBBTYPE,C'Z'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'L'                                                     
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_C3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_L3                                              
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_LS                                              
*                                                                               
* LIVE+1 BOOKTYPES LOOK UP LIVE+7 (STANDARD PUTS)                               
*&&DO                                                                           
* WE DONT KNOW WHAT TO DO WITH IMPACT BOOKTYPES.                                
         CLI   DBBTYPE,BOOKTYPE_Y1   IMPACT LIVE+1                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_YK   IMPACT DMA CABLE L+1                       
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_YM   IMPACT WIRED CABLE L+1                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_YN   IMPACT HISPANIC LIVE+1                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
*&&                                                                             
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_L1   STANDARD LIVE+1                            
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_C1   CABLE LIVE+1                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W1    WIRED LIVE+1                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
*                                                                               
         CLI   DBBTYPE,BOOKTYPE_H1   HISPANIC CABLE LIVE+1                      
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_H                                               
         CLI   DBBTYPE,BOOKTYPE_O1   OLYMPIC LIVE+1                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_O                                               
*                                                                               
*                                                                               
         OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         BNZ   *+10                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
         LA    R0,2                                                             
         TM    GETHIND,X'80'       DON'T BACK-UP IF 2ND PUT LOOK-UP             
         BZ    *+8                                                              
         LA    R0,1                                                             
         OC    SPUPSPL,SPUPSPL     TEST SPILL MARKET GIVEN                      
         BZ    *+14                                                             
         MVC   DBSELRMK,SPUPSPL    YES - GET H/P/T'S FROM HERE                  
         B     GETHPT10                                                         
         OC    DBSELRMK,MARKET     TEST MARKET NUMBER KNOWN                     
         BNZ   GETHPT10            YES - GO GET DEMOS                           
*                                                                               
GETHPT6  GOTO1 VDEMAND,DMCB,DBLOCK,0 VALIDATE STATION/BOOK                      
         CLI   DBERROR,0           TEST FOR ERRORS                              
         BNE   *+14                NO - EXTRACT MARKET NUMBER                   
         MVC   DBSELRMK,DBKEY+(BSRMKT-BSKEY)                                    
         B     GETHPT10                                                         
         CLI   HPT,OLD             TEST OLD H/P/T LINE REQUIRED                 
         BE    UPX                                                              
         CLI   DBERROR,X'10'       TEST FOR NOT FOUND                           
         BNE   GETHPT8                                                          
         BCT   R0,*+8              YES - TEST EST BOOK PROCESSED                
         B     GETHPT8                                                          
         ZIC   R1,DBSELBK          NO - DECREMENT BOOK & TRY AGAIN              
         BCTR  R1,0                                                             
         STC   R1,DBSELBK                                                       
         B     GETHPT6                                                          
*                                                                               
GETHPT8  MVI   DBFUNCT,DBVLST      STA/BOOK NOT FOUND TRY STATION ONLY          
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0           TEST FOR ERRORS                              
         BNE   GETHPT20                                                         
         MVC   DBSELRMK,DBACTRMK   NO - EXTRACT MARKET NUMBER                   
         MVC   DBSELBK,DUB         RESTORE SELECTED BOOK VALUE                  
*                                                                               
*ETHPT10 MVI   DBFUNCT,DBGETTTL   (DEFAULT CHANGED) <----DELETED---             
*        CLI   DBSELSRC,C'N'       TEST FOR NSI     <--------------             
*        BE    *+8                                  <--------------             
GETHPT10 MVI   DBFUNCT,DBGETTOT                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'WWWB' FORCE TO CHARLOTTE                          
         BNE *+10                                                               
         MVC   DBACTRMK,=H'117'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'KAKW' FORCE TO AUSTIN                             
         BNE *+10                                                               
         MVC   DBACTRMK,=H'235'                                                 
*                                                                               
         CLI   PUREFLAG,YES        SET DAY/TIME FOR LOOK-UP                     
         BE    GETHPT12                                                         
         MVC   DBSELDAY,SPUPDAY    SET DAY/TIME FROM UPGRADE BLOCK              
         MVC   DBSELTIM,SPUPTIM                                                 
         CLI   HPT,NEW                                                          
         BE    GETHPT14                                                         
         LA    R1,DBLOCK1          SET DAY/TIME FROM DBLOCK1                    
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         B     GETHPT14                                                         
*                                                                               
GETHPT12 MVC   DUB+0(1),PUREDW     EXTRACT DAY/TIME FROM PAV RECORD             
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))  PUT  DAY IN DEMAND FMT            
*        BAS   RE,GETDAY2          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB+0(1),PURESTIM                                                
         MVC   DUB+1(1),PUREDUR                                                 
         GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))  PUT  DAY IN DEMAND FMT            
*        BAS   RE,GETIME2          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
         B     GETHPT16                                                         
*                                                                               
GETHPT14 DS    0H                                                               
         L     RF,=A(CHKLPM)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         L     RF,=A(CHKLIVEM)     CHECK LIVE MONTHLY BOOK                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*&&DO                                                                           
         XC    UPLPMSD(4),UPLPMSD  CLEAR THE LPM DATES                          
         MVI   LPMHSP,C'N'                                                      
*                                                                               
         CLI   DBBTYPE,C'H'              CONTROL HISPANIC SEPARATELY            
         BE    UPLPMHSP                                                         
         CLI   DBBTYPE,C'I'                                                     
         BE    UPLPMHSP                                                         
*                                                                               
         CLC   DBSELRMK,=H'101'          NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   DBSELRMK,=H'403'          LA                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68056807'                                          
         CLC   DBSELRMK,=H'202'          CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         B     UPLPMSET                                                         
*                                                                               
*                                                                               
UPLPMHSP CLC   DBSELRMK,=H'101'          NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   DBSELRMK,=H'202'          CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         OC    UPLPMSD,UPLPMSD                                                  
         BZ    UPLPMSET                                                         
         MVI   LPMHSP,C'Y'                                                      
         B     UPLPMSET                                                         
UPLPMSET OC    UPLPMSD,UPLPMSD                                                  
         BZ    GETHPT14BO                                                       
*        CLC   DBSELRMK,=H'101'    DEAL WITH NY PPM DATA                        
*        BNE   GETHPT14BO                                                       
         CLI   DBBTYPE,C'I'                                                     
         BE    GNYHPT1                                                          
         CLI   DBBTYPE,C'P'        KILL PPM PRIOR                               
         BNE   GNYHPT2                                                          
         CLI   SPUPSYS,C'R'                                                     
         B     GNYHPT1             <<-HARD BRANCH                               
                                                                                
GNYHPT1  CLC   DBSELBK,UPLPMSD     COMPARE TO LPM START                         
         BNL   GNYHPT2              IF LESS THE USE REGULAR                     
         MVI   DBBTYPE,0           DEFAULT DIARY                                
         CLI   LPMHSP,C'Y'                                                      
         BNE   GETHPT16                                                         
         MVI   DBBTYPE,C'H'        DEFAULT HISPANIC                             
         B     GETHPT16                                                         
*                                                                               
GNYHPT2  DS    0C                                                               
         CLI   SPUPSYS,C'R'                                                     
         B     GNYHPT4             <<-HARD BRANCH                               
                                                                                
GNYHPT4  DS    0C                                                               
         CLC   DBSELBK,UPLPMED      IF IT'S WITHIN CURRENCY PERIOD              
         BH    GETHPT16                                                         
         CLC   DBSELBK,UPLPMSD                                                  
         BL    GETHPT16                                                         
         MVI   DBBTYPE,C'P'         USE THE LPM HPT DATA                        
         CLI   LPMHSP,C'Y'                                                      
         BNE   GETHPT16                                                         
         MVI   DBBTYPE,C'I'                                                     
         B     GETHPT16                                                         
                                                                                
GETHPT14BO CLC   DBSELRMK,=H'106'    DEAL WITH BOSTON PPM DATA                  
         BNE   GETHPT16                                                         
         CLC   DBSELBK,=X'6604'                                                 
         BH    GETHPT16                                                         
         CLC   DBSELBK,=X'650A'                                                 
         BL    GETHPT16                                                         
         MVI   DBBTYPE,C'P'                                                     
*&&                                                                             
*&&DO                                                                           
         CLI   DBBTYPE,C'O'        OLYMPIC BOOK                                 
         BNE   GETHPT16                                                         
         CLC   DBSELBK,=X'6202'    EXCEPT FOR FEB/98                            
         BE    GETHPT16                                                         
         CLC   DBSELBK,=X'6007'    AND JULY/96                                  
         BE    GETHPT16                                                         
         MVI   DBBTYPE,0           THERE IS NO OLYMPIC BOOK SCHMUCK             
*&&                                                                             
GETHPT16 MVC   DBTAPEP,TAPEOPT                                                  
*                                                                               
* FOR FUSION WE NEED TO GRAB THE HUT PUTS                                       
         CLI   DBSELSRC,C'F'                                                    
**       BNE   *+22                                                             
         BNE   GETHPT17                                                         
         MVC   DBSELSTA(5),=C'HUT T'                                            
         MVC   DBSELMK,SPUPSPL                                                  
*                                                                               
         OC    DBSELMK,DBSELMK       IF SPILL MKT NUMBER NOT SET                
         BNZ   *+20                  THEN GRAB ALPHA SPILL MKT                  
         OC    SPUPSYSC,SPUPSYSC     IF THE HEADEND IS PASSED IN                
         BZ    *+10                  IF ITS NOT HEADEND NOT PASSED              
         MVC   DBSELALF,SPUPMALF     IN THEN WE ARE NOT CABLE                   
*                                                                               
*  IF ONE OF THE BOOKS IS LIVE ZERO CELL THEN THE CURRENT BOOK WILL             
*  BE ADJUSTED TO THE CORRECT PARALLEL ZEROCELL BOOKTYPE                        
*                                                                               
GETHPT17 MVI   ZCELLBTY,0                                                       
         L     RF,=A(CHKLIVEZ)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
**       L     RF,=A(CHKNHUT)      FOR NEW HUT BOOKS                            
**       A     RF,RELO                                                          
**       BASR  RE,RF                                                            
*                                                                               
**       L     RF,=A(CHKNHUT)      FOR NEW HUT BOOKS                            
**       A     RF,RELO                                                          
**       BASR  RE,RF                                                            
*                                                                               
         L     RF,=A(CHKLEXPB)     FOR NEW HUT BOOKS                            
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
GETHP17A GOTO1 VDEMAND,DMCB,DBLOCK,GETHPTHK                                     
***      MVC   DBBTYPE,SVBKTYP                                                  
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
***      BNE   GETHPT20            NO - EXIT ON ANY ERROR SETTING               
         BNE   GETHPT19            NO - EXIT ON ANY ERROR SETTING               
         MVI   DBERROR,0                                                        
         OC    DBDIVSOR,DBDIVSOR                                                
***      BZ    GETHPT20                                                         
         BZ    GETHPT19                                                         
         MVC   DBBTYPE,ZSVBKTYP                                                 
*                                                                               
                                                                                
*                                                                               
         MVC   DBBTYPE,SVBTYPE                                                  
         CLI   HPT,OLD             TEST OLD HPT'S REQUIRED                      
         BE    GETHPT24            YES                                          
         CLI   SPUP2YRP,C'Y'       CALLER CAN SPECIFY 2 YEAR PUTS               
         BE    *+12                                                             
         TM    DBCPUTS,DBO2YEAR    TEST 2 YEAR AVERAGE PUT REQUIRED             
         BZ    GETHPT24            NO                                           
         TM    GETHIND,X'80'       TEST BOTH YEARS DONE                         
         BNZ   GETHPT18            YES                                          
         ZIC   R1,GETHBK           DECREMENT ACTUAL BOOK YEAR                   
         BCTR  R1,0                                                             
         STC   R1,GETHBK                                                        
         MVC   GETHDIV,DBDIVSOR    SAVE DBDIVSOR                                
         OI    GETHIND,X'80'                                                    
         LA    R1,GETHBK           POINT TO DECREMENTED BOOK VALUE              
         B     GETHPT1             GET PREVIOUS YEAR HPT VALUES                 
*                                                                               
GETHPT18 SR    R0,R0               CALCULATE TOTAL DBDIVSOR VALUE               
         ICM   R0,3,DBDIVSOR                                                    
         SR    R1,R1                                                            
         ICM   R1,3,GETHDIV                                                     
         AR    R0,R1                                                            
         STCM  R0,3,DBDIVSOR                                                    
         B     GETHPT24                                                         
* ONLY IF THERES A PROBLEM READING THE FILE FOR HPT DO WE GET HERE              
* DBFUNCT=GETDEM                                                                
GETHPT19 DS    0H                                                               
* NOT FOUND !                                                                   
* NEW HUT CATEGORIES ONLY AVAILABLE FOE LPM MARKETS                             
* IF NOT FOUND RELOOK UP USING ORIGINAL BOOKTYPE                                
*                                                                               
**       CLI   NHUTFLAG,C'Y'                                                    
**       BNE   GETHP19B                                                         
**       MVI   NHUTFLAG,C'N'                                                    
         CLI   NEXPFLAG,C'Y'                                                    
         BNE   GETHP19B                                                         
         MVI   NEXPFLAG,C'N'                                                    
         MVC   DBBTYPE,ZSVBKTYP                                                 
         OC    SVSELTIM,SVSELTIM                                                
         BZ    *+10                                                             
         MVC   DBSELTIM,SVSELTIM                                                
         B     GETHP17A                                                         
*                                                                               
*&&DO                                                                           
* CHECK IF WE ARE ASKING FOR ZERO CELL DATA                                     
* IF SO IT WILL SET THE BOOKTYPE TO NON ZEROCELL LOOKUP                         
         CLI   ZCELLBTY,0                                                       
         BNE   GETHP19B                                                         
         L     RF,=A(NONZEROC)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
         BNE   GETHP17A           ZERO CELL - REREAD FOR NON ZEROCELL           
*&&                                                                             
* RESTORE FROM SAVED BOOKTYPE AT CHKLIVEZ                                       
GETHP19B MVC   DBBTYPE,ZSVBKTYP                                                 
         MVC   DBBTYPE,SVBTYPE                                                  
*                                                                               
*                                                                               
GETHPT20 MVC   DBDIVSOR,GETHDIV                                                 
         TM    GETHIND,X'80'                                                    
         BNZ   GETHPT24                                                         
         L     R1,APARM                                                         
         MVI   0(R1),X'FE'         RETURN NO H/P/T'S FOUND                      
         B     UPERR                                                            
*                                                                               
GETHPT24 CLI   EXTBUFF,C'Y'                                                     
         BE    GETHPT25            USE EXTENDED BUFFERS?                        
*                                                                               
         LA    R0,NUMVALS*2        UNWEIGHT HPT VALUES                          
         LA    R1,OLDHPT           R1=A(OLD OR NEW HPT LINE)                    
         CLI   HPT,OLD                                                          
         BE    *+8                                                              
         LA    R1,NEWHPT                                                        
         ST    R1,DMCB+8                                                        
         SR    R2,R2                                                            
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
*        BAS   RE,UNWGHT           UNWEIGHT DEMO VALUES                         
*                                                                               
         B     UPX                                                              
*                                                                               
*  USE EXTENDED BUFFER                                                          
GETHPT25 LA    R0,NUMVALS*2        UNWEIGHT HPT VALUES                          
         LA    R2,OLDHPT           UNWEIGHT BACK TO REGULAR BUFFS               
         LA    R1,MBKOHPT                                                       
         CLI   HPT,OLD                                                          
         BE    *+12                                                             
         LA    R2,NEWHPT           EXPANDED BUFFER                              
         LA    R1,MBKNHPT                                                       
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('MBKUNWQ',(RC)),(R0),,(R2)                         
*                                                                               
         B     UPX                                                              
                                                                                
         SPACE 2                                                                
***********************************************************************         
* SUBROUTINE TO PROCESS HPT RECORDS FROM DEMAND                       *         
***********************************************************************         
         SPACE 1                                                                
GETHPTHK ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         XC    GETHPTRT(LENVALS*2),GETHPTRT CLEAR RTG/IMPS                      
         XC    GETHPTS(LENVALS*2),GETHPTS   CLEAR HPTS                          
         XC    GETHPTUN(LENVALS),GETHPTUN  CLEAR UNIVERSES                      
*                                                                               
         TM    GETHIND,X'40'       TEST ACTUAL BOOK SET                         
         BNZ   *+14                                                             
         OI    GETHIND,X'40'                                                    
         MVC   GETHBK,DBACTBK                                                   
*                                                                               
         LH    R0,DBFACTOR         FORCE NO WEIGHTING IN GETIUN                 
         MVC   DBFACTOR,=H'1'                                                   
                                                                                
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*  SET THIS SO GETIUN KNOWS WE ARE CABLE SO IT CAN COPY OVER                    
* TSA FROM DMA IMPS AND TOTS                                                    
         MVC   DBSELSYC,SPUPSYSC                                                
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),GETHPTUN                                 
         STH   R0,DBFACTOR                                                      
                                                                                
GETHP1A  LA    R0,NUMVALS*2                                                     
         LA    R1,GETHPTS          R1=A(UNWEIGHTED DEMO VALUES)                 
         LA    R2,OLDHPT           R2=A(WEIGHTED DEMO ACCUMULATORS)             
         CLI   HPT,OLD                                                          
         BE    GETHPTH4                                                         
         LA    R2,NEWHPT                                                        
                                                                                
GETHP1C  OC    LUNV(LENVALS),LUNV  TEST IF LOONEYVERSES THERE                   
         BNZ   *+10                YES                                          
         MVC   LUNV(LENVALS),GETHPTUN                                           
                                                                                
GETHPTH4 ST    R1,DMCB+8                                                        
         CLI   EXTBUFF,C'Y'                                                     
         BE    GETHPT4C            USE EXTENDED BUFFERS?                        
         GOTO1 VSUBR01,DMCB,('WGHTUPQ',(RC)),(R0),,(R2)                         
         B     GETHPTHX                                                         
*                                                                               
*                                                                               
GETHPT4C LA    R1,GETHPTS          R1=A(UNWEIGHTED DEMO VALUES)                 
         LA    R2,MBKOHPT          R2=A(WEIGHTED DEMO ACCUMULATORS)             
         CLI   HPT,OLD                                                          
         BE    *+8                                                              
         LA    R2,MBKNHPT                                                       
         GOTO1 VSUBR01,DMCB,('MBKWGTQ',(RC)),(R0),,(R2)                         
*                                                                               
         B     GETHPTHX                                                         
                                                                                
GETHPTHX L     RE,SAVERE           RETURN TO DEMAND FOR NEXT RECORD             
         BR    RE                                                               
*******  DROP  R8                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* SUBROUTINE TO INDEX 4 YEARS SHARES                                  *         
***********************************************************************         
         SPACE 1                                                                
                                                                                
***********************************************************************         
* SUBROUTINE TO CALCULATE NEW ROW VALUES                              *         
* P1=IN1,P2=IN2,P3=OUT                                                *         
* IF P2=0, P1 IS MULTIPLIED BY 'INDEX'                                *         
* IF P2 NOT 0, P1 VALUES ARE MULTIPLIED BY P2 VALUES                  *         
***********************************************************************         
         SPACE 1                                                                
CALC     NTR1                                                                   
         LM    R2,R4,0(R1)         R2=IN1,R3=IN2,R4=OUT                         
         SR    RE,RE               CLEAR INDEX REG                              
         ZIC   RF,0(R1)            RF=N'VALUES                                  
         LTR   R3,R3               TEST 2 ROWS SPECIFIED                        
         BNZ   CALC2                                                            
         SPACE 1                                                                
CALC1    L     R1,0(R2,RE)         ONE ROW SPECIFIED                            
         M     R0,INDEX            MULTIPLY IT BY VALUE IN INDEX                
         AH    R1,=H'5000'                                                      
         D     R0,=F'10000'                                                     
         ST    R1,0(R4,RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,CALC1                                                         
         B     UPX                                                              
         SPACE 1                                                                
CALC2    L     R1,0(R2,RE)         TWO ROWS SPECIFIED                           
         M     R0,0(R3,RE)         MULTIPLY CORRESPONDING VALUES                
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         ST    R1,0(R4,RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,CALC2                                                         
         B     UPX                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* SUBROUTINE TO COMPUTE INTEGER INDEX VALUE                           *         
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                 *         
* RETURN RESULT IN R1.                                                *         
***********************************************************************         
         SPACE 1                                                                
GETVALUE MH    R1,=H'100'                                                       
         LR    RF,R0               SAVE OLD VALUE                               
         SRL   R0,1                HALVE                                        
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         DR    R0,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
*&&DO                                                                           
***********************************************************************         
* SUBROUTINE TO COMPUTE INDEX VALUE TO 2 DECIMAL PLACES               *         
*                                                                     *         
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                 *         
* RETURN RESULT IN R1 AND IN 'INDEX'.                                 *         
***********************************************************************         
         SPACE 1                                                                
GETINDEX XC    INDEX,INDEX                                                      
         MH    R1,=H'10000'                                                     
         LTR   RF,R0               SAVE OLD VALUE                               
         BZR   RE                                                               
         SRL   R0,1                HALVE                                        
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         DR    R0,RF                                                            
         ST    R1,INDEX                                                         
         BR    RE                                                               
         EJECT                                                                  
*&&                                                                             
                                                                                
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO SET OUTPUT VALUES FOR OVERRIDE DEMOS                     *         
*                                                                     *         
* NTRY - R2=A(OVERRIDE ELEMENT)                                       *         
*        RF=A(DEMO LIST ENTRY OF FIRST MODIFIER FOR OVERRIDE DEMO)    *         
*        R1=A(CORRESPONDING OUTPUT VALUE OF RF)                       *         
***********************************************************************         
         SPACE 1                                                                
SETOVER  NTR1  ,                                                                
         ST    RF,DUB+0                                                         
         ST    R1,DUB+4                                                         
         LA    R4,WORK             BUILD OVERRIDE ENTRY IN WORK                 
         MVC   0(1,R4),2(R2)                                                    
         XC    1(2,R4),1(R4)                                                    
         MVC   3(2,R4),4(R2)                                                    
         MVI   5(R4),X'FF'                                                      
         CLI   2(R2),C'S'          TEST 'SHARE' OVERRIDE                        
         BNE   SETOVERA                                                         
*                                  CELL UPGRADE FOR SHARE OVERRIDE              
         MVC   OVERDEM(OVERSHRL),OVERSHRS                                       
         LA    R1,OVERDEM-L'OVERDEM                                             
SETOVER2 LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    *+14                                                             
         MVC   2(1,R1),3(R2)       INSERT DEMO NUMBER                           
         B     SETOVER2                                                         
         GOTO1 VDEMOUT,DMCB,(C'L',OVERDEM),DBLOCK,OVERDEMS                      
         CLI   DBERROR,0           TEST FOR ERRORS                              
         BNE   SETOVERA                                                         
         L     R0,OVERSHR          R0=OLD SHARE VALUE                           
         SR    R1,R1                                                            
         ICM   R1,3,4(R2)          R1=NEW SHARE VALUE                           
         BAS   RE,GETINDEX         CALCULATE INDEX VALUE                        
***      GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC)),R0,R1                              
         LA    R4,5(R4)            POINT TO NEXT OVERRIDE ENTRY                 
*                                                                               
         L     R1,OVERRTG          CALCULATE NEW RATING                         
         M     R0,INDEX                                                         
         AH    R1,=H'5000'                                                      
         D     R0,=F'10000'                                                     
         MVI   0(R4),C'R'                                                       
         STCM  R1,15,1(R4)                                                      
         LA    R4,5(R4)                                                         
*                                                                               
         L     R1,OVERIMP          CALCULATE NEW IMPRESSION                     
         M     R0,INDEX                                                         
         AH    R1,=H'5000'                                                      
         D     R0,=F'10000'                                                     
         MVI   0(R4),C'I'                                                       
         STCM  R1,15,1(R4)                                                      
         LA    R4,5(R4)                                                         
         MVI   0(R4),X'FF'                                                      
         B     SETOVERA                                                         
*                                                                               
SETOVERA LM    RE,RF,DUB           SLOT OVERRIDES INTO OUTPUT AREA              
SETOVERC CLI   0(RE),X'FF'         TEST E-O-L                                   
         BE    SETOVERX                                                         
         CLC   2(1,RE),3(R2)       MATCH ON DEMO                                
         BNE   SETOVERG                                                         
         CLI   0(RE),OVERELEM      IGNORE IF ALREADY AN OVERRIDE                
         BE    SETOVERG                                                         
         LA    R1,WORK             FIND OVERRIDE VALUE IN TABLE                 
SETOVERE CLI   0(R1),X'FF'                                                      
         BE    SETOVERG                                                         
         CLC   0(1,R1),1(RE)       MATCH ON MODIFIER                            
         BE    *+12                                                             
         LA    R1,5(R1)                                                         
         B     SETOVERE                                                         
         MVC   0(4,RF),1(R1)                                                    
         MVI   0(RE),OVERELEM      INDICATE THIS IS AN OVERRIDE                 
SETOVERG LA    RE,3(RE)            BUMP TO NEXT DEMO                            
         LA    RF,4(RF)                                                         
         B     SETOVERC                                                         
SETOVERX B     UPX                                                              
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO INITIALIZE DBLOCK FOR OLD DATA LOOK-UPS                  *         
***********************************************************************         
         SPACE 1                                                                
BLDBLK   NTR1                                                                   
         GOTO1 VSUBR01,DMCB,('BLDBLKQ',(RC))                                    
         J     UPX                                                              
         EJECT                                                                  
***********************************************************************         
* ADD AN OVERRIDE VALUE TO LIST OF DEMO OVERRIDE VALUES                         
*  ===== CODE MOVED DOWN TO SUBROUTINE POOL                                     
***********************************************************************         
ADDOVER  NTR1                                                                   
         GOTO1 VSUBR01,DMCB,('ADDOVERQ',(RC))                                   
         J     UPX                                                              
*&&DO                                                                           
* ADD AN OVERRIDE VALUE TO LIST OF DEMO OVERRIDE VALUES                         
*                                                                               
ADDOVER  ZIC   RF,OVERLIST         RF=NUMBER OF VALUES SO FAR                   
         LA    RF,1(RF)                                                         
         STC   RF,OVERLIST         SET NEW VALUE COUNT                          
         SLL   RF,2                                                             
         LA    RF,OVERLIST-3(RF)                                                
         MVC   0(4,RF),DUB         INSERT NEW ENTRY INTO LIST                   
         MVI   4(RF),X'FF'                                                      
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
*&&                                                                             
* HELPER ROUTINE TO CALL THE ACTUAL INSERTING ROUTINE                           
                                                                                
INXTND   NTR1                                                                   
         GOTO1 VSUBR01,DMCB,('INXTNDQ',(RC))                                    
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MEDCAN   EQU   C'C'                                                             
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
OLD      EQU   C'O'                                                             
NEW      EQU   C'N'                                                             
OVERELEM EQU   X'DE'                                                            
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                  FULLWORD DISPLACEMENT TO HOMES CELL          
LENVALS  EQU   NUMVALS*4                                                        
         SPACE 1                                                                
TPTVALS  DC    C'TTP '                                                          
PAVVALS  DC    C'PPAV'                                                          
IUNVALS  DC    C' IUN'                                                          
         SPACE 1                                                                
HOMEVUT  DC    X'81',C'V',AL1(1)   LIST OF HOMES VUTS (RTG/SHR)                 
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HOMEPUT  DC    X'81',C'P',AL1(1)   LIST OF HOMES PUTS                           
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
NETPUTS  DC    X'00',C'P',AL1(1)   LIST OF HOMES PUTS (NETWORK)                 
         DC    X'00',C'P',AL1(2)                                                
         DC    X'00',C'P',AL1(3)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
*&&DO                                                                           
OVERSHRS DC    X'00',C'S',X'00'    LIST OF DEMOS FOR SHARE OVERRIDES            
         DC    X'00',C'R',X'00'                                                 
         DC    X'00',C'I',X'00'                                                 
         DC    X'FF'                                                            
OVERSHRL EQU   *-OVERSHRS                                                       
         SPACE 1                                                                
*&&                                                                             
RHOMES   DC    C'R',AL1(1)                                                      
PHOMES   DC    C'P',AL1(1)                                                      
SHOMES   DC    C'S',AL1(1)                                                      
         SPACE 1                                                                
DIV      DC    C'DIV'                                                           
MAD      DC    C'MAD'                                                           
MUL      DC    C'MUL'                                                           
REP      DC    C'REP'                                                           
PROGPLUS DC    C'PROG+'                                                         
         SPACE 1                                                                
**DEMFILE  DC    C'DEMFILE '                                                    
OFORMAT  DC    C'PAVUIUN',AL1(82,11,00)                                         
*&&DO                                                                           
BOOK8306 DC    AL1(83,06)          BOOK UNROUNDED BOTTOM LINE                   
BOOK9011 DC    AL1(90,11)          TAPE BASED NORMAL                            
BOOK9106 DC    AL1(91,06)          TAPE BASED UPGRADE                           
BOOK9012 DC    AL1(90,12)          TAPE BASED INDEXED                           
BOOK8212 DC    AL1(82,12)          BOOK BASED INDEXED                           
BOOK8701 DC    AL1(87,01)          WTP                                          
*&&                                                                             
BOOK     DC    XL2'2000'                                                        
BOOKZ    DC    XL2'0000'           DUMMY ZERO BOOK                              
***NDXDEMO  DC    C'&&',AL1(254)      INDEX DEMO MODIFIER/NUMBER                
         DROP  R5                                                               
         EJECT                                                                  
SUBR01   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR01**,RA,RR=RE                                              
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         ST    R1,PARM01                                                        
         ST    RE,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
GETYRSE  EQU   (GETYRS#-*)/4+1                                                  
GETDAYQ  EQU   (GETDAY#-*)/4+1                                                  
GETTIMQ  EQU   (GETTIM#-*)/4+1                                                  
UNWGHTQ  EQU   (UNWGHT#-*)/4+1                                                  
WGHTUPQ  EQU   (WGHTUP#-*)/4+1                                                  
**STNHPTS  EQU   (STNHPT#-*)/4+1                                                
INXTNDQ  EQU   (INXTND#-*)/4+1                                                  
BLDBLKQ  EQU   (BLDBLK#-*)/4+1                                                  
NORMUQ   EQU   (NORMU#-*)/4+1                                                   
MBKUNWQ  EQU   (MBKUNW#-*)/4+1                                                  
MBKWGTQ  EQU   (MBKWGT#-*)/4+1                                                  
GETIDXQ  EQU   (GETIDX#-*)/4+1                                                  
SETOVRQ  EQU   (SETOVR#-*)/4+1                                                  
UPENDQ   EQU   (UPEND#-*)/4+1                                                   
ADDOVERQ EQU   (ADDOVER#-*)/4+1                                                 
NORMIMPQ EQU   (NORMIMP#-*)/4+1                                                 
LPMIDXQ  EQU   (LPMIDX#-*)/4+1                                                  
GETIDX2Q EQU   (GETIDX2#-*)/4+1                                                 
GETMRECQ EQU   (GETMREC#-*)/4+1                                                 
SETSCBLQ EQU   (SETSCBL#-*)/4+1                                                 
LPSIDXQ  EQU   (LPSIDX#-*)/4+1                                                  
         SPACE 1                                                                
GETYRS#  B     GETYRS              GET MULTI BOOK INDEXES                       
GETDAY#  B     GETDAY              GET DAY IN DEMAND FORMAT                     
GETTIM#  B     GETTIME             CONVERT QHR TO MILITARY ST & END             
UNWGHT#  B     UNWGHT              UNWEIGHT AND ACCUM DEMOS                     
WGHTUP#  B     WGHTUP              WEIGHT AND ACCUM DEMOS                       
**STNHPT#  B     STNHPT              PROCESS IN MARKET SHARES                   
INXTND#  B     INSXTND             INSERT INTO EXTENSION LIST                   
BLDBLK#  B     BLDBLK00            BUILD DBLOCK                                 
NORMU#   B     NORMU000            NORMALIZE HPT                                
MBKUNW#  B     MBKUNWGT            UNWEIGHT AND ACCUM DEMOS                     
MBKWGT#  B     MBKWGTUP            WEIGHT AND ACCUM DEMOS                       
GETIDX#  B     GETINDX             COMPUTE INTEGER INDEX VALUE TO 2 DEC         
SETOVR#  B     SETOVER             SET OUTPUT VALUES FOR OVERRIDE DEMOS         
UPEND#   B     UPEND00             REPLACE OLD DEMO ELEMENTS W IUN              
ADDOVER# B     ADDOVER0            ADD OVERRIDE VALUE TO LIST                   
NORMIMP# B     NORMIMP0            NORMALIZE FOR IMPRESSIONS MULTIBK            
LPMIDX#  B     UPLPMIDX            LPM UPGRADE APPLY PUT AND SHARE INDX         
GETIDX2# B     GETINDX2            NEWER GETINDEX ROUTINE                       
GETMREC# B     GETMKT              READ SPOT MARKET RECORD FOR SOURCE           
SETSCBL# B     SETSCBLE            SET CABLE INFO FOR SPOT                      
LPSIDX#  B     UPLPSIDX            LPM UPGRADE APPLY PUT AND SHARE INDX         
         EJECT                                                                  
ERR01    L     RD,SAVERD           ENTER HERE ON ERROR                          
XIT01    XIT1                      EXIT SUBR01  -- RTN TO CALLER                
*                                                                               
**********************************************************                      
GETYRS   DS    0C                                                               
         L     R0,AIO2                                                          
         LHI   R1,IOAREA2X-IOAREA2                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    BOOKLIST,BOOKLIST                                                
         CLC   =X'9999',SPUPFLD3       LPS UPGRADE                              
         BNE   GTYRS001                                                         
         MVC   BOOKLIST(2),SPUPFLD2                                             
         MVC   BOOKLIST+2(2),SPUPFLD1    THE 2 SEASONAL TREND BOOKS             
GTYRS001 DS    0C                                                               
*                                                                               
*    FOR SAQ/PAQ  WE NEED TO CREATE THE BOOKLIST                                
         CLI   SPUPTYPE,13     PAQ/SAQ                                          
         BE    *+8                                                              
         CLI   SPUPTYPE,14                                                      
         BNE   GETYRSA                                                          
         CLI   YRSAVTYP,C'B'   PAB/SAB                                          
         BNE   *+14                                                             
         MVC   BOOKLIST(2),SPUPFLD1                                             
         B     GETYRSA                                                          
         CLI   YRSAVTYP,C'Q'   PAQ/SAQ                                          
         BNE   GETYRSA                                                          
*                                                                               
         ZIC   RF,SPUPFLD1     GRAB YEAR                                        
         SHI   RF,1            GRAB 1 YEAR BACK FROM REQUESTED YEAR             
         MVC   QTRBKTAB,QTRBKTBQ                                                
         LA    RE,QTRBKTAB     FILL QTRBKTAB'S POSSIBLE BOOK RANGE              
         LA    R0,4            FILL WITH ONE YEAR PRIOR TO REQUEST              
         STC   RF,0(RE)        YEAR                                             
         AHI   RE,2                                                             
         BCT   R0,*-8                                                           
         AHI   RF,1            GRAB 1 REQUEST YEAR                              
         LA    R0,4            FILL REQUEST YEAR                                
         STC   RF,0(RE)                                                         
         AHI   RE,2                                                             
         BCT   R0,*-8                                                           
*                                                                               
* NOW QTRBKTAB HAS LATEST AND EARLIEST 4 QTRS BOOKS POSSIBLE                    
* DEPENDING ON THE REQUEST                                                      
         LA    RE,QTRBKTAB                                                      
         CLC   SPUPFLD1,0(RE)  COMPARE START BOOK TO RANGE IN TABLE             
         BL    *+12                                                             
         AHI   RE,2                                                             
         B     *-14                                                             
         MVI   SPUPFLD2,0                                                       
         ICM   R0,3,SPUPFLD2   NUMBER OF QTRS                                   
         SHI   RE,2                                                             
         LA    RF,BOOKLIST                                                      
*                                                                               
*                                                                               
GTYRS002 DS    0C                                                               
         MVC   0(2,RF),0(RE)   CREATE BOOKLIST                                  
         AHI   RF,2                                                             
         SHI   RE,2                                                             
         BCT   R0,GTYRS002                                                      
*                                                                               
*                                                                               
*                                                                               
******                                                                          
GETYRSA  EQU   *                                                                
         SR    R0,R0                                                            
         ICM   R0,3,SPUPFLD2                                                    
         STC   R0,Y4SYRS                                                        
         CLC   =X'9999',SPUPFLD3   LPS UPGRADE                                  
         BNE   *+8                                                              
         MVI   Y4SYRS,2                                                         
*                                                                               
         CLI   SPUPSTYP,C'M'       INDEX MULTIPLY                               
         BNE   *+12                                                             
         MVI   Y4MSW,C'Y'                                                       
         B     *+12                                                             
         CLI   SPUPSTYP,C'N'       INDEX AVERAGE                                
         BNE   GETYRS0                                                          
         MVI   Y4SYRS,2                                                         
         MVC   BOOKLIST(6),SPUPFLD1                                             
         OC    BOOKLIST+2(2),BOOKLIST+2                                         
         BZ    UPX2                                                             
         OC    BOOKLIST+4(2),BOOKLIST+4                                         
         BZ    GETYRS0                                                          
         MVI   Y4SYRS,3                                                         
*                                                                               
GETYRS0  XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVC   Y4SYRS2,Y4SYRS                                                   
GETYRS1  XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         GOTO1 VSUBR02,DMCB,('BLDLATBQ',(RC))                                   
*        MVC   DBFILE,TPTVALS+1                                                 
         L     RF,ATPTVALS                                                      
                                                                                
         MVC   DBFILE(L'DBFILE),1(RF)                                           
         MVC   DBTAPEP,TAPEOPT                                                  
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION TO GET MARKET               
*        CLC   0(2,R1),BOOK        TEST IF BOOK PASSED                          
*        BNH   *+10                                                             
*        MVC   DBSELBK,0(R1)       YES - SET SELECTED BOOK                      
*        NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
****     LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
         TM    SPUPOPTS,SPOPEXT                                                 
*&&DO                                                                           
         BZ    *+10                EXTEND BLOCK NOT IN USE                      
         MVC   DBEXTEND,SPUPEXTN    IN USE - SET IT                             
*&&                                                                             
         BZ    GETYRSG             EXTEND BLOCK NOT IN USE                      
         GOTO1 VSUBR01,DMCB,('INXTNDQ',(RC))   IN USE - INSERT IT               
GETYRSG  EQU   *                                                                
         MVC   DBCOMFCS,VCOMFACS   **MUST BE DONE FOR MKT OVERRIDES**           
         MVC   DBSELAGY,SPUPAGY    RESTORE AGENCY                               
         MVC   DBSELUMK,SPUPMKT    SET AGENCY MKT CODE                          
         MVC   DBSELCLI,SPUPCLI    SET AGENCY CLIENT CODE                       
         MVC   DBSELMED,SPUPMED                                                 
         TM    SPUPOPTS,SPOANGFR   CANADIAN ANGLO/FRANCO OPTION                 
         BZ    *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
         TM    SPUPOPT2,SPO2CMON   CANADIAN WEEKLIES AS MONTH                   
         BZ    *+8                                                              
         MVI   DBBEST,C'M'                                                      
         TM    SPUPOPT2,SPO2UBBM   CANADIAN USE BBM WEEKLIES                    
         BZ    *+8                                                              
         MVI   DBUSEBBM,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTY   SPORTS ONLY OPTION                           
         BZ    *+8                                                              
         MVI   DBSELSPO,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTN   EXCLUDE SPORTS OPTION                        
         BZ    *+8                                                              
         MVI   DBSELSPO,C'N'                                                    
         CLI   SPUPTPTT,C'P'       SET TP/TT                                    
         BNE   *+8                                                              
         MVI   DBTPTT,C'P'                                                      
         MVC   DBSELBK,SPUPFLD1                                                 
*                                                                               
         OC    SPUPPUR,SPUPPUR     TEST DAY/TIME LOOK-UP                        
         BNZ   GETYRS2                                                          
         LA    R1,DBLOCK1          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DUB(2),SPUPFBK                                                   
         NI    DUB+1,X'0F'         AND OFF WEEK BITS                            
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         B     GETYRS4                                                          
*                                                                               
GETYRS2  L     R1,SPUPAREC                                                      
         MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
         MVC   DBSELSRC,PRSRC-PRKEY(R1)                                         
         MVC   DBBTYPE,PRBTYP-PRKEY(R1)                                         
         MVC   DUB(2),PRBOOK-PRKEY(R1)                                          
*                                                                               
GETYRS4  OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         BNZ   *+10                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
*        LA    R0,2                                                             
*        TM    GETHIND,X'80'       DON'T BACK-UP IF 2ND PUT LOOK-UP             
*        BZ    *+8                                                              
*        LA    R0,1                                                             
         OC    SPUPSPL,SPUPSPL     TEST SPILL MARKET GIVEN                      
         BZ    *+14                                                             
         MVC   DBSELRMK,SPUPSPL    YES - GET H/P/T'S FROM HERE                  
         B     GETYRS10                                                         
         OC    DBSELRMK,MARKET     TEST MARKET NUMBER KNOWN                     
         BNZ   GETYRS10            YES - GO GET DEMOS                           
*                                                                               
*                                                                               
GETYRS7  DS    0C                                                               
GETYRS10 DS    0C                                                               
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         BNE   GETYR10A                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   GETYR10A                                                         
         OC    DBSELSYC,DBSELSYC    CABLE LOOKUPS                               
         BNZ   GETYR11A                                                         
         OC    SPUPSYSC,SPUPSYSC                                                
         BNZ   GETYR11A                                                         
         CLI   SPUPBTYP,C'U'        LIVE ONLY DMA CABLE                         
         BE    *+8                                                              
         CLI   SPUPBTYP,C'C'        DMA CABLE                                   
         BE    *+8                                                              
         CLI   SPUPBTYP,C'W'        WIRE CABLE                                  
         BE    *+8                                                              
         CLI   SPUPBTYP,C'4'        ZERO CELL WIRED CABLE                       
         BE    *+8                                                              
         CLI   SPUPBTYP,C'Z'        LIVE ONLY                                   
         BE    *+8                                                              
GETYR10A CLI   SPUPSRC,C'F'         FUSION                                      
         BNE   GETYRS11                                                         
GETYR11A MVC   DBSELMK,SPUPSPL                                                  
*                                                                               
GETYRS11 MVI   DBFUNCT,DBGETDEM                                                 
         L     R1,AIO2             BUILD A DUMMY DEMO RECORD IN IO2             
         XC    0(50,R1),0(R1)                                                   
*                                                                               
GETYRS16 XC    WORK(20),WORK       BUILD DEMOMATH FORMAT BLOCK                  
         ST    R7,SAVER7                                                        
         LA    R1,DBLOCK                                                        
         ST    R1,WORK                                                          
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),WORK+8                                                
         MVC   WORK+14(1),DBACTSRC                                              
         MVC   DBTAPEP,TAPEOPT                                                  
*        LA    R0,4                SET FOR 4 BOOKS                              
         ZIC   R0,Y4SYRS           SET FOR N BOOKS                              
         CLI   Y4SYRS,2                                                         
         BL    UPX2                                                             
         XC    DUB(4),DUB          DBDIVSOR COUNTER                             
         XC    TOTHMSHR,TOTHMSHR                                                
*                                  CALL DEMAND TO GET H/P/T DATA                
*                                  RECORDS ARE PROCESSED AT GETYRS18            
GETYRS17 OC    BOOKLIST(2),BOOKLIST BOOK LIST GIVEN                             
         BZ    GTYRS17A                                                         
         ZIC   RE,BOOKI             YES - INDEX THROUGH IT                      
         LR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BOOKI                                                         
         SLL   RE,1                                                             
         LA    RE,BOOKLIST(RE)                                                  
         CLC   0(2,RE),=X'0000'                                                 
         BE    GETYRS20                                                         
         MVC   DBSELBK,0(RE)                                                    
*                                                                               
*                                                                               
GTYRS17A L     RF,=A(CHKLPM)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         L     RF,=A(CHKLIVEM)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
**                                                                              
*    FOR SAB/PAB  WE NEED TO CHANGE FEB_09 TO MAR_09 FOR NON LPM                
         CLI   SPUPTYPE,13     PAQ/SAQ                                          
         BE    *+8                                                              
         CLI   SPUPTYPE,14                                                      
         BNE   GTYRS17F                                                         
         CLI   YRSAVTYP,C'B'   PAB/SAB                                          
         BE    GTYRS17F                                                         
         CLI   YRSAVTYP,C'G'       SAVG,PAVG                                    
         BE    GTYRS17F                                                         
*****    OC    UPLPMSD,UPLPMSD                                                  
*****    BNZ   GTYRS17F                                                         
         CLC   =AL2(FEB_09),DBSELBK  THERE IS NO FEB09 BOOK                     
         BNE   *+10                                                             
         MVC   DBSELBK,=AL2(MAR_09)  FORCE TO MAR09                             
**                                                                              
*                                                                               
GTYRS17F CLI   DBBTYPE,C'P' CHANGE MMU BACK TO NORMAL IF NOT LPM                
         BE    *+8                                                              
         CLI   DBBTYPE,C'I' CHANGE MMU BACK TO NORMAL IF NOT LPM                
         BE    *+8                                                              
         MVI   MMUPASS,0                                                        
*                                                                               
*  CALL ROUTINE TO CHECK IF ANY OF THE BOOKS ARE LIVE ZEROCELL                  
*  IF ONE OF THE BOOKS IS LIVE ZERO CELL THEN THE CURRENT BOOK WILL             
*  BE ADJUSTED TO THE CORRECT PARALLEL ZEROCELL BOOKTYPE                        
*                                                                               
GTYRS17H MVI   ZCELLBTY,0                                                       
         L     RF,=A(CHKLIVEZ)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
**       L     RF,=A(CHKNHUT)      FOR NEW HUT BOOKS                            
**       A     RF,RELO                                                          
**       BASR  RE,RF                                                            
*                                                                               
         L     RF,=A(CHKLEXPB)     FOR NEW HUT BOOKS                            
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
GTYRS17R MVI   HOOKFLAG,C'N'       INDICATOR FLAG FOR HOOK                      
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         BNE   GTYRS17T                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   GTYRS17T                                                         
         OC    DBSELSYC,DBSELSYC                                                
         BNZ   GTYRS17U                                                         
         OC    SPUPSYSC,SPUPSYSC                                                
         BNZ   GTYRS17U                                                         
         CLI   SPUPBTYP,C'U'        LIVE ONLY DMA CABLE                         
         BE    *+8                                                              
         CLI   SPUPBTYP,C'C'        DMA CABLE                                   
         BE    *+8                                                              
         CLI   SPUPBTYP,C'W'        WIRE CABLE                                  
         BE    *+8                                                              
         CLI   SPUPBTYP,C'4'        ZERO CELL WIRED CABLE                       
         BE    *+8                                                              
         CLI   SPUPBTYP,C'Z'        LIVE ONLY                                   
         BE    *+8                                                              
GTYRS17T CLI   SPUPSRC,C'F'                                                     
         BNE   *+10                                                             
GTYRS17U MVC   DBSELMK,SPUPSPL                                                  
*                                                                               
         MVC   DBSELSYC,SPUPSYSC                                                
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
         GOTO1 VDEMAND,DMCB,DBLOCK,GETYRS18                                     
* CHK FOR ZERO CELL BOOKTYPE                                                    
         CLI   DBERROR,X'10'       IF NOT FOUND                                 
         BNE   GTYRS17X                                                         
* NOT FOUND !                                                                   
* NEW HUT CATEGORIES ONLY AVAILABLE FOE LPM MARKETS                             
* IF NOT FOUND RELOOK UP USING ORIGINAL BOOKTYPE                                
*                                                                               
**       CLI   NHUTFLAG,C'Y'                                                    
**       BNE   GTYRS17X                                                         
**       MVI   NHUTFLAG,C'N'                                                    
         CLI   NEXPFLAG,C'Y'                                                    
         BNE   GTYRS17X                                                         
         MVI   NEXPFLAG,C'N'                                                    
         MVC   DBBTYPE,ZSVBKTYP                                                 
         OC    SVSELTIM,SVSELTIM                                                
         BZ    *+10                                                             
         MVC   DBSELTIM,SVSELTIM                                                
         B     GTYRS17R                                                         
*                                                                               
*&&DO                                                                           
         CLI   ZCELLBTY,0          ONLY DO IT ONCE                              
         BNE   GTYRS17X                                                         
         L     RF,=A(NONZEROC)      CHK FOR ZERO CELL                           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
         BNE   GTYRS17R           ZERO CELL - REREAD FOR NON ZEROCELL           
*&&                                                                             
                                                                                
GTYRS17X MVC   DBBTYPE,ZSVBKTYP   RESET THE BOOKTYPE BACK FROM CHKLIVEZ         
*                                                                               
                                                                                
                                                                                
         CLI   YRSAVTYP,C'B'       SAB/PAB ?                                    
         BNE   *+12                IF DBERROR SET  AS                           
         CLI   DBERROR,X'10'       RECORD NOT FOUND THEN                        
         BE    SLOTSHR3X           CONTINUE TO GRAB NEXT BOOK                   
*                                                                               
*SAQ/PAQ/SAVG/PAVG REMAINING BOOKS IF CURRENT BOOK LOOKUP FAILS                 
         CLI   YRSAVTYP,C'G'       SAVG,PAVG                                    
         BE    *+8                                                              
         CLI   YRSAVTYP,C'Q'       SAQ/PAQ ?                                    
         BNE   GTYRS17Y                                                         
         CLI   DBERROR,X'80'                                                    
         BNE   *+14                                                             
         OC    DUB(4),DUB          NOTHING FOUND                                
         BNZ   GTYRS17Z                                                         
         L     R7,SAVER7           RESTORE R7                                   
         MVI   DBERROR,0                                                        
         ZIC   RE,Y4SYRS2                                                       
         SHI   RE,1                                                             
         STC   RE,Y4SYRS2                                                       
         B     PABLOOPX                                                         
***      B     SLOTSHR3X                                                        
*                                                                               
GTYRS17Y CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BNE   UPX2                NO - EXIT ON ANY ERROR SETTING               
         OC    DUB(4),DUB          NOTHING FOUND                                
         BZ    UPX2                                                             
GTYRS17Z MVI   DBERROR,0                                                        
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         LA    RF,Y2SHR-Y1SHR                                                   
         MR    RE,RE                                                            
         L     RE,AIO2                                                          
         LA    RE,Y1SHR-IOAREA2(RE)                                             
         AR    RE,RF               POINT TO SHARE SLOT                          
         LA    R4,NEWRTG-OLDUNV                                                 
         LA    R5,NEWHPT-OLDUNV                                                 
         L     R6,AIO2                                                          
         AR    R4,R6               RATINGS                                      
         AR    R5,R6               PUTS                                         
         LA    R1,NUMVALS*2                                                     
*                                                                               
         CLI   Y4SSW,C'S'          SHARES NEED PRECISION                        
         BE    SLOTSHR             THIS IS CONFUSING BUT NEEDED                 
*                                                                               
         CLI   DBTAPEP,C'Y'        IF RATING BASED ITS OK                       
         BNE   SLOTSHR                                                          
         LA    R1,NUMVALS                                                       
         LR    RF,R6                                                            
CALCPCT  DS    0H                  GET RATING/PUT PERCENTS                      
         CLC   0(4,RF),=XL4'00'     SO USER CAN TRACK WHATS GOING ON            
         BE    CALCPCT2                                                         
*                                                                               
         L     R7,0(RF)                *                                        
         SR    R6,R6                   *                                        
         A     R7,=F'5'                *                                        
         D     R6,=F'10'               *                                        
         MH    R7,=H'10'               *                                        
         ST    R7,0(RF)                *                                        
*                                                                               
         L     R7,0(R4)            RATINGS                                      
         M     R6,=F'10'                                                        
         SLDA  R6,1                                                             
         D     R6,0(RF)                                                         
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,0(R4)                                                         
*                                                                               
*******************************************************************             
* CANT USE THIS ANYMORE - WE OVERFLOW THE INSTRUCTION FOR BROAD   *             
* DAYTIME EX M-SU/6A-2A ,WABC                                     *             
* WELL LETS CHECK EXTBUFF FLAG - IF ITS TURNED ON WE'LL USE       *             
* THE NEW PACK INSTRUCTIONS.  WE CAN IN THEORY ALWAYS BYPASS      *             
* THIS AND USE THE NEW PACK INSTRUCTIONS BUT IF WE CHECK EXTBUFF  *             
* FLAG WE CAN TURN EVERYTHING OFF AND GO BACK TO THE OLD SINGLE   *             
* REGISTER INSTRUCTIONS WHICH WOULD JUST REVERSE MY FIX FOR       *             
* BROAD DAYTIME ACCUMULATION OVERFLOWING THE OLD BUFFERS.         *             
*                                                                 *             
         CLI   EXTBUFF,C'Y'        USE EXTENDED BUFF?             *             
         BE    CALPCT1B                                           *             
*                                                                 *             
         L     R7,0(R5)            PUTS                           *             
         M     R6,=F'10'                                          *             
         SLDA  R6,1                                               *             
         D     R6,0(RF)                                           *             
         A     R7,=F'1'                                           *             
         SRL   R7,1                                               *             
*                                                                 *             
         B     CALPCT1G                                           *             
*******************************************************************             
*  USING PACK INSTRUCTIONS TO PREVENT OVERFLOWING SINGLE REGISTER               
*                                                                               
CALPCT1B L     R7,0(R5)            PUTS                                         
         XC    DUB2,DUB2                                                        
* HAVE TO TEST IF HIGH ORDE BIT IS ON *                                         
         TMH   R7,X'8000'                                                       
         BZ    CALPCT1D                                                         
* IF HIGH ORDER BIT IF ON JUST DEVIDE BY TWO AND LOSE A LITTLE                  
* PRECISION                                                                     
*                                                                               
         SR    R6,R6                                                            
         SRL   R7,1                                                             
         CVD   R7,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         XC    DUB3,DUB3                                                        
         MVC   DUB3(2),=X'040C'   MULTIPLY BY 20*2(LOST PRECSISON)              
         B     CALPCT1F                                                         
                                                                                
CALPCT1D CVD   R7,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         XC    DUB3,DUB3                                                        
         MVC   DUB3(2),=X'020C'   MULTIPLY BY 20                                
                                                                                
CALPCT1F XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB2                                                   
         MP    WORK(12),DUB3(2)                                                 
                                                                                
         XC    DUB3,DUB3           DIVIDE BY                                    
         ZICM  R7,0(RF),(15)                                                    
         CVD   R7,DUB3             UNIV                                         
         OI    DUB3+7,X'0C'                                                     
         DP    WORK(12),DUB3+4(4)                                               
         MVC   DUB2(8),WORK                                                     
*                                                                               
         XC    DUB3,DUB3                                                        
         MVC   DUB3+7(1),=P'1'      ADD ONE                                     
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB2                                                   
         AP    WORK(12),DUB3(8)    CANT GET THIS TO WORK YET                    
         MVC   DUB2(8),WORK+4                                                   
*                                                                               
         XC    DUB3,DUB3                                                        
         MVC   DUB3+6(2),=X'002C'   +1                                          
         XC    WORK,WORK          DIVIDE BY 2                                   
         MVC   WORK+4(8),DUB2                                                   
         DP    WORK(12),DUB3+4(4)  CANT GET THIS TO WORK YET                    
         MVC   DUB2(8),WORK                                                     
         SR    R6,R6                                                            
         CVB   R7,DUB2                                                          
         SR    R6,R6                                                            
**************************************************************                  
CALPCT1G ST    R7,0(R5)                                                         
CALCPCT2 LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,CALCPCT                                                       
*                                                                               
         LA    R4,NEWRTG-OLDUNV    RESET FOR AVERAGING                          
         LA    R5,NEWHPT-OLDUNV                                                 
         L     R6,AIO2                                                          
         AR    R4,R6               RATINGS                                      
         AR    R5,R6               PUTS                                         
         LA    R1,NUMVALS*2                                                     
*                                                                               
SLOTSHR  L     R7,0(R4)            DO THE AVERAGES FOR RTGS                     
         SR    R6,R6                SO I USE THE VALUES SHOWN BY APPS           
         SLDA  R6,1                                                             
         D     R6,DUB                                                           
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(R4)                                                         
*                                                                               
         L     R7,0(R5)            DO THE AVERAGES FOR PUTS                     
         SR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,DUB                                                           
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(R5)                                                         
*                                                                               
         L     R7,0(R4)            CALC THE SHARE AND TSA SHARE                 
         LA    R6,2000                                                          
         MR    R6,R6                                                            
         ICM   R8,15,0(R5)                                                      
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     *+6                                                              
         DR    R6,R8                                                            
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(RE)            SAVE IN YEAR SLOT                            
*                                                                               
         CLI   Y4SSW,C'P'          DOING A PUT INDEX                            
         BNE   *+10                                                             
         MVC   0(4,RE),0(R5)       SAVE THE PUTS - NOT THE SHARES               
         LA    R7,4                SET UP NEXT SLOT                             
         AR    R4,R7                                                            
         AR    R5,R7                                                            
         AR    RE,R7                                                            
         BCT   R1,SLOTSHR          DO 'EM ALL                                   
*                                                                               
         CLI   Y4SSW,C'S'          ONLY IF WE'RE SAVING THE SHARES              
         BNE   SLOTSHR3X                                                        
         SHI   RE,OLDRTGLN         GO BACK AND SEED ORIG HOME SHARES            
         AHI   RE,(DISPHOM*4)       RE-->SLOTS FOR HOMES VALUES                 
         LA    R5,TOTHMSHR          ACCUMULATED ORIG HOME SHARES                
         LHI   R1,(HOMSHRLN/4)      LOOP COUNTER                                
SLOTSHR3 L     R7,0(R5)                                                         
         SR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,DUB               UNWEIGH                                     
         AHI   R7,1                                                             
         SRA   R7,1                                                             
         ST    R7,0(RE)                                                         
         AHI   R5,4                                                             
         AHI   RE,4                                                             
         BCT   R1,SLOTSHR3                                                      
SLOTSHR3X EQU  *                                                                
*                                                                               
         L     R7,SAVER7                                                        
         L     RE,AIO2             RESET FOR NEXT YEAR                          
         LA    RF,IOAREA2X-IOAREA2                                              
         XCEF                                                                   
*&&DO                                                                           
         L     R0,AIO2             RESET FOR NEXT YEAR                          
         LHI   R1,IOAREA2X-IOAREA2                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*&&                                                                             
         XC    DUB,DUB                                                          
         XC    TOTHMSHR,TOTHMSHR                                                
         CLI   MMUPASS,0           METER MARKET PARALLEL                        
         BH    MMULOOP              SAME BOOK-DIFF BTYPE                        
* CHECK TO SEE IF WE ARE DOING BOOK INDEX UPGRADES PAB,SAB                      
         CLI   YRSAVTYP,C'B'       SAB/PAB ?                                    
         BNE   PABLOOPX                                                         
         CLI   HOOKFLAG,C'Y'       IF WE DIDNT GO INTO HOOK WE MUST NOT         
         BE    *+8                 HAVE FOUND THE BOOK SO RESET COUNTER         
         AHI   R0,1                TO +1 FOR AN EXTRA BOOK READ                 
*                                                                               
         ZIC   RE,DBSELBK+1        GRAB MONTH                                   
         SHI   RE,1                                                             
         CHI   RE,0                IF WE ALREADY PROCESSED JANUARY              
         BH    PABLOOP             THEN GO TO DEC OF PREVIOUS YR                
         LHI   RE,12                                                            
         ZIC   RF,DBSELBK                                                       
         SHI   RF,1                                                             
         STC   RF,DBSELBK                                                       
* PREVENTIVE CODE TO PREVENT INFINITE LOOP                                      
         CLI   DBSELBK,80          NEVER GO BACK PAST 1970                      
         BL    GETYRS20                                                         
PABLOOP  STC   RE,DBSELBK+1                                                     
         BCT   R0,GTYRS17A                                                      
*                                                                               
PABLOOPX EQU   *                                                                
         ZIC   RE,DBSELBK          DECREMENT THE YEAR                           
         BCTR  RE,0                                                             
         STC   RE,DBSELBK                                                       
                                                                                
         BCT   R0,GETYRS17         REPEAT FOR N PREV YEARS                      
         B     GETYRS20                                                         
MMULOOP  CLI   MMUPASS,2           SECOND PASS                                  
         BE    GETYRS20             YES - EXIT                                  
         MVI   MMUPASS,2            NO  - SET FOR IT                            
         CLI   DBBTYPE,C'I'        IF HISPANIC LPM                              
         BNE   MMULOOP2                                                         
         MVI   DBBTYPE,C'H'        SET TO HISPANIC REG                          
         B     *+8                                                              
MMULOOP2 MVI   DBBTYPE,0           OTHERWISE SET TO REG                         
         BCT   R0,GTYRS17H         DECREMENT FOR PROPER SLOTTING                
         B     GTYRS17H                                                         
         SPACE 2                                                                
*                                  ADD DEMO RECORD INTO COMPOSITE               
GETYRS18 NTR1                                                                   
*---->   L     R1,DBAQUART         IGNORE 2 WEEK ONLY DATA                      
*---->   TM    QHWKS-QHELEM(R1),X'10'                                           
*---->   BNZ   UPX2                                                             
         MVI   HOOKFLAG,C'Y'           YES WE GOT INTO THE HOOK !!              
                                                                                
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),AIO2                                     
         LA    R0,(OLDHPTX-OLDRTG)/4                                            
         L     R4,AIO2                                                          
         LA    R4,OLDRTG-OLDUNV(R4)    POINT TO CURR RATINGS                    
         LA    R5,NEWRTG-OLDRTG        AND OFFSET TO SUMMED RATINGS             
         AR    R5,R4                                                            
GETYRSML L     RF,0(R4)                                                         
         ST    RF,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,GETYRSML                                                      
         LH    R1,DBFACTOR                                                      
         A     R1,DUB                                                           
         ST    R1,DUB                                                           
*                                                                               
         DS    0H                  GET ORIGINAL HOME SHARES FROM RECD           
         MVC   SVDBACBK,DBACTBK                                                 
         L     R4,AIO2                                                          
         AHI   R4,(HOMSHR-UPREC)    R4-->OUTPUT AREA                            
         XC    0(HOMSHRLN,R4),0(R4)                                             
         OC    DBAQUART,DBAQUART                                                
         BZ    GETYRSSHX                                                        
*  FOR WIRED BOOKS RECALCULATE THE HOME SHARES BY IMP/HUT                       
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,(R4),0                        
*                                                                               
**       CLI   Y4SSW,C'S'          ONLY IF WE'RE SAVING THE SHARES              
**       BNE   GETYR19D                                                         
         CLI   DBSELMED,C'T'       NSI USTV                                     
         BE    GETYR19A            ALWAYS RELOOK HOMESHARE                      
         CLI   DBSELSRC,C'F'       FOR LPM MKTS UNDER FUSION                    
         BE    GETYR19A                                                         
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   GETYR19D                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   GETYR19D                                                         
GETYR19A LHI   R0,3                                                             
         LA    R3,TOTHMSHR                                                      
         L     RF,AIO2                                                          
         LA    R5,OLDHPT-OLDUNV(RF)                                             
         AHI   R5,(DISPHOM*4)                                                   
         LA    R6,OLDRTG-OLDUNV(RF)                                             
         AHI   R6,(DISPHOM*4)                                                   
GETYR19B L     R1,0(R5)               HUT                                       
         L     RF,0(R6)               HOMES                                     
*****    LA    RE,2000                                                          
         OR    R1,R1                                                            
         BZ    GETYR19C                                                         
         XC    WORK,WORK                                                        
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                HOMES                                     
         OI    DUB2+7,X'0C'                                                     
         MVC   WORK(3),=X'02000C'     *2000                                     
         MP    DUB2(8),WORK(3)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB2                                                   
         XC    DUB2,DUB2                                                        
         CVD   R1,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         DP    WORK(16),DUB2(8)                                                 
         MVC   DUB2(8),WORK                                                     
         CVB   RF,DUB2                                                          
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         MH    RF,DBDIVSOR                                                      
         LR    RE,RF                                                            
         ST    RE,0(R4)               HOME SHARES                               
         ST    RE,0(R3)               GETIUN ACCUMULATED VALUES                 
GETYR19C AHI   R4,4                   BUMP-HOMSHR                               
         AHI   R5,4                   BUMP-HUT                                  
         AHI   R6,4                   BUMP-DMA IMP                              
         AHI   R3,4                   BUMP-TOTHOMSHR                            
         BCT   R0,GETYR19B                                                      
         B     GETYRSSHX                                                        
*                                                                               
GETYR19D MVC   DBACTBK,SVDBACBK                                                 
         LHI   R0,3                                                             
         LA    R5,TOTHMSHR                                                      
GETYRSSHG DS   0H                                                               
         L     RF,0(R4)                                                         
         MH    RF,DBFACTOR                                                      
         A     RF,0(R5)                                                         
         ST    RF,0(R5)                                                         
         AHI   R4,4                                                             
         AHI   R5,4                                                             
         BCT   R0,GETYRSSHG                                                     
GETYRSSHX EQU  *                                                                
         B     UPX2                                                             
         SPACE 2                                                                
*       CALCULATE THE YEAR TO YEAR INDEXES                                      
*                                                                               
*                                                                               
GETYRS20 L     RE,AIO2                                                          
         LA    RE,Y1SHR-IOAREA2(RE)                                             
         ST    RE,DMCB                                                          
         ST    RE,DMCB+8           SAVE SUM AREA ADDR                           
         LR    R5,RE               SET SUM AREA                                 
         LA    RF,Y2SHR-Y1SHR(RE)                                               
         MVI   DMCB+4,1            FIRST TIME SWITCH                            
         ZIC   R0,Y4SYRS                                                        
         BCTR  R0,0                                                             
         LA    R1,(Y2SHR-Y1SHR)/4                                               
GETYRS21 LA    R6,2000             CALC THE YR TO YR INDEXES                    
         L     R7,0(RF)                                                         
         CLI   Y4ASW,C'Y'          DOING AVERAGE NOT INDEX                      
         BE    GETYRA21                                                         
         MR    R6,R6                                                            
         CLC   0(4,RE),=XL4'00'                                                 
         BNE   *+12                                                             
         LA    R7,2000                                                          
         B     *+8                                                              
         D     R6,0(RE)                                                         
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         CLI   DMCB+4,1            IF NOT FIRST TIME                            
         BE    GETYRM21                                                         
GETYRA21 DS    0C                  SUM THE INDEXES OR VALUES                    
         CLI   Y4MSW,C'Y'          OR MULIPLY THEM                              
         BNE   *+12                                                             
         MH    R7,2(R5)            MULT FOR GEOMETRIC AVERAGES                  
         B     *+8                                                              
         A     R7,0(R5)            SUM THE INDEXES OR VALUES                    
GETYRM21 ST    R7,0(R5)                                                         
         LA    R5,4(R5)            SUM AREA                                     
         LA    RE,4(RE)            DENOMINATOR                                  
         LA    RF,4(RF)            NUMERATOR                                    
         BCT   R1,GETYRS21                                                      
         L     R7,SAVER7                                                        
         L     R5,DMCB+8           SUM AREA                                     
         MVI   DMCB+4,0            RESET FIRST TIME                             
         LA    R1,(Y2SHR-Y1SHR)/4  SET UP FOR THE NEXT SET                      
         L     RE,DMCB                                                          
         LA    RE,Y2SHR-Y1SHR(RE)                                               
         ST    RE,DMCB                                                          
         LA    RF,Y2SHR-Y1SHR(RE)                                               
         BCT   R0,GETYRS21                                                      
         SPACE 2                                                                
*        NOW AVERAGE THE INDEXES                                                
         L     RE,DMCB+8           SUM AREA                                     
         LA    R1,(Y2SHR-Y1SHR)/4  SET UP FOR THE NEXT SET                      
         ZIC   R7,Y4SYRS                                                        
*   PAQ/SAQ TAKE LESSER OF Y4SYRS2 AND Y4SYRS                                   
         CLI   YRSAVTYP,C'G'       PAVG/SAVG                                    
         BE    *+8                                                              
         CLI   YRSAVTYP,C'Q'       SAQ/PAQ ?                                    
         BNE   GETYRN21                                                         
         CLC   Y4SYRS,Y4SYRS2                                                   
         BNH   GETYRN21                                                         
         OC    Y4SYRS2,Y4SYRS2     EXIT IF NOTHING FOUND                        
         BZ    UPX2                                                             
         ZIC   R7,Y4SYRS2                                                       
*                                                                               
GETYRN21 CLI   Y4ASW,C'Y'          AVERAGES                                     
         BE    *+6                                                              
         BCTR  R7,0                INDEXES 1 LESS                               
         ST    R7,DMCB                                                          
*                                                                               
         CLI   Y4MSW,C'Y'          GEOMETRIC INDEX                              
         BNE   GETYRS22                                                         
         LA    R6,1                                                             
         B     *+8                                                              
         MH    R6,=H'1000'         DETERMINE THE DIVISOR                        
         BCT   R7,*-4                                                           
         ST    R6,DMCB                                                          
*                                                                               
GETYRS22 L     R7,0(RE)            SUM OF INDEXES/AVERAGES                      
         SR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,DMCB             DIVIDE BY YEARS                              
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,GETYRS22                                                      
         L     R7,SAVER7                                                        
         CLI   Y4SSW,C'P'                                                       
         BE    GETYRS30                                                         
         SPACE 2                                                                
*        NOW INDEX THE CURRENT VALUES TO THIS                                   
*        1. CALCULATE THE OLD SHARE                                             
*        2. INDEX THE OLD SHARE                                                 
*        3. OLD PUT X INDEXED SHARE = OLD RATING/IMP                            
*        3. NEW PUT X INDEXED SHARE = NEW RATING/IMP                            
*                                                                               
*   RECALCULATE HOMESHR FOR WIRED CABLE ********************'                   
*   BECAUSE HOME SHARES ON RECORD IS NOT BASED OFF DMA                          
*                                                                               
*                                                                               
         CLI   DBSELSRC,C'F'          FOR LPM MKTS REQUESTING                   
         BE    GETYR22A               UNDER FUSION                              
         OC    SPUPSYSC,SPUPSYSC                                                
         BNZ   GETYR22A                                                         
         OC    DBSELSYC,DBSELSYC                                                
         BNZ   GETYR22A                                                         
         CLI   DBBTYPE,C'4'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'Z'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   GETYR22F                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   GETYR22F                                                         
GETYR22A LHI   R0,3                                                             
         LA    R3,HOMSHR                                                        
         LA    RF,UPREC                                                         
         LA    R5,OLDHPT-OLDUNV(RF)                                             
         AHI   R5,(DISPHOM*4)                                                   
         LA    R6,OLDRTG-OLDUNV(RF)                                             
         AHI   R6,(DISPHOM*4)                                                   
GETYR22B L     R1,0(R5)               HUT                                       
         L     RF,0(R6)               HOMES                                     
*****    LA    RE,2000                                                          
         OR    R1,R1                                                            
         BZ    GETYR22C                                                         
         XC    WORK,WORK                                                        
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                HOMES                                     
         OI    DUB2+7,X'0C'                                                     
         MVC   WORK(3),=X'02000C'     *2000                                     
         MP    DUB2(8),WORK(3)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB2                                                   
         XC    DUB2,DUB2                                                        
         CVD   R1,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         DP    WORK(16),DUB2(8)                                                 
         MVC   DUB2(8),WORK                                                     
         CVB   RF,DUB2                                                          
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R3)               GETIUN ACCUMULATED VALUES                 
GETYR22C AHI   R5,4                   BUMP-HUT                                  
         AHI   R6,4                   BUMP-DMA IMP                              
         AHI   R3,4                   BUMP-HOMESHR                              
         BCT   R0,GETYR22B                                                      
*                                                                               
GETYR22F L     RE,AIO2                                                          
         LA    RE,Y1SHR-IOAREA2(RE)                                             
         L     RF,AIO2                                                          
         AHI   RF,(OLDRTG-UPREC)                RF-->OLD RTG WORK AREA          
         LA    R8,(OLDHPT-OLDRTG)(RF)           R8-->OLD HPT WORK AREA          
         MVC   0(OLDRTGLN,RF),OLDRTG                                            
         MVC   0(OLDHPTLN,R8),OLDHPT                                            
         MVC   (DISPHOM*4)(HOMSHRLN,RF),HOMSHR  FUDGE TO USE ORIGINAL           
*                                                                               
         LHI   R1,1000                                                          
         STCM  R1,15,((DISPHOM+0)*4)(R8)         HOMES SHARES IN                
         STCM  R1,15,((DISPHOM+1)*4)(R8)         INDEXING                       
         STCM  R1,15,((DISPHOM+2)*4)(R8)         AND CALCULATING                
         LA    R0,(OLDRTGX-OLDRTG)/4                                            
         SR    R1,R1                                                            
*                                                                               
GETYRS23 CLI   Y4ASW,C'Y'          AVERAGE NOT INDEX                            
         BNE   *+12                                                             
         L     R7,0(RE)                                                         
         B     GETYRA23                                                         
*                                                                               
         SR    R6,R6                                                            
         LHI   R7,2000                                                          
         M     R6,0(RF)            OLDRTG                                       
         CLC   0(4,R8),=XL4'00'                                                 
         BNE   *+12                                                             
         LA    R7,1                                                             
         B     *+8                                                              
         D     R6,0(R8)            OLDHPT (SHARE CALC)                          
         AH    R7,=H'1'                                                         
         SRL   R7,1                                                             
         SR    R6,R6                                                            
         M     R6,0(RE)            NOW INDEX IT                                 
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
GETYRA23 ST    R7,DMCB             SAVE INDEXED OR AVG SHARE                    
         SR    R6,R6                                                            
         M     R6,OLDHPT(R1)       OLDHPT X INDEXED SHAREÙ                     
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         ST    R7,OLDRTG(R1)       OLDRTG (BASED ON INDEXED SHARE)              
         SR    R6,R6                                                            
         L     R7,DMCB             INDEXED SHARE                                
         M     R6,NEWHPT(R1)       NEWHPT X INDEXED SHAREÙ                     
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         ST    R7,NEWRTG(R1)       NEW RATING (BASED ON INDEXED SHR)            
         LA    R7,4                BUMP TO NEXT FIELDS                          
         AR    RE,R7                                                            
         AR    RF,R7                                                            
         AR    R8,R7                                                            
         AR    R1,R7                                                            
         BCT   R0,GETYRS23                                                      
*&&DO                                                                           
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
*&&                                                                             
         L     RE,AIO2                                                          
         LA    RE,(Y1SHR-IOAREA2)(RE)                                           
         AHI   RE,(DISPHOM*4)       RE-->INDICES FOR HOMES SHARES               
         LA    RF,HOMSHR            RF-->ORIG HOMES SHARE NEED INDEXING         
         LHI   R0,HOMSHRLN/4                                                    
                                                                                
GETYRS23SH DS  0H                                                               
         L     R7,0(RE)             ASSUME 0(RE) = AVG SHR VALUE                
         CLI   Y4ASW,C'Y'                                                       
         BE    GETYRA23SH           IF IT IS, THEN JUST SEED IT IN              
                                                                                
         L     R7,0(RF)             R7 = ORIGINAL HOME SHARE                    
         SR    R6,R6                                                            
         M     R6,0(RE)             INDEX IT                                    
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
GETYRA23SH ST  R7,0(RF)             AND PUT IT BACK                             
         AHI   RE,4                                                             
         AHI   RF,4                                                             
         BCT   R0,GETYRS23SH                                                    
*                                                                               
         L     R7,SAVER7           RESET DBLOCK POINTER                         
*                                  RESET ORIGINAL DBLOCK VALUES                 
GETYRS24 XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,SPUPAREC                                                  
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBFILE,FILEDEM                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
*&&DO                                                                           
         STCM  R9,15,DBCOMFCS                                                   
*&&                                                                             
         MVC   DBCOMFCS,VCOMFACS                                                
         B     UPX2                                                             
*                                                                               
GETYRS30 L     RE,AIO2                                                          
         LA    RE,Y1SHR-IOAREA2(RE)                                             
         LA    R3,OLDUNV           THESE POINT TO SHARE BOOK VALUES             
         LA    RF,OLDRTG                                                        
         LA    R8,OLDHPT                                                        
         LA    R1,NEWHPT                                                        
         LA    R4,NEWRTG                                                        
         LA    R0,(OLDRTGX-OLDRTG)/4                                            
GETYRS33 L     R7,0(RF)            OLDRTG                                       
         LA    R6,2000                                                          
         MR    R6,R6                                                            
         CLC   0(4,R8),=XL4'00'                                                 
         BNE   *+12                                                             
         LA    R7,1                                                             
         B     *+8                                                              
         D     R6,0(R8)            OLDHPT (SHARE CALC)                          
         AH    R7,=H'1'                                                         
         SRL   R7,1                                                             
         ST    R7,DMCB             SAVE  SHARE                                  
*                                                                               
         CLI   Y4ASW,C'Y'          DOING AVERAGE                                
         BNE   *+12                                                             
         L     R7,0(RE)            YES JUST REPLACE IT                          
         B     GETYRA33                                                         
*                                                                               
         L     R7,0(R1)            NEWHPT                                       
         SR    R6,R6                                                            
         BAS   R2,GYRSITOR                                                      
         M     R6,0(RE)            NOW INDEX IT                                 
         AH    R7,=H'500'                                                       
         D     R6,=F'1000'                                                      
GETYRA33 BAS   R2,GYRSRTOI                                                      
         ST    R7,0(R1)            INDEXED HPT                                  
         L     R6,DMCB             SHARE                                        
         MR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(R4)            NEW RATING (BASED ON INDEXED PUT)            
         LA    R7,4                BUMP TO NEXT FIELDS                          
         AR    RE,R7                                                            
         AR    RF,R7                                                            
         AR    R8,R7                                                            
         AR    R1,R7                                                            
         AR    R4,R7                                                            
         AR    R3,R7                                                            
         BCT   R0,GETYRS33                                                      
         L     R7,SAVER7                                                        
*                                                                               
*        MVC   SAVRTG(OLDRTGLN),OLDRTG BECAUSE IT GETS RESET                    
*                                                                               
         B     GETYRS24                                                         
         SPACE 2                                                                
UPX2     XIT1  ,                   EXIT FROM MODULE                             
         SPACE 2                                                                
GYRSITOR CLI   TAPEOPT,C'Y'        CONVERT IMPS TO PERCENT                      
*        BR    R2                                                               
         BNER  R2                                                               
         CH    R0,=H'33'                                                        
         BLR   R2                                                               
         CLC   0(4,R3),=XL4'00'                                                 
         BER   R2                                                               
         STM   RE,RF,SAVEMORE                                                   
         L     RF,0(R3)                                                         
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         MH    RF,=H'10'                                                        
         M     R6,=F'20'                                                        
         DR    R6,RF                                                            
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         SR    R6,R6                                                            
         LM    RE,RF,SAVEMORE                                                   
         BR    R2                                                               
         SPACE 1                                                                
GYRSRTOI CLI   TAPEOPT,C'Y'        CONVERT PECENT TO IMPS                       
*        BR    R2                                                               
         BNER  R2                                                               
         CH    R0,=H'33'                                                        
         BLR   R2                                                               
         STM   RE,RF,SAVEMORE                                                   
         L     RF,0(R3)                                                         
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         MH    RF,=H'10'                                                        
         SR    R6,R6                                                            
         MR    R6,RF                                                            
         AH    R7,=H'5'                                                         
         D     R6,=F'10'                                                        
         SR    R6,R6                                                            
         LM    RE,RF,SAVEMORE                                                   
         BR    R2                                                               
         EJECT                                                                  
QTRBKTBQ DC    X'000200050007000B000200050007000BFFFF'                          
*********************************************************************           
* GETDAY - CONVERT INTERNAL DAY CODE INTO DEMAND DAY CODE FORMAT                
*********************************************************************           
*                                                                               
GETDAY   DS    0H                                                               
         LA    R1,DAYTAB           LOOKUP DAY IN TABLE                          
         NI    DUB,X'F0'                                                        
GETDAY22 CLI   0(R1),X'FF'                                                      
         BE    GETDAY24                                                         
         CLC   0(1,R1),DUB                                                      
         BE    GETDAY24                                                         
         LA    R1,2(R1)                                                         
         B     GETDAY22                                                         
GETDAY24 MVC   DUB+4(1),1(R1)                                                   
         B     XIT01               RETURN TO CALLER                             
*                                                                               
DAYTAB   DC    X'007C1040202030104008500460027001FF7F'                          
*                                                                               
         SPACE 1                                                                
*********************************************************************           
* CONVERT START QUARTER HOUR & DURATION INTO MILITARY START & END TIMES         
*********************************************************************           
*                                                                               
GETTIME  DS    0H                                                               
         ZIC   R1,DUB              R1=START QUARTER HOUR                        
         ZIC   RF,DUB+1                                                         
         AR    RF,R1                                                            
         BAS   RE,GETIME22         CONVERT START TO MILITARY                    
         STH   R1,DUB+4                                                         
         LR    R1,RF                                                            
         BAS   RE,GETIME22         CONVERT END TO MILITARY                      
         STH   R1,DUB+6                                                         
         B     XIT01               RETURN TO CALLER                             
         SPACE 1                                                                
*                                                                               
GETIME22 DS    0H                  CONVERT REL QHR INTO MILIT TIME              
         SR    R0,R0                                                            
         D     R0,=F'4'            R1=HOURS, R0=QUARTER HOURS                   
         MH    R1,=H'100'          CALCULATE MILITARY HOUR                      
         MH    R0,=H'15'           CALCULATE MILITARY QUARTER HOUR              
         AR    R1,R0               R1=MILITARY TIME                             
         AH    R1,=H'600'          ADD BASE TIME (6AM)                          
         CH    R1,=H'2400'         TEST IF GR MIDNIGHT                          
         BNH   *+8                                                              
         SH    R1,=H'2400'         YES - SUBTRACT 24 HOURS                      
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* WGHTUP - WEIGHT AND ACCUMULATE DEMO VALUES                          *         
* RESTORE REGS FROM PARAM LIST:                                       *         
* R0 = NUMBER OF ENTRIES TO WEIGHT & ACCUMULATE                       *         
* R1 = A(INPUT VALUES)                                                *         
* R2 = A(ACCUMULATOR VALUES)                                          *         
***********************************************************************         
         SPACE 1                                                                
WGHTUP   DS    0H                                                               
         L     R4,PARM01                                                        
         LM    R0,R2,4(R4)         RESTORE REGS FROM PARM LIST                  
         SR    R4,R4                                                            
         ICM   R4,3,DBFACTOR                                                    
         SR    R3,R3                                                            
WGHTUP2  L     RF,0(R1,R3)         RF=DEMO VALUE                                
         CLI   TAPEOPT,C'Y'        TAPE BASED                                   
         BE    WGHTUP3              NO ROUNDING                                 
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
WGHTUP3  MR    RE,R4               RF=(VALUE+5/10*10)*WEIGHT                    
         ST    RF,0(R1,R3)         SET WEIGHTED VALUE IN INPUT                  
*&&DO                                                                           
         A     RF,0(R2,R3)         AND ACCUMULATE WEIGHTED VALUE                
*&&                                                                             
                                                                                
WGHTUP10 AL    RF,0(R2,R3)         AND ACCUMULATE WEIGHTED VALUE                
         BC    12,*+6              IF RESULTED IN A CARRY,                      
         DC    H'0'                 DIE (FOR NOW)                               
         ST    RF,0(R2,R3)                                                      
*                                                                               
         LA    R3,4(R3)                                                         
         BCT   R0,WGHTUP2                                                       
         B     XIT01                                                            
         SPACE 1                                                                
***********************************************************************         
* WGHTUP - WEIGHT AND ACCUMULATE DEMO VALUES  FOR MULTIBOOK           *         
* RESTORE REGS FROM PARAM LIST:                                       *         
* R0 = NUMBER OF ENTRIES TO WEIGHT & ACCUMULATE                       *         
* R1 = A(INPUT VALUES)                                                *         
* R2 = A(ACCUMULATOR VALUES)                                          *         
***********************************************************************         
         SPACE 1                                                                
MBKWGTUP DS    0H                                                               
         L     R4,PARM01                                                        
         LM    R0,R2,4(R4)         RESTORE REGS FROM PARM LIST                  
         SR    R4,R4                                                            
         ICM   R4,3,DBFACTOR                                                    
         SR    R3,R3                                                            
         SR    R5,R5                                                            
MBKWTUP2 L     RF,0(R1,R3)         RF=DEMO VALUE                                
                                                                                
         CLI   TAPEOPT,C'Y'                                                     
         BE    MBKWTUP3                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
                                                                                
MBKWTUP3 MR    RE,R4               RF=(VALUE+5/10*10)*WEIGHT                    
         ST    RF,0(R1,R3)         SET WEIGHTED VALUE IN INPUT                  
*   PACK THE NUMBER FOR MULTIBOOK AVERAGE SO WE DONT OVERFLOW                   
*   USE ADD PACK DECIMAL INSTRUCTION                                            
         XC    DUB,DUB             PACK INPUT  VALUE                            
                                                                                
********************************************************************            
*      WE HAVE TO CHECK HIGH ORDER BIT TO MAKE SURE                             
*      WE DONT OVERFLOW THE AND GET A NEGATIVE NUMBER                           
*      CVD MUST BE IN RANGE                                                     
*      IF WE EXCCEEDED A SINGLE REGISTER BOUNDARY-                              
*      GET HALF THE BINARY VALUE INTO PACK AND MULTIPLY BY P'2'                 
         TMH   RF,X'8000'                                                       
         BNO   MBKWTUP6                                                         
         SR    RE,RE                                                            
         SRL   RF,1                DIVIDE BY 2                                  
         CVD   RF,DUB              DUB = HALF PACK DECIMAL                      
         XC    DUB2,DUB2                                                        
         MVC   DUB2(2),=X'002C'    NOW MUTPLIPY BY 2 PACK                       
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB                                                    
         MP    WORK(12),DUB2(2)                                                 
         MVC   DUB(8),WORK+4       NOW DUB=PACK DECIMAL WE WANT                 
         B     MBKWTUP7                                                         
*********************************************************************           
MBKWTUP6 CVD   RF,DUB                                                           
         XC    DUB2,DUB2                                                        
MBKWTUP7 LA    RF,0(R2,R5)                                                      
         MVC   DUB2,0(RF)                                                       
         OI    DUB2+7,X'0C'                                                     
*                                                                               
*                                                                               
         AP    DUB(8),DUB2(8)                                                   
         LA    RF,0(R2,R5)                                                      
         MVC   0(8,RF),DUB                                                      
         LA    R3,4(R3)                                                         
         LA    R5,8(R5)            BUMP TOTAL ACCUMLATOR BUFFER                 
         BCT   R0,MBKWTUP2                                                      
         B     XIT01                                                            
                                                                                
         SPACE 1                                                                
***********************************************************************         
* SUBROUTINE TO UNWEIGHT ACCUMULATED DEMO VALUES                      *         
* R0 = NUMBER OF VALUES TO UNWEIGHT                                   *         
* R1 = A(ACCUMULATOR VALUES)                                          *         
* R2 = A(UNWEIGHTED ACCUMULATOR VALUES) OR ZERO IF SAME AS INPUT      *         
***********************************************************************         
         SPACE 1                                                                
UNWGHT   DS    0H                                                               
         L     R4,PARM01                                                        
         LM    R0,R2,4(R4)         RESTORE REGS FROM PARM LIST                  
         SR    R4,R4                                                            
         ICM   R4,3,DBDIVSOR                                                    
         SR    R3,R3                                                            
UNWGHT2  L     RE,0(R1,R3)                                                      
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         DR    RE,R4               RF=VALUE/DBDIVSOR (ROUNDED)                  
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LTR   R2,R2               TEST ACCUMULATORS GIVEN                      
         BNZ   *+12                                                             
         ST    RF,0(R1,R3)         NO - STORE UNWEIGHTED VALUE                  
*                                                                               
         B     *+12                                                             
         A     RF,0(R2,R3)         YES - ACCUMULATE UNWEIGHTED VALUE            
         ST    RF,0(R2,R3)                                                      
         LA    R3,4(R3)                                                         
         BCT   R0,UNWGHT2                                                       
         B     XIT01                                                            
                                                                                
***********************************************************************         
* SUBROUTINE TO UNWEIGHT ACCUMULATED DEMO VALUES FOR MBK              *         
* R0 = NUMBER OF VALUES TO UNWEIGHT                                   *         
* R1 = A(ACCUMULATOR VALUES)                                          *         
* R2 = A(UNWEIGHTED ACCUMULATOR VALUES) OR ZERO IF SAME AS INPUT      *         
***********************************************************************         
*                                                                               
MBKUNWGT DS    0H                                                               
         L     R4,PARM01                                                        
         LM    R0,R2,4(R4)         RESTORE REGS FROM PARM LIST                  
         SR    R4,R4                                                            
         ICM   R4,3,DBDIVSOR                                                    
         SR    R3,R3                                                            
         SR    R5,R5                                                            
MBKUNWG8 XC    DUB,DUB                                                          
         LA    RF,0(R1,R5)         GET ACUMMULATED VALUES                       
         MVC   DUB,0(RF)                                                        
         XC    DUB2,DUB2                                                        
         MVC   DUB2(2),=X'002C'   MULTIPLY BY 2                                 
****     MP    DUB(8),DUB2(2)     NOT BIG ENOUGH USE WORK                       
********************************                                                
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB                                                    
         MP    WORK(12),DUB2(2)                                                 
         MVC   DUB(8),WORK+4                                                    
*********************************                                               
         ZICM  RF,DBDIVSOR,(3)                                                  
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB                                                    
         DP    WORK(12),DUB2+4(4)                                               
         MVC   DUB(8),WORK                                                      
         CVB   RE,DUB                                                           
                                                                                
         SR    RF,RF                                                            
         AH    RE,=H'1'                                                         
         SRA   RE,1                                                             
                                                                                
         LTR   R2,R2               TEST ACCUMULATORS GIVEN                      
         BNZ   *+12                                                             
         ST    RE,0(R1,R3)         NO - STORE UNWEIGHTED VALUE                  
         B     *+12                                                             
         A     RE,0(R2,R3)         YES - ACCUMULATE UNWEIGHTED VALUE            
         ST    RE,0(R2,R3)                                                      
                                                                                
         LA    R3,4(R3)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,MBKUNWG8                                                      
         B     XIT01                                                            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* PROCESS IN MARKET SHARES:                                           *         
* COMPUTE 'PUT' FROM RTGS OF EACH STATION IN DEFINED 'MKT'            *         
* ROUTINE TO GET OLD OR NEW H/P/T VALUES FROM TIME PERIOD FILE        *         
* ON ENTRY HPT=OLD FOR OLD H/P/T OR NEW FOR NEW H/P/T WITH            *         
* R1=A(BOOK OPTION)                                                   *         
*                                                                     *         
* EXIT WITH H/P/T VALUES AT OLDHPT OR NEWHPT                          *         
***********************************************************************         
         SPACE 1                                                                
STNHPT   DS    0H                                                               
         USING GETHPTWK,R8         R8=A(LOCAL W/S)                              
         L     R1,PARM01                                                        
         MVI   GETHIND,0           SET INDICATOR BYTE                           
         CLI   HPT,OLD             CLEAR REQUIRED H/P/T VALUES                  
         BNE   *+14                                                             
         XC    OLDHPT(LENVALS*2),OLDHPT                                         
         B     *+10                                                             
         XC    NEWHPT(LENVALS*2),NEWHPT                                         
*                                                                               
STNINIT  DS    0H                                                               
         XC    STNBK,STNBK                                                      
         ICM   RE,15,4(R1)                                                      
         BZ    STNHPT1                                                          
         CLC   0(2,RE),=X'2000'    TEST IF BOOK PASSED                          
         BNH   *+10                                                             
         MVC   STNBK,0(RE)         YES - SET SELECTED BOOK                      
*                                                                               
STNHPT1  XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
         MVI   DBFUNCT,DBGETDEM    LOOK UP DEMOS FOR EA STN IN LIST             
         MVC   DBSELBK,STNBK                                                    
         MVI   DBTPTT,C'P'         CHANGE TO T4 BASED PUTS                      
*                                                                               
         NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
*****    LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,VCOMFACS   **MUST BE DONE FOR MKT OVERRIDES**           
         TM    SPUPOPTS,SPOPEXT                                                 
*&&DO                                                                           
         BZ    *+10                EXTEND BLOCK NOT IN USE                      
         MVC   DBEXTEND,SPUPEXTN    IN USE - SET IT                             
*&&                                                                             
         BZ    STNHPTG             EXTEND BLOCK NOT IN USE                      
         GOTO1 VSUBR01,DMCB,('INXTNDQ',(RC))   IN USE - INSERT IT               
STNHPTG  EQU   *                                                                
         MVC   DBSELAGY,SPUPAGY    RESTORE AGENCY                               
         MVC   DBSELUMK,SPUPMKT    SET AGENCY MKT CODE                          
         MVC   DBSELCLI,SPUPCLI    SET AGENCY CLIENT CODE                       
         MVC   DBSELMED,SPUPMED                                                 
         TM    SPUPOPTS,SPOANGFR   CANADIAN ANGLO/FRANCO OPTION                 
         BZ    *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
         TM    SPUPOPT2,SPO2CMON   CANADIAN WEEKLIES AS MONTH                   
         BZ    *+8                                                              
         MVI   DBBEST,C'M'                                                      
         TM    SPUPOPT2,SPO2UBBM   CANADIAN USE BBM WEEKLIES                    
         BZ    *+8                                                              
         MVI   DBUSEBBM,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTY   SPORTS ONLY OPTION                           
         BZ    *+8                                                              
         MVI   DBSELSPO,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTN   EXCLUDE SPORTS OPTION                        
         BZ    *+8                                                              
         MVI   DBSELSPO,C'N'                                                    
*                                                                               
         OC    SPUPPUR,SPUPPUR     TEST DAY/TIME LOOK-UP                        
         BNZ   STNHPT2                                                          
         LA    R1,DBLOCK1          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DUB(2),SPUPFBK                                                   
         NI    DUB+1,X'0F'         AND OFF WEEK BITS                            
         B     STNHPT4                                                          
*                                                                               
STNHPT2  L     R1,SPUPAREC                                                      
         MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
         MVC   DBSELSRC,PRSRC-PRKEY(R1)                                         
         CLI   DBSELMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   DBSELSRC,SPUPSRC                                                 
         MVC   DBBTYPE,PRBTYP-PRKEY(R1)                                         
         MVC   DUB(2),PRBOOK-PRKEY(R1)                                          
*                                                                               
STNHPT4  MVC   SVBTYPE,DBBTYPE     SAVE INITIAL BOOK TYPE                       
         CLI   DBBTYPE,C'A'        PARENT ONLY P+S COMBO                        
         BE    *+8                                                              
         CLI   DBBTYPE,C'O'        OLYMPIC BOOK                                 
         BNE   *+8                                                              
         CLI   HPT,NEW             AND NEW HPT                                  
         BNE   *+8                                                              
         MVI   DBBTYPE,0           KILL BOOK TYPE                               
         OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         BNZ   *+10                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
*                                                                               
STNHPT10 CLI   PUREFLAG,YES        SET DAY/TIME FOR LOOK-UP                     
         BE    STNHPT12                                                         
         MVC   DBSELDAY,SPUPDAY    SET DAY/TIME FROM UPGRADE BLOCK              
         MVC   DBSELTIM,SPUPTIM                                                 
         CLI   HPT,NEW                                                          
         BE    STNHPT13                                                         
         LA    R1,DBLOCK1          SET DAY/TIME FROM DBLOCK1                    
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         B     STNHPT13                                                         
*                                                                               
STNHPT12 MVC   DUB+0(1),PUREDW     EXTRACT DAY/TIME FROM PAV RECORD             
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))       DAY IN DEMAND FMT            
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB+0(1),PURESTIM                                                
         MVC   DUB+1(1),PUREDUR                                                 
         GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))       TIME IN DEMAND FMT           
         MVC   DBSELTIM,DUB+4                                                   
*                                                                               
STNHPT13 DS    0H                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         LA    RE,GETSTLST                                                      
         USING DBXMSD,RE                                                        
         XC    DBXMSD(20),DBXMSD                                                
         MVC   DBXMSID,=C'MSTA'                                                 
         LA    R0,DBXMSTA                                                       
*                                                                               
* COMPARE TO SEE IF WE WANT IMS OR ISS                                          
*                                                                               
         CLI   IMSFLAG,C'S'                                                     
         BNE   STNHPT15                                                         
         L     RF,STNAFFL                                                       
         LA    R1,DBXMSTA                                                       
STNHPT14 CLI   0(RF),0              EOL                                         
         BE    STNHPT16                                                         
         MVC   0(5,R1),0(RF)        MOVE STATIONS INTO MSTA LINK                
         MVI   4(R1),C'T'                                                       
         AHI   R1,5                                                             
         AHI   RF,5                                                             
         B     STNHPT14                                                         
*                                                                               
STNHPT15 GOTO1 VDEINMKT,DMCB,DBLOCKD,STNAFFL,(R0)                               
STNHPT16 LA    RE,GETSTLST                                                      
         OC    DBXMSTA,DBXMSTA                                                  
         BZ    STNHPTNX            NO STATIONS TO LOOK UP-- ERROR               
         ST    RE,DBEXTEND                                                      
         DROP  RE                                                               
*                                                                               
* CHECK IF WE HAVE TO SET UP DAYTIME ROTATION LIST IN EXTENSION                 
* CHECK ORIGINAL SPUPEXTN AREA                                                  
*                                                                               
         ICM   RE,15,SPUPEXTN                                                   
         LTR   RE,RE                                                            
         BZ    STNHPT17                                                         
         USING SPIMSD,RE                                                        
         CLC   SPIMSID,=C'DYTM'    IN MKT SHARE REQUESTED?                      
         BE    *+12                                                             
         L     RE,SPIMSNXT         BUMP TO NEXT EXTN BLOCK                      
         B     *-20                                                             
         DROP  RE                                                               
         LA    RF,GETSTLST                                                      
         USING DBXMSD,RF           SET DYTM LIST AS NEXT LINK                   
         ST    RE,DBXMSNXT                                                      
         DROP  RF                                                               
*                                                                               
*                                                                               
STNHPT17 XC    GETSTNBK(9),GETSTNBK                                             
         MVI   GETSTCNT,0          AND # STN'S COUNTER                          
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
         GOTO1 VDEMAND,DMCB,DBLOCK,STNHK                                        
         MVC   DBBTYPE,SVBTYPE                                                  
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BNE   STNHPTNX            NO - EXIT ON ANY ERROR SETTING               
         MVI   DBERROR,0                                                        
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    STNHPTNX                                                         
*                                                                               
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         CLI   HPT,OLD             TEST OLD HPT'S REQUIRED                      
         BE    STNHPT21            YES                                          
         CLI   SPUP2YRP,C'Y'       CALLER CAN SPECIFY 2 YEAR PUTS               
         BE    *+12                                                             
         TM    DBCPUTS,DBO2YEAR    TEST 2 YEAR AVERAGE PUT REQUIRED             
         BZ    STNHPT21            NO                                           
         TM    GETHIND,X'80'       TEST BOTH YEARS DONE                         
         BNZ   STNHPT18            YES                                          
         ZIC   R1,GETHBK           DECREMENT ACTUAL BOOK YEAR                   
         BCTR  R1,0                                                             
         STC   R1,GETHBK                                                        
         MVC   GETHDIV,DBDIVSOR    SAVE DBDIVSOR                                
         OI    GETHIND,X'80'                                                    
         LA    R1,GETHBK           POINT TO DECREMENTED BOOK VALUE              
         B     STNHPT1             GET PREVIOUS YEAR HPT VALUES                 
*                                                                               
STNHPT18 SR    R0,R0               CALCULATE TOTAL DBDIVSOR VALUE               
         ICM   R0,3,DBDIVSOR                                                    
         SR    R1,R1                                                            
         ICM   R1,3,GETHDIV                                                     
         AR    R0,R1                                                            
         STCM  R0,3,DBDIVSOR                                                    
         B     STNHPT21                                                         
*                                                                               
STNHPT21 CLI   GETSTCNT,0                                                       
         BE    STNHPT22                                                         
         SR    R1,R1                                                            
         ICM   R1,3,DBDIVSOR       DIVIDE DBDIVSOR BY # STATIONS HIT            
         SR    R0,R0                                                            
         SR    R2,R2                                                            
         IC    R2,GETSTCNT                                                      
         DR    R0,R2                                                            
         STCM  R1,3,DBDIVSOR                                                    
*                                                                               
STNHPT22 LA    R0,NUMVALS*2        UNWEIGHT HPT VALUES                          
         LA    R1,OLDHPT           R1=A(OLD OR NEW HPT LINE)                    
         CLI   HPT,OLD                                                          
         BE    *+8                                                              
         LA    R1,NEWHPT                                                        
         SR    R2,R2                                                            
         ST    R1,DMCB+8                                                        
         SR    R2,R2                                                            
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
         CLI   HPT,OLD                                                          
         BE    *+10                                                             
         MVC   OLDHPT(LENVALS*2),NEWHPT                                         
         MVI   DBERROR,0                                                        
         B     STNHPTX                                                          
*                                                                               
STNHPTNX MVI   DBERROR,X'10'      SET ERROR TO NOT FOUND                        
*                                                                               
STNHPTX  B     XIT02                                                            
         EJECT                                                                  
***********************************************************************         
* STNHPT DEMAND HOOK TO ACCUM RTGS FROM EACH RECD RETURNED                      
***********************************************************************         
         SPACE 1                                                                
STNHK    ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         XC    GETHPTUN(LENVALS),GETHPTUN  CLEAR UNIVERSES                      
         XC    GETHPTRT(LENVALS*2),GETHPTRT CLEAR RTG/IMPS                      
         XC    GETHPTS(LENVALS*2),GETHPTS   CLEAR HPTS                          
*                                                                               
         TM    GETHIND,X'40'       TEST ACTUAL BOOK SET                         
         BNZ   *+14                                                             
         OI    GETHIND,X'40'                                                    
         MVC   GETHBK,DBACTBK                                                   
*                                                                               
         LH    R0,DBFACTOR         FORCE NO WEIGHTING IN GETIUN                 
         MVC   DBFACTOR,=H'1'                                                   
                                                                                
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),GETHPTUN                                 
         STH   R0,DBFACTOR                                                      
*                                                                               
         MVC   GETHPTS(LENVALS*2),GETHPTRT  COPY RTS-IMPS TO PUT-TOTS           
*                                                                               
         LA    R0,NUMVALS*2                                                     
         LA    R1,GETHPTS          R1=A(UNWEIGHTED DEMO VALUES)                 
         LA    R2,OLDHPT           R2=A(WEIGHTED DEMO ACCUMULATORS)             
         CLI   HPT,OLD                                                          
         BE    STNHK20                                                          
         LA    R2,NEWHPT                                                        
         OC    LUNV(LENVALS),LUNV  TEST IF LOONEYVERSES THERE                   
         BNZ   *+10                YES                                          
         MVC   LUNV(LENVALS),GETHPTUN                                           
*                                                                               
STNHK20  ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('WGHTUPQ',(RC)),(R0),,(R2)                         
*                                                                               
         LA    RE,GETSTNBK         UPDATE STN-HIT TABLE                         
         LA    R0,GETSTNQ          MAX TABLE SIZE                               
STNHK25  CLI   0(RE),0             EOT?                                         
         BE    STNHK28             UPDATE TABLE IF NOT FOUND                    
         CLC   DBSELSTA,0(RE)      MATCH STATION AND BOOK                       
         BNE   *+14                                                             
         CLC   DBACTBK,5(RE)       SAME BOOK?                                   
         BE    STNHK30             MATCH, NO UPDATE NECC                        
         LA    RE,7(RE)                                                         
         BCT   R0,STNHK25                                                       
         DC    H'0'                REACHED MAX STN SIZE                         
STNHK28  MVC   0(5,RE),DBSELSTA                                                 
         MVC   5(2,RE),DBACTBK                                                  
         XC    7(9,RE),7(RE)       CLEAR FOR NEXT ENTRY                         
         ZIC   RE,GETSTCNT         UPDATE NUMBER STATIONS IN TABLE              
         LA    RE,1(RE)                                                         
         STC   RE,GETSTCNT                                                      
*                                                                               
STNHK30  L     RE,SAVERE           RETURN TO DEMAND FOR NEXT RECORD             
         BR    RE                                                               
         EJECT                                                                  
*&&                                                                             
* INSERT  SPUPEXTN  LINKS AT THE BEGINNING OF  DBEXTEND                         
* TWO THINGS WE NEED TO CHECK BEFORE DOING ANY INSERTIONS:                      
*  1) MAKE SURE  SPUPEXTN  NOT POINTING TO ANY NODE IN  DBEXTEND  LIST          
*  2) MAKE SURE NONE OF THE NODES OF THE  SPUPEXTN  LIST POINTS INTO            
*      THE  DBEXTEND  LIST                                                      
                                                                                
INSXTND  DS    0H                                                               
         LA    RF,DBEXTEND-4       CHECK #1                                     
*                                                                               
INSXTN05 DS    0H                   LOOP TO END OF LIST                         
         ICM   R1,15,4(RF)                                                      
         BZ    INSXTN09                                                         
         CLM   R1,15,SPUPEXTN                                                   
         BE    INSXTNX                                                          
         LR    RF,R1                                                            
         B     INSXTN05                                                         
INSXTN09 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RE,SPUPEXTN-4       CHECK #2                                     
*                                                                               
INSXTN12 DS    0H                   LOOP TO END OF LIST TO BE INSERTED          
         ICM   R0,15,4(RE)                                                      
         BZ    INSXTN29                                                         
*                                                                               
         LA    RF,DBEXTEND-4                                                    
INSXTN16 DS    0H                   LOOP TO END OF TARGET LIST                  
         ICM   R1,15,4(RF)                                                      
         BNZ   *+10                                                             
         LR    RE,R0                                                            
         B     INSXTN12                                                         
*                                                                               
         DS    0H                                                               
         CR    R0,R1                 IF EQUAL TO A(NODE) ON TRGT LIST,          
         BE    *+10                   EXIT LOOP                                 
         LR    RF,R1                 ELSE, BUMP TO NEXT NODE                    
         B     INSXTN16                                                         
*                                                                               
         XC    4(4,RE),4(RE)                                                    
INSXTN29 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RE-->LAST NODE OF LIST T.B. INSERTED         
         MVC   4(4,RE),DBEXTEND                                                 
         MVC   DBEXTEND,SPUPEXTN                                                
                                                                                
*                                                                               
INSXTNX  DS    0H                                                               
         J     XIT01                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE DBLOCK FOR OLD DATA LOOK-UPS                  *         
* AT ENTRY:                                                           *         
*   R7-->SOME DBLOCK AREA                                             *         
*   R9-->SPDEMUPD                                                     *         
***********************************************************************         
                                                                                
BLDBLK00 DS    0H                                                               
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR RATING LOOK-UP              
         MVC   DBAREC,SPUPAREC                                                  
         MVC   DBCOMFCS,SPUPAFAC                                                
         MVC   DBTAPEP,TAPEOPT                                                  
*&&DO                                                                           
         MVC   DBFILE,TPTVALS+1    SET FILE                                     
         CLC   SPUPFIL,PAVVALS                                                  
         BNE   *+10                                                             
         MVC   DBFILE,PAVVALS+1                                                 
*&&                                                                             
         L     RF,ATPTVALS                                                      
                                                                                
         CLC   SPUPFIL,0(RF)                                                    
         BE    *+8                                                              
         L     RF,APAVVALS                                                      
         MVC   DBFILE,1(RF)                                                     
                                                                                
         MVC   DBSELMED,SPUPMED                                                 
         MVC   DBSELSRC,SPUPSRC                                                 
         MVC   DBSELBK,SPUPFBK                                                  
*                                                                               
* SET THE SPILL MARKET FOR NSI WIRED AND FUSION                                 
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         BNE   BLDBLK01                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   BLDBLK01                                                         
         CLI   SPUPBTYP,C'W'        WIRE CABLE                                  
         BE    *+8                                                              
         CLI   SPUPBTYP,C'Z'        LIVE ONLY WIRE CABLE                        
         BE    *+8                                                              
         CLI   SPUPBTYP,C'4'        ZERO CELL WIRED CABLE                       
         BE    *+8                                                              
BLDBLK01 CLI   DBSELSRC,C'F'                                                    
         BNE   BLDBLK04                                                         
         MVC   DBSELMK,SPUPSPL                                                  
*                                                                               
         OC    DBSELMK,DBSELMK       ONLY SET ALPHA SPILL MKT IF                
         BNZ   BLDBLK04              HEADEND PASSED IN, ELSE WE ARE             
         OC    SPUPSYSC,SPUPSYSC     NOT CABLE                                  
         BZ    *+10                                                             
         MVC   DBSELALF,SPUPMALF                                                
*                                                                               
BLDBLK04 CLI   SPUPMED,C'W'                                                     
         BE    *+8                                                              
         CLI   SPUPMED,C'C'                                                     
         BE    *+8                                                              
         NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
         TM    SPUPOPTS,SPOANGFR   CANADIAN ANGLO/FRANCO OPTION                 
         BZ    *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
         TM    SPUPOPT2,SPO2CMON   CANADIAN WEEKLIES AS MONTH                   
         BZ    *+8                                                              
         MVI   DBBEST,C'M'                                                      
         TM    SPUPOPT2,SPO2UBBM   CANADIAN USE BBM WEEKLIES                    
         BZ    *+8                                                              
         MVI   DBUSEBBM,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTY   SPORTS ONLY OPTION                           
         BZ    *+8                                                              
         MVI   DBSELSPO,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTN   EXCLUDE SPORTS OPTION                        
         BZ    *+8                                                              
         MVI   DBSELSPO,C'N'                                                    
* WEEKLY MULTIBOOK - SET DBEXTEND                                               
         CLI   DBSELMED,C'T'                                                    
         BE    *+8                                                              
         CLI   LOCALMED,C'C'       IF CANADIAN MULTIBOOK AVERAGE                
         BE    *+8                 OR WEEKLY NSI MULTIBOOKS- PASS THE           
         CLI   DBSELMED,C'W'       NEW LINKS.  WEEKLY MBKS DONT SET             
         BNE   *+18                SPUPFBKL BECAUSE IT HAS MUCH BIGGER          
         CLI   SPUPTYPE,X'0F'      BOOKS LIST PASSED.                           
         BNE   *+10                                                             
         MVC   DBEXTEND,SPUPEXTN                                                
                                                                                
         OC    SPUPFBKL,SPUPFBKL   TEST BOOK LIST                               
         BZ    BLDBLK06                                                         
         XC    MBEXTEND,MBEXTEND   YES-BUILD EXTENSION FOR MULTIPLE BKS         
         LA    R1,MBEXTEND                                                      
         ST    R1,DBEXTEND         A(EXTENDED DBLOCK)                           
         USING DBXMBD,R1                                                        
         MVC   DBXMBID,=C'MBKS'                                                 
         MVC   DBXMBKS(2),DBSELBK                                               
         MVC   DBXMBKS+2(L'SPUPFBKL),SPUPFBKL                                   
         XC    DBSELBK,DBSELBK                                                  
         DROP  R1                                                               
BLDBLK06 DS    0H                                                               
*                                                                               
         TM    SPUPOPTS,SPOP1DEC    ONE DECIMAL OPTION SET                      
         BZ    BLDBLK16             NO                                          
         XC    SPEXTEND,SPEXTEND                                                
         LA    R1,SPEXTEND                                                      
         OC    DBEXTEND,DBEXTEND    1ST EXTENDED ADDR LOADED?                   
         BNZ   BLDBLK08                                                         
         ST    R1,DBEXTEND                                                      
         B     BLDBLK14                                                         
BLDBLK08 DS    0H                                                               
         L     R2,DBEXTEND          POINT TO THE FIRST BLOCK                    
BLDBLK10 DS    0H                                                               
         CLC   4(4,R2),=XL4'00'     LOOK AT ITS POINTER                         
         BE    BLDBLK12             ZERO MEANS WE CAN USE IT                    
         L     R2,4(R2)             ELSE, POINT TO NEXT                         
         B     BLDBLK10             AND LOOP                                    
BLDBLK12 DS    0H                                                               
         ST    R1,4(R2)             POINT TO IT                                 
BLDBLK14 DS    0H                                                               
         USING DBXTTID,R1           SET VALUES FOR 1 DEC RETURN                 
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
         DROP  R1                                                               
BLDBLK16 DS    0H                                                               
*                                                                               
         CLI   SPUPMED,C'W'                                                     
         BE    *+8                                                              
         CLI   SPUPMED,C'C'                                                     
         BE    *+14                                                             
         PACK  DBSELWKN,SPUPFBK+1(1)                                            
         NI    DBSELWKN,X'0F'      AND OFF MONTH BITS                           
         MVC   DBSELSTA,SPUPSTA                                                 
         MVC   DBSELUMK,SPUPMKT    SET AGY MKT CODE FOR RTGSVC OVRD             
         MVC   DBSELMK,SPUPSPL                                                  
         MVC   DBSELAGY,SPUPAGY                                                 
         MVC   DBSELCLI,SPUPCLI                                                 
         MVC   DBBTYPE,SPUPBTYP                                                 
*                                                                               
         OC    DBSELMK,DBSELMK    ONLY SET ALPHA SPILL MKT                      
         BNZ   BLDBLK17           IF HEADEND IS PASSED IN - CABLE               
         CLI   DBSELMED,C'C'      CANADA SUPPORT ALPHA MKT(SOFT DEMO)           
         BE    *+14                                                             
         OC    SPUPSYSC,SPUPSYSC                                                
         BZ    BLDBLK17                                                         
         MVC   DBSELALF,SPUPMALF                                                
*                                                                               
BLDBLK17 DS    0H                                                               
GOODBTYP DS    0H                                                               
*                                                                               
         CLI   ALTBTYP,0              ALTERNATE BOOK TYPE ACTIVE                
         BE    BLDBLK18                                                         
         CLC   UPLPMDAT(2),DBSELBK    CHECK THE DATE                            
         BH    BLDBLK18                                                         
         MVC   DBBTYPE,ALTBTYP                                                  
BLDBLK18 DS    0C                                                               
*                                                                               
         MVC   DBSELDAT,=X'7F0C'   DEC/2027                                     
         TM    SPUPOPTS,SPOPEXT                                                 
*&&DO                                                                           
         BZ    *+10                EXTEND BLOCK NOT IN USE                      
         MVC   DBEXTEND,SPUPEXTN    IN USE - SET IT                             
*&&                                                                             
*&&DO                                                                           
         BZ    *+8                 EXTEND BLOCK NOT IN USE                      
         BAS   RE,INXTND            IN USE - INSERT IT                          
*&&                                                                             
         BZ    BLDBLK19            EXTEND BLOCK NOT IN USE                      
         GOTO1 VSUBR01,DMCB,('INXTNDQ',(RC))   INSERT IT                        
BLDBLK19 EQU   *                                                                
         MVI   PUREFLAG,NO                                                      
*&&DO                                                                           
         CLC   SPUPFIL,PAVVALS                                                  
*&&                                                                             
         L     RF,APAVVALS                                                      
         CLC   SPUPFIL,0(RF)                                                    
         BNE   BLDBLK23                                                         
         MVC   DBSELPUR,SPUPPUR    SET PURE NUMBER                              
         MVC   DBBEST,SPUPBEST                                                  
         OC    DBSELPUR,DBSELPUR                                                
         BZ    BLDBLK23                                                         
         MVI   PUREFLAG,YES                                                     
         B     BLDBLKX                                                          
BLDBLK23 MVC   DBSELDAY,SPUPDAY    SET DAY/TIME VALUES                          
         MVC   DBSELTIM,SPUPTIM                                                 
*                                                                               
         CLI   DBSELMED,C'W'                                                    
         BE    BLDBLK30                                                         
         CLI   DBSELMED,C'C'                                                    
         BNE   BLDBLK40                                                         
BLDBLK30 MVI   DBTPTT,C'T'                                                      
                                                                                
BLDBLK40 CLI   SPUPTPTT,C'P'       SET TP OR 4WK AVG                            
         BNE   *+8                                                              
         MVI   DBTPTT,C'P'                                                      
         OC    SPUPUDAY(5),SPUPUDAY TEST OVERRIDE DAY/TIME GIVEN                
         BZ    BLDBLKX                                                          
         MVC   DBSELDAY,SPUPUDAY   YES - USE THEM                               
         MVC   DBSELTIM,SPUPUTIM                                                
*                                                                               
BLDBLKX  DS    0H                                                               
         J     XIT01                                                            
*                                                                               
* NORMALIZE HPT VALUES                                                          
                                                                                
NORMU000 DS    0H                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   NORMUX                                                           
         CLI   NORMHPT,C'Y'        NORMALIZE HPT                                
         BNE   NORMUX                                                           
         LA    R0,NUMVALS          CALCULATE UNIVERSE INDEX                     
         LA    R1,LUNV                                                          
         LA    R7,UPUIDX                                                        
         LA    R8,OLDUNV                                                        
NORMU1   L     RF,0(R8)                                                         
         M     RE,=F'20000'                                                     
         CLC   0(4,R1),=XL4'00'                                                 
         BE    *+12                                                             
         D     RE,0(R1)                                                         
         B     *+8                                                              
         L     RF,=F'20000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R7)                                                         
         LA    R8,4(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMU1                                                        
*                                                                               
         LA    R0,NUMVALS          CALCULATE ADJUSTED HUTS/PUTS                 
         LA    R1,NEWHPT                                                        
         LA    R7,UPUIDX                                                        
NORMU2   L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMU2                                                        
                                                                                
*                                                                               
NORMUX   DS    0H                                                               
         J     XIT01                                                            
                                                                                
                                                                                
**************************************************************                  
* THIS PART ADJUSTS THE THE IMPRESSIONS, RATINGS AND HPTS FOR                   
* MULTIBOOK AVERAGE BASED ON THE UNIVERSE                                       
**************************************************************                  
NORMIMP0 DS    0H                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   NORMIMPX                                                         
         LA    R0,NUMVALS          CALCULATE UNIVERSE INDEX                     
         LA    R1,NEWUNV                                                        
         LA    R7,UPUIDX                                                        
         LA    R8,OLDUNV                                                        
NORMIMP1 L     RF,0(R8)                                                         
         M     RE,=F'20000'                                                     
         CLC   0(4,R1),=XL4'00'                                                 
         BE    *+12                                                             
         D     RE,0(R1)                                                         
         B     *+8                                                              
         L     RF,=F'20000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R7)                                                         
         LA    R8,4(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMIMP1                                                      
*                                                                               
         LA    R0,NUMVALS          CALCULATE ADJUSTED HUTS/PUTS                 
         LA    R1,NEWHPT                                                        
         LA    R7,UPUIDX                                                        
NORMIMP2 L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMIMP2                                                      
                                                                                
         LA    R0,NUMVALS          CALCULATE ADJUSTED IMPRESSIONS               
         LA    R1,NEWIMP                                                        
         LA    R7,UPUIDX                                                        
NORMIMP4 L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMIMP4                                                      
                                                                                
         LA    R0,NUMVALS          CALCULATE ADJUSTED IMPRESSIONS               
         LA    R1,NEWRTG                                                        
         LA    R7,UPUIDX                                                        
NORMIMP6 L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMIMP6                                                      
                                                                                
NORMIMPX DS    0H                                                               
         J     XIT01                                                            
***********************************************************************         
* SUBROUTINE TO COMPUTE INDEX VALUE TO 2 DECIMAL PLACES               *         
* DMCB+4 HAS OLD VALUE  RETURN DMCB+8  NEW VALUE                      *         
* RETURN  RESULT IN DMCB+8 AND INDEX                                  *         
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                 *         
* RETURN RESULT IN R1 AND IN 'INDEX'.                                 *         
***********************************************************************         
         SPACE 1                                                                
GETINDX  XC    INDEX,INDEX                                                      
         L     R1,DMCB+8           GET PARAMETERS                               
         L     R0,DMCB+4                                                        
                                                                                
         MH    R1,=H'10000'                                                     
         LTR   RF,R0               SAVE OLD VALUE                               
******   BZR   RE                                                               
         BZ    XIT01                                                            
         SRL   R0,1                HALVE                                        
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         DR    R0,RF                                                            
         ST    R1,INDEX                                                         
         ST    R1,DMCB+8                                                        
*****    BR    RE                                                               
         J     XIT01                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO SET OUTPUT VALUES FOR OVERRIDE DEMOS                     *         
* DMCB WILL HAVE ADDRESS OF THE PARMETERS LIST                        *         
*                                                                               
*DMCB+4- R2=A(OVERRIDE ELEMENT)                                       *         
*DMCB+8  RF=A(DEMO LIST ENTRY OF FIRST MODIFIER FOR OVERRIDE DEMO)    *         
*DMCB+12 R1=A(CORRESPONDING OUTPUT VALUE OF RF)                       *         
***********************************************************************         
         SPACE 1                                                                
*SETOVER  NTR1  ,                                                               
SETOVER  MVC   DUB+0(4),DMCB+8                                                  
         MVC   DUB+4(4),DMCB+12                                                 
         ZICM  R2,DMCB+4,(15)                                                   
                                                                                
         LA    R4,WORK             BUILD OVERRIDE ENTRY IN WORK                 
         MVC   0(1,R4),2(R2)                                                    
         XC    1(2,R4),1(R4)                                                    
         MVC   3(2,R4),4(R2)                                                    
         MVI   5(R4),X'FF'                                                      
         CLI   2(R2),C'S'          TEST 'SHARE' OVERRIDE                        
         BNE   SETOVERA                                                         
*                                  CELL UPGRADE FOR SHARE OVERRIDE              
         MVC   OVERDEM(OVERSHRL),OVERSHRS                                       
         LA    R1,OVERDEM-L'OVERDEM                                             
SETOVER2 LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    *+14                                                             
         MVC   2(1,R1),3(R2)       INSERT DEMO NUMBER                           
         B     SETOVER2                                                         
         GOTO1 VDEMOUT,DMCB,(C'L',OVERDEM),DBLOCK,OVERDEMS                      
         CLI   DBERROR,0           TEST FOR ERRORS                              
         BNE   SETOVERA                                                         
         L     R0,OVERSHR          R0=OLD SHARE VALUE                           
         SR    R1,R1                                                            
         ICM   R1,3,4(R2)          R1=NEW SHARE VALUE                           
****     BAS   RE,GETINDEX         CALCULATE INDEX VALUE                        
         ST    R0,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('GETIDXQ',(RC))                                    
         LA    R4,5(R4)            POINT TO NEXT OVERRIDE ENTRY                 
*                                                                               
         L     R1,OVERRTG          CALCULATE NEW RATING                         
         M     R0,INDEX                                                         
         AH    R1,=H'5000'                                                      
         D     R0,=F'10000'                                                     
         MVI   0(R4),C'R'                                                       
         STCM  R1,15,1(R4)                                                      
         LA    R4,5(R4)                                                         
*                                                                               
         L     R1,OVERIMP          CALCULATE NEW IMPRESSION                     
         M     R0,INDEX                                                         
         AH    R1,=H'5000'                                                      
         D     R0,=F'10000'                                                     
         MVI   0(R4),C'I'                                                       
         STCM  R1,15,1(R4)                                                      
         LA    R4,5(R4)                                                         
         MVI   0(R4),X'FF'                                                      
         B     SETOVERA                                                         
*                                                                               
SETOVERA LM    RE,RF,DUB           SLOT OVERRIDES INTO OUTPUT AREA              
SETOVERC CLI   0(RE),X'FF'         TEST E-O-L                                   
         BE    SETOVERX                                                         
         CLC   2(1,RE),3(R2)       MATCH ON DEMO                                
         BNE   SETOVERG                                                         
         CLI   0(RE),OVERELEM      IGNORE IF ALREADY AN OVERRIDE                
         BE    SETOVERG                                                         
         LA    R1,WORK             FIND OVERRIDE VALUE IN TABLE                 
SETOVERE CLI   0(R1),X'FF'                                                      
         BE    SETOVERG                                                         
         CLC   0(1,R1),1(RE)       MATCH ON MODIFIER                            
         BE    *+12                                                             
         LA    R1,5(R1)                                                         
         B     SETOVERE                                                         
         MVC   0(4,RF),1(R1)                                                    
         MVI   0(RE),OVERELEM      INDICATE THIS IS AN OVERRIDE                 
SETOVERG LA    RE,3(RE)            BUMP TO NEXT DEMO                            
         LA    RF,4(RF)                                                         
         B     SETOVERC                                                         
SETOVERX J     XIT01                                                            
         EJECT                                                                  
                                                                                
         EJECT                                                                  
OVERSHRS DC    X'00',C'S',X'00'    LIST OF DEMOS FOR SHARE OVERRIDES            
         DC    X'00',C'R',X'00'                                                 
         DC    X'00',C'I',X'00'                                                 
         DC    X'FF'                                                            
OVERSHRL EQU   *-OVERSHRS                                                       
         SPACE 1                                                                
DEMOSHR  DS    0XL3                                                             
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* IUN RECORD VALUES HAVE NOW BEEN ADJUSTED BY UPGRADE ROUTINES.       *         
* REPLACE OLD DEMO ELEMENTS WITH IUN FORMAT DEMO ELEMENTS.            *         
* CREATE OVERRIDE ELEMENTS FOR ANY VALUE THAT WAS PASSED FROM CALLER. *         
* BOOK ELEMENT IS FORCED TO JUN/83 IF PUT UPGRADE TOOK PLACE.         *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
UPEND00  CLI   SPUPSRC,C'F'        ALLOW IMPRESSIONS FOR FUSION                 
         BE    UPEND4              AND NSI WIRED CABLE DATA                     
         CLI   SPUPMED,C'T'                                                     
         BNE   UPEND02                                                          
         CLI   SPUPBTYP,C'W'                                                    
         BE    UPEND4                                                           
         CLI   SPUPBTYP,C'Z'                                                    
         BE    UPEND4                                                           
         CLI   SPUPBTYP,C'4'       ZERO CELL WIRED CABLE                        
         BE    UPEND4                                                           
*                                                                               
UPEND02  OC    SPUPSPL,SPUPSPL     CLEAR IMPS/TOTS IF SPILL                     
         BZ    UPEND4                                                           
         OC    SPUPMALF,SPUPMALF                                                
         BZ    UPEND4                                                           
         XC    OLDIMP(LENVALS),OLDIMP                                           
         XC    OLDTOT(LENVALS),OLDTOT                                           
         XC    NEWIMP(LENVALS),NEWIMP                                           
         XC    NEWTOT(LENVALS),NEWTOT                                           
*                                                                               
UPEND4   XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR DEMAINT CALL                
         L     RE,APAVVALS                                                      
         MVC   DBFILE,1(RE)                                                     
         MVC   DBAREC,SPUPAREC                                                  
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBCOMFCS,VCOMFACS                                                
         LA    R1,(UPRECX-UPREC)/4                                              
         STCM  R1,3,DBNUMVLS       SET DBNUMVLS TO MAXIMUM                      
         L     RE,AOFORMAT                                                      
         MVC   WORK(10),0(RE)      BUILD DEMAINT OUTPUT FORMAT BLOCK            
                                                                                
                                                                                
UPEND13  CLI   INDEXUPG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK8212  YES - SET BOOK TO DEC/82                     
*                                                                               
         CLI   PUTUPGD,YES         TEST IF PUT UPGRADE DONE                     
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK8306  YES - SET BOOK TO JUN/83                     
                                                                                
         CLI   TAPEOPT,C'Y'        TAPE BASED ?                                 
         BNE   UPEND14                                                          
                                                                                
         MVC   WORK+7(2),BOOK9011  SWITCH FORMULA                               
                                                                                
         CLI   INDEXUPG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   WORK+7(2),BOOK9012  SWITCH TO INDEX FORMULA                      
*                                                                               
         CLI   PUTUPGD,YES         TEST IF PUT UPGRADE DONE                     
         BNE   UPEND15                                                          
         MVC   WORK+7(2),BOOK9106  YES - SET BOOK TO JUN/91                     
* LIVE+7 BOOKTYPES READ 1W PROFILE FOR DMA L7 EFFECTIVE MMYY                    
* IF ITS TURNED ON USE  I=R*U FORMULAS                                          
*                                                                               
*          FOR SYSCODE LEVEL LOOKUPS WE CHANGED                                 
*  SPGETIUN - WE ALWAYS MOVE IN DMA IMPS AND TOTS INTO TSA                      
*  BECAUSE ...TOTS ARE BASED ON BROADCAST STATON TOTAL IMPS                     
*  AND THERE ISNT SUCH THING AS TSA ANYWAYS FOR CABLE..ITS ALL                  
*  DMA BASED.  CABLE IS MARKET BY MARKET UNLIKE LIKE WABC                       
*  WHICH CAN ALSO INCLUDE ANOTHER AREA                                          
*  I THINK WE SHOULD ALWAYS USE I=RRU FOR SYSCODE CABLE LOOKUPS                 
*  SO WE CAN GET A BETTER MATCH TO THE RATINS CALCULATION                       
         OC   SPUPSYSC,SPUPSYSC                                                 
         BNZ  UPEND13D                                                          
*                                                                               
         GOTO1 VSUBR02,DMCB,('READPRFE',(RC))                                   
         OC    DMAL7EBK,DMAL7EBK                                                
         BZ    UPEND15                                                          
         ZICM  RE,DMAL7EBK,(3)     PROF EFFECTIVE BOOK 1=2001 10=2010           
         A     RE,=X'00006400'     AJUST TO DEMO EFF BOOK YR 116=2016           
         ZICM  RF,SPUPFBK,(3)      COMPARE SHARE BOOK WITH DMA EFF BK           
UPEND13C CR    RF,RE                                                            
         BL    UPEND15                                                          
UPEND13D MVC   WORK+7(2),BOOK9206  DEFAULT 1DEC - SET BOOK TO JUN/92            
**                                                                              
* FOR PUT TYPE UPGRADES WE HAVE A NEW FORMULA FOR IMPRESSION                    
* CALCULATION FOR 2 DECIMAL TSA IMPRESSIONS                                     
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         BZ    *+10                                                             
         MVC   WORK+7(2),BOOK9207  2DEC- SET BOOK TO JUL/92                     
*                                                                               
         B     UPEND15                                                          
*                                                                               
UPEND14  ZIC   RE,WORK+7                                                        
         ZIC   RF,SBKCTRL                                                       
         AR    RE,RF                                                            
         STC   RE,WORK+7                                                        
*                                                                               
*        UPGRADES DO A REP ON A TPT RECORD WHICH IS TOTALLY ILLEGAL             
*        THIS SECTION DEALS WITH THAT                                           
UPEND15  L     R1,DBAREC                                                        
         CLI   0(R1),C'R'          POSSIBLE TPT RECORD                          
         BNE   UPEND30             NO - LEAVE IT ALONE                          
         L     R1,DBAQUART         FORCE DEMEL TO INSERT DEMOS AT               
*                                  AT THIS QH                                   
UPEND16  CLI   0(R1),0                                                          
         BE    UPEND30                                                          
         ZIC   RE,1(R1)                                                         
         LA    RF,0(RE,R1)                                                      
         CLI   0(RF),X'5F'         60 AND ABOVE WILL STOP DEMAINT               
         BH    UPEND30             AT THE CORRECT SPOT                          
         CLC   0(1,RF),0(R1)       NEW SECTION                                  
         BL    UPEND20                                                          
         LR    R1,RF                                                            
         B     UPEND16                                                          
*                                                                               
UPEND20  CLI   0(RF),X'20'         ENSURE I HAVE ANOTHER QH                     
         BNE   UPEND30              NO-DON'T ZAP RECORD                         
UPEND24  MVI   0(RF),X'FF'         DELETE REMAINING ELEMENTS                    
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         BNE   UPEND24                                                          
*                                                                               
UPEND30  GOTO1 VDEMAINT,DMCB,=C'REP',DBLOCK,UPREC,WORK                          
         CLI   DBERROR,0                                                        
         BNE   ERR01                                                            
*                                                                               
         MVC   DUB(2),NDXDEMO      ADD INDEX VALUE OVERRIDE ELEMENT             
         MVC   DUB+2(2),INDEX+2                                                 
         OC    INDEX,INDEX                                                      
         BZ    UPEND32                                                          
         GOTO1 VSUBR01,DMCB,('ADDOVERQ',(RC))                                   
*                                                                               
UPEND32  DS    0H                                                               
         SR    R0,R0               CREATE DEMO OVERRIDE ELEMENTS                
         ICM   R0,1,OVERLIST       R0=NUMBER OF OVERRIDE VALUES                 
         BZ    UPENDX                                                           
         LA    R2,OVERLIST+1       R2=A(OVERRIDE LIST)                          
         MVI   WORK+0,OVERELEM     DELETE OVERRIDE ELEMENTS                     
         MVI   WORK+1,6                                                         
*                                                                               
UPEND34  MVC   WORK+2(4),0(R2)     ADD OVERRIDE ELEMENTS                        
         GOTO1 VHELLO,DMCB,(C'P',DEMFILE),SPUPAREC,WORK,0                       
         LA    R2,4(R2)                                                         
         BCT   R0,UPEND34                                                       
UPENDX   J     XIT01                                                            
                                                                                
DEMFILE  DC    C'DEMFILE '                                                      
NDXDEMO  DC    C'&&',AL1(254)      INDEX DEMO MODIFIER/NUMBER                   
BOOK8306 DC    AL1(83,06)          BOOK UNROUNDED BOTTOM LINE                   
BOOK9011 DC    AL1(90,11)          TAPE BASED NORMAL                            
BOOK9106 DC    AL1(91,06)          TAPE BASED UPGRADE                           
BOOK9012 DC    AL1(90,12)          TAPE BASED INDEXED                           
BOOK8212 DC    AL1(82,12)          BOOK BASED INDEXED                           
BOOK8701 DC    AL1(87,01)          WTP                                          
BOOK9207 DC    AL1(92,07)          2 DEC TSA IMP                                
BOOK9206 DC    AL1(92,06)          1 DEC TSA IMP                                
         EJECT                                                                  
                                                                                
* ADD AN OVERRIDE VALUE TO LIST OF DEMO OVERRIDE VALUES                         
*                                                                               
ADDOVER0 ZIC   RF,OVERLIST         RF=NUMBER OF VALUES SO FAR                   
         LA    RF,1(RF)                                                         
         STC   RF,OVERLIST         SET NEW VALUE COUNT                          
         SLL   RF,2                                                             
         LA    RF,OVERLIST-3(RF)                                                
         MVC   0(4,RF),DUB         INSERT NEW ENTRY INTO LIST                   
         MVI   4(RF),X'FF'                                                      
         J     XIT01                                                            
**********************************************************************          
* NEW CODE FOR LPM UPGRADE ..WE HAVE TO ALSO PROCESS SINDEX AND PINDEX          
* UPGRADE BASED ON A MKT, STATION INDEX LPM/DIARY                               
**********************************************************************          
* CALL DEMAND TO GET MARKET NUMBER FOR STATION                                  
*                                                                               
UPLPMIDX XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVC   DBTAPEP,TAPEOPT                                                  
         L     RE,ATPTVALS                                                      
         MVC   DBFILE(L'DBFILE),1(RE)                                           
*****    LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
*                                  VALIDATE STATION TO GET MARKET               
***      MVI   DBFUNCT,DBVLST                                                   
         MVI   DBFUNCT,DBVLSTBK                                                 
         MVC   DBSELBK,SPUPFLD1                                                 
*                                                                               
         MVC   DBSELSTA(4),SPUPFLD2 SPUPFLD2,3 HAS STATION IN IT                
         MVI   DBSELSTA+4,C'T'                                                  
*                                  CALL DEMAND TO VALIDATE STATION/BOOK         
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   UPLPMXX             IF ERROR JUST EXIT                           
***      MVC   DBSELRMK,DBACTRMK   NO - EXTRACT MARKET NUMBER                   
         MVC   DBSELRMK,DBKEY+(BSRMKT-BSKEY)                                    
*                                                                               
* NOW THAT WE HAVE THE HOME MARKET FOR STATION IN UPGRADE FORMULA               
* CHECK AGAINST OUR ALLOWED LPM MARKETS TABLE                                   
*                                                                               
*                                                                               
         GOTO1 VDEMTABS,DMCB,LPMUPG    GET A(LPM UPGRADE TABLE)                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
*                                       LOOK FOR SHARE INDEX MKT                
         USING LPMUDATD,RE                                                      
UPLPM006 CLC   =X'FFFF',0(RE)           IN TABLE                                
         BE    UPLPMXX                  IF NOT IN LPM TABLE EXIT                
         CLC   LPMUNMKT,DBSELRMK                                                
         BE    UPLPM008                                                         
         AR    RE,R0                                                            
         B     UPLPM006                                                         
*                                                                               
UPLPM008 MVC   LPMSBOOK,LPMUDATE       SET SHARE INDEX BOOK FROM TABLE          
         MVC   LPMSRMKT,DBSELRMK                                                
                                                                                
         LR    RE,RF                   CHECK PUT MKT IS ALSO IN TABLE           
         OC    SPLPMPMK,SPLPMPMK       PUT INDEX MARKET IS OPTIONAL             
         BNZ   UPLPM009                IF NOT GIVEN THEN DO PUT INDEX           
         MVC   LPMPBOOK,LPMSBOOK       OFF SHARE INDEX BOOK                     
         MVC   LPMPRMKT,LPMSRMKT       SET PUT INDEX MKT = SHARE INDEX          
         B     LPMSHARE                                                         
*                                      MKT                                      
UPLPM009 CLI   0(RE),X'FF'                                                      
         BE    UPLPMXX                 IF NOT IN LPM TABLE EXIT                 
         CLC   LPMUNMKT,SPLPMPMK                                                
         BE    UPLPM010                                                         
         AR    RE,R0                                                            
         B     UPLPM009                                                         
UPLPM010 MVC   LPMPBOOK,LPMUDATE       SET PUT INDEX BOOK FROM TABLE            
         MVC   LPMPRMKT,SPLPMPMK       SET PUT INDEX MARKET                     
         DROP  RE                                                               
                                                                                
* PROCESS SHARE INDEX                                                           
*                                                                               
LPMSHARE MVC   SVUPFLD(6),SPUPFLD1                                              
*                                                                               
         MVC   SPUPFLD1(2),LPMSBOOK     FORCE MAY04 FOR NOW                     
         MVI   Y4SSW,C'S'               DO A PUT INDEX                          
         MVI   Y4ASW,C'N'                                                       
         MVI   Y4SSW,C'S'                                                       
         MVI   SPUPSTYP,C'Y'                                                    
         MVC   MMUBOOK,SPUPFLD1     SET INDEX BASE BOOK                         
         MVI   MMUPASS,1            SET PASS FOR PARALLEL BOOK                  
                                                                                
         LA    RE,DBLOCK1                                                       
         MVC   DBSELSTA-DBLOCK(4,RE),SPUPFLD2                                   
         MVI   DBSELSTA+4-DBLOCK(RE),C'T'                                       
         MVC   MARKET,LPMSRMKT                                                  
         MVC   SPUPFLD2,=H'2'                                                   
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         MVC   SPUPFLD1(6),SVUPFLD                                              
*                                                                               
* PROCESS PUT INDEX                                                             
*                                                                               
UPLPM020 MVC   SPUPFLD1(6),SVUPFLD                                              
         MVC   SPUPFLD1(2),LPMPBOOK                                             
         MVI   Y4SSW,C'P'               DO A PUT INDEX                          
         MVI   Y4ASW,C'N'                                                       
         MVI   Y4SSW,C'P'                                                       
         MVI   SPUPSTYP,C'Y'                                                    
         MVC   MMUBOOK,SPUPFLD1     SET INDEX BASE BOOK                         
         MVI   MMUPASS,1            SET PASS FOR PARALLEL BOOK                  
         LA    RE,DBLOCK1                                                       
         ZICM  RF,LPMPRMKT,(3)                                                  
         CVD   RF,DUB                                                           
**       UNPK  DBSELSTA(4),DUB                                                  
**       OI    DBSELSTA+3,X'F0'                                                 
**       MVI   DBSELSTA+4,C'T'                                                  
***                                                                             
         UNPK  DBSELSTA-DBLOCK(4,RE),DUB                                        
         OI    DBSELSTA+3-DBLOCK(RE),X'F0'                                      
         MVI   DBSELSTA+4-DBLOCK(RE),C'T'                                       
***                                                                             
         MVC   MARKET,LPMPRMKT                                                  
         MVC   SPUPFLD2,=H'2'                                                   
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         MVC   SPUPFLD1(6),SVUPFLD                                              
***      MVC   MARKET,SVMARKET                                                  
UPLPMXX  DS    0C                                                               
         J     XIT01                                                            
*                                                                               
**********************************************************************          
*                                                                               
* LPS                                                                           
**********************************************************************          
                                                                                
UPLPSIDX MVI   Y4SSW,C'P'               DO A PUT INDEX                          
         MVI   Y4ASW,C'N'                                                       
         MVI   Y4SSW,C'P'                                                       
         MVI   SPUPSTYP,C'Y'                                                    
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
UPLPSXX  DS    0C                                                               
         J     XIT01                                                            
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* THIS IS A NEW GETINDEX ROUTINE USES PACK INSTRUCTIONS               *         
* THE RTG UPGRADE WAS OVERFLOWING THE FULL WORD *                     *         
*                                                                     *         
* SUBROUTINE TO COMPUTE INDEX VALUE TO 2 DECIMAL PLACES               *         
* DMCB+4 HAS OLD VALUE  RETURN DMCB+8  NEW VALUE                      *         
* RETURN  RESULT IN DMCB+8 AND INDEX                                  *         
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                 *         
* RETURN RESULT IN R1 AND IN 'INDEX'.                                 *         
***********************************************************************         
         SPACE 1                                                                
GETINDX2 XC    INDEX,INDEX                                                      
         L     R1,DMCB+8           GET PARAMETERS                               
         L     R0,DMCB+4                                                        
                                                                                
                                                                                
* PROCESS AS PACK INSTRUCTION                                                   
         XC    DUB,DUB             PACK INPUT  VALUE                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0C'                                                      
         XC    DUB2,DUB2                                                        
         MVC   DUB2(3),=X'10000C'   MULTIPLY BY 10000                           
         MP    DUB(8),DUB2(3)     DUB = PACK NUMBER DMCB+8                      
                                                                                
         LTR   RF,R0               SAVE OLD VALUE                               
         JZ    XIT01                                                            
                                                                                
         XC    DUB2,DUB2                                                        
         CVD   R0,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         AP    DUB(8),DUB2(8)                                                   
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB                                                    
                                                                                
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         DP    WORK(12),DUB2+4(4)                                               
         MVC   DUB(8),WORK                                                      
         CVB   R1,DUB                                                           
         ST    R1,INDEX                                                         
         ST    R1,DMCB+8                                                        
         J     XIT01                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE READS SPOT USTV MARKET RECORD TO GRAB THE SOURCE            *         
***********************************************************************         
GETMKT   DS    0C                                                               
*                                                                               
* READ SPOT PROFILE FOR SOURCE                                                  
*                                                                               
* NEED TO READ 00A PROFILE !                                                    
         XC    WORK(16),WORK       READ 00A PROFILE                             
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
**       MVC   WORK+4(2),DBSELAGY                                               
         MVC   WORK+4(2),SPUPAGY                                                
         L     RF,VCOMFACS                                                      
         L     R0,CDATAMGR-COMFACSD(RF)                                         
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    SV00APRF,SV00APRF                                                
         GOTO1 (RF),DMCB,WORK,SV00APRF,(R0)                                     
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
         CLI   SV00APRF,C'N'       TEST NSI DEFAULT                             
         BE    *+12                                                             
         CLI   SV00APRF,C'F'       OR FUSION                                    
         BNE   GETMKTX             IF NEITHER, SUPPRESS LOOKUP                  
         MVC   SPUPSRC,SV00APRF    SET THE LOOKUP SOURCE                        
*                                                                               
* READ SPOT MARKET RECORD FOR SOURCE                                            
* MARKET RECORD SOURCE OVERRIDES PROFILE SOURCE                                 
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),SPUPAREC                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R3,WORK                                                          
         USING MKTRECD,R3                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   MKTKTYPE,C'M'       RECORD TYPE                                  
         MVI   MKTKMED,C'T'        MEDIA                                        
         SR    R0,R0                                                            
*        ICM   R0,3,DBSELUMK       USER MARKET                                  
         ICM   R0,3,SPUPMKT        USER MARKET                                  
         BNZ   GETMKT2                                                          
         XC    SVMKTKEY,SVMKTKEY                                                
         B     GETMKTX                                                          
*                                                                               
GETMKT2  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
***      MVC   MKTKAGY,BUYALPHA    AGENCY CODE                                  
         MVC   MKTKAGY,SPUPAGY     AGENCY CODE                                  
*                                                                               
         L     RE,VCOMFACS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETMKT4X               AND NO SAVE AREA AVAILABLE                
*                                                                               
         CLC   SVMKTKEY(8),MKTKEY    TEST SAME MARKET AS PREV                   
         BNE   GETMKT4                                                          
* SINCE WE ALSO HAVE THE SAME MARKET AS PREV RESTORE SET THE SOURCE             
* AS SAME AS PREV                                                               
         MVC   SPUPSRC,SVMKCDEM      SAME SOURCE AS PREV READ MKT REC           
         MVC   GDMKCDEM,SVMKCDEM                                                
         MVC   GDMKALPH,SVMKALPH                                                
         MVC   LPMDTB,SVLPMDTB                                                  
         MVC   LPMDTP,SVLPMDTP                                                  
         B     GETMKTX                                                          
                                                                                
*                                                                               
GETMKT4  MVC   SVMKTKEY,MKTKEY        ELSE SAVE MKTKEY NOW                      
         DROP  R3                                                               
*                                                                               
GETMKT4X XC    GDMKCDEM,GDMKCDEM   CLEAR SAVE AREAS                             
         XC    GDMKALPH,GDMKALPH                                                
         XC    LPMDTB,LPMDTB                                                    
         XC    LPMDTP,LPMDTP                                                    
         GOTO1 (RF),(R1)           AND READ THE RECORD                          
*                                                                               
         L     R3,SPUPAREC                                                      
         USING MKTRECD,R3                                                       
*                                                                               
         CLC   WORK(8),0(R3)                                                    
         BNE   GETMKTX                                                          
*                                                                               
         MVC   GDMKALPH,MKTALST    MOVE ALPHA MARKET (NOT MKTAMKTC)             
         MVC   GDMKCDEM,SV00APRF   SET DFLT CBL SOURCE FROM 00A PRO             
         CLI   MKTCDEM,C'N'        TEST NSI SPECIFIED                           
         BE    GETMKT6                                                          
         CLI   MKTCDEM,C'F'        TEST FUSION SPECIFIED                        
         BE    GETMKT6                                                          
         CLI   MKTCDEM,C'0'        TEST NO CABLE DEMO LOOK UP                   
         BNE   GETMKT8             IF NOT LEAVE 00A PROFILE AS DFLT             
         MVI   SPUPSRC,0           CLEAR OUT SPUPSRC                            
         B     GETMKTX                                                          
*                                                                               
GETMKT6  MVC   GDMKCDEM,MKTCDEM    MOVE RATING SERVICE                          
*                                                                               
GETMKT8  MVC   LPMDTP,MKTLPMDT     SAVE 2-BYTE PACKED DATE                      
         MVC   SPUPSRC,GDMKCDEM    SET THE LOOKUP SOURCE                        
         OC    LPMDTP,LPMDTP       TEST FOR ANY 2-BYTE DATE                     
         BZ    GETMKTX             NONE - EXIT                                  
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,LPMDTP),(3,LPMDTB)                                  
         DROP  R3                                                               
*                                                                               
GETMKTX  MVC   SVLPMDTB,LPMDTB                                                  
         MVC   SVLPMDTP,LPMDTP                                                  
         MVC   SVMKALPH,GDMKALPH                                                
         MVC   SVMKCDEM,GDMKCDEM                                                
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         J     XIT01                                                            
         DROP  R4                                                               
***********************************************************************         
* ROUTINE SETS UP THE THE CABLE CALL LETERS AND SYSCODE CORRECTLY     *         
* FOR THE SPOT SYSTEM                                                 *         
*                                                                     *         
***********************************************************************         
SETSCBLE DS    0C                                                               
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
*   CHECK THE RATING SERVICE SET BY PROFILE OR MARKET RECORD                    
*                                                                               
         CLI   SPUPSRC,C'N'           ONLY PROCESS FOR NIELSON                  
         BE    *+8                    OR FUSION                                 
         CLI   SPUPSRC,C'F'           ELSE                                      
         BNE   SETSCBLX               WE'LL GRAB NOTHING LATER                  
*                                                                               
         PACK  DUB,SPUPSYSE(4)        PACK THE SPOT 4 CHAR SYSCODE TO           
         CVB   R0,DUB                 TWO BYTE BINARY NEEDED BY DEMOUT          
         STCM  R0,3,SPUPSYSC                                                    
*                                                                               
* CALL STAPACK TO CONVERT SPOT NETWORK CALL LETTERS TO 4 CHAR                   
* NETWORK EQUIVALENCE                                                           
         LA    R3,WORK                                                          
         USING STAPACKD,R3                                                      
         XC    STAPACKD(32),STAPACKD                                            
         MVC   STAPACOM,VCOMFACS                                                
         MVI   STAPACT,C'X'                                                     
         MVC   STAPQNET,SPUPSTA       SPOT PASSED IN 3 BYTE NETWORK             
         GOTO1 VSTAPACK,WORK                                                    
         OC    STAPQSTA,STAPQSTA      STAPACK CANT TRANSLATE-EXIT               
         BZ    SETSCBLX               WE'LL GRAB NOTHING LATER                  
         CLC   =C'    ',STAPQSTA      STAPACK CANT TRANSLATE-EXIT               
         BE    SETSCBLX               WE'LL GRAB NOTHING LATER                  
*                                                                               
         MVC   SPUPSTA,STAPQSTA       TRANSLATED NETWORK CODE                   
         MVI   SPUPSTA+4,C'T'                                                   
         DROP  R3                                                               
*                                                                               
*                                                                               
         OC    SPUPSYSC,SPUPSYSC      FOR NSI CABLE FORCE TO WIRED              
         BZ    SETSCB06               CABLE                                     
         CLI   SPUPSRC,C'N'                                                     
         BNE   SETSCB06                                                         
         CLI   SPUPBTYP,C'4'          UNLESS ITS WIRED ZERO CELL THEN           
         BE    *+8                    ALLOW WIRED ZERO CELL                     
         MVI   SPUPBTYP,C'W'                                                    
*                                                                               
SETSCB06 CHI   R0,7000                FOR 7000-7500 WE HAVE TO CHECK            
         BL    SETSCBLX               FOR ALTERNATE SYSCODE                     
         CHI   R0,7500                                                          
         BH    SETSCBLX                                                         
*  ============ READ STATION MASTER FOR ALTERNATE SYSCODE ======                
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
***      MVC   DMCB+12(4),DBAREC                                                
         MVC   DMCB+12(4),SPUPAREC                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
         LA    R3,WORK             BUILD KEY OF STATION RECORD                  
         USING STARECD,R3                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'T'                                                     
         SR    R0,R0                                                            
         ICM   R0,3,SPUPSYSC                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  STAKCALL(4),DUB                                                  
         MVI   STAKCALL+4,C'T'     NEED TO APPEND A T                           
         MVC   STAKAGY,SPUPAGY     AGENCY CODE                                  
         MVC   STAKCLT,=C'000'     READ AGENCY RECORD ONLY                      
         L     RE,VCOMFACS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETSYSC2            AND NO SAVE AREA AVAILABLE                   
         CLC   SVCBLKEY(12),STAKEY TEST SAME STA/CLT AS PREV                    
         BE    GETSYSC4                                                         
         MVC   SVCBLKEY,STAKEY     ELSE SAVE STA/CLT NOW                        
         DROP  R3                                                               
*                                                                               
GETSYSC2 GOTO1 (RF),(R1)                                                        
***      L     R9,DBAREC                                                        
         L     R3,SPUPAREC                                                      
         USING STARECD,R3                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCBLKUP,SCBLLKUP   SET 2 BYTE ALTERNATE SYSCODE                 
         DROP  R3                                                               
*                                                                               
GETSYSC4 MVC   SPUPSYSC,SVCBLKUP                                                
                                                                                
SETSCBLX DS    0H                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         J     XIT01                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETMKTCS CSECT                                                                  
SVMKTKEY DS    CL8                                                              
SVLPMDTB DS    XL3                                                              
SVLPMDTP DS    XL2                                                              
SVMKCDEM DS    C                                                                
SVMKALPH DS    CL3                                                              
SVCBLKEY DS    CL12                                                             
SVCBLKUP DS    H                                                                
         EJECT                                                                  
*--------------------------------------------------------------------           
*                                                                               
SUBR02   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR02**,RA,RR=RE                                              
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         ST    R1,PARM02                                                        
         ST    RE,RELO3            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
INITQ    EQU   (INIT#-*)/4+1                                                    
STNHPTS  EQU   (STNHPT#-*)/4+1                                                  
BLDLATBQ EQU   (BLDLATB#-*)/4+1                                                 
READPRFE EQU   (RDPROF#-*)/4+1                                                  
         SPACE 1                                                                
INIT#    B     INIT                INITIAL VARIABLES                            
STNHPT#  B     STNHPT              PROCESS IN MARKET SHARES                     
BLDLATB# B     BLDLATBK            BUILD LATEST UPGRADE BOOK EXTENSION          
RDPROF#  B     READPROF            READ PROFILE                                 
         EJECT                                                                  
ERR02    L     RD,SAVERD           ENTER HERE ON ERROR                          
XIT02    XIT1                      EXIT SUBR02  -- RTN TO CALLER                
***********************************************************************         
* INITIALIZATION ROUTINE                                              *         
***********************************************************************         
*                                                                               
         USING SPDEMUPD,R9         R9=A(LOOK-UP/UPG   E EXPRESSION)             
INIT     L     RF,SPUPAFAC                                                      
         ST    RF,VCOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOMTH,CDEMOMTH                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VDEMAINT,CDEMAINT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDEMTABS,CDEMTABS                                                
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A7A' STAPACK                               
         MVC   VSTAPACK,0(R1)                                                   
***                                                                             
         GOTO1 VCALLOV,DMCB,0,X'D9000A26' DEFINE                                
         MVC   VDEFINE,0(R1)                                                    
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A24' SPGETIUN                              
*&&DO                                                                           
*REMOVED BPOO ON NOV14/19 CAUSES DUMPS IN BUY PROGRAM                           
*WE DO NOT HAVE R7 POINTING TO DBLOCK HERE - CANT                               
* REFER TO DBLOCK                                                               
* WE SEEM NOT TO NEED THESE INSTRUCTIONS ANYWAYS                                
* BECAUSE THEY GET SET BEFORE EVERY GETIUN CALL                                 
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*&&                                                                             
         MVC   VGETIUN,0(R1)                                                    
**                                                                              
         OI    SPUPOPTS,SPOPDMAI           SHOULD ALWAYS BE DMA IMP             
***                                                                             
*                                                                               
**       L     RF,=V(SUBR01)                                                    
**       A     RF,RELO                                                          
**       ST    RF,VSUBR01                                                       
         MVI   Y4ASW,X'00'                                                      
         MVI   Y4MSW,X'00'                                                      
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4SYRS,2                                                         
         MVI   SBKCTRL,0                                                        
                                                                                
         MVI   TAPEOPT,C'N'        NORMAL PRECISION IS BOOK                     
         MVI   NORMHPT,C'N'        NORMALIZE HPT VALUES                         
         MVI   PRECUPG,C'N'        1 DECIMAL UPGRADE VALUES                     
         TM    SPUPTYPE,X'C0'                                                   
         BNM   *+12                                                             
         MVI   PRECUPG,C'Y'                                                     
         XI    SPUPTYPE,X'40'                                                   
         CLI   SPUPMED,C'T'        USTV                                         
         BNE   FIXARBX                                                          
         CLI   SPUPSRC,C'A'        AND ARB                                      
         BNE   FIXARBX                                                          
         MVI   SPUPSRC,C'N'        SWITCH TO NSI                                
FIXARBX  DS    0C                                                               
*                                                                               
                                                                                
* FOR SPOT SYSTEM- IF SPUPSYSE IS SET THEN WE MUST DO A FEW THINGS              
* TO GET THE SYSCODE INTO THE DEMO SYSTEM                                       
                                                                                
         OC    SPUPSYSC,SPUPSYSC   IF BINARY SYSCODE IS PASSED-IGNORE           
         BNZ   INITSYSX            4 CHAR SPOT SYSCODE                          
         OC    SPUPSYSE,SPUPSYSE                                                
         BZ    INITSYSX                                                         
*                                                                               
         MVI   SPUPSRC,0           SET SPUPSRC INVALID                          
         GOTO1 VSUBR01,DMCB,('GETMRECQ',(RC))   GET SPOT MARKET REC             
* IF SRC IS NOT SET FROM THE PROFILE AND DEMO OVERRIDE LIST                     
* IS PASSED IN THEN PASS BACK OVERRIDE LIST                                     
         CLI   SPUPSRC,0                                                        
         BNE   INITSYS                                                          
         L     RF,=A(GODEMOUT)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
*                                                                               
         B     INITX                            EXIT IF NOT VALID               
INITSYS  GOTO1 VSUBR01,DMCB,('SETSCBLQ',(RC))   SET CABLE INFO                  
INITSYSX DS    0C                                                               
         MVC   SVSYSC,SPUPSYSC                                                  
*                                                                               
         CLI   SPUPSRC,C'F'        FUSIONS ALSO DO EXTENDED PREC                
         BE    ENSICHKX                                                         
         CLI   SPUPMED,C'N'        DO EXTENDED PREC FOR NET ALSO                
         BE    ENSICHKX                                                         
         CLI   SPUPMED,C'T'        EXTENDED PRECISION NSI/USTV ONLY             
         BNE   ENSINO                                                           
         CLI   SPUPSRC,C'N'                                                     
         BNE   ENSINO                                                           
         OC    SPUPFBKL,SPUPFBKL   FOR MULTI-BOOK REQUESTS,                     
         BNZ   ENSINO              FORCE RTG-BASED TO PREVENT OVERFLOW          
*                                                                               
         CLI   SPUPMED,C'W'        WEEKLY MULTIBOOK.  WEEKLY MUTLIBOOK          
         BNE   *+12                SPUPFBKL NOT FILLED IN.                      
         CLI   SPUPTYPE,X'0F'      ALL THE BOOKS COME IN THE DBEXTEND           
         BE    ENSINO                                                           
*                                                                               
ENSICHKX EQU   *                                                                
         TM    SPUPOPTS,SPOPDMAI                                                
         BZ    *+8                                                              
         MVI   TAPEOPT,C'Y'                                                     
         CLI   SPUPMED,C'N'        NET ALWAYS NORMALIZED                        
         BE    *+12                                                             
         TM    SPUPOPTS,SPOPDMAI+SPOPNORM                                       
         BNO   *+8                                                              
         MVI   NORMHPT,C'Y'        NORMALIZE HPT VALUES                         
                                                                                
ENSINO   EQU   *                                                                
         MVI   EXTBUFF,C'N'        DEFAULT NO EXTENDED BUFF                     
         CLI   SPUPSRC,C'F'                                                     
         BE    INIT20                                                           
         CLI   SPUPFIL,C'T'        TEST FOR TPT FILE LOOK-UP                    
         BNE   INITLPMX                                                         
         CLI   SPUPMED,C'T'        ONLY IF - USTV                               
         BNE   INITLPM1                                                         
         CLI   SPUPSRC,C'N'        ONLY IF - NSI                                
         BNE   INITLPM1                                                         
* SET EXTENDED BUFFERS FLAG ON FOR USTV NSI                                     
* WE OVERFLOW SINGLE REGISTER FOR SOME UPGRADES WHEN ACUCMULATING               
* PUTS. IF WE WANT TO USE EXTENDED BUFFER FOR ANYTHING ELSE                     
* JUST TURN ON THIS FLAG                                                        
INIT20   MVI   EXTBUFF,C'Y'                                                     
*                                                                               
         CLI   SPUPBTYP,0          MUST BE  FOR REGULAR OR                      
         BE    *+12                                                             
         CLI   SPUPBTYP,C'H'       HISPANIC BOOK TYPES                          
         BNE   INITLPM1                                                         
*                                                                               
         B     *+16                PATCH OUT FOR TESTING                        
         MVC   SPUPLPM,=X'D0B7'    MAY23/04                                     
         MVC   SPUPMALF(3),=C'NY '                                              
*&&DO                                                                           
         CLC   SPUPSPL,=H'101'     NEW YORK                                     
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(2),=C'NY'  NEW YORK                                     
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'202'     CHI                                          
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'CHI' CHI                                          
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'403'     LA                                           
         JE    UPLPMOLA                                                         
         CLC   SPUPMALF(2),=C'LA'  LA                                           
         JE    UPLPMOLA                                                         
         CLC   SPUPSPL,=H'407'     SF                                           
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(2),=C'SF'  SF                                           
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'104'     PHL                                          
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'PHL' PHL                                          
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'111'     WAS                                          
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'WAS' WAS                                          
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'105'     DET                                          
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'DET' DET                                          
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'223'     DALLAS FT WORTH                              
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'DF'  DALLAS FT WORTH                              
         JE    UPLPMOK                                                          
         CLC   SPUPMALF(3),=C'ATL' ATLANTA                                      
         JE    UPLPMOK                                                          
         CLC   SPUPSPL,=H'168'     ATLANTA                                      
         JE    UPLPMOK                                                          
         J     INITLPM1            N/A - CLEAR THE LPM DATE                     
*&&                                                                             
*CHECK DEMTABS TABLE TO SEE IF LPM MARKET                                       
*                                                                               
         GOTO1 VDEMTABS,DMCB,LPMSTEND   GET A(LOCAL CABLE TABLE)                
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING LPMSETD,RE                                                       
INIT60   CLI   0(RE),X'FF'                                                      
         BE    INIT100                                                          
***      CLC   LPMSETMK,SPUPBTYP   MUST MATCH BOOKTYPE IN TABLE                 
         CLC   LPMSEBKT,SPUPBTYP   MUST MATCH BOOKTYPE IN TABLE                 
         BNE   INIT70              IF LPM MKT DOESNT EXIST FOR BOOKTYPE         
*                                  WE WE'LL CLEAR OUT THE BOOKTYPE              
*                                                                               
         CLC   LPMSETMK,SPUPSPL    MATCH NUMERIC MKT?                           
         BE    INIT80                                                           
         CLC   LPMAMKT,SPUPMALF    MATCH ALPHA MKT?                             
         BE    INIT80                                                           
INIT70   AR    RE,RF                                                            
         B     INIT60                                                           
INIT80   J     UPLPMOK                                                          
INIT100  J     INITLPM1            N/A - CLEAR THE LPM DATE                     
         DROP  RE                                                               
UPLPMOLA DS    0C                                                               
***      CLI   DBBTYPE,C'H'        NO HISPANIC FOR SOME                         
         CLI   SPUPBTYP,C'H'        NO HISPANIC FOR SOME                        
         JE    INITLPM1                                                         
*                                                                               
UPLPMOK  OC    SPUPLPM,SPUPLPM     AND AN LPM DATE                              
         BZ    INITLPMX                                                         
         GOTO1 VDATCON,DMCB,(2,SPUPLPM),(3,UPLPMDAT)                            
         CLI   SPUPBTYP,0          REGUALR TO P                                 
         BNE   *+8                                                              
         MVI   ALTBTYP,C'P'                                                     
         CLI   SPUPBTYP,C'H'       HISPANIC TO I                                
         BNE   *+8                                                              
         MVI   ALTBTYP,C'I'                                                     
INITLPM1 XC    SPUPLPM,SPUPLPM     CLEAR IT BECAUSE IT'S ALSO SPUPPUR           
INITLPMX DS    0C                                                               
*                                                                               
* THIS FOLLOWING SETS UP YRSAVTYP WHICH IS USED TO INDICATE                     
* WE ARE DOING SOME SORT OF SAVG OR PAVG UPGRADE                                
* WHICH WILL USE THE EXTENDED DOUBLE WORD BUFFERS                               
*                                                                               
         MVI   YRSAVTYP,0                                                       
         CLI   SPUPTYPE,13                                                      
         BE    *+8                                                              
         CLI   SPUPTYPE,14                                                      
         BNE   INITMBK                                                          
         MVI   EXTBUFF,C'Y'          ALWAYS USE EXTENDED                        
         MVI   YRSAVTYP,C'G'         DEFAULT PAVG - SET TO G                    
         CLI   SPUPFLD2,C'Q'         PAQ/SAQ/PAY/SAY/PAB/SAB                    
         BE    *+8                                                              
         CLI   SPUPFLD2,C'B'         PAQ/SAQ/PAY/SAY/PAB/SAB                    
         BE    *+8                                                              
         CLI   SPUPFLD2,C'Y'                                                    
         BNE   *+14                                                             
         MVC   YRSAVTYP,SPUPFLD2                                                
         MVI   SPUPFLD2,0          CLEAR THE 1 BYTE FROM DDUPVAL                
*                                                                               
INITMBK  CLI   SPUPTYPE,X'0F'      WEEKLY MULITBOOK AVERAGE?                    
         BE    *+8                                                              
         CLI   SPUPTYPE,X'0A'      IMS NEW CODE TO NOT BREAK NWS                
         BE    *+8                                                              
         CLI   SPUPTYPE,X'04'      MONTHLY MULITBOOK AVERAGE?                   
         BNE   INITX               MULTIBOOK AVERAGE LET                        
         TM    SPUPOPTS,SPOPDMAI   DMA BE SET BY CALLING APPLICATION            
         BZ    *+8                                                              
         MVI   TAPEOPT,C'Y'                                                     
*                                  ONLY DMA=I, WTP COULD BE IMPRESSIONS         
         MVI   MBKIFLAG,C'N'       BASED FOR MULTIBOOK AVERAGE FOR NOW          
         CLI   SPUPTYPE,X'0F'      WEEKLY MULITBOOK AVERAGE?                    
         BE    *+8                                                              
         CLI   SPUPTYPE,X'04'      MULTIBOOK AVERAGE                            
         BNE   *+16                                                             
         CLI   TAPEOPT,C'Y'        DMA=I USE EXPANDED BUFFERS                   
         BNE   *+8                                                              
         MVI   MBKIFLAG,C'Y'                                                    
INITX    J     XIT02                                                            
         DROP  RF                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS IN MARKET SHARES:                                           *         
* COMPUTE 'PUT' FROM RTGS OF EACH STATION IN DEFINED 'MKT'            *         
* ROUTINE TO GET OLD OR NEW H/P/T VALUES FROM TIME PERIOD FILE        *         
* ON ENTRY HPT=OLD FOR OLD H/P/T OR NEW FOR NEW H/P/T WITH            *         
* R1=A(BOOK OPTION)                                                   *         
*                                                                     *         
* EXIT WITH H/P/T VALUES AT OLDHPT OR NEWHPT                          *         
***********************************************************************         
         SPACE 1                                                                
STNHPT   DS    0H                                                               
         USING GETHPTWK,R8         R8=A(LOCAL W/S)                              
**       L     R1,PARM01           WAS THIS WRONG??? HAVE TO TEST               
         L     R1,PARM02                                                        
         MVI   GETHIND,0           SET INDICATOR BYTE                           
         CLI   HPT,OLD             CLEAR REQUIRED H/P/T VALUES                  
         BNE   *+14                                                             
         XC    OLDHPT(LENVALS*2),OLDHPT                                         
         B     *+10                                                             
         XC    NEWHPT(LENVALS*2),NEWHPT                                         
*                                                                               
STNINIT  DS    0H                                                               
         XC    STNBK,STNBK                                                      
         ICM   RE,15,4(R1)                                                      
         BZ    STNHPT1                                                          
         CLC   0(2,RE),=X'2000'    TEST IF BOOK PASSED                          
         BNH   *+10                                                             
         MVC   STNBK,0(RE)         YES - SET SELECTED BOOK                      
*                                                                               
STNHPT1  XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
         MVI   DBFUNCT,DBGETDEM    LOOK UP DEMOS FOR EA STN IN LIST             
         MVC   DBSELBK,STNBK                                                    
         MVI   DBTPTT,C'P'         CHANGE TO T4 BASED PUTS                      
*                                                                               
         NI    DBSELBK+1,X'0F'     AND OFF WEEK BITS                            
*****    LA    R1,IOAREA1                                                       
         L     R1,AIO1                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,VCOMFACS   **MUST BE DONE FOR MKT OVERRIDES**           
         TM    SPUPOPTS,SPOPEXT                                                 
*&&DO                                                                           
         BZ    *+10                EXTEND BLOCK NOT IN USE                      
         MVC   DBEXTEND,SPUPEXTN    IN USE - SET IT                             
*&&                                                                             
         BZ    STNHPTG             EXTEND BLOCK NOT IN USE                      
         GOTO1 VSUBR01,DMCB,('INXTNDQ',(RC))   IN USE - INSERT IT               
STNHPTG  EQU   *                                                                
         MVC   DBSELAGY,SPUPAGY    RESTORE AGENCY                               
         MVC   DBSELUMK,SPUPMKT    SET AGENCY MKT CODE                          
         MVC   DBSELCLI,SPUPCLI    SET AGENCY CLIENT CODE                       
         MVC   DBSELMED,SPUPMED                                                 
         TM    SPUPOPTS,SPOANGFR   CANADIAN ANGLO/FRANCO OPTION                 
         BZ    *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
         TM    SPUPOPT2,SPO2CMON   CANADIAN WEEKLIES AS MONTH                   
         BZ    *+8                                                              
         MVI   DBBEST,C'M'                                                      
         TM    SPUPOPT2,SPO2UBBM   CANADIAN USE BBM WEEKLIES                    
         BZ    *+8                                                              
         MVI   DBUSEBBM,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTY   SPORTS ONLY OPTION                           
         BZ    *+8                                                              
         MVI   DBSELSPO,C'Y'                                                    
         TM    SPUPOPTS,SPOSPRTN   EXCLUDE SPORTS OPTION                        
         BZ    *+8                                                              
         MVI   DBSELSPO,C'N'                                                    
*                                                                               
         OC    SPUPPUR,SPUPPUR     TEST DAY/TIME LOOK-UP                        
         BNZ   STNHPT2                                                          
         LA    R1,DBLOCK1          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DUB(2),SPUPFBK                                                   
         NI    DUB+1,X'0F'         AND OFF WEEK BITS                            
         B     STNHPT4                                                          
*                                                                               
STNHPT2  L     R1,SPUPAREC                                                      
         MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
         MVC   DBSELSRC,PRSRC-PRKEY(R1)                                         
         CLI   DBSELMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   DBSELSRC,SPUPSRC                                                 
         MVC   DBBTYPE,PRBTYP-PRKEY(R1)                                         
         MVC   DUB(2),PRBOOK-PRKEY(R1)                                          
*                                                                               
STNHPT4  MVC   SVBTYPE,DBBTYPE     SAVE INITIAL BOOK TYPE                       
         CLI   DBBTYPE,C'A'        PARENT ONLY P+S COMBO                        
         BE    *+8                                                              
         CLI   DBBTYPE,C'O'        OLYMPIC BOOK                                 
         BNE   *+8                                                              
         CLI   HPT,NEW             AND NEW HPT                                  
         BNE   *+8                                                              
         MVI   DBBTYPE,0           KILL BOOK TYPE                               
         OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         BNZ   *+10                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
*                                                                               
STNHPT10 CLI   PUREFLAG,YES        SET DAY/TIME FOR LOOK-UP                     
         BE    STNHPT12                                                         
         MVC   DBSELDAY,SPUPDAY    SET DAY/TIME FROM UPGRADE BLOCK              
         MVC   DBSELTIM,SPUPTIM                                                 
         CLI   HPT,NEW                                                          
         BE    STNHPT13                                                         
         LA    R1,DBLOCK1          SET DAY/TIME FROM DBLOCK1                    
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         B     STNHPT13                                                         
*                                                                               
STNHPT12 MVC   DUB+0(1),PUREDW     EXTRACT DAY/TIME FROM PAV RECORD             
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))       DAY IN DEMAND FMT            
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB+0(1),PURESTIM                                                
         MVC   DUB+1(1),PUREDUR                                                 
         GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))       TIME IN DEMAND FMT           
         MVC   DBSELTIM,DUB+4                                                   
*                                                                               
STNHPT13 DS    0H                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         LA    RE,GETSTLST                                                      
         USING DBXMSD,RE                                                        
         XC    DBXMSD(20),DBXMSD                                                
         MVC   DBXMSID,=C'MSTA'                                                 
         LA    R0,DBXMSTA                                                       
*                                                                               
* COMPARE TO SEE IF WE WANT IMS OR ISS                                          
*                                                                               
         CLI   IMSFLAG,C'S'                                                     
         BNE   STNHPT15                                                         
         L     RF,STNAFFL                                                       
         LA    R1,DBXMSTA                                                       
STNHPT14 CLI   0(RF),0              EOL                                         
         BE    STNHPT16                                                         
         MVC   0(5,R1),0(RF)        MOVE STATIONS INTO MSTA LINK                
         MVI   4(R1),C'T'                                                       
         MVI   5(R1),0              EOL                                         
         AHI   R1,5                                                             
         AHI   RF,5                                                             
         B     STNHPT14                                                         
*                                                                               
STNHPT15 GOTO1 VDEINMKT,DMCB,DBLOCKD,STNAFFL,(R0)                               
STNHPT16 LA    RE,GETSTLST                                                      
         OC    DBXMSTA,DBXMSTA                                                  
         BZ    STNHPTNX            NO STATIONS TO LOOK UP-- ERROR               
         ST    RE,DBEXTEND                                                      
         DROP  RE                                                               
*                                                                               
* CHECK IF WE HAVE TO SET UP DAYTIME ROTATION LIST IN EXTENSION                 
* CHECK ORIGINAL SPUPEXTN AREA                                                  
*                                                                               
         ICM   RE,15,SPUPEXTN                                                   
         LTR   RE,RE                                                            
         BZ    STNHPT17                                                         
         USING SPIMSD,RE                                                        
         CLC   SPIMSID,=C'DYTM'    IN MKT SHARE REQUESTED?                      
         BE    *+12                                                             
         L     RE,SPIMSNXT         BUMP TO NEXT EXTN BLOCK                      
         B     *-20                                                             
         DROP  RE                                                               
         LA    RF,GETSTLST                                                      
         USING DBXMSD,RF           SET DYTM LIST AS NEXT LINK                   
         ST    RE,DBXMSNXT                                                      
         DROP  RF                                                               
*                                                                               
*                                                                               
STNHPT17 XC    GETSTNBK(9),GETSTNBK                                             
         MVI   GETSTCNT,0          AND # STN'S COUNTER                          
* SET DBCOPT TO X'40' - LIVE ONLY TRANSPARENCY CODE                             
         MVI   DBVOPT,X'40'                                                     
         GOTO1 VDEMAND,DMCB,DBLOCK,STNHK                                        
         MVC   DBBTYPE,SVBTYPE                                                  
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BNE   STNHPTNX            NO - EXIT ON ANY ERROR SETTING               
         MVI   DBERROR,0                                                        
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    STNHPTNX                                                         
*                                                                               
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         CLI   HPT,OLD             TEST OLD HPT'S REQUIRED                      
         BE    STNHPT21            YES                                          
         CLI   SPUP2YRP,C'Y'       CALLER CAN SPECIFY 2 YEAR PUTS               
         BE    *+12                                                             
         TM    DBCPUTS,DBO2YEAR    TEST 2 YEAR AVERAGE PUT REQUIRED             
         BZ    STNHPT21            NO                                           
         TM    GETHIND,X'80'       TEST BOTH YEARS DONE                         
         BNZ   STNHPT18            YES                                          
         ZIC   R1,GETHBK           DECREMENT ACTUAL BOOK YEAR                   
         BCTR  R1,0                                                             
         STC   R1,GETHBK                                                        
         MVC   GETHDIV,DBDIVSOR    SAVE DBDIVSOR                                
         OI    GETHIND,X'80'                                                    
         LA    R1,GETHBK           POINT TO DECREMENTED BOOK VALUE              
         B     STNHPT1             GET PREVIOUS YEAR HPT VALUES                 
*                                                                               
STNHPT18 SR    R0,R0               CALCULATE TOTAL DBDIVSOR VALUE               
         ICM   R0,3,DBDIVSOR                                                    
         SR    R1,R1                                                            
         ICM   R1,3,GETHDIV                                                     
         AR    R0,R1                                                            
         STCM  R0,3,DBDIVSOR                                                    
         B     STNHPT21                                                         
*                                                                               
STNHPT21 CLI   GETSTCNT,0                                                       
         BE    STNHPT22                                                         
         SR    R1,R1                                                            
         ICM   R1,3,DBDIVSOR       DIVIDE DBDIVSOR BY # STATIONS HIT            
         SR    R0,R0                                                            
         SR    R2,R2                                                            
         IC    R2,GETSTCNT                                                      
         DR    R0,R2                                                            
         STCM  R1,3,DBDIVSOR                                                    
*                                                                               
STNHPT22 LA    R0,NUMVALS*2        UNWEIGHT HPT VALUES                          
         LA    R1,OLDHPT           R1=A(OLD OR NEW HPT LINE)                    
         CLI   HPT,OLD                                                          
         BE    *+8                                                              
         LA    R1,NEWHPT                                                        
         SR    R2,R2                                                            
         ST    R1,DMCB+8                                                        
         SR    R2,R2                                                            
         GOTO1 VSUBR01,DMCB,('UNWGHTQ',(RC)),(R0),,(R2)                         
         CLI   HPT,OLD                                                          
         BE    *+10                                                             
         MVC   OLDHPT(LENVALS*2),NEWHPT                                         
         MVI   DBERROR,0                                                        
         B     STNHPTX                                                          
*                                                                               
STNHPTNX MVI   DBERROR,X'10'      SET ERROR TO NOT FOUND                        
*                                                                               
STNHPTX  B     XIT02                                                            
         EJECT                                                                  
***********************************************************************         
* STNHPT DEMAND HOOK TO ACCUM RTGS FROM EACH RECD RETURNED                      
***********************************************************************         
         SPACE 1                                                                
STNHK    ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         XC    GETHPTUN(LENVALS),GETHPTUN  CLEAR UNIVERSES                      
         XC    GETHPTRT(LENVALS*2),GETHPTRT CLEAR RTG/IMPS                      
         XC    GETHPTS(LENVALS*2),GETHPTS   CLEAR HPTS                          
*                                                                               
         TM    GETHIND,X'40'       TEST ACTUAL BOOK SET                         
         BNZ   *+14                                                             
         OI    GETHIND,X'40'                                                    
         MVC   GETHBK,DBACTBK                                                   
*                                                                               
         LH    R0,DBFACTOR         FORCE NO WEIGHTING IN GETIUN                 
         MVC   DBFACTOR,=H'1'                                                   
                                                                                
         TM    SPUPOPT2,SPOP2IPR   2 DECIMAL IMPRESSION PREC?                   
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   2 DECIMAL IMPRESSIONS                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),GETHPTUN                                 
         STH   R0,DBFACTOR                                                      
*                                                                               
         MVC   GETHPTS(LENVALS*2),GETHPTRT  COPY RTS-IMPS TO PUT-TOTS           
*                                                                               
         LA    R0,NUMVALS*2                                                     
         LA    R1,GETHPTS          R1=A(UNWEIGHTED DEMO VALUES)                 
         LA    R2,OLDHPT           R2=A(WEIGHTED DEMO ACCUMULATORS)             
         CLI   HPT,OLD                                                          
         BE    STNHK20                                                          
         LA    R2,NEWHPT                                                        
         OC    LUNV(LENVALS),LUNV  TEST IF LOONEYVERSES THERE                   
         BNZ   *+10                YES                                          
         MVC   LUNV(LENVALS),GETHPTUN                                           
*                                                                               
STNHK20  ST    R1,DMCB+8                                                        
         GOTO1 VSUBR01,DMCB,('WGHTUPQ',(RC)),(R0),,(R2)                         
*                                                                               
         LA    RE,GETSTNBK         UPDATE STN-HIT TABLE                         
         LA    R0,GETSTNQ          MAX TABLE SIZE                               
STNHK25  CLI   0(RE),0             EOT?                                         
         BE    STNHK28             UPDATE TABLE IF NOT FOUND                    
         CLC   DBSELSTA,0(RE)      MATCH STATION AND BOOK                       
         BNE   *+14                                                             
         CLC   DBACTBK,5(RE)       SAME BOOK?                                   
         BE    STNHK30             MATCH, NO UPDATE NECC                        
         LA    RE,7(RE)                                                         
         BCT   R0,STNHK25                                                       
         DC    H'0'                REACHED MAX STN SIZE                         
STNHK28  MVC   0(5,RE),DBSELSTA                                                 
         MVC   5(2,RE),DBACTBK                                                  
         XC    7(9,RE),7(RE)       CLEAR FOR NEXT ENTRY                         
         ZIC   RE,GETSTCNT         UPDATE NUMBER STATIONS IN TABLE              
         LA    RE,1(RE)                                                         
         STC   RE,GETSTCNT                                                      
*                                                                               
STNHK30  L     RE,SAVERE           RETURN TO DEMAND FOR NEXT RECORD             
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
* READ 1W PROFILE FOR DMAL7 PROFILE                                             
****************************************************************                
READPROF DS    0H                                                               
*                                                                               
* DMA L7 IMP PROFILE IS TURNED ON USE NEW FORMULA FOR TSA IMPRESSIONS           
* I=R*U                                                                         
* ONLY SET DMAL7EBK FOR L7 BOOKTYPES BECAUSE THIS PROFILE ONLY IMPACTS          
* L7 BOOKTYPES                                                                  
         XC    DMAL7EBK,DMAL7EBK                                                
         CLI   DBBTYPE,BOOKTYPE_Y7  IMPACT L7                                   
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_YH  IMPACT H                                    
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_YB  IMPACT B                                    
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_YC  IMPACT C                                    
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_YW  IMPACT W                                    
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_C   L7 BOOKTYPES                                
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W                                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_I                                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_H                                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_B                                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_P                                               
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_STANDARD                                        
         BNE   XIT02               L7 BOOKTYPE COPY TSA FROM DMA                
*                                                                               
         XC    WORK2,WORK2                                                      
         MVC   WORK2+0(4),=C'S01W'                                              
         MVC   WORK2+4(2),SPUPAGY                                               
         MVI   WORK2+6,C'T'                                                     
         L     RF,DBCOMFCS                                                      
         USING COMFACSD,RF                                                      
         GOTOR CGETPROF,DMCB,WORK2,PROF1W,CDATAMGR                              
         MVC   DMAL7EBK,PROF1W+10                                               
         J     XIT02                                                            
*                                                                               
****************************************************************                
* BUILD EXTENSION FOR LATEST UPGRADE BOOK FOR CABLE                             
****************************************************************                
* CHECK ALL THE BOOKS AREA TO FIND THE LATEST BOOK                              
* IF ANY BOOKS IS JAN08 OR LATER CREATE INDICATE WE WANT TO                     
* APPLY LATEST INDEX FROM THE LATEST BOOK TO ALL COMPONENTS                     
* THIS IS DONE IN DEGETTP- DEDEMOUT WILL NEED TO KNOW NOT TO                    
* ADJUST THE NUMBERS AGAIN.                                                     
BLDLATBK OC    SPUPSYSC,SPUPSYSC                                                
         BZ    BLDLATX                                                          
*                                                                               
         MVC   LATINDBK,SPUPFBK                                                 
*&&DO                                                                           
         CLC   SPUPFBK(2),LATINDBK                                              
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFBK                                                 
         CLC   SPUPFBK+2(2),LATINDBK                                            
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFBK+2                                               
         CLC   SPUPFBK+4(2),LATINDBK                                            
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFBK+4                                               
*&&                                                                             
         CLC   SPUPFLD1(2),LATINDBK                                             
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFLD1                                                
         CLC   SPUPFLD2(2),LATINDBK                                             
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFLD2                                                
         CLC   SPUPFLD3(2),LATINDBK                                             
         BL    *+10                                                             
         MVC   LATINDBK,SPUPFLD3                                                
* CHECK FOR MBK EXTENSION                                                       
BLDLAT10 ICM   RE,15,DBEXTEND      CHECK FOR MULTIPLE BOOKS                     
         LTR   RE,RE                                                            
BLDLAT14 BZ    BLDLAT20                                                         
         USING DBXMBID,RE                                                       
         CLC   DBXMBID,=C'MBKS'                                                 
         BE    *+12                                                             
         ICM   RE,15,DBXMBNXT                                                   
         B     BLDLAT14                                                         
         LA    RF,DBXMBKS                                                       
         DROP  RE                                                               
BLDLAT18 CLI   0(RF),0                                                          
         BE    BLDLAT20             SURVIVE IT                                  
         CLC   0(2,RF),LATINDBK                                                 
         BL    *+10                                                             
         MVC   LATINDBK,0(RF)                                                   
         AHI   RF,2                                                             
         B     BLDLAT18                                                         
* BUILD EXTENSION  OR LATEST INDEX BOOK                                         
BLDLAT20 LA    RF,LBEXTEND                                                      
         XC    LBEXTEND,LBEXTEND                                                
         ICM   RE,15,DBEXTEND       POINT TO THE FIRST BLOCK                    
         BNZ   BLDLAT22                                                         
         ST    RF,DBEXTEND                                                      
         B     BLDLAT30                                                         
BLDLAT22 DS    0C                                                               
*                                                                               
BLDLAT26 CLC   0(4,RE),=XL4'00'     NEXT LINK IS EMPTY ALREADY                  
         BE    BLDLAT30                                                         
         CLC   4(4,RE),=XL4'00'     LOOK AT ITS POINTER                         
         BE    BLDLAT28             ZERO MEANS WE CAN USE IT                    
         L     RE,4(RE)             ELSE, POINT TO NEXT                         
         B     BLDLAT22             AND LOOP                                    
BLDLAT28 DS    0H                                                               
         ST    RF,4(RE)             POINT TO IT                                 
         USING DBLATUBK,RF                                                      
BLDLAT30 MVC   DBLBKID(4),=C'UPBK'                                              
         MVC   DBLUPBK,LATINDBK                                                 
         DROP  RF                                                               
*                                                                               
*                                                                               
BLDLATX  J     XIT02                                                            
         EJECT                                                                  
*********************************************************************           
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB,RA                                                            
*=====================================================================          
* CALL TO DEMOUT MOVED HERE FOR ADDRESSABILITY PROBLEMS 9/17/04  MHER           
*=====================================================================          
                                                                                
GODEMOUT NTR1  BASE=*,LABEL=*                                                   
         CLI   SPUPSRC,0           IF SRC = NOT SET- THIS SHOULD ONLY           
         BNE   GODM01              BE FOR CABLE AT THIS POINT                   
         ICM   R2,15,SPUPAOVR      POINT TO USER DEMO OVERRIDE LIST             
         BZ    GODMOX              ELSE JUST SET OVERRIDES INTO DEMO            
         B     GODM25              LIST.                                        
*                                                                               
GODM01   OC    ADEMLST,ADEMLST     TEST DEMO LOOK-UP REQUIRED                   
         BZ    GODM20                                                           
*                                                                               
         CLI   LOCALMED,MEDCAN     CANADIAN LOOK-UPS                            
         BE    GODMO2                                                           
*                                                                               
         L     RE,SPUPAREC                                                      
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVC   DBCOMFCS,SPUPAFAC                                                
**       CLI   SPUPSRC,C'F'         SET SYSCODE FOR FUSION                      
**       BNE   *+10                                                             
         MVC   DBSELSYC,SPUPSYSC                                                
         MVC   DBSELSYC,SVSYSC     FORCE FROM SV SYSCODE                        
                                                                                
GODMO2   ICM   R0,15,ADEMLST       TEST DEMO LOOK-UP REQUIRED                   
         BZ    GODM20                                                           
*                                                                               
GODM10   GOTO1 VDEMOUT,DMCB,(C'L',(R0)),DBLOCK,ADEMOUT                          
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
GODM11   XC    DBEXTEND,DBEXTEND                                                
         XC    DBSELSYC,DBSELSYC                                                
                                                                                
         ICM   R2,15,SPUPAOVR      POINT TO USER DEMO OVERRIDE LIST             
         BZ    GODM20              FLAG USER OVERRIDES IN DEMO LIST             
GODM12   CLI   0(R2),0             TEST E-O-R                                   
         BE    GODM20                                                           
         CLI   0(R2),OVERELEM      TEST DEMO OVERRIDE ELEMENT                   
         BNE   GODM16                                                           
         L     RF,ADEMLST          LOOK-UP DEMO IN LIST                         
         L     R1,ADEMOUT                                                       
GODM14   CLI   0(RF),X'FF'         TEST E-O-L                                   
         BE    GODM16                                                           
         CLC   2(1,RF),3(R2)       MATCH ON DEMO                                
         BE    *+16                                                             
         LA    RF,3(RF)                                                         
         LA    R1,4(R1)                                                         
         B     GODM14                                                           
*                                                                               
         ST    R2,DMCB+4                                                        
         ST    RF,DMCB+8                                                        
         ST    R1,DMCB+12                                                       
         GOTO1 VSUBR01,DMCB,('SETOVRQ',(RC))                                    
GODM16   ZIC   R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     GODM12                                                           
                                                                                
GODM20   MVI   SPUPPRG,C' '        RETURN PROGRAM NAME(S)                       
         MVC   SPUPPRG+1(L'SPUPPRG-1),SPUPPRG                                   
         MVC   SPUPPRG(L'STPROG),STPROG                                         
         CLC   STPROG,NDPROG                                                    
         BE    *+14                                                             
         MVI   SPUPPRG+7,C'/'                                                   
         MVC   SPUPPRG+8(7),NDPROG                                              
                                                                                
*================================================================               
* SET X'40' FLAGS IN DEMO RATINGS IF 2-DECIMAL LOOKUP                           
*================================================================               
                                                                                
GODM25   L     RE,ADEMLST                                                       
         L     RF,ADEMOUT                                                       
*                                                                               
GODM30   CLI   0(RE),X'FF'                                                      
         BE    GODMOX                                                           
         TM    SPUPOPTS,SPOP2DEC   TEST 2-DEC RTG LOOKUP                        
         BZ    GODM40                                                           
         CLI   1(RE),C'R'          IF RTG OR EXTENDED                           
         BE    GODM45               TURN ON 2-DEC INDICATOR                     
         CLI   1(RE),C'E'                                                       
         BE    GODM45                                                           
*                                                                               
GODM40   TM    SPUPOPT2,SPOP2IPR   TEST 2-DEC IMP LOOKUP                        
         BZ    GODM50                                                           
         CLI   1(RE),C'I'          IF IMP TURN ON 2-DEC INDICATOR               
         BNE   GODM50                                                           
*                                                                               
GODM45   OI    0(RF),X'40'                                                      
*                                                                               
GODM50   AHI   RE,3                                                             
         AHI   RF,4                                                             
         B     GODM30                                                           
*                                                                               
GODMOX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
* CHECK THE BOOK TO SEE IF WE SHOULD KEEP LIVE ONLY/LIVE+3 BOOKTYPE             
* FOR THE NIELSON MONTHLY BOOK                                                  
CHKLIVEM NTR1  BASE=*,LABEL=*                                                   
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKLIVMX                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLIVMX                                                         
**       CLC   DBSELBK,=X'6A0B'      NOV06 AND AFTER                            
**       BNL   CHKLIVMX                                                         
* FOR ANY BOOK PRIOR TO NOV/06 WE DONT HAVE LIVE ONLY BOOKS                     
* SO ALWAYS LOOK AT LIVE PLUS BOOKTYPES                                         
                                                                                
         GOTO1 VDEMTABS,DMCB,LIVEBKTY   GET A(LIVE BOOKTYPE TABLE)              
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING LVBKTYD,RE                                                       
CHKLVM30 CLC   =X'FFFF',0(RE)                                                   
         BE    CHKLIVMX                                                         
         CLC   DBBTYPE,LVONLYBT                                                 
         BE    CHKLVM60                                                         
         AR    RE,RF                                                            
         B     CHKLVM30                                                         
CHKLVM60 DS    0C                    FOUND LIVE+ BOOKTYPE                       
         CLC   DBSELBK,LEFFBOOK      AFTER EFFECTIVE BOOK WHEN DATA             
         BNL   CHKLIVMX              IS AVAILABLE?                              
         OC    DBSELBK,DBSELBK       LATEST DONT USE LIVE+ BOOKTYPE             
         BZ    CHKLIVMX                                                         
*                                                                               
         MVC   DBBTYPE,LVPLUSBT      USE LIVE+ BOOKTYPE                         
         DROP  RE                                                               
*&&DO                                                                           
         CLI   DBBTYPE,C'L'          LIVE ONLY                                  
         BNE   *+8                                                              
         MVI   DBBTYPE,0             LIVE PLUS                                  
         CLI   DBBTYPE,C'Z'          LIVE ONLY WIRED                            
         BNE   *+8                                                              
         MVI   DBBTYPE,C'W'          LIVE PLUS WIRED                            
         CLI   DBBTYPE,C'U'          LIVE ONLY CABLE                            
         BNE   *+8                                                              
         MVI   DBBTYPE,C'C'          LIVE PLUS CABLE                            
         CLI   DBBTYPE,C'J'          LIVE ONLY HISPANIC                         
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'          LIVE PLUS HISPANIC                         
*&&                                                                             
CHKLIVMX XIT1                                                                   
         LTORG                                                                  
****************************************************************                
CHKLPM   NTR1  BASE=*,LABEL=*                                                   
         XC    UPLPMSD(4),UPLPMSD  CLEAR THE LPM DATES                          
         MVI   LPMHSP,C'N'                                                      
*                                                                               
         CLI   DBBTYPE,C'B'           BLACK SURVERY HAS DIFFERENT               
         BE    CHKLPMX                TOTALS - KEEP IT.                         
*                                                                               
         CLI   MMUPASS,2               SECOND MMU PASS                          
         BNE   CHKLPM2                                                          
         CLI   DBBTYPE,C'P'            NEEDS TO RESET BTYPE                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
         B     CHKLPMX                                                          
*&&DO                                                                           
CHKLPM2  CLI   DBBTYPE,C'H'              CONTROL HISPANIC SEPARATELY            
         BE    UPLPMHSP                                                         
         CLI   DBBTYPE,C'I'                                                     
         BE    UPLPMHSP                                                         
*&&                                                                             
* CALL DEDEMTABS TO GET LPM START-END TABLE                                     
*                                                                               
CHKLPM2  GOTO1 VDEMTABS,DMCB,LPMSTEND  GET A(LPM DUAL DATA DATES)               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
         USING LPMSETD,RE                                                       
CHKLPM3  CLI   0(RE),X'FF'         EOT?                                         
         BE    UPLPMSET                                                         
         CLC   DBBTYPE,LPMSEBKT    SAME BOOKTYPE?                               
         BNE   CHKLPM5                                                          
         CLC   DBSELRMK,LPMSETMK   SAME MARKET                                  
         BNE   CHKLPM5                                                          
         MVC   UPLPMSD(4),LPMSTART                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   LPMHSP,C'Y'                                                      
         B     UPLPMSET                                                         
CHKLPM5  AR    RE,R0                                                            
         B     CHKLPM3                                                          
         DROP  RE                                                               
*&&DO                                                                           
* TAKE THIS OUT- CALL DEDEMTABS TO DERIVE UPLPMSD                               
************************************************************                    
         CLC   DBSELRMK,=H'101'          NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   DBSELRMK,=H'403'          LA                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68056807'                                          
         CLC   DBSELRMK,=H'202'          CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         CLC   DBSELRMK,=H'407'          SF                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'680A680C'                                          
         CLC   DBSELRMK,=H'111'          WAS   MAY05,JUN05 PARALLEL             
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
         CLC   DBSELRMK,=H'104'          PHL                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
         CLC   DBSELRMK,=H'105'          DET  NOV05                             
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   DBSELRMK,=H'223'          DALLAS FT WORTH  NOV05                 
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   DBSELRMK,=H'168'          ATLANTA                                
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'6A056A05'                                          
         B     UPLPMSET                                                         
*                                                                               
*                                                                               
UPLPMHSP CLC   DBSELRMK,=H'101'          NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   DBSELRMK,=H'202'          CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         CLC   DBSELRMK,=H'407'          SF                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'680A680C'                                          
         OC    UPLPMSD,UPLPMSD                                                  
         BZ    UPLPMSET                                                         
         MVI   LPMHSP,C'Y'                                                      
         B     UPLPMSET                                                         
*&&                                                                             
****************************************************************                
UPLPMSET OC    UPLPMSD,UPLPMSD                                                  
         BZ    GETHPT14BO                                                       
         CLI   DBBTYPE,C'I'                                                     
         BE    GNYHPT1                                                          
         CLI   DBBTYPE,C'P'        KILL PPM PRIOR                               
         BNE   GNYHPT2                                                          
         CLI   SPUPSYS,C'R'                                                     
         B     GNYHPT1             <<-HARD BRANCH                               
                                                                                
GNYHPT1  CLC   DBSELBK,UPLPMSD     COMPARE TO LPM START                         
         BNL   GNYHPT2              IF LESS THE USE REGULAR                     
         MVI   DBBTYPE,0           DEFAULT DIARY                                
         CLI   LPMHSP,C'Y'                                                      
         BNE   CHKLPMX                                                          
         MVI   DBBTYPE,C'H'        DEFAULT HISPANIC                             
         B     CHKLPMX                                                          
*                                                                               
GNYHPT2  DS    0C                                                               
         CLI   SPUPSYS,C'R'                                                     
         B     GNYHPT4             <<-HARD BRANCH                               
                                                                                
GNYHPT4  DS    0C                                                               
         CLC   DBSELBK,UPLPMED      IF IT'S WITHIN CURRENCY PERIOD              
         BH    CHKLPMX                                                          
         CLC   DBSELBK,UPLPMSD                                                  
         BL    CHKLPMX                                                          
         MVI   DBBTYPE,C'P'         USE THE LPM HPT DATA                        
         CLI   LPMHSP,C'Y'                                                      
         BNE   CHKLPMX                                                          
         MVI   DBBTYPE,C'I'                                                     
         B     CHKLPMX                                                          
GETHPT14BO DS  0H                                                               
*&&DO                                                                           
* REMOVE WE DONT HAVE DATA THAT FAR BACK ANYMORE                                
GETHPT14BO CLC   DBSELRMK,=H'106'    DEAL WITH BOSTON PPM DATA                  
         BNE   CHKLPMX                                                          
         CLC   DBSELBK,=X'6604'                                                 
         BH    CHKLPMX                                                          
         CLC   DBSELBK,=X'650A'                                                 
         BL    CHKLPMX                                                          
         MVI   DBBTYPE,C'P'                                                     
*&&                                                                             
*&&DO                                                                           
         CLI   DBBTYPE,C'O'        OLYMPIC BOOK                                 
         BNE   CHKLPMX                                                          
         CLC   DBSELBK,=X'6202'    EXCEPT FOR FEB/98                            
         BE    CHKLPMX                                                          
         CLC   DBSELBK,=X'6007'    AND JULY/96                                  
         BE    CHKLPMX                                                          
         MVI   DBBTYPE,0           THERE IS NO OLYMPIC BOOK SCHMUCK             
*&&                                                                             
CHKLPMX  XIT1                                                                   
         LTORG                                                                  
*******************************************************************             
* ROUTINE TO CHECK IF ANY OF THE UPGRADE BOOKS IS                               
* NOV14-NOV15 TO USE IMPACT BOOKTYPES                                           
********************************************************************            
CHKLEXPB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* LOOK AT ALL THE BOOKS PARAMETER                                               
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
         MVI   NEXPFLAG,C'N'                                                    
         CLI   MBKIFLAG,C'Y'                                                    
         BE    CHKLIVEX                                                         
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         CLI   DBSELSRC,C'F'       ALLOW FOR FUSION                             
         BE    *+8                 BECAUSE OF IMPACT BOOKTYPES                  
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLEXPX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKLEXPX            IF SHARE BOOK                                
         CLC   SPUPFBK,=AL2(JAN_16)     ARE ANY BOOKS JAN16 OR LATER?           
         BL    CHKLEXPX                                                         
*                                                                               
         CLC   SPUPFLD1,=AL2(NOV_14) IF A VALID BOOK BTWEEN NOV14-NOV15         
         BL    *+14                                                             
         CLC   SPUPFLD1,=AL2(NOV_15)                                            
         BNH   CHKLEXPY                                                         
*                                                                               
         CLC   SPUPFLD2,=AL2(NOV_14) IF A VALID BOOK BTWEEN NOV14-NOV15         
         BL    *+14                                                             
         CLC   SPUPFLD2,=AL2(NOV_15)                                            
         BNH   CHKLEXPY                                                         
*                                                                               
         CLC   SPUPFLD3,=AL2(NOV_14) IF A VALID BOOK BTWEEN NOV14-NOV15         
         BL    *+14                                                             
         CLC   SPUPFLD3,=AL2(NOV_15)                                            
         BNH   CHKLEXPY                                                         
*                                                                               
* ANY SHARE BOOK EFFECTIVE OCT19 (IMPACT DATA LIVE ON OCT19 BOOK)               
* ALSO USE TRANSPARENCY CODE SO THEY CAN LOOK UP BOOKS                          
* FOR PARALLEL IMPACT BOOKTYPE                                                  
         CLC   SPUPFBK,=AL2(OCT_19)     SHARE BOOK EFF SEP19 FOR TEST           
         BL    CHKLEXPX                                                         
**********************************************************************          
*         NEW BBO IMPACT TRANSPARENCY                                           
* EFFECTIVE JAN21 NEW BBO IMPACT CURRENCY FOR RPD+ MKTS                         
* OTHER MKTS GO LIVE APR21                                                      
* SO IF SHARE BOOK IS < JAN21 DONT BOTHER WITH NEW IMPACT CHECK                 
* JUST CHEKC OLD IMPACT BOOKS AS PRIOR @CHKLEC60                                
*                                                                               
         CLC   SPUPFBK,=AL2(JAN_21)     SHARE BOOK EFF JAN21                    
         BL    CHKLEX60                                                         
*&&DO                                                                           
* TEST USING SEP_20 BOOK AS LIVE BBO BOOK FOR RPD                               
* TEST USING OCT_20 BOOK AS LIVE BBO BOOK FOR OTHER MKTS                        
         CLC   SPUPFBK,=AL2(SEP_20)     TESTING USE OCT_20                      
         BL    CHKLEX60                                                         
*&&                                                                             
* CHK MARKET TABLE IN DEMTABS TO SEE THE EFFECTIVE BBO BOOK FOR MKT             
         GOTO1 VDEMTABS,DMCB,NSIMKTS                                            
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
         USING NMKTD,RE                                                         
CHKLEX20 CLI   0(RE),X'FF'         END OF TABLE - SOMETHING IS WRONG            
         BE    CHKLEX60            PROCEED                                      
         CLC   MARKET,NMKTNUM      SAME MARKET                                  
         BE    CHKLEX22                                                         
         AR    RE,R0                                                            
         B     CHKLEX20                                                         
*                                                                               
* FOUND MARKET                                                                  
CHKLEX22 DS    0H                                                               
*                                                                               
* SHARE BOOK IS ON OR AFTER LIVE BOOK - CHECK HUT BOOKS                         
* TO SEE IF WE WANT TO USE IMPACT BBO BOOKTYPES                                 
* ONLY USE PARALLEL BBO BOOTKYPES IF HUT BOOKS IS BETWEEN JAN20 AND             
* PRIOR TO LIVE BBO BOOK                                                        
* THE 137 DIARY MARKETS (RPD) GOES LIVE JAN21 BOOK                              
*                                                                               
* FOR DIARY (RPD) MKTS IF SHARE BOOK >=JAN21                                    
* AND ANY PUT BOOKS IS BETWEEN FEB20 AND NOV20 THEN USE IMPACT                  
* HUTS                                                                          
         CLI   NMKTTYPE,NMKTDQ                                                  
         BNE   CHKLEX40                                                         
*                                                                               
         CLC   SPUPFLD1,=AL2(FEB_20)                                            
         BL    CHKLEX26                                                         
         CLC   SPUPFLD1,=AL2(NOV_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX26 OC    SPUPFLD2,SPUPFLD2                                                
         BZ    CHKLEX28                                                         
         CLC   SPUPFLD2,=AL2(FEB_20)                                            
         BL    CHKLEX28                                                         
         CLC   SPUPFLD2,=AL2(NOV_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX28 OC    SPUPFLD3,SPUPFLD3                                                
         BZ    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(FEB_20)                                            
         BL    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(NOV_20)                                            
         BNH   CHKLEXPY                                                         
         B     CHKLEXPX                                                         
*                                                                               
*                                                                               
*&&DO                                                                           
*TEST USING FEB_20 TO AUG_20 AS PARALLEL BBO BOOKS FOR RDP                      
         CLC   SPUPFLD1,=AL2(FEB_20)                                            
         BL    CHKLEX26                                                         
         CLC   SPUPFLD1,=AL2(AUG_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX26 OC    SPUPFLD2,SPUPFLD2                                                
         BZ    CHKLEX28                                                         
         CLC   SPUPFLD2,=AL2(FEB_20)                                            
         BL    CHKLEX28                                                         
         CLC   SPUPFLD2,=AL2(AUG_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX28 OC    SPUPFLD3,SPUPFLD3                                                
         BZ    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(FEB_20)                                            
         BL    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(AUG_20)                                            
         BNH   CHKLEXPY                                                         
         B     CHKLEXPX                                                         
*&&                                                                             
* NON RPD MKTS                                                                  
CHKLEX40 DS    0H                                                               
**       CLC   SPUPFBK,=AL2(APR_21)   NON RPD MKTS GO LIVE BBM APR21            
**       BL    CHKLEX60               SO ONLY PROCESS IF SHRBK>=APR21           
* TEST USING OCT20 AS LIVE BOOK FOR LPM/SMM MKTS                                
         CLC   SPUPFBK,=AL2(OCT_20)   USE OCT20 AS LIVE BBO BOOK FOR            
         BL    CHKLEX60               TESTING ONLY                              
*                                                                               
         CLC   SPUPFLD1,=AL2(MAY_20)                                            
         BL    CHKLEX46                                                         
         CLC   SPUPFLD1,=AL2(MAR_21)                                            
         BNH   CHKLEXPY                                                         
CHKLEX46 OC    SPUPFLD2,SPUPFLD2                                                
         BZ    CHKLEX48                                                         
         CLC   SPUPFLD2,=AL2(MAY_20)                                            
         BL    CHKLEX48                                                         
         CLC   SPUPFLD2,=AL2(MAR_21)                                            
         BNH   CHKLEXPY                                                         
CHKLEX48 OC    SPUPFLD3,SPUPFLD3                                                
         BZ    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(MAY_20)                                            
         BL    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(MAR_21)                                            
         BNH   CHKLEXPY                                                         
CHKLEX50 B     CHKLEXPX                                                         
*&&DO                                                                           
* TEST USING MAY20 TO SEP20 AS PARALLEL BBO BOOKS FOR LPM/SMM                   
         CLC   SPUPFLD1,=AL2(MAY_20)                                            
         BL    CHKLEX46                                                         
         CLC   SPUPFLD1,=AL2(SEP_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX46 OC    SPUPFLD2,SPUPFLD2                                                
         BZ    CHKLEX48                                                         
         CLC   SPUPFLD2,=AL2(MAY_20)                                            
         BL    CHKLEX48                                                         
         CLC   SPUPFLD2,=AL2(SEP_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX48 OC    SPUPFLD3,SPUPFLD3                                                
         BZ    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(MAY_20)                                            
         BL    CHKLEX50                                                         
         CLC   SPUPFLD3,=AL2(SEP_20)                                            
         BNH   CHKLEXPY                                                         
CHKLEX50 B     CHKLEXPX                                                         
*&&                                                                             
                                                                                
*                                                                               
         DROP  RE                                                               
*                                                                               
**********************************************************************          
*  IF ANY OF THE PUT BOOKS BEFORE LIVE IMPACT                                   
* GO THROUGH IMPACT BOOKTYPE CHECK                                              
* FOR IMPACT HUTS PRIOR TO OCT19                                                
*                                                                               
CHKLEX60 CLC   SPUPFLD1,=AL2(OCT_19)                                            
         BL    CHKLEXPY                                                         
         OC    SPUPFLD2,SPUPFLD2                                                
         BZ    *+14                                                             
         CLC   SPUPFLD2,=AL2(OCT_19)                                            
         BL    CHKLEXPY                                                         
         OC    SPUPFLD3,SPUPFLD3                                                
         BZ    *+14                                                             
         CLC   SPUPFLD3,=AL2(OCT_19)                                            
         BL    CHKLEXPY                                                         
*                                                                               
         B     CHKLEXPX                                                         
*                                                                               
* IF ANY OF THE BOOKS USED IS JUL06 OR LATER THEN IF THE CURRENT BOOK           
* BEING LOOKED UP IS PRIOR TP JUL06 THEN SET THE BOOKTYPE TI SEARCH FOR         
* PARARELL EXPANSION BOOKTYPE                                                   
*                                                                               
CHKLEXPY CLC   DBSELBK,=AL2(JAN_16)                                             
**       CLC   DBSELBK,=AL2(JAN_16)                                             
**       BL    CHKLEXPX                                                         
         MVI   NEXPFLAG,C'Y'                                                    
**       OC    DBSELBK,DBSELBK             LATEST BOOK                          
**       BZ    CHKLEXPX                    ALSO DONT PROCESS EXPANSION          
*                                                                               
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YL                                              
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YS                                              
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Y3                                              
         CLI   DBBTYPE,0                                                        
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Y7                                              
*                                                                               
         CLI   DBBTYPE,C'U'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YU                                              
         CLI   DBBTYPE,BOOKTYPE_C3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YD                                              
         CLI   DBBTYPE,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YC                                              
*                                                                               
         CLI   DBBTYPE,C'Z'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YZ                                              
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YA                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YB                                              
         CLI   DBBTYPE,C'W'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YW                                              
*                                                                               
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YJ                                              
         CLI   DBBTYPE,BOOKTYPE_HS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YE                                              
         CLI   DBBTYPE,BOOKTYPE_H3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YF                                              
         CLI   DBBTYPE,C'H'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YH                                              
* FORCE NIELSEN                                                                 
         CLI   DBSELSRC,C'F'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'T'                                                    
         BNE   *+18                                                             
         MVI   DBSELSRC,C'N'                                                    
         MVI   SPUPSRC,C'N'                                                     
         MVC   DBSELSTA,SPUPSTA  RESET STATION TO CALL LETTERS FOR NSI          
CHKLEXPX XIT1                                                                   
         LTORG                                                                  
*                                                                               
*******************************************************************             
* ROUTINE TO CHECK IF ANY OF THE UPGRADE BOOKS IS LIVE ZERO CELL                
* JUL06 OR LATER                                                                
* IF ONE OF THE  BOOKS IS LIVE ZERO CELL THEN ALL THE OTHER BOOKS               
* PRIOR TO JUL06 WILL READ ZERO CELL BOOKTYPE (1-4)                             
********************************************************************            
CHKLIVEZ NTR1  BASE=*,LABEL=*                                                   
* LOOK AT ALL THE BOOKS PARAMETER                                               
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
         CLI   MBKIFLAG,C'Y'                                                    
         BE    CHKLIVEX                                                         
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLIVEX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKLIVEX                                                         
         CLC   SPUPFBK,=X'6A07'    ARE ANY BOOKS JUL06 OR LATER?                
         BNL   CHKLIVEY                                                         
*                                                                               
         CLC   SPUPFLD1,=X'6A07'   IF A VALID BOOK GREATER THAN JUL06           
         BL    *+14                                                             
         CLC   SPUPFLD1,=X'9999'                                                
         BL    CHKLIVEY                                                         
*                                                                               
         CLC   SPUPFLD2,=X'6A07'                                                
         BL    *+14                                                             
         CLC   SPUPFLD2,=X'9999'                                                
         BL    CHKLIVEY                                                         
*                                                                               
         CLC   SPUPFLD3,=X'6A07'                                                
         BL    *+14                                                             
         CLC   SPUPFLD3,=X'9999'                                                
         BL    CHKLIVEY                                                         
         B     CHKLIVEX                                                         
*                                                                               
* IF ANY OF THE BOOKS USED IS JUL06 OR LATER THEN IF THE CURRENT BOOK           
* BEING LOOKED UP IS PRIOR TP JUL06 THEN SET THE BOOKTYPE TI SEARCH FOR         
* PARARELL ZEROCELL BOOKTYPE                                                    
*                                                                               
CHKLIVEY CLC   DBSELBK,=X'6A07'                                                 
         BNL   CHKLIVEX                                                         
         OC    DBSELBK,DBSELBK             LATEST BOOK                          
         BZ    CHKLIVEX                    ALSO DONT PROCESS ZEROCELL           
         CLI   DBBTYPE,C'W'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'4'                                                     
         CLI   DBBTYPE,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'3'                                                     
         CLI   DBBTYPE,C'H'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'2'                                                     
         CLI   DBBTYPE,0                                                        
         BNE   *+8                                                              
         MVI   DBBTYPE,C'1'                                                     
*                                                                               
CHKLIVEX XIT1                                                                   
         LTORG                                                                  
*                                                                               
*******************************************************************             
* ROUTINE TO CHECK IF WE SHOULD USE THE NEW HUT/PUTS                            
* IF ONE OF THE PROJECTED BOOKS LATER THEN OR EQUAL TO JAN/13                   
* AND THE CURRENT BOOKS FALL BETWEEN JAN/13 AND DEC/13                          
* THEN USE NEW HUT BOOKYTYPE                                                    
********************************************************************            
CHKNHUT  NTR1  BASE=*,LABEL=*                                                   
* CALL DEDEMTABS TO GET LPM START-END TABLE                                     
*                                                                               
         GOTO1 VDEMTABS,DMCB,LPMUPG  GET A(LPM DUAL DATA DATES)                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
*  SEE IF THIS IS AN LPM MARKET                                                 
         USING LPMUDATD,RE                                                      
CHKNHUTC CLI   0(RE),X'FF'         EOT?                                         
         BE    CHKNHUTX                                                         
         CLC   DBSELRMK,LPMUNMKT   SAME MARKET                                  
         BNE   CHKNHUTE                                                         
         CLC   DBSELBK,LPMUDATE    CHECK IF VALID FOR THIS BOOK                 
         BNL   CHKNHUTJ                                                         
*                                                                               
CHKNHUTE AR    RE,R0                                                            
         B     CHKNHUTC                                                         
         DROP  RE                                                               
*                                                                               
* LOOK AT ALL THE BOOKS PARAMETER                                               
CHKNHUTJ MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
         MVI   NHUTFLAG,C'N'                                                    
******** CLI   MBKIFLAG,C'Y'                                                    
******** BE    CHKNHUTX                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKNHUTX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKNHUTX                                                         
         CLC   SPUPFBK,=AL2(JAN_14)  ARE ANY BOOKS JAN14 OR LATER?              
         BNL   CHKNHUTY                                                         
*                                                                               
         CLC   SPUPFLD1,=AL2(JAN_14) IF A VALID BOOK GREATER THAN JAN14         
         BL    *+14                                                             
         CLC   SPUPFLD1,=X'9999'                                                
         BL    CHKNHUTY                                                         
*                                                                               
         CLC   SPUPFLD2,=AL2(JAN_14)                                            
         BL    *+14                                                             
         CLC   SPUPFLD2,=X'9999'                                                
         BL    CHKNHUTY                                                         
*                                                                               
         CLC   SPUPFLD3,=AL2(JAN_14)                                            
         BL    *+14                                                             
         CLC   SPUPFLD3,=X'9999'                                                
         BL    CHKNHUTY                                                         
         B     CHKNHUTX                                                         
*                                                                               
* IF ANY OF THE BOOKS USED IS BETWEEN JAN13-DEC13                               
* CHECK TO CONVERT TO THE NEW HUT BOOKTYPE                                      
*                                                                               
CHKNHUTY MVI   NHUTFLAG,C'Y'       SET NHUT FLAG TO YES                         
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         CLC   DBSELBK,=AL2(JAN_13)                                             
         BL    CHKNHUTX                                                         
         CLC   DBSELBK,=AL2(DEC_13)                                             
         BH    CHKNHUTX                                                         
         CLI   DBBTYPE,BOOKTYPE_L                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZL                                              
         CLI   DBBTYPE,BOOKTYPE_U                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZU                                              
         CLI   DBBTYPE,BOOKTYPE_Z                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZZ                                              
         CLI   DBBTYPE,BOOKTYPE_J                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZJ                                              
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZS                                              
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZA                                              
         CLI   DBBTYPE,BOOKTYPE_HS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZE                                              
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Z3                                              
         CLI   DBBTYPE,BOOKTYPE_C3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZD                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZB                                              
         CLI   DBBTYPE,BOOKTYPE_H3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZF                                              
         CLI   DBBTYPE,BOOKTYPE_C                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZC                                              
         CLI   DBBTYPE,BOOKTYPE_W                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZW                                              
         CLI   DBBTYPE,BOOKTYPE_H                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZH                                              
         CLI   DBBTYPE,BOOKTYPE_STANDARD                                        
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Z7                                              
*                                                                               
*  OLYMPIC BOOKS                                                                
         CLI   DBBTYPE,BOOKTYPE_O                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Z7                                              
         CLI   DBBTYPE,BOOKTYPE_OS                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZS                                              
         CLI   DBBTYPE,BOOKTYPE_O3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_Z3                                              
         CLI   DBBTYPE,BOOKTYPE_OL                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_ZL                                              
*                                                                               
CHKNHUTX XIT1                                                                   
         LTORG                                                                  
*                                                                               
NONZEROC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DBSELDAY,SVSELDAY                                                
         MVC   DBSELTIM,SVSELTIM                                                
* CHECK IF WE ARE ASKING FOR ZERO CELL DATA                                     
*                                                                               
*  CHECK IF ANY OF THE BOOKS TO BE USED IN THE UPGRADE FORMULA                  
*  IS JUL06 OR LATER ..IF SO MAKE SURE WE ONLY SET THE BOOKTYPES                
*  TO 1-4 FOR THE PARALLEL PERIOD BOOKS                                         
*                                                                               
*                                                                               
         MVI   ZCELLBTY,X'FF'         SET DEFAULT VALUE                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   NOZEROX                                                          
         CLI   DBSELMED,C'T'                                                    
         BNE   NOZEROX                                                          
*                                                                               
         CLI   DBBTYPE,C'1'           X'FF'= NOT ZEROCELL BKTYPE                
         BNE   NOZERO2                                                          
         MVC   ZCELLBTY,DBBTYPE                                                 
         MVI   DBBTYPE,0                                                        
         B     NOZEROX                                                          
                                                                                
NOZERO2  CLI   DBBTYPE,C'2'                                                     
         BNE   NOZERO3                                                          
         MVC   ZCELLBTY,DBBTYPE                                                 
         MVI   DBBTYPE,C'H'                                                     
         B     NOZEROX                                                          
NOZERO3  CLI   DBBTYPE,C'3'                                                     
         BNE   NOZERO4                                                          
         MVC   ZCELLBTY,DBBTYPE                                                 
         MVI   DBBTYPE,C'C'                                                     
         B     NOZEROX                                                          
NOZERO4  CLI   DBBTYPE,C'4'                                                     
         BNE   NOZERO5                                                          
         MVC   ZCELLBTY,DBBTYPE                                                 
         MVI   DBBTYPE,C'W'                                                     
         B     NOZEROX                                                          
NOZERO5  DS    0X                                                               
*                                                                               
*                                                                               
*                                                                               
NOZEROX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
WORKD    DSECT                     ** SPDEMUP GLOBAL W/S **                     
ATPTVALS DS    F                                                                
APAVVALS DS    F                                                                
AOFORMAT DS    F                                                                
VSUBR01  DS    F                                                                
VSUBR02  DS    F                                                                
RELO2    DS    F                                                                
RELO3    DS    F                                                                
MYBASE2  DS    F                                                                
PARM01   DS    F                   A(PARM LIST) COMING INTO SUBR01              
PARM02   DS    F                   A(PARM LIST) COMING INTO SUBR02              
SAVER7   DS    F                                                                
SAVEMORE DS    D                                                                
Y4ASW    DS    C                   INDICATE AVERAGES                            
Y4MSW    DS    C                   INDICATE MULTIPLY INDEX(GEOMETRIC)           
Y4SSW    DS    C                   INDICATE PINDEX/SINDEX                       
Y4SYRS   DS    C                   NUMBER OF IDX/AVG BOOKS                      
Y4SYRS2  DS    C                   NUMBER OF IDX/AVG BOOKS                      
SVUPFLD  DS    6C                  SAVE UPGRADE FIELDS                          
LPMSBOOK DS    CL2                 SHARE INDEX BOOK FOR LPM UPGRADE             
LPMPBOOK DS    CL2                 PUT INDEX BOOK FOR LPM UPGRADE               
LPMSRMKT DS    XL2                 SHARE INDEX HOME MARKET                      
LPMPRMKT DS    XL2                 PUT INDEX HOME MARKET                        
BOOKI    DS    C                   BOOK INDEX                                   
INDEXUPG DS    C                   INDEXED UPGRADE                              
BOOKLIST DS    CL10                4 BOOKS + 00 END                             
STNBK    DS    XL2                 BOOK PARAM PASSED                            
DUB      DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
PROF1W   DS    CL16                                                             
DMCB     DS    6F                                                               
VCOMFACS DS    V                                                                
VCALLOV  DS    V                                                                
VDEMAND  DS    V                                                                
VDEMOMTH DS    V                                                                
VDEMOUT  DS    V                                                                
VDEMAINT DS    V                                                                
VHELLO   DS    V                                                                
VDEFINE  DS    V                                                                
VGETIUN  DS    V                                                                
VDEINMKT DS    V                                                                
VDATCON  DS    V                                                                
VSTAPACK DS    V                                                                
VDEMTABS DS    V                                                                
AIO2     DS    A                                                                
AIO1     DS    A                                                                
RELO     DS    A                                                                
APARM    DS    A                   A(USER PARAMETER LIST)                       
ADISPDTA DS    A                   A(IUN MASTER DISP. TABLE)                    
ADEMLST  DS    A                   A(INPUT DEMO LIST)                           
ADEMOUT  DS    A                   A(OUTPUT DEMO AREA)                          
AFRSTEL  DS    A                   A(FIRST ELEMENT)                             
SAVERD   DS    A                   SAVED RD VALUE FOR ERROR EXIT                
SAVERE   DS    A                   SAVED RE VALUE IN DEMAND HOOK                
INDEX    DS    F                   CALCULATION INDEX FACTOR                     
SVDIVSOR DS    H                   SAVED DBDIVSOR VALUE                         
DBLOCK1  DS    264X                DBLOCK FOR DEMO LOOK-UPS                     
DBLOCK2  DS    264X                DBLOCK FOR UPGRADES                          
FILEVALS DS    0CL4                INPUT FILE VALUES                            
FILETYPE DS    C                   INTERNAL FILE (I=INV,P=PAV)                  
FILEDEM  DS    CL3                 DBLOCK FILE NAME                             
UPLPMDAT DS    CL3                 EXPANDED LPM DATE                            
ALTBTYP  DS    C                   ALTERNATE LPM BOOK TYPE                      
UPLPMSD  DS    XL2                 LPM PARALELL START FOR MARKET                
UPLPMED  DS    XL2                 LPM PARALELL END FOR MARKET                  
LPMHSP   DS    C                   LPM HISPANIC SWTICH                          
MMUPASS  DS    X                   METER MARKET INDEX PASS                      
MMUBOOK  DS    XL2                 METER MARKET INDEX BOOK                      
SBKCTRL  DS    C                   SHARE BOOK CONTROL                           
PUTUPGD  DS    C                   Y=THIS IS A PUT UPGRADE                      
HPT      DS    C                   O=GET OLD H/P/T, N=GET NEW H/P/T             
PERSHR   DS    C                   Y=COMPUTE SHARES FROM PERIOD R/P             
PUREFLAG DS    C                   Y=PURE PAV NUMBER LOOK-UP                    
PUREDUR  DS    X                   DURATION OF PURE PROGRAM                     
PUREDW   DS    X                   PROGRAM DAY/WEEK                             
PURESTIM DS    X                   PROGRAM START QTR HOUR                       
TAPEOPT  DS    C                   CALCULATE BASED ON TAPE PRECISION            
PRECUPG  DS    C                   BASED ON DECIMAL PREC. UPGRADE VALS          
NORMHPT  DS    C                   NOMALIZE HPT TAPE PRECISION                  
LOCALMED DS    C                   SO I CAN DO CSI UPGRADES                     
STNAFFL  DS    A                   A(AFFIL) LIST FOR IN MKT SHARES              
IMSFLAG  DS    C                   IMS FLAG (A/S) A=AFFIL S=STATION             
SVSELDAY DS    CL(L'DBSELDAY)                                                   
SVSELTIM DS    CL(L'DBSELTIM)                                                   
ZCELLBTY DS    C                   ZERO CELL BOOKTYPE                           
ZSVBKTYP DS    C                   NON ZERO CELL BOOKTYPE                       
SVBTYPE  DS    C                   SAVE THE BOOK TYPE                           
SVDBACBK DS    XL(L'DBACTBK)       SAVE AREA FOR DBACTBK                        
OVERLIST DS    XL31                LIST OF DEMO OVERRIDE VALUES                 
MARKET   DS    XL2                 RATING SERVICE MARKET NUMBER                 
SVMARKET DS    XL2                 RATING SERVICE MARKET NUMBER                 
STPROG   DS    CL16                START QHR PROGRAM NAME                       
NDPROG   DS    CL16                END QHR PROGRAM NAME                         
OVERDEM  DS    3XL3,X              USER SHARE DEMO LIST                         
OVERDEMS DS    0F                  USER SHARE DEMO VALUES                       
OVERSHR  DS    F                                                                
OVERRTG  DS    F                                                                
OVERIMP  DS    F                                                                
HOMEVUTS DS    3F                    HOME VUTS (SHR/RTG)                        
         DS    0F                                                               
TOTHMSHR DS    XL(HOMSHRLN)        COMPOSITE AREA FOR HOME SHARES               
DEMOLST  DS    XL20                                                             
OLDSHR   DS    (NUMVALS*2)F        ORIGINAL SHARES                              
MBEXTEND DS    CL(DBXMBKS-DBXMBD+L'SPUPFBKL+4) EXTENSION FOR MULT BOOKS         
SPEXTEND DS    CL(DBXTTIDX-DBXTTID)            EXTENSION FOR SPOT DATA          
MSEXTEND DS    CL(DBXMSTA-DBXMSD+50)   MULT STNS EXTND - 10 STN MAX             
MBKIFLAG DS    C                    MBK DMA=I FLAG                              
TWNFLAG  DS    C                                                                
NHUTFLAG DS    C                                                                
NEXPFLAG DS    C                                                                
MBKNHPT  DS    (NUMVALS*2)D          EXPANDED NEWHPT/NEWTOT FOR MBK             
MBKOHPT  DS    (NUMVALS*2)D          EXPANDED OLDHPT/OLDTOT FOR MBK             
MBKHVUTS DS    3D                    MBK HOME VUTS (SHR/RTG)                    
MBKHMSHR DS    3D                    NEED BIGGER AREA FOR MBK IMP BASE          
GDMKCDEM DS    C          CABLE DEMO RTF SERVICE OVERRIDE                       
GDMKALPH DS    CL3        MARKET ALPHA                                          
LPMDTB   DS    XL3                 LPM 3-BYTE START DATE                        
LPMDTP   DS    XL2                 LPM 2-BYTE START DATE                        
SVSYSC   DS    XL2                 SAVE SYSCODE                                 
LBEXTEND DS    CL(DBLATUBX-DBLATUBK)  LATEST UPGRADE BOOK EXTENSION             
*                                                                               
DMAL7EBK DS    XL2                                                              
DEXTRA1  DS    0CL64                                                            
         DS    CL4                   EYECATCHER  'SPOT'                         
         DS    AL4                   NEXT LINK                                  
         DS    CL56                                                             
*                                                                               
SVFACTOR DS    CL(L'DBFACTOR)                                                   
SV00APRF DS    CL16                                                             
YRSAVTYP DS    C                     TYPE OF YR AVG                             
HOOKFLAG DS    C                                                                
EXTBUFF  DS    C                     Y/N USE EXTENED BUFFER?                    
SVRF     DS    A                                                                
SVRF2    DS    A                                                                
SVRF3    DS    A                                                                
SVTOTHOM DS    A                                                                
LATINDBK DS    XL2                   LATEST INDEX BOOK                          
QTRBKTAB DS    XL18                                                             
*                                                                               
         EJECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
         ORG   OLDUNV+(DISPHOM*4)                                               
UOUHOMES DS    F                                                      *         
         ORG                                                                    
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
OLDRTGLN EQU   OLDRTGX-OLDRTG                                                   
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
OLDHPTLN EQU   OLDHPTX-OLDHPT                                                   
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR SPGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMSHR   DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
*                                                                               
UPRECX   DS    0F                                                               
*                                                                               
UPUIDX   DS    (NUMVALS)F          INDEX FOR DMA IMPS                           
IOAREA1  DS    2000C               ALLLOW FOR 2000 BYTE NETWORK RECDS           
IOAREA2  DS    (UPRECX-UPREC)C                                                  
IOAREA2X DS    0C                                                               
*                                                                               
Y1SHR    DS    (NUMVALS*2)F        YEAR 1 SHARES FOR INDEXING                   
Y2SHR    DS    (NUMVALS*2)F        YEAR 2 SHARES FOR INDEXING                   
Y3SHR    DS    (NUMVALS*2)F        YEAR 3 SHARES FOR INDEXING                   
Y4SHR    DS    (NUMVALS*2)F        YEAR 4 SHARES FOR INDEXING                   
                                                                                
MBKIUN   DS    0C                   ACUMULATE AREAS                             
MBKUNV   DS    (NUMVALS)D                                                       
MBKRTG   DS    (NUMVALS)D                                                       
MBKIMP   DS    (NUMVALS)D                                                       
MBKHPT   DS    (NUMVALS)D                                                       
MBKTOT   DS    (NUMVALS)D                                                       
MBKIUNL  EQU   *-MBKIUN                                                         
WORKX    EQU   *                                                                
         EJECT                                                                  
GETHPTWK DSECT                     ** GETHPT S/R LOCAL W/S **                   
GETHIND  DS    X                   INDICATOR BYTE                               
GETHBK   DS    XL2                 SAVED DBACTBK                                
GETHDIV  DS    XL2                 SAVED DBDIVSOR                               
*                                                                               
**GETSTNQ  EQU   50                  MAX STNS TABLES CAN HANDLE                 
GETSTNQ  EQU   200                 MAX STNS TABLES CAN HANDLE                   
GETSTLST DS    XL((5*GETSTNQ)+10)                                               
GETSTNBK DS    XL((7*GETSTNQ)+10)  STN/BK FOUND IN STNHK                        
GETSTCNT DS    XL1                 TOT NUMBER STATIONS FOUND                    
*                                                                               
GETHPTUN DS    (NUMVALS*1)F        UNVS                                         
GETHPTRT DS    (NUMVALS*2)F        RTGS/IMPS                                    
GETHPTS  DS    (NUMVALS*2)F        PUTS/TOTS                                    
GETHPTLN EQU   *-GETHPTWK                                                       
         SPACE 1                                                                
*                                                                               
       ++INCLUDE SPSTAPACKD                                                     
STARECD DSECT                                                                   
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*                                                                               
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117SPDEMUP   02/26/21'                                      
         END                                                                    
