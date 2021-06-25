*          DATA SET SPOTBOOKS  AT LEVEL 045 AS OF 05/01/02                      
*PHASE T00A49A,*                                                                
*INCLUDE GETBROAD                                                               
         SPACE 1                                                                
*================================================================*              
* 19JUN98  MHER IF NO JAN OR MAR BOOK, USE BROADCST MONTH DATES  *              
*               TO DECIDE TO USE OLYMPIC EXCLUSION OPTION        *              
*               FOR PURCHASED RERATES                            *              
* 26JUN98  MHER IF THERE IS A MAR BOOK, USE REGULAR FEB BOOK     *              
*               FOR ALL SPOTS IN BROADCAST MONTH OF FEB          *              
*               TO MAKE DATA AGREE WITH OLD SPOT REPORTS         *              
* 27AUG98  BZEH REMOVE CLEVELAND FROM JAN/98 LIST                *              
* 06OCT98  AATK BUILD WEEK LIST OBSERVING SPLITS ON SWEEP DATES  *              
* 30OCT98  EJOR KILL AATK (BECAUSE IT WOULD BE FUN)              *              
* 02NOV98  EJOR/BZEH MORE UNREAL BS                              *              
* 11NOV98  EJOR TRY TO MAKE ACT WORK FOR RADIO                   *              
* 02FEB00  EJOR DON'T DIE ON FUNNY DATES                         *              
*================================================================*              
         SPACE 2                                                                
************************************************************                    
*                                                          *                    
* SPOTBOOK - T00A49                                        *                    
*                                                          *                    
* BUILD A TABLE OF DEMO LOOKUP BOOKS FOR A SET OF REQUEST  *                    
* DATES POINTED TO BY SBADATE.  OUTPUT ENTRIES ARE 6 BYTES *                    
* START DATE(COMPRESSED)/END DATE(COMPRESSED)/BOOK(Y/M)    *                    
*                                                          *                    
* ON ENTRY, P1=A(SPOT BLOCK)                               *                    
* ON EXIT,  SBABKLST=A(BOOK LOOKUP TABLE)                  *                    
*           SBNBKS=N'BOOK LOOKUP TABLE ENTRIES             *                    
*                 =0 IF ERROR                              *                    
*           SBBKOPT CONTAINS OPTIMIZATION AREA             *                    
*                                                          *                    
* IF OLYMPIC EXCLUSION BOOK APPLIES TO FEB/98 BOOK,        *                    
*  BOOK MONTH WILL BE SET TO X'82'  EJOR 11JUN98           *                    
************************************************************                    
         TITLE 'T00A49 - SPOTBOOK - GET BOOK FOR DEMO LOOKUP'                   
SPOTBOOK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPBK**,RA,RR=RE,CLEAR=YES                          
         USING WORKD,RC                                                         
         ST    R1,APARM                                                         
         MVC   PARMS(PARML),0(R1)                                               
         L     R8,0(R1)                                                         
         USING SBLOCKD,R8                                                       
         L     R9,SBCOMFAC                                                      
         USING COMFACSD,R9                                                      
         ST    RE,RELO                                                          
*                                                                               
BOOK1    GOTO1 CCALLOV,DMCB,0,X'D9000A72'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VMOBILE,0(R1)                                                    
         L     RE,=V(GETBROAD)                                                  
         A     RE,RELO                                                          
         ST    RE,VGETBRD                                                       
         MVC   VADDAY,CADDAY       BUILD ADCON LIST FOR MOBILE                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VDATCON,CDATCON                                                  
*                                                                               
         MVC   OPTCLT,SBBCLT       EXTRACT SOME OPTIMIZATION VALUES             
         MVC   OPTMKT,SBEMKT       SET MARKET AND STATION                       
         MVC   OPTSTA,SBSTA                                                     
         MVC   OPTQBOOK,SBQBOOK                                                 
         MVC   OPTBKTY,SBBKTYPE    SET BOOK TYPE                                
         MVC   OPTDAYS,SBDAYS      DAYS ROTATION (IF ANY)                       
         OC    SBEMKT,SBEMKT       TEST FOR REQUESTED MARKET                    
         BZ    BOOK2                                                            
         CLC   SBEMKT,SBBMKT       AND FOR SPILL LOOKUP                         
         BE    BOOK2                                                            
         MVC   OPTSPMKT,SBBSPL     YES-SET SPILL MKT RTG SRV NUMBER             
*                                                                               
BOOK2    LM    R2,R3,SBADATE       R2=A(REQUEST DATE LIST),R3=N'ENTRIES         
         MVC   OPTSTART,0(R2)                                                   
         GOTO1 CDATCON,DMCB,(2,0(R2)),STDATE                                    
         BCTR  R3,0                                                             
         SLL   R3,2                INDEX TO LAST ENTRY                          
         LA    R2,0(R3,R2)                                                      
         MVC   OPTEND,2(R2)                                                     
         GOTO1 (RF),(R1),(2,2(R2)),ENDATE                                       
*                                                                               
BOOK4    CLC   SBBKOPT,OPTVALS     TEST CHANGE IN OPTIMIZATION CONTROLS         
         BE    BOOKX               NO-ALL DONE                                  
         MVC   SBBKOPT,OPTVALS     UPDATE OPTIMIZATION CONTROLS                 
         MVC   FILE(3),=C'TTN'     SET FILE/MEDIA/SOURCE                        
*                                                                               
         CLI   SBMED,C'R'          FIX FOR RADIO RE-RATE                        
         BNE   *+8                                                              
         MVI   MEDIA,C'R'                                                       
*                                                                               
         CLI   SBCPROF+3,C'0'      TEST FOR NSI                                 
         BE    *+8                                                              
         MVI   SOURCE,C'A'                                                      
         OC    SBAXSPIL,SBAXSPIL   TEST CANADIAN SPILL LOOKUP                   
         BNZ   BOOK5                                                            
         LA    RE,SBAGYREC                                                      
         USING AGYHDR,RE                                                        
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   BOOK6               NO                                           
         CLI   SBCEXTRA+0,C'U'     TEST US DEMO LOOKUP FOR CLIENT               
         BE    BOOK6                                                            
*                                                                               
BOOK5    MVI   MEDIA,C'C'          CANADIAN LOOKUP                              
*                                                                               
BOOK6    CLC   SBQBOOK(3),=C'ACT'  TEST ACTUAL BOOK LOOKUP                      
         BNE   REG                 NO-DEAL WITH REGULAR BOOK NOW                
         XC    SBNBKS,SBNBKS                                                    
         BAS   RE,TORMTH           ANY WEEKLY BOOKS                             
         OC    SBNBKS,SBNBKS       YES - SETUP IS DONE                          
         BNZ   EXIT                                                             
*                                                                               
         MVC   PROFILE,SBSPPROF+3                                               
         CLI   PROFILE,C'0'                                                     
         BL    *+8                                                              
         NI    PROFILE,X'0F'                                                    
         LA    RE,SBMKTREC                                                      
         USING MKTREC,RE                                                        
         MVC   CLASS,MKTCLAS1      GET SWEEP CLASS                              
         CLC   SBCPROF+3(1),MKTRS1 TEST SAME SERVICE AS CLIENT                  
         BE    *+10                                                             
         MVC   CLASS,MKTCLAS2      NO-USE OTHER SWEEP CLASS                     
         OI    CLASS,X'F0'         FORCE SWEEP CLASS TO NUMERIC                 
         XC    DAYNUM,DAYNUM                                                    
         SR    R1,R1                                                            
         ICM   R1,8,SBDAYS         TEST DAYS ROTATION SET                       
         BZ    ACT                                                              
         SR    R0,R0               YES-SET THE ROTATION START DAY NUM           
         SR    RE,RE               0=MON,1=TUES,ETC.                            
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-14                                                             
         BCTR  RE,0                                                             
         ST    RE,DAYNUM                                                        
         B     ACT                                                              
*                                                                               
BOOKX    DS    0H                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* ASSIGN BOOK BASED ON SBQBOOK                                                  
*                                                                               
REG      LA    R0,MAXWEEKS         WEEK LIST LIMIT                              
         GOTO1 VMOBILE,DMCB,((R0),STDATE),(5,IOAREA),ADCONS,SBSPPROF            
         ZIC   R0,0(R1)            N'PAIRS GENERATED                            
*                                                                               
**Y2K**  PACK  DUB,SBQBOOK(2)                                                   
**Y2K**  CVB   RF,DUB                                                           
**Y2K**  STC   RF,LOOKBOOK                                                      
**Y2K**  PACK  DUB,SBQBOOK+2(2)                                                 
**Y2K**  CVB   RF,DUB                                                           
**Y2K**  STC   RF,LOOKBOOK+1                                                    
         SPACE 2                                                                
         MVC   WORK(4),SBQBOOK                                                  
         MVC   WORK+4(2),=C'01'    DUMMY DAY                                    
         GOTO1 VDATCON,DMCB,WORK,(3,FULL)                                       
         MVC   LOOKBOOK,FULL       YM BINARY                                    
         SPACE 2                                                                
*                                                                               
         BAS   RE,VALSTA           VALIDATE STATION/BOOK                        
         BE    *+10                                                             
         XC    LOOKBOOK,LOOKBOOK   BOOK IS NOT ON FILE                          
*                                                                               
         L     R4,SBABKLST         R4=A(OUTPUT)                                 
         ST    R0,SBNBKS           RETURN N'ENTRIES                             
         LA    R5,IOAREA           R5=A(MOBILE DATE LIST)                       
*                                                                               
REG2     MVC   0(4,R4),0(R5)                                                    
         MVC   4(2,R4),LOOKBOOK                                                 
         LA    R4,6(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,REG2                                                          
*                                                                               
REGX     B     BOOKX                                                            
         EJECT                                                                  
* ACTUAL BOOK LOOKUP ROUTINE                                                    
*                                                                               
ACT      CLI   SBMED,C'R'                                                       
         BE    ACTRADIO                                                         
*                                                                               
         XC    SBNBKS,SBNBKS       CLEAR N'BOOK LOOKUP ENTRIES                  
         LA    R6,ACTTAB           R6=A(ACTUAL BOOK TABLE ENTRY)                
         USING ACTTABD,R6                                                       
*                                                                               
ACT2     CLI   ACTPROF,X'FF'       TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTPROF,PROFILE     MATCH ON PROFILE                             
         BNE   ACT3                NO                                           
         CLI   ACTCLASS,X'FF'      TEST FOR DEFAULT TABLE                       
         BE    ACT4                YES-ALWAYS TAKE IT                           
         CLC   ACTCLASS,CLASS      MATCH ON MARKET SWEEP CLASS                  
         BE    ACT4                YES                                          
*                                                                               
ACT3     LA    R6,ACTTABL(R6)                                                   
         B     ACT2                                                             
*                                                                               
ACT4     ST    R6,AACTTAB          SAVE A(TABLE ENTRY)                          
         L     R0,=F'-7'           EXTEND REQUEST 1 WEEK FORWARD                
         GOTO1 CADDAY,DMCB,STDATE,STDATE,(R0)                                   
         LCR   R0,R0               AND BACKWARD                                 
         GOTO1 (RF),(R1),ENDATE,ENDATE,(R0)                                     
         BAS   RE,BLDSWP           BUILD SHORT SWEEP TABLE                      
*                                                                               
ACT8     BAS   RE,SPLTWKS          MERGE WKS/SWP PERS TO IOAREA                 
*                                                                               
         LA    R4,WEEKLIST                                                      
         SPACE 1                                                                
*=================================================================*             
* NOW PROCESS THE WEEK LIST USING DEFAULT OR FORCE ACTUAL         *             
* BOOK TABLE                                                                    
*=================================================================*             
         SPACE 1                                                                
ACT14    MVC   THISBOOK,4(R4)                                                   
         MVC   THISWEEK,6(R4)      RE-SET WEEK TO ONE                           
*                                                                               
* TEST CURRENT LOOKUP IS FORCE                                                  
*                                                                               
         CLI   ACTTYPE,ACTFORCE                                                 
         BE    ACT16                                                            
         CLI   LOOKTYPE,ACTFORCE                                                
         BNE   ACT18                                                            
         SPACE 1                                                                
*=========================================================*                     
* FORCE DOES TABLE LOOKUP THEN TESTS IF BOOK ON FILE      *                     
*=========================================================*                     
         SPACE 1                                                                
ACT16    BAS   RE,GETBOOK          GET THE ACTUAL BOOK                          
         MVC   4(2,R4),LOOKBOOK    SET IT INTO LIST                             
         BAS   RE,VALSTA                                                        
         BE    *+10                FOUND FORCED BOOK                            
         XC    4(2,R4),4(R4)       NOT FOUND SO CLEAR BOOK SLOT                 
         B     ACT30                                                            
         SPACE 1                                                                
*=========================================================*                     
* DEFAULT TESTS IF CURRENT BOOK ON FILE                   *                     
* IF NOT, TRY TABLE BOOK                                  *                     
*=========================================================*                     
         SPACE 1                                                                
ACT18    MVC   LOOKBOOK,THISBOOK                                                
         BAS   RE,VALSTA           SEE IF SWEEP IS ON FILE                      
         BE    ACT30               YES                                          
*                                                                               
         GOTO1 CDATCON,DMCB,(2,2(R4)),(3,DUB)                                   
         MVC   THISBOOK,DUB                                                     
         BAS   RE,GETBOOK                                                       
         MVC   4(2,R4),LOOKBOOK    SET THE BOOK IN THE ENTRY                    
*                                                                               
         BAS   RE,VALSTA                                                        
         BE    ACT30                                                            
*                                                                               
         CLI   OLYMPIC,C'Y'        TEST SPECIAL OLYMPIC SITUATION               
         BNE   ACT24                                                            
*                                                                               
         CLI   BOOK,X'62'          98 OLYMPICS                                  
         BNE   ACT20                                                            
         MVC   BOOK,=X'610B'       SET FOR NOV/97                               
         B     ACT22                                                            
*                                                                               
ACT20    CLI   BOOK,X'5C'          AFTER 1992                                   
         BH    *+14                                                             
         MVC   BOOK,=X'5B0B'       NO-TRY NOV/91 BOOK                           
         B     *+10                                                             
         MVC   BOOK,=X'5D0B'       YES-TRY NOV/93 BOOK                          
*                                                                               
ACT22    MVC   4(2,R4),BOOK                                                     
         MVC   LOOKBOOK,BOOK                                                    
         BAS   RE,VALSTA                                                        
         BE    ACT30                                                            
*                                                                               
ACT24    TM    SBQBOOK+3,X'80'     FINALLY TEST ACT/YY                          
         BZ    ACT26                                                            
*                                                                               
         BAS   RE,ALTYEAR                                                       
         MVC   4(2,R4),BOOK                                                     
*                                                                               
         MVC   LOOKBOOK,BOOK                                                    
         BAS   RE,VALSTA                                                        
         BE    ACT30                                                            
*                                                                               
ACT26    XC    4(2,R4),4(R4)       DID NOT FIND BOOK                            
*                                                                               
ACT30    BAS   RE,OLYS96           CHECK OLYMPICS                               
*                                                                               
         LA    R4,7(R4)            NEXT WEEK LIST ENTRY                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   ACT14                                                            
         B     ACTX                                                             
*                                                                               
* FORCE PROCESSING - FIND ACTUAL BOOK AND USE IT                                
*                                                                               
ACT32    MVC   THISBOOK,4(R4)                                                   
         MVC   THISWEEK,6(R4)      SET WEEK TO ONE                              
*                                                                               
         BAS   RE,GETBOOK          GET THE FORCE BOOK                           
         MVC   BOOK,LOOKBOOK                                                    
         MVC   4(2,R4),BOOK        SET IT INTO LIST                             
         BAS   RE,VALSTA                                                        
         BE    ACT38                                                            
         TM    SBQBOOK+3,X'80'     TEST ACT/YY                                  
         BZ    ACT36                                                            
         BAS   RE,ALTYEAR                                                       
         MVC   4(2,R4),BOOK                                                     
         BAS   RE,VALSTA                                                        
         BE    ACT38                                                            
*                                                                               
ACT36    XC    4(2,R4),4(R4)       NOT FOUND SO ZERO BOOK                       
*                                                                               
ACT38    BAS   RE,OLYS96           SUMMER 96 OLYMPICS CHECK                     
         LA    R4,7(R4)            NEXT WEEK                                    
         OC    0(2,R4),0(R4)       TEST EOL                                     
         BNZ   ACT32                                                            
*                                                                               
ACTX     DS    0H                  RETURN 6 BYTE ENTRIES                        
         LA    R1,WEEKLIST                                                      
         L     RE,SBABKLST                                                      
         SR    R0,R0                                                            
*                                                                               
ACTX2    MVC   0(6,RE),0(R1)                                                    
         LA    RE,6(RE)                                                         
         LA    R1,7(R1)                                                         
         OC    0(2,R1),0(R1)       TEST EOL                                     
         BZ    ACTX4                                                            
         BCT   R0,ACTX2                                                         
*                                                                               
ACTX4    LPR   R0,R0                                                            
         ST    R0,SBNBKS           RETURN ENTRY COUNT                           
         B     BOOKX                                                            
         SPACE 2                                                                
* ROUTINE TO CHANGE BOOK TO ALTERNATE YEAR FOR ACT/YY SITUATION                 
*                                                                               
ALTYEAR  DS    0H                                                               
         MVC   BOOK,LOOKBOOK                                                    
         MVC   BOOK(1),SBQBOOK+3                                                
         NI    BOOK,255-X'80'                                                   
         CLI   BOOK,92             TEST ALT YEAR NOT 92                         
         BE    ALTYEAR2                                                         
         TM    ATLOCT92,AO92SWIT   AND SWITCHED TO ATLANTA NOV/92 BOOK          
         BZ    ALTYEAR2                                                         
         MVI   BOOK+1,10           YES-SWITCH MONTH BACK TO OCT                 
         BR    RE                                                               
ALTYEAR2 TM    ATLOCT92,AO92ATLA   NO-TEST IT'S AN NSI ATLANTA STATION          
         BZR   RE                                                               
         CLC   BOOK,=X'5C0A'       AND BOOK = OCT/92                            
         BNER  RE                                                               
         MVC   BOOK,=X'5C0B'       YES-FORCE TO NOV/92                          
         BR    RE                                                               
         EJECT                                                                  
OLYS96   CLC   4(2,R4),=X'6007'                                                 
         BNE   OLYS98                                                           
         CLC   0(2,R4),=X'C0E8'                                                 
         BL    OLYS962                                                          
         CLC   2(2,R4),=X'C108'                                                 
         BH    OLYS962                                                          
         BR    RE                                                               
*                                                                               
OLYS962  CLI   SBEDEMTY,C'A'       MUST BE AFFIDS TO                            
         BNE   OLYS964                                                          
         CLI   SBD0PROF+7,C'J'     USE EXCLUSION TAPE                           
         BE    *+8                                                              
         CLI   SBD0PROF+7,C'Y'                                                  
         BNE   OLYS964                                                          
         TM    OLYEXCL,XJ96SWIT    AND EXCLUSION MUST BE THERE                  
         BZ    OLYS964                                                          
         BR    RE                                                               
*                                                                               
OLYS964  CLI   SBD0PROF+6,C'M'                                                  
         BNE   *+10                                                             
         MVC   4(2,R4),=X'6005'                                                 
         CLI   SBD0PROF+6,C'J'                                                  
         BNER  RE                                                               
         MVC   4(2,R4),=X'5F07'                                                 
         BR    RE                                                               
*                                                                               
OLYS98   CLC   4(2,R4),=X'6202'   TEST BOOK = FEB/98                            
         BNER  RE                                                               
         CLI   SBBKTYPE,C' '       NO OLYMPIC IF BKTYP PASSED                   
         BL    *+10                                                             
         CLI   SBBKTYPE,C'O'                                                    
         BNER  RE                                                               
         CLI   SBD0PROF+8,C'W'    TEST FORCE JAN TO NOV OPTION                  
         BNE   OLYS98A                                                          
         CLC   2(2,R4),=X'C39D'   TEST ENDS BEFORE DEC29/97                     
         BL    OLYS98A                                                          
         CLC   0(2,R4),=X'C439'   TEST STARTS AFTER JAN25/98                    
         BH    OLYS98A                                                          
         MVC   4(2,R4),=X'610B'   THEN FORCE BOOK TO NOV/97                     
         BR    RE                                                               
*                                                                               
OLYS98A  CLI   SBD0PROF+7,C'Y'    TEST TO USE OLY EXCL DATA AT ALL              
         BE    *+10                                                             
         CLI   SBD0PROF+7,C'F'                                                  
         BNER  RE                                                               
         CLI   SBEDEMTY,C'A'       MUST NOT BE AFFIDS!                          
         BER   RE                                                               
* NEED TO CHECK IF MARKET HAS MAR/98 BOOK - IF NOT, USE OLY EXCL                
         LA    RF,NSIMAR98                                                      
OLYS98B  CLC   DBACTRMK,0(RF)                                                   
         BE    OLYS98C                                                          
         LA    RF,2(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   OLYS98B                                                          
         B     OLYS98X                                                          
* BOOK IS FEB/98. IF AFTER MARCH SWEEP, USE OLY EXCL BOOK                       
OLYS98C  CLC   0(2,R4),=X'C481'                                                 
         BH    OLYS98X2                                                         
         BR    RE                                                               
* TEST IF DATES ARE OUTSIDE OF B'CAST FEB/98                                    
OLYS98X  CLC   0(2,R4),=X'C456'    TEST STARTS AFTER 980222                     
         BH    OLYS98X2             YES                                         
         CLC   2(2,R4),=X'C43A'    TEST ENDS BEFORE 980126                      
         BL    OLYS98X2             YES                                         
         BR    RE                                                               
*                                                                               
OLYS98X2 OI    5(R4),X'80'         SET BOOK TO X'82'                            
         BR    RE                                                               
         EJECT                                                                  
* RADIO ACTUAL BOOK LOOKUP ROUTINE                                              
*                                                                               
ACTRADIO DS    0H                                                               
         LA    R0,MAXWEEKS         WEEK LIST LIMIT                              
         GOTO1 VMOBILE,DMCB,((R0),STDATE),(5,IOAREA),ADCONS,SBSPPROF            
         ZIC   R0,0(R1)                                                         
         ST    R0,SBNBKS                                                        
         L     R4,SBABKLST         R4=A(OUTPUT)                                 
         LA    R5,IOAREA           R5=A(MOBILE DATE LIST)                       
*                                                                               
ACTR2    DS    0H                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELSRC,SOURCE                                                  
         MVC   DBSELSTA,SBSTA                                                   
         MVC   DBSELAGY,SBAGY                                                   
         MVC   DBSELCLI,SBCLT                                                   
         ST    R9,DBCOMFCS                                                      
         LA    R1,IOAREA2                                                       
         ST    R1,DBAREC                                                        
*                                                                               
         GOTO1 CDATCON,DMCB,(X'02',2(R5)),(X'03',BOOK)                          
         MVC   DBSELDAT,BOOK                                                    
         MVI   DBSELDAY,X'7F'      MON-FRI IS BASE                              
         MVC   DBSELTIM,=X'07080726'                                            
*                                                                               
         MVC   DBSELRMK,SBMKNUM                                                 
         MVC   DBSELMK,SBMKNUM                                                  
*                                                                               
         MVC   DBSELBK,=X'FFF5'                                                 
         MVI   RADBOOK,0                                                        
         GOTO1 CDEMAND,DMCB,DBLOCK,RLHOOK                                       
         MVC   DBSELDAT,BOOK                                                    
         MVC   DBSELBK,=X'FFF1'    LATEST BOOK                                  
         XC    DBACTBK,DBACTBK                                                  
         CLI   DBSELDAT+1,1        ADJUST JAN                                   
         BE    *+8                                                              
         CLI   DBSELDAT+1,4        ADJUST APRIL                                 
         BE    *+8                                                              
         CLI   DBSELDAT+1,10       ADJUST OCTOBER                               
         BNE   ACTR14                                                           
         ZIC   RE,DBSELDAT+1                                                    
         LA    RE,1(RE)                                                         
         STC   RE,DBSELDAT+1                                                    
*                                                                               
ACTR14   GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,NOTFOUND    FOUND STATION FOR BOOK                       
         BNE   ACTR16                                                           
         MVC   0(4,R4),0(R5)       MOVE IN PERIOD                               
         XC    4(2,R4),4(R4)       SET NO BOOK LOOKUP                           
         B     ACTR60                                                           
*                                                                               
ACTR16   MVC   0(4,R4),0(R5)       SET DATE TABLE                               
         MVC   4(2,R4),DBACTBK                                                  
*                                                                               
         LA    R6,RADFILT                                                       
ACTR30   CLI   0(R6),X'FF'         FIND CORRECT MODEL                           
         BE    ACTR50                                                           
         CLC   0(1,R6),RADBOOK     KEYED BY BOOKS SWEPT FOR STATION             
         BE    ACTR40                                                           
         LA    R6,13(R6)                                                        
         B     ACTR30                                                           
*                                                                               
ACTR40   ZIC   RE,BOOK+1           SET INDEX                                    
         AR    RE,R6                                                            
*                                                                               
         CLI   BOOK+1,4            ALLOW PREV YEAR TO APRIL                     
         BL    *+14                                                             
         CLC   4(1,R4),BOOK                                                     
         BNE   ACTR50                                                           
*                                                                               
         CLC   5(1,R4),0(RE)       IT'S OK                                      
         BE    *+10                                                             
ACTR50   XC    4(2,R4),4(R4)                                                    
*                                                                               
         LA    R4,6(R4)                                                         
         LA    R5,4(R5)                                                         
ACTR60   BCT   R0,ACTR2                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RLHOOK   NTR1                                                                   
*                                                                               
         CLC   DBACTKMK,=H'269'    MCALLEN DROPPED BOOKS                        
         BNE   *+12                                                             
         MVI   RADBOOK,X'50'                                                    
         B     RLHOOKX                                                          
*                                                                               
         CLI   DBACTBK+1,2                                                      
         BNE   *+8                                                              
         OI    RADBOOK,B'10000000'                                              
         CLI   DBACTBK+1,5                                                      
         BNE   *+8                                                              
         OI    RADBOOK,B'01000000'                                              
         CLI   DBACTBK+1,7                                                      
         BNE   *+8                                                              
         OI    RADBOOK,B'00100000'                                              
         CLI   DBACTBK+1,11                                                     
         BNE   *+8                                                              
         OI    RADBOOK,B'00010000'                                              
RLHOOKX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
*==================================================================*            
*        THIS SECTION FOR CANADIAN MARKETS THAT HAVE WEEKLY        *            
*        DATA. ACTUAL BOOK IS ALWAYS THE BROADCAST MONTH FOR       *            
*        THESE                                                     *            
*==================================================================*            
TORMTH   NTR1                                                                   
         CLI   MEDIA,C'C'                                                       
         BNE   TORMTHX                                                          
         CLI   SB1WPROF+8,C'M'                                                  
         BNE   TORMTHX                                                          
         LA    R0,MAXWEEKS                                                      
         GOTO1 VMOBILE,DMCB,((R0),STDATE),(5,IOAREA),ADCONS,SBSPPROF            
         ZIC   R0,0(R1)                                                         
         L     R4,SBABKLST                                                      
         LA    R5,IOAREA                                                        
*                                                                               
         CLC   SBMKNUM,=H'9109'    ALLOW SPILL INTO TOR                         
         BE    *+10                                                             
         CLC   SBMKNUM,=H'5199'    VANCOUVER                                    
         BE    *+14                                                             
         OC    SBMKNUM,SBMKNUM                                                  
         BNZ   TORMTHX                                                          
         CLC   SBSTA(4),=C'JXVA'                                                
         BNE   *+10                                                             
         MVC   SBSTA(4),=C'SPNP'                                                
         LA    RE,BBMTORL                                                       
         CLC   SBSTA+1(3),=C'TOR'     GLOBAL ETC.                               
         BE    TORMTH1                                                          
         LA    RE,BBMVANL                                                       
         CLC   SBSTA+1(3),=C'VAN'     GLOBAL ETC.                               
         BE    TORMTH1                                                          
         LA    RE,BBMTORL                                                       
TORMTH1  CLI   0(RE),X'FF'         END OF ALL LISTS                             
         BE    TORMTHX             NO CSI MARKET                                
         CLC   SBSTA(4),0(RE)                                                   
         BE    TORMTH2                                                          
         LA    RE,4(RE)                                                         
         B     TORMTH1                                                          
*                                                                               
TORMTH2  LA    RF,BBMVANL-4                                                     
         MVI   DUB,97              TORONTO START                                
         CR    RE,RF                                                            
         BL    *+8                                                              
         MVI   DUB,98              VANCOUVER START                              
TORMTH3  GOTO1 CDATCON,DMCB,(X'02',2(R4)),(X'03',DUB+1)                         
         CLC   DUB+1(1),DUB         OUTSIDE PERIOD IGNORE                       
         BL    TORMTHX                                                          
         MVC   0(4,R4),0(R5)       OTHERWISE BUILD LIST                         
         MVC   4(2,R4),DUB+1                                                    
         LA    R4,6(R4)                                                         
         LA    R5,4(R5)                                                         
         ICM   RE,15,SBNBKS                                                     
         AHI   RE,1                                                             
         STCM  RE,15,SBNBKS                                                     
         BCT   R0,TORMTH2                                                       
*                                                                               
TORMTHX  XIT1                                                                   
*                                                                               
BBMTORL  DC    C'CBLT'                                                          
         DC    C'CFTO'                                                          
         DC    C'CITY'                                                          
         DC    C'CFMT'                                                          
*        DC    C'CKCO'                                                          
*        DC    C'CKVR'                                                          
         DC    C'GTOR'                                                          
         DC    C'ONTV'                                                          
         DC    C'CHCH'                                                          
         DC    C'CIII'                                                          
         DC    C'JXTO' SPNP                                                     
         DC    C'WGRZ'                                                          
         DC    C'WIVB'                                                          
         DC    C'WKBW'                                                          
         DC    C'WNED'                                                          
         DC    C'WUTV'                                                          
BBMVANL  DC    C'CBUT'                                                          
         DC    C'CHAN'                                                          
         DC    C'CHEK'                                                          
         DC    C'CIVT'                                                          
*        DC    C'CKNO'                                                          
         DC    C'CKVU'                                                          
         DC    C'KCPQ'                                                          
         DC    C'KCTS'                                                          
         DC    C'KING'                                                          
         DC    C'KIRO'                                                          
         DC    C'KOMO'                                                          
         DC    C'KSTW'                                                          
         DC    C'KVOS'                                                          
         DC    C'SPNP'                                                          
         DC    C'JXVA' SPNP                                                     
         DC    X'FF'                                                            
         DS    0F                                                               
         EJECT                                                                  
*==================================================================*            
* SUB-ROUTINE TO BUILD AN ABBREVIATED SWEEP TABLE FOR THE REQUEST  *            
*                                                                  *            
* SWEEP TABLE CONTAINS 6 BYTE ENTRIES-SWEEP START(COMPRESSED),     *            
*      SWEEP END(COMPRESSED), SWEEP BOOK (Y/M)                     *            
* ON EXIT, NSWEEPS CONTAINS N'SWEEP TABLE ENTRIES                  *            
*==================================================================*            
         SPACE 1                                                                
BLDSWP   NTR1                                                                   
         L     R2,=A(SWEEPTAB)                                                  
         A     R2,RELO                                                          
         USING SWPTABD,R2          R2=A(MAIN SWEEP TABLE)                       
         SR    R3,R3               R3=N'SHORT SWEEP TABLE ENTRIES               
         LA    R5,SWTAB            R5=A(SHORT SWEEP TABLE)                      
*                                                                               
BLDSWP2  CLI   SWPKEY,X'00'        TEST FOR EOT                                 
         BE    BLDSWPX             YES                                          
         CLC   SWPKEY(3),FILE      MATCH ON FILE/MEDIA/SOURCE                   
         BNE   BLDSWP8                                                          
*                                                                               
         MVC   WORK(6),SWPST       SWEEP START                                  
         ZIC   R0,SWPWEEKS                                                      
         MH    R0,=H'7'            CALCULATE SWEEP END                          
         BCTR  R0,0                                                             
         GOTO1 CADDAY,DMCB,WORK,WORK+6,(R0)                                     
*                                                                               
BLDSWP4  CLC   STDATE,WORK+6       TEST FOR ANY OVERLAP WITH REQUEST            
         BH    BLDSWP8             NO                                           
         CLC   ENDATE,WORK         TEST REQUEST ENDS BEFORE SWEEP START         
         BL    BLDSWP8             YES                                          
*                                                                               
BLDSWP6  GOTO1 CDATCON,DMCB,WORK,(2,0(R5))                                      
         GOTO1 (RF),(R1),WORK+6,(2,2(R5))                                       
         MVC   4(2,R5),SWPBOOK     SET SWEEP BOOK                               
         LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         LA    R5,6(R5)            BUMP TABLE ENTRY POINTER                     
*                                                                               
BLDSWP8  LA    R2,SWPTABL(R2)      NEXT MAIN SWEEP TABLE ENTRY                  
         B     BLDSWP2                                                          
*                                                                               
BLDSWPX  STC   R3,NSWEEPS                                                       
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* SUB-ROUTINE TO LOOK UP THE BOOK IN ACTUAL BOOK TABLE              *           
*                                                                   *           
* AT ENTRY, THISBOOK CONTAINS Y/M, THISWEEK CONTAINS WEEK NUMBER    *           
*           WITHIN MONTH, AND R6=A(ACTUAL BOOK TABLE ENTRY)         *           
* ON EXIT,  LOOKBOOK CONTAINS DEMO LOOKUP BOOK                      *           
*===================================================================*           
         SPACE 1                                                                
GETBOOK  ST    RE,SAVERE                                                        
         MVC   LOOKTYPE,ACTTYPE    INITIALIZE DEFAULT OR FORCE BOOK             
         MVC   LOOKBOOK(1),THISBOOK INITIALIZE YEAR                             
         ZIC   RE,THISBOOK+1                                                    
         LA    RE,ACTBOOKS-1(RE)   INDEX INTO LOOKUP BOOK TABLE                 
         CLI   0(RE),X'FF'         TEST FOR WEEKLY MONTH                        
         BE    GETBK2              YES                                          
         MVC   LOOKBOOK+1(1),0(RE) EXTRACT LOOKUP MONTH                         
         B     GETBK5                                                           
*                                                                               
GETBK2   LA    RE,ACTSPEC                                                       
         LA    R0,4                                                             
         CLC   THISBOOK+1(1),0(RE) FIND WEEKLY TABLE FOR MONTH                  
         BE    GETBK4                                                           
         LA    RE,L'ACTSPEC(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
GETBK4   ZIC   R1,THISWEEK                                                      
         LA    RE,0(R1,RE)         INDEX TO WEEK ENTRY                          
         MVC   LOOKBOOK+1(1),0(RE)                                              
*                                                                               
GETBK5   TM    LOOKBOOK+1,X'80'    TEST FOR FORCE BOOK                          
         BZ    *+12                NO                                           
         NI    LOOKBOOK+1,X'FF'-X'80' TURN OFF FORCE BIT                        
         MVI   LOOKTYPE,ACTFORCE   NOTE BOOK IS FORCED                          
         CLI   LOOKBOOK+1,13       TEST FOR REGULAR MONTH                       
         BL    GETBK7              YES-ALL DONE                                 
         CLI   LOOKBOOK+1,24       TEST 25-36=PREVIOUS YEAR                     
         BH    GETBK6              YES                                          
*                                                                               
         ZIC   RE,LOOKBOOK         GET YEAR                                     
         LA    RE,1(RE)            AND INCREMENT IT                             
         STC   RE,LOOKBOOK                                                      
         ZIC   RE,LOOKBOOK+1                                                    
         SH    RE,=H'12'           ADJUST MONTH                                 
         STC   RE,LOOKBOOK+1                                                    
         B     GETBK7                                                           
*                                                                               
GETBK6   ZIC   RE,LOOKBOOK         DECREMENT YEAR                               
         BCTR  RE,0                                                             
         STC   RE,LOOKBOOK                                                      
         ZIC   RE,LOOKBOOK+1       ADJUST MONTH                                 
         SH    RE,=H'24'                                                        
         STC   RE,LOOKBOOK+1                                                    
*                                                                               
GETBK7   MVI   OLYMPIC,C'N'                                                     
         MVI   ATLOCT92,0                                                       
*                                                                               
         CLI   SBBKTYPE,C' '       NO OLYMPICS FOR SPEC BOOKS                   
         BL    *+12                                                             
         CLI   SBBKTYPE,C'O'                                                    
         BNE   GETBOOKX                                                         
*                                                                               
* D0PROF+7 WILL NOT BE N IF WE ARE USING OLYMPIC EXCLUSION TAPE                 
*                                                                               
         CLI   SBD0PROF+7,C'J'     TEST USE JULY OLYM EXCL TAPE                 
         BE    *+12                                                             
         CLI   SBD0PROF+7,C'N'     FIELD 8 WINS THIS SITUATION                  
         BNE   GETBOOKX                                                         
* THIS CODE ONLY IF NOT USING OLYMPIC EXCLUSION TAPE                            
         CLI   SBD0PROF+2,C'Y'     TEST SPECIAL OLYMPIC OPTION ON               
         BE    *+12                                                             
         CLI   SBD0PROF+2,C'F'     TEST SPECIAL OLYMPIC OPTION ON               
         BNE   GETBKB                                                           
         CLI   THISBOOK,X'62'      1998 OLYMPIC                                 
         BE    GETBK9                                                           
         CLI   THISBOOK,X'5E'      1994 OLYMPIC                                 
         BE    GETBK8                                                           
         CLC   THISBOOK,=X'5C02'   AND THE Y/M IS NOT FEB/92                    
         BE    GETBOOKX                                                         
         CLC   LOOKBOOK,=X'5C02'   BUT THE ACTUAL BOOK IS FEB/92                
         BNE   GETBKB                                                           
         MVI   OLYMPIC,C'Y'        YES-                                         
         MVC   LOOKBOOK,=X'5C01'   CHANGE TO JAN/92 FOR DEFAULT BK              
         CLI   LOOKTYPE,ACTFORCE                                                
         BNE   GETBOOKX                                                         
         MVC   LOOKBOOK,=X'5B0B'   OR NOV/91 FOR FORCE BOOK                     
         B     GETBOOKX                                                         
*                                                                               
GETBK8   CLC   THISBOOK,=X'5E02'   AND THE Y/M IS NOT FEB/94                    
         BE    GETBOOKX                                                         
         CLC   LOOKBOOK,=X'5E02'   BUT THE ACTUAL BOOK IS FEB/94                
         BNE   GETBKB                                                           
         MVI   OLYMPIC,C'Y'        YES-                                         
         MVC   LOOKBOOK,=X'5E01'   CHANGE TO JAN/94 FOR DEFAULT BK              
         CLI   LOOKTYPE,ACTFORCE                                                
         BNE   GETBOOKX                                                         
         MVC   LOOKBOOK,=X'5D0B'   OR NOV/93 FOR FORCE BOOK                     
         B     GETBOOKX                                                         
*                                                                               
GETBK9   CLC   THISBOOK,=X'6202'   IF THE Y/M IS NOT FEB/98                     
         BE    GETBOOKX                                                         
         CLC   LOOKBOOK,=X'6202'   BUT THE ACTUAL BOOK IS FEB/98                
         BNE   GETBKB                                                           
         MVI   OLYMPIC,C'Y'        YES-                                         
         MVC   LOOKBOOK,=X'6201'   CHANGE TO JAN/98 FOR DEFAULT BK              
         LA    R1,NSIJAN98                                                      
         CLC   DBACTRMK,0(R1)                                                   
         BE    GTBKJ98                                                          
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   *-18                                                             
         MVC   LOOKBOOK,=X'610B'   OR NOV/97 FOR FORCE BOOK                     
         B     GETBOOKX                                                         
GTBKJ98  CLC   THISBOOK,=X'6202'   WHAT ABOUT MARCH                             
         BNH   GETBOOKX                                                         
         LA    R1,NSIMAR98                                                      
         CLC   DBACTRMK,0(R1)                                                   
         BE    GTBKM98                                                          
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   *-18                                                             
         B     GETBOOKX                                                         
GTBKM98  MVC   LOOKBOOK,=X'6203'                                                
         B     GETBOOKX                                                         
*                                                                               
GETBKB   CLI   SOURCE,C'N'         TEST NSI                                     
         BNE   GETBOOKX                                                         
         LA    R1,ATLSTA           AND ATLANTA STATION                          
*                                                                               
GETBKC   CLI   0(R1),X'FF'                                                      
         BE    GETBOOKX                                                         
         CLC   SBSTA(4),0(R1)                                                   
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETBKC                                                           
         OI    ATLOCT92,AO92ATLA   YES                                          
         CLC   LOOKBOOK,=X'5C0A'   TEST ACTUAL BOOK = OCT/92                    
         BNE   GETBOOKX                                                         
         OI    ATLOCT92,AO92SWIT   YES-                                         
         MVC   LOOKBOOK,=X'5C0B'   FORCE TO NOV/92 BOOK                         
*                                                                               
GETBOOKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*===================================================================*           
* SUB-ROUTINE TO VALIDATE A STATION/BOOK                            *           
*                                                                   *           
* AT ENTRY, LOOKBOOK CONTAINS BOOK TO BE VALIDATED                  *           
*                                                                   *           
* ON EXIT, VSTAOK IS Y/N IF BOOK VALID/INVALID                      *           
*          AND VSTABOOK CONTAINS BOOK THAT WAS VALIDATED (LOOKBOOK) *           
*===================================================================*           
         SPACE 1                                                                
VALSTA   NTR1                                                                   
         CLC   LOOKBOOK,VSTABOOK   TEST IF BOOK=LAST BOOK VALIDATED             
         BE    VALSTAX             YES-SET CC AND EXIT                          
*                                                                               
VALSTA1  MVC   VSTABOOK,LOOKBOOK   SAVE LAST BOOK VALIDATED                     
         MVI   VSTAOK,C'N'         SET VALID SWITCH TO NO                       
         MVI   OLYEXCL,0                                                        
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         ST    R9,DBCOMFCS                                                      
         LA    R1,IOAREA2                                                       
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,SOURCE                                                  
         MVC   DBSELSTA,SBSTA                                                   
         CLI   DBSELSTA+4,C' '                                                  
         BE    *+12                                                             
         CLI   DBSELSTA+4,C'N'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   DBSELBK,LOOKBOOK                                                 
         MVI   DBSELDAY,X'40'      MONDAY                                       
         MVC   DBSELTIM,=AL2(1800,1830) 6-630P                                  
         MVC   DBSELAGY,SBAGY                                                   
         MVC   DBSELCLI,SBCLT                                                   
         MVC   DBBTYPE,SBBKTYPE    SET BOOK TYPE                                
         TM    SBINDS,SBIRSOVR     TEST RATING SERVICE OVERRIDE                 
         BO    *+10                                                             
         MVC   DBSELUMK,SBBMKT     NO-PASS AGENCY MARKET NUMBER                 
*                                                                               
         CLI   MEDIA,C'R'          FIX FOR RADIO RE-RATE                        
         BNE   *+16                                                             
         MVC   DBSELRMK,SBMKNUM                                                 
         MVC   DBSELMK,SBMKNUM                                                  
*                                                                               
         OC    SBEMKT,SBEMKT       TEST FOR REQUESTED MARKET                    
         BZ    VALSTA2             NO                                           
         CLC   SBEMKT,SBBMKT       TEST FOR SPILL LOOKUP                        
         BE    VALSTA2             NO                                           
         MVC   DBSELMK,SBBSPL      YES-SET SPILL MKT RTG SRV NUMBER             
         TM    SBINDS,SBIRSOVR     TEST RATING SERVICE OVERRIDE                 
         BO    VALSTA2                                                          
         MVC   DBSELUMK,SBEMKT     NO-PASS SPILL AGENCY MARKET NUMBER           
*                                                                               
VALSTA2  CLI   DBSELMED,C'T'       REDIRECT ARB TO NSI AFTER 1993               
         BNE   VALSTA3                                                          
         CLI   DBSELBK,X'5E'                                                    
         BL    VALSTA3                                                          
         CLI   DBSELSRC,C'A'                                                    
         BNE   VALSTA3                                                          
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
VALSTA3  GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         MVC   DBSELSRC,SOURCE     RESTORE SOURCE                               
         CLI   DBERROR,NOTFOUND                                                 
         BE    VALSTAX                                                          
*                                                                               
         LA    R1,OLY96M           CHECK JUL96 OLY EXCLUSION MKT                
VALSTA4  CLC   DBACTRMK,0(R1)                                                   
         BL    VALSTA6                                                          
         BE    *+12                                                             
         LA    R1,2(R1)                                                         
         B     VALSTA4                                                          
         OI    OLYEXCL,XJ96SWIT    FOUND AN EXCLUSION                           
*                                                                               
VALSTA6  MVI   VSTAOK,C'Y'         SET LAST LOOKUP BOOK IS OK                   
*                                                                               
VALSTAX  CLI   VSTAOK,C'Y'         SET CC ON EXIT                               
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* SUB-ROUTINE TO GET THE SWEEP DATES FOR A BOOK                     *           
*                                                                   *           
* AT ENTRY, BOOK CONTAINS SWEEP MONTH AND P1=A(START/END DATES)     *           
*===================================================================*           
         SPACE 1                                                                
GETSWP   NTR1                                                                   
         L     R2,0(R1)            R3=A(OUTPUT)                                 
         MVC   0(4,R2),=X'FFFF0000'  FORCE NO OVERLAP AS A DEFAULT              
         SR    R3,R3                                                            
         ICM   R3,1,NSWEEPS                                                     
         BZ    GETSWPX                                                          
         LA    R5,SWTAB                                                         
*                                                                               
GETSWP2  CLC   BOOK,4(R5)          TEST FOR MATCH ON BOOK                       
         BE    GETSWP4                                                          
         LA    R5,6(R5)                                                         
         BCT   R3,GETSWP2                                                       
         B     GETSWPX                                                          
*                                                                               
GETSWP4  MVC   0(4,R2),0(R5)       GET SWEEP START/END                          
*                                                                               
GETSWPX  B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* SPLIT REQUEST DATES ON SWEEP DATES                                *           
* INPUT SWEEP LIST IS IN SWTAB                                      *           
* OUTPUT WEEK LIST IN IOAREA2                                       *           
*                                                                   *           
* THIS CODE CONTRIBUTED BY AATK 06OCT98.                            *           
* WITH OUR DEEPEST GRATITUDE     MHER/BZEH                          *           
* WITH MY MOST VENEMOUS HATRED   EJOR/BZEH                          *           
*===================================================================*           
         SPACE 1                                                                
SPLTWKS  NTR1                                                                   
         GOTO1 VGETBRD,DMCB,(1,STDATE),WORK,CGETDAY,CADDAY                      
         MVC   STDATE(6),WORK                                                   
*                                                                               
         LA    R0,MAXWEEKS                                                      
         GOTO1 VMOBILE,DMCB,((R0),STDATE),(5,IOAREA),ADCONS,SBSPPROF            
         MVC   NWEEKS,0(R1)        SAVE N'WEEKS                                 
*                                                                               
         LA    R2,SWTAB            R2=A(SWEEP TABLE)                            
         LA    R3,BIGSWTAB         R3=A(RESOLVED SWEEP LIST)                    
         XR    R4,R4                                                            
         ICM   R4,1,NSWEEPS        NO SWEEP DATES TO RESOLVE                    
         BZ    SPLTW08                                                          
*                                                                               
SPLTW02  GOTO1 VDATCON,DMCB,(2,(R2)),(0,DUB),0                                  
         LHI   R0,-1                                                            
         GOTO1 CADDAY,(R1),DUB,DUB2,(R0)    GET SWEEP START -1                  
         GOTO1 CDATCON,(R1),(0,DUB2),(2,(R3)),0                                 
*                                                                               
         MVC   2(2,R3),0(R2)                GET SWEEP START                     
         MVC   4(2,R3),2(R2)                GET SWEEP END                       
*                                                                               
         GOTO1 CDATCON,(R1),(2,2(R2)),(0,DUB),0                                 
         LA    R0,1                                                             
         GOTO1 CADDAY,(R1),DUB,DUB2,(R0)    GET SWEEP END +1                    
         GOTO1 CDATCON,(R1),(0,DUB2),(2,6(R3)),0                                
*                                                                               
         LA    R2,6(R2)                     NEXT IN SWTAB                       
         LA    R3,8(R3)                     NEXT IN SPLIT TABLE                 
         BCT   R4,SPLTW02                                                       
* FIND START/END DUPS IN BIGSWTAB                                               
         LA    R2,BIGSWTAB         R2=A(RESOLVED SWEEP LIST)                    
         LA    R3,8(R2)            NEXT IN SPLIT TABLE                          
         LA    R2,4(R2)            FIRST END PAIR                               
*                                                                               
SPLTW04  CLI   0(R3),0             ANOTHER START PAIR?                          
         BE    SPLTW08             NO                                           
         CLC   0(4,R2),0(R3)       LAST END SAME AS NEXT START?                 
         BNE   SPLTW06                                                          
         MVC   0(4,R3),XFF         FLAG START AS A DUPLICATE                    
*                                                                               
SPLTW06  LA    R2,8(R2)            NEXT END PAIR                                
         LA    R3,8(R3)            NEXT START PAIR                              
         B     SPLTW04                                                          
         EJECT                                                                  
* BIG MOMENT OCCURS NOW - BUILD OUTPUT IN IOAREA2                               
*                                                                               
SPLTW08  LA    R2,IOAREA           R2=A(MOBILE DATE LIST)                       
         LA    R3,BIGSWTAB         R3=A(RESOLVED SWEEP LIST)                    
         LA    R4,IOAREA2          R4=A(MERGED LIST)                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,NWEEKS           R0=N'ENTRY PAIRS OFF R2                      
         SLL   R0,1                R0=N'DATES OFF R2                            
*                                                                               
SPLTW10  LTR   R0,R0               ANY DATES REMAINING?                         
         BZ    SPLTW20             NO - FINISHED                                
         CLC   0(4,R3),XFF         DUPLICATE START PAIR?                        
         BNE   *+12                NO                                           
         LA    R3,4(R3)            GO PAST DUPLICATE                            
         B     SPLTW10             AND TRY AGAIN                                
*                                                                               
         CLI   0(R3),0             ANY SWEEP DATES REMAINING?                   
         BE    SPLTW14             NO                                           
         CLC   0(2,R2),0(R3)       FIND EARLIEST BTW DATE AND SWEEP             
         BL    SPLTW14             DATE EARLIER                                 
         BH    SPLTW12                                                          
         LA    R3,2(R3)            GO PAST EQUAL SWEEP DATE                     
         B     SPLTW14             AND MOVE IN FROM DATE                        
*                                                                               
SPLTW12  MVC   0(2,R4),0(R3)       MOVE IN NEXT SWEEP DATE                      
         LA    R3,2(R3)                                                         
         LA    R4,2(R4)                                                         
         B     SPLTW10                                                          
*                                                                               
SPLTW14  MVC   0(2,R4),0(R2)       MOVE IN NEXT DATE                            
         LA    R2,2(R2)                                                         
         AHI   R0,-1               REDUCE COUNT OF DATES IN LIST                
         LA    R4,2(R4)                                                         
         B     SPLTW10             NEXT DATE                                    
*                                                                               
SPLTW20  XC    0(2,R4),0(R4)       CLEAR LAST ENTRY                             
         EJECT                                                                  
*===================================================================*           
* MATCH PERIODS TO SWEEP TABLE TO SET DEFAULT BOOK                  *           
* AS BUILT, A START END DATE PAIR EITHER FALLS WITHIN A SWEEP       *           
* OR OUTSIDE A SWEEP, SO ONLY ONE DATE OF THE PAIR IS USED          *           
* ENTRIES OUTSIDE ANY SWEEP ARE LEFT BLANK !                        *           
*===================================================================*           
         SPACE 1                                                                
SPLTW30  LA    R4,WEEKLIST                                                      
         LA    R6,IOAREA2                                                       
         XR    R0,R0                                                            
*                                                                               
SPLTW32  MVC   0(4,R4),0(R6)       MOVE START/END DATES                         
         ICM   R0,1,NSWEEPS                                                     
         BZ    SPLTW34                                                          
         LA    R5,SWTAB                                                         
*                                                                               
SPLTW33  CLC   2(2,R6),0(R5)       ENTRY END TO SWEEP START                     
         BNL   SPLTW36             NOT LOW - SO WITHIN OR AFTER SWP             
* BEFORE SWEEP PERIOD - SET MONTH OF END DATE AS BOOK                           
SPLTW34  GOTO1 CDATCON,DMCB,(2,2(R6)),(3,FULL)                                  
         MVC   4(2,R4),FULL        SET BOOK                                     
         CLC   2(2,R4),=X'C73D'    OR IF WEEK ENDS ON 9/29/99                   
         BNE   *+10                                                             
         MVC   4(2,R4),=X'630A'    THEN USE OCT BOOK                            
         CLC   2(2,R4),=X'C69C'    OR IF WEEK ENDS ON 4/28/99                   
         BNE   *+10                                                             
         MVC   4(2,R4),=X'6305'    THEN USE MAY BOOK                            
         B     SPLTW41                                                          
*                                                                               
SPLTW36  CLC   2(2,R6),2(R5)       ENTRY END TO SWEEP END                       
         BNH   SPLTW38             LOW OR EQUAL - USE THIS SWEEP ENTRY          
         LA    R5,6(R5)            NEXT SWEEP TABLE ENTRY                       
         BCT   R0,SPLTW33                                                       
         B     SPLTW34                                                          
*                                                                               
SPLTW38  MVC   4(2,R4),4(R5)       USE THIS SWEEP                               
*                                                                               
* FOLLOWING INSTRUCTIONS BECAUSE ZEN SAYS SO                                    
         CLC   2(2,R4),=X'C481'    OR IF WEEK ENDS ON 4/1/98                    
         BNE   SPLTW39                                                          
         L     RE,AACTTAB                                                       
         CLI   ACTBOOKS+2-ACTTABD(RE),3  AND THERE IS NO MARCH BOOK             
         BE    SPLTW41                                                          
         MVC   4(2,R4),=X'6204'    THE USE APRIL BOOK                           
*                                                                               
SPLTW39  CLC   2(2,R4),=X'C67F'    OR IF WEEK ENDS ON 3/31/99                   
         BNE   SPLTW40                                                          
         L     RE,AACTTAB                                                       
         CLI   ACTBOOKS+2-ACTTABD(RE),3  AND THERE IS NO MARCH BOOK             
         BE    SPLTW41                                                          
         MVC   4(2,R4),=X'6304'    THE USE APRIL BOOK                           
*                                                                               
SPLTW40  CLC   2(2,R4),=X'C69C'    OR IF WEEK ENDS ON 4/28/99                   
         BNE   *+10                                                             
         MVC   4(2,R4),=X'6305'    THE USE MAY BOOK                             
         CLC   2(2,R4),=X'C73D'    OR IF WEEK ENDS ON 9/29/99                   
         BNE   *+10                                                             
         MVC   4(2,R4),=X'630A'    THEN USE OCT BOOK                            
*                                                                               
SPLTW41  LA    R4,7(R4)            NEXT WEEKLIST ENTRY                          
         LA    R6,4(R6)            NEXT DATETABLE ENTRY                         
         OC    0(2,R6),0(R6)                                                    
         BNZ   SPLTW32                                                          
         XC    0(4,R4),0(R4)       CLEAR LAST ENTRY IN WEEKLIST                 
*                                                                               
         LA    R0,MAXWEEKS                                                      
         GOTO1 VMOBILE,DMCB,((R0),STDATE),(5,IOAREA2),ADCONS,SBSPPROF           
*                                                                               
* NOW BUILD LIST OF START/END/WEEK NUMBER WITHIN MONTH IN IOAREA                
*                                                                               
         LA    R4,IOAREA2                                                       
         LA    R5,IOAREA                                                        
*                                                                               
SPLTW42  ICM   R0,3,2(R4)          GET WEEK END DATE                            
         N     R0,=X'0000FFE0'     DROP DAYS                                    
         LA    RE,1                                                             
*                                                                               
SPLTW44  MVC   0(4,R5),0(R4)       MOVE START/END DATES                         
         STC   RE,4(R5)            SET WEEKNUM                                  
*                                                                               
         LA    R4,4(R4)            NEXT ENTRY                                   
         LA    R5,5(R5)                                                         
         LA    RE,1(RE)            ADD 1 TO WEEK                                
*                                                                               
         CLI   0(R4),X'FF'         TEST EOL                                     
         BE    SPLTW50                                                          
*                                                                               
         ICM   R1,3,2(R4)                                                       
         N     R1,=X'0000FFE0'     DROP DAYS                                    
         CR    R0,R1               TEST SAME YEAR/MONTH                         
         BE    SPLTW44                                                          
         B     SPLTW42                                                          
*                                                                               
SPLTW50  XC    0(5,R5),0(R5)       SET EOL FLAG                                 
*                                                                               
         LA    R4,WEEKLIST         LET'S DO THIS THE EASY WAY...                
SPLTW52  LA    R5,IOAREA                                                        
*                                                                               
SPLTW54  CLC   0(2,R4),0(R5)       START EARLIER THAN LIST                      
         BL    SPLTW56                                                          
***      BL    EXIT                                                             
*                                                                               
         CLC   0(2,R4),2(R5)       START AFTER END OF BRDWK ENTRY               
         BNH   *+12                                                             
         LA    R5,5(R5)            NEXT BRDWK ENTRY                             
         B     SPLTW54             AND TRY AGAIN                                
*                                                                               
         MVC   6(1,R4),4(R5)       MOVE WEEKNUM TO WEEKLIST                     
SPLTW56  LA    R4,7(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   SPLTW52                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
XFF      DC    16X'FF'                                                          
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* TABLE OF NSI ATLANTA STATIONS FOR OCT/92 MISSING BOOK                         
*                                                                               
ATLSTA   DC    C'WAGA'                                                          
         DC    C'WATL'                                                          
         DC    C'WGNX'                                                          
         DC    C'WGTV'                                                          
         DC    C'WPBA'                                                          
         DC    C'WSB '                                                          
         DC    C'WTBS'                                                          
         DC    C'WVEU'                                                          
         DC    C'WXIA'                                                          
         DC    X'FF'                                                            
* TABLE OF NSI JUL/96 OLYMPIC EXCLUSION MARKETS                                 
*                                                                               
OLY96M   DC    AL2(101,104,105,106,109,110,111,112)                             
         DC    AL2(113,114,115,117,127,128,132,134)                             
         DC    AL2(135,138,139,140,142,147,148,156)                             
         DC    AL2(160,163,166,167,168,170,175,202)                             
         DC    AL2(206,213,216,217,218,222,223,234)                             
         DC    AL2(235,236,237,241,250,257,271,278)                             
         DC    AL2(292,309,317,351,353,370,400,403)                             
         DC    AL2(407,419,420,425,462,466)                                     
         DC    X'FF'                                                            
*                                                                               
* THESE TABLES ARE USED FOR RADIO ACTUAL BOOKS                                  
* LIMITS MUST BE SET FOR BOOKS EVEN THOUGH LATEST IS USED TO                    
* DETERMINE WHICH BOOKS APPLY.                                                  
RADFILT  DC    X'40',AL1(05,05,05,05,05,05,05,05,05,05,05,05)                   
         DC    X'50',AL1(11,11,11,05,05,05,05,05,05,11,11,11)                   
         DC    X'70',AL1(11,11,11,05,05,05,07,07,07,11,11,11)                   
         DC    X'F0',AL1(02,02,02,05,05,05,07,07,07,11,11,11)                   
****THE FOLLOWING ENTRIES COVER NEW STATIONS WITH INCOMPLETE DATA               
         DC    X'10',AL1(11,11,11,00,00,00,00,00,00,11,11,11)                   
         DC    X'30',AL1(11,11,11,00,00,00,07,07,07,11,11,11)                   
         DC    X'60',AL1(00,00,00,05,05,05,07,07,07,00,00,00)                   
         DC    X'B0',AL1(02,02,02,00,00,00,07,07,07,11,11,11)                   
         DC    X'90',AL1(02,02,02,00,00,00,00,00,00,11,11,11)                   
         DC    X'D0',AL1(02,02,02,05,05,05,00,00,00,11,11,11)                   
         DC    X'FF'                                                            
*                                                                               
* THESE TABLES ARE USED FOR THE 1998 SPECIAL OLYMPIC OPTION                     
NSIJAN98 DC    AL2(168,112,106,202,115,105,216,403,128,217)                     
         DC    AL2(213,101,104,108,407,209,111)                                 
         DC    X'FFFF'                                                          
NSIMAR98 DC    AL2(101,104,202,403)                                             
         DC    X'FFFF'                                                          
         EJECT                                                                  
       ++INCLUDE SPSWPTABS                                                      
         EJECT                                                                  
* DSECT TO COVER SPOTBOOK WORKING STORAGE                                       
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
APARM    DS    A                                                                
PARMS    DS    0A                                                               
ASBLOCK  DS    A                                                                
PARML    EQU   *-PARMS                                                          
SAVERE   DS    A                                                                
DAYNUM   DS    F                                                                
*                                                                               
VMOBILE  DS    V                                                                
ADCONS   DS    0C                                                               
VGETBRD  DS    V                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VDATCON  DS    V                                                                
*                                                                               
AACTTAB  DS    A                   A(ACTUAL BOOK TABLE ENTRY)                   
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
         DS    0F                                                               
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
*                                                                               
OPTVALS  DS    0CL(L'SBBKOPT)      OPTIMIZATION CONTROL VALUES                  
OPTCLT   DS    CL(L'SBBCLT)        PACKED CLIENT CODE                           
OPTMKT   DS    CL(L'SBBMKT)        MARKET CODE (BINARY)                         
OPTSTA   DS    CL(L'SBSTA)         CALL LETTERS                                 
OPTQBOOK DS    CL(L'SBQBOOK)       REQUESTED BOOK                               
OPTBKTY  DS    CL(L'SBBKTYPE)      BOOK TYPE                                    
OPTSTART DS    XL2                 REQUEST START DATE (COMPRESSED)              
OPTEND   DS    XL2                 REQUEST END DATE (COMPRESSED)                
OPTDAYS  DS    XL1                 DAYS ROTATION                                
OPTSPMKT DS    XL2                 SPILL MARKET                                 
         DS    CL(L'OPTVALS-(*-OPTVALS))  SPARE                                 
*                                                                               
NWEEKS   DS    X                                                                
NSWEEPS  DS    X                                                                
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
FILE     DS    C                                                                
MEDIA    DS    C                                                                
SOURCE   DS    C                                                                
BOOK     DS    XL2                                                              
PROFILE  DS    X                                                                
CLASS    DS    X                                                                
OLYMPIC  DS    C                                                                
OLYEXCL  DS    X                   OLYMPIC EXCLUSION SWITCH                     
XJ96SWIT EQU   X'80'               JUL96 EXCLUSION MARKET                       
ATLOCT92 DS    X                                                                
AO92SWIT EQU   X'80'                                                            
AO92ATLA EQU   X'40'                                                            
*                                                                               
RADBOOK  DS    C                   RADIO BOOK                                   
THISBOOK DS    XL2                                                              
THISWEEK DS    X                                                                
LOOKBOOK DS    XL2                                                              
LOOKTYPE DS    X                                                                
VSTABOOK DS    XL2                 LAST LOOKUP BOOK                             
VSTAOK   DS    C                   Y=LAST LOOKUP BOOK ON FILE,N=NO              
*                                                                               
SWEEP1ST DS    XL2                                                              
SWEEP1ND DS    XL2                                                              
SWEEP2ST DS    XL2                                                              
SWEEP2ND DS    XL2                                                              
*                                                                               
MAXSWEEP EQU   15                  MAXIMUM N'SWEEPS                             
SWTAB    DS    CL(MAXSWEEP*6)                                                   
BIGSWTAB DS    CL(MAXSWEEP*4*2)                                                 
*                                                                               
MAXWEEKS EQU   105                 MAXIMUM N'BROADCAST WEEKS                    
*                                  (ENOUGH FOR 2 YEARS)                         
WEEKLIST DS    CL((MAXWEEKS*7)+2)   BROADCAST WEEK DATES                        
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
IOAREA   DS    CL1000              IO AREAS FOR DEMO READ                       
IOAREA2  DS    CL1000                                                           
IOAREA2L EQU   *-IOAREA2                                                        
*                                                                               
WORKX    EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE SPSWPTABSD                                                     
         EJECT                                                                  
* DSECT TO COVER SPOTBLOCK                                                      
*                                                                               
SBLOCKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         SPACE 2                                                                
* DDCOMFACS                                                                     
* DEDEMEQUS                                                                     
* SPGENAGY                                                                      
* SPGENMKT                                                                      
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMEQUS                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBUY                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPOTBOOKS 05/01/02'                                      
         END                                                                    
