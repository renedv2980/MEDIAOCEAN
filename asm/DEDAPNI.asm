*          DATA SET DEDAPNI    AT LEVEL 012 AS OF 01/03/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEDAPNIA                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE NSIWEEK                                                                
         TITLE 'DEMO CONVERSION - LOCAL DAILIES PROGRAM NAMES'                  
*                                                                               
***********************************************************************         
* SEE DEDAILYSRT (AND DEPREDALY) TO UNDERSTAND HOW THIS PROGRAM GETS            
* ITS INPUT FILE.                                                               
***********************************************************************         
*                                                                               
DEDAPNI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEDAPNI                                                        
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING IDAILYD,RC                                                       
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
OPENOK   L     R4,ARREC                                                         
         CLI   BYPREAD,0           NOT 0 MEANS TO RE-PROC LAST READ REC         
         BNE   READOK                                                           
*                                                                               
READREC  GET   IN1,(R4)                                                         
*                                                                               
         L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AHI   R0,1                                                             
         ST    R0,0(RE)                                                         
*                                                                               
READOK   XC    PREVKEY,PREVKEY     CLEAR LAST KEY FOR MKT CHANGE                
         CLC   IDRECDE,=C'11'                                                   
         BE    MKREC                                                            
         CLC   IDRECDE,=C'12'                                                   
         BE    PNREC                                                            
         B     SKIP                (SHOULD NEVER HAPPEN!)                       
*                                                                               
*============================================================                   
*        MARKET RECORD ROUTINES                                                 
*============================================================                   
*                                                                               
MKREC    LH    R1,SVMKCNT                                                       
         AHI   R1,1                                                             
         STH   R1,SVMKCNT                                                       
*                                                                               
         MVI   BYPREAD,0                                                        
*                                                                               
         XC    PREVSTAT,PREVSTAT                                                
*                                                                               
         LAY   RE,M2STALST         CLEAR STATION LIST STORAGE                   
         MVC   0(2,RE),=X'FFFF'                                                 
         LA    RE,2(RE)                                                         
         LHI   RF,M2STALST_LEN-2                                                
         XCEF  ,                                                                
*                                                                               
**************************************************************                  
* SEARCH THROUGH OUR MARKET TABLE TO SEARCH FOR MARKET NUMBER*                  
**************************************************************                  
*                                                                               
         MVI   CBFLG,0                                                          
         CLC   =C'CABLE',IDPHDST                                                
         BNE   *+8                                                              
         MVI   CBFLG,1                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CDEMTABS,DMCB,ONITEMKT                                           
         DROP  RF                                                               
         ICM   RE,15,0(R1)         RE = A(TABLE)                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),0             TABLE HAS NO ENTRIES ?!?                     
         JE    *+2                                                              
         L     RF,4(R1)            RF = LENGTH OF EACH ENTRY                    
*                                                                               
         CLC   IDPHMKC,=C'124'     WE FORCE ATLANTA TO USE                      
         BNE   *+10                                                             
         MVC   IDPHMKC,=C'168'     THE OLD MKT # TO MATCH MONTHLY               
*                                                                               
         PACK  DUB,IDPHMKC                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMRKT                                                     
*                                                                               
MKR20    CLI   0(RE),X'00'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   IDPHABR(3),0(RE)                                                 
         BE    MKR30                                                            
         AR    RE,RF                                                            
         B     MKR20                                                            
*                                                                               
*KR20    CLI   0(RE),X'00'                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        CLC   INTMRKT,3(RE)                                                    
*        BE    MKR30                                                            
*        AR    RE,RF                                                            
*        B     MKR20                                                            
*                                                                               
MKR30    DS    0H                                                               
         MVC   SVMKNO,IDPHMKC                                                   
         MVC   SVMKALF,0(RE)                                                    
*                                                                               
         MVC   PMKEYE,=C'PROCESSED MKT - '                                      
         MVC   PMKTNO,SVMKNO                                                    
         MVC   PMKALF,SVMKALF                                                   
         MVC   PPHGEO,IDPHGEO                                                   
         MVC   PPHABR,IDPHABR                                                   
         MVC   PPHDAT,IDPHDAT                                                   
         MVC   PPHRTG,IDPHRTG                                                   
         MVC   PPHPFI,IDPHPFI                                                   
         MVC   PPHDST,IDPHDST                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     OPENOK                                                           
*                                                                               
*--------------------------PROGRAM NAME RECORD------------------------*         
PNREC    DS    0C                                                               
*                                                                               
         MVI   INTBTYP,0                 BOOK TYPE                              
*                                                                               
         PACK  DUB,SVMKNO                                                       
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT              MARKET                                 
*                                                                               
         LA    RE,IDPNSTT                SQH                                    
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTO1 VHRTOQH,DMCB,(1,DUB),INTSQH                                      
         LA    RE,IDPNETT                EQH                                    
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTO1 VHRTOQH,DMCB,(1,DUB),INTEQH                                      
         CLI   INTEQH,0                                                         
         BNE   *+8                                                              
         MVI   INTEQH,X'60'                                                     
*                                                                               
         ZIC   R1,INTEQH                                                        
         ZIC   RE,INTSQH                                                        
         CR    R1,RE                                                            
         BNL   *+8                                                              
         AHI   R1,96                                                            
         SR    R1,RE                                                            
         STC   R1,INTADUR                DURATION                               
         AR    R1,RE                                                            
         SHI   R1,1                                                             
         STC   R1,INTEQH                 END QTR HOUR                           
*                                                                               
*        ZIC   R1,INTEQH                                                        
*        SHI   R1,1                                                             
*        STC   R1,INTEQH                                                        
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,IDPNSTD),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
* ONLY MINUS A DAY FOR PRE 3A START TIME                                        
***      CLC   =X'7305',INTBOOK    FEB0115 FOR TEST DATA                        
****     CLC   IDPNSTD,=C'12/03/2015' DEC0315 LIVE DATE FOR 3A                  
* LOCAL DAILIES START OF DAY IS 3AM ON DEC03/2015                               
* BUT DUE TO A BUG IN THE COMPARE LOGIC IN THIS CONVERSION PROGRAM              
* WE WERE INCORRECTLY STILL PROCESSING AS IF WE ARE 5AM START OF DAY            
* IN ORDER NO TO CHANGE THE EXISTING POSTING NUMBERS WE ARE GOING               
* TO SET THE DATE TO THE DATE THIS CODE WILL GO INTO EFFECT.                    
         GOTO1 VDATCON,DMCB,(9,=C'20151203'),DUB2                               
         CLC   DUB,DUB2            CHECK IF PRIOR TO 12/03/2015                 
         BNL   PNR05A                                                           
*                                  WHEN GOING LIVE                              
*                                                                               
* ON FILE  12-245A HAS A A CALENDAR  DAY AHEAD                                  
* EXAMPLE 12A-245A =02/18/15... 3A-1145P = 02/17/15                             
* PRE 3A START TIME- SUB DAY 12A-545A                                           
         CLI   INTSQH,X'5C'              12A-445A MINUS A DAY                   
         BNL   PNR05                     SO 3A-445A GOES TO PREVIOUS            
         CLI   INTSQH,X'48'              DAY OF WEEK AND WILL MERGE             
         BL    PNR05                     PREV DAY                               
         GOTO1 VADDAY,DMCB,DUB,DUB,F'-1'                                        
         B     PNR05                                                            
*                                                                               
*                                                                               
* 3A START TIME LOGIC ONLY SUBTRACT TIME FROM 12A-245A TO GET IT                
* BACK TO THE SAME INTERNAL DDS DAY OF WEEK                                     
* 3A-445A DO NOT SUBTRACT BECAUSE WE WANT TO KEEP IT TO THE CURRENT             
* WEEK OF DAY                                                                   
PNR05A   CLI   INTEQH,X'53'        12A-245A MINUS A DAY                         
         BH    PNR05                                                            
         CLI   INTSQH,X'48'                                                     
         BL    PNR05                                                            
         GOTO1 VADDAY,DMCB,DUB,DUB,F'-1'                                        
*                                                                               
*                                                                               
PNR05    XC    DMCB(6*4),DMCB                                                   
         MVI   BYTE,1                    SET START DAY TO MONDAY                
         GOTO1 VNSIWEEK,DMCB,DUB,(BYTE,VGETDAY),VADDAY,VDATCON                  
*                                                                               
         MVC   NSIBKYM+1(1),0(R1)        WEEK                                   
         MVC   NSIBKYM(1),4(R1)          YEAR                                   
         MVC   INTBOOK,NSIBKYM           BOOK                                   
*                                                                               
         GOTO1 VGETDAY,DMCB,DUB,DUB                                             
*                                                                               
         LA    RF,DAYTABL                GET DAYS                               
PNR10    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DUB(3),0(RF)                                                     
         BE    *+12                                                             
         AHI   RF,L'DAYTABL                                                     
         B     PNR10                                                            
         MVC   INTDAYWK,3(RF)                                                   
         MVI   INTNDAYS,1                DAILIES DUH.                           
         MVI   INTWEEKS,1                SET UP SINGLE WEEK                     
*                                                                               
         PACK  DUB,IDPNSTC               GO RETRIEVE CALL LETTERS               
         CVB   RE,DUB                                                           
         STCM  RE,7,SVSTA                                                       
         BAS   RE,GETCALL                                                       
         CLI   BYPREAD,0                                                        
         BE    *+12                                                             
         MVI   BYPREAD,0                                                        
         B     OPENOK                                                           
*                                                                               
         MVI   INTSTYP,PARENTE                                                  
         MVC   INTPNAME,IDPNPGN                                                 
         MVC   INTPRSRC,IDPNSRC                                                 
         MVI   INTDTYP,0                                                        
         PACK  DUB,IDPNTID                                                      
         MVC   INTTRKID,DUB+2                                                   
         PACK  DUB,IDPNTEID                                                     
         MVC   INTTELID,DUB+2                                                   
         PACK  DUB,IDPNNVID                                                     
         MVC   INTVARID,DUB+2                                                   
         BAS   RE,SETKEY                                                        
*                                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------------------- SORT RECORD ----------------------------*         
*                                                                               
SETKEY   NTR1  ,              BUILD PAV SORT KEY                                
         L     R6,AIREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'P'                                                     
         MVI   PRCODE,PRCODEQU                                                  
         MVI   PRMEDIA,C'O'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP      FORCE SATELLITES TO SORT TOG                 
         CLI   CBFLG,1                                                          
         BE    *+12                                                             
         TM    INTSTYP,X'20'       SET UP SPILL STATION                         
         BZ    *+14                                                             
         MVI   INTSPILL,C'Y'                                                    
         MVC   PRKMKT,INTMRKT                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVI   PRBTYP+1,C'P'-X'40'                                              
         MVC   PRBTYP+2(1),INTDAYWK                                             
         MVC   PRSTIM,INTEQH                                                    
*                                                                               
         MVC   PRDW,INTDAYWK       SET UP DAY AND WEEK                          
         MVC   INTDAYWK,PRDW       SET INTERIM DAY AND WEEK                     
         MVC   PRDW+1(1),INTDTYP   FORCE (NOR) TO SORT FIRST                    
         DROP  R6                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*        GET CALL LETTERS USING OFFICIAL NIELSEN STATION NUMBERS                
***********************************************************************         
GETCALL  NTR1                                                                   
*                                                                               
         LAY   R5,M2STALST                                                      
         USING ONSTAD,R3                                                        
GC01     LA    R3,4(R5)                                                         
         CLC   0(2,R5),=X'FFFF'    EMPTY TABLE?                                 
         BE    GC05                                                             
         CLC   INTBOOK,0(R5)       TABLE ALREADY FILLED W/ LATEST               
         BNH   GC70                                                             
*                                                                               
         LAY   RE,M2STALST         CLEAR STATION LIST STORAGE                   
         MVC   0(2,RE),=X'FFFF'                                                 
         LA    RE,2(RE)                                                         
         LHI   RF,M2STALST_LEN-2                                                
         XCEF  ,                                                                
         B     GC01                                                             
*                                                                               
* DEIS OCT/2019:                                                                
*                                                                               
*   THE CODE BELOW READS THE MARKET LIST PASSIVES FOR THE PROGRAM               
*   LINEUP DATA BEING PROCESSED. THIS CODE IS PROBABLY LEFT OVER FROM           
*   THE "OVERNIGHTS" CONVERSION (AS OPPOSED TO THE "LOCAL DAILIES").            
*   IF A MARKET PASSIVE IS NOT FOUND, THE MARKET IS SKIPPED.                    
*                                                                               
*   AS IT TURNS OUT, WE PROBABLY DON'T EVEN NEED THE INFORMATION WE             
*   GET BY READING THESE PASSIVES. BUT SINCE IT WAS HARMLESS TO READ            
*   THEM, WE JUST LEFT THIS CODE IN PLACE (PROBABLY WITHOUT THINKING            
*   ABOUT IT) WHEN THE OVERNIGHTS WERE ELIMINATED AND THE LOCAL DAILIES         
*   BEGAN. THIS WAS PERFECTLY SAFE: THE MARKET PASSIVES ARE GENERATED           
*   BY THE TIME PERIOD CONVERSION, AND PROVIDED THAT THE TIME PERIOD            
*   CONVERSION RUNS BEFORE THIS ONE, THERE WILL BE NO PROBLEM.                  
*                                                                               
*   ALL THAT CHANGED IN OCT/2019, WHEN NIELSEN CHANGED THE LOCAL                
*   DAILIES DELIVERY SCHEDULE SUCH THAT THE PROGRAM LINEUP DATA                 
*   ARRIVED *BEFORE* THE TIME PERIOD DATA. THIS MEANT THAT WHEN WE RAN          
*   THIS CONVERSION ON A TUESDAY (I.E., FOR MONDAY'S DATA, WHICH IS             
*   THE FIRST DAY OF THE WEEK), WE FOUND NO TIME PERIOD DATA FOR THAT           
*   WEEK, AND THE CODE BELOW CAUSED THIS PROGRAM TO SKIP ALL OF THE             
*   MARKETS. AS A RESULT, WE LOADED NO PROGRAM NAMES FOR THAT DAY.              
*                                                                               
*   THE "BEST" SOLUTION TO THIS PROBLEM WOULD BE TO MODIFY THE CODE             
*   BELOW SO THAT IT NO LONGER READS THE MARKET PASSIVES. BUT AS A              
*   STOPGAP (POSSIBLY PERMANENT) SOLUTION, OPERATIONS IS MODIFYING              
*   THE LOAD SCHEDULE SO THAT WE DO NOT ATTEMPT TO PROCESS THE PROGRAM          
*   LINEUP DATA UNTIL AFTER THE TIME PERIOD DATA HAS BEEN LOADED.               
*                                                                               
GC05     DS    0H                  RETRIEVE POINTERS FOR BOOK                   
         LA    R7,KEY                                                           
         XC    KEY,KEY                                                          
         USING MLKEY,R7                                                         
         MVI   MLCODE,MLCODEQU                                                  
         MVI   MLMEDIA,C'O'                                                     
         MVI   MLSRC,C'N'                                                       
         MVC   MLBOOK,NSIBKYM                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
*                                                                               
GC10     CLC   0(2,R5),=X'FFFF'                                                 
         BE    GC20                                                             
         LH    RE,2(R5)                                                         
         AR    R5,RE                                                            
         B     GC10                                                             
*                                                                               
GC20     MVC   0(2,R5),INTBOOK                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,KEY                      
         B     GC40                                                             
GC30     GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',KEY,KEY                      
GC40     CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MLRMKT,INTMRKT                                                   
         BNE   GC60                                                             
         XC    MLBOOK,=X'FFFF'                                                  
         CLC   MLBOOK,NSIBKYM                                                   
         BNE   GC60                                                             
*                                                                               
         CLC   PREVSTAT,MLSTAT                                                  
         BE    GC30                                                             
         MVC   PREVSTAT,MLSTAT                                                  
*                                                                               
*        CLI   CBFLG,1                                                          
*        BNE   *+12                                                             
*        LA    RE,GC55                                                          
*        B     *+8                                                              
*        LA    RE,GC30                                                          
*                                                                               
*        CLI   MLBTYP,C'C'                                                      
*        BER   RE                                                               
*        CLI   MLBTYP,C'U'                                                      
*        BER   RE                                                               
*                                                                               
GC55     MVC   ONSTA,MLSTAT                                                     
         MVC   ONSTANO,MLSTANO                                                  
         OC    MLKMKT,MLKMKT                                                    
         BZ    *+8                                                              
         MVI   ONSPILL,C'Y'                                                     
         LA    R3,ONSTAEQ(R3)                                                   
         C     R3,=A(M2STALST_X)   ANY MORE ROOM IN TABLE?                      
         JNL   *+2                 NO: INCREASE M2STALST_LEN                    
         B     GC30                                                             
*                                                                               
GC60     MVC   ONSTA(2),=X'FFFF'      LEAD DESCRIPTION                          
         LA    RE,ONSTA+2                                                       
         MVC   0(2,RE),=X'FFFF'                                                 
         LAY   R5,M2STALST         STORE LENGTH                                 
GC61     LA    R3,4(R5)                                                         
         CLC   NSIBKYM,0(R5)       MAKE SURE IT'S THE CORRECT BOOK              
         BE    GC62                                                             
         LH    R0,2(R5)                                                         
         AR    R5,R0                                                            
         B     GC61                                                             
GC62     SR    RE,R5                                                            
         STH   RE,2(R5)                                                         
*                                                                               
*C70     CLC   0(2,R5),=X'FFFF'    STATIONS HAS TO BE FOUND                     
*        BNE   *+12                                                             
*        MVI   BYPREAD,2                                                        
*        B     GCX                                                              
GC70     CLC   ONSTA(2),=X'FFFF'                                                
         BNE   *+12                                                             
         MVI   BYPREAD,2                                                        
         B     GCX                                                              
         CLC   SVSTA,ONSTANO                                                    
         BE    GC80                                                             
         LA    R3,ONSTAEQ(R3)                                                   
         B     GC70                                                             
*                                                                               
GC80     MVC   INTSTA,ONSTA                                                     
         MVC   INTSPILL,ONSPILL                                                 
*                                                                               
GCX      XIT1                                                                   
***********************************************************************         
*------------USE TABLE TO CONVERT FROM RREC TO IREC                             
* R4 = A(CONVERSION TABLE)                                                      
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,9                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MHI   R5,9                ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         PACK  DUB,0(9,R5)         PACK IT                                      
         CVB   RF,DUB              AND CONVERT                                  
         ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MHI   R5,4                ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
         ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
*              SORT RECORD PROCESSING FOR KEY SEQUENCING                        
*                                                                               
CNVWR    DS    0H                                                               
         L     R6,ASREC            POINT AT SORT RECORD                         
         LR    R2,R6               ADDRESSABILITY FOR INTERIM VALUES            
         LA    R6,4(R6)            POINT AT RECORD START                        
         USING PRKEY,R6                                                         
         CLC   PREVKEY,PRKEY       TEST FOR KEYS IN SEQUENCE                    
         BL    CNVWR2              OK                                           
         MVI   BYPSORT,X'80'                                                    
         B     CNVWR2              UNCONDITIONAL BRANCH TO SKIP OPMSG           
*                                                                               
* DEIS OCT/2019. NOTE: AT SOME POINT IN 2006 OR PRIOR, THE WARNING CODE         
*    BELOW WAS DISABLED (VIA AN UNCONDITIONAL BRANCH AROUND IT). I'M            
*    NOT SURE WHETHER THIS WARNING EVER MEANT ANYTHING, OR WHETHER WE           
*    WOULD EVER CARE ABOUT IT.                                                  
*                                                                               
         CLC   PRSTAT(4),WARNSTA                                                
         BE    CNVWR2                                                           
         GOTO1 VHEXOUT,DMCB,PRKEY,WARNMSG2,18                                   
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('WARNMSGQ',WARNMSG)                     
         MVC   WARNSTA,PRSTAT                                                   
*                                                                               
CNVWR2   MVC   PREVKEY,PRKEY       UPDATE PREVIOUS KEY                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 2                                                                
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVC   P(43),=C'IPHASE: TOTAL NUMBER OF MARKETS PROCESSED -'            
         EDIT  SVMKCNT,(3,P+44),ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 VPRINTER                                                         
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
*                                                                               
SKIP     DS    0H                                                               
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                RETURN TO DEMCNV                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DAYTABL  DS    0CL4                                                             
         DC    C'MON',X'01'         MON                                         
         DC    C'TUE',X'02'         TUE                                         
         DC    C'WED',X'03'         WED                                         
         DC    C'THU',X'04'         THU                                         
         DC    C'FRI',X'05'         FRI                                         
         DC    C'SAT',X'06'         SAT                                         
         DC    C'SUN',X'07'         SUN F LIST                                  
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
CANOUT   EQU   32                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
GLOBIND  EQU   4                                                                
*                                                                               
DUB2     DS    D                                                                
*                                                                               
WARNMSG  DC    C'AUTONOTE*US-MFDEMOSPROGRAMMERS:PON DUPLICATE KEY!'             
WARNMSG2 DS    CL(18*2)            FOR HEXOUT OF MAJOR KEY                      
WARNMSGQ EQU   *-WARNMSG                                                        
*                                                                               
WARNSTA  DS    CL4                                                              
*                                                                               
BYTE     DS    C                                                                
BYPREAD  DS    XL1                                                              
NSIBKYM  DS    CL2                                                              
KEY      DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PREVSTAT DS    CL5                                                              
CBFLG    DC    X'00'                                                            
SVMKNO   DS    CL3                                                              
SVMKALF  DS    CL3                                                              
SVMKCNT  DC    H'0'                                                             
SVSTA    DS    XL3                                                              
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
RRECL    EQU   2400                                                             
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'M2STALST'                                                      
M2STALST_LEN EQU 7000                                                           
M2STALST DS    (M2STALST_LEN)C                                                  
M2STALST_X EQU *                                                                
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
       ++INCLUDE DEINTOPND                                                      
         EJECT                                                                  
       ++INCLUDE DEDAILYD                                                       
         EJECT                                                                  
* DEDEMCNVD                                                                     
* DEDEMFILE                                                                     
* DEDEMTABD                                                                     
* DDDPRINT                                                                      
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   P                                                                
PMKEYE   DS    CL16                                                             
PMKTNO   DS    CL(L'SVMKNO)                                                     
         DS    C                                                                
PMKALF   DS    CL(L'SVMKALF)                                                    
         DS    C                                                                
PPHGEO   DS    CL10                                                             
         DS    C                                                                
PPHABR   DS    CL6                                                              
         DS    C                                                                
PPHDAT   DS    CL10                                                             
         DS    C                                                                
PPHRTG   DS    CL5                                                              
         DS    C                                                                
PPHPFI   DS    CL6                                                              
         DS    C                                                                
PPHDST   DS    CL9                                                              
         ORG                                                                    
         EJECT                                                                  
ONSTAD   DSECT                                                                  
*NSTABK  DS    XL2                                                              
*NSTALN  DS    H                                                                
ONLIVE   DS    CL1                                                              
ONSTA    DS    CL5                                                              
ONSTANO  DS    XL3                                                              
ONSPILL  DS    CL1                                                              
ONSTAEQ  EQU   *-ONSTAD                                                         
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DEDAPNI   01/03/20'                                      
         END                                                                    
