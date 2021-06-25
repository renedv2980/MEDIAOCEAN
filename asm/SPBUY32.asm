*          DATA SET SPBUY32    AT LEVEL 103 AS OF 02/17/21                      
*PHASE T21132C                                                                  
                                                                                
*================================================================               
* THIS PROGRAM IS CALLED FROM SPBUY31 TO CREATE A MAKEGOOD GROUP                
* THE TSAR BUFFER HAS BEEN BUILT AND IS IN STORAGE                              
* ON ENTRY R1 POINTS TO A(SPBUYWK), A(SPBUY31WK)                                
*================================================================               
*                                                                               
*===============================================================                
* YYMMMDD WHO  DESCRIPTION                                                      
*                                                                               
* 05APR04 HWON SELF APPLY MAKEGOODS (SVDARPRF+13)                               
*                                                                               
* 08JAN04 HWON  DON'T DIE IF THERE ARE NO COMMENTS ON A SPLIT                   
*                                                                               
* 20JUN09 HWON SPEC-39434 - SUPPORT MULTI-COMMENT MKGD REJECTIONS               
*                                                                               
* 20NOV04 WHOA SPEC-36302 - INCORPORATE AUTO-AVAIL UUIDS FOR DARE MG'S          
*                                                                               
*================================================================               
*                                                                               
T21132   TITLE 'SPBUY32 - SPOTPAK BUY - MAKEGOODS TO BUYS'                      
T21132   CSECT                                                                  
*&&ONLIN SET   Y                                                                
         PRINT NOGEN                                                            
         NMOD1 0,T21132,R9,RR=R7                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     R8,4(R1)                                                         
         USING BSPOOLD,R8                                                       
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
T        USING TSARD,TSARBLK                                                    
*                                                                               
         C     R7,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R7,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO XC    DMCB,DMCB           LOAD ORIGINAL BUY SCREEN                     
         MVC   DMCB+4(4),=X'D90211FE'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         MVI   SVSCR,X'FE'                                                      
***NOTE***                                                                      
* DATADISP IS DIFFERENT FOR BCAST (SPOT) VS CABLE (XSPOT) MKGDS                 
*   BE CAREFUL WHEN USING GETEL FOR NON MG RECORDS                              
***NOTE***                                                                      
****     MVC   DATADISP,=H'24'                                                  
         LA    R0,MNRFRST-MNKEY    SET DSPL TO FIRST DARE ELEM                  
         CLI   BUYST,C'0'                                                       
         BL    *+8                                                              
         LA    R0,MNXFRST-MNKEY                                                 
         STH   R0,DATADISP                                                      
*                                                                               
         OI    WRKRUPSW,WRKRUPSW_ISDRMG+WRKRUPSW_NEWDRMG                        
         OI    DRMGFLG,DRMG_NOSTAR                                              
         B     DOMK0                                                            
RELO     DC    A(0)                                                             
XSPREC   DC    AL2(42,32,5970)                                                  
         EJECT                                                                  
*=====================================================================*         
* PROCESS THE MAKEGOOD OFFER(S)                                                 
* IF THERE ARE NO OFFERS, GROUP IS A PREEMPT                                    
* IF THERE ARE NO MISSED SPOTS, GROUP IS A BONUS                                
*=====================================================================*         
                                                                                
DOMK0    LHI   R1,SVBUYOPH-BUYSAVE                                              
         AR    R1,RA                                                            
         MVC   0(L'SVBUYOPH,R1),BUYOPH  SAVE FLDHDR+24 BYTES                    
*                                                                               
         BRAS  RE,CKPREBON         SET PREEMPT/BONUS FLAG                       
*                                                                               
         XC    SKDTABLE,SKDTABLE                                                
         XC    SKDMKID,SKDMKID                                                  
*                                                                               
         OC    FRSTOFF,FRSTOFF                                                  
         BZ    DOMK31                                                           
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,FRSTOFF                                                 
*                                                                               
DOMK1    BRAS  RE,CALLTSAR                                                      
         BNE   DOMK20                                                           
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MOTYPE,MOTYPEQ                                                   
         BNE   DOMK1                                                            
*                                                                               
         MVC   MYSTTN,SVKEY+6                                                   
         CLI   BUYST,C'0'                                                       
         BL    *+10                                                             
DOMKCBL  MVC   MYSTTN,MOSTTN                                                    
         BRAS  RE,READMKO          READ OFFER RECORDS                           
*                                                                               
DOMK2    MVC   SKDSTTN,MOSTTN      SAVE STA/NET                                 
         MVC   SKDMKID,MORECID     SAVE OFFER/REC                               
         MVC   SVTSRNUM,T.TSRNUM   SAVE FIRST TSAR REC NUMBER                   
         EJECT                                                                  
*============================================================                   
* PUT PURPOSE CODE IN OPTIONS FIELD                                             
*============================================================                   
         SPACE 1                                                                
         XC    BUYOP,BUYOP         CLEAR WHATEVER'S THERE                       
         LHI   R1,SVBUYOPH-BUYSAVE                                              
         AR    R1,RA                                                            
         MVC   BUYOPH(L'SVBUYOPH),0(R1) RESTORE FLDHDR+24 BYTES                 
*                                                                               
         NI    BUYOPH+4,X'DF'      INVALIDATE OPTIONS &                         
         OI    BUYOPH+6,X'80'      TRANSMIT BUY OPTION FIELD                    
*                                                                               
         MVI   ELCODE,MOPRPELQ     FIND PURPOSE CODE ELEMENT                    
*                                                                               
         BRAS  RE,GETMKEL                                                       
         BNE   DOMK10                                                           
         B     DOMK6                                                            
*                                                                               
DOMK4    BRAS  RE,NXTMKEL                                                       
         BNE   DOMK10                                                           
*                                                                               
         USING MOPRPELD,R6                                                      
*                                                                               
DOMK6    OC    MOPRPOFR(2),MOPRPOFR  IF NO OFFER REC                            
         BZ    *+14                  ALWAYS USE IT                              
         CLC   MOPRPOFR(2),SKDMKID   TEST SAME OFFER/REC                        
         BNE   DOMK4                                                            
*                                                                               
         LA    RF,BUYOP                                                         
         SR    R0,R0                                                            
         ICM   R0,1,BUYOPH+5                                                    
         BZ    DOMK8                                                            
         AR    RF,R0                                                            
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
DOMK8    MVC   0(4,RF),=C'PUR='                                                 
         MVC   4(6,RF),MOPRPCOD                                                 
         LA    RF,10(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT BEYOND LAST CHAR                       
         LA    R0,BUYOP                                                         
         SR    RF,R0               GIVES CURRENT INPUT LEN                      
         STC   RF,BUYOPH+5         SET NEW INPUT LEN                            
***      NI    BUYOPH+4,X'DF'      INVALIDATE OPTIONS                           
***      OI    BUYOPH+6,X'80'                                                   
*                                                                               
DOMK10   MVI   ELCODE,MOUPGELQ     FIND DEMO UPGRADE ELEMENT                    
*                                                                               
         BRAS  RE,GETMKEL                                                       
         BNE   DOMK20                                                           
         B     DOMK14                                                           
*                                                                               
DOMK12   BRAS  RE,NXTMKEL                                                       
         BNE   DOMK20                                                           
*                                                                               
         USING MOUPGELD,R6                                                      
DOMK14   OC    MOUPGOFR(2),MOUPGOFR  IF NO OFFER REC                            
         BZ    *+14                  ALWAYS USE IT                              
         CLC   MOUPGOFR(2),SKDMKID   TEST SAME OFFER/REC                        
         BNE   DOMK12                                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,BUYOPH+5                                                    
         BNZ   DOMK16                                                           
         LA    RF,BUYOP            POINT TO INPUT POS                           
         B     DOMK18                                                           
*                                                                               
DOMK16   LA    RF,BUYOP(RE)        POINT TO NEXT INPUT POS                      
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)            POINT TO NEXT INPUT POSN                     
         LA    RE,1(RE)            BUMP INPUT LENGTH                            
         STC   RE,BUYOPH+5                                                      
*                                                                               
DOMK18   LA    R1,MOUPGTXT+27      WORK OUT ACTUAL TEXT LEN                     
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,MOUPGTXT                                                      
         SR    R1,R0               GIVES EX LENGTH                              
         BM    DOMK20                                                           
*                                                                               
         EX    R1,*+4                                                           
         MVC   0(0,RF),MOUPGTXT    MOVE UPGRADE TEXT GO OPTIONS                 
         AHI   R1,1                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYOPH+5                                                      
         AR    R0,R1                                                            
         STC   R0,BUYOPH+5         SET INPUT LENGTH                             
***      NI    BUYOPH+4,X'DF'      INVALIDATE OPTIONS                           
***      OI    BUYOPH+6,X'80'                                                   
         DROP  R6                                                               
*                                                                               
*****                                                                           
* MOVE THIS CKPREBON                                                            
****                                                                            
DOMK20   DS    0H                                                               
         OC    FRSTMSS,FRSTMSS                                                  
         BZ    DOMK24                                                           
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,FRSTMSS                                                 
*                                                                               
DOMK21   BRAS  RE,CALLTSAR                                                      
         BNE   DOMK24                                                           
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   DOMK24                                                           
         OC    MSREP,MSREP         DO WE HAVE A REP CODE?                       
         BZ    DOMK24              NO                                           
*                                                                               
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',MSREP),DUB                                       
*****                                                                           
* MOVE THIS CKPREBON                                                            
****                                                                            
*                                                                               
         LA    RF,BUYOP                                                         
         SR    R0,R0                                                            
         ICM   R0,1,BUYOPH+5                                                    
         BZ    DOMK22                                                           
         AR    RF,R0                                                            
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
DOMK22   MVC   0(4,RF),=C'REP='                                                 
         MVC   4(3,RF),DUB                                                      
*                                                                               
         LA    RF,7(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RF,1(RF)            POINT BEYOND LAST CHAR                       
         LA    R0,BUYOP                                                         
         SR    RF,R0               GIVES CURRENT INPUT LEN                      
         STC   RF,BUYOPH+5         SET NEW INPUT LEN                            
***      NI    BUYOPH+4,X'DF'      INVALIDATE OPTIONS                           
***      OI    BUYOPH+6,X'80'                                                   
*                                                                               
DOMK24   MVC   T.TSRNUM,SVTSRNUM   REREAD FIRST TSAR REC FOR GRP                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
DOMK26   BRAS  RE,BLDSKED                                                       
*                                                                               
         MVC   T.TSRNUM,SVTSRNUM   REREAD FIRST TSAR REC FOR GRP                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         BRAS  RE,BLDBUY           BUILD BUY INPUT AND ADD RECORD               
***      BNE   DOMK60              ON ERROR - GEN REPORT AND EXIT               
*                                                                               
* REBUILD GETREC TABLE SO CAN DO PUTREC                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(18),SVKEY       MOVE KEY/DISK ADDR                           
         GOTO1 READ                                                             
         MVC   AREC,AREC1          READ ADDED BUY RECORD INTO AREC1             
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,BLDDAREL         UPDATE DARE ELEMENT                          
*                                                                               
         BRAS  RE,BLDDEM           DO DEMO OVERRIDES                            
*                                                                               
         BRAS  RE,BLDCOM           DO COMMENTS                                  
*                                                                               
         BRAS  RE,BLDAAU           DO AUTO AVAIL UUID                           
*                                                                               
         BRAS  RE,BLDORB           DO ORBITS                                    
*                                                                               
         GOTO1 PUTREC              WRITE RECORD WITH CHANGES                    
*                                                                               
         XC    SKDTABLE,SKDTABLE   CLEAR SKED TABLE                             
*                                                                               
DOMK28   MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   DOMK30                                                           
         CLI   MOTYPE,MOTYPEQ      ANY MORE OFFER RECORDS                       
         BNE   DOMK30              NO - DONE                                    
*                                                                               
         CLC   MOSTTN,SKDSTTN      SAME STATION                                 
         BE    DOMK29              YES                                          
* FOR CABLE NEED TO WRITE BACK OFFER ON CHANGE OF STATION                       
         CLI   BUYST,C'0'                                                       
         BL    DOMK30                                                           
         MVI   USEMYIO,C'Y'                                                     
         MVC   AREC,AREC1          READ OFFER TO AREC1                          
         BRAS  RE,READMKO                                                       
         MVC   AREC,AREC3          WRITE OFFER FROM AREC3                       
         BRAS  RE,MYPUTREC                                                      
         B     DOMKCBL             AND GO PROCESS                               
*                                                                               
DOMK29   CLC   MORECID,SKDMKID     SAME OFFER/REC                               
         BE    DOMK28              YES - GET NEXT TSAR REC                      
         B     DOMK2               CHANGED - GO PROCESS                         
         EJECT                                                                  
*============================================================                   
* PROCESS MISSED RECORDS (IF NONE, THIS IS A BONUS OFFER)                       
*============================================================                   
         SPACE 1                                                                
DOMK30   CLI   BUYST,C'0'                                                       
         BL    DOMK31                                                           
         MVI   USEMYIO,C'Y'                                                     
         MVC   AREC,AREC1          READ OFFER TO AREC1                          
         BRAS  RE,READMKO                                                       
         MVC   AREC,AREC3          WRITE OFFER FROM AREC3                       
         BRAS  RE,MYPUTREC                                                      
*                                                                               
DOMK31   OC    FRSTMSS,FRSTMSS     TEST NO MISSED SPOTS                         
         BZ    DOMK60              NONE - THIS IS A BONUS                       
*                                                                               
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,FRSTMSS                                                 
*                                                                               
DOMK32   BRAS  RE,CALLTSAR                                                      
         BNE   DOMK60                                                           
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   DOMK34                                                           
*                                                                               
         OC    FRSTOFF,FRSTOFF     TEST ANY OFFERS                              
         BZ    DOMK36              NONE - THIS IS A PRE-EMPT                    
*&&DO                                                                           
* THIS BLOCK APPEAR UNNECESSARY                                                 
         MVC   MYSTTN,SVKEY+6                                                   
         CLI   BUYST,C'0'                                                       
         BL    DOMK34                                                           
         MVC   MYSTTN,MSSTTN       SET THE RIGHT STATION                        
*                                  HOW DO WE READ OFFER IF TSAR ENTRY           
*                                    IS MSTYPEQ??                               
         BRAS  RE,READMKO          MAKE SURE HAVE RIGHT OFFER REC               
* THIS BLOCK APPEAR UNNECESSARY                                                 
*&&                                                                             
DOMK34   BRAS  RE,DOGRP            BUILD MAKEGOOD GROUP                         
* TEMP TRAP CODE                                                                
         JE    *+4                                                              
* TEMP TRAP CODE                                                                
         B     DOMK50                                                           
*                                                                               
DOMK36   LHI   R0,ESTLOCK                                                       
         STCM  R0,3,BMGEERR                                                     
         TM    SVECNTRL,X'08'      TEST ESTIMATE LOCKED                         
         BO    DOMK36ER                                                         
         LHI   R0,PWLOCK                                                        
         OC    SVECOST2,SVECOST2                                                
         BZ    *+8                                                              
         LHI   R0,C2LOCK                                                        
         STCM  R0,3,BMGEERR        SIMULATE ERROR FROM CALLBASE                 
         TM    SVPWFLG,X'C0'       TEST PW BUY LOCK                             
         BZ    DOMK40                                                           
DOMK36ER BRAS  RE,REPORT           NO RETURN FROM ERROR REPORT                  
*                                                                               
DOMK40   XC    BMGEERR,BMGEERR     CLEAR THE ERROR BUFFER                       
         BRAS  RE,MINOTO           MINUS OTO THE MISSED SPOTS                   
                                                                                
*============================================================                   
* PUT COMMENTS FROM NOTICE RECORD INTO MISSED BUYLINES                          
*============================================================                   
                                                                                
DOMK50   DS    0H                                                               
         BRAS  RE,MICOM                                                         
                                                                                
*========================================================                       
* NOW UPDATE NOTICE RECORD STATUS AND GENERATE REPORT                           
*========================================================                       
                                                                                
DOMK60   MVI   SVMGINIT,C'N'       TELL BUY PROGRAM TO REBUILD TABLES           
         XC    MYSTTN,MYSTTN       WANT FIRST NOTICE REC FOR CABLE              
         BRAS  RE,READMKN                                                       
*                                                                               
         MVI   ELCODE,MNSTELQ      FIND FIRST STATUS ELEMENT                    
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
****     BRAS  RE,FIRSTEL          POINT R6 TO FIRST STATUS ELEMENT             
         BRAS  RE,GETEL                                                         
         JNE   *+2                                                              
*                                                                               
         MVI   BYTE,MNSTOKAY       SET STATUS TO OK                             
         TM    MGMSCFLG,MGMFSAPP   SELF APPLY?                                  
         BZ    *+8                                                              
         MVI   BYTE,MNSTSAPP                                                    
         BRAS  RE,BLDSTAT          BUILD AND INSERT STATUS ELEMENT              
*                                                                               
         MVC   AREC,AREC2                                                       
         BRAS  RE,MYPUTREC                                                      
*                                                                               
         TM    MGMSCFLG,MGMFSAPP   SELF APPLY?                                  
         BO    *+8                 YES, ONLY GENERATE ON THE OKAY               
         BRAS  RE,REPORT           GENERATE REPORT                              
*                                                                               
YES      CR    RB,RB               SET CC =                                     
         J     EXIT                                                             
*                                                                               
NO       LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*================================================================               
* USE C,MG=JAN01-3 TO MISS THIS SPOT AND CREATE A MAKEGOOD GROUP                
*================================================================               
         SPACE 1                                                                
DOGRP    NTR1  LABEL=*,WORK=(R7,DGWORKL)                                        
         USING DGWORKD,R7                                                       
         XC    MYSTTN,MYSTTN       CLEAR SAVED STATION                          
         LA    R2,BUYINP1H                                                      
         XC    BUYINP1,BUYINP1                                                  
*                                                                               
         L     R6,AREC3                                                         
         USING MOKEY,R6                                                         
****     AH    R6,DSPFRST                                                       
*                                                                               
         MVI   ELCODE,MNUMELQ      FIND MAKEGOOD BUY ELEMENT                    
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         JNE   *+2                                                              
         USING MNUMELD,R6                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MNUMNUM        BUYLINE NUMBER                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYINP1(3),DUB                                                   
         MVI   4(R2),0                                                          
         MVI   5(R2),3             SET INPUT LENGTH                             
*                                                                               
DOGRP10  XC    BUYINP2,BUYINP2                                                  
         LA    R2,BUYINP2                                                       
         MVC   0(5,R2),=C'C,MG='                                                
         LA    R2,5(R2)                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MSACTBUY                                                    
         JZ    *+2                                                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
         LA    R2,3(R2)                                                         
*                                                                               
DOGRP15  GOTO1 VDATCON,DMCB,(2,MSACTDAT),(4,(R2))                               
         MVI   5(R2),C'-'                                                       
         LA    R2,6(R2)                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,MSACTEL                                                     
         JZ    *+2                                                              
         CVD   R0,DUB              SPOT MUST HAVE BEEN FOUND                    
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         LA    R2,2(R2)                                                         
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    DOGRP17             NO                                           
* UPDATE THE STATION FIELD FOR CABLE IF IT HAS CHANGED                          
         CLC   MYSTTN,MSSTTN       TEST SAME AS PREVIOUS                        
         BE    DOGRP16                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MSSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVC   MYSTTN,MSSTTN       AND SAVE THE STATION!                        
         MVC   DGNETWK,WORK+20                                                  
*                                                                               
DOGRP16  CLC   BUYST+5(3),DGNETWK   MATCH IN NETWORK?                           
         BE    DOGRP17                                                          
         MVI   0(R2),C'/'                                                       
         MVC   1(3,R2),DGNETWK                                                  
         LA    R2,4(R2)                                                         
*                                                                               
DOGRP17  LA    R0,BUYINP2                                                       
         SR    R0,R2                                                            
         LPR   R0,R0                                                            
*                                                                               
         LA    R2,BUYINP2H                                                      
         MVI   4(R2),0                                                          
         STC   R0,5(R2)                                                         
* CALL BUYBASE TO CREATE MAKEGOOD GROUP                                         
         BRAS  RE,CALLBASE                                                      
* TEMP TRAP CODE                                                                
         CLI   MSELEMNO,2                                                       
         JE    *+4                                                              
* TEMP TRAP CODE                                                                
         OC    BMGEERR,BMGEERR     ERRORS ARE NOT ACCEPTABLE !                  
         BZ    DOGRP20                                                          
         BRAS  RE,REPORT           NO RETURN FROM ERROR REPORT                  
*                                                                               
* UNLOCK ISSUED TO GUARD AGAINST VERY LARGE MAKEGOODS                           
*                                                                               
DOGRP20  CLI   BUYST,C'0'          ARE WE CABLE?                                
         B     DOGRP25             **** DON'T DO THE UNLOCK ****                
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTFILE'                             
*                                                                               
DOGRP25  MVI   T.TSACTN,TSANXT     LOOK FOR MORE MISSED SPOTS                   
         BRAS  RE,CALLTSAR                                                      
         BNE   DOGRP40             NO MORE ELEMS - WRITE RECORD                 
*                                                                               
         CLI   MSTYPE,MSTYPEQ                                                   
         BE    DOGRP10                                                          
                                                                                
*==================================================================             
* NO MORE MISSED SPOTS - ADD OTHER BUYLINES TO THIS GROUP                       
* READ THROUGH THE OFFER RECS AND SET BDMGDATE IN NEW BUYS                      
*==================================================================             
         CLI   BUYST,C'0'          ARE WE CABLE?                                
         BNL   DOGRP50             YES, NEED TO READ THE OFFER RECORD           
*                                                                               
DOGRP30  MVC   MNUMCD,BMGEMGCD     UPDATE THE MAKEGOOD GRP CODE                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+6(3),MOSTTN     MOVE STATION                                 
         MVC   KEY+11(2),MNUMNUM   BUYLINE                                      
*                                                                               
DOGRP35  GOTO1 HIGH                READ FOR BUYLINE                             
         CLC   KEY(13),KEYSAVE     BUY MUST BE PRESENT!                         
         JNE   *+2                                                              
*                                                                               
         MVC   AREC,AREC1          READ BUY INTO AREC1                          
         GOTO1 GETREC                                                           
         MVC   BDMGDATE,BMGEMGCD   MOVE THE MAKEGOOD CODE IN BUYLINE            
         GOTO1 PUTREC                                                           
*                                                                               
         BRAS  RE,NEXTEL           ANYMORE X'80' IN OFFER REC?                  
         BE    DOGRP30             YES, WE HAVE ANOTHER MKGD BUYLN EL           
         DROP  R6                                                               
                                                                                
*=================================================================              
* WRITE OFFER RECORD BACK WHEN NO MORE BUYLINE ELEMS                            
*=================================================================              
                                                                                
DOGRP40  MVI   USEMYIO,C'Y'                                                     
         MVC   AREC,AREC1          READ OFFER REC TO AREC1                      
         BRAS  RE,READMKO                                                       
*                                                                               
         L     R6,AREC3            AND WRITE FROM AREC3!                        
         ST    R6,AREC                                                          
*                                                                               
         BRAS  RE,MYPUTREC                                                      
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         JL    EXIT                                                             
                                                                                
* PROCESS OFFER FROM NEXT STATION                                               
                                                                                
DOGRP45  BRAS  RE,CALLTSAR         GET NEXT OFFER FROM TSAR                     
         JNE   EXIT                                                             
                                                                                
         CLC   MYSTTN,MOSTTN       SAME AS LAST NETWORK?                        
         BE    DOGRP45             YES, SKIP IT & DON'T PROCESS AGAIN           
DOGRP50  MVC   MYSTTN,MOSTTN                                                    
         BRAS  RE,READMKO          GET THE RIGHT OFFER RECORD!                  
*                                                                               
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
****     BRAS  RE,FIRSTEL          FIND MAKEGOOD BUY ELEM                       
         BRAS  RE,GETEL            FIND MAKEGOOD BUY ELEM                       
         BE    DOGRP30                                                          
         DC    H'0'                                                             
         DROP  R7                                                               
*                                                                               
DGWORKD  DSECT                   ** DOGRP LOCAL WORKING STORAGE **              
DGNETWK  DS    CL3                                                              
DGWORKL  EQU   *-DGWORKD                                                        
T21132   CSECT                                                                  
         EJECT                                                                  
*==============================================================                 
* BUILD THE SKED TABLE                                                          
*==============================================================                 
         SPACE 1                                                                
BLDSKED  NTR1                                                                   
*                                                                               
* GET TABLE START DATE                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MOSTRT),SKDSTART                                 
                                                                                
BLDSK10  BRAS  RE,CALLTSAR                                                      
         BNE   BLDSK20                                                          
*                                                                               
         CLI   MOTYPE,MOTYPEQ      OFFER TYPE?                                  
         BNE   BLDSK20                                                          
         CLC   MOSTTN,MYSTTN       TEST SAME STATION                            
         BNE   BLDSK20             NO - DONE, EXIT OUT                          
         CLC   MORECID,SKDMKID     STILL SAME OFFER/REC                         
         BNE   BLDSK20             NO - DONE, EXIT OUT                          
         CLI   MOCOMNUM,0          TEST COMMENT OR ORBIT                        
         BNE   BLDSK10             YES - SKIP IT                                
         GOTO1 VDATCON,DMCB,(8,MOSTRT),WORK                                     
         CLC   WORK,SKDSTART       EARLIER THAN SKDSTART?                       
         BNL   BLDSK10              NO                                          
         MVC   SKDSTART,WORK        YES, HAVE NEW SKDSTART                      
         B     BLDSK10                                                          
*                                                                               
BLDSK20  MVC   T.TSRNUM,SVTSRNUM   REREAD FIRST TSAR REC FOR GRP                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         MVI   T.TSACTN,TSANXT                                                  
         B     BLDSK65                                                          
*                                                                               
         SR    R4,R4               SET DSPL IN TABLE TO 0                       
BLDSK30  LA    R4,SKDTABLE(R4)     POINT TO SLOT IN TABLE                       
         SR    R0,R0                                                            
         ICM   R0,1,MOWKS            GET NUMBER OF WEEKS                        
         JNZ   BLDSK40                                                          
         LHI   R0,1       *BUG* NCC SENT OFFER W/0 WEEKS  DSSUP-5226            
*                               *** DIE IF IT HAPPENS AGAIN - HWON ***          
         MVC   BMGEERR,=Y(ZWMGOFFR)                                             
         BRAS  RE,REPORT             SHOULD NOT RETURN FROM REPORT              
         DC    H'0'                  BUT JUST TO MAKE SURE                      
*                                                                               
BLDSK40  LLC   RE,0(R4)            GET CURRENT TOTAL SPOTS/WEEK                 
         LLC   RF,MONPW            GET ADDITIONAL SPOTS/WEEK                    
         AR    RE,RF               ADD THEM TOGETHER                            
         STC   RE,0(R4)            PUT NEW TOTAL SPOTS/WEEK                     
***      MVC   0(1,R4),MONPW       SET SPOTS/WEEK                               
*                                                                               
         LA    R4,1(R4)                                                         
         BCT   R0,BLDSK40                                                       
*                                                                               
BLDSK50  BRAS  RE,CALLTSAR         GET NEXT SCHEDULE ITEM                       
         BE    BLDSK60                                                          
         MVI   MOTYPE,X'FF'        SET EOF FLAG                                 
*                                                                               
BLDSK60  CLI   MOTYPE,MOTYPEQ                                                   
         BNE   BLDSK90                                                          
         CLC   MOSTTN,MYSTTN       TEST SAME STATION                            
         BNE   BLDSK90             NO                                           
         CLC   MORECID,SKDMKID     STILL SAME OFFER/REC                         
         BNE   BLDSK90             NO - ADD THIS BUY                            
         CLI   MOCOMNUM,0          TEST COMMENT OR ORBIT                        
         BNE   BLDSK50             YES - SKIP                                   
*                                                                               
* NEED TO FIND HOW MANY WEEKS INTO TABLE TO BEGIN                               
*                                                                               
BLDSK65  LA    R4,0                            COUNTER STARTS AT 0              
         MVC   WORK(6),SKDSTART                TABLE START DATE                 
         MVC   WORK+6(6),WORK                                                   
         GOTO1 VDATCON,DMCB,(8,MOSTRT),WORK+12 NEWREC START DATE                
         B     BLDSK80                                                          
*                                                                               
BLDSK70  GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
BLDSK80  CLC   WORK+6(6),WORK+12         TEST REACHED START                     
         BE    BLDSK30                   YES - MOVE SPOTS TO TABLE              
         JNL   *+2                                                              
         MVC   WORK(6),WORK+6                                                   
         AHI   R4,1                      INCREMENT COUNTER                      
         B     BLDSK70                                                          
         SPACE 1                                                                
*===========================================================                    
* FIND NUMWEEKS BETWEEN FIRST/LAST NPW VALUES                                   
*===========================================================                    
         SPACE 1                                                                
BLDSK90  LA    R1,SKDTABLE+52      POINT TO END OF TABLE                        
         CLI   0(R1),0             FIND A NON-ZERO VALUE                        
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,SKDTABLE-1                                                    
         SR    R1,R0                                                            
         STC   R1,SKDNUM           SAVE NUMBER OF ACTIVE ENTRIES                
*                                                                               
* FIND OUT IF THERE IS MORE THAN ONE VALUE AND IF SO                            
* COUNT HOW MANY TIMES EACH VALUE APPEARS                                       
*                                                                               
         XC    ELEM,ELEM           CLEAR COUNTER TABLE                          
         LA    R1,SKDTABLE                                                      
         SR    R0,R0                                                            
         IC    R0,SKDNUM                                                        
         SR    RF,RF                                                            
*                                                                               
BLDSK100 SR    RE,RE                                                            
         IC    RE,0(R1)            GET NPW VALUE                                
         LA    RE,ELEM(RE)         POINT TO COUNTER                             
         IC    RF,0(RE)            GET CURRENT COUNT FOR VALUE                  
         AHI   RF,1                                                             
         STC   RF,0(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,BLDSK100                                                      
*                                                                               
         LA    R1,ELEM                                                          
         SR    R0,R0                                                            
         MVI   SKDHICNT,0          NUM TIMES SKDHINPW OCCURS                    
         MVI   SKDHINPW,0          MOST FREQUENT NPW                            
         MVI   SKDCOUNT,0          NUMBER OF DIFFERENT NPW VALUES               
*                                                                               
BLDSK110 CLI   0(R1),0             TEST VALUE PRESENT                           
         BE    BLDSK120                                                         
         IC    RF,SKDCOUNT                                                      
         AHI   RF,1                                                             
         STC   RF,SKDCOUNT         BUMP NON-ZERO COUNTER COUNT                  
*                                                                               
BLDSK120 CLC   0(1,R1),SKDHICNT    NEW COUNTER TO HIGH SO FAR                   
         BNH   BLDSK130            NOT MORE                                     
*                                                                               
         LTR   R0,R0               DO NOT ALLOW 0 NPW TO WIN !                  
         BZ    BLDSK130                                                         
         MVC   SKDHICNT,0(R1)      SAVE NEW HIGH COUNTER                        
         STC   R0,SKDHINPW         AND THE ASSOCIATED NPW                       
*                                                                               
BLDSK130 LA    R1,1(R1)                                                         
         AHI   R0,1                                                             
         CHI   R0,99                                                            
         BNH   BLDSK110                                                         
* MAKE SURE THERE ARE NOT MORE THAN MAXIMUM SPOTS                               
         LHI   R0,53                                                            
         LA    R1,SKDTABLE                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
*                                                                               
BLDSK140 IC    RE,0(R1)                                                         
         AR    RF,RE                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,BLDSK140                                                      
****     CLM   RF,1,SVMAXSPT       *NOP* RF MAY BE >255                         
         LLC   RE,SVMAXSPT         RE = MAX SPOTS                               
         CR    RF,RE               COMPARE REGISTER RF AGAINST RE               
         JNH   EXIT                                                             
*                                                                               
         MVC   BMGEERR,=Y(BADSPTS)   SET FOR ERROR REPORT                       
         BRAS  RE,REPORT             SHOULD NOT RETURN FROM REPORT              
         DC    H'0'                  BUT JUST TO MAKE SURE                      
         EJECT                                                                  
*==============================================================                 
* BUILD THE BUY INPUT STRING                                                    
*==============================================================                 
         SPACE 1                                                                
BLDBUY   NTR1                                                                   
*                                                                               
         LA    R2,BUYINP1H         BUY INPUT LINE                               
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         OI    6(R2),X'80'         TRANSMIT                                     
         XC    BUYINP1,BUYINP1                                                  
         CLI   BUYST,C'0'                                                       
         BL    BLDB0                                                            
* UPDATE THE STATION FIELD FOR CABLE                                            
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),SKDSTTN                                                
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVI   WORK+19,C'/'                                                     
         LA    R2,BUYSTH                                                        
         MVC   8(8,R2),WORK+15                                                  
         MVI   4(R2),0             INVALIDATE INPUT                             
         MVI   5(R2),8             SET LENGTH                                   
         OI    6(R2),X'80'         SET XMT                                      
                                                                                
*=========================================================                      
* SET REASON CODE IN BUYSAVE                                                    
*=========================================================                      
                                                                                
BLDB0    MVI   ELCODE,MORSNELQ                                                  
         BRAS  RE,GETMKEL                                                       
         BNE   BLDB2                                                            
         USING MORSNELD,R6                                                      
*                                                                               
         LHI   R7,SVRSNEL-BUYSAVE                                               
         AR    R7,RA                                                            
         XC    0(69,R7),0(R7)      CLEAR PREVIOUS                               
*                                                                               
         MVI   0(R7),X'90'         SET ELEMENT CODE                             
         MVI   1(R7),RCELLENQ      SET ELEM LEN W/O TEXT                        
         MVC   2(6,R7),MORSNCOD    MOVE THE REASON CODE                         
         OC    2(6,R7),SPACES                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-MORSNOVR                                                     
         BNP   BLDB1X              ZERO IF NO TEXT                              
*                                                                               
         LHI   RF,SVRSNTXT-BUYSAVE                                              
         AR    RF,RA                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),MORSNTXT                                                 
* UPDATE ELEMENT LENGTH                                                         
         LHI   RF,SVRSNEL-BUYSAVE                                               
         AR    RF,RA                                                            
         IC    R0,1(RF)            GET CURRENT LENGTH                           
         AR    R0,RE                                                            
         AHI   R0,1                                                             
         STC   R0,1(RF)            SET LEN INCLUDING TEXT                       
         DROP  R6                                                               
*                                                                               
BLDB1X   MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         OI    6(R2),X'80'         TRANSMIT                                     
         XC    BUYINP1,BUYINP1                                                  
*                                                                               
BLDB2    CLI   SVTRDTYP,C'R'       TEST SPECIAL REP ACTIVE                      
         BNE   BLDB4                                                            
         MVC   SVOPTREP,SVTRDDTA   SPBUY04 WILL LOOK HERE                       
*                                                                               
BLDB4    LA    R2,BUYINP2H         BUY INPUT LINE                               
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         OI    6(R2),X'80'         TRANSMIT                                     
         XC    BUYINP2,BUYINP2                                                  
*                                                                               
         LA    R2,BUYINP1                                                       
*                                                                               
         MVC   0(2,R2),=C'B,'      ADD NEW BUY                                  
         LA    R2,2(R2)                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,SKDSTART,(4,(R2))   DATE AS MMMDD                   
         MVI   5(R2),C'-'                                                       
         LA    R2,6(R2)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SKDNUM           NUMBER OF WEEKS START-END                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVC   2(2,R2),=C'W,'                                                   
         LA    R2,4(R2)                                                         
*&&DO                                                                           
* REMOVED BECAUSE IT SHOULD ALWAYS START ON MONDAY!!!                           
         SR    R1,R1                                                            
         IC    R1,MOOOW            OUT-OF-WEEK START DAY                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
*&&                                                                             
* FOLLOWING INSTRUCTION NOP'D SO GET SHORT FORM MTWTFSS IF TOO LONG             
**NOP**  OI    BYTE,X'80'          11 BYTE OUTPUT                               
         XC    WORK,WORK                                                        
         GOTO1 VDAYUNPK,DMCB,(0,MODAYS),WORK                                    
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,11                                                            
*                                                                               
BLDB10   CLI   0(R1),C','          CHANGE COMMAS TO SLASHES                     
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,BLDB10                                                        
*                                                                               
         MVC   0(11,R2),WORK                                                    
*                                                                               
         LA    R2,11(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C','          ADD COMMA                                    
         LA    R2,2(R2)            BUMP PAST                                    
                                                                                
* TEST TO SEE IF HINPW*NUMWEEKS GREATER THAN MAXSPOTS                           
* IF SO, SET HINPW TO 1 !                                                       
                                                                                
         SR    R0,R0                                                            
         IC    R0,SKDHINPW                                                      
         SR    R1,R1                                                            
         IC    R1,SKDNUM                                                        
         MR    R0,R0                                                            
**NOP**  CLM   R1,1,SVMAXSPT       *NOP* R1 MAY BE >255                         
         LLC   RE,SVMAXSPT         RE = MAX SPOTS                               
         CR    R1,RE               COMPARE REGISTER R1 AGAINST RE               
         BNH   *+8                                                              
         MVI   SKDHINPW,1          OVERFLOW, USE 1 NPW                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SKDHINPW         GET MOST FREQ NPW                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C','                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'    GET UNTIME ADDRESS                 
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),MOSTIM,(R2)                                            
*                                                                               
         LA    R2,12(R2)             FIND LAST CHAR IN STRING                   
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(1,R2),MODPT         DAYPART                                    
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MOSLN            SPOT LENGTH                                  
         EDIT  (R0),(3,(R2)),ALIGN=LEFT                                         
         AR    R2,R0               OUTPUT LENGTH                                
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         LA    RE,MOPROG           COMMAS IN PROGRAM NAME ARE DEATH!            
         LHI   RF,17                                                            
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         MVC   0(17,R2),MOPROG     MOVE PROGRAM NAME                            
*                                                                               
         LA    R2,17(R2)           POINT TO END OF PROGRAM                      
         CLI   0(R2),X'40'         FIND LENGTH OF NAME                          
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
         CLI   SVCPROF+9,C'0'      TEST ADJ CODE REQUIRED                       
         BE    BLDB14                                                           
         MVC   0(1,R2),MOADJ       MOVE FIRST CHAR                              
         OI    0(R2),X'40'         MAKE SURE IT'S UPPERCASE                     
*                                                                               
         CLI   SVCPROF+9,C'2'      TEST ALPHA ADJ                               
         BNE   BLDB12                                                           
         SR    R0,R0                                                            
         IC    R0,MOADJ                                                         
         SRDL  R0,4                                                             
         STC   R0,0(0,R2)                                                       
         OI    0(R2),X'F0'                                                      
         SRL   R1,28                                                            
         STC   R1,1(0,R2)                                                       
         OI    1(R2),X'F0'                                                      
         LA    R2,1(R2)                                                         
*                                                                               
BLDB12   LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
BLDB14   TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    BLDB14C             NO                                           
         CLI   SVTRDTYP,C'R'       IS THIS A TRADE ORDER?                       
         BNE   BLDB14C             NO                                           
         CLI   SVDARPRF+15,C'Y'    ADD COMM RATE TYPE FOR TRADE?                
         BNE   BLDB14A                                                          
         MVI   0(R2),C'C'          YES, USE COMMISSION RATE TYPE                
         LA    R2,1(R2)                                                         
BLDB14A  SR    R0,R0               SWITCH COST AND COS2                         
         ICM   R0,7,MOCOS2                                                      
         EDIT  (R0),(8,(R2)),2,ALIGN=LEFT                                       
         AR    R2,R0               ADJUST STRING LEN                            
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                ADJUST STRING LEN                            
         B     BLDB14G                                                          
*                                                                               
BLDB14C  CLI   SVDARPRF+15,C'Y'    ADD COMM RATE TYPE FOR TRADE?                
         BNE   BLDB14G                                                          
         CLI   SVTRDTYP,C'R'       IS THIS A TRADE ORDER?                       
         BNE   BLDB14G             NO                                           
         MVI   0(R2),C'C'          YES, USE COMMISSION RATE TYPE                
         LA    R2,1(R2)                                                         
*                                                                               
BLDB14G  SR    R0,R0                                                            
         ICM   R0,7,MOCOST                                                      
         EDIT  (R0),(10,(R2)),2,ALIGN=LEFT                                      
         AR    R2,R0               ADJUST STRING LEN                            
         TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    *+12                NO, DISPLAY COS2 IF ANY                      
         CLI   SVTRDTYP,C'R'       IS THIS A TRADE ORDER?                       
         BE    BLDB15              YES, ALREADY SWITCHED                        
         SR    R0,R0               NO, DON'T SWITCH                             
         ICM   R0,7,MOCOS2                                                      
         BZ    BLDB15                                                           
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                                                             
         EDIT  (R0),(8,(R2)),2,ALIGN=LEFT                                       
         AR    R2,R0                                                            
*                                                                               
BLDB15   CLC   =C'POL',BUYPR                                                    
         BNE   BLDB20                                                           
         MVC   0(3,R2),=C',M='                                                  
         MVC   3(3,R2),MGQPRD1                                                  
         LA    R2,5(R2)            POINT TO LAST CHAR OF PRD                    
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   MGQPRD2,C' '        TEST PIGGYBACK                               
         BNH   BLDB20              NO                                           
         MVI   0(R2),C'-'                                                       
         MVC   1(3,R2),MGQPRD2                                                  
         LA    R2,4(R2)                                                         
*                                                                               
BLDB20   NI    WRKRUPSW,X'FF'-WRKRUPSW_NODMER                                   
*                                                                               
         LA    RE,SVDEMOS          RE = A(POL DEMOS IN DISPLAYED ORDER)         
         OC    SVBRDEMS,SVBRDEMS   HAVE BRAND DEMO LIST?                        
         BZ    *+8                  NO                                          
         LA    RE,SVBRDEMS         RE = A(BRD DEMOS IN DISPLAYED ORDER)         
*                                                                               
         LA    RF,MODEM1           RF = A(MODEM LIST)                           
         LA    R0,MOPROG-MODEM1                                                 
         SRL   R0,2                R0 = (LOOP FOR NUM OF MODEMS)                
*                                                                               
BLDB23   OC    0(3,RE),0(RE)       ANY MORE DEMOS?                              
         BZ    BLDB27               NO                                          
         CLI   2(RE),0             NSI DEMO?                                    
         BE    BLDB25               NO, SKIP IT                                 
         TM    0(RF),X'80'         HAVE OVERRIDE?                               
         BZ    BLDB30               NO, DONE                                    
*                                                                               
BLDB25   LA    RF,L'MODEM1(RF)     NEXT DEMO IN MODEMS                          
         LA    RE,3(RE)            NEXT DEMO IN EST DEMO LIST                   
         BCT   R0,BLDB23                                                        
*                                                                               
BLDB27   OI    WRKRUPSW,WRKRUPSW_NODMER                                         
*                                                                               
BLDB30   OI    BUYINP1H+4,X'80'    FIELD INPUT THIS TIME                        
         LA    R1,BUYINP1                                                       
         SR    R2,R1                                                            
         STC   R2,BUYINP1H+5       SET FIELD LENGTH                             
*                                                                               
         CLI   SKDCOUNT,1          SHOULD WE SKED                               
         BNH   BLDB36              NO                                           
*                                                                               
         BRAS  RE,BLDBSKED                                                      
*                                                                               
*===========================================================                    
* ADD THE BUY RECORD                                                            
*===========================================================                    
                                                                                
BLDB36   BRAS  RE,CALLBASE                                                      
         OC    BMGEERR,BMGEERR                                                  
         BZ    BLDB38                                                           
         BRAS  RE,REPORT           SHOULD NOT RETURN                            
         DC    H'0'                                                             
*                                                                               
E        USING MNUMELD,ELEM                                                     
*                                                                               
BLDB38   XC    ELEM,ELEM           CREATE BUYLINE NUMBER ELEMENT                
         MVI   E.MNUMEL,MNUMELQ                                                 
         MVI   E.MNUMLEN,MNUMLNQ                                                
         MVC   E.MNUMOFR(3),MORECID    OFFER#/REC#/SEQ#                         
         MVI   E.MNUMSTAT,MNUMBUY      SET BUY MADE                             
         MVC   E.MNUMNUM(2),BMGELINE   SET BUYLINE NUMBER                       
*                                                                               
BLDB40   BRAS  RE,ADDMKEL                                                       
         DROP  E                                                                
         J     EXIT                                                             
         EJECT                                                                  
*==========================================================                     
* BUILD SKED= STRING BEGINNING ON BUY INPUT LINE 2                              
*==========================================================                     
BLDBSKED NTR1                                                                   
         LA    R2,BUYINP2H         R2 = A(BUY INPUT HEADER)                     
         LA    R4,SKDTABLE         R4 = A(53 WEEK SKED TABLE)                   
         LLC   R5,SKDNUM           R5 = NUMBER OF ACTIVE WEEKS                  
*                                                                               
         USING FLDHDRD,R2                                                       
BLDBSK10 MVI   FLDIIND,X'80'       FIELD INPUT THIS TIME                        
         MVI   FLDILEN,0           RESET INPUT DATA LENGTH                      
         OI    FLDOIND,X'80'       TRANSMIT                                     
*                                                                               
         LA    RE,FLDDATA-FLDHDRD  GET L'HEADER                                 
         TM    FLDATB,FATBXHDR     HAVE X'02' EXTENDED FIELD?                   
         BZ    *+8                  NO                                          
         LA    RE,FLDXDATA-FLDHDRD  YES, GET L'HEADER+L'EXT                     
*                                                                               
         LA    R6,0(RE,R2)         POINT R6 = A(START OF INPUT LINE)            
*                                                                               
         LLC   RF,FLDLEN           GET L'HDR+L'EXT+L'FIELD                      
         SR    RF,RE               MINUS L'HDR+L'EXT, HAVE VALID LEN?           
         JNP   *+2                  NO, SOMETHING WRONG                         
         BCTR  RF,0                                                             
         XC    0(0,R6),0(R6)       CLEAR THE BUY INPUT LINE                     
         EX    RF,*-6                                                           
*                                                                               
         LA    R0,7                R0 = DEFAULT L'OUTPUT                        
         MVC   0(7,R6),=C'C,SKED='                                              
*                                                                               
         LLC   RE,SKDNUM           GET NUMBER OF ACTIVE WEEKS                   
         SR    RE,R5               ARE WE PROCESSING 1ST WEEK?                  
         JZ    BLDBSK20             YES, WE ARE                                 
         AHI   RE,1                 NO, ADD 1 TO GET CORRECT WEEK INDEX         
         LA    R6,6(R6)            ADD L'OUTPUT, "C'SKED", TO A(INPUT)          
*                                                                               
         EDIT  (RE),(3,(R6)),ALIGN=LEFT,ZERO=NOBLANK,TRAIL=C'='                 
BLDBSK20 AR    R6,R0               ADD L'OUTPUT TO A(INPUT)                     
*                                                                               
BLDBSK30 LLC   RF,FLDLEN           GET L'HDR+L'EXT+L'FIELD                      
         LAY   RF,-BLDBBUF(R2,RF)  RF = A(END ON INPUT FIELD MINUS 3)           
         CR    R6,RF               BEYOND END ON INPUT FIELD MINUS 3?           
         BNH   BLDBSK40             NO                                          
         BRAS  RE,BLDBSKUP          YES,  REM LAST '/' & UPDATE FLDLEN          
*                                                                               
         LLC   RF,FLDLEN           GET L'HDR+L'EXT+L'FIELD                      
         LA    R2,0(RF,R2)         R2 = A(NEXT BUY INPUT HEADER)                
         DROP  R2                                                               
*                                                                               
         LA    RF,BUYINP4                                                       
         CR    R2,RF               CHECK RAN OUT OF 4 INPUT LINES               
         JL    BLDBSK10             NO, STILL HAVE SPACE                        
         DC    H'0'                 YES, HAVE PROBLEM                           
*                                                                               
BLDBSK40 CLC   SKDHINPW,0(R4)      MATCH NPW WE USED                            
         BNE   BLDBSK50             NO                                          
         MVI   0(R6),C'/'           YES, SKIP WEEK AND PUT /                    
         LHI   R0,1                 BUMP BY 1                                   
         B     BLDBSK60                                                         
*                                                                               
BLDBSK50 LLC   R0,0(R4)            GET SKED NUMBER                              
         EDIT  (R0),(3,(R6)),ALIGN=LEFT,ZERO=NOBLANK,TRAIL=C'/'                 
*                                                                               
BLDBSK60 AR    R6,R0               ADD LENGTH OF OUTPUT                         
         LA    R4,1(R4)            NEXT IN TABLE                                
         BCT   R5,BLDBSK30                                                      
*                                                                               
         BRAS  RE,BLDBSKUP         REM LAST '/' & UPDATE FLDLEN                 
         J     EXIT                                                             
*                                                                               
****************************************                                        
*  REMOVE TRAILING / AND UPDATE FLDLEN *                                        
****************************************                                        
BLDBSKUP DS    0H                                                               
         BCTR  R6,0                                                             
         MVI   0(R6),C' '          REMOVE TRAILING /                            
*                                                                               
         LA    RF,FLDDATA-FLDHDRD(0,R2)                                         
         TM    FLDATB-FLDHDRD(R2),FATBXHDR X'02' - EXTENDED FIELD?              
         BZ    *+8                                                              
         LA    RF,FLDXDATA-FLDDATA(0,R2)                                        
         SR    R6,RF               GET THE LENGTH OF INPUT                      
*                                                                               
         STC   R6,FLDILEN-FLDHDRD(R2)                                           
         BR    RE                                                               
BLDBBUF  EQU   3                   3 CHARACTER END OF LINE BUFFER               
*==========================================================                     
* UPDATE DARE TRACE ELEMENT WITH ORD/BYR/GRP/OFFER DETAILS                      
*==========================================================                     
         SPACE 1                                                                
BLDDAREL NTR1                                                                   
         MVI   ELCODE,BDARCODQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         USING BDARELEM,R6                                                      
         MVC   BDARORD,MGORDERQ                                                 
         MVC   BDARBYR,BUYBU                                                    
         MVC   BDARMKGP,MGGRPCOD                                                
         MVC   BDARFLT,MGFLTCOD                                                 
         MVC   BDARSEQN,MORECID    OFFER/REC/SEQNUM                             
         J     EXIT                                                             
         EJECT                                                                  
*==========================================================                     
* PROCESS BUY DEMO ELEMENT                                                      
* - ONLY COPY NIELSEN DEMO OVERRIDES                                            
* - COPY ALL COMSCORE DEMO VALUE AND UPDATE X'50' NTDELEM                       
*==========================================================                     
         SPACE 1                                                                
BLDDEM   NTR1                                                                   
*                                                                               
* PROCESS MKGD NIELSEN DEMO OVERRIDES                                           
*                                                                               
         MVI   ELCODE,MODMELQ      LOOK FOR X'60' DEMOV ELEMENTS                
         BRAS  RE,GETMKEL                                                       
         BNE   BLDDM50             DID NOT FIND ONE                             
         B     BLDDM20                                                          
*                                                                               
BLDDM10  BRAS  RE,NXTMKEL                                                       
         BNE   BLDDMX                                                           
*                                                                               
         USING MODMELD,R6                                                       
BLDDM20  CLC   SKDMKID,MODMOFFR    MATCH OFFER/REC                              
         BNE   BLDDM10                                                          
* LOOK FOR DEMO OVERRIDES                                                       
         LLC   R7,1(R6)            GET ELEMENT LENGTH                           
         SRL   R7,3                SET FOR NUM DEMS                             
         LA    R6,4(R6)                                                         
*                                                                               
BLDDM30  TM    4(R6),X'80'         TEST OVERRIDE PRESENT                        
         BZ    *+8                                                              
         BAS   RE,SET02DEM         SET VALUE IN BUY 02 DEMEL                    
*                                                                               
         LA    R6,8(R6)            NEXT ENTRY IN X'60'                          
         BCT   R7,BLDDM30          DO FOR NUM DEMS                              
*                                                                               
* PROCESS MKGD NON-TRAD DEMO                                                    
*  & UPDATE BUY X'50' NTDELEM FLAG FIELD                                        
*                                                                               
BLDDM50  MVI   ELCODE,MONTELQ      LOOK FOR X'5E' DEMOV ELEMENTS                
         BRAS  RE,GETMKEL                                                       
         BNE   BLDDMX              DIDN'T FIND ONE                              
         B     BLDDM70                                                          
*                                                                               
BLDDM60  BRAS  RE,NXTMKEL                                                       
         BNE   BLDDMX                                                           
*                                                                               
         USING MONTELD,R6                                                       
BLDDM70  CLC   SKDMKID,MONTOFFR    MATCH OFFER/REC                              
         BNE   BLDDM60                                                          
* LOOK FOR DEMO OVERRIDES                                                       
         SR    R0,R0                                                            
         LLC   R1,MONTLEN          GET ELEMENT LENGTH                           
         D     R0,=A(L'MONTDEMO)   SET FOR NUM DEMS                             
         LA    R6,MONTDEMO         POINT TO DEMO ENTRY                          
*                                                                               
BLDDM80  BRAS  RE,UPNTDMF          UPDATE BUY NON-TRAD FLAGS                    
         BNE   BLDDM90              NON-TRAD DEMO NOT FOUND                     
*                                                                               
         BAS   RE,SET02DEM         SET VALUE IN BUY 02 DEMEL                    
*                                                                               
BLDDM90  LA    R6,L'MONTDEMO(R6)   NEXT ENTRY IN X'5E'                          
         BCT   R1,BLDDM80          DO FOR NUM DEMS                              
*                                                                               
BLDDMX   J     EXIT                                                             
*                                                                               
SET02DEM NTR1                                                                   
         LR    R7,R6               SAVE OVERRIDE DEMO ADDR                      
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),NDCORGQ       POINT TO BUYREC X'02 ORIGINAL DEMEL          
         JNE   *+2                                                              
*                                                                               
         IC    R0,1(R6)                                                         
         AHI   R0,-(NDEMNO-NDELEM)                                              
         JNP   *+2                                                              
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,NDEMNO-NDELEM(R6)  R1=A(TO START OF DEMO LIST)                
*                                                                               
SETD2    CLC   0(3,R1),0(R7)       MATCH DEMO CODES                             
         BE    SETD4                                                            
         LA    R1,NDEMLNQ(R1)      CHECK NEXT DEMO                              
         BCT   R0,SETD2                                                         
         J     EXIT                IGNORE IF DEMOS CHANGED                      
*                                                                               
SETD4    MVC   3(5,R1),3(R7)       SET HUT/OVERRIDE VALUE                       
         J     EXIT                                                             
*                                                                               
UPNTDMF  NTR1  ,                   UPDATE NON-TRAD DEMO FLAGS                   
         IC    R1,1(R6)            GET NT-DEMO INDEX                            
         LAY   R2,SVNTDMS                                                       
         OC    0(L'SVNTDMS,R2),0(R2) ESTIMATE HAS NTDEMOS?                      
         BZ    NO                    NO, SKIP IF NTDEMOS REMOVED                
*                                                                               
         LR    R7,R6               SAVE OVERRIDE DEMO ADDR                      
         USING MONTDEMO,R7                                                      
         LA    R6,BDELEM                                                        
UPND10   LLC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO BUYREC DEMEL                        
         CLI   0(R6),0             EOR?                                         
         JE    *+2                  - ELEM REQ'D IF EST HAS NTDEMOS             
*                                                                               
UPND20   CLI   0(R6),NTDELCDQ      X'50' NT DEMO SHORT NAME BUY ELEM            
         JNE   UPND10                                                           
*                                                                               
         IC    R1,1(R7)            GET NT-DEMO INDEX                            
         BCTR  R1,0                                                             
         LR    R2,R1                                                            
         MHI   R2,L'NTDDMONM                                                    
         LAY   R2,SVNTDMS(R2)                                                   
         MHI   R1,NTDDLEN          GET DISPLACEMENT TO INDEX ENTRY              
         LA    R1,NTDOVHDQ(R1,R6)  POINT TO INDEXED ENTRY IN X'50'              
         USING NTDDMONM,R1                                                      
         CLC   NTDDMONM,0(R2)      BUY NTDEMO MATCH TO ESTIMATE?                
         JNE   NO                  NO, SKIP IF NTDEMOS CHANGED                  
*                                                                               
         TM    MONTFLG1,MONT1NLK   X'80' -MKGD DEMO DOES NOT NEED LKUP?         
         JZ    *+8                  NO                                          
         OI    NTDDMOFL,NTDDFDLK    YES, SET X'80 - BUY NT FLAG                 
         TM    MONTFLG1,MONT1NA    X'80' -MKGD DEMO LKUP IS N/A?                
         JZ    *+8                  NO                                          
         OI    NTDDMOFL,NTDDLKNA    YES, SET X'20 - BUY NT FLAG                 
         J     YES                                                              
         DROP  R7,R1                                                            
         EJECT                                                                  
*==========================================================                     
* INSERT COMMENT ELEMENTS IN BUY                                                
*==========================================================                     
         SPACE 1                                                                
BLDCOM   NTR1                                                                   
         MVI   ELCODE,MOBBCELQ     LOOK FOR X'70' COMMENTS                      
         BRAS  RE,GETMKEL                                                       
         BNE   BLDCOMX                                                          
         B     BLDCOM4                                                          
*                                                                               
BLDCOM2  BRAS  RE,NXTMKEL                                                       
         BNE   BLDCOMX                                                          
*                                                                               
         USING MOBBCELD,R6                                                      
BLDCOM4  CLC   SKDMKID,MOBBCOFR    MATCH OFFER/REC                              
         BNE   BLDCOM2                                                          
*                                                                               
         LR    R7,R6               SAVE MKO COMMENT ELEM ADDR                   
*                                                                               
         XC    ELEM,ELEM                                                        
CM       USING COMELEM,ELEM                                                     
         MVI   CM.CMCODE,CMCODEQ   SET X'66' COMMENT ELEM                       
         MVC   CM.CMNUM,MOBBCLNE   SET COMMENT NUMBER                           
         SR    RE,RE                                                            
         IC    RE,MOBBCLEN                                                      
         AHI   RE,-MOBBCOVH        SUB MG OFFER COMMENT OVHD                    
         EX    RE,*+4                                                           
         MVC   CM.CMDATA(0),MOBBCTXT  MOVE COMMENT DATA                         
         AHI   RE,CMDATA-COMELEM   ADD BUY-COMMENT OVHD                         
         STC   RE,CM.CMLEN         AND SET IN ELEMENT                           
         DROP  R6                                                               
*                                                                               
* FIND COMMENT IN BUY RECORD OR ADD IT                                          
         LA    R6,BDELEM                                                        
****     MVI   ELCDLO,X'66'                                                     
****     MVI   ELCDHI,X'66'                                                     
         MVI   ELCODE,CMCODEQ      X'66'                                        
*                                                                               
         USING MOBBCELD,R6                                                      
BLDCOM10 BRAS  RE,NEXTEL                                                        
         BNE   BLDCOM20                                                         
         CLC   CM.CMNUM,CMNUM      MATCH COMMENT NUMBER                         
         BH    BLDCOM10            MUST GO AFTER LOWER COMNUM                   
         BL    BLDCOM20            INSERT BEFORE HIGHER COMNUM                  
         BRAS  RE,DELEL            DELETE EXISTING ELEM                         
*                                                                               
BLDCOM20 BRAS  RE,ADDEL                                                         
         LR    R6,R7               POINT TO MKO COMMENT ELEMENT                 
         B     BLDCOM2                                                          
*                                                                               
BLDCOMX  J     EXIT                                                             
         DROP  CM,R6                                                            
         EJECT                                                                  
*==========================================================                     
* INSERT AUTO AVAIL UUID IN BUY                                                 
*==========================================================                     
         SPACE 1                                                                
BLDAAU   NTR1                                                                   
         MVI   ELCODE,MOMBAELQ     LOOK FOR X'22' ELEMENTS                      
         BRAS  RE,GETMKEL                                                       
         JNE   BLDAAUX                                                          
         J     BLDAAU4                                                          
*                                                                               
BLDAAU2  BRAS  RE,NXTMKEL                                                       
         JNE   BLDAAUX                                                          
*                                                                               
         USING MOMBAAUD,R6                                                      
BLDAAU4  CLC   SKDMKID,MOMBAOFR    MATCH OFFER/REC                              
         JNE   BLDAAU2                                                          
*                                                                               
         LR    R7,R6               SAVE MKO AA UUID ELEM ADDR                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING AAVELEM,R2                                                       
         MVI   AAVCODE,AAVCODQ     X'7A' ELEM CODE                              
         SR    RE,RE                                                            
         IC    RE,MOMBALEN                                                      
         AHI   RE,-(MOMBAOLQ+1)    GET UUID TEXT LEN - 1                        
         EX    RE,*+4                                                           
         MVC   AAVUUID(0),MOMBAAU   MOVE UUID TEXT                              
         AHI   RE,AAVLENQ+1                                                     
         STC   RE,AAVLEN           AND SET ELEMENT LENGTH                       
* FIND UUID IN BUY RECORD TO CHANGE OR ADD IT                                   
         LA    R6,BDELEM                                                        
****     MVI   ELCDLO,X'7A'                                                     
****     MVI   ELCDHI,X'7A'                                                     
         MVI   ELCODE,AAVCODQ      X'7A'                                        
*                                                                               
BLDAAU10 BRAS  RE,NEXTEL                                                        
         JNE   BLDAAU20                                                         
         BRAS  RE,DELEL            DELETE EXISTING ELEM                         
*                                                                               
BLDAAU20 BRAS  RE,ADDEL                                                         
         LR    R6,R7               POINT TO MKO COMMENT ELEMENT                 
         J     BLDAAU2                                                          
*                                                                               
BLDAAUX  J     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
*==========================================================                     
* INSERT ORBIT ELEMENT IN BUY                                                   
*==========================================================                     
         SPACE 1                                                                
BLDORB   NTR1                                                                   
         MVI   ELCODE,MORNELQ      LOOK FOR X'41' ORBIT ELEMENTS                
         BRAS  RE,GETMKEL                                                       
         BNE   BLDORBX                                                          
*                                                                               
         USING ORBELEM,R2                                                       
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ORBCODE,X'67'       SET BUY ORBIT CODE                           
         MVI   ORBLEN,ORBDAY-ORBELEM                                            
*                                                                               
         LA    R4,ORBDAY                                                        
         USING ORBDAY,R4                                                        
         B     BLDORB4                                                          
*                                                                               
BLDORB2  BRAS  RE,NXTMKEL                                                       
         BNE   BLDORB10                                                         
*                                                                               
         USING MORNELD,R6                                                       
BLDORB4  CLC   SKDMKID,MORNOFFR    MATCH OFFER/REC                              
         BNE   BLDORB2                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ORBLEN                                                        
         AHI   R0,16                                                            
         STC   R0,ORBLEN                                                        
         MVC   ORBDAY,MORNDAYS                                                  
         MVC   ORBTIME,MORNSTIM                                                 
         MVC   ORBDESC,MORNPROG                                                 
         MVC   ORBDEM,MORNDEM                                                   
         AHI   R4,16               NEXT ORBIT POSITION                          
         B     BLDORB2                                                          
         DROP  R4                                                               
*                                                                               
BLDORB10 LA    R6,BDELEM                                                        
****     MVI   ELCDLO,X'67'                                                     
****     MVI   ELCDHI,X'67'                                                     
         MVI   ELCODE,X'67'                                                     
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL            DELETE EXISTING ELEMENT                      
*                                                                               
         CLI   ORBLEN,ORBDAY-ORBELEM ORBIT ELEM HAS NO DATA?                    
         BE    BLDORBX             NONE, DON'T WANT ELEM IN BUY REC             
*                                                                               
         BRAS  RE,ADDEL                                                         
*                                                                               
BLDORBX  J     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
*============================================================                   
* SEARCH AREC3/AREC4 FOR ELCODE                                                 
* RETURN WITH CC EQ IF ELEMENT FOUND                                            
*============================================================                   
         SPACE 1                                                                
NXTMKEL  NTR1                                                                   
         BRAS  RE,NEXTEL                                                        
         BE    MKELYES                                                          
         C     R6,AREC4            R6 > AREC4                                   
         BH    MKELNO              --> NO MORE OF THESE ELEMENTS                
         B     MKEL10                                                           
*                                                                               
GETMKEL  NTR1                                                                   
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLC   ELCODE,0(R6)                                                     
         BE    MKELYES                                                          
         BRAS  RE,NEXTEL                                                        
         BE    MKELYES                                                          
*                                                                               
* NOT AREC3. LOOK IN AREC4 IF THERE IS A SECOND RECORD                          
*                                                                               
MKEL10   L     R6,AREC4                                                         
         OC    0(13,R6),0(R6)      IS THERE A RECORD THERE                      
         BZ    MKELNO              NO                                           
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLC   ELCODE,0(R6)                                                     
         BE    MKELYES                                                          
         BRAS  RE,NEXTEL                                                        
         BE    MKELYES                                                          
*                                                                               
MKELNO   LTR   RB,RB                                                            
         B     MKELX                                                            
*                                                                               
MKELYES  CR    RB,RB                                                            
*                                                                               
MKELX    XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*==================================================================             
*==> THIS ROUTINE IS ONLY CALLED TO INSERT BUY REFERENCE ELEMENTS               
*==> IF THE FIRST RECORD GETS FULL, AND THERE IS ALREADY A SECOND               
*==> RECORD,  WE WILL JUST STOP ADDING THEM.                                    
*==> THAT'S BETTER THAN BLOWING UP ON A HUGE MAKEGOOD OFFER                     
*==================================================================             
                                                                                
*==================================================================             
* IF SECOND RECORD EXISTS, IT CONTAINS X70 OR X75 ELEMENTS                      
* IF ADDING AN ELEMENT TO AREC3 AND IT WON'T FIT, THEN THE                      
* RECORDS MUST BE SPLIT. FIRST MOVE AREC3 TO AREC4.                             
*  DELETE ALL X70 OR X75 COMMENTS FROM REC3                                     
*  DELETE ALL BUT X70 OR X75 ELEMENTS FROM REC4                                 
*  REC3 WILL BE PUT BACK TO THE FILE AND                                        
*  REC4 WILL BE ADDED TO THE FILE                                               
*==================================================================             
                                                                                
ADDMKEL  NTR1                                                                   
         L     R7,AREC3            POINT TO FIRST REC                           
*                                                                               
         LR    R6,R7                                                            
****     AH    R6,DSPFRST          POINT TO FIRST ELEMENT                       
         MVI   ELCODE,MOMBELQ      X'20' MAKEGOOD BUY ELEMENT                   
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
ADDMK2   BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         CLC   2(2,R6),ELEM+2      MATCH OFFER/REC                              
         BL    ADDMK2                                                           
         JNE   *+2                                                              
*                                                                               
ADDMK4   SR    R0,R0                                                            
         IC    R0,1(R6)            NOW FIND INSERTION POINT                     
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    ADDMK6                                                           
         CLI   0(R6),MOMBSELQ      X'21' - SKIP THE SUPP ELEMENT                
         BE    ADDMK4                                                           
         CLC   2(2,R6),ELEM+2      SAME OFFER/REC                               
         BNE   ADDMK6              NO - INSERT NOW                              
         CLC   ELEM(1),0(R6)       NEW ELEM TO EXISTING                         
         BNL   ADDMK4              IF NOT LOW, GO ON                            
*                                                                               
ADDMK6   GOTO1 SVRECUP,DMCB,AREC3,ELEM,(C'R',(R6))                              
         ORG   *-2                                                              
         CLI   BUYST,C'0'                                                       
         BL    ADDMK7                                                           
* WE CAN'T USE THE C'T' IN THE 1ST PARAM BECAUSE RECUP WAS NOT CHANGED          
*  TO USE THE MAX RECORD SIZE THAT DMFILTAB HAS FOR XSPFIL                      
         MVI   0(R1),X'FE'                                                      
         LA    RE,XSPREC                                                        
         ST    RE,12(R1)                                                        
ADDMK7   BASR  RE,RF                                                            
         CLI   8(R1),C'R'          TEST OVERFLOW                                
         BE    ADDMKX              NO                                           
* IF RECS SPLIT ALREADY, JUST EXIT                                              
         L     R6,AREC4                                                         
         OC    0(13,R6),0(R6)      GOOD ENOUGH FOR XSPFILE TOO                  
         BNZ   ADDMKX                                                           
                                                                                
*=============================================================                  
* NEED TO SPLIT RECORD                                                          
*=============================================================                  
                                                                                
         XC    KEY,KEY                                                          
         L     R6,AREC3            GET THE MAKEGOOD OFFER KEY                   
         MVC   KEY(13),0(R6)                                                    
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BNL   ADDMK8                                                           
         GOTO1 READ                                                             
         B     ADDMK8X                                                          
*                                                                               
ADDMK8   MVC   WORK(36),0(R6)                                                   
         BRAS  RE,MYHIGH                                                        
         CLC   WORK(36),WORK2      MAKE SURE FOUND RECORD                       
         JNE   *+2                                                              
*                                                                               
ADDMK8X  MVC   AREC,AREC4          NEED THIS FOR THE PUTREC LATER               
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         BRAS  RE,MYGETREC                                                      
****                                                                            
         L     RE,AREC3                                                         
         L     RF,AREC4                                                         
         SR    RF,RE                                                            
         L     R0,AREC4                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE TO REC4 FROM REC3                       
*                                                                               
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
ADDMK10  CLI   0(R6),0                                                          
         BE    ADDMK18                                                          
         CLI   0(R6),MNMBCELQ                                                   
         BE    *+12                                                             
         CLI   0(R6),MNB2CELQ                                                   
         BNE   ADDMK12                                                          
         GOTO1 SVRECUP,DMCB,AREC3,(R6)                                          
         ORG   *-2                                                              
         CLI   BUYST,C'0'                                                       
         BL    *+8                                                              
         MVI   0(R1),C'T'                                                       
ADDMK11  BASR  RE,RF                                                            
         B     ADDMK10                                                          
*                                                                               
ADDMK12  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ADDMK10                                                          
*                                                                               
ADDMK18  MVC   AREC,AREC3          WRITE NEW VERSION OF REC3                    
         GOTO1 PUTREC                                                           
*                                                                               
ADDMK20  L     R6,AREC4                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
ADDMK22  CLI   0(R6),0                                                          
         BE    ADDMK30                                                          
         CLI   0(R6),MNMBCELQ                                                   
         BE    ADDMK24                                                          
         CLI   0(R6),MNB2CELQ                                                   
         BE    ADDMK24                                                          
         GOTO1 SVRECUP,DMCB,AREC4,(R6)                                          
         ORG   *-2                                                              
         CLI   BUYST,C'0'                                                       
         BL    *+8                                                              
         MVI   0(R1),C'T'                                                       
ADDMK23  BASR  RE,RF                                                            
         B     ADDMK22                                                          
*                                                                               
ADDMK24  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ADDMK22                                                          
*                                                                               
ADDMK30  L     R6,AREC4                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLI   0(R6),0             SHOULD HAVE AT LEAST ONE COMMENT             
         BE    ADDMKX              DON'T DIE IF NO COMMENT, JUST EXIT!!         
*        DC    H'0'                                                             
         MVC   AREC,AREC4                                                       
         BRAS  RE,MYADDREC                                                      
         MVC   MGMKODA2,MYDA       SAVE DISK ADDRESS                            
*                                                                               
         MVC   AREC,AREC3          ALL RECORDS READ TO REC3 !                   
         CLI   ELEM,MNMBCELQ       DO WE NEED REC3 OR REC4 ?                    
         BE    ADDMK32             REC4                                         
         CLI   ELEM,MNB2CELQ                                                    
         BE    ADDMK32                                                          
*                                                                               
         MVC   KEY+14(4),MGMKODA1                                               
         MVC   MYDA,MGMKODA1                                                    
*                                                                               
ADDMK32  BRAS  RE,MYGETREC                                                      
*                                                                               
ADDMKX   J     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         LA    RE,64                   TSAR BLOCK + TSARKEY                     
         STC   RE,TSARTRC              SET DATA LENGTH IN BYTE 0                
         GOTO1 VDATAMGR,DMCB,QDMTRACE,QDMDATA,TSARTRC                           
         ORG   *-2                                                              
         DC    X'0700'                                                          
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   T.TSERRS,0              SET CC ON EXIT                           
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
ADDEL    LR    R0,RE                                                            
         GOTO1 SVRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R6))                        
         CLI   DMCB+8,C'R'                                                      
         JE    ADDEL05                                                          
         MVI   ERRCD,MAXSZERR                                                   
         MVI   ERRAREA,X'FE'       SEND ERROR AND UNWIND TRANSACTION            
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR               NO RETURN FROM ERROR                         
         DC    H'0'                                                             
*                                                                               
ADDEL05  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 SVRECUP,DMCB,(C'S',AREC),(R6)                                    
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* NO OFFERS - NEED TO  -OTO THE MISSED SPOTS                                    
* AND UNSET MAKEGOOD PENDING BITS                                               
*=================================================================              
         SPACE 1                                                                
MINOTO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,FRSTMSS                                                 
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
*                                                                               
MIN2     XC    KEY,KEY             READ MISSED BUYLINE REC                      
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN                                                  
         MVC   KEY+11(2),MSACTBUY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1          READ MISSED BUYLINE REC INTO AREC1           
         GOTO1 GETREC                                                           
*                                                                               
****MIN10    L     R6,AREC                                                      
****         USING BUYREC,R6                                                    
MIN10    LA    R6,BDELEM                                                        
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         USING REGELEM,R6                                                       
MIN12    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             EOR?                                         
         JE    *+2                                                              
         CLI   RCODE,RCPOLOQ       X'0B' - POOL ORIGINAL                        
         BL    MIN12                                                            
         CLI   RCODE,RCPOTOQ       X'0C' - POOL OTO                             
         BH    MIN12                                                            
         TM    RSTATUS,RSMINUSQ    X'80' - MINUS SPOT?                          
         BO    MIN12                                                            
*                                                                               
         IC    R0,ELEMNO                                                        
         CLC   RDATE,ELEMDT        SAME DATE?                                   
         BE    *+6                  YES                                         
         SR    R0,R0                NO, RESET SPOT SEQ#                         
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         CLC   MSACTDAT,RDATE                                                   
         BNE   MIN12                                                            
         CLC   MSACTEL,ELEMNO                                                   
         BNE   MIN12                                                            
* FOUND THE ELEMENT                                                             
         OC    RPAY,RPAY           TEST PAID                                    
         BZ    MIN14               NO                                           
         TM    RSTATUS,RSMINSDQ    TEST (X'40') MISSED ALREADY                  
         BO    MIN20               YES - FUGGEDABOUDIT                          
         B     MIN16                                                            
*                                                                               
MIN14    TM    RSTATUS,RSMINSDQ    TEST (X'40') MISSED ALREADY                  
         BO    MIN20               YES - DON'T MISS IT AGAIN                    
*                                                                               
MIN16    NI    RSTATUS,X'EF'       UNSET MAKEGOOD PENDING                       
         OI    RSTATUS,X'40'       SET MINUSSED FLAG                            
*                                                                               
MN       USING REGELEM,ELEM                                                     
         MVC   MN.REGELEM(RLPOL2LQ),REGELEM    COPY ELEMENT                     
         MVI   MN.RCODE,RCPOTOQ    SET X'0C' - POL OTO CODE                     
         XC    MN.RPAY,MN.RPAY     CLEAR PAY DATE                               
         NI    MN.RSTATUS,X'FF'-RSMINSDQ-RSMKGDPQ  DROP MINUSSED/MGPNDG         
         OI    MN.RSTATUS,RSMINUSQ SET MINUS SPOT (-OTO)                        
         DROP  MN,R6                                                            
*                                                                               
         LLC   R0,1(R6)            ADD -OTO AFTER ORIGINAL/OTO                  
         AR    R6,R0                                                            
         BRAS  RE,ADDEL            INSERT -OTO                                  
*                                                                               
* CLEAN UP RECORD IN CASE ANY RELATED ELEMENTS FOLLOW                           
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
MIN18    CLI   0(R6),X'10'                                                      
         BL    MIN20                                                            
         CLI   0(R6),X'1F'                                                      
         BH    MIN20                                                            
         BRAS  RE,DELEL                                                         
         B     MIN18                                                            
*                                                                               
MIN20    MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   MIN24                                                            
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   MIN24                                                            
         CLC   KEY+BUYKSTA-BUYKEY(3),MSSTTN    TEST SAME STATION                
         BNE   MIN22               NO, WRITE OUT RECORD                         
         CLC   KEY+11(2),MSACTBUY  TEST SAME LINE NUMBER                        
         BE    MIN10               YES - CONTINUE                               
MIN22    GOTO1 PUTREC              ELSE WRITE THIS RECORD                       
         B     MIN2                AND GET NEXT                                 
*                                                                               
MIN24    GOTO1 PUTREC                                                           
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* SEARCH NOTICE RECORD FOR COMMENTS                                             
* THEN READ THE BUYS AND REPLACE ALL THE COMMENTS                               
*===================================================================            
         SPACE 1                                                                
MICOM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,FRSTMSS                                                 
*                                                                               
MICOM2   BRAS  RE,CALLTSAR                                                      
         BNE   MICOMX                                                           
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
MICOM4   CLI   MSTYPE,MSTYPEQ      TEST MISSED                                  
         BNE   MICOMX                                                           
*                                                                               
         MVC   MYSTTN,MSSTTN                                                    
         BRAS  RE,READMKN          GET NOTICE REC TO AREC2                      
*                                                                               
         MVI   ELCODE,MNMBCELQ     LOOK FOR 1-BYTE LINENUM ELS FIRST            
*                                                                               
MICOM6   L     R6,AREC2                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
MICOM8   BRAS  RE,NEXTEL                                                        
         BE    MICOM10                                                          
         CLI   ELCODE,MNB2CELQ     ALREADY LOOKED FOR 75'S                      
         BE    MICOMX              YES - DONE                                   
         MVI   ELCODE,MNB2CELQ     LOOK FOR 2-BYTE LINENUM ELS                  
         B     MICOM6                                                           
*                                                                               
MICOM10  XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+12(1),MNMBCBUY-MNMBCEL(R6)  MOVE 1-BYTE BUYLINE              
         CLI   0(R6),MNMBCELQ                                                   
         BE    *+10                                                             
         MVC   KEY+11(2),MNB2CBUY-MNB2CEL(R6)  MOVE 2-BYTE BUYLINE              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1          READ MISSED BUY RECORD INTO AREC1            
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BDELEM           REMOVE COMMENTS PRESENTLY IN BUY             
****     L     R6,AREC1            REMOVE COMMENTS PRESENTLY IN BUY             
****     LA    R6,24(R6)                                                        
*                                                                               
MICOM14  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
MICOM16  CLI   0(R6),0             EOR?                                         
         BE    MICOM20                                                          
         CLI   0(R6),CMCODEQ       X'66' - COMMENT ELEM?                        
         BNE   MICOM14                                                          
         BRAS  RE,DELEL            YES, DELETE IT                               
         B     MICOM16                                                          
*                                                                               
* OK - NOW ADD THE COMMENTS FROM NOTICE RECORD                                  
* NOTE THAT ELCODE STILL HAS X'70' OR X'75'                                     
*                                                                               
MICOM20  LR    R7,R6               SAVE INSERTION POINT                         
         L     R6,AREC2                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
CM       USING COMELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   CM.CMCODE,CMCODEQ   SET X'66' COMMENT ELEM                       
*                                                                               
MICOM22  BRAS  RE,NEXTEL                                                        
         BNE   MICOM30                                                          
*                                                                               
         LLC   R0,MNMBCBUY-MNMBCEL(R6)   GET 1-BYTE LINE NUMBER                 
         CLI   ELCODE,MNMBCELQ                                                  
         BE    *+8                                                              
         ICM   R0,3,MNB2CBUY-MNB2CEL(R6) GET 2-BYTE LINE NUMBER                 
*                                                                               
         CLM   R0,3,KEY+11         MATCH BUYLINE NUMBER                         
         BNE   MICOM22             NO - KEEP LOOKING                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,CM.CMNUM         COMMENT SEQNUM                               
         AHI   RE,1                                                             
         STC   RE,CM.CMNUM                                                      
         CHI   RE,5                                                             
         JH    *+2                 NO MORE THAN 5                               
*                                                                               
         IC    RE,1(R6)            GET MKN COMMENT LEN                          
         LA    R0,MNMBCOVH         GET LENGTH OF OVERHEAD                       
         CLI   ELCODE,MNMBCELQ                                                  
         BE    *+8                                                              
         LA    R0,MNB2COVH                                                      
         SR    RE,R0               GIVES DATA LENGTH -1                         
         BM    MICOM24                                                          
*                                                                               
         LA    RF,MNMBCTXT-MNMBCEL(R6)                                          
         CLI   ELCODE,MNMBCELQ                                                  
         BE    *+8                                                              
         LA    RF,MNB2CTXT-MNB2CEL(R6)                                          
         EX    RE,*+4                                                           
         MVC   CM.CMDATA(0),0(RF)  MOVE COMMENT DATA                            
         AHI   RE,CMDATA-COMELEM   ADD BUY-COMMENT OVHD                         
         STC   RE,CM.CMLEN                                                      
         DROP  CM                                                               
*                                                                               
MICOM24  GOTO1 SVRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R7))                        
         CLI   DMCB+8,C'R'                                                      
         JNE   *+2                                                              
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0               NEXT COMMENT GOES AFTER THIS ONE             
         B     MICOM22                                                          
*                                                                               
MICOM30  GOTO1 PUTREC                                                           
         CLI   0(R6),MNMBCELQ      TEST ANY MORE COMMENTS                       
         BE    MICOM10             YES - GO PROCESS                             
         CLI   0(R6),MNB2CELQ                                                   
         BE    MICOM10                                                          
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         BNL   MICOM2                                                           
*                                                                               
MICOMX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* GENERATE MAKEGOOD ACCEPTANCE REPORT                                           
*===================================================================            
         SPACE 1                                                                
         USING REPWORKD,R7                                                      
REPORT   NTR1  BASE=*,LABEL=*,WORK=(R7,REPWORKL)                                
* AS OF 03OCT02, NO ERROR REPORT FOR C2 LOCK                                    
         CLC   BMGEERR,=Y(C2LOCK)                                               
         BE    REPX1                                                            
*                                                                               
         OC    BMGEERR,BMGEERR     TEST ERROR REPORT                            
         BNZ   REP1A10             YES, SKIP PROFILE TEST                       
*                                                                               
* CHECK MKGD TRANSACTION REPORT PROFILE!!                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         L     R1,VCOMFACS         RF = A(GETPROF)                              
         L     RF,CGETPROF-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,WORK,WORK+32,VDATAMGR                                  
         CLI   WORK+47,C'N'       PROFILE SET TO PRINT THIS REPORT?             
         JE    EXIT                                                             
*                                                                               
REP1A10  XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSPOOL       GET A(SPOOL)                                 
         GOTO1 VCALLOV,DMCB                                                     
         MVC   SPOOL,0(R1)                                                      
*                                                                               
         MVC   BSPOOLID,=C'DAR'    SETUP PRINT QUEUE INFO                       
         XC    BSPOOLKEY,BSPOOLKEY                                              
         LA    R2,BSPOOLKEY                                                     
         USING PQPLD,R2                                                         
*                                                                               
         MVC   PLDESC(11),=C'MKGD ACCEPT'                                       
         MVI   PLCLASS,C'Z'          DARE CLASS Z                               
         OI    BSPOOLIND,BSPUINIT    ALLOWS ME TO SET THE CLASS                 
         MVI   BUSERLANG,0                                                      
         MVC   PLSUBID,BSPOOLID                                                 
         MVC   PLUSER,10(R3)       TWAORIG                                      
*                                                                               
         OC    BMGEERR,BMGEERR     TEST ERROR REPORT                            
         BZ    REP1A50                                                          
         MVI   PLCLASS,C'Z'        ERROR REPORT STAYS HERE !                    
         MVC   PLSUBID,=C'DXX'                                                  
         MVC   PLUSER,=X'0011'     SET TO SJR                                   
         MVC   PLDESC(11),=CL11'*MKGD ERR*'                                     
         CLC   BMGEERR,=Y(C2LOCK)                                               
         BNE   REP1A50                                                          
         MVC   PLDESC(11),=CL11'*MKGD LCKD*'                                    
*                                                                               
REP1A50  XC    ELEM,ELEM                                                        
         LA    R2,ELEM             POINT TO PRINT LINE                          
         USING PQPLD,R2                                                         
         ST    R2,BSPOOLQLK        THIS DOES MAGIC                              
*                                                                               
         OC    BMGEERR,BMGEERR     TEST ERROR REPORT                            
         BZ    REP2                                                             
         MVC   QLRETNL,=H'48'                                                   
         MVC   QLRETND,=H'48'                                                   
*                                                                               
REP2     MVC   BSPOOLDM,VDATAMGR                                                
         MVC   BRCDATCON,VDATCON                                                
         MVC   BRCCOMFAC,VCOMFACS                                               
         MVC   BSPOOLBUF,VTIA                                                   
         MVI   BSPMODE,0                                                        
         XC    BVPRINT,BVPRINT       NOT OFF-LINE                               
         XC    BABOX,BABOX           NO BOXES                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   BSPOOLRPN,PLREPNO                                                
         DROP  R2                                                               
*                                                                               
L        USING LINED,BP                                                         
*                                                                               
         MVC   BP+26(27),=C'MAKEGOOD TRANSACTION REPORT'                        
         OC    BMGEERR,BMGEERR                                                  
         BZ    *+10                                                             
         MVC   BP+26(27),=C'** MAKEGOOD ERROR REPORT **'                        
*                                                                               
         BAS   RE,PRINT                                                         
         MVI   BP+26,C'-'                                                       
         MVC   BP+27(26),BP+26                                                  
         BAS   RE,PRINT                                                         
*                                                                               
         OC    BMGEERR,BMGEERR                                                  
         BZ    REP10                                                            
*                                                                               
         MVC   ERRCD,BMGEERR+1     ASSUME 1-BYTE ERROR                          
         CLI   BMGEERR,0           TEST FIRST BYTE ZERO                         
         BE    *+14                YES                                          
         MVI   ERRCD,NEWERRS       ELSE IT'S A 2-BYTE ERROR                     
         MVC   NERRCD,BMGEERR      MOVE ERROR FOR GETTXT                        
*                                                                               
         BAS   RE,PRINT            SKIP A LINE                                  
         MVC   BP(8),MGAGYID       SET AGENCY ID                                
         MVC   BP+9(9),=C'BUY ERROR'                                            
         BAS   RE,GETERR           GET THE ERROR TEXT                           
         L     R4,ERRAREA                                                       
         MVC   BP+19(60),8(R4)                                                  
         BAS   RE,PRINT                                                         
*                                                                               
REP10    MVC   BP+4(5),=C'MEDIA'                                                
         MVC   BP+14(4),BUYMD                                                   
         MVC   BP+23(10),BUYMDXP                                                
         MVC   BP+46(5),=C'BUYER'                                               
         MVC   BP+54(12),BUYBU                                                  
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   BP+4(6),=C'CLIENT'                                               
         MVC   BP+14(4),BUYCL                                                   
         MVC   BP+23(22),BUYCLXP                                                
         MVC   BP+46(7),=C'PRODUCT'                                             
         MVC   BP+54(4),BUYPR                                                   
         MVC   BP+59(24),BUYPRXP                                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   BP+4(8),=C'ESTIMATE'                                             
         MVC   BP+14(4),BUYES                                                   
         MVC   BP+23(54),BUYESXP                                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   BP+4(7),=C'STATION'                                              
         MVC   BP+14(4),BUYST      MOVE FIRST 4 CHARS                           
         CLI   BUYST,C'0'          TEST CABLE                                   
         BNL   *+10                THAT'S ALL FOR CABLE                         
         MVC   BP+14(8),BUYST      ELSE SHOW 8 CHARS                            
         MVC   BP+23(22),BUYSTXP                                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   BP+4(4),=C'MGE='                                                 
         MVC   BP+8(3),MGGRPCOD                                                 
         CLC   =C'POL',BUYPR                                                    
         BNE   REP25                                                            
         MVI   BP+11,C','                                                       
         LA    R6,BP+12                                                         
         MVC   0(3,R6),MGQPRD1                                                  
         LA    R6,3(R6)                                                         
         OC    MGQPRD2,MGQPRD2                                                  
         BZ    REP25                                                            
         MVI   0(R6),C'-'                                                       
         MVC   1(3,R6),MGQPRD2                                                  
*                                                                               
REP25    DS    0H                                                               
         MVC   BP+23(5),=C'ORDER'                                               
         MVC   BP+29(8),MGORDERQ                                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   BP+4(12),=C'REP COMMENTS'                                        
         BAS   RE,PRINT                                                         
* READ TSAR REP ORDER COMMENTS                                                  
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
REP30    BRAS  RE,CALLTSAR                                                      
         BNE   REP40                                                            
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MGCTYPE,MGCTYPEQ                                                 
         BNE   REP30                                                            
         MVC   BP+6(75),MGCCOM                                                  
         BAS   RE,PRINT                                                         
         B     REP30                                                            
*                                                                               
REP40    BAS   RE,PRINT            SKIP A LINE                                  
*                                                                               
         MVC   BP+4(12),=C'MISSED SPOTS'                                        
         OC    FRSTMSS,FRSTMSS                                                  
         BNZ   REP42                                                            
         MVC   BP+4(15),=C'NO MISSED SPOTS'                                     
         BAS   RE,PRINT                                                         
         B     REP50                                                            
*                                                                               
REP42    BAS   RE,PRINT                                                         
*                                                                               
         BAS   RE,SETHEAD                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
REP44    BRAS  RE,CALLTSAR                                                      
         BNE   REP50                                                            
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   REP44                                                            
         BRAS  RE,DSPMSS                                                        
         BAS   RE,PRINT                                                         
         B     REP44                                                            
*                                                                               
REP50    BAS   RE,PRINT            SKIP A LINE                                  
         MVC   BP+4(6),=C'OFFERS'                                               
         OC    FRSTOFF,FRSTOFF                                                  
         BNZ   REP52                                                            
         MVC   BP+4(9),=C'NO OFFERS'                                            
         BAS   RE,PRINT                                                         
         B     REP100                                                           
*                                                                               
REP52    BAS   RE,PRINT                                                         
         OC    FRSTMSS,FRSTMSS     TEST ANY MISSED SPOTS                        
         BNZ   REP54               YES - DON'T REPRINT HEADLINES                
*                                                                               
         BAS   RE,SETHEAD          FORMAT HEADLINES                             
         BAS   RE,PRINT            AND PRINT THEM                               
*                                                                               
REP54    MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
REP56    BRAS  RE,CALLTSAR                                                      
         BNE   REP100                                                           
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MOTYPE,MOTYPEQ                                                   
         BNE   REP56                                                            
         CLI   MOCOMNUM,0                                                       
         BE    REP58                                                            
         CLI   MOCOMNUM,X'10'      TEST ORBIT OR COMMENT                        
         BL    REP70               ORBIT                                        
         MVC   BP+6(75),MOCOM                                                   
         B     REP60                                                            
*                                                                               
REP58    BRAS  RE,DSPOFF                                                        
*                                                                               
REP60    BAS   RE,PRINT                                                         
         B     REP56                                                            
*                                                                               
REP70    LA    R4,MOORB            FIRST ORBIT POSN                             
         LHI   R5,4                MAX ORBITS IN TSAR REC                       
*                                                                               
REP72    MVC   L.LINDATE(7),=C'*ORBIT*'                                         
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),DMCB,MOORBDAY,DUB                                           
         MVC   L.LINDAYS,DUB                                                    
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MOORBTIM,L.LINTIME                                     
*                                                                               
         MVC   L.LINPROG,MOORBPRG                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MOORBDEM                                                    
         N     R0,=X'00003FFF'                                                  
         TM    MOORBDEM,X'40'                                                   
         BO    REP74                                                            
         EDIT  (R0),(5,L.LINDEMO),1                                             
         B     REP76                                                            
*                                                                               
REP74    EDIT  (R0),(5,L.LINDEMO),2                                             
*                                                                               
REP76    BAS   RE,PRINT                                                         
*                                                                               
         AHI   R4,L'MOORB                                                       
         BCT   R5,REP72                                                         
         B     REP56                                                            
*                                                                               
REP100   BAS   RE,PRINT            SKIP A LINE                                  
         MVC   BP+4(7),=C'HISTORY'                                              
         BAS   RE,PRINT                                                         
*                                                                               
         CLI   BUYST,C'0'                                                       
         BNL   REP102                                                           
*                                                                               
         XC    KEY,KEY                                                          
K        USING DAREMGND,KEY                                                     
         MVI   K.MNKTYPE,MNKTYPQ                                                
         MVI   K.MNKSUBTY,MNKSTYPQ                                              
         MVC   K.MNKAGMD,SVAGYMD                                                
         MVC   K.MNKBYR,BUYBU                                                   
         OC    K.MNKBYR,SPACES                                                  
         MVC   K.MNKORDER,MGORDER                                               
         MVC   K.MNKGROUP,MGGRPCOD                                              
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      SHOULD EXIST                                
         JNE   *+2                                                              
         B     REP104                                                           
*                                                                               
REP102   XC    WORK,WORK           NOTICE HAS STATION X000000                   
K        USING DAREMGND,WORK                                                    
         MVI   K.MNXKTYPE,MNXKTYPQ                                              
         MVI   K.MNXKSBTY,MNXKSBTQ                                              
         MVC   K.MNXKAGMD,SVAGYMD                                               
         MVC   K.MNXKORDR,MGORDER                                               
         MVC   K.MNXKGRP,MGGRPCOD                                               
*                                                                               
         BRAS  RE,MYHIGH                                                        
*                                                                               
         CLC   WORK(28),WORK2                                                   
         JNE   *+2                                                              
         DROP  K                                                                
*                                                                               
REP104   MVC   AREC,AREC1          READ MG NOTICE RECORD INTO AREC1             
         BRAS  RE,MYGETREC                                                      
*                                                                               
         MVI   PRTREJCM,C'N'       REJECT COMMENTS NOT PRINTED                  
*                                                                               
         L     R6,AREC                                                          
****     AH    R6,DSPFRST          FIRST ELEMENT                                
         AH    R6,DATADISP         FIRST ELEMENT                                
*                                                                               
         MVC   L.LINLIN-1(6),=C'STATUS'                                         
         MVC   L.LINSLN(4),=C'DATE'                                             
         MVC   L.LINCOST(4),=C'TIME'                                            
         BAS   RE,PRINT                                                         
         B     REP115                                                           
*                                                                               
REP110   MVI   ELCODE,MNSTELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   REPX                                                             
         USING MNSTELD,R6                                                       
*                                                                               
*                                                                               
REP115   LA    R3,STATABLE                                                      
         B     REP120                                                           
*                                                                               
STATABLE DS    0H                                                               
         DC    AL1(MNSTNEW),CL9'NEW      '                                      
         DC    AL1(MNSTAPP),CL9'APPROVED '                                      
         DC    AL1(MNSTREJ),CL9'REJECTED '                                      
         DC    AL1(MNSTERR),CL9'ERROR    '                                      
         DC    AL1(MNSTCAN),CL9'CANCELLED'                                      
         DC    AL1(MNSTOKAY),CL9'OKAYED   '                                     
         DC    AL1(MNSTAMND),CL9'AMENDED  '                                     
         DC    AL1(MNSTCANM),CL9'SELRCL   '                                     
         DC    AL1(MNSTGOIN),CL9'TO BE OKD'                                     
         DC    AL1(MNSTHOLD),CL9'ON HOLD  '                                     
         DC    AL1(MNSTDELV),CL9'DELIVERED'                                     
         DC    X'FF'                                                            
*                                                                               
REP120   CLI   0(R3),X'FF'                                                      
         BE    REP130                                                           
         CLC   0(1,R3),MNSTSTAT                                                 
         BE    *+12                                                             
         LA    R3,10(R3)                                                        
         B     REP120                                                           
REP130   MVC   L.LINLIN-1(9),1(R3)                                              
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MNSTDATE),(11,L.LINSLN)                          
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,MNSTTIME,WORK,L'MNSTTIME                               
         MVC   L.LINCOST(2),WORK                                                
         MVI   L.LINCOST+2,C':'                                                 
         MVC   L.LINCOST+3(2),WORK+2                                            
*                                                                               
         BAS   RE,PRINT                                                         
         CLI   MNSTSTAT,MNSTREJ    REJECTED?                                    
         BNE   REP110                                                           
         DROP  R6                                                               
*                                                                               
         CLI   PRTREJCM,C'Y'       ALREADY PRINTED REJECT COMMENTS?             
         BE    REP110                                                           
         MVI   PRTREJCM,C'Y'                                                    
*                                                                               
         ST    R6,SAVER6           SAVE A(LAST STATUS ELEMENT)                  
         MVI   ELCODE,MNMRJCQ                                                   
         L     R6,AREC                                                          
****     LA    R6,24(R6)                                                        
         AH    R6,DATADISP                                                      
REP135   BRAS  RE,NEXTEL                                                        
         BNE   REP140                                                           
         USING MNMRJCD,R6                                                       
         SR    R1,R1                                                            
         IC    R1,MNMRLEN                                                       
         AHI   R1,-(MNMROVRH+1)   LENGTH OF COMMENT -1                          
         EX    R1,*+4                                                           
         MVC   L.LINSLN(0),MNMRTEXT                                             
         BAS   RE,PRINT                                                         
         B     REP135              PRINT MULT-MG REJ COMMENTS                   
         DROP  R6                                                               
*                                                                               
REP140   L     R6,SAVER6           RESTORE LAST STATUS ELEM ADDR                
         B     REP110                                                           
*                                                                               
REPX     MVI   BSPMODE,X'FF'       CLOSE THE PRINT QUEUE                        
         BAS   RE,PRINT                                                         
*                                                                               
REPX1    OC    BMGEERR,BMGEERR     TEST REPORT WAS FOR ERRORS                   
         JZ    EXIT                                                             
         DROP  T                                                                
* NEED TO XMT ALL                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         SR    R0,R0                                                            
         LA    R1,BUYMSGH                                                       
*                                                                               
REPX2    IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    REPX4                                                            
         TM    1(R1),X'20'         TEST PROT                                    
         BO    REPX2                                                            
         OI    1(R1),X'01'         SET MODIFIED                                 
         OI    6(R1),X'80'         SET XMT                                      
         B     REPX2                                                            
*                                                                               
REPX4    MVC   1(2,R1),=X'0101'                                                 
*                                                                               
         MVI   ERRAREA,X'FE'       SEND ERROR AND UNWIND TRANSACTION            
         MVC   ERRCD,BMGEERR+1     ASSUME 1-BYTE ERROR                          
         CLI   BMGEERR,0           TEST FIRST BYTE ZERO                         
         BE    *+14                YES                                          
         MVI   ERRCD,NEWERRS       ELSE IT'S A 2-BYTE ERROR                     
         MVC   NERRCD,BMGEERR      MOVE ERROR FOR GETTXT                        
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR               NO RETURN FROM ERROR                         
         DC    H'0'                                                             
*                                                                               
SETHEAD  MVC   L.LINLIN(3),=C'LIN'                                              
         MVC   L.LINDATE(7),=C'BUY PER'                                         
         MVC   L.LINDAYS(4),=C'DAYS'                                            
         MVC   L.LINNPW(2),=C'NO'                                               
         MVC   L.LINTIME(4),=C'TIME'                                            
         MVI   L.LINDPT,C'D'                                                    
         MVC   L.LINSLN+1(2),=C'LN'                                             
         MVC   L.LINPROG(7),=C'PROGRAM'                                         
         MVC   L.LINCOST(4),=C'COST'                                            
         MVC   L.LINDEMO(6),MGDEMNM1                                            
         BR    RE                                                               
         SPACE 1                                                                
* GET ERROR MESSAGE TEXT FOR ERROR REPORT                                       
         SPACE 1                                                                
GETERR   NTR1                                                                   
         L     R4,ERRAREA          GET ADDR OF ERROR FIELD                      
         CLI   ERRAREA,X'FF'       MESSAGE PRESENT                              
         BNE   GETERR40                                                         
         MVC   8(60,R4),BUYMSG     YES, MOVE IT IN                              
         B     GETERRX                                                          
*                                                                               
GETERR40 CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   GETERR50                                                         
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,3            AND MESSAGE SYSTEM                           
         LA    RE,ERRTEXT          AREA FOR APPENDED ERR TEXT                   
         CLI   0(RE),C' '          TEST IF THERE IS ANY TEXT                    
         BL    *+12                                                             
         ST    RE,GTATXT-1                                                      
         MVI   GTLTXT,L'ERRTEXT                                                 
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         XC    ERRTEXT,ERRTEXT                                                  
         B     GETERRX                                                          
*                                                                               
GETERR50 GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB),(3,0)                 
*                                                                               
GETERRX  XIT1                                                                   
*                                                                               
PRINT    LR    R0,RE                                                            
         MVI   BLINE,1                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
DSPMSS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   L.LINSEL,C'-'                                                    
*                                                                               
DSM2     SR    R0,R0                                                            
         ICM   R0,3,MSACTBUY                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LINLIN,DUB                                                     
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    DSM4                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MSSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVC   L.LINNET,WORK+20                                                 
*                                                                               
DSM4     GOTO1 VDATCON,DMCB,MSELDTQ,(4,L.LINDATE)                               
*                                                                               
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),(R1),MSDAYS,DUB                                             
         MVC   L.LINDAYS,DUB                                                    
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MSSTIM,L.LINTIME                                       
*                                                                               
         MVC   L.LINDPT,MSDPT                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MSSLN                                                         
         EDIT  (R0),(3,L.LINSLN)                                                
*                                                                               
         MVC   L.LINPROG,MSPROG                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,MSCOST                                                      
         EDIT  (R0),(9,L.LINCOST),2,ALIGN=LEFT                                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,MSDEM1                                                     
         EDIT  (R0),(5,L.LINDEMO),1                                             
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R7                                                               
REPWORKD DSECT                                                                  
SAVER6   DS    F                                                                
PRTREJCM DS    X                                                                
REPWORKL EQU   *-REPWORKD                                                       
T21132   CSECT                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY AN OFFER                                                              
*================================================================               
         SPACE 1                                                                
DSPOFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    DSPOFF2                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MOSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVI   L.LINLIN,C'*'                                                    
         MVC   L.LINLIN+1(3),WORK+20   SHOW NETWORK                             
         MVI   L.LINLIN+4,C'*'                                                  
*                                                                               
DSPOFF2  GOTO1 VDATCON,DMCB,(8,MOSTRT),(4,L.LINDATE-3)                          
         LA    R5,L.LINDATE+2                                                   
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         SR    R0,R0                                                            
         IC    R0,MOWKS                                                         
         EDIT  (R0),(2,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C'W'                                                       
*                                                                               
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),(R1),MODAYS,DUB                                             
         MVC   L.LINDAYS,DUB                                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MONPW                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LINNPW,DUB                                                     
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MOSTIM,L.LINTIME                                       
*                                                                               
         MVC   L.LINDPT,MODPT                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MOSLN                                                         
         EDIT  (R0),(3,L.LINSLN)                                                
*                                                                               
         MVC   L.LINPROG,MOPROG                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,MOCOST                                                      
         EDIT  (R0),(9,L.LINCOST),2,ALIGN=LEFT                                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,MODEM1                                                     
         N     R0,=X'3FFFFFFF'     DROP FLAGS                                   
         TM    MODEM1,X'40'        2-DECIMAL?                                   
         BZ    DSPOFF3                                                          
         EDIT  (R0),(5,L.LINDEMO),2                                             
         B     DSPOFFX                                                          
DSPOFF3  EDIT  (R0),(5,L.LINDEMO),1                                             
         B     DSPOFFX                                                          
*                                                                               
DSPOFFX  J     EXIT                                                             
         DROP  L                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THE MAKEGOOD NOTICE RECORD INTO REC2                           *         
*=====================================================================*         
                                                                                
READMKN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYST,C'0'          TEST US CABLE                                
         BNL   RDMKN10                                                          
*                                                                               
         XC    KEY,KEY                                                          
K        USING DAREMGND,KEY                                                     
         MVI   K.MNKTYPE,MNKTYPQ                                                
         MVI   K.MNKSUBTY,MNKSTYPQ                                              
         MVC   K.MNKAGMD,SVAGYMD                                                
         MVC   K.MNKBYR,BUYBU                                                   
         OC    K.MNKBYR,SPACES                                                  
         MVC   K.MNKORDER,MGORDER                                               
         MVC   K.MNKGROUP,MGGRPCOD                                              
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,MYHIGH                                                        
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
         B     RDMKN14                                                          
*                                                                               
RDMKN10  XC    WORK,WORK           THIS CODE FOR CABLE                          
K        USING DAREMGND,WORK                                                    
         MVI   K.MNXKTYPE,MNXKTYPQ                                              
         MVI   K.MNXKSBTY,MNXKSBTQ                                              
         MVC   K.MNXKAGMD,SVAGYMD                                               
         MVC   K.MNXKORDR,MGORDER                                               
         MVC   K.MNXKGRP,MGGRPCOD                                               
         MVC   K.MNXKSTTN,MYSTTN                                                
*                                                                               
         BRAS  RE,MYHIGH                                                        
*                                                                               
         CLC   WORK(MNXKSTTN-MNXKEY),WORK2  SAME UPTO THE STATION?              
         JNE   *+2                                                              
*                                                                               
RDMKN12  CLI   USEMYIO,C'Y'        IF USER SET AREC                             
         BNE   RDMKN14             JUST READ ONE RECORD                         
         BRAS  RE,MYGETREC                                                      
         B     READMKX             AND EXIT                                     
*                                                                               
RDMKN14  MVC   AREC,AREC2                                                       
         BRAS  RE,MYGETREC                                                      
*                                                                               
RDMKNX   MVI   USEMYIO,C'N'                                                     
         J     EXIT                                                             
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THE MAKEGOOD OFFER RECORDS INTO REC3/REC4                      *         
* UNLESS USEMYIO=Y, IN WHICH CASE READ FIRST REC INTO AREC            *         
* SET BY CALLER                                                                 
*=====================================================================*         
                                                                                
READMKO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYST,C'0'          TEST CABLE                                   
         BNL   RDMKO10                                                          
*                                                                               
         XC    KEY,KEY                                                          
K        USING MOKEY,KEY                                                        
         MVI   K.MOKTYPE,MOKTYPQ                                                
         MVI   K.MOKSUBTY,MOKSTYPQ                                              
         MVC   K.MOKAGMD,SVAGYMD                                                
         MVC   K.MOKORDER,MGORDER                                               
         MVC   K.MOKMGCD,MGGRPCOD                                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   MGMKODA1,KEY+14     SAVE DA OF FIRST REC                         
         XC    MGMKODA2,MGMKODA2   CLEAR SECOND REC DA                          
*                                                                               
         CLI   USEMYIO,C'Y'        IF USER SET AREC                             
         BNE   RDMKO05             JUST READ ONE RECORD                         
         GOTO1 GETREC                                                           
         B     READMKX             AND EXIT                                     
*                                                                               
RDMKO05  L     R6,AREC3            USE AREC3 FOR OFFER RECORD                   
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AREC4                                                         
         XC    0(256,R6),0(R6)                                                  
         GOTO1 SEQ                 TEST FOR ANOTHER MKO RECORD                  
         CLC   KEY(10),KEYSAVE                                                  
         BNE   READMKX                                                          
         CLI   KEY+10,X'FF'        DON'T COUNT DEFAULT DEMO RECORD              
         BE    READMKX                                                          
         CLI   KEY+10,X'FE'        DON'T COUNT SELLER DEMO RECORD               
         BE    READMKX                                                          
         MVC   MGMKODA2,KEY+14     SAVE DISK ADDRESS                            
         ST    R6,AREC                                                          
         GOTO1 GETREC              READ MKO CONTINUATION REC                    
         J     READMKX                                                          
         EJECT                                                                  
RDMKO10  XC    WORK,WORK                                                        
K        USING MOKEY,WORK                                                       
*                                                                               
         MVI   K.MOXKTYPE,MOXKTYPQ                                              
         MVI   K.MOXKSBTY,MOXKSBTQ                                              
         MVC   K.MOXKAGMD,SVAGYMD                                               
         MVC   K.MOXKORDR,MGORDER                                               
         MVC   K.MOXKMGCD,MGGRPCOD                                              
         MVC   K.MOXKSTTN,MYSTTN   SET STA/NET                                  
         DROP  K                                                                
*                                                                               
         BRAS  RE,MYHIGH                                                        
         CLC   WORK(30),WORK2      SAME A-M/ORD/GRP/STA                         
****     BNE   READMKX             NO MORE!                                     
         JNE   *+2                 DIE !!!                                      
*                                                                               
         MVC   MGMKODA1,MYDA       SAVE DA OF FIRST REC                         
         XC    MGMKODA2,MGMKODA2   CLEAR SECOND REC DA                          
*                                                                               
         CLI   USEMYIO,C'Y'        IF USER SET AREC                             
         BNE   RDMKO11             JUST READ ONE RECORD                         
         BRAS  RE,MYGETREC                                                      
         B     READMKX             AND EXIT                                     
*                                                                               
RDMKO11  L     R6,AREC3            USE AREC3 FOR OFFER RECORD                   
         ST    R6,AREC                                                          
         BRAS  RE,MYGETREC                                                      
*                                                                               
RDMKO12  L     R6,AREC4                                                         
         ST    R6,AREC                                                          
         XC    0(256,R6),0(R6)                                                  
*                                                                               
         MVC   WORK2,WORK                                                       
         BRAS  RE,MYSEQ                                                         
*                                                                               
         CLC   WORK(30),WORK2      TYPE/A-M/ORD/GRP                             
         BNE   READMKX                                                          
         CLI   MOXKDTYP-MOXKEY+WORK,MOXKDDMQ  TEST DEMO RECORD                  
         BE    READMKX                                                          
         MVC   MGMKODA2,MYDA       SAVE THE DISK ADDRESS                        
                                                                                
RDMKO14  BRAS  RE,MYGETREC         READ MKO CONTINUATION REC                    
*                                                                               
READMKX  MVI   USEMYIO,C'N'                                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* READ THROUGH TSAR RECORDS CHECKING FOR NOTICES AND OFFERS                     
*=============================================================                  
                                                                                
T        USING TSARD,TSARBLK                                                    
                                                                                
CKPREBON NTR1  BASE=*,LABEL=*                                                   
         SR    R4,R4               CLEAR SAVE REG                               
         SR    R5,R5                                                            
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         MVI   T.TSACTN,TSANXT                                                  
         B     CKPB4                                                            
*                                                                               
CKPB2    BRAS  RE,CALLTSAR                                                      
         JNE   CKPBX                                                            
*                                                                               
CKPB4    CLI   MSTYPE,MSTYPEQ                                                   
         BNE   CKPB6                                                            
         LTR   R4,R4               TEST FOUND ONE ALREADY                       
         BNZ   CKPB2               YES                                          
         LH    R4,T.TSRNUM                                                      
         B     CKPB2                                                            
*                                                                               
CKPB6    CLI   MOTYPE,MOTYPEQ                                                   
         BNE   CKPB2                                                            
         LTR   R5,R5                                                            
         BNZ   CKPB2                                                            
         LH    R5,T.TSRNUM                                                      
***NOP***B     CKPB2               NO REASON TO CONTINUE                        
*                                                                               
CKPBX    STCM  R4,3,FRSTMSS                                                     
         STCM  R5,3,FRSTOFF                                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUILD A NEW STATUS ELEMENT IN ELEM AND INSERT AT 0(R6)                        
* UNLESS R6=0                                                                   
* BYTE CONTAINS STATUS VALUE                                                    
*=====================================================================*         
         SPACE 1                                                                
BLDSTAT  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
E        USING MNSTELD,ELEM                                                     
         MVI   E.MNSTEL,MNSTELQ                                                 
         MVI   E.MNSTLEN,MNSTLENQ                                               
         MVC   E.MNSTSTAT,BYTE                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(19,E.MNSTDATE)                               
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,DUB                                                        
         STCM  R0,15,DUB+4                                                      
         AP    DUB(4),DUB+4(4)     DDS TIME IS OFFSET BY 6AM                    
*                                                                               
         CP    DUB+4(4),=P'240000'    PAST MIDNIGHT?                            
         JL    BLDSTAT2                                                         
         SP    DUB+4(4),=P'240000'    YUP, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)  THE TIME                            
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
         GOTO1 VDATCON,DMCB,(0,WORK),(19,E.MNSTDATE)                            
*                                                                               
BLDSTAT2 ICM   R1,15,DUB                                                        
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,E.MNSTTIME     CURRENT TIME PWOS                            
         STH   R1,MGTIME                                                        
*                                                                               
         LTR   R6,R6               TEST TO ADD TO CURRENT RECORD                
         BZ    BLDSTATX                                                         
         GOTOR MYRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R6))                        
*                                                                               
BLDSTATX J     EXIT                                                             
         LTORG                                                                  
         DROP  E                                                                
*=================================================================*             
* COMMON CALL TO BASE SAVES I/O AREA CONTENTS                     *             
* AND TSAR BUFFERS                                                              
*=================================================================*             
         SPACE 1                                                                
CALLBASE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVTSRNUM,T.TSRNUM   SAVE CURRENT TSAR REC                        
         MVI   T.TSACTN,TSASAV                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
* BUILD CALL TO W/S SAVE                                                        
         L     R1,=A(SPAUTHWK-BUYSAVE)                                          
         AR    R1,RA                                                            
         XC    0(L'SPAUTHWK,R1),0(R1)                                           
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=CL4'SV32'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   R0,REC4X-REC3       LEN OF REC3 + REC4                           
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,AREC3                                                    
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JNE   *+2                                                              
*                                                                               
         L     RF,VCALLBAS                                                      
         BASR  RE,RF                                                            
*                                                                               
         L     RF,VCOMFACS         BUILD CALL TO W/S SAVE                       
         L     RF,CWSSVR-COMFACSD(RF)                                           
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=CL4'SV32'                                              
         MVI   FAWSACTN,FAWSARST                                                
         MVC   FAWSADR,AREC3                                                    
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JNE   *+2                                                              
         MVI   T.TSACTN,TSARES                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,SVTSRNUM                                                
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* MKO/MKN RECORDS LIVE ON XSPFILES, SO DIRECT COMMANDS TO                       
* XSPFILES AS NEEDED                                                            
*===============================================================                
                                                                                
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYADDX                                                           
         GOTO1 ADDREC                                                           
         J     EXIT                                                             
MYADDX   GOTO1 VDATAMGR,DMCB,=C'ADDREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
                                                                                
MYGETREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYGETX                                                           
         GOTO1 GETREC                                                           
         J     EXIT                                                             
MYGETX   GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
*                                                                               
MYPUTREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         BNL   MYPUTX                                                           
         GOTO1 PUTREC                                                           
         J     EXIT                                                             
MYPUTX   GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
*                                                                               
MYHIGH   NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYHIGHX                                                          
         GOTO1 HIGH                                                             
         J     EXIT                                                             
*                                                                               
MYHIGHX  MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WORK2,WORK                   
         MVC   MYDA,WORK+36        SAVE DISK ADDRESS                            
         J     EXIT                                                             
*                                                                               
MYSEQ    NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYSEQX                                                           
         GOTO1 SEQ                                                              
         J     EXIT                                                             
*                                                                               
MYSEQX   GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WORK2,WORK                   
         MVC   MYDA,WORK+36        SAVE DISK ADDRESS                            
         J     EXIT                                                             
*                                                                               
MYRECUP  NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'          TEST US CABLE                                
         BL    *+8                                                              
         MVI   DMCB,C'T'           SET FOR XSPFILE                              
         GOTO1 SVRECUP                                                          
         J     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
         PRINT ON                                                               
       ++INCLUDE SPBUY31WRK                                                     
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDFLDHDR                                                       
FLDXDATA DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103SPBUY32   02/17/21'                                      
         END                                                                    
