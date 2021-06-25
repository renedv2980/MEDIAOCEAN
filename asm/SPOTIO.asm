*          DATA SET SPOTIO     AT LEVEL 026 AS OF 06/17/19                      
*PHASE T00A45A                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE DPTRD                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'T00A45 - SPOTPAK WRITER IO HANDLER'                             
*                                                                               
*GOTOR =V(PRNTBL),PARM,=C'*KEY **',IOKEY,C'DUMP',13,=C'1D',    X                
*      (C'P',SBPRINT)                                                           
*                                                                               
***********************************************************************         
*                                                                     *         
*              SPOTIO (T00A45) - SPOT WRITER IO HANDLER               *         
*                                                                     *         
*     NOTE: HISTORY PRIOR TO 03JAN11 IS AT BOTTOM OF SOURCE CODE      *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-24962  06/17/19 WEEKLY DATA FOR POSTS OVERRIDE OPTION     *         
* AKAT SPEC-28203  09/27/18 HONOR 2 CHARACTER OFFICE LISTS            *         
* AKAT SPEC-11692  02/16/18 NEW TARGET3 & TARGET4 KEYWORD SUPPORT     *         
* AKAT SPEC-12852  02/16/18 REPORT NEW MEDIA FRAMEWORKS DATA          *         
* AKAT SPEC-13692  06/22/17 INCLUDE MEDIA X FOR CANADIAN MEDIA *      *         
* AKAT SPEC-13149  05/22/17 SET PARENT+ STATION FOR COMSCORE          *         
* AKAT SPEC-6939   02/24/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5  *         
* AKAT ITMF-7260   05/11/16 CANNOT RELY ON FSTA HOOK FOR READCLS      *         
* AKAT MOSTK-20    03/17/16 NEW -CM BAND SUPPORT FOR IHEART RADIO     *         
* AKAT MOSTK-76    01/15/16 SUPPORT NEW BUYMG=Y/N OPTION              *         
* 04JUN15 17 AKAT-- FIX BUG FOR CANADIAN COST2 BILLING              *           
* 04SEP14 16 AKAT-- FIX LIMIT ACCESS BUG ON OFFICER CALL            *           
* 30JAN14 15 AKAT-- HONOR ALL/ AND ALL- CABLE FILTER FOR OM KEYWORDS*           
* 29OCT13 14 AKAT-- FIX DIGITAL INV BUG WHERE MKT WAS NOT SET       *           
* 26AUG13 13 AKAT-- SUPPORT FOR NEW NTYPE KEYWORD                   *           
* 31JUL13 12 AKAT-- LOOK UP STATIONS PROPERLY FOR DIGITAL REQUESTS  *           
* 15MAR13 11 AKAT-- ALL-DV AND ALL-SM SUPPORT                       *           
* 04OCT12 10 AKAT-- FIX CLEARANCE STATUS BUG                        *           
* 22MAY12 09 AKAT-- SRDS SUPPORT                                    *           
* 13FEB12 08 AKAT-- FIX SBQMED BUG FOR ALL MEDIA SPILL RADIO AFFIDS *           
* 18NOV11 07 AKAT-- HONOR NEGATIVE SREP FILTER AND SUPPORT COST2    *           
*                -- FOR BILL RECORDS                                *           
* 28OCT11 06 AKAT-- PASS CLEARANCE STATUS D/A IN CHECK TABLE        *           
* 27JUL11 05 AKAT-- 2-BYTE BUYLINE FIX                              *           
* 04APR11 04 AKAT-- SUPPORT NEW MFID# KEYWORD                       *           
* 22MAR11 03 AKAT-- HONOR NEW MATCHCOM=N FILTER                     *           
* 14FEB11 02 AKAT-- HONOR NEW FLAG TO IGNORE ESTIMATES OOWR         *           
* 03JAN11 01 EFJ -- LEVEL RESET                                     *           
*                -- MAJOR CODE RE-ORG                               *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
SPOTIO   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPIO**,RR=RE,CLEAR=YES                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         L     R8,0(R1)                                                         
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         L     R7,SBCOMFAC                                                      
         USING COMFACSD,R7         R7 = A(COMFACS)                              
*                                                                               
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         ST    RE,RELO                                                          
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
*                                                                               
         BRAS  RE,INIT             INITIALIZE VALUES                            
         BNE   SP20                                                             
*                                                                               
SP18     BRAS  RE,GETAGYMD         GET AGENCY/MEDIA                             
         BNE   SPX                                                              
         BRAS  RE,FILTERS          DETERMINE REQUEST FILTERS                    
         CLI   SBQSGRD,C' '        TEST STATION GROUP REQUEST                   
         BNH   SP19                                                             
         GOTOR AGETSGR             YES-GET STATION GROUPS                       
         BNE   SPX                                                              
*                                                                               
SP19     XC    ACLTEL,ACLTEL                                                    
*                                                                               
SP20     MVI   SBINDS,0            CLEAR THE INDICATOR BYTE                     
         BRAS  RE,GETNCLT          GET NEXT CLIENT                              
         BNE   SP40                                                             
         BRAS  RE,BLDBUFF          SET UP PRODUCT BUFFER AND MKTGRP TAB         
         BNE   SP30                NEXT CLIENT IF CLIENT REJECTED               
         CLI   SBMODE,SBSTOP       TEST TO STOP NOW                             
         BE    SPX                                                              
*                                                                               
         TM    SBEFLAG2,SBEPETAB   DON'T EVEN ASK.                              
         BNZ   *+12                                                             
         BRAS  RE,PRDEST           BUILD PRODUCT/ESTIMATE TABLE                 
         BNE   SPX                 AND ESTIMATE BUFFER                          
*                                                                               
         TM    SBQREAD2,SBQRD2BH   READ BILL HEADER RECS UP FRONT?              
         BNO   *+12                                                             
         BRAS  RE,READBH           READ THE BILL HEADER RECORDS                 
         BNE   SPX                                                              
*                                                                               
         TM    SBQREAD2,SBQRD2BL   READ STATION BILLRECS BEFORE BUYS            
         BNO   SP21                                                             
         GOTOR ARDBILL                                                          
         JNE   SPX                                                              
*                                                                               
SP21     TM    SBQREAD,SBQRDINF    TEST TO READ INFOMERCIAL RECORDS             
         BZ    SP22                                                             
         GOTOR AREADINF            READ THE INFOMERCIAL RECORDS                 
         BNE   SPX                                                              
*                                                                               
SP22     TM    SBQSKIP,SBQSKBUY    TEST TO SKIP BUY READ                        
         BO    *+12                YES                                          
         BRAS  RE,READBUYS         READ THE BUY RECORDS                         
         BNE   SPX                                                              
*                                                                               
         TM    SBQSKIP,SBQSKGL     TEST TO SKIP GOAL READ                       
         BO    *+12                                                             
         BRAS  RE,READGOLS         READ THE GOAL RECORDS                        
         BNE   SPX                                                              
*                                                                               
         CLI   SBQMED,C'*'                                                      
         BE    *+10                                                             
         MVC   SBMED,SBQMED        MAKE SURE MEDIA IS REQUEST MEDIA             
*                                                                               
         TM    SBQREAD2,SBQRD2BL   SKIP IF BILLRECS READ UP FRONT               
         BO    SP22A                                                            
*                                                                               
         TM    SBQSKIP,SBQSKBIL    TEST TO SKIP STATION BILL RECORDS            
         BO    SP22A                                                            
         GOTOR ARDBILL                                                          
         JNE   SPX                                                              
*                                                                               
SP22A    DS    0H                                                               
*                                                                               
         TM    SBQREAD,SBQRDINV    TEST TO READ INVOICE RECORDS                 
         BZ    *+12                                                             
         BRAS  RE,READINV          READ THE INVOICE RECORDS                     
         BNE   SPX                                                              
*                                                                               
         TM    SBQREAD,SBQRDBH     SKIP IF NOT READING BILL HEADER RECS         
         BNO   SP221                                                            
*                                                                               
         TM    SBQREAD2,SBQRD2BH   SKIP IF BILL HDR RECS READ UP FRONT          
         BO    SP221                                                            
*                                                                               
         BRAS  RE,READBH           READ THE BILL HEADER RECORDS                 
         BNE   SPX                                                              
*                                                                               
SP221    DS    0H                                                               
*                                                                               
         TM    SBQREAD,SBQRDSLK    TEST TO READ STATION LOCKIN                  
         BZ    SP23                                                             
         GOTOR READSLK             READ THE STATION LOCKIN RECORDS              
         BNE   SPX                                                              
*                                                                               
SP23     TM    SBQREAD,SBQRDMSR    TEST TO READ MATCHING STATUS RECORDS         
         BZ    SP24                                                             
         GOTOR READMSR                                                          
         BNE   SPX                                                              
*                                                                               
SP24     DS    0H                                                               
         TM    SBQREAD2,SBQRD2PW    READ PW MARKET RECS?                        
         BZ    SP25                                                             
         GOTOR READPW                                                           
         BNE   SPX                                                              
*                                                                               
SP25     DS    0H                                                               
         TM    SBQREAD2,SBQRD2OM    READ OM KEYWORDS?                           
         BZ    SP30                 NO                                          
         TM    SBQSKIP,SBQSKBUY     SKIP BUY READ                               
         BZ    SP30                 NO - WE PROCESSED OM RECS W/BUYS            
         BRAS  RE,READOM                                                        
         BNE   SPX                                                              
*                                                                               
SP30     CLI   ONLINE,C'Y'         ONLY READ ONE CLIENT IF ONLINE               
         BE    SP40                                                             
         TM    SBQRDOPT,SBQROSCL   OR SINGLE CLIENT MODE                        
         BZ    SP20                                                             
*                                                                               
SP40     CLI   SBMODE,SBSTOP       DONE- TEST MODE=STOP                         
         BE    SPX                                                              
         CLI   SBQMED,C'*'         TEST MEDIA T AND R REQUEST                   
         BNE   SPX                                                              
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   SPX                 YES - ONLY PROCESS MEDIA T                   
         CLI   CANADA,C'Y'         YES- FOR NON CANADIAN, FOLLOW MEDIA          
         BE    SP42                     T WITH MEDIA R FOLLOWED BY X            
*                                                                               
         CLI   SBMED,C'T'                                                       
         BNE   *+12                                                             
         MVI   SBMED,C'R'                                                       
         B     SP44                                                             
*                                                                               
         CLI   SBMED,C'R'                                                       
         BNE   *+12                                                             
         MVI   SBMED,C'X'                                                       
         B     SP44                                                             
*                                                                               
         CLC   SBQAGY,=C'TR'       ONLY US AGY W/MEDIA N IS TR                  
         BNE   SPX                                                              
         CLI   SBMED,C'X'                                                       
         BNE   SPX                                                              
         MVI   SBMED,C'N'                                                       
         B     SP44                                                             
*                                                                               
SP42     CLI   SBMED,C'T'          FOR CANADIAN, FOLLOW T WITH N,               
         BNE   *+12                FOLLOWED BY R                                
         MVI   SBMED,C'N'                                                       
         B     SP44                                                             
*                                                                               
         CLI   SBMED,C'N'          JUST PROCESSED MEDIA N?                      
         BNE   *+12                NO                                           
         MVI   SBMED,C'R'          YES - SET TO MEDIA R                         
         B     SP44                PROCESS MEDIA R                              
*                                                                               
         CLI   SBMED,C'R'          JUST PROCESSED MEDIA R?                      
         BNE   SPX                 NO - DONE                                    
         MVI   SBMED,C'X'          YES - SET TO MEDIA X                         
*                                                                               
SP44     XC    SBCLT,SBCLT                                                      
         XC    SBBCLT,SBBCLT                                                    
         ICM   R1,15,SBADPTTB      CLEAR DAYPART TABLES BUFFER                  
         BZ    SP18                                                             
         LA    R0,36                                                            
         XC    0(180,R1),0(R1)                                                  
         LA    R1,180(R1)                                                       
         BCT   R0,*-10                                                          
         B     SP18                                                             
*                                                                               
SPX      J     EXIT                                                             
*                                                                               
         EJECT                                                                  
* GET AGENCY AND MEDIA                                                          
*                                                                               
GETAGYMD LR    R0,RE                                                            
         MVC   SBAGY,SBQAGY                                                     
         CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BE    GETAM1              YES-AGY/MED VALIDATION DONE                  
*                                                                               
         GOTOR VMEDGET,PARM,(SBMED,SBAGY),CDATAMGR,WORK                         
         CLI   8(R1),FF            TEST MEDIA NOT VALID                         
         JE    NEBR                                                             
         MVC   SBBAGYMD,WORK                                                    
         PACK  SBBAGY,SBBAGYMD                                                  
         NI    SBBAGY,X'0F'                                                     
         CLI   SBQMED,C'*'         TEST MEDIA ALL REQUEST                       
         BNE   *+12                                                             
         CLI   SBMED,C'T'                                                       
         BNE   *+14                                                             
         MVC   SBMEDNM,WORK+1                                                   
         B     GETAM0                                                           
         CLI   SBMED,C'R'          AND WE'RE ON RADIO NOW                       
         BNE   *+14                                                             
         MVC   SBMEDNMR,WORK+1     YES-SAVE NAME SEPARATLEY                     
         B     GETAM0                                                           
         CLI   SBMED,C'N'          SAME FOR NETWORK                             
         BNE   *+14                                                             
         MVC   SBMEDNMN,WORK+1                                                  
         B     GETAM0                                                           
         CLI   SBMED,C'X'          SAME FOR NETWORK                             
         BNE   GETAM0                                                           
         MVC   SBMEDNMX,WORK+1                                                  
*                                                                               
GETAM0   XC    IOKEY,IOKEY         READ AGENCY RECORD                           
         LA    R2,IOKEY                                                         
         USING AGYKEY,R2                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SBQAGY                                                   
         CLC   SBAGYREC(L'AGYKEY),AGYKEY  SKIP IF ALREADY READ                  
         BE    GETAM1                                                           
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(L'AGYKEY),IOKEYSAV                                         
         JNE   NEBR                                                             
         GOTOR (RF),IOSPTFIL+IOGET+IO1                                          
         L     R2,IOADDR                                                        
         MVC   SBAGYREC,0(R2)      SAVE RECORD IN BLOCK                         
*                                                                               
GETAM1   LA    R2,SBAGYREC                                                      
         MVC   COUNTRY,AGYPROF+7                                                
         MVI   CANADA,0                                                         
         MVI   CNETWK,0                                                         
         MVI   MARKET0,0                                                        
         MVI   QNETBITS,0                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   GETAM2                                                           
         MVI   CANADA,C'Y'         YES                                          
         CLI   SBMED,C'N'          TEST CANADIAN NETWORK REQUEST                
         BE    *+12                                                             
         CLI   SBMED,C'C'          OR COMBINED                                  
         BNE   GETAM2                                                           
         MVI   CNETWK,C'Y'         YES                                          
         TM    SBQCAN,SBQCMKT0     TEST MARKET 0 REQUEST                        
         BZ    *+8                                                              
         MVI   MARKET0,C'Y'        YES                                          
*                                                                               
         CLI   SBQNBITS,1          TEST NO NETWORK OR CABLE                     
         BNH   GETAM2              DON'T SET QNETBITS !                         
         MVC   QNETBITS,SBQNBITS   YES                                          
*                                                                               
GETAM2   OC    SBQDEMOS,SBQDEMOS   TEST DEMO MENU                               
         BZ    GETAM8                                                           
         XC    SBPDEMOS,SBPDEMOS                                                
         L     R4,SBCOMDEM         A(COMSCORE DEMO LIST)                        
         XC    0(SBCOMDLN,R4),0(R4) CLEAR COMSCORE DEMO LIST                    
*                                                                               
         XC    IOKEY,IOKEY         YES - GET DEMO MENU RECORD                   
         LA    R2,IOKEY                                                         
         USING DMNRECD,R2                                                       
         MVC   DMNKTYP,=X'0D26'                                                 
         MVC   DMNKAGMD,SBBAGYMD                                                
         MVC   DMNKCODE,SBQDEMOS                                                
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BNE   GETAM8                                                           
         L     R2,AIO1                                                          
         LA    R2,24(R2)                                                        
         LA    R3,SBPDEMOS                                                      
         SR    RF,RF                                                            
*                                                                               
GETAM4   CLI   0(R2),0             LOOK FOR DEMO ELEMENTS                       
         BE    GETAM8                                                           
         CLI   0(R2),5                                                          
         BNE   GETAM6                                                           
         USING DMNEL05,R2                                                       
         MVC   0(3,R3),DMNRTN                                                   
         CLI   2(R3),0             COMSCORE DEMO ON DEMO MENU?                  
         BNE   GETAM5              NO                                           
         MVC   0(8,R4),DMNRTGC     YES - MOVE IN COMSCORE DEMO                  
         LA    R4,8(R4)            BUMP TO NEXT COMSCORE DEMO SLOT              
*                                                                               
GETAM5   LA    R3,3(R3)                                                         
*                                                                               
GETAM6   IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     GETAM4                                                           
         DROP  R2                                                               
*                                                                               
GETAM8   MVI   SBMODE,SBPROCAG                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEBR                                                             
         J     EQBR                                                             
         EJECT                                                                  
* GET NEXT CLIENT                                                               
*                                                                               
GETNCLT  LR    R0,RE                                                            
         MVC   IOADDR,ACLTREC      CLIENT RECORD ADDRESS                        
         CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BE    NC22                YES-MUST BE SINGLE CLIENT                    
*                                                                               
         CLI   SBQCGRD,0           TEST CLIENT GROUPS                           
         BE    NC20                                                             
         ICM   R3,15,ACLTEL        YES-TEST FIRST TIME                          
         BNZ   NC16                NO-GET NEXT CLIENT IN GROUP                  
         LA    R2,IOKEY            YES-READ GROUP ID DEFINITION RECORD          
         USING GRPRECD,R2                                                       
         XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,SBBAGYMD                                                
         MVC   GRPKID,SBQCGRD                                                   
         LAY   R2,CGRPREC                                                       
         ST    R2,IOADDR                                                        
         GOTOR AIO,IOSPTFIL+IORD                                                
         JNE   NEBR                                                             
         LA    R3,GRPEL                                                         
         SR    RF,RF               GET BREAK DESCRIPTION                        
*                                                                               
NC2      CLI   0(R3),0                                                          
         BE    NC10                                                             
         CLI   0(R3),GRPBRKCQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     NC2                                                              
         USING GRPBRKD,R3                                                       
         MVC   SBCGR1BK,GRPBK1     BREAK NAMES                                  
         MVC   SBCGR2BK,GRPBK2                                                  
         LLC   R1,GRPBK1LN         BREAK LENGTHS                                
         STC   R1,SBCGR1LN                                                      
         LLC   RF,GRPBK2LN                                                      
         AR    RF,R1                                                            
         STC   RF,SBCGR2LN                                                      
         CLC   SBCGR1LN,SBCGR2LN   TEST 2 LEVELS                                
         BE    *+12                                                             
         BRAS  RE,SETMASK          YES-SET LEVEL 1 MASK                         
         STCM  R1,3,SBCG1MSK                                                    
*                                                                               
         XC    QCGRLEN,QCGRLEN                                                  
         CLC   SBQCGRF,BLANKS      TEST CLIENT GROUP FILTER                     
         BNH   NC6                                                              
         LA    R1,SBQCGRF+3        YES-WORK OUT NUMBER OF DIGITS                
         LA    RF,4                    FOR FILTERING                            
*                                                                               
NC4      CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,NC4                                                           
         ST    RF,QCGRLEN                                                       
*                                                                               
NC6      GOTOR AIO,IOSPTDIR+IOSQ   READ GROUP CODE RECORD                       
         LA    R2,IOKEY                                                         
         CLC   IOKEY(GRPKCODE-GRPKEY),IOKEYSAV                                  
         JNE   NEBR                                                             
         ICM   RF,15,QCGRLEN       TEST FILTERING                               
         BZ    NC8                                                              
         UNPK  DUB,GRPKCODE(3)     YES-                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SBQCGRF(0),DUB+3                                                 
         BNE   NC6                 FILTERED OUT-NEXT NEXT GROUP                 
*                                                                               
NC8      MVC   SBBCGR,GRPKCODE                                                  
         GOTOR AIO,IOSPTFIL+IOGET  READ THE RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         CLC   IOKEY(GRPKELCD-GRPKEY),IOKEYSAV TEST SAME CODE AS BEFORE         
         BE    NC12                                                             
         LA    R3,GRPEL            NO-NEW GROUP CODE                            
         SR    RF,RF               LOOK FOR GROUP CODE DESCRIPTION              
         MVC   SBCGR1NM,BLANKS                                                  
         MVC   SBCGR2NM,BLANKS                                                  
*                                                                               
NC10     CLI   0(R3),0                                                          
         BE    NC12                                                             
         CLI   0(R3),GRPGRPCQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     NC10                                                             
         USING GRPGRPD,R3                                                       
         MVC   SBCGR1NM,GRPGNAM1   GROUP NAMES                                  
         MVC   SBCGR2NM,GRPGNAM2                                                
         MVI   SBMODE,SBPROCCG     CLIENT GROUP FIRST                           
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEBR                                                             
*                                                                               
NC12     LA    R3,GRPEL            LOOK FOR CLIENT ELEMENTS                     
*                                                                               
NC14     CLI   0(R3),0                                                          
         BE    NC18                                                             
         CLI   0(R3),GRPVALCQ                                                   
         BNE   NC16                                                             
         USING GRPVALD,R3                                                       
         ST    R3,ACLTEL           SET A(CURRENT ELEMENT)                       
         MVC   SBCLT,GRPVALUE      SET THE CLIENT                               
         MVC   IOADDR,ACLTREC                                                   
         LA    R2,IOKEY                                                         
         B     NC24                AND READ CLIENT RECORD                       
*                                                                               
NC16     LLC   RF,1(R3)            NEXT CLIENT ELEMENT                          
         AR    R3,RF                                                            
         B     NC14                                                             
*                                                                               
NC18     LAY   R2,CGRPREC          READ HI FOR CURRENT RECORD                   
         MVC   IOKEY(13),0(R2)     TO RE-ESTABLISH READ SEQUENCE                
         GOTOR AIO,IOSPTDIR+IOHI                                                
         ST    R2,IOADDR                                                        
         B     NC6                                                              
*                                                                               
NC20     CLC   =C'ALL',SBQCLT                                                   
         BE    NC30                                                             
         CLI   SBQCLT,C'*'         TEST OFFICE REQUEST                          
         BE    NC30                                                             
         CLI   SBQCLT,C'$'         TEST OFFICE LIST                             
         BE    NC30                                                             
         CLI   SBQCLT,C'='         TEST CLIENT GROUP                            
         BE    NC30                                                             
         OC    SBCLT,SBCLT         TEST FIRST TIME                              
         JNZ   NEBR                                                             
*                                                                               
NC22     MVC   SBCLT,SBQCLT        SINGLE CLIENT                                
*                                                                               
NC24     GOTOR VCLPACK,PARM,SBCLT,SBBCLT                                        
         CLI   0(R1),0                                                          
         BNE   NC25                                                             
         LA    R2,IOKEY                                                         
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,SBBCLT                                                   
         ICM   R1,15,SBACLTRC      TEST A(CLIENT RECORD) SUPPLIED               
         BZ    *+14                                                             
         CLC   CKEY,0(R1)          YES-TEST RECORD ALREADY READ                 
         BE    NC26                YES                                          
         GOTOR AIO,IOSPTFIL+IORD                                                
         BE    NC26                                                             
*                                                                               
NC25     CLI   SBQCGRD,0           RECORD NOT FOUND-TEST CLIENT GROUPS          
         BH    NC16                YES-NEXT CLIENT IN GROUP                     
         J     NEBR                NO-STOP NOW                                  
*                                                                               
NC26     L     R2,IOADDR                                                        
         ZICM  RE,SBQEXCL,1        TEST CLIENT FILTER                           
         BZ    NC50                                                             
         MVC   BYTE,CPROF+13                                                    
         NI    BYTE,X'0F'                                                       
         N     RE,=F'15'                                                        
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    NC50                                                             
*                                                                               
         CLI   SBQCGRD,0           TEST CLIENT GROUPS                           
         BH    NC16                YES - NEXT CLT                               
         J     NEBR                NO-STOP NOW                                  
*                                                                               
NC26A    OC    SBTWAACS,SBTWAACS   ANY LIMITS?                                  
         BZ    NC50                 NO                                          
         CLI   SBTWAACS,C'*'       TEST OFFICE LOCKOUT                          
         BNE   NC27                 NO                                          
*                                                                               
*                                  IS IT A CLT GROUP LOCKOUT?                   
         CLI   SBTWAACS+1,C'A'                                                  
         BL    NC26B                                                            
         CLI   SBTWAACS+1,C'Z'                                                  
         BH    NC26B                                                            
         CLI   SBTWAACS+2,C'0'                                                  
         BL    NC26B                                                            
         CLI   SBTWAACS+2,C'9'                                                  
         BNH   NC50                 YES - ALL CLT'S IN GRP VALID                
*                                                                               
NC26B    LA    R1,CACCESS                                                       
         LA    RF,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    RF,1                                                             
*                                                                               
NC26C    CLC   SBTWAACS+1(1),0(R1)   TEST RIGHT OFFICE                          
         BE    NC50                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,NC26C                                                         
         B     NC29                                                             
*                                                                               
NC27     CLI   SBTWAACS,C'+'       TEST MKT LOCKOUT                             
         BE    NC50                 YES - NOT A RUNTIME ISSUE                   
         CLI   SBTWAACS,C'$'       TEST OFFICE LIST                             
         BE    NC28                                                             
*                                                                               
* SPOTIO NOT READING CLT LISTS YET - I THINK WE'RE ONLY HERE FOR                
* SINGLE CLIENT, AND SBBCLT=SBTWAACS SHOULD HAVE BEEN CHECKED AT                
* REQUEST TIME.                                                                 
         CLI   ONLINE,C'Y'         ONLINE?                                      
         BNE   NC50                 NO, SHOULD'VE BEEN CKED AT REQ TIME         
         CLC   SBTWAACS(2),SBBCLT  NONE OF ABOVE - MATCH CLIENT                 
         BNE   NC29                                                             
         B     NC50                                                             
*                                                                               
NC28     LA    R1,DUB              PROCESS A LIST OF OFFICES                    
         USING OFFICED,R1                                                       
         XC    DUB,DUB                                                          
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,SBTWAACS                                                 
         MVC   OFCAGY,SBAGY                                                     
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         GOTOR VOFFICER,PARM,DUB,SBCOMFAC                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BE    NC50                 YES                                         
NC29     OI    SBEFLAG,SBECLSK      NO - SET CLIENT SKIPPED FLAG                
         B     NC16                NEXT CLT                                     
*                                                                               
NC30     SR    RE,RE                                                            
         ICM   RE,3,SBBCLT         ALL CLIENT PROCESSING                        
         LA    RE,1(RE)                                                         
         STCM  RE,3,SBBCLT                                                      
         LA    R2,IOKEY                                                         
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,SBBCLT                                                   
         GOTOR AIO,IOSPTDIR+IOHI                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CKEY(CKEYCLT-CKEY),IOKEYSAV  CHECK SAME AGYMD                    
         JNE   NEBR                                                             
*                                                                               
         MVC   SBBCLT,CKEYCLT                                                   
         OC    CKEY+4(9),CKEY+4    TEST IT'S REALLY A CLIENT RECORD             
         BNZ   NC30                NO-READ NEXT CLIENT                          
*                                                                               
         GOTOR (RF),IOSPTFIL+IOGET                                              
         JNE   NEBR                                                             
         L     R2,IOADDR                                                        
         ZICM  RE,SBQEXCL,1        TEST CLIENT FILTER                           
         BZ    NC30B                                                            
         MVC   BYTE,CPROF+13                                                    
         NI    BYTE,X'0F'                                                       
         N     RE,=F'15'                                                        
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BNZ   NC30                                                             
*                                                                               
NC30B    CLI   SBQCLT,C'*'         TEST OFFICE REQUEST                          
         BNE   NC32                                                             
         BRAS  RE,OFCFILT                                                       
         BNE   NC30                                                             
         B     NC40                                                             
*                                                                               
NC34     CLI   SBQCLT,C'='         TEST CLIENT GROUP                            
         BNE   NC40                                                             
         CLC   CPROF+13(1),SBQCLT+1                                             
         BNE   NC30                                                             
*                                                                               
NC32     CLI   SBQCLT,C'$'         TEST OFFICE LIST                             
         BNE   NC34                                                             
         LA    R1,WORK             PROCESS A LIST OF OFFICES                    
         USING OFFICED,R1                                                       
         XC    WORK,WORK                                                        
         MVI   OFCSYS,C'S'                                                      
***      MVC   OFCAUTH,SBQCLT                                                   
         MVC   OFCLMT,SBQCLT                                                    
         MVC   OFCAGY,SBAGY                                                     
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
***      GOTOR VOFFICER,PARM,DUB,SBCOMFAC                                       
         GOTOR VOFFICER,PARM,(C'2',WORK),(0,SBCOMFAC)                           
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   NC30                                                             
*                                                                               
NC40     DS    0H                                                               
         TM    SBIOFLAG,SBXFILE    CROSS FILE READING?                          
         BZ    NC44                 NO                                          
         LA    RF,TROFC                                                         
         CLC   SBQAGY,=C'TR'                                                    
         BE    NC42                                                             
         LA    RF,NEOFC                                                         
         CLC   SBQAGY,=C'NE'                                                    
         BE    NC42                                                             
         LA    RF,GBOFC                                                         
         CLC   SBQAGY,=C'GB'                                                    
         BNE   NC44                                                             
*                                                                               
NC42     CLI   0(RF),X'FF'                                                      
         BE    NC30                SKIP THIS OFFICE                             
         CLC   COFFICE,0(RF)       INCLUDE THIS OFFICE?                         
         BE    NC44                 YES                                         
         LA    RF,1(RF)                                                         
         B     NC42                                                             
*                                                                               
NC44     GOTOR VCLUNPK,PARM,(CPROF+6,SBBCLT),SBCLT                              
*                                                                               
NC50     LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,SBTWAACS                                                 
         MVC   OFCLMT,SBTWAACS                                                  
         MVC   OFCAGY,SBAGY                                                     
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCACCSC,CACCESS                                                 
         MVC   OFCCLT,SBCLT                                                     
         MVC   OFCCLT2,SBBCLT                                                   
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCSAGMD,SBBAGYMD                                                
         MVI   OFCACCSM,X'FF'       IGNORE MKT LIMIT ACCESS FOR CLT             
*                                                                               
         L     RF,SBCOMFAC                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         USING MASTD,RF                                                         
         MVC   OFCSECD,MCASECRT                                                 
         DROP  R1,RF                                                            
         GOTOR VOFFICER,PARM,(C'2',WORK),(0,SBCOMFAC)                           
         CLI   0(R1),0              HAVE ACCESS?                                
         BE    NC51                 YES                                         
         OI    SBEFLAG,SBECLSK      NO - SET CLIENT SKIPPED FLAG                
         CLI   SBQCGRD,0            PROCESSING CLIENT GROUPS                    
         BH    NC16                 YES - GET NEXT CLT IN CGROUP RECORD         
         B     NC20                 PROCESS THE NEXT CLIENT                     
*                                                                               
NC51     MVC   SBCLTNM,CNAME                                                    
         MVC   SBCOFF,COFFICE                                                   
         MVC   SBCPROF,CPROF                                                    
         MVC   SBCEXTRA,CEXTRA                                                  
         CLI   SBCEXTRA+2,C'N'                                                  
         BNE   *+8                                                              
         MVI   SBCEXTRA+2,0         'N' MEANS 'NO'                              
         MVC   SBCIDTIT,CTITLE                                                  
         MVC   SBCACCOF,CACCOFC                                                 
         MVC   SBCACCES,CACCESS                                                 
         MVC   SBCLTIFC,CCLTIFC                                                 
         MVC   SBBCMCLT,CMCLTCOD                                                
         MVC   SBCMEDNM,CMEDNAME                                                
         MVC   SBCLTPW,CPWPCT                                                   
         MVC   SBC2FACT,CCOST2                                                  
         MVC   SBCPST,CPST                                                      
         MVC   SBCMPST,CMPST                                                    
*                                                                               
         MVC   SBUP1DES,CPU1       USER FIELD DEFINITIONS                       
         MVC   SBUP1TYP,CPU1TYPE                                                
         MVC   SBUP1LEN,CPU1LEN                                                 
         MVC   SBUP2DES,CPU2                                                    
         MVC   SBUP2TYP,CPU2TYPE                                                
         MVC   SBUP2LEN,CPU2LEN                                                 
         MVC   SBUE1DES,CEU1                                                    
         MVC   SBUE1TYP,CEU1TYPE                                                
         MVC   SBUE1LEN,CEU1LEN                                                 
         MVC   SBUE2DES,CEU2                                                    
         MVC   SBUE2TYP,CEU2TYPE                                                
         MVC   SBUE2LEN,CEU2LEN                                                 
         NI    SBEUDEF,X'FF'-SBEUPCOM-SBEUECOM-SBEUMCOM   RESET FLAG            
*                                                                               
         TM    SBQSKIP,SBQSKMED    TEST ACCOUNTING ONLY                         
         BO    NC52                                                             
         XC    DUB,DUB             NO-GET EQUIVALENCE RECORD                    
         MVC   DUB(2),SBAGY                                                     
         MVC   DUB+2(1),SBMED                                                   
         TM    SBIOFLAG,SBXFILE    CROSS FILE READING?                          
         BZ    *+8                                                              
         MVI   DUB+2,C'Z'                                                       
         MVC   DUB+3(2),SBBCLT     PACKED CLIENT CODE                           
         OC    SBBEQCLT,SBBEQCLT   TEST SPECIAL CLIENT FOR EQUIV TABLE          
         BZ    *+10                                                             
         MVC   DUB+3(2),SBBEQCLT   YES-USE THAT CLIENT                          
         GOTOR VEQVRD,PARM,DUB,SBDPEQTB,AIO3,CDATAMGR                           
         CLI   12(R1),0            TEST FOR ERROR                               
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         L     RE,AIO3                                                          
         MVC   SBEQTAB,0(RE)       EXTRACT EQUIVALENCE TABLES                   
*                                                                               
NC52     CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BE    NC53                YES-00 PROFILE MUST HAVE BEEN READ           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'    SPOT CONTROL PROFILE                         
         MVC   WORK+4(2),SBAGY                                                  
         MVC   WORK+6(1),SBQMED                                                 
         CLI   SBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   WORK+6(1),SBMED                                                  
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         GOTOR CGETPROF,PARM,WORK,SBSPPROF,CDATAMGR                             
*                                                                               
NC53     XC    SBDRPROF,SBDRPROF   CLEAR DAR PROFILE                            
         MVC   WORK(4),=C'SDAR'    GET B1 PROFILE                               
         NI    WORK,X'FF'-X'40'    MAKE THE S LOWERCASE                         
         GOTOR CGETPROF,PARM,WORK,SBDRPROF,CDATAMGR                             
*                                                                               
         TM    SBQSKIP,SBQSKMED    UNLESS ACCOUNTING ONLY,                      
         BO    NC54                                                             
         MVC   WORK(4),=C'S0D0'    GET D0 PROFILE                               
         GOTOR (RF),(R1),WORK,SBD0PROF,CDATAMGR                                 
         MVC   WORK(4),=C'S01W'    GET 1W PROFILE                               
         GOTOR (RF),(R1),WORK,SB1WPROF,CDATAMGR                                 
         CLI   SBEDMA,0            ANY DMA OVERRIDE                             
         BE    *+10                 NO                                          
         MVC   SB1WPROF+5(1),SBEDMA                                             
         CLI   SBEWDP,0            ANY WEEKLY DATA FOR POSTS OVERRIDE?          
         BE    *+10                NO                                           
         MVC   SB1WPROF+15(1),SBEWDP                                            
*                                                                               
         XC    SBSTPROF,SBSTPROF   CLEAR ST PROFILE                             
         MVC   SBSTPROF(3),=C'CME' SET DEFAULT TO CLT/MKT/EST                   
         MVC   WORK(4),=C'S0ST'    GET ST PROFILE                               
         GOTOR (RF),(R1),WORK,SBSTPROF,CDATAMGR                                 
*                                                                               
         XC    SBDFPROF,SBDFPROF   CLEAR DF PROFILE                             
         MVC   WORK(4),=C'S0DF'    GET DF PROFILE                               
         GOTOR (RF),(R1),WORK,SBDFPROF,CDATAMGR                                 
*                                                                               
         XC    SBB1XPRF,SBB1XPRF   CLEAR B1X PROFILE                            
         MVC   WORK(4),=C'SB1X'    GET B1X PROFILE                              
         NI    WORK,255-X'40'                                                   
         GOTOR (RF),(R1),WORK,SBB1XPRF,CDATAMGR                                 
*                                                                               
         XC    SBB1PROF,SBB1PROF   CLEAR B1 PROFILE                             
         MVC   WORK(4),=C'S0B1'    GET B1 PROFILE                               
         GOTOR (RF),(R1),WORK,SBB1PROF,CDATAMGR                                 
*                                                                               
NC54     CLI   SBQMGOPT,C'Y'       TEST MG IN MISSED MON ACTIVE                 
         BNE   NC60                NO                                           
         CLI   CEXTRA+7,C'Y'       TEST OPTION VALID FOR CLIENT                 
         BNE   NC60                NO                                           
         MVC   WORK(4),=C'S0A0'                                                 
         GOTOR CGETPROF,PARM,WORK,WORK+16,CDATAMGR                              
         CLI   8(R1),0             TEST PROFILE FOUND                           
         BNE   NC60                                                             
         GOTOR CDATCON,PARM,(3,WORK+23),(2,SBBMGST)                             
*                                                                               
NC60     MVI   SBMODE,SBPROCCL                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEBR                                                             
         TM    SBIOFLAG,SBSKPCLT                                                
         BZ    NCX                                                              
         NI    SBIOFLAG,X'FF'-SBSKPCLT                                          
         B     NC20                                                             
*                                                                               
NCX      J     EQBR                                                             
         DROP  R2                                                               
         EJECT                                                                  
* DETERMINE REQUEST FILTERS - MKT/STA,MKTGRP                                    
*                                                                               
FILTERS  LR    R0,RE                                                            
*                                                                               
         XC    QBMKTST,QBMKTST     MARKET AND STATION FILTERS                   
         XC    QBMKT,QBMKT                                                      
         MVC   QBMKTND,XFF                                                      
         XC    QBSTA,QBSTA                                                      
         XC    QBNET,QBNET                                                      
         XC    QBSTACAN,QBSTACAN                                                
*                                                                               
         CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP REQUEST                
         BNH   FL10                                                             
         LA    R1,SBQMGRF+L'SBQMGRF-1 COMPUTE NUM OF DIGITS FOR                 
         LA    RF,L'SBQMGRF        MKTGRP FILTERING                             
*                                                                               
FL5      CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,FL5                                                           
*                                                                               
         STCM  RF,3,QMGRLEN                                                     
*                                                                               
FL10     TM    SBIOFLAG,SBXFILE    READ ACROSS FILES?                           
         BNZ   FL20                 YES - ALWAYS ALL MKTS                       
         OC    SBQMKT,SBQMKT       TEST FOR DEFAULT=ALL MKTS                    
         BZ    FL12                                                             
         CLC   SBQMKT(3),=C'ALL'                                                
         BE    FL12                                                             
         PACK  DUB,SBQMKT                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,QBMKTST                                                     
         STCM  R1,3,QBMKT                                                       
         TM    SBQRDOPT,SBQROMAO   TEST READ 'AS OF' REQUESTED MKT              
         BO    FL12                YES-DON'T SET END MKT = START MKT            
         STCM  R1,3,QBMKTND                                                     
*                                                                               
FL12     OC    SBQBMKTX,SBQBMKTX   TEST UPPER MARKET LIMIT                      
         BZ    FL20                                                             
         MVC   QBMKTND,SBQBMKTX    YES                                          
*                                                                               
FL20     OC    SBQSTA,SBQSTA       TEST FOR DEFAULT=ALL STAS                    
         BZ    FL30                                                             
         CLC   =C'ALL',SBQSTA                                                   
         BE    FL30                                                             
         XC    DUB,DUB                                                          
         MVC   DUB(5),SBQSTA                                                    
         MVC   FULL,SBQMKT                                                      
         CLI   MARKET0,C'Y'                                                     
         BE    *+14                                                             
         OC    QBMKT,QBMKT                                                      
         BNZ   *+10                                                             
         MVC   FULL,=C'0000'                                                    
         GOTOR AMSPACK,PARM,FULL,DUB,WORK                                       
         MVC   QBMKTSTA,WORK                                                    
*                                                                               
FL24     CLI   SBMED,C'R'          TEST NOT RADIO                               
         BE    FL30                                                             
         CLI   CANADA,C'Y'         AND CANADA                                   
         BNE   FL30                                                             
         CLI   QNETBITS,0          YES-TEST NETWORK BITS SET                    
         BE    FL26                                                             
         MVC   QBSTA+2(1),QNETBITS   <=== NETWORK BITS NOW FULL BYTE            
         B     FL30                                                             
*                                                                               
FL26     MVC   QBSTACAN,QBSTA      NO-SET CANADIAN STATION FILTER               
*                                                                               
FL30     OC    SBQNET,SBQNET       TEST CANADIAN NETWORK FILTER                 
         BZ    FL40                                                             
         CLC   SBQNET(3),=C'ALL'                                                
         BE    FL40                                                             
         MVC   FULL,=C'0000'       YES                                          
         MVC   DUB(4),SBQNET                                                    
         MVI   DUB+4,C'T'                                                       
         OI    FLAG2,STPNODIE                                                   
         GOTOR AMSPACK,PARM,FULL,DUB,WORK                                       
         MVC   QBNET,WORK+2                                                     
*                                                                               
FL40     J     EQBR                                                             
         DROP  RB                                                               
         EJECT                                                                  
* BUILD MKTGRP ASSIGNMENT TABLE AND PRODUCT BUFFER                              
*                                                                               
BLDBUFF  NTR1  BASE=*,LABEL=*                                                   
*                                  BUILD PRODUCT BUFFER                         
BLDPRDBF CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BNE   BG2                                                              
         MVC   SBPRD,SBQPRD        YES-MUST BE SINGLE PRODUCT                   
         MVC   SBBPRD,SBQBPRD                                                   
         B     BG68                                                             
*                                                                               
BG2      TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    BG2A                NOPE                                         
         XC    POLUDEF1,POLUDEF1   CLEAR POL UDEF1                              
         XC    POLUDEF2,POLUDEF2   CLEAR POL UDEF2                              
         XC    IOKEY,IOKEY         READ POL PRD RECORD FOR THIS CLIENT          
         LA    R2,IOKEY                                                         
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,=C'POL'                                                  
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BNE   BG2A                                                             
         L     R2,AIO1                                                          
         MVC   POLUDEF1,PUSER1     SAVE POL'S UDEF1                             
         MVC   POLUDEF2,PUSER2     SAVE POL'S UDEF2                             
*                                                                               
BG2A     ICM   R3,15,SBAPRDBF      BUILD PRODUCT BUFFER                         
         BNZ   *+12                                                             
         LA    R4,SBQPRD                                                        
         B     BG61                                                             
         USING PRDBUFFD,R3                                                      
         LA    RF,PRDBUFFL         CLEAR                                        
         SLL   RF,8                256 PRODUCTS                                 
         XCEF  (R3),(RF)                                                        
*                                                                               
         CLI   SBEEFCST,C'Y'       TEST EFFECTIVE COSTS REQUESTED               
         BNE   BG55                                                             
         XC    SBBILFOR,SBBILFOR   YES - GET DEFAULT BILL FORMULA               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,=C'AAA'                                                  
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BNE   BG55                                                             
         L     R2,AIO1                                                          
         MVC   SBBILBAS,PBILLBAS                                                
         MVC   SBBILCOM,PBILLCOM                                                
*                                                                               
BG55     L     R4,ACLTREC          FIND PRODUCTS FROM CLTHDR                    
         LA    R4,CLIST-CLTHDRD-4(R4)                                           
         SR    R5,R5               R5=SEQUENCE NUMBER IN CLIST                  
*                                                                               
BG60     LA    R4,4(R4)                                                         
         LA    R5,1(R5)                                                         
         CLI   0(R4),C' '                                                       
         BNH   BG68                                                             
*                                                                               
BG61     XC    IOKEY,IOKEY         READ PRODUCT RECORD                          
         LA    R2,IOKEY                                                         
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,0(R4)                                                    
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BNE   BG60                PRODUCT RECORD NOT FOUND                     
         L     R2,AIO1                                                          
         ICM   R3,15,SBAPRDBF                                                   
         BNZ   *+14                                                             
         MVC   QPNAME,PNAME                                                     
         B     BG66                                                             
         LLC   RE,3(R4)                                                         
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         AR    R3,RE                                                            
         STC   R5,PBSEQ            SAVE SEQUENCE NUMBER                         
         MVC   PBBCODE,3(R4)       PRODUCT CODE                                 
***                                                                             
* FIX BUG WHERE SBQBPRD GET SET ONCE IN ONLINE IN SPWRI0C AND FOR AN            
* ALL MEDIA/SINGLE PRODUCT REQUEST THE WRITER WILL ONLY REPORT MEDIA T          
***                                                                             
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   BG62                NO                                           
         CLI   SBQBPRD,0           HAVE A SINGLE PRODUCT FILTER?                
         BE    BG62                YES                                          
         CLI   SBQBPRD,X'FF'       HAVE A POL PRODUCT?                          
         BE    BG62                YES                                          
         CLC   SBQPRD,0(R4)        MATCH ON PRODUCT?                            
         BNE   BG62                NO                                           
         MVC   SBQBPRD,3(R4)       UPDATE SBQBPRD!                              
*                                                                               
BG62     MVC   PBALPH,0(R4)        PRODUCT ALPHA                                
         MVC   PBNAME,PNAME        PRODUCT NAME                                 
         MVC   PBINT,PACCT         PRODUCT INTERFACE CODE                       
         MVC   PBGSTCD,PGSTCODE    PRODUCT'S CANADIAN GST CODE                  
         MVC   PBUFLD1,PUSER1      PRODUCT'S USER FIELD 1                       
         MVC   PBUFLD2,PUSER2      PRODUCT'S USER FIELD 2                       
         TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    *+16                NOPE                                         
         MVC   PBUFLD1,POLUDEF1    SAVE POL'S UDEF1 TO BUFFER                   
         MVC   PBUFLD2,POLUDEF2    SAVE POL'S UDEF2 TO BUFFER                   
         TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB                                   
         BZ    BG63                                                             
         XC    PBUFLD2,PBUFLD2     CLEAR USER2 FIELD                            
         MVC   PBUFLD2(10),PPST    NO USER2 WHEN EXTRACTING PST                 
         MVC   PBUFMPST,PMPST      MAIN PST                                     
*                                                                               
BG63     CLI   SBEEFCST,C'Y'       TEST EFFECTIVE COSTS REQUESTED               
         BNE   BG66                                                             
         MVC   PBBILBAS,SBBILBAS   YES - SET PRODUCT BILL FORMULA               
         MVC   PBBILCOM,SBBILCOM                                                
         CLI   PBBCODE,FF          IF PRD=POL, USE PRD=AAA FORMULA              
         BE    BG66                                                             
         CLI   PBILLBAS,0          TEST PRODUCT'S BILL FORMULA SET              
         BNE   *+14                                                             
         OC    PBILLCOM,PBILLCOM                                                
         BZ    BG66                NO-USE AAA FORMULA                           
         MVC   PBBILBAS,PBILLBAS                                                
         MVC   PBBILCOM,PBILLCOM                                                
*                                                                               
BG66     OC    SBAPRDBF,SBAPRDBF   TEST FOR PRODUCT BUFFER                      
         BNZ   BG60                YES                                          
*                                                                               
BG68     CLI   SBQPGR,C' '         TEST FOR PRODUCT GROUPS                      
         BNH   BG90                NO                                           
*                                                                               
         CLC   SBQPGRF,BLANKS      TEST PRODUCT GROUP FILTER                    
         BNH   BG70                NO                                           
         LA    R1,SBQPGRF+L'SBQPGRF-1  WORK OUT NUM OF DIGITS FOR               
         LA    RF,L'SBQPGRF            PRODUCT GROUP FILTERING                  
*                                                                               
BG69     CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,BG69                                                          
         STCM  RF,3,QPGRLEN                                                     
*                                                                               
BG70     XC    IOKEY,IOKEY         SET PRODUCT GROUP ASSIGNS                    
         LA    R2,IOKEY                                                         
         USING PRGKEY,R2                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SBBAGYMD                                                
         MVC   PRGKCLT,SBBCLT                                                   
         MVC   PRGKID,SBQPGR                                                    
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         JNE   NEQEXIT             NOT FOUND - SKIP CLT                         
*                                                                               
BG71     MVC   PRGPTYP,=X'0D81'    SET FOR PRDGRP PASSIVES                      
         L     R3,AIO1                                                          
         LA    R3,24(R3)                                                        
         SR    RF,RF                                                            
*                                                                               
BG72     CLI   0(R3),0                                                          
         BE    BG74                                                             
         CLI   0(R3),1             PRDGRP BREAK DESCRIPTION                     
         BNE   BG73                                                             
         USING PRGEL01,R3                                                       
         MVC   SBPGR1BK,PRGBK1                                                  
         MVC   SBPGR2BK,PRGBK2                                                  
         LLC   RE,PRGBK1LN                                                      
         STC   RE,SBPGR1LN                                                      
         LLC   RF,PRGBK2LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBPGR2LN                                                      
         XC    SBPG1MSK,SBPG1MSK   SET MASK FOR PRODUCT GROUPS                  
         CLC   SBPGR2LN,SBPGR1LN   TEST 2 LEVELS OF PRDGRP                      
         BE    BG74                NO                                           
         LLC   R1,SBPGR1LN                                                      
         BRAS  RE,SETMASK                                                       
         STCM  R1,3,SBPG1MSK                                                    
         B     BG74                                                             
*                                                                               
BG73     LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     BG72                                                             
         DROP  R3                                                               
*                                                                               
BG74     LA    R1,IOSPTDIR+IOHI                                                 
         B     BG75+4                                                           
*                                                                               
BG75     LA    R1,IOSPTDIR+IOSQ                                                 
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PRGKEY(PRGPGRP-PRGKEY),IOKEYSAV                                  
         BNE   BG90                                                             
         L     RF,ACLTREC          FIND PRODUCT CODE FROM CLTHDR                
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
BG76     CLC   0(3,RF),PRGPPRD                                                  
         BE    BG80                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    BG76                                                             
         B     BG75                IGNORE INVALID PRODUCTS                      
*                                                                               
BG80     CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTER                           
         BNH   BG82                NO                                           
         UNPK  DUB,PRGPGRP(3)      YES -                                        
         SR    RE,RE                                                            
         ICM   RE,3,QPGRLEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SBQPGRF(0),DUB+3                                                 
         BNE   BG75                                                             
*                                                                               
BG82     LLC   RE,3(RF)            PRODUCT CODE                                 
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R3,SBAPRDBF                                                      
         AR    R3,RE                                                            
         MVC   PBGROUP-PRDBUFFD(L'PBGROUP,R3),PRGPGRP                           
*                                                                               
         CLC   PRGKEY(PRGPPRD-PRGKEY),IOKEYSAV  TEST NEW PRODUCT GROUP          
         BE    BG75                                                             
         MVC   SBBPGR,PRGPGRP                                                   
         GOTOR AGETPGR             GET PRODUCT GROUP RECORD                     
         MVI   SBMODE,SBPROCPG     PRODUCT GROUP FIRST                          
         BRAS  RE,GO               HOOK TO USER                                 
         BNE   BMX                                                              
         B     BG75                                                             
*                                                                               
BG90     B     BLDMGR                                                           
         EJECT                                                                  
BLDMGR   CLI   SBQMGR,0            TEST MKTGRP REQUEST                          
         BE    BMX                                                              
*                                                                               
         L     RE,SBAMGTAB                                                      
         L     RF,=F'20000'        CLEAR TABLES                                 
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ICM   RE,15,SBAMGTB2                                                   
         BZ    BM2                                                              
         L     RF,=F'20000'                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
BM2      XC    IOKEY,IOKEY         READ MARKET GROUP DEFINITION RECORD          
         LA    R2,IOKEY                                                         
         USING MKGRECD,R2                                                       
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SBBAGYMD                                                
         MVC   MKGKMID,SBQMGR                                                   
         CLI   SBQMGRD,C' '        ID'S A-F REQUIRE CLIENT                      
         BL    BM4                                                              
         CLI   SBQMGR,C'F'         ID'S A-F REQUIRE CLIENT                      
         BH    BM4                                                              
         MVC   MKGKCLT,SBBCLT                                                   
         OC    SBQMGCLT,SBQMGCLT                                                
         BZ    BM4                                                              
         MVC   MKGKCLT,SBQMGCLT    ALTERNATE CLIENT                             
*                                                                               
BM4      DS    0H                                                               
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BE    BM5                                                              
         CLI   SBQMED,C'*'                                                      
         JE    NEQEXIT                                                          
         DC    H'0'                                                             
*                                                                               
BM5      MVC   MKGPTYP,=X'0D82'    SET FOR MKTGRP PASSIVES                      
*                                                                               
         OC    SBQMGCLT,SBQMGCLT                                                
         BZ    *+10                                                             
         MVC   MKGPCLT,SBQMGCLT    ALTERNATE CLIENT                             
*                                                                               
         CLC   =C'ALL',SBQCLT                                                   
         BE    *+18                                                             
         MVI   HALF,C'*'           BUILD OFFICE OVERRIDE                        
         L     RE,ACLTREC                                                       
         MVC   HALF+1(1),COFFICE-CLTHDRD(RE)                                    
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,24(R3)                                                        
         SR    RF,RF                                                            
         LA    R4,SBPGRPEX         R4=A(PRDGRP EXCEPTION LIST)                  
         XC    0(2*SBEXMAX,R4),0(R4)                                            
         LA    R5,SBEXMAX+1        R5=MAX N'PRDGRP EXCEPTIONS                   
*                                                                               
BM10     CLI   0(R3),0                                                          
         BE    BM30                                                             
         CLI   0(R3),1             MKTGRP BREAK DESCRIPTION                     
         BNE   BM14                                                             
         USING MKGEL01,R3                                                       
         CLI   MKGPGA,C'Y'         TEST MKTGRPS BY PRDGRPS                      
         BNE   BM11                                                             
         CLI   SBQMGR,C'F'         YES-MUST BE CLIENT SPECIFIC                  
         BH    BM11                                                             
         CLI   SBQPGR,C' '         TEST FOR PRODUCT GROUPS                      
         BNH   BM11                                                             
         MVC   MKGPPID,SBQPGR      YES-PRDGRP ID GOES IN KEY                    
*                                                                               
BM11     MVC   SBMGR1BK,MKGBK1                                                  
         MVC   SBMGR2BK,MKGBK2                                                  
         MVC   SBMGR3BK,MKGBK3                                                  
         LLC   RE,MKGBK1LN                                                      
         STC   RE,SBMGR1LN                                                      
         LLC   RF,MKGBK2LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBMGR2LN                                                      
         IC    RF,MKGBK3LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBMGR3LN                                                      
         DROP  R3                                                               
*                                                                               
         XC    SBMG2MSK,SBMG2MSK   SET MASKS FOR MKTGRPS                        
         CLC   SBMGR3LN,SBMGR2LN   TEST 3 LEVELS OF MKTGRP                      
         BE    BM12                NO                                           
         LLC   R1,SBMGR2LN                                                      
         BRAS  RE,SETMASK                                                       
         STCM  R1,3,SBMG2MSK                                                    
*                                                                               
BM12     XC    SBMG1MSK,SBMG1MSK                                                
         CLC   SBMGR2LN,SBMGR1LN   TEST 2 LEVELS OF MKTGRP                      
         BE    BM20                                                             
         LLC   R1,SBMGR1LN                                                      
         BRAS  RE,SETMASK                                                       
         STCM  R1,3,SBMG1MSK                                                    
         B     BM20                                                             
*                                                                               
BM14     CLI   0(R3),2             CLIENT EXCEPTION ELEMENT                     
         BNE   BM20                                                             
         CLI   SBQMGR,C'F'                                                      
         BNH   BM16                                                             
         CLC   =C'ALL',SBQCLT                                                   
         BE    BM20                                                             
         CLC   SBQCLT,2(R3)        MATCH UNPACKED CLIENT CODE                   
         BNE   *+14                                                             
         MVC   MKGPCLT,SBBCLT      MOVE PACKED CLT CODE TO KEY                  
         B     BM20                                                             
         OC    MKGPCLT,MKGPCLT                                                  
         BNZ   BM20                                                             
         CLC   HALF,2(R3)          MATCH OFFICE CODE                            
         BNE   BM20                                                             
         MVC   MKGPCLT,HALF        AND USE IT UNLESS CLT MATCH                  
*                                                                               
BM16     CLC   SBQPGRD,2(R3)       TEST EXCEPTION PGRP ID = REQUEST             
         BNE   BM20                                         PGRP ID             
         BCT   R5,*+6              YES-                                         
         DC    H'0'                                                             
         MVC   0(2,R4),3(R3)       MOVE TO PGRP EXCEPTION ID LIST               
         LA    R4,2(R4)                                                         
*                                                                               
BM20     LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     BM10                                                             
*                                                                               
BM30     L     R4,SBAMGTAB                                                      
         MVC   WORK(13),IOKEY      SAVE KEY                                     
         SR    R3,R3                                                            
*                                                                               
BM35     LA    R1,IOSPTDIR+IOHI                                                 
         B     BM41                                                             
*                                                                               
BM40     LA    R1,IOSPTDIR+IOSQ                                                 
*                                                                               
BM41     GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MKGKEY(MKGPMGRP-MKGKEY),IOKEYSAV                                 
         BNE   BM44                                                             
         CLC   SBQMGRF,BLANKS      TEST MKTGRP FILTER                           
         BNH   BM42                                                             
         UNPK  DUB,MKGPMGRP(3)     YES -                                        
         SR    RE,RE                                                            
         ICM   RE,3,QMGRLEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SBQMGRF(0),DUB+3                                                 
         BNE   BM40                                                             
*                                                                               
BM42     LTR   R4,R4               TEST A(MKTGRP TABLE) SET YET                 
         BZ    BM43                NO-HOPEFULLY THE HOOK WILL GET IT            
         SR    RE,RE                                                            
         ICM   RE,3,MKGPMKT        GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         AR    RE,R4               + TABLE START                                
         MVC   0(2,RE),MKGPMGRP    MOVE MKTGRP NUMBER                           
*                                                                               
         CLC   MKGKEY(MKGPMKT-MKGKEY),IOKEYSAV    TEST NEW MARKET GROUP         
         BE    BM40                                                             
*                                                                               
BM43     GOTOR AGETMGR             YES - GET MKTGRP RECORD                      
         MVC   SBBMGR,MKGPMGRP                                                  
         MVC   SBBPGR,MKGPPGRP                                                  
         MVI   SBMODE,SBPROCMG     MARKET GROUP FIRST                           
         BRAS  RE,GO               HOOK TO USER                                 
         BNE   BMX                                                              
         LTR   R4,R4               TEST A(MKTGRP TABLE) SET                     
         BNZ   BM40                                                             
         LTR   R3,R3               NO-CHECK WE'RE DOING PGRP EXCEPTIONS         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,0(R3)         GET A(MKTGRP TABLE)                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,MKGPMKT        GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         AR    RE,R4               + TABLE START                                
         MVC   0(2,RE),MKGPMGRP    MOVE MKTGRP NUMBER                           
         B     BM40                                                             
*                                                                               
BM44     OC    SBPGRPEX(2),SBPGRPEX  TEST PRODUCT GROUP EXCEPTION(S)            
         BZ    BM50                NO-DONE                                      
         L     R1,SBAMGTAB         YES-TEST FIRST EXCEPTION                     
         CR    R1,R4                                                            
         BNE   BM46                                                             
         LA    R3,SBAMGTB2         YES-R3=A(MKTGRP TABLE ADDRESSES)             
         LA    R5,SBPGRPEX             R5=A(PRDGRP EXCEPTION LIST)              
         LA    R0,SBEXMAX              R0=MAX N'EXCEPTIONS                      
         B     BM48                                                             
*                                                                               
BM46     LA    R3,4(R3)                                                         
         LA    R5,2(R5)                                                         
         OC    0(2,R5),0(R5)       TEST ANY MORE EXCEPTIONS                     
         BZ    BM50                                                             
         BCT   R0,BM48             YES-                                         
         B     BM50                                                             
*                                                                               
BM48     L     R4,0(R3)            R4=A(CURRENT MKTGRP TABLE)                   
         MVC   MKGKEY,WORK         RESTORE PASSIVE KEY                          
         MVC   MKGPPID,SBQPGRD                                                  
         MVC   MKGPPGRP,0(R5)                                                   
         XC    MKGPMGRP(4),MKGPMGRP                                             
         B     BM35                                                             
*                                                                               
BM50     B     BMX                                                              
*                                                                               
BMX      J     EQEXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
* BUILD PRODUCT/ESTIMATE TABLE                                                  
*                                                                               
PRDEST   NTR1  BASE=*,LABEL=*                                                   
         MVI   SVPRD,0                                                          
*                                                                               
         BRAS  RE,BPCT                                                          
*                                                                               
         ICM   R1,15,SBAESTTB      CLEAR ESTIMATE TABLE                         
         BNZ   PE2                                                              
         XC    ESTTAB,ESTTAB                                                    
         ICM   R1,15,SBASVETB      TEST SAVED 256-BYTE ESTIMATE TABLE           
         BZ    *+14                                                             
         MVC   ESTTAB,0(R1)        YES-USE THAT AND EXIT                        
         B     PEX                                                              
         LA    R1,ESTTAB           NO-SAVE A(ESTIMATE TABLE)                    
         ST    R1,SBASVETB                                                      
         B     PE20                                                             
*                                                                               
PE2      LA    RF,256                                                           
*                                                                               
PE10     XC    0(256,R1),0(R1)     CLEAR 256 BYTES FOR EACH BRAND               
         LA    R1,256(R1)                                                       
         BCT   RF,PE10                                                          
*                                                                               
         ICM   R1,15,SBAESTBF                                                   
         BZ    PE20                                                             
         LHI   RF,256                                                           
         XC    0(ESTBUFFL,R1),0(R1)                                             
         LA    R1,ESTBUFFL(R1)                                                  
         BCT   RF,*-10                                                          
*                                                                               
PE20     MVI   ESTFOUND,C'N'                                                    
         L     R2,ACLTREC          START READING THE DIRECTORY                  
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(4),0(R2)      A-M/CLT                                      
         MVI   IOKEY+4,C'A'        FORCE PAST CLTHDR                            
         LA    R3,3                                                             
         CLI   SBQBPRD,0           TEST FOR PRODUCT FILTER                      
         BE    PE22                                                             
         LA    R3,6                YES                                          
         MVC   IOKEY+4(3),SBQPRD                                                
         CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BE    PE46                YES-ASSUME PRD RECORD ALREADY READ           
*                                                                               
PE22     DS    0H                                                               
         GOTOR AIO,IOSPTDIR+IOHI   PRODUCT HEADER                               
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    R3,KEYCOMP                                                       
         BNE   PEX                                                              
         B     PE40                                                             
*                                                                               
PE30     LA    R1,IOSPTDIR+IOHI    READ HIGH                                    
         B     PE35+4                                                           
*                                                                               
PE35     LA    R1,IOSPTDIR+IOSQ    READ SEQUENTIAL                              
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    R3,KEYCOMP                                                       
         BNE   PE90                                                             
*                                                                               
PE40     OC    IOKEY+8(5),IOKEY+8  TEST BILL                                    
         BNZ   PE70                YES-NEXT ESTIMATE                            
         CLI   IOKEY+7,0           TEST EST                                     
         BNE   PE47                YES                                          
         MVI   FRSTEST,C'Y'        NO - PRODUCT HEADER                          
         L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PE42     CLC   0(3,RF),IOKEY+4                                                  
         BE    PE43                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    PE42                                                             
         B     PE80                INVALID PRODUCT                              
*                                                                               
PE43     LLC   RE,3(RF)            INDEX INTO PRODUCT BUFFER                    
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BNZ   PE44                                                             
         MVC   SBPRD,SBQPRD        SET PRODUCT DATA                             
         MVC   SBBPRD,SBQBPRD                                                   
         MVC   SBPRDNM,BLANKS                                                   
         MVC   SBPRDNM(L'QPNAME),QPNAME                                         
         B     PE46                                                             
*                                                                               
PE44     AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         CLC   SBQPGR,BLANKS      CHECK FOR PRDGRP FILTER                       
         BNH   PE45                                                             
         XC    SBBPGR,SBBPGR                                                    
         CLC   IOKEY+4(3),=C'POL'  YES-ALLOW PRD=POL                            
         BE    PE45                                                             
         OC    PBGROUP,PBGROUP                                                  
         BZ    PE80                PRD NOT IN PRDGRP - READ NEXT PRD            
         MVC   SBBPGR,PBGROUP      SET PRD GRP                                  
*                                                                               
PE45     MVC   SBPRD,PBALPH        SET PRODUCT DATA                             
         MVC   SBBPRD,PBBCODE                                                   
         MVC   SBPRDNM,BLANKS                                                   
         MVC   SBPRDNM(L'PBNAME),PBNAME                                         
         MVC   BILLFORM,PBBILFOR   SAVE PRODUCT BILL FORMULA                    
         DROP  R1                                                               
*                                                                               
PE46     MVI   SBMODE,SBPROCPR                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
         CLI   ONLINE,C'Y'         TEST ONLINE                                  
         BNE   PE35                NO-READ SEQUENTIAL                           
         MVI   FRSTEST,C'Y'        YES-                                         
         MVC   IOKEY+7(1),SBQEST   READ HIGH FOR START EST                      
         CLC   SBQEST,SBQESTND     UNLESS SINGLE EST REQUEST AND                
         BNE   PE30                THE ESTIMATE RECORD IS PROVIDED              
         ICM   RE,15,SBAESTRC                                                   
         BZ    PE30                                                             
         CLC   0(8,RE),IOKEY                                                    
         BNE   PE30                                                             
         SR    RF,RF               MOVE ESTIMATE RECORD TO IO AREA              
         ICM   RF,3,ELEN-ESTHDRD(RE)  MUCH SAFER WAY TO GET L'EST REC           
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,AIO1                                                          
         B     PE55                                                             
*                                                                               
PE47     CLC   SBQEST,IOKEY+7      CHECK ITS WITHIN REQ EST RANGE               
         BL    PE48                                                             
         BE    PE50                                                             
         MVC   IOKEY+7(1),SBQEST   SKIP TO FIRST EST                            
         XC    IOKEY+8(5),IOKEY+8                                               
         B     PE30                                                             
*                                                                               
PE48     CLC   SBQESTND,IOKEY+7                                                 
         BL    PE80                SKIP TO NEXT PRODUCT                         
*                                                                               
PE50     GOTOR AIO,IOSPTFIL+IOGET+IO1   READ ESTIMATE RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING ESTHDRD,R2                                                       
*                                                                               
         TM    SBEFLAG4,SBE4YLOK   WANT ONLY LOCKED ESTIMATE DATA?              
         BZ    PE50A               NO                                           
         TM    IOKEY+13,X'08'      YES - HAVE EST STATUS LOCK?                  
         BNZ   PE50B               YES                                          
         OC    ELOCKYM,ELOCKYM     NO  - HAVE EST YYMM LOCK?                    
         BZ    PE70                NO  - GET THE NEXT ESTIMATE                  
*                                                                               
PE50A    TM    SBEFLAG4,SBE4NLOK   WANT ONLY UNLOCKED ESTIMATES?                
         BZ    PE50B               NO                                           
         TM    IOKEY+13,X'08'      YES - STATUS LOCK?                           
         BNZ   PE70                YES - GET THE NEXT ESTIMATE                  
         OC    ELOCKYM,ELOCKYM     NO  - YYMM LOCK?                             
         BNZ   PE70                YES - GET THE NEXT ESTIMATE                  
*                                                                               
PE50B    OC    SBQSTART,SBQSTART   TEST FOR REQUEST DATES                       
         BZ    PE51                NO                                           
         TM    SBQRDOPT,SBQROEST   YES-TEST READ ALL ESTIMATES ANYWAY           
         BO    PE51                                                             
         CLC   EEND,SBQSTART       EST END BEFORE REQ START                     
         BL    PE70                                                             
         CLC   ESTART,SBQEND       EST START AFTER REQ END                      
         BH    PE70                                                             
         CLI   ESTFOUND,C'Y'       TEST ANY ESTIMATES FOUND YET                 
         BE    PE51                                                             
         CLI   EOWSDAY,1           NO-TEST OUT-OF-WEEK-ROTATOR                  
         BNH   PE51                                                             
         CLC   EEND,SBQREQST       YES-CHECK ESTIMATE DATES ARE WITHIN          
         BL    PE70                    THE REQUESTOR'S ORIGINAL DATES           
         CLC   ESTART,SBQREQND                                                  
         BH    PE70                                                             
*                                                                               
PE51     CLC   SBQESFLT,BLANKS     TEST FOR ESTIMATE FILTERING                  
         BNH   PE55                                                             
         TM    SBEFLAG5,SBE5EFNP   EST FILTER "OR" & NON-POSITIONAL?            
         BZ    PE51A               NO                                           
         BRAS  RE,ESTFILT          "OR" NON-POSITIONAL FILTER PASSED?           
         BNE   PE70                NO - FILTER OUT                              
         BRAS  RE,ESTFILTN         "OR" NON-POSITIONAL "-" FILT PASS?           
         BNE   PE70                NO - FILTER OUT                              
         B     PE55                YES                                          
*                                                                               
PE51A    LA    R1,3                                                             
         LA    RE,SBQESFLT                                                      
         LA    RF,EPROF                                                         
*                                                                               
PE52     CLI   0(RE),C'*'                                                       
         BE    PE54                                                             
         CLI   0(RE),C' '                                                       
         BE    PE54                                                             
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    PE53                YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   PE70                                                             
         B     PE54                                                             
*                                                                               
PE53     MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    PE70                                                             
*                                                                               
PE54     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,PE52                                                          
*                                                                               
PE55     DS    0H                                                               
         NI    SBEFLAG,X'FF'-SBEEEST                                            
         TM    ECONTROL,EBILESTQ   IS IT 'E' TYPE?                              
         BZ    *+12                 NO                                          
         OI    SBEFLAG,SBEEEST      YES - SET SPOTBUY FLAG                      
         B     PE55A                 AND PROCESS                                
         TM    SBEFLAG,SBEECNTL    FILTER FOR CONTROL 'E' ESTIMATES?            
         BNZ   PE70                 YES                                         
*                                                                               
PE55A    CLI   SBQETYPE,0          ANY ETYPE FILTER?                            
         BE    PE55B                NO                                          
         CLI   SBQETYPE,C'*'       INCLUDE ALL TYPES?                           
         BE    PE55C                YES - KEEP IT                               
         CLC   SBQETYPE,ETYPE      THIS TYPE?                                   
         BE    PE55C                YES - KEEP IT                               
         CLI   SBQETYPE,C'W'       FILTERING ON WIRED?                          
         BNE   PE70                 NO  - NOTHING ELSE CAN MATCH                
         B     *+12                ONLY WANT ETYPE 0/R WITH WIRED OPT           
*                                                                               
PE55B    CLI   ETYPE,C'U'          UNWIRED?                                     
         BE    PE55C               YES, KEEP THIS EST                           
         CLI   ETYPE,0             IS THERE AN ETYPE?                           
         BE    PE55C               NO                                           
         CLI   ETYPE,C'R'          IS THERE AN ETYPE?                           
         BNE   PE70                YES, SKIP THIS EST                           
*                                                                               
PE55C    CLI   SBQERATE,0          HAVE EST RATE FILTER?                        
         BE    PE55D               NO                                           
         CLC   ERATE,SBQERATE      YES - DOES IT MATCH THIS ESTIMATE?           
         BNE   PE70                NO, SKIP THIS EST                            
*                                                                               
PE55D    TM    SBEFLAG6,SBE6NOOW   IGNORE OOWR?                                 
         BZ    *+8                 NO                                           
         MVI   EOWSDAY,0           YES - CLEAR ESTIMATE'S OOWR                  
*                                                                               
         ICM   R1,15,SBAESTTB                                                   
         BNZ   PE56                                                             
         LLC   RE,EKEYEST          GET ESTIMATE NUMBER                          
         LA    RE,ESTTAB(RE)                                                    
         B     PE57                                                             
*                                                                               
PE56     LLC   RE,EPRDCD+1         GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   RF,EKEYEST          EST NUM                                      
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         AR    RE,R1                                                            
*                                                                               
PE57     ST    RE,FULL             FULL = A(PRD/EST TABLE ENTRY)                
*                                                                               
         MVC   SBDPTMEN,SBQDPTMN   SET DAYPART MENU                             
         CLI   SBDPTMEN,C' '       TEST MENU OVERRIDE                           
         BH    *+10                                                             
         MVC   SBDPTMEN,EDAYMENU   NO - MENU FROM ESTIMATE HDR                  
         CLI   SBDPTMEN,0                                                       
         BNE   *+8                                                              
         MVI   SBDPTMEN,C'Z'                                                    
         CLI   SBQSEPES,C'Y'       TEST SEPERATE ESTIMATES                      
         BE    PE57A                                                            
         CLI   FRSTEST,C'Y'        NO-TEST FIRST ESTIMATE FOR PRD               
         BE    PE57A                                                            
         MVC   0(1,RE),ESTBFENT    NO-SET SAVED EST BUFFER ENTRY NUM            
         B     PE59                   (CONTROL ESTIMATE)                        
*                                                                               
PE57A    ICM   R1,15,SBAESTBF      TEST FOR ESTIMATE BUFFER                     
         BNZ   PE57B                                                            
         MVC   0(1,RE),SBDPTMEN                                                 
         OC    SBPDEMOS,SBPDEMOS                                                
         BNZ   PE59                                                             
         MVC   SBPDEMOS,EDEMLST    NO - POST ESTIMATE VALUES FOR                
         MVC   SBPWTLST,EWGTLST         BRAND INTO BLOCK                        
         MVC   SBPUSN,EUSRNMS                                                   
         B     PE59                                                             
*                                                                               
PE57B    LA    R4,WORK                                                          
         USING ESTBUFFD,R4                                                      
         XC    0(ESTBUFFL,R4),0(R4)                                             
         MVC   EBDPTMEN,SBDPTMEN                                                
         CLI   SBEDEM,C'N'         TEST DEMOS NOT REQUIRED                      
         BE    PE57C                                                            
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OVERRIDE                      
         BZ    *+14                                                             
         MVC   EBDEMOS,SBPDEMOS    YES - USE THOSE                              
         B     PE57C                                                            
         SR    RE,RE                                                            
         ICM   RE,1,SBENDEM        NO - ESTIMATE DEMOS                          
         BZ    PE57C                                                            
         CHI   RE,8                LIMIT TO 8                                   
         BNH   *+8                                                              
         LHI   RE,8                                                             
         MHI   RE,3                ONLY SAVE AS MANY ESTIMATE DEMOS             
         BCTR  RE,0                AS WE NEED                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   EBDEMOS(0),EDEMLST                                               
         MVC   EBWGTLST,EWGTLST                                                 
         MVC   EBSVI,EHUTADJ                                                    
         GOTOR ASETUSER                                                         
*                                                                               
PE57C    CLI   SBEEFCST,C'Y'       TEST EFFECTIVE COSTS REQUESTED               
         BNE   PE57E                                                            
         CLC   SBQEST,SBQESTND     YES - TEST MULTIPLE ESTIMATES                
         BE    *+12                      LUMPED TOGETHER                        
         CLI   SBQSEPES,C'Y'                                                    
         BNE   PE57E                     YES - USE PRDBUFF BILL FORMULA         
         CLI   SBBPRD,FF           TEST PROD=POL                                
         BE    PE57D               YES - USE DEFAULT FORMULA                    
         MVC   EBBILBAS,EBILLBAS                                                
         MVC   EBBILCOM,EBILLCOM                                                
         OC    EBILLBAS(5),EBILLBAS  TEST ESTIMATE BILL FORMULA                 
         BNZ   PE57E                 YES - USE IT                               
         MVC   EBBILFOR,BILLFORM     NO - USE PRODUCT BILL FORMULA              
         OC    BILLFORM,BILLFORM     TEST PRODUCT BILL FORMULA                  
         BNZ   PE57E                                                            
*                                                                               
PE57D    DS    0H                                                               
         GOTOR AGETBILF            GET DEFAULT FORMULA                          
*                                                                               
PE57E    CLI   SBEFILT,C'Y'        TEST ESTIMATE FILTERS REQUIRED               
         BNE   *+10                                                             
         MVC   EBFILT,EPROF        YES                                          
         CLI   SBESREP,C'Y'        TEST SPECIAL REPS REQUIRED                   
         BNE   *+10                                                             
         MVC   EBSREP,EREP         YES                                          
         CLI   SBERTLSC,C'Y'       TEST RETAIL SCHEME CODE REQUIRED             
         BNE   *+10                                                             
         MVC   EBRTLSCH,ERTLSCHM   YES                                          
         TM    SBEUDEF,SBEUEST1    TEST USER FIELD 1 REQUIRED                   
         BZ    *+10                                                             
***      MVC   EBUFLD1,EUSER1                                                   
         MVC   SBUE1FLD,EUSER1                                                  
         TM    SBEUDEF,SBEUEST2    TEST USER FIELD 2 REQUIRED                   
         BZ    *+10                                                             
***      MVC   EBUFLD2,EUSER2                                                   
         MVC   SBUE2FLD,EUSER2                                                  
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
*                                                                               
         TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    PE57G               NOPE                                         
         CLC   =C'POL',IOKEY+4     PROCESSING POL ESTIMATE?                     
         BE    PE57G               YES                                          
         TM    SBEUDEF,SBEUEST1+SBEUEST2                                        
         BZ    PE57G                                                            
*                                                                               
         MVC   SVKEY,IOKEY         SAVE OFF KEY                                 
         MVC   IOKEY+4(3),=C'POL'  READ POL ESTIMATE                            
*                                                                               
         GOTOR AIO,IOSPTDIR+IOHI                                                
*                                                                               
         CLC   IOKEY(13),IOKEYSAV  HAVE POL ESTIMATE?                           
         BNE   PE57F               NOPE                                         
*                                                                               
         GOTOR (RF),IOSPTFIL+IOGET+IO2  YES-READ INTO AIO2                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO2                                                          
         TM    SBEUDEF,SBEUEST1    TEST USER FIELD 1 REQUIRED                   
         BZ    *+10                                                             
         MVC   SBUE1FLD,EUSER1-ESTHDRD(R1)                                      
         TM    SBEUDEF,SBEUEST2    TEST USER FIELD 2 REQUIRED                   
         BZ    *+10                                                             
         MVC   SBUE2FLD,EUSER2-ESTHDRD(R1)                                      
*                                                                               
PE57F    MVC   IOKEY(13),SVKEY     RESTORE KEY                                  
         ICM   R1,15,SBAESTBF      RESTORE ESTIMATE BUFFER                      
*                                                                               
PE57G    TM    SBEFLAG,SBEWIPW                                                  
         BZ    *+10                                                             
         MVC   EBUFPCT,EPWPCT      WESTERN PW PERCENT                           
         MVC   ECOS2,ECOST2        COST2 FACTOR                                 
         DROP  R4                                                               
*                                                                               
         LHI   R0,256                                                           
         LHI   RE,1                                                             
*                                                                               
PE58     OC    0(ESTBUFFL,R1),0(R1)                                             
         BNZ   *+14                                                             
         MVC   0(ESTBUFFL,R1),0(R4)                                             
         B     PE58A                                                            
         CLC   0(ESTBUFFL,R1),0(R4)                                             
         BE    PE58A                                                            
         LA    R1,ESTBUFFL(R1)                                                  
         LA    RE,1(RE)                                                         
         BCT   R0,PE58                                                          
         DC    H'0'                ESTIMATE BUFFER NEEDS EXPANSION              
*                                  DON'T EXPAND THE SF EST BUFFER!!!            
*                                                                               
PE58A    L     R1,FULL             MARK PRD/EST COMBO ACTIVE WITH               
         STC   RE,0(R1)            ESTIMATE BUFFER ENTRY NUMBER                 
         MVI   FRSTEST,C'N'                                                     
*                                                                               
PE59     L     R1,FULL             SAVE EST BUFF ENTRY NUM                      
         MVC   ESTBFENT,0(R1)                                                   
*                                                                               
         MVC   SBBEST,EKEYEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
         MVC   SBESTNM,EDESC                                                    
         MVC   SBESTST,ESTART                                                   
         MVC   SBESTND,EEND                                                     
         MVC   SBESTDEM,EDEMOS                                                  
         MVC   SBESTDM2,EDEMOS+6                                                
         MVC   SBESTOWD,EOWSDAY                                                 
         GOTOR CDATCON,PARM,(0,SBESTST),(2,SBESTSTP)                            
         GOTOR CDATCON,PARM,(0,SBESTST),(3,SBESTSTB)                            
         GOTOR CDATCON,PARM,(0,SBESTND),(2,SBESTNDP)                            
         GOTOR CDATCON,PARM,(0,SBESTND),(3,SBESTNDB)                            
*                                                                               
         LA    R4,SBDPTTAB                                                      
         OC    SBADPTTB,SBADPTTB   TEST DAYPART TABLES BUFFER                   
         BZ    PE59A                                                            
         GOTOR AGETDPTB            YES - GET DPT TABLE ADDRESS                  
         L     R4,FULL                                                          
*                                                                               
PE59A    TM    SBQSKIP,SBQSKMED    UNLESS ACCOUNTING ONLY,                      
         BO    PE60                                                             
         OC    0(180,R4),0(R4)     TEST DAYPART TABLE ALREADY READ              
         BNZ   PE60                                                             
         XC    PARM,PARM           READ DAYPART RECORD                          
         MVC   PARM(2),SBAGY                                                    
         MVC   PARM+2(1),SBMED                                                  
         MVC   PARM+3(1),SBDPTMEN                                               
         GOTOR VDPTRD,PARM,,AIO3,CDATAMGR                                       
         CLI   PARM+8,X'FF'                                                     
         BE    PE60                                                             
         L     R1,AIO3                                                          
         MVC   0(180,R4),0(R1)                                                  
         BRAS  RE,DOLDPT           CHECK FOR $ DAYPARTS                         
*                                                                               
*                                                                               
PE60     TM    SBQREAD,SBQRDPG     TEST READ PG ESTIMATE RECORDS                
         BZ    PE66                                                             
         L     R1,SBAIO2           YES-READ INTO AIO2                           
         XC    0(256,R1),0(R1)                                                  
         MVC   WORK(L'IOKEY),IOKEY                                              
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PGESTD,R2                                                        
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         MVC   PGKPRD,SBPRD                                                     
         MVC   PGKEST,SBBEST                                                    
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(PGKLENQ),IOKEYSAV TEST RECORD FOUND                        
         BNE   PE62                    NO                                       
         GOTOR (RF),IOSPTFIL+IOGET+IO2  YES-READ INTO AIO2                      
         BE    PE62                                                             
         DC    H'0'                                                             
*                                                                               
PE62     L     R1,SBAIO3                                                        
         XC    0(256,R1),0(R1)                                                  
         CLC   SBBPRD,SVPRD        TEST FIRST TIME FOR PRODUCT                  
         BE    PE64                                                             
         MVC   SVPRD,SBBPRD                                                     
         CLI   SBBPRD,X'FF'        AND IT'S NOT POL                             
         BE    PE64                                                             
         MVC   IOKEY,IOKEYSAV      YES-READ EST=0 RECORD INTO AIO3              
         MVI   PGKEST,0                                                         
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(PGKLENQ),IOKEYSAV                                          
         BNE   PE64                                                             
         GOTOR (RF),IOSPTFIL+IOGET+IO3                                          
         BE    PE64                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
PE64     MVC   IOKEY,WORK          RESTORE THE KEY                              
*                                                                               
PE66     MVI   SBMODE,SBPROCES                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
         MVI   ESTFOUND,C'Y'                                                    
*                                                                               
PE70     MVC   IOKEY+8(5),XFF      NEXT ESTIMATE                                
         CLC   SBQEST,SBQESTND                                                  
         BNE   PE30                                                             
*                                                                               
PE80     CLI   SBQBPRD,0           NEXT PRODUCT                                 
         BNE   PE90                                                             
         MVC   IOKEY+7(6),XFF                                                   
         B     PE30                                                             
*                                                                               
PE90     CLI   SBQPGRD,C' '        TEST PRODUCT GROUPS                          
         BH    *+12                                                             
         CLI   SBQBPRD,0           OR SINGLE PRODUCT                            
         BE    PE92                                                             
         OC    SBAESTTB,SBAESTTB   AND ESTIMATE TABLE PROVIDED                  
         BZ    PE92                                                             
         LA    R3,6                YES - TRY POOL ESTIMATES                     
         MVC   IOKEY,IOKEYSAV                                                   
         CLC   IOKEY+4(3),=C'POL'                                               
         BE    PE92                                                             
         MVC   IOKEY+4(3),=C'POL'                                               
         XC    IOKEY+7(6),IOKEY+7                                               
         B     PE30                                                             
*                                                                               
PE92     ICM   R1,15,SBAESTTB      YES - ONLY PRODUCT POOL DPT MENUS            
         BZ    PEX                                         AND DEMOS            
         LA    R4,254                                                           
         SLL   R4,8                                                             
         AR    R4,R1               R4 = A(PRODUCT POOL)                         
         CLC   SBQPRD,=C'POL'      TEST PRODUCT POOL                            
         BE    PE96                YES - ONLY PRD POL DPT MENUS, DEMOS          
         LA    RE,253              NO - MAKE PRD=UNALLOC SAME AS POOL           
         SLL   RE,8                                                             
         AR    R1,RE               R1 = A(PRD=UNALLOC)                          
         LA    R5,255                                                           
         MVC   0(1,R1),0(R4)                                                    
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,*-14                                                          
         B     PEX                                                              
*                                                                               
PE96     LA    R0,254                                                           
*                                                                               
PE97     LA    R5,255                                                           
         LR    RE,R1                                                            
         LR    RF,R4                                                            
*                                                                               
PE98     CLI   0(RE),0                                                          
         BE    *+10                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R5,PE98                                                          
*                                                                               
         LA    R1,256(R1)          NEXT PRODUCT                                 
         BCT   R0,PE97                                                          
*                                                                               
PEX      J     EQEXIT                                                           
*                                                                               
KEYCOMP  CLC   IOKEY(0),IOKEYSAV   ** EXECUTED                                  
         EJECT                                                                  
* CHECK DAYPART TABLE FOR DOLLAR DAYPARTS                                       
* INPUT  : R4=A(DAYPART TABLE)                                                  
*                                                                               
DOLDPT   LR    R0,RE                                                            
         LR    RE,R4                                                            
         LA    RF,36                                                            
*                                                                               
DOLDPT2  CLI   0(RE),C'$'          TEST DPT=$                                   
         BNE   *+8                                                              
         MVI   2(RE),C''          YES-ALTER 1ST CHAR OF NAME TO C''           
         LA    RE,5(RE)                                                         
         BCT   RF,DOLDPT2                                                       
*                                                                               
DOLDPTX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* READ THE BUY RECORDS                                                          
*                                                                               
READBUYS NTR1  BASE=*,LABEL=*                                                   
         MVI   SBBPRD,0                                                         
         MVI   SBBEST,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SVSTA,SVSTA                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING BUYRECD,R2                                                       
         MVC   BUYKAM,SBBAGYMD                                                  
*                                                                               
         TM    SBEFLAG2,SBEORIG    EXTRACT ORIGINAL COST?                       
         BZ    *+8                  NO                                          
         OI    BUYKAM,X'08'                                                     
*                                                                               
         CLC   BUYKEY(BUYKCLT-BUYKEY),SBSVKEY   TEST SAVED KEY IS A BUY         
         BNE   *+14                             KEY FOR THIS AGYMD              
         MVC   BUYKEY,SBSVKEY      YES-START THERE                              
         B     RB60                                                             
         MVC   BUYKCLT,SBBCLT                                                   
         CLC   SBQPRD,=C'POL'      POL REQUEST                                  
         BNE   *+12                                                             
         MVI   BUYKPRD,X'FF'                                                    
         B     RB50                                                             
         CLI   SBQBPRD,0           SINGLE PRODUCT REQUEST                       
         BE    RB60                                                             
         MVC   BUYKPRD,SBQBPRD                                                  
*                                                                               
RB50     OC    QBMKT,QBMKT         MARKET FILTER                                
         BZ    RB60                                                             
         MVC   BUYMSTA(2),QBMKT                                                 
         OC    QBSTA,QBSTA         STATION FILTER                               
         BZ    RB60                                                             
         MVC   BUYMSTA+2(3),QBSTA                                               
         MVC   BUYKEST,SBQEST      START ESTIMATE                               
*                                                                               
RB60     LA    R2,IOKEY            REESTABLISH KEY ADDRESSABILITY               
         GOTOR AIO,IOSPTDIR+IOHID                                               
*                                                                               
* SET SBBUYCH FLAGS ON CHANGE OF CLT/PRD/EST/STA FOR CABLE AGG                  
RB70     CLC   BUYKCLT,SBBCLT                                                   
         BE    *+8                                                              
         OI    SBBUYCH,SBBCCLT                                                  
*                                                                               
* FLAG SET IN WRI01                                                             
*         CLI   BUYKPRD,X'FF'       DON'T SET CHANGED IF POL                    
*         BE    *+18                                                            
*         CLC   BUYKPRD,SBBPRD                                                  
*         BE    *+8                                                             
*         OI    SBBUYCH,SBBCPRD                                                 
*                                                                               
*        CLC   BUYKEST,SBBEST                                                   
*        BE    *+8                                                              
*        OI    SBBUYCH,SBBCEST                                                  
*                                                                               
         LA    RF,2                                                             
         CLI   CANADA,C'Y'                                                      
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BUYMSTA+2(0),SBBMKTST+2                                          
         BE    *+8                                                              
         OI    SBBUYCH,SBBCSTA                                                  
*                                                                               
* IF MED, CLT, PRD, EST OR MKT CHANGES, RE-READ PW DATA (IF EXTRACTING)         
         TM    SBEFLAG,SBEWIPW                                                  
         BZ    RB90                DON'T NEED IT                                
         CLC   BUYKEST,SBBEST      SAME EST?                                    
         BNE   RB71                                                             
         CLC   BUYKCLT,SBBCLT      SAME CLT                                     
         BNE   RB80                                                             
         CLC   BUYMSTA(2),SBBMKTST SAME MKT?                                    
         BE    *+12                                                             
RB71     OI    FLAG2,FLGETPW       SET TO GET MKT & STA                         
         B     RB90                                                             
*                                                                               
         CLI   CANADA,C'Y'         (NOT THAT I THINK CANADA USES PW!)           
         BE    RB72                NFC (NO CABLE)                               
         CLI   BUYMSTA+2,X'E8'     TEST CABLE?                                  
         BL    RB72                 NO                                          
         MVC   WORK(3),BUYMSTA+2                                                
         MVC   WORK+3(3),SBBMKTST+2                                             
         NI    WORK+2,X'80'                                                     
         NI    WORK+5,X'80'                                                     
         CLC   WORK(3),WORK+3                                                   
         BE    RB90                                                             
*                                                                               
RB72     CLC   BUYMSTA+2(3),SBBMKTST+2    SAME STA?                             
         BE    RB90                                                             
RB80     OI    FLAG2,FLGTPWS        ONLY GET NEW PW STA REC                     
*                                                                               
RB90     MVC   SBRECDA,IODA        SAVE RECORD'S D/A                            
         CLC   BUYKEY(BUYKPRD-BUYKEY),IOKEYSAV                                  
         BNE   RBX                                                              
         LLC   RE,BUYKPRD          TEST ANY VALID ESTS FOR THIS PRODUCT         
         BCTR  RE,0                                                             
         ICM   R1,15,SBAESTTB                                                   
         BNZ   *+18                                                             
         OC    ESTTAB,ESTTAB                                                    
         BZ    RB370                                                            
         B     RB100                                                            
         SLL   RE,8                                                             
         AR    RE,R1                                                            
         OC    0(256,RE),0(RE)                                                  
         BZ    RB370               NO - SKIP TO NEXT PRODUCT                    
*                                                                               
RB100    CLC   BUYKEY(BUYMSTA-BUYKEY),IOKEYSAV  TEST NEW PRODUCT                
         BE    *+10                                                             
         MVC   AXSPILL,SBAXSPIL    YES-INITIALIZE A(CANADIAN SPILL)             
*                                                                               
         TM    SBEFLAG,SBEWIPW     SKIP SPILL CHECK FOR PW                      
         BNZ   RB110                                                            
         CLI   BUYKBUY,X'80'       TEST SPILL POINTER                           
         BNE   RB110               NO                                           
         CLI   SBQSPILL,C'N'       YES-TEST SPILL REQUIRED                      
         BE    RB280                NO - NEXT EST                               
*                                                                               
RB110    CLI   SBQBPRD,0           TEST ONE PRODUCT ONLY                        
         BNE   RB130                                                            
         CLC   SBQPGRF,BLANKS      NO - TEST PRDGRP FILTERING                   
         BNH   RB120                                                            
         CLI   BUYKPRD,X'FF'            YES - TEST PRODUCT POL                  
         BNL   RBX                            YES - FINISHED                    
         LLC   RE,BUYKPRD                     NO - FIND PRDGRP                  
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    RB370                                                            
         B     RB140                                                            
         DROP  R1                                                               
*                                                                               
RB120    CLI   BUYKBUY,0           ALL PRODUCTS-TEST ACTIVE POINTER             
         BE    RB140               YES                                          
         CLI   SBQMED,C'C'         NO-TEST MEDIA=COMBINED                       
         BE    *+12                YES                                          
         CLI   BUYKBUY,X'80'       NO-TEST SPILL POINTER                        
         BNE   RB280               NO-NEXT ESTIMATE                             
***      CLI   BUYKBUY+1,0         TEST POL PASSIVE                             
***      BE    RB280               YES-NEXT ESTIMATE                            
         CLI   BUYKPRD,X'FF'       POL?                                         
         BNE   RB280               NO - NEXT ESTIMATE                           
         B     RB140               NO-OK                                        
*                                                                               
RB130    CLC   SBQBPRD,BUYKPRD     FILTER ON SINGLE PRODUCT                     
         BE    RB140                                                            
         BL    RBX                                                              
         MVC   BUYKPRD,SBQBPRD     SKIP TO REQ PRODUCT                          
         XC    BUYMSTA(9),BUYMSTA                                               
         B     RB60                                                             
*                                                                               
RB140    ICM   R1,15,AXSPILL       TEST CANADIAN SPILL REQUEST                  
         BZ    RB170                                                            
*                                                                               
         USING XSPILLD,R1                                                       
RB150    OC    0(XSPILLL,R1),0(R1) YES-TEST REACHED END OF TABLE                
         BZ    RBX                 YES-DONE WITH BUYS                           
         CLC   BUYMSTA,XSBMSTA     NO-COMPARE MKT/STA IN KEY TO TABLE           
         BE    RB170               EQUAL-OK                                     
         BH    RB160                                                            
         MVC   BUYMSTA,XSBMSTA     LOW-SKIP TO STATION IN TABLE                 
         XC    BUYKEST(4),BUYKEST                                               
         B     RB60                                                             
RB160    LA    R1,XSPILLL(R1)      HIGH-ADVANCE TO NEXT STATION                 
         ST    R1,AXSPILL                                                       
         B     RB150                                                            
         DROP  R1                                                               
*                                                                               
RB170    TM    SBQCAN,SBQCBYM0     TEST READ MARKET 0 RECORDS                   
         BO    *+14                                                             
         OC    BUYMSTA(2),BUYMSTA  NO-IGNORE MARKET 0                           
         BZ    RB340                                                            
         CLC   BUYMSTA(2),QBMKTST  TEST MARKET WITHIN RANGE                     
         BL    *+18                                                             
         CLC   BUYMSTA(2),QBMKTND                                               
         BH    RB370               DONE WITH MARKET - NEXT PRODUCT              
         B     RB180                                                            
         MVC   BUYMSTA(2),QBMKTST  SKIP TO MARKET/STATION                       
         MVC   BUYMSTA+2(3),QBSTA                                               
         XC    BUYKEST(4),BUYKEST                                               
         B     RB60                                                             
*                                                                               
RB180    TM    SBIOFLAG,SBXFILE    READ ACROSS FILES?                           
         BZ    RB182                NO                                          
         CLI   SBQMGRD,0           MKT GROUP?                                   
         BNE   RB182                                                            
         OC    SBQMKT,SBQMKT       ALL MARKETS?                                 
         BZ    RB182               YES                                          
         CLC   =C'ALL',SBQMKT                                                   
         BE    RB182                                                            
         ZICM  RE,BUYMSTA,2                                                     
         AR    RE,RE                                                            
         A     RE,SBAMGTAB                                                      
         OC    0(2,RE),0(RE)       ACCEPT THIS MKT?                             
         BNZ   RB190                YES                                         
         B     RB340                                                            
*                                                                               
RB182    CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP FILTERING              
         BNH   RB190               NO                                           
         MVC   SVMKT,BUYMSTA       YES                                          
         MVC   SVPRD,BUYKPRD                                                    
         GOTOR ACHKMGRP                                                         
*        BE    RB250               MKT IN VALID MKTGRP                          
         BE    RB190               MKT IN VALID MKTGRP                          
         B     RB340                                                            
*                                                                               
RB190    MVC   STAFILT,BUYMSTA                                                  
         GOTOR ACBLFILT                                                         
         BNE   RB300               SKIP TO NEXT STATION                         
*                                                                               
RB250    SR    R0,R0                                                            
         ICM   R0,7,SVSTA                                                       
         SR    RF,RF                                                            
         ICM   RF,7,BUYMSTA+2                                                   
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    RB252                NFC (NO CABLE)                              
         CLI   SVSTA,X'E8'         TEST CABLE                                   
         BL    RB252                                                            
         N     R0,=X'00FFFF80'                                                  
         N     RF,=X'00FFFF80'                                                  
*                                                                               
RB252    CR    R0,RF                                                            
         BE    RB260                                                            
         MVC   SVSBSTA,SBSTA                                                    
         MVC   SVSTA,BUYMSTA+2     YES-GET STATION DETAILS                      
         GOTOR AGETSTA                                                          
         MVC   SBSTA,SVSBSTA                                                    
         BNE   RB300                                                            
*                                                                               
RB260    GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BNE   RB300                                                            
*                                                                               
         ICM   R1,15,SBAESTTB                                                   
         BNZ   RB270                                                            
         LLC   RE,BUYKEST                                                       
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE EST                              
         BE    RB280               SKIP ESTIMATE                                
         B     RB380                                                            
*                                                                               
RB270    LLC   RE,BUYKPRD          PRODUCT                                      
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   RF,BUYKEST          ESTIMATE                                     
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BNE   RB380                                                            
*                                                                               
RB280    CLC   BUYKEST,SBQEST      NEXT ESTIMATE                                
         BNL   RB290                                                            
         MVC   BUYKEST,SBQEST                                                   
         XC    BUYKBUY(3),BUYKBUY                                               
         B     RB60                                                             
*                                                                               
RB290    CLC   BUYKEST,SBQESTND                                                 
         BH    RB300                                                            
         MVC   BUYKBUY,XFF                                                      
         B     RB60                                                             
*                                                                               
RB300    OC    QBSTA,QBSTA         NEXT STATION                                 
         BZ    RB330                                                            
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    RB310                NFC (NO CABLE)                              
         CLI   BUYMSTA+2,X'E8'     TEST CABLE                                   
         BNL   RB330               YES - DO ALL STATIONS THIS MKT               
*                                                                               
RB310    OC    QBMKTSTA(2),QBMKTSTA   READ ACROSS ALL MARKETS?                  
         BNZ   RB315                  NO                                        
         CLC   BUYKSTAC,QBMKTSTA+2    WANT TO BUMP TO A STATION?                
         BL    RB311                  YES, LEAVE THE MARKET AS IS               
         MVC   BUYKSTAC,XFF           FORCE NEXT MARKET                         
         B     RB60                                                             
*                                                                               
RB311    MVC   BUYKSTAC,QBMKTSTA+2    BUMP TO THIS STATION                      
         B     RB321                                                            
*                                                                               
RB315    CLC   BUYMSTA,QBMKTSTA                                                 
         BL    RB320                                                            
         OC    QBSTACAN,QBSTACAN                                                
         BZ    RB370                                                            
         B     RB330                                                            
*                                                                               
RB320    MVC   BUYMSTA,QBMKTSTA    SKIP TO REQUESTED STATION                    
*                                                                               
RB321    XC    BUYKEST(4),BUYKEST                                               
         B     RB60                                                             
*                                                                               
RB330    MVC   BUYKEST(4),XFF                                                   
         B     RB60                                                             
*                                                                               
RB340    OC    QBMKT,QBMKT         NEXT MARKET                                  
         BZ    RB360                                                            
         CLC   BUYMSTA(2),QBMKTST                                               
         BNL   RB350                                                            
         MVC   BUYMSTA(2),QBMKTST                                               
         XC    BUYMSTA+2(7),BUYMSTA+2                                           
         B     RB60                                                             
*                                                                               
RB350    CLC   BUYMSTA(2),QBMKTND                                               
         BH    RB370                                                            
         CLC   QBMKTST,QBMKTND                                                  
         BE    RB370                                                            
*                                                                               
RB360    MVC   BUYMSTA+2(7),XFF    NEXT MARKET                                  
         B     RB60                                                             
*                                                                               
RB370    CLI   BUYKPRD,X'FF'       NEXT PRODUCT                                 
         BE    RBX                                                              
         CLI   SBQBPRD,0                                                        
         BNE   RBX                                                              
         MVC   BUYMSTA(9),XFF                                                   
         B     RB60                                                             
*                                                                               
RB380    ICM   R1,15,SBAESTBF                                                   
         BNZ   *+14                                                             
         MVC   SBDPTMEN,0(RE)                                                   
         B     RB390                                                            
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R1,RF                                                            
         USING ESTBUFFD,R1                                                      
         MVC   SBDPTMEN,EBDPTMEN   SET DAYPART MENU                             
         MVC   SBESTFLT,EBFILT     SET ESTIMATE FILTERS                         
         MVC   SBESTSR,EBSREP      SET ESTIMATE'S SPECIAL REP                   
         MVC   SBRTLSCH,EBRTLSCH   SET ESTIMATE'S RETAIL SCHEME CODE            
***      MVC   SBUE1FLD,EBUFLD1                                                 
***      MVC   SBUE2FLD,EBUFLD2                                                 
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
         DROP  R1                                                               
*                                                                               
RB390    CLC   BUYKPRD,SBBPRD      TEST PRODUCT CHANGE                          
         BE    RB420                                                            
         MVC   SBBPRD,BUYKPRD      SET PRODUCT                                  
         L     RF,ACLTREC          FIND PRODUCT IN CLTHDR                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
RB400    CLC   SBBPRD,3(RF)                                                     
         BE    RB410                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    RB400                                                            
         DC    H'0'                                                             
*                                                                               
RB410    MVC   SBPRD,0(RF)         PRODUCT                                      
         LLC   RE,SBBPRD           SET PRODUCT GROUP                            
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    RB420                                                            
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBBPGR,PBGROUP                                                   
         OC    SBBPGR,SBBPGR                                                    
         BNZ   RB420                                                            
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
RB420    NI    FIRSTSW,255-FEST    TURN OFF EST FIRST FLAG                      
         CLC   SBBEST,BUYKEST      TEST ESTIMATE CHANGE                         
         BE    RB430                                                            
         OI    FIRSTSW,FEST        TURN ON EST FIRST FLAG                       
         MVC   SBBEST,BUYKEST      YES - SET ESTIMATE                           
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
RB430    TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    *+8                         NO                                   
         BRAS  RE,ESTUSERF                 YES - GET EST USER FIELDS            
*                                                                               
         MVC   IODA,SBRECDA             GET THE BUY RECORD                      
         GOTOR AIO,IOSPTFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    RB790                                                            
         L     RE,AIO1             ADDRESS THE RECORD                           
*                                                                               
         SR    R1,R1               MAKE SURE END-OF-RECORD IS SECURE            
         ICM   R1,3,BUYRLEN-BUYREC(RE)                                          
         AR    R1,RE                                                            
         MVI   0(R1),0                                                          
         MVC   SBBMPRD,BDMASPRD-BUYREC(RE)  4 STA_FIRST 4 CABL AGG              
*                                                                               
RB440    CLI   SBQMGR,0            TEST  MARKET GROUPS                          
         BE    RB450                                                            
         CLC   SBBMKT,BUYMSTA      YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    RB450                                                            
         MVC   SVMKT,BUYMSTA       SET MARKET GROUP                             
         MVC   SVPRD,BUYKPRD                                                    
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
RB450    TM    SBIOFLAG,SBXFILE    READ ACROSS FILES?                           
         BZ    RB460                NO                                          
         TM    SBEFLAG5,SBE5BFRM   GET BILL FORM RECORD?                        
         BZ    *+12                NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BNE   *+14                YES - EVEN IF MKT HASN'T CHANGED!            
         CLC   SBBMKT,SBBSTMKT     ELSE TEST NEW STATION MKT                    
         BE    RB480                                                            
         MVC   SBBMKT,SBBSTMKT                                                  
         B     RB470                                                            
*                                                                               
* SBBMKT WILL BE 0000 THE FIRST TIME, SO IF WE'RE READING CANADIAN              
* MARKET 0 BUYS, THE FIRST TIME CHECK IS A LITTLE MORE COMPLICATED!             
RB460    TM    SBQCAN,SBQCBYM0     EXTRACTING MKT 0?                            
         BZ    RB462                NO                                          
         OC    BUYMSTA(2),BUYMSTA  MKT 0 BUY?                                   
         BNZ   RB462                NO                                          
         OC    SBMKT,SBMKT         HAVE WE FILLED IN A MARKET BEFORE?           
         BZ    RB464                NO - GET ONE!                               
*                                                                               
RB462    TM    SBEFLAG5,SBE5BFRM   GET BILL FORM RECORD?                        
         BZ    *+12                NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BNE   RB464               YES - EVEN IF MKT HASN'T CHANGED!            
*                                                                               
         CLC   SBBMKT,BUYMSTA      TEST NEW MARKET                              
         BE    RB480                                                            
*                                                                               
RB464    MVC   SBBMKT,BUYMSTA      YES-SET MARKET                               
*                                                                               
RB470    SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
*                                                                               
         TM    SBEFLAG,SBEWIPW     SKIP UNLESS EXTRACTING PW DATA               
         BZ    *+14                                                             
         L     RF,SBAMKEST         NEW MKT - CLEAR MKT/EST TABLE                
         XC    0(256,RF),0(RF)                                                  
*                                                                               
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   RB480                                                            
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
RB480    NI    FIRSTSW,255-FSTA                                                 
         CLC   SBBSTA,BUYMSTA+2    TEST STATION CHANGE                          
         BE    RB520                                                            
         CLI   CNETWK,C'Y'         YES-TEST CANADIAN NETWORK REQUEST            
         BNE   RB490                NO                                          
         CLC   SBBSTA(2),BUYMSTA+2    <== NETWORK NOW FULL BYTE                 
         BNE   RB490                                                            
*                                                                               
         CLI   SBBSTA+2,X'B0'      IF NEW TYPE NETWORK,                         
         BL    RB520                NEED TO GET CORRECT NETWORK                 
         GOTOR AMSUNPK,PARM,(X'80',BUYMSTA),WORK,WORK+4                         
         MVC   SBCBLNET,WORK+9                                                  
         B     RB520                                                            
*                                                                               
RB490    DS    0H                                                               
         GOTOR AMSUNPK,PARM,(X'80',BUYMSTA),WORK,WORK+4                         
         CLI   WORK+4,C'0'         TEST NEW AND OLD STATIONS ARE CABLE          
         BL    RB500                                                            
         CLI   SBSTA,C'0'                                                       
         BL    RB500                                                            
         ICM   RE,7,SBBSTA         YES-REMOVE NETWORK BITS FROM BOTH            
         SRL   RE,7                                                             
         SLL   RE,7                                                             
         ICM   RF,7,BUYMSTA+2                                                   
         SRL   RF,7                                                             
         SLL   RF,7                                                             
         CR    RE,RF               TEST HEADEND CHANGE                          
         BE    RB510               NO-NOT STATION FIRST FOR CLEARANCE           
*                                                           RECORDS             
RB500    OI    FIRSTSW,FSTA        STATION FIRST                                
*                                                                               
RB510    MVC   SBBSTA,BUYMSTA+2    SET STATION                                  
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
         B     RB521                                                            
*                                                                               
RB520    TM    FIRSTSW,FEST        EST FIRST FLAG?                              
         BZ    RB525               NOPE                                         
         XC    SVSYSCD,SVSYSCD     CLEAR LAST SYSCODE FOR NEW EST!              
*                                                                               
RB521    TM    SBQREAD2,SBQRD2OM   READ OM KEYWORDS?                            
         BZ    RB525               NO                                           
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    RB523               YES - NFC (NO CABLE)                         
         CLI   SBBSTA,X'E8'        CABLE?                                       
         BL    RB523               NO                                           
         MVC   FULL(3),SBBSTA                                                   
         NI    FULL+2,X'80'        STRIP NETWORK BITS                           
         CLC   SVSYSCD,FULL        SAME SYSCODE AS PREVIOUS?                    
         BNE   RB522               NO - BUFFER OM RECS!                         
         TM    FIRSTSW,FEST        EST FIRST FLAG?                              
         BZ    RB525               NO - SAME SYSCODE/EST AS BEFORE              
*                                                                               
RB522    MVC   SVSYSCD,SBBSTA      SAVE THE SYSCODE                             
         NI    SVSYSCD+2,X'80'     STRIP NETWORK BITS                           
RB523    MVI   SBMODE,SBPROCOB                                                  
         BRAS  RE,GO                                                            
*                                                                               
RB525    CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   RB530               NO                                           
         CLC   SBSTYPE,SBQSTATY    YES-CHECK STATION TYPE                       
         BNE   RB790               (ASSUMES HOOK SET TYPE)                      
*                                                                               
RB530    DS    0H                  ***GET THE BUY RECORD WAS HERE***            
         L     R2,AIO1             ADDRESS THE RECORD                           
*                                                                               
         TM    SBEFLAG,SBEWIPW     SKIP UNLESS EXTRACTING PW DATA               
         BZ    RB570                                                            
*                                                                               
* ALL BUYS THAT HAVE HAD A PW MAINT DONE WILL HAVE A ^0 X'9B' ELEM.             
* IF MKT IN X'9B' <> BUYMSTA, SKIP BUY FOR PW, SINCE PW DATA IS                 
* FROM A DIFFERENT MARKET.                                                      
*                                                                               
         SR    R0,R0               FIND X'9B' ELEM                              
         LA    R1,BDELEM                                                        
RB540    CLI   0(R1),0                                                          
         BE    RB550                                                            
         CLI   0(R1),X'9B'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RB540                                                            
*                                                                               
         OC    2(2,R1),2(R1)       HAD PW MAINT DONE?                           
         BZ    RB550                NO                                          
         CLC   2(2,R1),BUYMSTA     IS THIS ORIGINAL MKT?                        
         BNE   RB790                NO - SKIP BUY                               
*                                                                               
* BUILD PW KEY                                                                  
RB550    CLC   SBBMKT,BUYMSTA      DON'T READ PW RECS FOR SPILL MKTS            
         BNE   RB570                                                            
         CLC   SBPRD,=C'POL'       NEED TO SET SBPRD & SBBPRD TO                
         BNE   RB552                  BDMASPRD FOR PROCPW & PROCP2              
*                                                                               
         OI    FLAG2,FLWASPOL                                                   
         MVC   SBBPRD,BDMASPRD     SET PRODUCT                                  
         L     RF,ACLTREC          FIND PRODUCT IN CLTHDR                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
         CLC   SBBPRD,3(RF)                                                     
         BE    *+18                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *-18                                                             
         DC    H'0'                                                             
*                                                                               
         MVC   SBPRD,0(RF)         PRODUCT                                      
*                                                                               
RB552    DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    RF,SVKEY                                                         
         USING PWRECD,RF                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD(3),SBBAGYMD AGMD/CLT                                     
         MVC   PWKPRD,BDMASPRD     PRODUCT                                      
         TM    BDSTAT2,X'30'       TEST NON-TBS TRADE BUY                       
         BZ    RB554                                                            
         CLI   SBQPRD+2,C'#'       WESTERN TRADE?                               
         BNE   *+8                  NO                                          
         OI    PWKPRD,X'80'        READ TRADE PW REC                            
*                                                                               
RB554    MVC   PWKEST,BUYKEST      ESTIMATE                                     
         MVC   PWKMKT(5),BUYMSTA   MARKET & STATION                             
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    RB556                NFC (NO CABLE)                              
         CLI   PWKSTA,X'E8'        TEST CABLE?                                  
         BL    RB556                NO                                          
         NI    PWKSTA+2,X'80'                                                   
         DROP  RF                                                               
*                                                                               
RB556    CLC   BDMASPRD,LMASPRD                                                 
         BE    *+14                                                             
         MVC   LMASPRD,BDMASPRD                                                 
         B     RB560               IF PRD CHANGES, GET MKT & STA                
         TM    FLAG2,FLGETPW       GET MKT & STA LEV?                           
         BNZ   RB560                YES                                         
         TM    FLAG2,FLGTPWS       GET STA LEV ONLY?                            
         BZ    RB580                NO                                          
         GOTOR AGETPWS                                                          
         TM    FLAG2,FLWASPOL                                                   
         BZ    RB580                                                            
         MVC   SBPRD,=C'POL'                                                    
         MVI   SBBPRD,X'FF'                                                     
         B     RB580                                                            
*                                                                               
RB560    GOTOR AGETPWM                                                          
         TM    FLAG2,FLWASPOL                                                   
         BZ    RB570                                                            
         MVC   SBPRD,=C'POL'                                                    
         MVI   SBBPRD,X'FF'                                                     
*                                                                               
RB570    TM    SBIOFLAG,SBXFILE    CROSS FILE READING?                          
         BNZ   RB580                YES - IGNORE SPILL CHECK                    
         CLI   SBQSPILL,C'N'       TEST SPILL WANTED                            
         BNE   RB580                                                            
         CLC   SBBMKT,BUYMSTA      NO-REJECT ALL SPILL MARKETS                  
         BNE   RB790                                                            
*                                                                               
RB580    CLI   SBQDPT,0            TEST DAYPART FILTER                          
         BE    RB590                                                            
         GOTOR AFILTDPT,BDDAYPT                                                 
         BNE   RB790                                                            
*                                                                               
RB590    CLI   SBQMASDP,0          TEST MASTER DAYPART FILTER                   
         BE    RB600                                                            
         LA    R1,BDDAYPT                                                       
         GOTOR ACHKMDPT                                                         
         BNE   RB790                                                            
*                                                                               
RB600    CLI   SBQLEN,0            TEST SPOT LENGTH FILTER                      
         BE    *+14                                                             
         CLC   BDSEC,SBQLEN                                                     
         BNE   RB790                                                            
         CLI   SBQADJ,0            TEST PROGRAM ADJACENCY CODE FILTER           
         BE    RB620                                                            
         LA    R1,WORK                                                          
         MVC   0(1,R1),SBQADJ                                                   
         MVC   1(L'SBQADLST,R1),SBQADLST                                        
         LA    RF,L'SBQADLST+1                                                  
*                                                                               
RB610    CLI   0(R1),0                                                          
         BE    RB790                                                            
         CLC   BDPROGT,0(R1)                                                    
         BE    RB620                                                            
         LA    R1,1(R1)                                                         
         BCT   RF,RB610                                                         
         B     RB790                                                            
*                                                                               
RB620    CLI   BUYKPRD,X'FF'       TEST NON-POL BUY                             
         BE    RB650               NO                                           
         CLI   BDTIME,0            YES-TEST PIGGYBACK BUY                       
         BE    *+16                                                             
         TM    SBQPIND2,SBQPIGNO   YES-TEST SUPPRESS PIGGYBACKS                 
         BZ    RB630                                                            
         B     RB790                                                            
         TM    SBQPIND,SBQPIGS     NO-REJECT IF ONLY PIGGIES WANTED             
         BO    RB790                                                            
         CLI   SBQBPRD2,0                                                       
         BNE   RB790                                                            
         B     RB650                                                            
*                                                                               
RB630    CLI   SBQBPRD2,0          PIGGYBACK BUY - TEST 2ND PRD FILTER          
         BE    RB650                                                            
         SR    R0,R0               YES-CHECK FOR 2ND PROD IN P/B ELEM           
         LA    R1,BDELEM                                                        
RB640    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),4                                                          
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RB640                                                            
         USING PBELEM,R1                                                        
         CLC   PBPROD,SBQBPRD2                                                  
         BNE   RB790                                                            
         DROP  R1                                                               
*                                                                               
RB650    OC    SBQSREP,SBQSREP     SPECIAL REP                                  
         BZ    RB656               NO                                           
         TM    SBEFLAG7,SBE7NREP   SPECIAL REP FILTER NEGATIVE?                 
         BZ    RB655               NO                                           
         CLC   BDREP,SBQSREP       REP MATCHES?                                 
         BE    RB790               YES - SKIP                                   
         B     RB656               NO - PROCESS THIS BUY                        
*                                                                               
RB655    CLC   BDREP,SBQSREP       REP MATCHES?                                 
         BNE   RB790               NO - SKIP                                    
RB656    GOTOR VRCPACK,PARM,(C'U',BDREP),SBSREP                                 
*                                                                               
         OC    SBESTTIM,SBESTTIM   START TIME FILTER                            
         BZ    RB660                                                            
         ZICM  RE,BDTIMST,2                                                     
         CHI   RE,600                                                           
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         ZICM  RF,SBESTTIM,2                                                    
         CHI   RF,600                                                           
         BNL   *+8                                                              
         AHI   RF,2400                                                          
         CR    RE,RF                                                            
         BNE   *+16                                                             
         TM    SBETIMIN,SBETISGE+SBETISLE                                       
         BZ    RB790                                                            
         B     RB660                                                            
         BL    *+16                                                             
         TM    SBETIMIN,SBETISGT+SBETISGE                                       
         BZ    RB790                                                            
         B     RB660                                                            
         TM    SBETIMIN,SBETISLT+SBETISLE                                       
         BZ    RB790                                                            
*                                                                               
RB660    OC    SBEENTIM,SBEENTIM   END TIME FILTER                              
         BZ    RB670                                                            
         ZICM  RE,BDTIMEND,2                                                    
         CHI   RE,600                                                           
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         ZICM  RF,SBEENTIM,2                                                    
         CHI   RF,600                                                           
         BNL   *+8                                                              
         AHI   RF,2400                                                          
         CR    RE,RF                                                            
         BNE   *+16                                                             
         TM    SBETIMIN,SBETIEGE+SBETIELE                                       
         BZ    RB790                                                            
         B     RB670                                                            
         BL    *+16                                                             
         TM    SBETIMIN,SBETIEGT+SBETIEGE                                       
         BZ    RB790                                                            
         B     RB670                                                            
         TM    SBETIMIN,SBETIELT+SBETIELE                                       
         BZ    RB790                                                            
*                                                                               
RB670    OC    SBERANGE,SBERANGE   ANY TIME RANGE FILTER?                       
         BZ    RB672                                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SBERANGE       FILTER START                                 
         XR    RE,RE                                                            
         ICM   RE,3,BDTIMST        BUY START                                    
         CR    RE,RF               BUY START < RANGE START?                     
         BL    RB790               YES - FILTER THIS OUT!                       
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SBERANGE+2     FILTER END                                   
         XR    RE,RE                                                            
         ICM   RE,3,BDTIMEND       BUY END                                      
         CR    RE,RF               BUY END > RANGE END?                         
         BH    RB790               YES - FILTER THIS OUT!                       
*                                                                               
RB672    OC    SBEPROG,SBEPROG     ANY PROGRAM FILTER?                          
         BZ    RB680                                                            
         XR    RF,RF                                                            
         IC    RF,SBEPROG                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SBEPROG+1(0),BDPROGRM                                            
         BNE   RB790                                                            
*                                                                               
RB680    OC    SBECDAT,SBECDAT     CREATION DATE FILTER?                        
         BNZ   RB681               YES                                          
         OC    ACTIVDAT,ACTIVDAT   ACTIVITY DATE FILTER?                        
         BZ    RB692               NO                                           
*                                                                               
RB681    LA    R1,BDELEM           FIND ACTIVITY ELEM                           
         SR    R0,R0                                                            
RB690    CLI   0(R1),0                                                          
         BE    RB790               NONE                                         
         CLI   0(R1),X'99'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RB690                                                            
*                                                                               
         USING ACTVELEM,R1                                                      
         OC    SBECDAT,SBECDAT     CREATION DATE FILTER?                        
         BZ    RB690A              NO                                           
         CLC   SBECDAT,ACTVADD+2                                                
         BH    RB790                                                            
*                                                                               
RB690A   OC    ACTIVDAT,ACTIVDAT   ACTIVITY DATE FILTER?                        
         BZ    RB692               NO                                           
         CLC   ACTIVDAT,ACTVCHG+2                                               
         BH    RB790                                                            
         DROP  R1                                                               
*                                                                               
RB692    TM    SBEFLAG2,SBESRC     FILTER ON SRC COMMENTS?                      
         BZ    RB695               NO                                           
         LA    R1,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
RB693    CLI   0(R1),0                                                          
         BE    RB790                                                            
         CLI   0(R1),X'66'                                                      
         BE    *+14                                                             
RB694    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RB693                                                            
*                                                                               
         USING COMELEM,R1                                                       
         CLI   CMLEN,7                                                          
         BL    RB790                                                            
         CLC   =C'SRC=',CMDATA                                                  
         BNE   RB694                                                            
         DROP  R1                                                               
*                                                                               
RB695    TM    SBEFLAG6,SBE6MATN   FILTER OUT MATCH=NO BUYS?                    
         BZ    RB700               NO                                           
         LA    R1,BDELEM           FIRST BUY ELEMENT                            
         XR    R0,R0               CLEAR R0                                     
*                                                                               
RB696    CLI   0(R1),0             END OF BUY RECORD?                           
         BE    RB700               YES                                          
         CLI   0(R1),X'66'         HAVE COMMENT ELEMENT?                        
         BE    RB698               YES                                          
RB697    IC    R0,1(R1)            ELEMENT LENGTH                               
         AR    R1,R0               BUMP ELEMENT                                 
         B     RB696               LOOK FOR ANOTHER COMMENT ELEMENT             
*                                                                               
         USING COMELEM,R1          COMMENT ELEMENT DSECT                        
RB698    CLC   =C'MATCH=NO',CMDATA BUY COMMENT IS MATCH=NO?                     
         BE    RB790               YES - FILTER OUT                             
         B     RB697               CONTINUE LOOKING FOR MORE COMMENTS           
         DROP  R1                  DROP COMMENT ELEMENT USING                   
*                                                                               
RB700    TM    SBEFLAG7,SBE7MGY    ONLY REPORT BUYS WITH MAKEGOODS?             
         BNZ   *+12                YES                                          
         TM    SBEFLAG7,SBE7MGN    ONLY REPORT BUYS WITHOUT MAKEGOODS?          
         BZ    RB701               NO                                           
*                                                                               
         MVI   BYTE,C'N'           SET TO NO MAKEGOOD ON BUYLINE                
         L     RF,AIO3             USE AIO3 FOR MAKEGOOD DSECT & TABLE          
         USING MGABLKD,RF          MAKEGOOD DSECT                               
         XC    0(MGALNQ,RF),0(RF)  CLEAR FOR 175 BYTES                          
         LA    RE,MGALNQ           CURRENTLY 175 BYTES                          
         AR    RE,RF               BUILD MAX ONE TABLE ENTRY                    
         ST    RE,MGATAB           TABLE IS BEYOND PARM AREA                    
         XC    0(MGERECL*2,RE),0(RE) CLEAR FOR 2*ENTRY LEN (68*2)               
         MVC   MGATABLN,=Y(MGERECL*2) OPTIMIZE FOR 1 ENTRY+TOTAL ENTRY          
         MVC   MGAACOM,SBCOMFAC    A(COMFACS)                                   
         ST    R2,MGABUY           PASS A(BUY RECORD)                           
         MVI   MGAACT,MGAQBLN      ACTION = BUILD A BUYLINE TABLE               
         GOTO1 VBLDMGN,(RF)        CALL SPBLDMGN                                
         L     RF,AIO3             A(MAKEGOOD DSECT)                            
         L     RE,MGATAB           A(MAKEGOOD TABLE)                            
         OC    0(MGERECL*2,RE),0(RE) MAKEGOOD ACTIVITY ON THIS LINE?            
         BZ    *+8                 NO                                           
         MVI   BYTE,C'Y'           YES - HAVE MAKEGOOD ON THIS BUY              
         DROP  RF                  DROP MAKEGOOD DSECT                          
*                                                                               
         TM    SBEFLAG7,SBE7MGY    ONLY REPORT BUYS WITH MAKEGOODS?             
         BZ    *+12                NO                                           
         CLI   BYTE,C'Y'           YES - HAVE MAKEGOOD ON BUY?                  
         BNE   RB790               NO - FILTER THIS BUY OUT                     
         TM    SBEFLAG7,SBE7MGN    ONLY REPORT BUYS WITHOUT MAKEGOODS?          
         BZ    RB701               NO                                           
         CLI   BYTE,C'Y'           YES - HAVE MAKEGOOD ON BUY?                  
         BE    RB790               YES - FILTER THIS BUY OUT                    
*                                                                               
RB701    TM    SBEFLAG3,SBE3XBON   FILTER OUT BONUS SPOTS?                      
         BZ    *+14                 NO                                          
         CLC   =C'*B',BDPROGRM     IS THIS A BONUS BUY?                         
         BE    RB790                YES - SKIP IT                               
*                                                                               
         TM    SBEFLAG3,SBE3STYP   FILTER ON -S TYPE BUYS?                      
         BZ    *+12                 NO                                          
         CLI   BDPROGRM+17,0       IS THIS A '-S' BUY?                          
         BNE   RB790                NO - SKIP IT                                
*                                                                               
         BRAS  RE,PBD              WANT ONLY PBD AND FOUND NO PBD ELEM?         
         BNE   RB790               YES - SKIP THIS BUY                          
*                                                                               
RB704    CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK REQUEST                
         BNE   RB730                                                            
         OC    BUYMSTA(2),BUYMSTA  IS THIS MARKET 0                             
         BNZ   RB706                                                            
         CLC   SBQNET,BLANKS       TEST NETWORK FILTER                          
         BNH   *+14                                                             
         CLC   SBQNET,SBSTA        YES-MATCH AGAINST FILTER                     
         BNE   RB790                                                            
         MVC   SBNETWK,SBSTA                                                    
         XC    WORK(12),WORK       ENSURE NO GARBAGE FOR MSPACK                 
         MVC   WORK(4),SBMKT                                                    
         MVC   WORK+4(4),SBNETWK                                                
         MVI   WORK+8,C'N'                                                      
         GOTOR AMSPACK,PARM,WORK,WORK+4,DUB                                     
         MVC   SBBCNTWK,DUB+2      SAVE PACKED CANADIAN NETWORK                 
         MVI   SBBCNTWK+2,0        NWK NULL NOT X'03' IN CLRSTAT RECS           
         B     RB740                                                            
*                                                                               
RB706    SR    R0,R0               YES-LOOK FOR CANADIAN NETWORK                
         LA    R4,BDELEM               STATION ELEMENT                          
*                                                                               
RB710    CLI   0(R4),0                                                          
         BNE   RB720                                                            
         CLI   SBQMED,C'C'         NOT FOUND-TEST MEDIA=COMBINED                
         BNE   RB790               NO-IGNORE THIS BUY                           
         MVI   SBMED,C'T'          YES-OK, MEDIA IS T                           
         B     RB730                                                            
*                                                                               
RB720    CLI   0(R4),X'68'                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RB710                                                            
         CLC   SBQNET,BLANKS       TEST NETWORK FILTER                          
         BNH   *+14                                                             
         CLC   SBQNET,2(R4)        YES-MATCH AGAINST FILTER                     
         BNE   RB790                                                            
         MVI   SBMED,C'N'          SET MEDIA TO NETWORK                         
         NI    FIRSTSW,255-FSTA                                                 
         CLC   SBNETWK,2(R4)       TEST NEW NETWORK                             
         BE    RB740                                                            
         OI    FIRSTSW,FSTA        YES                                          
         OI    SBBUYCH,SBBCSTA     TREAT AS NEW STA FOR CBL AGG                 
         MVC   SBNETWK,2(R4)                                                    
         XC    WORK(12),WORK       ENSURE NO GARBAGE FOR MSPACK                 
         MVC   WORK(4),SBMKT                                                    
         MVC   WORK+4(4),SBNETWK                                                
         MVI   WORK+8,C'N'                                                      
         GOTOR AMSPACK,PARM,WORK,WORK+4,DUB                                     
         MVC   SBBCNTWK,DUB+2      SAVE PACKED CANADIAN NETWORK                 
         MVI   SBBCNTWK+2,0        NWK NULL NOT X'03' IN CLRSTAT RECS           
         B     RB740                                                            
*                                                                               
RB730    XC    SBNETWK,SBNETWK                                                  
*                                                                               
RB740    DS    0H                  SET BUY TYPE                                 
         MVI   SBBYTYPE,C'C'       CASH                                         
         OC    SBDRPROF+6(3),SBDRPROF+6  ANY DARE SPECIAL REP IN PROF?          
         BZ    RB770                     NO, NOT A TRADE BUY                    
         CLC   SBDRPROF+6(3),=C'000'     DARE SPECIAL REP=000?                  
         BE    RB770                     YES, NOT A TRADE BUY                   
         CLI   SBDRPROF+14,C'Y'    MULTIPLE TRADE REP CODES?                    
         BE    *+14                                                             
         CLC   SBSREP,SBDRPROF+6   NO, REP MATCHES FOR 3 DIGITS?                
         B     *+10                                                             
         CLC   SBSREP(2),SBDRPROF+6   YES, REP MATCHES FOR 2 DIGITS?            
         BNE   *+8                 NO, WE HAVE A CASH BUYLINE                   
         MVI   SBBYTYPE,C'T'       TRADE                                        
*&&DO                                                                           
RB740    DS    0H                  SET BUY TYPE                                 
         MVI   SBBYTYPE,C'C'       CASH                                         
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    RB750                                                            
         CLI   BDCIND,BDCNTP                                                    
         BNE   RB770                                                            
         B     RB760                                                            
RB750    TM    BDCIND,X'FE'                                                     
         BNZ   RB770                                                            
RB760    MVI   SBBYTYPE,C'T'       TRADE                                        
*&&                                                                             
RB770    TM    SBQREAD,SBQRDCLS    AND READ CLEARANCE STATUS RECORDS            
         BZ    RB780                                                            
         BRAS  RE,READCLS          YES                                          
*                                                                               
RB780    MVI   SBMODE,SBPROCSP                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
         MVI   SBBUYCH,0                                                        
*                                                                               
RB790    LA    R2,IOKEY            MAKE KEY ADDRESSABLE AGAIN                   
         TM    SBIOFLAG,SBNOIO     TEST NO IO EXECUTED IN HOOK TO USER          
         BO    RB800                                                            
         GOTOR AIO,IOSPTDIR+IOHID  RE-ESTABLISH READ SEQUENCE                   
*                                                                               
RB800    GOTOR AIO,IOSPTDIR+IOSQD  READ NEXT BUY RECORD                         
         B     RB70                                                             
*                                                                               
RBX      TM    SBQREAD2,SBQRD2OM   READ OM KEYWORDS?                            
         JZ    EQEXIT              NO                                           
         MVI   SBMODE,SBPROCDO     DELETE ALL OM RECS FROM TSAR BUFFER          
         BRAS  RE,GO               HOOK TO SPWRI01                              
         J     EQEXIT                                                           
         EJECT                                                                  
PBD      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BUYRECD,R2          R2 = BUY RECORD DSECT                        
*                                                                               
         TM    SBEFLAG3,SBE3PBDM   ONLY WANT BUYS WITH POST BUY DEMOS?          
         JZ    EQEXIT              NO - EXIT CC EQU                             
         LA    R1,BDELEM           A(X'01' ELEMENT)                             
         SR    R0,R0               CLEAR R0                                     
*                                                                               
PBD00    CLI   0(R1),0             END OF BUY RECORD?                           
         JE    NEQEXIT             YES - EXIT CC NEQ                            
         CLI   0(R1),X'22'         POST BUY ELEM *ORIG*?                        
         JE    PBD10               YES - MAKE SURE THERE ARE OVERRIDES          
         CLI   0(R1),X'23'         POST BUY ELEM *SPILL*?                       
         JE    PBD10               YES - MAKE SURE THERE ARE OVERRIDES          
PBD05    IC    R0,1(R1)            ELEM LENGTH                                  
         AR    R1,R0               BUMP TO NEXT ELEM                            
         J     PBD00               CHECK FOR EOR OR PBD ELEM                    
*                                                                               
PBD10    LLC   RE,1(R1)            ELEMENT LENGTH                               
         SHI   RE,3                MINUS ELEM/EX OVERHEAD FOR X'22'ELEM         
         LA    RF,2(R1)            POINT TO DEMO OVERRIDES FOR X'22'            
         CLI   0(R1),X'22'         HAVE A X'22' ELEMENT?                        
         JE    *+12                YES                                          
         SHI   RE,4                NO - X'23' ELEM HAS 4 MORE BYTES             
         AHI   RF,4                AND DEMO OVERRIDES ARE 6 BYTES IN            
         EX    RE,*+8              EXECUTE                                      
         J     *+10                SO IDF DOESN'T COMPLAIN                      
         OC    0(0,RF),0(RF)       HAVE ANY DEMO OVERRIDES?                     
         JZ    PBD05               NO - DON'T COUNT ELEM AS PBD!                
         J     EQEXIT              EXIT CC EQU                                  
*                                                                               
* READ THE GOAL RECORDS                                                         
*                                                                               
READGOLS NTR1  BASE=*,LABEL=*                                                   
         XC    SBNETWK,SBNETWK     CLEAR THE NETWORK                            
         CLI   SBQMED,C'*'                                                      
         BE    *+10                                                             
         MVC   SBMED,SBQMED        SET REQUESTED MEDIA                          
         MVC   SVBAGYMD,SBBAGYMD   SAVE BINARY AGENCY/MEDIA                     
         TM    SBQRDOPT,SBQROCGL   TEST READ MEDIA C/T/N GOALS FOR              
         BZ    RG4                 MEDIA=C                                      
         MVC   GLMEDS(3),=C'CTN'   YES-                                         
         MVI   GLMEDS+3,X'FF'                                                   
         LA    R1,GLMEDS                                                        
         ST    R1,AGLMED                                                        
*                                                                               
RG1      CLI   0(R1),X'FF'                                                      
         BE    RGX                                                              
         LA    RE,8                START WITH MEDIA=C                           
         CLI   0(R1),C'C'                                                       
         BE    RG2                                                              
         LA    RE,1                MEDIA=T                                      
         CLI   0(R1),C'T'                                                       
         BE    RG2                                                              
         LA    RE,3                MEDIA=N                                      
*                                                                               
RG2      MVC   SBMED,0(R1)                                                      
         LLC   RF,SVBAGYMD                                                      
         SRL   RF,4                                                             
         SLL   RF,4                                                             
         OR    RF,RE                                                            
         STC   RF,SBBAGYMD                                                      
*                                                                               
RG4      MVI   SBBEST,0                                                         
         MVI   SBBPRD,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING GOALRECD,R2                                                      
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SBBAGYMD                                                  
         MVC   GKEYCLT,SBBCLT                                                   
         LA    R4,GKEYPRD-GKEY-1                                                
         MVI   QCPPGUID,C'N'                                                    
         TM    SBQPIND,SBQPOLCP    TEST CPP GUIDES REQUESTED                    
         BZ    RG6                                                              
         CLC   SBQPRD,=C'POL'      AND IT'S A POL REQUEST                       
         BNE   RG6                                                              
         MVI   QCPPGUID,C'Y'       YES                                          
         MVI   GKEYPRD,X'FF'                                                    
         B     RG8                                                              
*                                                                               
RG6      CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BE    RG10                                                             
         CLI   SBQBPRD,X'FF'                                                    
         BE    RG10                                                             
         MVC   GKEYPRD,SBQBPRD     YES                                          
*                                                                               
RG8      LA    R4,L'GKEYPRD(R4)                                                 
         MVC   GKEYMKT,QBMKTST                                                  
         CLC   QBMKTST,QBMKTND                                                  
         BNE   RG10                                                             
         LA    R4,L'GKEYMKT(R4)                                                 
*                                                                               
RG10     LA    R2,IOKEY                                                         
         LA    R1,IOSPTDIR+IOHID   READ HIGH                                    
         B     RG14                                                             
*                                                                               
RG12     LA    R1,IOSPTDIR+IOSQD   READ SEQUENTIAL                              
*                                                                               
RG14     DS    0H                                                               
         GOTOR AIO                                                              
*                                                                               
* IF AM, CLT, PRD, EST, MKT CHANGES, NEED TO READ NEW PW REC                    
         CLC   GKEY(GKEYDPT-GKEYTYPE),IOKEYSAV                                  
         BE    *+8                                                              
         OI    FLAG2,FLGETPW       SET TO READ NEW PW MKT REC                   
*                                                                               
RG15     EX    R4,*+8              R4 = KEY COMPARE LENGTH - 1                  
         B     *+10                                                             
         CLC   GKEY(0),IOKEYSAV                                                 
         BNE   RG92                                                             
         CLI   QCPPGUID,C'Y'       IF NOT READING CPP GUIDE,                    
         BE    *+12                                                             
         CLI   GKEYPRD,X'FF'       TEST FOR CPP GUIDE                           
         BE    RG92                YES - FINISHED                               
         TM    SBQPIND2,SBQPIGNO   TEST SUPPRESS PIGGYBACKS                     
         BZ    *+12                                                             
         CLI   GKEYPRD2,0          YES-GET NEXT GOAL IF PIGGYBACK               
         BNE   RG12                                                             
         MVI   PRD2ONLY,C'N'                                                    
         CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BE    RG18                                                             
         CLI   SBQBPRD,X'FF'       TEST FOR 'POL' REQUEST                       
         BE    RG20                YES-RETURN ALL PRODUCTS                      
         CLC   SBQBPRD,GKEYPRD     NO - FILTER ON PRODUCT                       
         BE    RG20                                                             
         BL    RG92                                                             
         MVC   GKEYPRD,SBQBPRD     SKIP TO REQ PRODUCT                          
         XC    GKEYMKT(8),GKEYMKT                                               
         B     RG10                                                             
*                                                                               
RG18     CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   RG20                                                             
         LLC   RE,GKEYPRD          FIND PRDGRP                                  
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    RE,R1                                                            
         USING PRDBUFFD,RE                                                      
         OC    PBGROUP,PBGROUP                                                  
         BNZ   RG20                                                             
         CLI   GKEYPRD2,0          IS THERE A SECOND PRODUCT?                   
         BE    RG12                NO-READ NEXT GOAL                            
         LLC   RE,GKEYPRD2         YES-TRY IT                                   
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         AR    RE,R1                                                            
         OC    PBGROUP,PBGROUP                                                  
         BZ    RG12                                                             
         MVI   PRD2ONLY,C'Y'       YES-INDICATE PRD 2 ONLY IS GOOD              
         DROP  RE                                                               
*                                                                               
RG20     CLC   GKEYMKT,QBMKTST     TEST MARKET WITHIN RANGE                     
         BL    *+18                                                             
         CLC   GKEYMKT,QBMKTND                                                  
         BH    RG55                DONE WITH MARKET - NEXT PRODUCT              
         B     RG24                                                             
         MVC   GKEYMKT,QBMKTST     SKIP TO MARKET START                         
         XC    GKEYEST(6),GKEYEST                                               
         B     RG10                                                             
*                                                                               
RG24     CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP FILTERING              
         BNH   RG26                NO                                           
         MVC   SVMKT,GKEYMKT       YES-CHECK FOR VALID MKTGRP                   
         MVC   SVPRD,GKEYPRD                                                    
         GOTOR ACHKMGRP                                                         
         BNE   RG50                MARKET NOT IN MKTGRP - READ NEXT MKT         
*                                                                               
RG26     ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   RG28                YES                                          
         LLC   R3,GKEYEST                                                       
         LA    R3,ESTTAB(R3)                                                    
         CLI   0(R3),0             TEST ACTIVE ESTIMATE                         
         BE    RG45                NO                                           
         B     RG30                YES                                          
*                                                                               
RG28     LLC   R3,GKEYPRD          PRODUCT                                      
         CLI   PRD2ONLY,C'Y'                                                    
         BNE   *+8                                                              
         IC    R3,GKEYPRD2                                                      
         CLI   SBQBPRD,X'FF'       TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    R3,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  R3,0                                                             
         SLL   R3,8                X 256                                        
         LLC   R0,GKEYEST          ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    R3,R0                                                            
         AR    R3,R1                                                            
         CLI   0(R3),0             TEST ACTIVE                                  
         BE    RG45                NO                                           
*                                                                               
RG30     CLI   SBQDPT,0            TEST DAYPART FILTER                          
         BE    RG32                                                             
         GOTOR AFILTDPT,GKEYDPT    YES                                          
         BE    RG32                                                             
         MVC   GKEYSLN(4),XFF                                                   
         B     RG10                                                             
*                                                                               
RG32     CLI   SBQLEN,0            TEST LENGTH FILTER                           
         BE    RG34                                                             
         CLC   GKEYSEC,SBQLEN      YES                                          
         BE    RG34                                                             
         BH    RG40                                                             
         MVC   GKEYSEC,SBQLEN                                                   
         XC    GKEYAGY(2),GKEYAGY                                               
         B     RG10                                                             
*                                                                               
RG34     CLI   SBQBPRD2,0          TEST 2ND PRODUCT FILTER                      
         BE    RG36                                                             
         CLC   SBQBPRD2,GKEYPRD2   YES-CHECK 2ND PRODUCT                        
         BNE   RG90                                                             
         B     RG60                                                             
*                                                                               
RG36     TM    SBQPIND,SBQPIGS     TEST PIGGYBACKS ONLY                         
         BZ    RG60                                                             
         CLI   GKEYPRD2,0          YES-TEST FOR 2ND PRODUCT                     
         BE    RG90                                                             
         B     RG60                                                             
*                                                                               
RG40     MVC   GKEYSEC(3),XFF      NEXT SPOT LENGTH                             
         B     RG10                                                             
*                                                                               
RG42     MVC   GKEYSLN(4),XFF      NEXT DAYPART                                 
         B     RG10                                                             
*                                                                               
RG45     MVC   GKEYDPT(5),XFF      NEXT ESTIMATE                                
         B     RG10                                                             
*                                                                               
RG50     MVC   GKEYEST(6),XFF      NEXT MARKET                                  
         B     RG10                                                             
*                                                                               
RG55     MVC   GKEYMKT(8),XFF      NEXT PRODUCT                                 
         B     RG10                                                             
*                                                                               
RG60     ICM   R5,15,SBAESTBF      SET DAYPART MENU                             
         BNZ   *+14                                                             
         MVC   SBDPTMEN,0(R3)                                                   
         B     RG61                                                             
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R5,RF                                                            
         USING ESTBUFFD,R5                                                      
         MVC   SBDPTMEN,EBDPTMEN   SET DAYPART MENU                             
         MVC   SBESTFLT,EBFILT     SET ESTIMATE FILTERS                         
         MVC   SBRTLSCH,EBRTLSCH   SET ESTIMATE'S RETAIL SCHEME CODE            
         CLI   SBESREP,C'Y'        TEST SPECIAL REPS REQUIRED                   
         BNE   RG61                                                             
         GOTOR VRCPACK,PARM,(C'U',EBSREP),SBSREP                                
         DROP  R5                                                               
*                                                                               
RG61     CLI   SBQMASDP,0          TEST MASTER DAYPART FILTER                   
         BE    RG62                                                             
         LA    R1,GKEYDPT          YES                                          
         GOTOR ACHKMDPT                                                         
         BNE   RG42                DPT REJECTED - NEXT DAYPART                  
*                                                                               
RG62     CLC   GKEYPRD,SBBPRD      TEST PRODUCT CHANGE                          
         BE    RG65                                                             
         MVC   SBBPRD,GKEYPRD                                                   
         L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
RG63     CLC   SBBPRD,3(RF)                                                     
         BE    RG64                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    RG63                                                             
         B     RG55                PRD NOT IN CLTHDR - READ NEXT PRD            
*                                                                               
RG64     MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    RG65                                                             
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         OC    SBBPGR,SBBPGR                                                    
         BNZ   *+10                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
RG65     CLC   SBBEST,GKEYEST      TEST ESTIMATE CHANGE                         
         BE    RG66                                                             
         MVC   SBBEST,GKEYEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
*                                                                               
RG66     CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    RG68                                                             
         CLC   SBBMKT,GKEYMKT      YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    RG68                                                             
         MVC   SVMKT,GKEYMKT       YES-SET THE MARKET GROUP                     
         MVC   SVPRD,GKEYPRD                                                    
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
RG68     CLC   SBBMKT,GKEYMKT      TEST MARKET CHANGE                           
         BE    RG70                                                             
         MVC   SBBMKT,GKEYMKT      YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   RG70                                                             
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
RG70     GOTOR AIO,IOSPTFIL+IOGET+IO1   GET THE GOAL RECORD                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    RG90                                                             
         L     R2,AIO1                                                          
         SPACE 1                                                                
*                                                                               
         OC    SBECDAT,SBECDAT     CREATION DATE FILTER?                        
         BZ    RG70A               YES                                          
         GOTOR CDATCON,PARM,(3,SBECDAT),(2,WORK)                                
*                                                                               
RG70A    OC    ACTIVDAT,ACTIVDAT   ACTIVITY DATE FILTER?                        
         BZ    RG70B               NO                                           
         GOTOR CDATCON,PARM,(3,ACTIVDAT),(2,WORK+2)                             
*                                                                               
RG70B    LR    RF,R2               GOAL RECORD                                  
         LA    RF,24(RF)           FIRST ELEMENT                                
         CLI   0(RF),X'20'         HAVE GOAL DESCRIPTION ELEMENT?               
         BNE   RG90                NO, SKIP THIS GOAL RECORD                    
*                                                                               
         USING GDELEM,RF                                                        
         OC    SBECDAT,SBECDAT     CREATION DATE FILTER?                        
         BZ    RG70C               NO                                           
         CLC   WORK(2),GREDATE                                                  
         BH    RG90                                                             
*                                                                               
RG70C    OC    ACTIVDAT,ACTIVDAT   ACTIVITY DATE FILTER?                        
         BZ    RG71                NO                                           
         CLC   WORK+2(2),GACTDATE                                               
         BH    RG90                                                             
         DROP  RF                                                               
*                                                                               
RG71     TM    SBEFLAG,SBEWIPW     SKIP UNLESS EXTRACTING PW DATA               
         BZ    RG72                                                             
         TM    FLAG2,FLGETPW       KEY CHANGE?                                  
         BZ    RG72                 NO                                          
*                                                                               
* BUILD PW KEY IF NEEDED                                                        
         XC    SVKEY,SVKEY                                                      
         LA    RF,SVKEY                                                         
         USING PWRECD,RF                                                        
         MVC   PWKTYP,=X'0D7A'     ESTIMATE                                     
         MVC   PWKAGMD(3),GKEYAM   AGMD/CLT                                     
         MVC   PWKPRD,GKEYPRD      PRODUCT                                      
         MVC   PWKEST,GKEYEST      ESTIMATE                                     
         MVC   PWKMKT,GKEYMKT      MARKET                                       
         DROP  RF                                                               
*                                                                               
         OI    FLAG2,SKIPSTA       DON'T READ PW STA REC                        
         GOTOR AGETPWM                                                          
*                                                                               
* GET CPP GUIDE                                                                 
*                                                                               
RG72     CLI   QCPPGUID,C'Y'       TEST READING CPP GUIDES ALREADY              
         BE    RG80                                                             
         CLI   SBCPROF+8,C'0'      TEST CLIENT USES CPP GUIDE                   
         BH    RG80                NO                                           
         MVC   WORK(L'IOKEY),IOKEY                                              
         LA    R3,IOKEY                                                         
         MVI   GKEYPRD-GKEY(R3),X'FF'   GOAL PRODUCT POL                        
         XC    GKEYSLN-GKEY(4,R3),GKEYSLN-GKEY(R3)                              
         MVI   FLAG,0                                                           
         L     R1,AIO2             AIO2 = A(FIRST CPP GUIDE)                    
         ST    R1,IOADDR                                                        
         XC    0(256,R1),0(R1)                                                  
         L     R1,AIO3             AIO3 = A(SECOND CPP GUIDE)                   
         XC    0(256,R1),0(R1)                                                  
         CLI   GDCPPES,0                                                        
         BE    RG75                                                             
         MVC   GKEYCLT-GKEY(L'GKEYCLT,R3),GDCPPCL                               
         MVC   GKEYEST-GKEY(L'GKEYEST,R3),GDCPPES                               
         MVC   FLAG,GDCPPES2       SAVE 2ND CPP EST                             
*                                                                               
RG75     GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(GKEYDPT-GKEY+L'GKEYDPT),IOKEYSAV                           
         BE    RG78                                                             
*                                                                               
RG76     CLI   FLAG,0              TEST NEED 2ND CPP GUIDE                      
         BE    RG79                NO - DONE                                    
         MVC   IOKEY,IOKEYSAV      RESTORE KEY                                  
         MVC   GKEYEST-GKEY(L'GKEYEST,R3),FLAG   2ND EST                        
         MVI   FLAG,0                                                           
         L     R1,AIO3                                                          
         MVC   IOADDR,AIO3                                                      
         B     RG75                                                             
*                                                                               
RG78     GOTOR AIO,IOSPTFIL+IOGET  GET CPP GUIDE RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RG76                                                             
*                                                                               
RG79     MVC   IOKEY,WORK          RESTORE KEY                                  
         GOTOR AIO,IOSPTDIR+IOHID                                               
         BE    RG80                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
* HOOK TO USER                                                                  
*                                                                               
RG80     MVI   SBMODE,SBPROCGL                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
*                                                                               
RG90     LA    R2,IOKEY                                                         
         GOTOR AIO,IOSPTDIR+IOHID  READ NEXT GOAL RECORD                        
         B     RG12                                                             
*                                                                               
RG92     TM    SBQRDOPT,SBQROCGL   TEST READ MEDIA=C/T/N FOR MEDIA=C            
         BZ    RGX                                                              
         L     R1,AGLMED           YES-READ GOALS FOR NEXT MEDIA                
         LA    R1,1(R1)                                                         
         ST    R1,AGLMED                                                        
         B     RG1                                                              
*                                                                               
RGX      J     EQEXIT                                                           
         EJECT                                                                  
* READ INVOICE RECORDS                                                          
*                                                                               
READINV  NTR1  BASE=*,LABEL=*                                                   
         XC    INVST(4),INVST                                                   
         OC    SBNDATES,SBNDATES   TEST DATES PASSED                            
         BZ    RN1                                                              
         ICM   R2,15,SBADATE                                                    
         BZ    RN1                                                              
         GOTOR CDATCON,PARM,(2,0(R2)),(0,DUB)  YES-GET BRDCST MNTH STRT         
         GOTOR VGETBROD,(R1),(1,DUB),WORK,CGETDAY,CADDAY  OF START DATE         
         GOTOR CDATCON,(R1),(0,WORK),(2,INVST)                                  
         L     R1,SBNDATES                                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R2,R1                                                            
         GOTOR CDATCON,PARM,(2,2(R2)),(0,DUB)  GET BROADCAST MONTH STRT         
         GOTOR VGETBROD,(R1),(1,DUB),WORK,CGETDAY,CADDAY  OF END DATE           
         GOTOR CDATCON,(R1),(0,WORK),(2,INVEN)                                  
*                                                                               
RN1      MVI   SBBEST,0                                                         
         MVI   SBBPRD,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SBNETWK,SBNETWK                                                  
*                                  FIRST SET START-END MOS                      
         MVC   INVSTMOS,XFF                                                     
         XC    INVENMOS,INVENMOS                                                
         OC    INVST(4),INVST                                                   
         BZ    RN51                                                             
*                                                                               
         GOTOR CDATCON,PARM,(2,INVST),(0,DUB)                                   
         GOTOR CADDAY,(R1),DUB,DUB,7   (DATE+7 WILL BE IN RIGHT MON)            
         MVC   DUB+4(2),=C'01'         1ST OF MONTH                             
         GOTOR CDATCON,(R1),(0,DUB),(2,WORK)                                    
         MVC   INVSTMOS,WORK                                                    
         XC    INVSTMOS,XFF          COMPLEMENT                                 
*                                                                               
         GOTOR CDATCON,PARM,(2,INVEN),(0,DUB)                                   
         GOTOR CADDAY,(R1),DUB,DUB,7   (DATE+7 WILL BE IN RIGHT MON)            
         MVC   DUB+4(2),=C'01'         1ST OF MONTH                             
         GOTOR CDATCON,(R1),(0,DUB),(2,WORK)                                    
         MVC   INVENMOS,WORK                                                    
         XC    INVENMOS,XFF          COMPLEMENT                                 
*                                                                               
RN51     DS    0H                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING SNVKEYD,R2                                                       
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,SBBAGYMD                                                  
*                                                                               
RN52     LA    R2,IOKEY                                                         
         GOTOR AIO,IOXSPDIR+IOHI                                                
*                                                                               
RN54     CLC   SNVKEY(3),IOKEYSAV  CHECK AGENCY/MEDIA                           
         BNE   RNX                                                              
         CLC   SNVKCLT,SBBCLT      CHECK CLIENT                                 
         BE    RN55                                                             
         BH    RNX                 HI-FINISHED                                  
         MVC   SNVKCLT,SBBCLT      LO-ADVANCE TO CLIENT                         
         MVC   SNVKSTA,QBSTA                                                    
         MVC   SNVKMOS,INVENMOS    (MOS IS COMPLEMENTED)                        
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
*                                                                               
RN55     XC    SVSTA,SVSTA                                                      
         XC    STAFILT,STAFILT                                                  
         MVC   STAFILT+2(3),SNVKSTA                                             
         GOTOR ACBLFILT                                                         
         BNE   RN68                NO - NEXT STATION                            
*                                                                               
         OC    QBNET,QBNET         NETWORK FILTER?                              
         BZ    RN56                 NO                                          
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    *+12                 NFC (NO CABLE)                              
         CLI   SNVKSTA,X'E8'       TEST CABLE                                   
         BNL   RN68                YES - NEXT STATION                           
*                                                                               
         CLC   SNVKSTA(2),QBNET                                                 
         BE    RN62                                                             
         BH    RN70                HI-SKIP TO NEXT CLIENT                       
         MVC   SNVKSTA(2),QBNET    LO-ADVANCE TO STATION                        
         MVI   SNVKSTA+2,0                                                      
         B     RN57                                                             
*                                                                               
RN56     OC    QBSTA,QBSTA         TEST FOR STATION FILTER                      
         BZ    RN58                                                             
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    *+12                 NFC (NO CABLE)                              
         CLI   SNVKSTA,X'E8'       TEST CABLE                                   
         BNL   RN58                YES - MUST HAVE PASSED ALREADY               
*                                                                               
         CLC   SNVKSTA,QBSTA       YES-CHECK THE STATION                        
         BE    RN62                                                             
         BH    RN70                HI-SKIP TO NEXT CLIENT                       
         MVC   SNVKSTA,QBSTA       LO-ADVANCE TO STATION                        
*                                                                               
RN57     MVC   SNVKMOS,INVENMOS                                                 
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
* IF MKTGRP ID'S ACTIVE, NEED REC BEFORE MKT FILTERING                          
RN58     CLI   SBCEXTRA+2,C'Y'     TEST MKTGRP ID'S ACTIVE THIS CLT             
         BE    RN59                 NO                                          
         CLI   SBCEXTRA+2,C'A'                                                  
         BL    RN59                                                             
         CLI   SBCEXTRA+2,C'Z'                                                  
         BNH   RN62                                                             
*                                                                               
RN59     XC    STAFILT,STAFILT     CHECK MKT FILTERS                            
         MVC   STAFILT+2(3),SNVKSTA                                             
         BRAS  RE,NVMKFLT                                                       
         BNE   RN68                NEXT STA                                     
*                                                                               
RN62     OC    INVENMOS,INVENMOS   TEST HAVE A END MOS                          
         BZ    RN64                                                             
         CLC   SNVKMOS,INVENMOS    YES-TEST MONTH AFTER END                     
         BNL   RN64                                                             
         MVC   SNVKMOS,INVENMOS    YES-ADVANCE TO END                           
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
RN64     CLC   INVSTMOS,XFF        TEST HAVE A START MONTH                      
         BE    RN80                                                             
         CLC   SNVKMOS,INVSTMOS    YES-TEST BEFORE START MONTH                  
         BNH   RN80                                                             
*                                                                               
RN68     DS    0H                  NEXT STATION                                 
         MVC   SNVKMOS,XFF                                                      
         B     RN52                                                             
*                                                                               
RN70     MVC   SNVKSTA(3),XFF      SKIP TO NEXT CLIENT                          
         XC    SNVKINV(20),SNVKINV                                              
         B     RN52                                                             
*                                                                               
* IF MKTGRP ID'S ACTIVE, NEED REC BEFORE MKT FILTERING                          
RN80     CLI   SBCEXTRA+2,C'Y'     TEST MKTGRP ID'S ACTIVE THIS CLT             
         BE    RN82                 NO                                          
         CLI   SBCEXTRA+2,C'A'                                                  
         BL    RN82                                                             
         CLI   SBCEXTRA+2,C'Z'                                                  
         BNH   RN90                                                             
*                                                                               
RN82     XC    STAFILT,STAFILT     PROCESS NEW? MARKET/STA                      
         MVC   STAFILT+2(3),SNVKSTA                                             
         BRAS  RE,NVPMS                                                         
         BNE   RN68                NEXT STA                                     
*                                                                               
RN90     GOTOR AIO,IOXSPDIR+IOHI                                                
         GOTOR AIO,IOXSPFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO1                                                          
         LA    R5,SNVELS-SNVKEY(R5)                                             
         CLI   0(R5),X'10'         MAKE SURE X'10' EL                           
         BNE   RN92                                                             
*                                                                               
         USING SNVHDELD,R5                                                      
*                                                                               
         TM    SBEINV,SBEIEAY      EASI INVOICES ONLY?                          
         BZ    *+14                 NO                                          
         OC    SNVHDEZS,SNVHDEZS   EASI INVOICE?                                
         BZ    RNSKIP               NO - SKIP IT                                
*                                                                               
         TM    SBEINV,SBEIEAN      SKIP EASI INVOICES?                          
         BZ    *+14                 NO                                          
         OC    SNVHDEZS,SNVHDEZS   EASI INVOICE?                                
         BNZ   RNSKIP               YES - SKIP IT                               
*                                                                               
         TM    SBEINV,SBEIMCY      MCT INVOICES ONLY?                           
         BZ    *+12                 NO                                          
         TM    SNVHDCTL,SNVHDMCQ   MCT INVOICE?                                 
         BZ    RNSKIP               NO - SKIP IT                                
*                                                                               
         TM    SBEINV,SBEIMCN      SKIP MCT INVOICES?                           
         BZ    *+12                 NO                                          
         TM    SNVHDCTL,SNVHDMCQ   MCT INVOICE?                                 
         BNZ   RNSKIP               YES - SKIP IT                               
*                                                                               
         TM    SBEINV,SBEIPAID     PAID INVOICES ONLY?                          
         BZ    *+12                                                             
         TM    SNVHDCTL,SNVHDPDQ   PAID INVOICE?                                
         BZ    RNSKIP               NO                                          
*                                                                               
         TM    SBEINV,SBEIUNPD     UNPAID INVOICES ONLY?                        
         BZ    *+12                                                             
         TM    SNVHDCTL,SNVHDPDQ   PAID INVOICE?                                
         BNZ   RNSKIP               YES                                         
*                                                                               
         TM    SBEINV,SBEIAUD      ONLY PASS AUDIT RECS?                        
         BZ    *+12                                                             
         TM    SNVHDCTL,SNVHDAUQ   REC MARKED FOR AUDIT?                        
         BZ    RNSKIP               NO - NEXT INVOICE #                         
*                                                                               
         CLI   SBCEXTRA+2,0        TEST MKTGRP ID'S ACTIVE THIS CLT             
         BE    RN92                 NO                                          
         CLI   SBCEXTRA+2,C'Y'                                                  
         BE    RN92                                                             
         XC    STAFILT,STAFILT     CHECK MKT FILTERS                            
         CLC   SNVHDCON(1),SBCEXTRA+2  RIGHT MKGRP ID?                          
         BNE   RN91                                                             
         BRAS  RE,MGREQ                                                         
*                                                                               
RN91     L     RF,AIO1                                                          
         MVC   STAFILT+2(3),SNVKSTA-SNVKEY(RF)                                  
         BRAS  RE,NVMKFLT                                                       
         BNE   RNSKIP              DON'T GO TO NXTSTA - NOT IN MKT SEQ!         
         BRAS  RE,NVPMS                                                         
         BNE   RNSKIP                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
* HOOK TO USER                                                                  
*                                                                               
RN92     DS    0H                                                               
         MVI   SBMODE,SBPROCNV                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
         CLI   SBMODE,SBSKIP       IF MODE SET TO SKIP, JUMP TO NXT INV         
         BE    RNSKIP                                                           
*                                                                               
RN99     GOTOR AIO,IOXSPDIR+IOSQ   READ NEXT INVOICE RECORD                     
         B     RN54                                                             
*                                                                               
RNSKIP   MVC   SNVKMINK,XFF               SKIP TO NEXT INVOICE#                 
         GOTOR AIO,IOXSPDIR+IOHI                                                
         B     RN99                                                             
*                                                                               
RNX      J     EQEXIT                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CHECK THE INVOICE RECORD AGAINST MARKET FILTERS                     *         
*                                                                     *         
* ON ENTRY: STAFILT(2)   = OPTIONAL PACKED MKT (OR NULLS)             *         
*           STAFILT+2(3) = PACKED STATION                             *         
* EXIT CC NEQ TO SKIP THIS STATION                                    *         
***********************************************************************         
*                                                                               
NVMKFLT  NTR1                                                                   
         OC    QBMKT,QBMKT         TEST FOR MARKET FILTER                       
         BNZ   *+14                                                             
         CLC   SBQMGRF,BLANKS      OR MARKET GROUP FILTERING                    
         BNH   NVMFEQ                                                           
*        CLC   SBBSTA,STAFILT+2    TEST NEW STATION                             
*        BE    NVMFEQ               NO-THEN STATION'S ALREADY OK                
         MVC   SVSTA,STAFILT+2                                                  
         GOTOR GETSTREC            GET STATION'S MARKET                         
         BNE   NVMFNEQ                                                          
*                                                                               
         OC    STAFILT(2),STAFILT                                               
         BZ    *+10                                                             
         MVC   SVMKT,STAFILT                                                    
*                                                                               
         OC    QBMKT,QBMKT                                                      
         BZ    NVMF10                                                           
         CLC   QBMKT,SVMKT                                                      
         BE    NVMFEQ                                                           
         BH    NVMFNEQ             NEXT STATION                                 
         TM    SBQRDOPT,SBQROMAO   TEST READ MKTS 'AS OF' REQUESTED MKT         
         BO    NVMFEQ              YES                                          
         B     NVMFNEQ             NEXT STATION                                 
*                                                                               
NVMF10   MVI   SVPRD,0                                                          
         GOTOR ACHKMGRP            CHECK VALID MARKET GROUP                     
         BNE   NVMFNEQ              NO-NEXT STATION                             
*                                                                               
NVMFEQ   J     EQEXIT                                                           
NVMFNEQ  J     NEQEXIT                                                          
*                                                                               
***********************************************************************         
* PROCESS STATION/MKT FIRST CALLS FOR INVOICES                        *         
*                                                                     *         
* ON ENTRY: STAFILT(2)   = OPTIONAL PACKED MKT (OR NULLS)             *         
*           STAFILT+2(3) = PACKED STATION                             *         
* EXIT CC NEQ TO SKIP THIS STATION                                    *         
***********************************************************************         
*                                                                               
NVPMS    NTR1                                                                   
         NI    FLAG2,X'FF'-FLSTA   TURN OFF SAME STATION FLAG                   
         CLC   SBBSTA,STAFILT+2    NEW STATION?                                 
         BNE   *+8                 NO                                           
         OI    FLAG2,FLSTA         YES - TURN ON FLAG                           
*                                                                               
         MVC   SBBSTA,STAFILT+2    YES-                                         
*                                                                               
         CLI   SBQAFFIL,C' '       TEST STATION DETAILS NEEDED                  
         BH    *+12                                                             
         CLI   SBQSGRD,C' '                                                     
         BNH   NVPMS10                                                          
         MVC   SVSTA,SBBSTA                                                     
         GOTOR AGETSTA                                                          
         BNE   NVPMSNEQ                                                         
*                                                                               
         MVC   SVMKT,SBBSTMKT      IN CASE GETSTA DOESN'T CALL GETSTRC          
         OC    STAFILT(2),STAFILT                                               
         BZ    *+10                                                             
         MVC   SVMKT,STAFILT                                                    
*                                                                               
         GOTOR ACHKSTA                                                          
         BNE   NVPMSNEQ                                                         
         B     NVPMS20                                                          
*                                                                               
NVPMS10  OC    SVSTA,SVSTA         TEST STATION'S MARKET FOUND YET              
         BNZ   NVPMS20                                                          
         MVC   SVSTA,SBBSTA        NO-                                          
         GOTOR GETSTREC            GET THE MARKET                               
         BNE   NVPMSNEQ                                                         
         CLI   SBQSTA+4,C'D'       DIGITAL REQUEST?                             
         BE    NVPMS11             YES                                          
         CLI   SBQSTA+4,C'S'       STREAMING REQUEST?                           
         BE    NVPMS11             YES                                          
         CLI   SBQSTA+4,C'C'       IHEART STREAMING REQUEST?                    
         BNE   NVPMS15             NO                                           
*                                                                               
NVPMS11  GOTOR AMSUNPK,PARM,(X'80',SVMKT),WORK,WORK+4                           
         CLC   WORK+8(1),SBQSTA+4  MATCH ON DIGITAL/STREAMING?                  
         BNE   NVPMSNEQ            NO                                           
NVPMS15  OC    STAFILT(2),STAFILT                                               
         BZ    *+10                                                             
         MVC   SVMKT,STAFILT                                                    
*                                                                               
NVPMS20  CLC   SBBMKT,SVMKT        TEST NEW MARKET                              
         BE    NVPMS40                                                          
         MVC   SBBMKT,SVMKT        YES-                                         
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    NVPMS30                                                          
         MVI   SVPRD,0             YES-SET MARKET GROUP                         
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
NVPMS30  GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   NVPMS40                                                          
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
NVPMS40  TM    FLAG2,FLSTA         SAME STA AS BEFORE?                          
         BNZ   NVPMSEQ             YES, ALREADY HONORED BY-ID FOR MKT           
*                                                                               
         GOTOR AMSUNPK,PARM,(X'80',SVMKTSTA),WORK,WORK+4                        
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
         CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   NVPMSEQ             NO                                           
         CLC   SBSTYPE,SBQSTATY    YES-CHECK STATION TYPE                       
         BNE   NVPMSNEQ            (ASSUMES HOOK SET TYPE)                      
*                                                                               
NVPMSEQ  J     EQEXIT                                                           
NVPMSNEQ J     NEQEXIT                                                          
         EJECT                                                                  
*                                                                               
* READ BILL HEADER RECORDS                                                      
*                                                                               
READBH   NTR1  BASE=*,LABEL=*                                                   
         MVI   SBBEST,0                                                         
         XC    SBPRD,SBPRD                                                      
         XC    SBBMKT,SBBMKT                                                    
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING BILLRECD,R2                                                      
         MVC   BKEYAM,SBBAGYMD                                                  
         MVC   BKEYCLT,SBBCLT                                                   
         LA    R4,BKEYPRD-BKEY-1                                                
         MVI   BKEYPRD,C'A'                                                     
         CLC   SBQPRD,=C'ALL'                                                   
         BE    RH1                                                              
         CLC   SBQPRD,=C'POL'                                                   
         BE    RH1                                                              
         MVC   BKEYPRD,SBQPRD                                                   
         LA    R4,L'BKEYPRD(R4)                                                 
         CLC   SBQEST,SBQESTND    SINGLE ESTIMATE?                              
         BNE   RH1                NO - DON'T FILTER                             
         MVC   BKEYEST,SBQEST                                                   
         MVC   BKEYYSRV(2),SBBQST                                               
         LA    R4,L'BKEYEST(R4)                                                 
*                                                                               
RH1      LA    R2,IOKEY            READ HIGH                                    
         GOTOR AIO,IOSPTDIR+IOHID                                               
*                                                                               
RH2      EX    R4,*+8              R4=KEY COMPARE LENGTH-1                      
         B     *+10                                                             
         CLC   BKEY(0),IOKEYSAV                                                 
         BNE   RHX                                                              
         CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BNE   RH3                                                              
         OC    SBAESTTB,SBAESTTB   AND PROD/EST BUFFER NOT PASSED               
         BNZ   RH3                                                              
         CLI   BKEYEST,0           AND THIS IS AN ESTIMATE HEADER               
         BE    RH5                                                              
         OC    BKEYYSRV(5),BKEYYSRV                                             
         BNZ   RH6                                                              
         GOTOR ACHKEST             YES-CHECK ESTIMATE IS VALID                  
         BE    RH5                 YES                                          
         B     RH1                 NO-SKIP                                      
*                                                                               
RH3      OC    BKEYYSRV(5),BKEYYSRV MAKE SURE BILL HEADER RECORD                
         BZ    RH5                  AND NOT CLT,EST,PROD HEADER RECORD          
         CLI   SBQYMST,0            TEST YEAR/MONTH FILTERS                     
         BE    RH6                                                              
         CLC   BKEYMBIL,SBQYMST     YES-                                        
         BNL   RH4                                                              
         CLC   SBQYMST,SBQYMEND                                                 
         BH    RH4                                                              
         MVC   BKEYMBIL,SBQYMST    SKIP TO Y/M START                            
         XC    BKEYINV,BKEYINV                                                  
         B     RH1                                                              
*                                                                               
RH4      CLC   BKEYMBIL,SBQYMEND                                                
         BNH   RH6                                                              
         B     RH21                SKIP TO NEXT MONTH OF SERVICE                
*                                                                               
RH5      GOTOR AIO,IOSPTDIR+IOSQD                                               
         B     RH2                                                              
*                                                                               
RH6      CLC   SBQPRD,=C'POL'      TEST PRD=POL REQUEST                         
         BNE   RH7                 NO                                           
         CLI   SBEBYPRD,C'Y'       YES-TEST TO SPLIT OUT THE PRODUCTS           
         BE    RH7                 YES                                          
         CLC   SBPRD,=C'POL'       NO-FORCE PRODUCT=POL                         
         BE    RH10                                                             
         MVC   SBPRD,=C'POL'                                                    
         B     RH8                                                              
*                                                                               
RH7      CLC   BKEYPRD,SBPRD       TEST CHANGE OF PRODUCT                       
         BE    RH10                                                             
         MVC   SBPRD,BKEYPRD       YES-FIND ENTRY IN PRD BUFFER                 
*                                                                               
RH8      ICM   R1,15,SBAPRDBF                                                   
         BZ    RH10                                                             
         USING PRDBUFFD,R1                                                      
         LA    R0,256                                                           
         CLC   SBPRD,PBALPH                                                     
         BE    RH9                                                              
         LA    R1,PRDBUFFL(R1)                                                  
         BCT   R0,*-14                                                          
         B     RH24                PER TSMY                                     
*                                                                               
RH9      MVC   SBBPRD,PBBCODE      SET PRODUCT VALUES                           
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBPRDINT,PBINT                                                   
         MVC   SBUP1FLD,PBUFLD1                                                 
         MVC   SBUP2FLD,PBUFLD2                                                 
         OI    SBEUDEF,SBEUPCOM    TELL CALLER TO GET PRD UCOMS                 
         MVC   SBBPGR,PBGROUP                                                   
         OC    SBBPGR,SBBPGR                                                    
         BNZ   *+10                                                             
         MVC   SBBPGR,=X'9999'                                                  
         CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   RH10                                                             
         OC    PBGROUP,PBGROUP     YES-TEST PRODUCT ASSIGNED TO GROUP           
         BZ    RH24                NO-SKIP TO NEXT PRODUCT                      
         DROP  R1                                                               
*                                                                               
RH10     CLC   BKEYEST,SBQEST      NO-FILTER ON ESTIMATE                        
         BNL   RH12                                                             
         MVC   BKEYEST,SBQEST      SKIP TO REQ ESTIMATE                         
         MVC   BKEYYSRV(2),SBBQST                                               
         XC    BKEYMBIL(3),BKEYMBIL                                             
         B     RH1                                                              
*                                                                               
RH12     CLC   BKEYEST,SBQESTND    TEST BEYOND ESTIMATE END                     
         BH    RH24                YES-SKIP TO NEXT PRODUCT                     
*                                                                               
RH14     ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   RH16                YES                                          
         LLC   RE,BKEYEST                                                       
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE ESTIMATE                         
         BE    RH22                NO-SKIP TO NEXT ESTIMATE                     
         B     RH18                YES                                          
*                                                                               
RH16     LLC   RE,SBBPRD           PRODUCT                                      
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    RE,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   R0,BKEYEST          ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    RH22                NO-SKIP TO NEXT ESTIMATE                     
         ICM   R1,15,SBAESTBF      YES-GET ESTIMATE DETAILS                     
         BZ    RH18                                                             
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R1,RF                                                            
         USING ESTBUFFD,R1                                                      
         MVC   SBESTFLT,EBFILT                                                  
         MVC   SBRTLSCH,EBRTLSCH                                                
***      MVC   SBUE1FLD,EBUFLD1                                                 
***      MVC   SBUE2FLD,EBUFLD2                                                 
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
         DROP  R1                                                               
*                                                                               
RH18     LA    RE,SBBQST           NFIS SHOULD CONTINUE TO USE REQUEST          
         LA    RF,SBBQEND          DATES                                        
         CLI   ONLINE,C'Y'                                                      
         BE    *+12                                                             
         LA    RE,SBBQBLST         WRITER USES NEW BILL START/END               
         LA    RF,SBBQBLEN                                                      
         OC    0(2,RE),0(RE)        TEST REQUEST DATES                          
         BZ    RH26                                                             
         CLC   BKEYYSRV(2),0(RE)    YES-CHECK MONTH OF SERVICE                  
         BNL   RH20                                                             
         MVC   BKEYYSRV(2),0(RE)    SKIP TO REQUEST START                       
         XC    BKEYMBIL(3),BKEYMBIL                                             
         B     RH1                                                              
*                                                                               
RH20     CLC   BKEYYSRV(2),0(RF)    TEST BEYOND REQUEST END                     
         BNH   RH26                 NO-DON'T SKIP TO NEXT ESTIMATE              
         B     RH22                                                             
*                                                                               
RH21     MVC   BKEYMBIL(3),XFF     NEXT MONTH OF SERVICE                        
         B     RH1                                                              
*                                                                               
RH22     MVC   BKEYYSRV(5),XFF     NEXT ESTIMATE                                
         B     RH1                                                              
*                                                                               
RH24     MVC   BKEYEST(6),XFF      NEXT PRODUCT                                 
         B     RH1                                                              
*                                                                               
RH26     CLC   SBBEST,BKEYEST      TEST ESTIMATE CHANGE                         
         BE    RH28                                                             
         MVC   SBBEST,BKEYEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
RH28     TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    *+8                         NO                                   
         BRAS  RE,ESTUSERF                 YES - GET EST USER FIELDS            
*                                                                               
         GOTOR AIO,IOSPTFIL+IOGET+IO1   GET THE BILL RECORD                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    RH90                                                             
         L     R2,AIO1                                                          
*                                                                               
         CLI   SBQBHTYP,0          TEST BILL TYPE FILTER                        
         BE    RH29                                                             
         CLI   SBQBHTYP,SBQBHTYA   INCLUDE AOR                                  
         BNE   *+16                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   RH29                                                             
         B     RH90                                                             
         CLI   SBQBHTYP,SBQBHTYB   EXCLUDE AOR                                  
         BNE   *+16                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   RH90                                                             
         B     RH29                                                             
         CLI   SBQBHTYP,SBQBHTYC   INCLUDE SEP COMMISSION                       
         BNE   *+16                                                             
         TM    BILSTAT,BSTSCOMQ                                                 
         BO    RH29                                                             
         B     RH90                                                             
         CLI   SBQBHTYP,SBQBHTYD   EXCLUDE SEP COMMISSION                       
         BNE   *+16                                                             
         TM    BILSTAT,BSTSCOMQ                                                 
         BO    RH90                                                             
         B     RH29                                                             
         CLI   SBQBHTYP,SBQBHTYN   INCLUDE NET BILL                             
         BNE   *+16                                                             
         TM    BILSTAT,BSTSNETQ                                                 
         BO    RH29                                                             
         B     RH90                                                             
         CLI   SBQBHTYP,SBQBHTYO   EXCLUDE NET BILL                             
         BNE   *+16                                                             
         TM    BILSTAT,BSTSNETQ                                                 
         BO    RH90                                                             
         B     RH29                                                             
         CLI   SBQBHTYP,SBQBHTYE   INCLUDE RETAIL CONTROL BILLING               
         BNE   *+16                                                             
         TM    BRETAIL,X'81'                                                    
         BO    RH29                                                             
         B     RH90                                                             
         CLI   SBQBHTYP,SBQBHTYF   EXCLUDE RETAIL CONTROL BILLING               
         BNE   *+16                                                             
         TM    BRETAIL,X'81'                                                    
         BO    RH90                                                             
         B     RH29                                                             
*                                                                               
         CLI   SBQBHTYP,SBQBHTYG   INCLUDE MANUAL BILLING                       
         BNE   *+16                                                             
         TM    BILSTAT,BSTMANQ                                                  
         BO    RH29                                                             
         B     RH90                                                             
*                                                                               
         CLI   SBQBHTYP,SBQBHTYH   EXCLUDE MANUAL BILLING                       
         BNE   *+16                                                             
         TM    BILSTAT,BSTMANQ                                                  
         BO    RH90                                                             
         B     RH29                                                             
*                                                                               
         CLI   SBQBHTYP,SBQBHTYQ   INCLUDE ORDERED/EXCLUDE CLEARED?             
         BNE   *+16                                                             
         TM    BILSTAT2,BSTCLRDQ   CLEARED?                                     
         BO    RH90                YES, DO NOT WANT THIS REC                    
         B     RH29                                                             
*                                                                               
         CLI   SBQBHTYP,SBQBHTYR   INCLUDE CLEARED/EXCLUDE ORDERED?             
         BNE   RH29                                                             
         TM    BILSTAT2,BSTCLRDQ   CLEARED?                                     
         BZ    RH90                NO                                           
***                                                                             
* A SPECIFIC MARKET GROUP FILTER SHOULD ONLY ALLOW BILL HEADERS WITH            
* THAT SPECIFIC MARKET GROUP!                                                   
***                                                                             
RH29     CLC   SBQMGRF,BLANKS      SPECIFIC MARKET GROUP REQUEST?               
         BNH   RH29A               NO                                           
         CLC   SBQMGR,BLMGR        YES - HAVE A MATCH ON SCHEME/GROUP?          
         BNE   RH90                NO - READ THE NEXT RECORD                    
         B     RH29B                                                            
*                                                                               
RH29A    MVC   SBBMGR,=X'9999'     SET MARKET GROUP=UNKNOWN                     
         CLC   BLMGR,BLANKS        TEST FOR MARKET GROUP                        
         BE    RH30                2 CHAR < SPACES                              
         OC    BLMGR,BLMGR                                                      
         BZ    RH30                                                             
         CLI   SBQMGR,0            YES-TEST MARKET GROUP REQUEST                
         BE    RH30                                                             
         CLC   SBQMGRD,BLMGR       YES-TEST SCHEMES MATCH                       
         BNE   RH30                                                             
*                                                                               
RH29B    PACK  DUB,BLMGR+1(5)      YES-SET THE MARKET GROUP                     
         MVC   SBBMGR,DUB+5                                                     
*                                                                               
RH30     CLC   BLMKT,BLANKS        TEST MARKET SET                              
         BH    *+14                                                             
         XC    SBBMKT,SBBMKT                                                    
         B     RH31                                                             
         MVC   SBMKT,BLMKT         YES-                                         
         PACK  DUB,SBMKT                                                        
         CVB   RE,DUB                                                           
         CLM   RE,3,SBBMKT         TEST NEW MARKET                              
         BE    RH31                NO - WE STILL MAY NEED TO GET BFORM!         
         STCM  RE,3,SBBMKT                                                      
         B     RH31B                                                            
*                                                                               
RH31     TM    SBEFLAG5,SBE5BFRM   GET BILL FORM RECORD?                        
         BZ    RH32                NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BE    RH32                NO                                           
         OC    SBBMKT,SBBMKT       HAVE A MARKET?                               
         BZ    RH31C               NO, SKIP GETTING THE MARKET RECORD           
*                                                                               
RH31B    GOTOR AGETMKT             YES-GET THE MARKET RECORD                    
         BNE   RH32                                                             
RH31C    MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
RH32     OC    SBQBHIST,SBQBHIST   CHECK INVOICE DATE                           
         BZ    RH34                                                             
         CLC   BQDATE,IVCST                                                     
         BL    RH90                                                             
         CLC   BQDATE,IVCEN                                                     
         BH    RH90                                                             
*                                                                               
RH34     OC    SBQBHRST,SBQBHRST   CHECK RUN DATE                               
         BZ    RH36                                                             
         CLC   BDATE,RUNST                                                      
         BL    RH90                                                             
         CLC   BDATE,RUNEN                                                      
         BH    RH90                                                             
*                                                                               
RH36     OC    SBQBHDST,SBQBHDST   CHECK DUE DATE                               
         BZ    RH38                                                             
         CLC   BDUEDATE,DUEST                                                   
         BL    RH90                                                             
         CLC   BDUEDATE,DUEEN                                                   
         BH    RH90                                                             
*                                                                               
RH38     OC    SBQBHPST,SBQBHPST   CHECK POST DATE                              
         BZ    RH40                                                             
         CLC   BILPOST,SBQBHPST                                                 
         BL    RH90                                                             
         CLC   BILPOST,SBQBHPEN                                                 
         BH    RH90                                                             
*                                                                               
RH40     OC    SBQBHEST,SBQBHEST   CHECK EDI DATE                               
         BZ    RH45                                                             
         CLC   BEDIDTE,SBQBHEST                                                 
         BL    RH90                                                             
         CLC   BEDIDTE,SBQBHEEN                                                 
         BH    RH90                                                             
*                                                                               
RH45     OC    SBECDAT,SBECDAT     CREATION DATE FILTER?                        
         BZ    RH46                NO                                           
         GOTOR CDATCON,PARM,(3,SBECDAT),(0,WORK)                                
         CLC   BDATE,WORK          CREATED BEFORE FILTERING DATE?               
         BL    RH90                YES                                          
*                                                                               
RH46     MVI   SBGMBILL,0          GM BILLING FOR BILL HEADERS                  
         TM    BILSTAT3,BSTREGQ    REGIONAL?                                    
         BZ    *+8                 NO                                           
         MVI   SBGMBILL,C'R'                                                    
         TM    BILSTAT3,BSTLMGQ    LMG?                                         
         BZ    *+8                 NO                                           
         MVI   SBGMBILL,C'L'                                                    
*                                                                               
         TM    SBEFLAG6,SBE6TGMY   MTRADE=Y?                                    
         BZ    *+12                NO                                           
         TM    BILSTAT3,BSTTRDQ    YES - GROUP M TRADE?                         
         BZ    RH90                NO - NEXT BILL HEADER                        
         TM    SBEFLAG6,SBE6TGMN   MTRADE=N?                                    
         BZ    *+12                NO                                           
         TM    BILSTAT3,BSTTRDQ    YES - GROUP M TRADE?                         
         BNZ   RH90                YES - NEXT BILL HEADER                       
*                                                                               
* HOOK TO USER                                                                  
*                                                                               
RH50     MVI   SBMODE,SBPROCBH                                                  
*                                                                               
         TM    SBQREAD2,SBQRD2BH   BUFFER BILL HEADERS?                         
         BNO   *+8                                                              
         BRAS  RE,GOCSHR                                                        
*                                                                               
         TM    SBQREAD,SBQRDBH     IF READING BILL HEADERS                      
         BNO   *+12                                                             
         BRAS  RE,GO                  GO TO DRIVER                              
         JNE   NEQEXIT                                                          
*                                                                               
RH90     LA    R2,IOKEY                                                         
         GOTOR AIO,IOSPTDIR+IOHID  READ NEXT BILL RECORD                        
         GOTOR (RF),IOSPTDIR+IOSQD                                              
         B     RH2                                                              
*                                                                               
RHX      J     EQEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
EXTRA    NMOD1 0,**SIOX**,R6                                                    
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     READINF                                                          
         B     SETUSER                                                          
         B     GETPGR                                                           
         B     GETMGR                                                           
         B     GETMKT                                                           
         B     GETSGR                                                           
         B     GETSTA                                                           
         B     GETDPTTB                                                         
         B     GETBILFO                                                         
         B     CHKEST                                                           
         B     CHKMGRP                                                          
         B     CHKSTA                                                           
         B     CHKMASDP                                                         
         B     FILTDPT                                                          
         B     MSPACK                                                           
         B     MSUNPK                                                           
         B     CBLFILT                                                          
         B     GETALPH                                                          
         B     GETPWMKT                                                         
         B     GETPWSTA                                                         
         B     RDBILL                                                           
*                                                                               
         EJECT                                                                  
* READ INFOMERCIAL RECORDS                                                      
*                                                                               
READINF  DS    0H                                                               
         MVI   SBBPRD,0                                                         
         MVI   SBBPRD2,0                                                        
         MVI   SBBEST,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    SVMKTSTA,SVMKTSTA                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            INFOMERCIAL KEY                              
         USING INFORECD,R2                                                      
         MVC   INFKTYP,=X'0D79'                                                 
         MVC   INFKAGMD,SBBAGYMD                                                
         MVC   INFKCLT,SBBCLT                                                   
         LA    R4,INFKPRD-INFKEY-1                                              
         CLI   SBQBPRD,0           TEST FOR PRODUCT FILTER                      
         BE    IN2                                                              
         MVC   INFKPRD,SBQBPRD     YES                                          
         LA    R4,L'INFKPRD(R4)                                                 
*                                                                               
IN2      LA    R1,IOSPTDIR+IOHI    READ HIGH                                    
         B     IN4                                                              
*                                                                               
IN3      LA    R1,IOSPTDIR+IOSQ    READ SEQUENTIAL                              
*                                                                               
IN4      DS    0H                                                               
         GOTOR AIO                                                              
*                                                                               
IN5      MVC   SBRECDA,IODA        SAVE DISK ADDRESS                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   INX                                                              
         SR    R3,R3               TEST ANY VALID ESTS FOR THIS PRODUCT         
         ICM   R1,15,SBAESTTB                                                   
         BNZ   *+18                                                             
         OC    ESTTAB,ESTTAB                                                    
         BZ    IN32                                                             
         B     IN6                                                              
         IC    R3,INFKPRD                                                       
         BCTR  R3,0                                                             
         SLL   R3,8                                                             
         AR    R3,R1                                                            
         OC    0(256,R3),0(R3)                                                  
         BZ    IN32                NO - SKIP TO NEXT PRODUCT                    
*                                                                               
IN6      CLC   SBQPGRF,BLANKS      TEST PRODUCT GROUP FILTERING                 
         BNH   IN7                                                              
         LLC   RE,INFKPRD          YES-FIND PRDGRP                              
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         A     RE,SBAPRDBF                                                      
         USING PRDBUFFD,RE                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    IN3                                                              
*                                                                               
IN7      SR    RE,RE                                                            
         LTR   R3,R3               TEST PRODUCT/ESTIMATE TABLE PASSED           
         BNZ   IN8                 YES                                          
         IC    RE,INFKEST          NO-USE ESTIMATE TABLE                        
         LA    R3,ESTTAB(RE)                                                    
         B     IN9                                                              
*                                                                               
IN8      IC    RE,INFKEST                                                       
         BCTR  RE,0                                                             
         AR    R3,RE                                                            
*                                                                               
IN9      CLI   0(R3),0             TEST ESTIMATE IS ACTIVE                      
         BE    IN30                NO-SKIP TO NEXT ESTIMATE                     
*                                                                               
         OC    INFKMKT,INFKMKT     TEST MARKET IN KEY                           
         BZ    IN40                                                             
         CLC   INFKMKT,QBMKTST     YES-TEST MARKET WITHIN RANGE                 
         BL    *+18                                                             
         CLC   INFKMKT,QBMKTND                                                  
         BH    IN30                DONE WITH MARKET - SKIP TO NEXT EST          
         B     IN10                                                             
         MVC   INFKMKT,QBMKTST     SKIP TO MARKET START                         
         XC    INFKSTA(4),INFKSTA                                               
         B     IN2                                                              
*                                                                               
IN10     CLC   SBQMGRF,BLANKS      TEST MARKET GROUP FILTERING                  
         BNH   IN12                                                             
         MVC   SVMKT,INFKMKT       YES-CHECK FOR VALID MKTGRP                   
         MVC   SVPRD,INFKPRD                                                    
         GOTOR ACHKMGRP                                                         
         BNE   IN24                MARKET NOT IN MKTGRP - READ NEXT MKT         
*                                                                               
IN12     OC    INFKSTA,INFKSTA     TEST STATION IN KEY                          
         BZ    IN40                                                             
*                                                                               
         XC    STAFILT,STAFILT     SET TO FILTER ON CBLNET                      
         MVC   STAFILT+2(3),INFKSTA                                             
         GOTOR ACBLFILT                                                         
         BNE   IN20                SKIP TO NEXT STATION                         
*                                                                               
IN14     SR    R0,R0                                                            
         ICM   R0,7,SVSTA                                                       
         SR    RF,RF                                                            
         ICM   RF,7,INFKSTA                                                     
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    IN14A                NFC (NO CABLE)                              
         CLI   SVSTA,X'E8'         TEST CABLE                                   
         BL    IN14A                                                            
         N     R0,=X'00FFFF80'                                                  
         N     RF,=X'00FFFF80'                                                  
*                                                                               
IN14A    CR    R0,RF                                                            
         BE    IN16                                                             
         MVC   SVMKT,INFKMKT       SAVE MARKET                                  
         MVC   SVSTA,INFKSTA       YES-GET STATION DETAILS                      
         GOTOR AGETSTA                                                          
*                                                                               
IN16     GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BE    IN40                                                             
*                                                                               
IN18     OC    QBSTA,QBSTA                                                      
         BZ    IN20                                                             
         CLC   INFKMKT(2),QBMKTSTA   TEST REACHED MARKET YET                    
         BL    IN24                                                             
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    *+12                 NFC (NO CABLE)                              
         CLI   INFKSTA,X'E8'       TEST CABLE                                   
         BNL   IN20                NEXT STATION                                 
*                                                                               
         CLC   QBSTA,INFKSTA                                                    
         BE    IN30                                                             
*                                                                               
IN20     MVI   INFKSTA+3,X'FF'     SKIP TO NEXT STATION                         
         B     IN2                                                              
*                                                                               
IN22     MVC   INFKMKT(5),QBMKTSTA SKIP TO REQUESTED STATION                    
         MVI   INFKSTA+3,0                                                      
         B     IN2                                                              
*                                                                               
IN24     OC    QBMKT,QBMKT         NEXT MARKET                                  
         BZ    IN28                                                             
         CLC   INFKMKT,QBMKTST                                                  
         BNL   IN26                                                             
         MVC   INFKMKT,QBMKTST                                                  
         XC    INFKSTA(4),INFKSTA                                               
         B     IN2                                                              
*                                                                               
IN26     CLC   INFKMKT,QBMKTND                                                  
         BH    IN30                                                             
         CLC   QBMKTST,QBMKTND                                                  
         BE    IN30                                                             
*                                                                               
IN28     MVC   INFKSTA(4),XFF      SKIP TO NEXT MARKET                          
         B     IN2                                                              
*                                                                               
IN30     MVC   INFKMKT(6),XFF      NEXT ESTIMATE                                
         B     IN2                                                              
*                                                                               
IN32     CLI   INFKPRD,X'FF'       NEXT PRODUCT                                 
         BE    INX                                                              
         CLI   SBQBPRD,0                                                        
         BNE   INX                                                              
         MVC   INFKEST(7),XFF                                                   
         B     IN2                                                              
*                                                                               
*                                                                               
IN40     ICM   R5,15,SBAESTBF      GET ESTIMATE DETAILS                         
         BNZ   *+14                                                             
         MVC   SBDPTMEN,0(R3)                                                   
         B     IN42                                                             
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R5,RF                                                            
         USING ESTBUFFD,R5                                                      
         MVC   SBDPTMEN,EBDPTMEN   SET DAYPART MENU                             
         MVC   SBESTFLT,EBFILT     SET ESTIMATE FILTERS                         
         MVC   SBRTLSCH,EBRTLSCH   SET RETAIL SCHEME CODE                       
         GOTOR VRCPACK,PARM,(C'U',EBSREP),SBSREP                                
         DROP  R5                                                               
*                                                                               
IN42     CLC   SBBPRD,INFKPRD      TEST PRODUCT CHANGE                          
         BE    IN48                                                             
         MVC   SBBPRD,INFKPRD      YES                                          
         L     RF,ACLTREC          GET PRODUCT FROM CLIENT HEADER               
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
IN44     CLC   SBBPRD,3(RF)                                                     
         BE    IN46                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    IN44                                                             
         B     IN32                PRD NOT IN CLTHDR - READ NEXT PRD            
*                                                                               
IN46     MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD           SET PRODUCT GROUP                            
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    IN48                                                             
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         OC    SBBPGR,SBBPGR                                                    
         BNZ   IN48                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
IN48     CLC   SBBEST,INFKEST      TEST ESTIMATE CHANGE                         
         BE    IN50                                                             
         MVC   SBBEST,INFKEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
IN50     OC    INFKMKT,INFKMKT     TEST MARKET IN KEY                           
         BNZ   IN51                                                             
         XC    SBBMKT,SBBMKT       NO                                           
         XC    SBBSTA,SBBSTA                                                    
         MVC   SBMKT,BLANKS                                                     
         MVC   SBSTA,BLANKS                                                     
         B     IN60                                                             
*                                                                               
IN51     CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    IN52                                                             
         CLC   SBBMKT,INFKMKT      YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    IN52                                                             
         MVC   SVPRD,INFKPRD                                                    
         MVC   SVMKT,INFKMKT                                                    
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
IN52     CLC   SBBMKT,INFKMKT      TEST MARKET CHANGE                           
         BE    IN54                                                             
         MVC   SBBMKT,INFKMKT      YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   IN54                                                             
         MVI   SBMODE,SBPROCMK     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
IN54     CLC   SBBSTA,INFKSTA      TEST STATION CHANGE                          
         BE    IN56                                                             
         MVC   SBBSTA,INFKSTA      SET STATION                                  
         GOTOR AMSUNPK,PARM,(X'80',SVMKTSTA),WORK,WORK+4                        
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
IN56     CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   IN60                                                             
         CLC   SBSTYPE,SBQSTATY                                                 
         BNE   IN70                                                             
*                                                                               
IN60     DS    0H                                                               
         GOTOR AIO,IOSPTFIL+IOGET+IO1   GET THE INFOMERCIAL RECORD              
*                                                                               
         MVI   SBMODE,SBPROCIN     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
IN70     LA    R2,IOKEY            READ NEXT INFOMERCIAL RECORD                 
         GOTOR AIO,IOSPTDIR+IOHI                                                
*                                                                               
         GOTOR (RF),IOSPTDIR+IOSQ                                               
         B     IN5                                                              
*                                                                               
INX      J     EQEXIT                                                           
         EJECT                                                                  
* SUB-ROUTINE TO SET THE USER DEMO FIELDS IN THE ESTIMATE BUFFER                
* AT ENTRY, R4=A(ESTIMATE ENTRY)                                                
*           R2=A(ESTIMATE RECORD)                                               
*                                                                               
         USING ESTHDRD,R2                                                       
         USING ESTBUFFD,R4                                                      
SETUSER  DS    0H                                                               
         ICM   RF,15,SBAUSNTB      RF=A(USER DEMO NAME TABLE)                   
         BZ    SETUSERX            NONE-PASSED                                  
         LA    R0,L'EBDEMOS/3      R0=COUNTER                                   
         LA    RE,EBDEMOS          RE=A(DEMO)                                   
*                                                                               
SETUSER2 CLI   1(RE),0             TEST FOR EOT                                 
         BE    SETUSERX                                                         
         CLI   1(RE),X'21'         TEST FOR USER DEMO                           
         BE    SETUSER4                                                         
*                                                                               
SETUSER3 LA    RE,3(RE)                                                         
         BCT   R0,SETUSER2                                                      
         B     SETUSERX                                                         
*                                                                               
SETUSER4 LLC   R3,2(RE)            GET USER DEMO NUMBER                         
         BCTR  R3,0                                                             
         MHI   R3,7                                                             
         LA    R3,EUSRNMS(R3)      R3=A(USER DEMO NAME)                         
         LA    R5,8(RF)            R5=A(TABLE ENTRY)                            
         ICM   R1,15,0(RF)         GET N'ENTRIES                                
         BZ    SETUSER6            NONE-ADD FIRST ONE                           
*                                                                               
         CLC   0(7,R3),0(R5)       TEST USER NAME VS TABLE                      
         BE    SETUSER5            FOUND NAME IN TABLE                          
         LA    R5,7(R5)            NEXT TABLE ENTRY                             
         BCT   R1,*-14                                                          
         B     SETUSER6            NAME NOT IN TABLE SO ADD IT                  
*                                                                               
SETUSER5 BCTR  R1,0                                                             
         LNR   R1,R1                                                            
         A     R1,0(RF)            COMPUTE TABLE NUMBER                         
         B     SETUSER8                                                         
*                                                                               
SETUSER6 L     R1,0(RF)                                                         
         LA    R1,1(R1)            INCREMENT ENTRY COUNT                        
         C     R1,4(RF)            TEST IF PAST TABLE LIMIT                     
         BH    SETUSERX                                                         
         ST    R1,0(RF)                                                         
         MVC   0(7,R5),0(R3)       SLOT NAME IN LAST ENTRY                      
*                                                                               
SETUSER8 LLC   R3,2(RE)                                                         
         LA    R3,EBUSER1-1(R3)                                                 
         STC   R1,0(R3)                                                         
         B     SETUSER3                                                         
*                                                                               
SETUSERX J     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
* GET PRODUCT GROUP RECORD                                                      
* INPUT  : SBBPGR = PRDGRP                                                      
*                                                                               
GETPGR   MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PRGKEY,R2                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SBBAGYMD                                                
         MVC   PRGKCLT,SBBCLT                                                   
         MVC   PRGKID,SBQPGR                                                    
         MVC   PRGKGRP,SBBPGR                                                   
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,PRGEL                                                         
         SR    RE,RE                                                            
*                                                                               
GP10     CLI   0(R3),0                                                          
         BE    GPX                                                              
         CLI   0(R3),X'10'         PRDGRP BREAK NAMES                           
         BE    *+14                                                             
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     GP10                                                             
         USING PRGEL10,R3                                                       
         MVC   SBPGR1NM,PRGNAM1                                                 
         MVC   SBPGR2NM,PRGNAM2                                                 
         DROP  R3                                                               
*                                                                               
GPX      MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK                                   
         GOTOR AIO,IOSPTDIR+IOHI                                                
         J     EXIT                                                             
         EJECT                                                                  
* GET MARKET GROUP RECORD                                                       
* INPUT  : IOKEY = FIRST PASSIVE KEY FOR THE MARKET GROUP                       
*                                                                               
GETMGR   MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY                                   
         LA    R2,IOKEY            READ MARKET GROUP DEFINITION RECORD          
         USING MKGRECD,R2                                                       
         MVC   MKGKTYP,=X'0D02'                                                 
         XC    MKGPMKT,MKGPMKT                                                  
         GOTOR AIO,IOSPTFIL+IORD+IO1                                            
         BE    GM05                                                             
         MVC   SBMGR1NM,=C'*UNKNOWN*'                                           
         MVC   SBMGR2NM,=C'*UNKNOWN*'                                           
         MVC   SBMGR3NM,=C'*UNKNOWN*'                                           
         B     GMX                                                              
*                                                                               
GM05     L     R2,AIO1                                                          
         LA    R3,MKGEL                                                         
         SR    RE,RE                                                            
*                                                                               
GM10     CLI   0(R3),0                                                          
         BE    GMX                                                              
         CLI   0(R3),X'10'                                                      
         BE    *+14                                                             
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     GM10                                                             
         USING MKGEL10,R3                                                       
         MVC   SBMGR1NM,MKGNAM1                                                 
         MVC   SBMGR2NM,MKGNAM2                                                 
         MVC   SBMGR3NM,MKGNAM3                                                 
         DROP  R3                                                               
*                                                                               
GMX      MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK                                   
         GOTOR AIO,IOSPTDIR+IOHI                                                
         J     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO GET THE MARKET RECORD                                          
* RETURN - CC EQ - OK                                                           
*             NE - MARKET=0                                                     
*                                                                               
GETMKT   OC    SBBMKT,SBBMKT                                                    
         JZ    NEQEXIT                                                          
         OI    SBEUDEF,SBEUMCOM    TELL CALLER TO EXTRACT MKT UCOM              
         XC    SBMKTREC,SBMKTREC                                                
         MVC   SBMKTNM,BLANKS                                                   
         MVC   SBMKTNM(9),=C'*UNKNOWN*'                                         
         XC    SBMKTWGT,SBMKTWGT   DEFAULT WEIGHT IS 0 OR 1                     
         TM    SBQDEMOP,SBQDOMWZ                                                
         BO    *+10                                                             
         MVC   SBMKTWGT,=F'1'                                                   
         TM    SBQSKIP,SBQSKMKT    TEST SKIP READING MARKET RECORDS             
         JO    NEQEXIT                                                          
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY    SAVE KEY AND KEYSAVE           
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING MKTKEY,RE                                                        
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SBQMED                                                   
         CLI   SBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   MKTKMED,SBMED                                                    
         MVC   MKTKMKT,SBMKT                                                    
         TM    SBIOFLAG,SBXFILE    READ ACROSS FILES?                           
         BZ    MK1                  NO                                          
         ZICM  RF,SBBSTMKT,2                                                    
         CVD   RF,DUB               YES - USE STATION'S MKT                     
         OI    DUB+7,X'0F'                                                      
         UNPK MKTKMKT,DUB                                                       
MK1      MVC   MKTKAGY,SBAGY                                                    
         OC    SBSTAAGY,SBSTAAGY   USE ALT STA FILE?                            
         BZ    *+10                                                             
         MVC   MKTKAGY,SBSTAAGY                                                 
         LA    R1,SBMKTREC         TEST MARKET RECORD ALREADY IN CORE           
         CLC   MKTKEY,0(R1)                                                     
         BE    MK2                                                              
         GOTOR AIO,IORD+IOSTAFIL+IO2                                            
         BNE   MKX                                                              
         L     RE,AIO2                                                          
         MVC   SBMKTREC(L'SBMKTREC),MKTREC  MOVE MARKET TO BLOCK                
*                                                                               
MK2      LA    RE,SBMKTREC                                                      
         MVC   SBMKTNM,MKTNAME                                                  
         MVC   SBBRSM0,MKTRSM1     NSI MARKET NUM                               
         MVC   SBBRSM1,MKTRSM2     ARB MARKET NUM                               
         CLC   MKTWT,BLANKS        CHECK FOR ZERO WEIGHT                        
         BNH   MKX                                                              
         PACK  DUB,MKTWT                                                        
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    MKX                                                              
         STCM  R0,15,SBMKTWGT      SET MARKET WEIGHT                            
*                                                                               
MKX      DS    0H                                                               
         MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         GOTOR AGETALPH                                                         
         J     EQEXIT                                                           
         DROP  RE                                                               
         EJECT                                                                  
* READ STATION GROUP RECORDS                                                    
*                                                                               
GETSGR   DS    0H                                                               
         CLI   SBQSGRD,C'*'        SPECIAL CODE FOR EIX STATION GROUP           
         BNE   GETSGR1                                                          
         MVC   SBSGR1BK,ESGR1BK                                                 
         MVC   SBSGR2BK,ESGR2BK                                                 
         MVC   SBSGR1LN,ESGR1LN                                                 
         MVC   SBSGR2LN,ESGR2LN                                                 
         MVC   SBBSGR,EBSGR                                                     
         MVC   SBSGR1NM,ESGR1NM                                                 
         MVC   SBSGR2NM,ESGR2NM                                                 
         MVI   SBMODE,SBPROCSG                                                  
         BRAS  RE,GO                                                            
         BNE   GETSGRN                                                          
         B     GETSGR3                                                          
*                                                                               
GETSGR1  XC    IOKEY,IOKEY         READ STATION GROUP ID RECORD                 
         LA    R2,IOKEY                                                         
         USING GRPRECD,R2                                                       
         XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKAGMD,SBBAGYMD                                                
         MVC   GRPKID,SBQSGRD                                                   
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   GETSGRY                                                          
         GOTOR (RF),IOSPTFIL+IOGET+IO1                                          
         L     R2,IOADDR                                                        
         LA    R3,GRPEL                                                         
         SR    RF,RF               GET BREAK DESCRIPTION                        
*                                                                               
GETSGR2  CLI   0(R3),0                                                          
         BE    GETSGRN                                                          
         CLI   0(R3),GRPBRKCQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETSGR2                                                          
         USING GRPBRKD,R3                                                       
         MVC   SBSGR1BK,GRPBK1     BREAK NAMES                                  
         MVC   SBSGR2BK,GRPBK2                                                  
         LLC   R1,GRPBK1LN         BREAK LENGTHS                                
         STC   R1,SBSGR1LN                                                      
         LLC   RF,GRPBK2LN                                                      
         AR    RF,R1                                                            
         STC   RF,SBSGR2LN                                                      
*                                                                               
GETSGR3  CLC   SBSGR1LN,SBSGR2LN   TEST 2 LEVELS                                
         BE    *+12                                                             
         BRAS  RE,SETMSK           YES-SET LEVEL 1 MASK                         
         STCM  R1,3,SBSG1MSK                                                    
*                                                                               
         XC    QSGRLEN,QSGRLEN                                                  
         CLC   SBQSGRF,BLANKS      TEST STATION GROUP FILTER                    
         BNH   GETSGR6                                                          
         LA    R1,SBQSGRF+3        YES-WORK OUT NUMBER OF DIGITS                
         LA    RF,4                    FOR FILTERING                            
*                                                                               
GETSGR4  CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,GETSGR4                                                       
         ST    RF,QSGRLEN                                                       
*                                                                               
GETSGR6  CLI   SBQSGRD,C'*'                                                     
         BE    GETSGRY                                                          
         GOTOR AIO,IOSPTDIR+IOSQ   READ GROUP CODE RECORDS                      
         LA    R2,IOKEY                                                         
         CLC   IOKEY(GRPKCODE-GRPKEY),IOKEYSAV                                  
         BNE   GETSGRY                                                          
         CLC   IOKEY(GRPKELCD-GRPKEY),IOKEYSAV TEST SAME CODE AS BEFORE         
         BE    GETSGR6                                                          
         ICM   RF,15,QSGRLEN       TEST FILTERING                               
         BZ    GETSGR8                                                          
         UNPK  DUB,GRPKCODE(3)     YES-                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SBQSGRF(0),DUB+3                                                 
         BNE   GETSGR6             FILTERED OUT-NEXT NEXT GROUP                 
*                                                                               
GETSGR8  MVC   SBBSGR,GRPKCODE                                                  
         GOTOR AIO,IOSPTFIL+IOGET+IO1  READ THE RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         LA    R3,GRPEL                                                         
         SR    RF,RF               LOOK FOR GROUP CODE DESCRIPTION              
         MVC   SBSGR1NM,BLANKS                                                  
         MVC   SBSGR2NM,BLANKS                                                  
*                                                                               
GETSGR10 CLI   0(R3),0                                                          
         BE    GETSGR6                                                          
         CLI   0(R3),GRPGRPCQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETSGR10                                                         
         USING GRPGRPD,R3                                                       
         MVC   SBSGR1NM,GRPGNAM1   GROUP NAMES                                  
         MVC   SBSGR2NM,GRPGNAM2                                                
         MVI   SBMODE,SBPROCSG     STATION GROUP FIRST                          
         BRAS  RE,GO               HOOK TO USER                                 
         BNE   GETSGRN                                                          
         B     GETSGR6                                                          
*                                                                               
GETSGRY  J     EQEXIT                                                           
*                                                                               
GETSGRN  J     NEQEXIT                                                          
         SPACE 2                                                                
SETMSK   LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,GRPMASKS-2                                                    
         LA    RF,2(RF)                                                         
         BCT   R1,*-4                                                           
         LH    R1,0(RF)                                                         
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET STATION DETAILS                                            
* INPUT  : SVSTA=PACKED STATION                                                 
* OUTPUT : SBAFFIL=STATION'S AFFILIATE                                          
*          SBBSGR=STATION'S GROUP                                               
*          CC EQ - OK                                                           
*          CC NE - STATION NOT FOUND                                            
*                                                                               
GETSTA   XC    STAMKALF,STAMKALF                                                
         TM    SBIOFLAG,SBXFILE    CROSS FILE REPORTING?                        
         BNZ   GETSTA1              YES - ALWAYS READ TO GET STA'S MKT          
*                                                                               
         CLI   SBQMED,C'R'         RADIO?                                       
         BNE   *+12                                                             
         CLI   SBEDEM,C'Y'         AND DEMOS?                                   
         BE    GETSTA1              YES - NEED STATION DETAILS                  
*                                                                               
         TM    SBEFLAG3,SBE3MS     WANT TO EXTRACT MSTREET DATA?                
         BNZ   GETSTA1             YES                                          
*                                                                               
         TM    SBEFLAG4,SBE4GSTA   NEED FULL STATION DETAILS?                   
         BNZ   GETSTA1              YES                                         
*                                                                               
         TM    SBEFLAG8,SBE8CP1+SBE8CP2 COMSCORE REQUEST?                       
         BNZ   GETSTA1             YES - NEED PARENT+ FLAG                      
*                                                                               
         CLI   SBQAFFIL,C' '       TEST STATION DETAILS NEEDED                  
         BH    *+12                                                             
         CLI   SBQSGRD,C' '                                                     
         BNH   GETSTAX                                                          
*                                                                               
GETSTA1  MVC   THISSTA,SVSTA       PASS THIS STATION TO BINSRCH                 
         CLI   CANADA,C'Y'         CANADA?                                      
         BE    GETSTA1A            YES, NFC(ABLE) FOR CANADA                    
         CLI   THISSTA,X'E8'       CABLE?                                       
         BL    GETSTA1A            NO                                           
         NI    THISSTA+2,X'80'     STRIP NETWORK BITS                           
*                                                                               
GETSTA1A XR    R4,R4                                                            
         ICM   R4,7,SBASTANT       HAVE A(STATION BUFFER ENTRY)?                
         BZ    GETSTA2             NO                                           
*                                                                               
         SHI   R4,TSARKEYL+2                                                    
         XC    0(TSARKEYL+2,R4),0(R4)                                           
         MVI   2(R4),NPSTACDQ      REC TYPE (17)                                
         MVC   3(3,R4),THISSTA     STATION                                      
         BRAS  RE,TSARHIGH                                                      
         BNE   GETSTA2                                                          
*                                                                               
         ICM   R4,7,SBASTANT                                                    
         USING STABUFFD,R4                                                      
         MVC   SBAFFIL,STBAFFIL    EXTRACT THE AFFILIATE                        
         MVC   SBBSGR,STBGRP       AND GROUP                                    
         MVC   SBBSTMKT,STBMKT     AND MARKET                                   
         CLI   SBQSGRD,C' '        TEST STATION GROUPS NEEDED                   
         BNH   GETSTAX                                                          
         OC    SBBSGR,SBBSGR       AND GROUP NOT SET YET                        
         BNZ   GETSTAX                                                          
         BRAS  RE,GETSGRP          YES-GET THE GROUP NOW                        
         MVC   STBGRP,SBBSGR       AND MOVE IT INTO THE TABLE                   
         OC    STBGRP,STBGRP       TEST STILL ZERO                              
         BNZ   GETSTAX                                                          
         MVC   STBGRP,=X'9999'     YES-SET TO 'UNKNOWN'                         
         MVC   SBBSGR,=X'9999'                                                  
         B     GETSTAX                                                          
*                                                                               
GETSTA2  GOTOR GETSTREC            GET THE STATION RECORD                       
         JNE   NEQEXIT                                                          
*                                                                               
         XC    SBBSGR,SBBSGR                                                    
         CLI   SBQSGRD,C' '        TEST NEED STATION GROUPS                     
         BNH   *+8                                                              
         BRAS  RE,GETSGRP          YES-GET STATION'S GROUP CODE                 
         L     R2,AIO2                                                          
         USING STARECD,R2                                                       
         MVC   STAMKALF,SMKTALPH   IN CASE THIS IS NOT THE WRITER               
         ICM   R4,7,SBASTANT       HAVE A(STATION BUFFER ENTRY)?                
         BZ    GETSTAX             NO, EXIT                                     
*                                                                               
         CLC   STAKLEN,=Y(STACRLNQ) REC BIG ENOUGH TO HAVE PARENT+?             
         BNH   GSTA2AA0            NO                                           
         CLI   SPARPLUS,C'Y'       IS THIS STATION PARENT+?                     
         BNE   GSTA2AA0            NO                                           
         MVI   HALF,SPARENTP       ELEMENT CODE                                 
         MVI   HALF+1,S10-2        FIXED ELEMENT LENGTH                         
         LA    R5,SPARPLUS         POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
GSTA2AA0 MVC   STBTYPE,STYPE       STATION TYPE                                 
         MVC   STBAFFIL,SNETWRK    NETWORK AFFILIATION                          
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   *+16                NO                                           
         MVC   STBAFFIL,SCANNTWK   YES - THIS IS THE CANADIAN AFFILIATE         
         MVC   STBTYPE,SCANNTWK+3        FINAL BYTE IN (UNUSED) 'TYPE'          
         MVC   STBCHAN,SCHNL       STATION CHANNEL                              
         MVC   STBREP,SCONREP      CONTRACT REP                                 
         MVC   STBPREP,SPAYREP     PAYING REP                                   
         MVC   STBGRP,SBBSGR       STATION GROUP CODE                           
         OC    STBGRP,STBGRP       TEST ZERO                                    
         BNZ   *+10                                                             
         MVC   STBGRP,=X'9999'     YES-SET TO UNKNOWN GROUP                     
         MVC   STBFORM,SFORMAT     FORMAT                                       
         MVC   STBFAX,SFAX         FAX NUMBER                                   
         MVC   STBMALPH,SMKTALPH   AND MARKET ALPHA                             
*                                                                               
         MVC   SBAFFIL,SNETWRK     SET THE AFFILIATE                            
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   *+10                NO                                           
         MVC   SBAFFIL,SCANNTWK    YES - THIS IS THE CANADIAN AFFILIATE         
         MVC   SBBSGR,STBGRP       AND STATION GROUP                            
*                                                                               
         PACK  DUB,SMKT            EXTRACT THE MARKET                           
         CVB   RE,DUB                                                           
         STCM  RE,3,STBMKT                                                      
         STCM  RE,3,SBBSTMKT                                                    
*                                                                               
         MVC   STBNTI,SNTISTA      SET NTI STATION CODE                         
         MVC   STBCALL1,SRS1CALL   RTG SVC 1 CALL LETTERS - NSI(0)              
         MVC   STBCALL2,SRS2CALL   RTG SVC 2 CALL LETTERS - BBM/ARB(1)          
*                                                                               
         TM    SBEFLAG3,SBE3MS     WANT TO EXTRACT MSTREET DATA?                
         BZ    GETSTA09            NOPE                                         
***                                                                             
* EXTRACT MSTREET DATA                                                          
***                                                                             
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY  SAVE KEY AND KEYSAVE             
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING CT99KEY,R3                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSRA   READ RADIO FOR FREQ, STATE, & PARENT         
         MVC   CT99KSTA,STAKCALL   NOW READING THESE RECORD BY STATION          
         LR    R1,R3               SETSRC HAS USING CT99KEY,R1                  
         BRAS  RE,SETSRC           SET THE SOURCE                               
*                                                                               
GSTA2AA  GOTOR AIO,IOCTFILE+IOHI+IO3                                            
*                                                                               
         L     R1,IOADDR                                                        
         CLC   IOKEY(L'CT99KEY),0(R1)                                           
         BE    GSTA2AB                                                          
         CLI   CT99KSTA+4,C'T'     LOOKING FOR MEDIA T STATION?                 
         BNE   GETSTA09            NO, NO MATCH                                 
         CLI   CT99KSTA+4,C'L'     JUST LOOKED FOR BAND "L"?                    
         BE    GETSTA09            YES - NO MATCH                               
         MVI   CT99KSTA+4,C'L'     YES - LOOK FOR BAND "L"                      
         B     GSTA2AA             AND READ AGAIN WITH BAND "L"                 
         DROP  R3                                                               
*                                                                               
GSTA2AB  LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   GETSTA09            YES, LET E'M COMPLAIN ABOUT IT               
         USING CRCLD,R1                                                         
         MVC   MSFREQ,CRCLFRQ      FREQUENCY                                    
         MVC   MSSTATE,CRCLSTE     MSTREET STATE                                
         MVC   MSPARENT,CRCLPRNT   PARENT CODE                                  
         MVC   MSUNIQID,CRCLUIDX   UNIQUE ID                                    
         DROP  R1                                                               
*                                                                               
         LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),X'02'         DO WE HAVE AN FM ELEMENT?                    
         BNE   GETSTA09            NOPE                                         
*                                                                               
         USING CRFMD,R1                                                         
         MVC   MSFORMAT,CRFMFMT    SAVE FORMAT (TO READ FORMAT REC)             
         MVC   MSOWNER,CRFMOWN     SAVE OWNER (TO READ OWNER RECORD)            
         MVI   MSMOWNER,0          MSTREET MINORITY OWNER                       
         MVI   MSMOWNF,0           MINORITY OWNERSHIP IS FEMALE                 
         MVI   MSFCCM,0            QUALIFIED FCC MINORITY                       
         MVI   MSFCCMF,0           FCC MINORITY OWNERSHIP IS FEMALE             
         CLI   CRFMLN,CRFMLNQ2     HAVE NEW ELEMENT LENGTH?                     
         BL    GSTA2AC             NO, NO MINORITY OWNER                        
         MVC   MSMOWNER,CRFMMIN    MSTREET MINORITY OWNER                       
         MVC   MSMOWNF,CRFMMINF    MINORITY OWNERSHIP IS FEMALE                 
         MVC   MSFCCM,CRFMQFCC     QUALIFIED FCC MINORITY                       
         MVC   MSFCCMF,CRFMQFCF    FCC MINORITY OWNERSHIP IS FEMALE             
*                                                                               
GSTA2AC  MVI   HALF,S07ELEM        ELEMENT CODE                                 
         MVI   HALF+1,S7-2         FIXED ELEMENT LENGTH                         
         LA    R5,MSFREQ           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
*                                                                               
GETSTA2A LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BE    GETSTA2D            YES                                          
         CLI   0(R1),X'08'         HAVE X'08' ELEMENT?                          
         BNE   GETSTA2A            NO, KEEP LOOKING                             
*                                                                               
         USING CMADRMD,R1                                                       
         CLC   CMADAD1,BLANKS      HAVE ADDRESS LINE 1?                         
         BNH   GETSTA2A            NO, DON'T BUFFER ANY OF IT                   
         LA    R5,WORK+80                                                       
*                                                                               
         MVI   HALF,STADD1         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD1         ELEMENT CODE                                 
         MVC   0(L'CMADAD1,R5),CMADAD1                                          
         MVI   HALF+1,S8-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
         CLC   CMADAD2,BLANKS      HAVE ADDRESS LINE 2?                         
         BNH   GETSTA2B            NO                                           
         MVI   HALF,STADD2         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD2         ELEMENT CODE                                 
         MVC   0(L'CMADAD2,R5),CMADAD2                                          
         MVI   HALF+1,SA-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
GETSTA2B CLC   CMADCITY,BLANKS     HAVE CITY?                                   
         BNH   GETSTA2C            NO                                           
         MVI   HALF,STCITY         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMCITY         ELEMENT CODE                                 
         MVC   0(L'CMADCITY,R5),CMADCITY                                        
         MVI   HALF+1,SC-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
GETSTA2C MVI   HALF,STADD3         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD3         ELEMENT CODE                                 
         MVC   0(L'CMADSTAT,R5),CMADSTAT                                        
         MVC   2(L'CMADZIP,R5),CMADZIP                                          
         MVC   12(L'CMADCTRY,R5),CMADCTRY                                       
         CLC   0(15,R5),BLANKS     HAVE ANY DATA?                               
         BNH   GETSTA2A            NO, DON'T BUFFER IT                          
         MVI   HALF+1,SE-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         B     GETSTA2A            GET THE NEXT ELEMENT                         
         DROP  R1                                                               
***                                                                             
* READ OWNER RECORD                                                             
**                                                                              
GETSTA2D XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         USING CT99KEY,R1                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSOW   READ OWNER RECORD                            
         MVC   CT99KOME,SBMED      MEDIA                                        
         MVC   CT99KOWN,MSOWNER    5 CHAR OWNER CODE                            
         BRAS  RE,SETSRC           SET THE SOURCE                               
         GOTOR AIO,IOCTFILE+IOHI+IO3                                            
         L     R1,IOADDR                                                        
         CLC   IOKEY(L'CT99KEY),0(R1)                                           
         BNE   GETSTA03                                                         
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         USING CONAMD,R1                                                        
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   GETSTA03            YES, LET E'M COMPLAIN ABOUT IT               
         CLC   CONAME(S6-2),BLANKS HAVE ANY DATA?                               
         BNH   GETSTA03            NO, DON'T BUFFER IT                          
         MVI   HALF,SOWNR          ELEMENT CODE                                 
         MVI   HALF+1,S6-2         MAX ELEMENT LENGTH                           
         LA    R5,CONAME           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
***                                                                             
* READ FORMAT RECORD                                                            
***                                                                             
GETSTA03 XC    IOKEY,IOKEY                                                      
         USING CT99KEY,R1                                                       
         LA    R1,IOKEY                                                         
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSFM   READ FORMAT RECORD                           
         MVC   CT99KFME,SBMED      MEDIA                                        
         MVC   CT99KFRM,MSFORMAT   3 CHAR FORMAT CODE                           
         BRAS  RE,SETSRC           SET THE SOURCE                               
         GOTOR AIO,IOCTFILE+IOHI+IO3                                            
         L     R1,IOADDR                                                        
         CLC   IOKEY(L'CT99KEY),0(R1)                                           
         BNE   GETSTA04                                                         
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   GETSTA04            YES, LET E'M COMPLAIN ABOUT IT               
         USING CFNAMD,R1                                                        
         CLC   CFNAME(S4-2),BLANKS HAVE ANY DATA?                               
         BNH   GETSTA04            NO, DON'T BUFFER IT                          
         MVI   HALF,SFRMAT         ELEMENT CODE                                 
         MVI   HALF+1,S4-2         MAX ELEMENT LENGTH                           
         LA    R5,CFNAME           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
***                                                                             
* READ MULTIMEDIA RECORD                                                        
***                                                                             
GETSTA04 XC    IOKEY,IOKEY                                                      
         USING CT99KEY,R1                                                       
         LA    R1,IOKEY                                                         
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSMM   READ MULTIMEDIA RECORD                       
         MVC   CT99KMMD,MSPARENT   5 CHAR PARENT CODE                           
         BRAS  RE,SETSRC           SET THE SOURCE                               
         GOTOR AIO,IOCTFILE+IOHI+IO3                                            
         L     R1,IOADDR                                                        
         CLC   IOKEY(L'CT99KEY),0(R1)                                           
         BNE   GETSTA09                                                         
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   GETSTA09            YES, LET E'M COMPLAIN ABOUT IT               
         USING CMMNAMD,R1                                                       
         CLC   CMMNAME(S5-2),BLANKS    HAVE ANY DATA?                           
         BNH   GETSTA09            NO, DON'T BUFFER IT                          
         MVI   HALF,SPRENT         ELEMENT CODE                                 
         MVI   HALF+1,S5-2         MAX ELEMENT LENGTH                           
         LA    R5,CMMNAME          POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
*                                                                               
GETSTA09 MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   GETSTA9A            NO                                           
         MVC   STBGST,SGSTCODE     GST                                          
         MVC   STBPST,SPST         PST                                          
         MVC   STBNTYPE,SNETTYPE   NETWORK TYPE                                 
         B     GETSTA10            NO CABLE FOR CANADA                          
*                                                                               
GETSTA9A CLC   STAKLEN,=Y(STANCLNQ) GROUP CD & ORD DEADLINE PRESENT?            
         BL    GETSTA10                                                         
         MVC   STBGRPCD,SGRPCD     CABLE GROUP CODE                             
*                                                                               
GETSTA10 CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    GETSTA20             NO                                          
         CLC   STAKCLT,=C'000'     ELSE GET CBL NAME FROM NON-CLT REC           
         BE    GETSTA15                                                         
         MVC   THREE,SBCLT         I SWEAR I DIDN'T ADD THIS LABEL!             
         MVC   SBCLT,=C'000'       FORCE NON-CLT SPECIFIC REC                   
         GOTOR GETSTREC            GET THE STATION RECORD                       
         MVC   SBCLT,THREE         RESTORE CLT                                  
*                                                                               
GETSTA15 CLC   SSYSNAME(S1-2),BLANKS    HAVE ANY DATA?                          
         BNH   GETSTA20            NO, DON'T BUFFER IT                          
         MVI   HALF,SNAME          ELEMENT CODE                                 
         MVI   HALF+1,S1-2         MAX ELEMENT LENGTH                           
         LA    R5,SSYSNAME         POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
GETSTA20 LA    R2,STABUFFL+1+TSARKEYL+2                                         
         LA    R1,STBELEM          START OF ELEMENTS IN STA BUFFER              
         XR    RF,RF                                                            
*                                                                               
GETSTA25 CLI   0(R1),0             END OF STATION BUFFER?                       
         BE    GETSTA30            YES                                          
         IC    RF,1(R1)            LENGTH OF ELEMENT                            
         AR    R2,RF               ADD ELEMENT LENGTH TO BUFFER LENGTH          
         AR    R1,RF               BUMP PAST THIS ELEMENT                       
         B     GETSTA25            AND CHECK FOR END OF STATION BUFFER          
         DROP  R4                                                               
*                                                                               
GETSTA30 SHI   R4,TSARKEYL+2                                                    
         XC    0(TSARKEYL+2,R4),0(R4)                                           
         STCM  R2,3,0(R4)          LENGTH OF STATION BUFFER ENTRY               
         MVI   2(R4),NPSTACDQ      REC TYPE (17)                                
         MVC   3(3,R4),THISSTA     STATION                                      
         BRAS  RE,TSARADD                                                       
*                                                                               
GETSTAX  DS    0H                                                               
         GOTOR AGETALPH                                                         
         J     EQEXIT                                                           
*                                                                               
                                                                                
         USING CT99KEY,R1                                                       
SETSRC   MVI   CT99KSRC,C'S'       DEFAULT TO MSTREET                           
         TM    SBEFLAG6,SBE6MF     AGENCY USES MEDIA FRAMEWORKS?                
         BZ    *+8                                                              
         MVI   CT99KSRC,C'F'       MEDIA FRAMEWORKS                             
         TM    SBEFLAG7,SBE7SRDS   AGENCY USES SRDS?                            
         BZ    *+8                 NO                                           
         MVI   CT99KSRC,C'R'       SRDS                                         
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
* SUB-ROUTINE TO READ STATION'S STATION GROUP PASSIVE POINTER                   
* INPUT  : SVSTA=PACKED STATION                                                 
* OUTPUT : SBBSGR=STATION'S GROUP CODE (NULL=NO GROUP)                          
*                                                                               
GETSGRP  NTR1                                                                   
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY    SAVE KEY AND KEYSAVE           
         XC    SBBSGR,SBBSGR                                                    
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING GRPPKEY,R2                                                       
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVI   GRPPSTYP,GRPPSTYQ                                                
         MVC   GRPPAGMD,SBBAGYMD                                                
         MVC   GRPPVAL,BLANKS                                                   
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SVSTA                                                   
         GOTOR AMSUNPK,PARM,DUB,FULL,GRPPVAL                                    
         MVC   DUB(5),GRPPVAL      SAVE FOR BELOW                               
*                                                                               
         CLI   CNETWK,C'Y'         CANADIAN NETWORK REQUEST?                    
         BNE   GETSGRP1            NO                                           
***                                                                             
* IF SVSTA+2 >= B0, IT IS CANADIAN CABLE                                        
***                                                                             
         CLI   SVSTA+2,X'B0'       CANADIAN CABLE?                              
         BL    GETSGRP1            NO                                           
         MVI   GRPPVAL+4,C'N'                                                   
*                                                                               
GETSGRP1 CLI   GRPPVAL,C'0'        TEST CABLE                                   
         BNL   *+16                PASSIVE GRP KEYS HAVE NO BAND 4 CBL          
         CLI   GRPPVAL+4,C' '                                                   
         BH    *+8                                                              
         MVI   GRPPVAL+4,C'T'                                                   
         MVC   GRPPID,SBQSGRD                                                   
         CLI   SBQSGRD,C'*'        SPECIAL CODE FOR EIX STATION GROUP           
         BNE   GETSGRP2                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         USING CT3REC,R2                                                        
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVI   CT3ESUB,CT3EASIQ                                                 
         MVC   CT3EMED,SBQMED      MEDIA                                        
         MVC   CT3ESTAT,DUB        STATION                                      
         GOTOR AIO,IOCTFILE+IOHI+IO3                                            
         L     R1,IOADDR                                                        
         CLC   IOKEYSAV(L'CT3KEY-1),0(R1)   SKIP BAND                           
         BNE   GETSGRPX                                                         
         MVC   SBBSGR,EBSGR                                                     
         B     GETSGRPX                                                         
*                                                                               
         USING GRPPKEY,R2                                                       
GETSGRP2 DS    0H                                                               
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(GRPPCODE-GRPPKEY),IOKEYSAV                                 
         BNE   *+14                                                             
         MVC   SBBSGR,GRPPCODE                                                  
         B     GETSGRPX                                                         
         CLI   SBQMED,C'C'         NOT FOUND - TEST MEDIA=C                     
         BNE   GETSGRPX                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         CLI   GRPPVAL+4,C'C'      YES-TRY STATION'S MEDIA=C                    
         BE    GETSGRPX                                                         
         MVI   GRPPVAL+4,C'C'                                                   
         B     GETSGRP2                                                         
*                                                                               
GETSGRPX MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         J     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
         EJECT                                                                  
* GET DAYPART TABLE                                                             
* INPUT  : SBDPTMEN = DAYPART MENU                                              
* OUTPUT : FULL = A(DAYPART TABLE)                                              
*                                                                               
LGETDPTB NTR1                      LOCAL ENTRY POINT                            
*                                                                               
GETDPTTB DS    0H                                                               
         LA    R1,ALPHATAB                                                      
         SR    RE,RE                                                            
*                                                                               
GD10     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RE,25                                                            
         B     GD20                                                             
         CLC   SBDPTMEN,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,GD10                                                          
         LPR   RE,RE                                                            
*                                                                               
GD20     MHI   RE,180                                                           
         L     R1,SBADPTTB                                                      
         LA    R1,0(RE,R1)                                                      
         ST    R1,FULL                                                          
*                                                                               
GDX      J     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
* GET DEFAULT BILL FORMULA                                                      
* INPUT  : R2 = A(ESTIMATE RECORD)                                              
*          R4 = A(ESTIMATE BUFFER ENTRY)                                        
*                                                                               
         USING ESTHDRD,R2                                                       
         USING ESTBUFFD,R4                                                      
GETBILFO DS    0H                                                               
         LLC   R3,EKEYEST          INDEX TO ESTIMATE ENTRY IN                   
         BCTR  R3,0                BILL FORMULA TABLE                           
         MHI   R3,6                                                             
         LA    R3,BILFOTAB(R3)                                                  
         CLI   0(R3),0             TEST BILL FORMULA FOUND YET                  
         BNE   GB20                YES                                          
         MVC   0(1,R3),EKEYEST     NO                                           
         MVC   1(5,R3),SBBILFOR    DEFAULT BILL FORMULA                         
         MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY         READ PRODUCT AAA ESTIMATE HEADER             
         L     RF,ACLTREC                                                       
         MVC   IOKEY(4),0(RF)      A-M/CLT                                      
         MVC   IOKEY+4(3),=C'AAA'  PRD=AAA                                      
         MVC   IOKEY+7(1),EKEYEST  EST                                          
         GOTOR AIO,IOSPTFIL+IORD+IO2                                            
         BNE   GB10                                                             
         L     R1,AIO2             RECORD FOUND                                 
         LA    R1,EBILLBAS-EKEY(R1)                                             
         OC    0(5,R1),0(R1)       TEST FOR BILL FORMULA                        
         BZ    GB10                                                             
         MVC   1(5,R3),0(R1)       YES                                          
*                                                                               
GB10     MVC   IOKEY,SVIOKEY                                                    
*                                                                               
GB20     MVC   EBBILFOR,1(R3)      SET BILL FORMULA IN ESTIMATE BUFFER          
*                                                                               
GBX      J     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
* CHECK ESTIMATE FOR VALIDITY                                                   
* OUTPUT : CC EQ - ESTIMATE IS OK                                               
*          CC NE - ESTIMATE NOT OK AND KEY SET FOR READ HIGH                    
*                                                                               
CHKEST   DS    0H                                                               
         TM    IOKEY+13,X'80'      TEST DELETED                                 
         BO    CHKEST14            YES-SKIP TO NEXT ESTIMATE                    
         LLC   RE,IOKEY+7          SHOW INVALID IN ESTIMATE TABLE               
         LA    RE,ESTTAB(RE)                                                    
         MVI   0(RE),0                                                          
         CLC   SBQEST,IOKEY+7      CHECK ITS WITHIN REQ EST RANGE               
         BL    CHKEST2                                                          
         BE    CHKEST4                                                          
         MVC   IOKEY+7(1),SBQEST   SKIP TO FIRST EST                            
         XC    IOKEY+8(5),IOKEY+8                                               
         B     CHKESTNE                                                         
*                                                                               
CHKEST2  CLC   SBQESTND,IOKEY+7                                                 
         BL    CHKEST16            SKIP TO NEXT PRODUCT                         
*                                                                               
CHKEST4  GOTOR AIO,IOSPTFIL+IOGET+IO1   READ ESTIMATE RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING ESTHDRD,R2                                                       
*                                                                               
         OC    SBQSTART,SBQSTART   TEST FOR REQUEST DATES                       
         BZ    CHKEST6             NO                                           
         TM    SBQRDOPT,SBQROEST   YES-TEST READ ALL ESTIMATES ANYWAY           
         BO    CHKEST6                                                          
         CLC   EEND,SBQSTART       EST END BEFORE REQ START                     
         BL    CHKEST14                                                         
         CLC   ESTART,SBQEND       EST START AFTER REQ END                      
         BH    CHKEST14                                                         
*                                                                               
CHKEST6  CLC   SBQESFLT,BLANKS     TEST FOR ESTIMATE FILTERING                  
         BNH   CHKESTEQ                                                         
         TM    SBEFLAG5,SBE5EFNP   EST FILTER "OR" & NON-POSITIONAL?            
         BZ    CHKEST7             NO                                           
         BRAS  RE,ESTFILT          "OR" NON-POSITIONAL FILTER PASSED?           
         BNE   CHKEST14            NO - FILTER OUT                              
         BRAS  RE,ESTFILTN         "OR" NON-POSITIONAL "-" FILT PASS?           
         BNE   CHKEST14            NO - FILTER OUT                              
         B     CHKESTEQ            YES                                          
*                                                                               
CHKEST7  LA    R1,3                                                             
         LA    RE,SBQESFLT                                                      
         LA    RF,EPROF                                                         
*                                                                               
CHKEST8  CLI   0(RE),C'*'                                                       
         BE    CHKEST12                                                         
         CLI   0(RE),C' '                                                       
         BE    CHKEST12                                                         
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    CHKEST10            YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   CHKEST14                                                         
         B     CHKEST12                                                         
*                                                                               
CHKEST10 MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    CHKEST14                                                         
*                                                                               
CHKEST12 LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CHKEST8                                                       
         B     CHKESTEQ                                                         
*                                                                               
CHKEST14 MVC   IOKEY+8(5),XFF      SKIP TO NEXT ESTIMATE                        
         CLC   SBQESTND,IOKEY+7                                                 
         BNE   CHKESTNE                                                         
*                                                                               
CHKEST16 MVC   IOKEY+7(6),XFF      SKIP TO NEXT PRODUCT                         
*                                                                               
CHKESTNE J     NEQEXIT                                                          
*                                                                               
CHKESTEQ LLC   RE,IOKEY+7          SHOW VALID IN ESTIMATE TABLE                 
         LA    RE,ESTTAB(RE)                                                    
         MVI   0(RE),1                                                          
         J     EQEXIT                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK MARKET GROUPS                                            
* INPUT  : SVMKT=MARKET CODE                                                    
*          SVPRD=PRODUCT CODE                                                   
* OUTPUT : CC EQ - MARKET IS IN VALID MKTGRP AND MGRP RETURNED IN HALF          
*          CC NE - MARKET NOT IN VALID MARKET GROUP                             
*                                                                               
CHKMGRP  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,3,SVMKT          INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         OC    SBPGRPEX(2),SBPGRPEX   TEST PRODUCT GROUP EXCEPTION              
         BZ    CM4                                                              
         LLC   RF,SVPRD            YES-FIND PRODUCT'S PRODUCT GROUP             
         LTR   RF,RF                                                            
         BZ    CM4                                                              
         BCTR  RF,0                                                             
         MHI   RF,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    CM4                                                              
         AR    R1,RF                                                            
         USING PRDBUFFD,R1                                                      
         MVC   HALF,PBGROUP                                                     
         OC    HALF,SBPG1MSK                                                    
         DROP  R1                                                               
         LA    R0,SBEXMAX                                                       
         LA    R1,SBPGRPEX                                                      
         LA    RF,SBAMGTB2                                                      
*                                                                               
CM2      OC    0(2,R1),0(R1)                                                    
         BZ    CM4                                                              
         MVC   HALF2,0(R1)                                                      
         OC    HALF2,SBPG1MSK                                                   
         CLC   HALF,HALF2          TEST IT'S THE EXCEPTION GROUP                
         BNE   *+12                                                             
         A     RE,0(RF)            YES-THEN USE APPROPRIATE MGRP TABLE          
         B     CM6                                                              
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,CM2                                                           
*                                                                               
CM4      A     RE,SBAMGTAB                                                      
*                                                                               
CM6      MVC   HALF,=X'9999'                                                    
         OC    0(2,RE),0(RE)                                                    
         JZ    NEQEXIT                                                          
         MVC   HALF,0(RE)                                                       
         J     EQEXIT                                                           
         EJECT                                                                  
* CHECK STATION AGAINST REQUEST FILTERS                                         
* OUTPUT : CC EQ - STATION IS OK                                                
*             NE - STATION SHOULD BE REJECTED                                   
*                                                                               
CHKSTA   CLI   SBQSTA+4,C'D'       DIGITAL REQUEST?                             
         BE    CHKSTA00            YES                                          
         CLI   SBQSTA+4,C'S'       STREAMING REQUEST?                           
         BE    CHKSTA00            YES                                          
         CLI   SBQSTA+4,C'C'       IHEART STREAMING REQUEST?                    
         BNE   CHKSTA10            NO                                           
*                                                                               
CHKSTA00 GOTOR AMSUNPK,PARM,(X'80',SVMKT),WORK,WORK+4                           
         CLC   WORK+8(1),SBQSTA+4  MATCH ON DIGITAL/STREAMING?                  
         BNE   CHKSTAN             NO                                           
*                                                                               
CHKSTA10 CLI   SBQAFFIL,C' '       AFFILIATE FILTER                             
         BNH   *+14                                                             
         CLC   SBQAFFIL,SBAFFIL                                                 
         BNE   CHKSTAN                                                          
         CLI   SBQSGRD,C' '        TEST STATION GROUP REQUEST                   
         BNH   CHKSTAY                                                          
         ICM   RF,15,QSGRLEN       TEST FILTERING                               
         BZ    CHKSTAY                                                          
         CLC   SBBSGR,=X'9999'     YES-                                         
         BE    CHKSTAN                                                          
         UNPK  DUB,SBBSGR(3)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SBQSGRF(0),DUB+3                                                 
         BE    CHKSTAY                                                          
*                                                                               
CHKSTAN  J     NEQEXIT                                                          
*                                                                               
CHKSTAY  J     EQEXIT                                                           
         EJECT                                                                  
* CHECK DAYPART BELONGS TO MASTER DAYPART GROUP FILTER                          
* INPUT  : R1=A(DAYPART)                                                        
*          SBDPTMEN=DAYPART MENU                                                
* OUTPUT : CC EQ - DAYPART BELONGS TO MASTER DAYPART GROUP                      
*             NE - DAYPART REJECTED                                             
*                                                                               
CHKMASDP DS    0H                                                               
         LR    R3,R1                                                            
         CLC   SBQMASDP,0(R3)      TEST DAYPART=MASTER DAYPART FILTER           
         BE    CHKMDEQ                                                          
         LA    R4,SBDPTTAB         GET A(DAYPART TABLE)=R4                      
         OC    SBADPTTB,SBADPTTB                                                
         BZ    *+12                                                             
         BRAS  RE,LGETDPTB                                                      
         L     R4,FULL                                                          
         LR    R1,R4                                                            
*                                                                               
CHKMD2   CLI   0(R1),0             LOOK FOR MASTER DAYPART IN TABLE             
         BE    CHKMDNE                                                          
         CLC   SBQMASDP,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,5(R1)                                                         
         B     CHKMD2                                                           
         LLC   RE,1(R1)            FOUND-CHECK IT'S PART OF A GROUP             
         SRL   RE,4                                                             
         LTR   RE,RE               RE=DAYPART GROUP NUMBER                      
         BZ    CHKMDNE                                                          
         MVI   BYTE,0                                                           
         LR    R1,R4                                                            
*                                                                               
CHKMD4   CLI   0(R1),0             LOOK FOR DAYPART IN TABLE                    
         BE    CHKMDNE                                                          
         LLC   RF,1(R1)                                                         
         SRL   RF,4                                                             
         CR    RE,RF               TEST THIS DAYPART PART OF GROUP              
         BNE   CHKMD8                                                           
         CLI   BYTE,0              YES-TEST FIRST IN GROUP                      
         BNE   CHKMD6                                                           
         CLC   SBQMASDP,0(R1)      YES-CHECK IT'S THE MASTER DAYPART            
         BNE   CHKMDNE                                                          
         MVI   BYTE,1                                                           
*                                                                               
CHKMD6   CLC   0(1,R1),0(R3)                                                    
         BE    CHKMDEQ             FOUND                                        
*                                                                               
CHKMD8   LA    R1,5(R1)                                                         
         B     CHKMD4                                                           
*                                                                               
CHKMDNE  J     NEQEXIT                                                          
*                                                                               
CHKMDEQ  J     EQEXIT                                                           
         EJECT                                                                  
* ROUTINE TO FILTER DAYPART                                                     
* INPUT  : R1=A(DAYPART)                                                        
*          SBQDPT AND SBQDPLST CONTAIN DAYPART FILTERS                          
* OUTPUT : CC EQ - DAYPART IS OK                                                
*          CC NE - DAYPART NOT OK                                               
*                                                                               
         SPACE 1                                                                
FILTDPT  DS    0H                                                               
         LA    RE,WORK             RE=A(DAYPART FILTER LIST)                    
         MVC   0(1,RE),SBQDPT                                                   
         MVC   1(L'SBQDPLST,RE),SBQDPLST                                        
         LA    R0,L'SBQDPLST+1                                                  
*                                                                               
FILTDPT2 CLI   0(RE),0             TEST END OF LIST                             
         BE    FILTDPT4                                                         
         TM    SBQDPTLN,SBQDLEXC   NO-TEST TO EXCLUDE DAYPARTS                  
         BO    *+18                                                             
         CLC   0(1,R1),0(RE)       NO-TEST FOR HIT                              
         BE    FILTDPTY            BINGO                                        
         B     *+14                                                             
         CLC   0(1,R1),0(RE)       YES-EXCLUDE IF HIT                           
         BE    FILTDPTN                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,FILTDPT2                                                      
*                                                                               
FILTDPT4 TM    SBQDPTLN,SBQDLEXC                                                
         BO    FILTDPTY                                                         
*                                                                               
FILTDPTN J     NEQEXIT                                                          
*                                                                               
FILTDPTY J     EQEXIT                                                           
         EJECT                                                                  
*==============================================================*                
* REDIRECT CALLS FOR MSPACK/MSUNPK TO STAPACK                  *                
* USER IS RESPONSIBLE FOR MODIFYING COUNTRY IF HE SWITCHES     *                
* FROM US TO CANADIAN AGENCY                                   *                
*==============================================================*                
         SPACE 1                                                                
MSPACK   DS    0H                                                               
         LM    R2,R4,0(R1)         GET A(MKT)/A(STA)/A(MKTSTA)                  
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,SBQAGY                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,SBMED                                                    
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPQMKT,0(R2)      MARKET                                       
         MVC   STAPQSTA(8),0(R3)   STATION                                      
*                                                                               
         GOTOR VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    MSP10               NO ERROR                                     
         TM    FLAG2,STPNODIE                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    0(5,R4),0(R4)                                                    
         NI    FLAG2,X'FF'-STPNODIE      RESET FLAG AFTER EACH CALL             
         J     NEQEXIT                                                          
*                                                                               
MSP10    MVC   0(5,R4),STAPMKST    RETURN RESULT                                
         NI    FLAG2,X'FF'-STPNODIE      RESET FLAG AFTER EACH CALL             
         J     EQEXIT                                                           
         DROP  R1                                                               
         EJECT                                                                  
MSUNPK   DS    0H                                                               
         LM    R2,R4,0(R1)         GET A(MKTSTA)/A(MKT)/A(STA)                  
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SBQAGY                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,SBMED                                                    
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPMKST,0(R2)      MKTSTA                                       
*                                                                               
         GOTOR VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    MSU10                                                            
         TM    STAPERR,QSTP_INVCBL                                              
         BNZ   MSU10                                                            
         TM    FLAG2,STPNODIE                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    0(8,R4),0(R4)                                                    
         XC    0(4,R3),0(R3)                                                    
         NI    FLAG2,X'FF'-STPNODIE      RESET FLAG AFTER EACH CALL             
         J     NEQEXIT                                                          
*                                                                               
MSU10    MVC   0(4,R3),STAPQMKT    RETURN RESULT                                
         MVC   0(5,R4),STAPQSTA                                                 
         TM    STAPERR,QSTP_INVCBL INVALID CABLE NETWORK?                       
         BZ    MSU15               NO                                           
         MVC   5(3,R4),=C'???'                                                  
         J     EQEXIT                                                           
*                                                                               
MSU15    LTR   R2,R2               TEST 8 BYTE STATION REQ                      
         BNM   *+10                                                             
         MVC   0(8,R4),STAPQSTA                                                 
         J     EQEXIT                                                           
         DROP  R1                                                               
         EJECT                                                                  
*===============================================================*               
* ON ENTRY STAFILT POINTS TO BMKSTA                             *               
*===============================================================*               
         SPACE 1                                                                
CBLFILT  DS    0H                                                               
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    CBLF20               NFC (NO CABLE)                              
*                                                                               
         CLI   STAFILT+2,X'E8'     TEST CABLE STATION                           
         BL    *+14                NO                                           
         CLC   SBQCNET,BLANKS      TEST CABLE NETWORK FILTER                    
         BH    CBLF2               YES - ALWAYS APPLY IT                        
*                                                                               
         CLI   SBQSTA+4,C'/'       TEST CABLE ONLY                              
         BNE   CBLF10                                                           
         CLI   STAFILT+2,X'E8'     TEST CABLE STATION                           
         BL    CBLNEQ              NO - NEXT STATION                            
         CLC   SBQCNET,BLANKS      TEST CABLE NETWORK FILTER                    
         BNH   CBLF20              NO                                           
*                                                                               
CBLF2    XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'K'                                                     
         MVC   STAPAGY,SBQAGY                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,SBMED                                                    
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPMKST,STAFILT    MKTSTA                                       
         MVC   STAPQNET,SBQCNET    CABLE NETWORK                                
*                                                                               
         GOTOR VSTAPACK,(R1)                                                    
         CLI   STAPERR,QSTP_YES                                                 
         BE    CBLF20                                                           
         CLI   STAPERR,QSTP_NO                                                  
         BE    CBLNEQ                                                           
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
CBLF10   CLI   SBQSTA+4,C'-'       TEST ALL BUT CABLE                           
         BNE   CBLF20                                                           
         CLI   STAFILT+2,X'E8'     TEST CABLE STATION                           
         BNL   CBLNEQ              YES - NEXT MARKET                            
*                                                                               
CBLF20   OC    SBQSTA,SBQSTA       TEST FOR STATION FILTER                      
         BZ    *+14                                                             
         CLC   =C'ALL',SBQSTA                                                   
         BNE   CBLF30                                                           
         CLI   QNETBITS,0          NO-TEST CAN NET FILTER                       
         BE    CBLEQ               NO                                           
         CLC   QNETBITS,STAFILT+4  ELSE MATCH BITS                              
         BNE   CBLNEQ                                                           
         B     CBLEQ                                                            
*                                                                               
* NOT DOING ALL STATIONS - SO MATCH ON STATION                                  
*                                                                               
CBLF30   OC    QBSTACAN,QBSTACAN   TEST CANADIAN STATION FILTER                 
         BZ    CBLF40                                                           
         CLC   QBSTACAN(2),STAFILT+2  COMPARE 2 BYTES OF STATION                
         BE    CBLEQ                                                            
         B     CBLNEQ                                                           
*                                                                               
CBLF40   SR    R1,R1                                                            
         ICM   R1,7,QBSTA                                                       
         SR    RF,RF                                                            
         ICM   RF,7,STAFILT+2                                                   
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    CBLF42               NFC (NO CABLE)                              
         CLI   QBSTA,X'E8'         TEST CABLE                                   
         BL    CBLF42              NOT CABLE - DO FULL COMPARE                  
         N     R1,=X'00FFFF80'     DROP NETWORK BITS                            
         N     RF,=X'00FFFF80'     DROP NETWORK BITS                            
*                                                                               
CBLF42   CR    R1,RF               TEST FOR RIGHT STATION                       
         BNE   CBLNEQ              READ NEXT MARKET                             
*                                                                               
CBLEQ    J     EQEXIT                                                           
CBLNEQ   J     NEQEXIT                                                          
         EJECT                                                                  
*                                                                               
** GET RATING SVCE MKT# FROM ALPHAMKT RECORD **                                 
*                                                                               
GETALPH  DS    0H                                                               
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY    SAVE KEY AND KEYSAVE           
         LA    R2,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    R2,SBMED            YES - THIS IS THE CURRENT MEDIA              
         CLI   0(R2),C'R'          MEDIA R?                                     
         BNE   GAX                 NO - THIS IS ONLY FOR RADIO                  
         XC    SBMKNUM,SBMKNUM                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTDMREC,R2                                                       
         XC    CTDMKEY,CTDMKEY                                                  
         MVI   CTDMKTYP,CTDMKTEQ   RECORD TYPE                                  
         MVI   CTDMKTY2,CTDMKT2E   SUB-RECORD TYPE                              
         MVI   CTDMKMED,C'R'       MEDIA                                        
         MVI   CTDMKSRC,C'N'       SOURCE                                       
         CLI   SBCPROF+3,C'0'      TEST FOR NSI                                 
         BE    *+8                                                              
         MVI   CTDMKSRC,C'A'                                                    
*                                                                               
         USING MKTKEY,RE                                                        
         LA    RE,SBMKTREC                                                      
         MVC   CTDMKMKT,MKTALST                                                 
*                                                                               
         OC    STAMKALF,STAMKALF                                                
         BZ    *+10                                                             
         MVC   CTDMKMKT,STAMKALF                                                
         OC    CTDMKMKT,CTDMKMKT                                                
         BZ    GAX                                                              
*                                                                               
GALPH10  MVC   CTDMKBKT,SBQBKTYP   BOOKTYPE                                     
         CLI   CTDMKBKT,0                                                       
         BNE   *+8                                                              
         XI    CTDMKBKT,X'FF'                                                   
         GOTOR AIO,IOCTFILE+IOHI+IO2                                            
         CLC   IOKEY(CTDMKNUM-CTDMKEY),IOKEYSAV                                 
         BNE   GAX                                                              
         L     R2,IOADDR           PULL OUT RTNG SVCE MKT#                      
         USING CTDMREC,R2                                                       
         MVC   SBMKNUM,CTDMKNUM                                                 
         DROP  R2                                                               
*                                                                               
GAX      MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         J     EQEXIT                                                           
         EJECT                                                                  
*                                                                               
* FOR GETPWMKT & GETPWSTA, PASS PWKEY IN SVKEY                                  
*                                                                               
GETPWMKT DS    0H                                                               
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY  SAVE KEY AND KEYSAVE             
         NI    FLAG2,X'FF'-FLGETPW RESET FLAG                                   
         SR    RF,RF                                                            
         ICM   RF,7,SBAWIPW         A(PW AREA) PASSED?                          
         BZ    GETPWMX              NO                                          
         XC    0(SBWIPWL,RF),0(RF) CLEAR PW DATA EXTRACT AREA                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING PWRECD,R5                                                        
*                                                                               
         MVC   PWFKEY(PWKSTA-PWKTYP),SVKEY                                      
         L     R5,AIO2                                                          
         XC    0(100,R5),0(R5)     CLEAR PW REC AREA                            
         GOTOR AIO,IOSPTDIR+IORD                                                
         BNE   GETPW20             NO PW REC - HOOK TO CLEAR DATES              
*                                                                               
         GOTOR (RF),IOSPTFIL+IOGET+IO2                                          
         BNE   GETPWMX                                                          
         SR    RF,RF                                                            
         ICM   RF,7,SBAWIPW                                                     
         USING SBWIPWD,RF                                                       
*                                                                               
         MVC   SBPWTAX,PWGNTAX                                                  
         SR    R0,R0                                                            
         LA    RE,PWEL                                                          
         LA    R1,SBPWWK                                                        
         LA    R3,14                                                            
         USING PWWKEL,RE                                                        
GETPW10  CLI   0(RE),0                                                          
         BE    GETPW20                                                          
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'05'         WEEK ELEM?                                   
         BNE   GETPW10                                                          
*                                                                               
         MVC   0(2,R1),PWWKDATE                                                 
         MVC   2(4,R1),PWWKPCT                                                  
         LA    R1,L'SBPWWK(R1)                                                  
         BCT   R3,GETPW10                                                       
         DROP  RE,RF                                                            
*                                                                               
GETPW20  ST    R5,SBACURCH         PASS A(PW MKT REC)                           
         TM    FLAG2,SKIPSTA       CALLED FROM READGOLS?                        
         BZ    *+8                  NO                                          
         OI    SBACURCH,X'80'       YES - SET FLAG                              
*                                                                               
* SEE IF THIS RECORD HAS ALREADY BEEN READ - IF SO, CLEAR REC BEFORE            
* CALLING TO AVOID PICKING UP BILLING ADJUSTMENTS                               
         L     RF,SBAMKEST                                                      
         LLC   RE,PWKEST           GET THE CURRNT EST #                         
         AR    RF,RE                                                            
         CLI   0(RF),0             HAVE WE SEEN THIS EST/MKT BEFORE?            
         BNE   GETPW30              YES - DON'T CALL WITH PROCPW                
         STC   RE,0(RF)             NO - SET BEEN HERE BEFORE                   
*                                                                               
         MVI   SBMODE,SBPROCPW     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
GETPW30  DS    0H                                                               
         MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         TM    FLAG2,SKIPSTA       SKIP STA REC (READGOLS)                      
         BNZ   GETPWMX                                                          
         GOTOR AGETPWS             IF MARKET CHANGES, SO MUST STA               
*                                                                               
GETPWMX  NI    FLAG2,X'FF'-SKIPSTA RESET FLAG                                   
         B     GETPWX                                                           
*                                                                               
*                                                                               
GETPWSTA DS    0H                                                               
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY  SAVE KEY AND KEYSAVE             
         NI    FLAG2,X'FF'-FLGTPWS RESET FLAG                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING PWRECD,R5                                                        
*                                                                               
         MVC   PWFKEY,SVKEY                                                     
*                                                                               
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    GETPWS10             NFC (NO CABLE)                              
         CLI   PWKSTA,X'E8'        TEST CABLE?                                  
         BL    GETPWS10             NO                                          
         CLC   PWFKEY,PWLKEY       SAME STA AS LAST?                            
         BE    GETPWX              YES - DON'T PROCESS AGAIN                    
*                                                                               
GETPWS10 MVC   PWLKEY,PWFKEY                                                    
*                                                                               
         GOTOR AIO,IOSPTDIR+IORD                                                
         BNE   GETPWX              NO PW STA REC                                
         GOTOR (RF),IOSPTFIL+IOGET+IO2                                          
         BNE   GETPWX                                                           
         L     R5,AIO2                                                          
*                                                                               
         ST    R5,SBACURCH         PASS A(PW STA REC)                           
         MVI   SBMODE,SBPROCP2     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
GETPWX   DS    0H                                                               
         MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         J     EQEXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
RDBILL   BRAS  RE,RDBILL1          READ NETDEF                                  
*                                                                               
RS00     MVI   SBBEST,0                                                         
         MVI   SBBPRD,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    SBDPT,SBDPT                                                      
         XC    SBDPTGRP,SBDPTGRP                                                
         MVI   SBLEN,0                                                          
         XC    SVSTA,SVSTA                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING STABUCKD,R2                                                      
         MVC   STABKCOD,=X'0E01'                                                
         MVC   STABKAM,SBBAGYMD                                                 
         CLI   SBQMED,C'C'         TEST MEDIA = COMBINED                        
         BNE   RS7                                                              
         NI    STABKAM,X'F0'       YES-ALTER MEDIA TO T OR C                    
         CLI   MEDCMED,C'T'                                                     
         BNE   *+12                                                             
         OI    STABKAM,X'01'                                                    
         B     RS7                                                              
         OI    STABKAM,X'03'                                                    
*                                                                               
RS7      MVC   STABKCLT,SBBCLT                                                  
         LA    R4,STABKPRD-STABUCK-1                                            
         CLI   SBQBPRD,0                                                        
         BE    RS8                                                              
         CLI   SBQBPRD,FF                                                       
         BE    RS8                                                              
         MVC   STABKPRD,SBQBPRD                                                 
         LA    R4,L'STABKPRD(R4)                                                
         CLC   SBQEST,SBQESTND                                                  
         BNE   RS8                                                              
         MVC   STABKEST,SBQEST                                                  
         LA    R4,L'STABKEST(R4)                                                
         MVC   STABKMKT,QBMKT                                                   
         OC    QBMKT,QBMKT                                                      
         BZ    RS8                                                              
         CLC   QBMKTST,QBMKTND                                                  
         BNE   RS8                MARKET RANGE                                  
         LA    R4,L'STABKMKT(R4)                                                
         OC    SBQSTA,SBQSTA                                                    
         BZ    RS8                                                              
         CLC   =C'ALL',SBQSTA                                                   
         BE    RS8                                                              
         OC    QBSTACAN,QBSTACAN                                                
         BNZ   RS8                                                              
         MVC   STABKSTA,QBSTA                                                   
         AHI   R4,L'STABKSTA-1                                                  
*                                                                               
RS8      EX    R4,*+8              TEST SAVED KEY HAS A GOOD BILL KEY           
         B     *+10                                                             
         CLC   IOKEY(0),SBSVKEY                                                 
         BNE   RS10                                                             
         MVC   IOKEY(L'SBSVKEY),SBSVKEY   YES-THEN START THERE                  
*                                                                               
RS10     LA    R2,IOKEY                                                         
         TM    SBIOFLAG,SBRDDEL                                                 
         BNZ   RS12                                                             
         GOTOR AIO,IOSPTDIR+IOHI                                                
         B     RS15                                                             
RS12     GOTOR AIO,IOSPTDIR+IOHID                                               
*                                                                               
RS15     MVC   SBRECDA,IODA        SAVE DISK ADDRESS                            
         EX    R4,*+8              R4 = KEY COMPARE LENGTH - 1                  
         B     *+10                                                             
         CLC   STABUCK(0),IOKEYSAV                                              
         BNE   RS94                                                             
*                                                                               
         NI    SBEFLAG7,X'FF'-SBE7COS2   TURN OFF COST2 BILL REC FLAG           
         TM    SBEFLAG7,SBE7BCO2   REPORTING BILL COST2?                        
         BZ    RS15AA              NO                                           
         CLI   STABKCUR,X'01'      COST2 RECORD?                                
         BNE   *+8                 NO                                           
         OI    SBEFLAG7,SBE7COS2   YES - SET COST2 BILL REC FLAG                
         B     RS15B                                                            
*                                                                               
RS15AA   TM    SBEFLAG,SBE2COS     TEST COS2 REQUEST                            
         BZ    RS15A               NO                                           
         CLI   STABKCUR,X'01'                                                   
         BNE   RS92                                                             
         B     RS15B                                                            
*                                                                               
RS15A    TM    SBQRDOPT,SBQROSTA   SKIP CUR=X'01'?                              
         BZ    *+12                NO                                           
         CLI   STABKCUR,X'01'                                                   
         BE    RS92                                                             
*                                                                               
RS15B    TM    SBQRDOPT,SBQROMAB   UNLESS OPTION TO READ MANUAL BLG,            
         BO    *+14                                                             
         CLC   STABKMKT,=X'270E'   IGNORE MARKET 9998                           
         BE    RS50                                                             
         CLI   CANADA,C'Y'         TEST CANADIAN AGENCY                         
         BNE   RS16                                                             
         CLI   SBQCURR,C'C'        YES-TEST CANADIAN DOLLARS REQUESTED          
         BNE   *+12                                                             
         CLI   STABKCUR,C'U'       US DOLLARS?                                  
         BE    RS92                YES - EXCLUDE                                
         CLI   SBQCURR,C'U'        TEST US DOLLARS REQUESTED                    
         BNE   RS16                                                             
         CLI   STABKCUR,C'U'       YES-TEST KEY FOR US DOLLARS                  
         BNE   RS92                                                             
*                                                                               
RS16     CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BE    RS18                                                             
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BE    RS20                YES - RETURN ALL PRODUCTS                    
         CLC   SBQBPRD,STABKPRD    NO - FILTER ON PRODUCT                       
         BE    RS20                                                             
         BL    RS94                                                             
         MVC   STABKPRD,SBQBPRD    SKIP TO REQ PRODUCT                          
         XC    STABKEST(7),STABKEST                                             
         B     RS10                                                             
*                                                                               
RS18     CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   RS20                                                             
         LLC   RE,STABKPRD         FIND PRDGRP                                  
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    RS55                                                             
         DROP  R1                                                               
*                                                                               
RS20     ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   RS22                YES                                          
         LLC   RE,STABKEST                                                      
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE ESTIMATE                         
         BNE   RS30                YES                                          
         LR    R1,RE               NO-SKIP TO NEXT ACTIVE ESTIMATE              
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         LLC   R3,STABKEST                                                      
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     RS55                NEXT PRODUCT                                 
         STC   R3,STABKEST         NEXT ACTIVE ESTIMATE                         
         MVC   STABKMKT,QBMKT                                                   
         MVC   STABKSTA,QBSTA                                                   
         MVI   STABKCUR,0                                                       
         B     RS10                YES                                          
*                                                                               
RS22     LLC   RE,STABKPRD         PRODUCT                                      
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    RE,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   R0,STABKEST         ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    RS50                NO                                           
         ICM   R5,15,SBAESTBF      YES-GET ESTIMATE DETAILS                     
         BZ    RS30                                                             
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R5,RF                                                            
         USING ESTBUFFD,R5                                                      
         MVC   SBESTFLT,EBFILT                                                  
         MVC   SBRTLSCH,EBRTLSCH                                                
***      MVC   SBUE1FLD,EBUFLD1                                                 
***      MVC   SBUE2FLD,EBUFLD2                                                 
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
         GOTOR VRCPACK,PARM,(C'U',EBSREP),SBSREP                                
         DROP  R5                                                               
*                                                                               
RS30     OC    QBMKTST(4),QBMKTST  TEST MARKET START/END SPECIFIED              
         BNZ   RS31                YES                                          
         OC    QBMKT,QBMKT         TEST REQUEST FOR MKT 0 (CAN NTWK)            
         BZ    RS32                YES - DO NOT TEST MARKET                     
RS31     CLC   STABKMKT,QBMKTST    TEST MARKET WITHIN RANGE                     
         BL    *+18                                                             
         CLC   STABKMKT,QBMKTND                                                 
         BH    RS50                DONE WITH MARKET - NEXT ESTIMATE             
         B     RS32                                                             
         MVC   STABKMKT,QBMKTST    SKIP TO MARKET START                         
         MVC   STABKSTA,QBSTA                                                   
         MVI   STABKCUR,0                                                       
         B     RS10                                                             
*                                                                               
RS32     CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP FILTERING              
         BNH   RS33                NO                                           
         MVC   SVMKT,STABKMKT                                                   
         MVC   SVPRD,STABKPRD                                                   
         GOTOR ACHKMGRP                                                         
         BNE   RS45                MARKET NOT IN MKTGRP - READ NEXT MKT         
*                                                                               
RS33     OC    QBMKT,QBMKT         TEST MARKET 0 REQUEST                        
         BNZ   RS37                                                             
         OC    STABKMKT,STABKMKT   TEST MARKET 0 BILL                           
         BNZ   RS37                NO                                           
         OC    QBNET,QBNET         YES-TEST NETWORK FILTER                      
         BZ    RS37                                                             
         CLC   STABKSTA(2),QBNET   YES - MATCH CALL LETTERS                     
         BH    RS50                                                             
         BE    RS37                                                             
         MVC   STABKSTA,QBNET                                                   
         MVI   STABKSTA+2,0                                                     
         MVI   STABKCUR,0                                                       
         B     RS10                                                             
*                                                                               
RS37     SR    R0,R0                                                            
         ICM   R0,7,SVSTA                                                       
         SR    RF,RF                                                            
         ICM   RF,7,STABKSTA                                                    
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    RS40                 NFC (NO CABLE)                              
         CLI   STABKSTA,X'E8'      TEST CABLE                                   
         BL    RS40                                                             
         N     R0,=X'00FFFF80'                                                  
         N     RF,=X'00FFFF80'                                                  
*                                                                               
RS40     CR    R0,RF                                                            
         BE    RS41                                                             
*                                                                               
         MVC   SVSTA,STABKSTA                                                   
         GOTOR AGETSTA                                                          
         BNE   RS44                                                             
*                                                                               
RS41     MVC   STAFILT,STABKMKT                                                 
         GOTOR ACBLFILT                                                         
         BNE   RS44                SKIP TO NEXT STATION                         
*                                                                               
RS42     GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BE    RS60                                                             
*                                                                               
RS44     XC    SBBSTA,SBBSTA       CLEAR BINARY STATION                         
         MVI   STABKCUR,X'FF'      NEXT STATION                                 
         B     RS10                                                             
*                                                                               
RS45     MVC   STABKSTA(4),XFF     NEXT MARKET                                  
         B     RS10                                                             
*                                                                               
RS50     MVC   STABKMKT(6),XFF     NEXT ESTIMATE                                
         ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   RS10                YES                                          
         LLC   R1,STABKEST         NO-FIND NEXT ACTIVE ESTIMATE                 
         LA    R1,1(R1)                                                         
         LR    R3,R1                                                            
         LA    R1,ESTTAB(R1)                                                    
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     RS55                                                             
         STC   R3,STABKEST                                                      
         MVC   STABKMKT,QBMKT                                                   
         MVC   STABKSTA,QBSTA                                                   
         MVI   STABKCUR,0                                                       
         B     RS10                                                             
*                                                                               
RS55     CLI   SBQBPRD,0           NEXT PRODUCT                                 
         BE    *+12                                                             
         CLI   SBQBPRD,FF                                                       
         BNE   RS94                                                             
         MVC   STABKEST(7),XFF                                                  
         B     RS10                                                             
*                                                                               
RS60     CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   RS61                NO                                           
         CLI   SBEBYPRD,C'Y'       YES-TEST TO SPLIT OUT THE PRODUCTS           
         BE    RS61                YES                                          
         CLI   SBBPRD,FF           NO-FORCE PRODUCT=POL                         
         BE    RS65                                                             
         MVI   SBBPRD,FF                                                        
         B     RS62                                                             
*                                                                               
RS61     CLC   STABKPRD,SBBPRD     TEST PRODUCT CHANGE                          
         BE    RS65                                                             
         MVC   SBBPRD,STABKPRD                                                  
*                                                                               
RS62     L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
RS63     CLC   SBBPRD,3(RF)                                                     
         BE    RS64                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    RS63                                                             
         B     RS55                PER TSMY                                     
*                                                                               
RS64     MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    RS65                                                             
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         MVC   SBPRDINT,PBINT-PRDBUFFD(R1)                                      
         MVC   SBUP1FLD,PBUFLD1-PRDBUFFD(R1)                                    
         MVC   SBUP2FLD,PBUFLD2-PRDBUFFD(R1)                                    
         OI    SBEUDEF,SBEUPCOM    TELL CALLER TO GET PRD UCOMS                 
         OC    SBBPGR,SBBPGR                                                    
         BNZ   *+10                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
RS65     CLC   SBBEST,STABKEST     TEST ESTIMATE CHANGE                         
         BE    RS66                                                             
         MVC   SBBEST,STABKEST                                                  
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
RS66     TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    *+8                         NO                                   
         BRAS  RE,ESTUSERF                 YES - GET EST USER FIELDS            
*                                                                               
         CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    RS68                                                             
         CLC   SBBMKT,STABKMKT     YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    RS68                                                             
         MVC   SVMKT,STABKMKT      YES-SET THE MARKET GROUP                     
         MVC   SVPRD,STABKPRD                                                   
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
RS68     TM    SBEFLAG5,SBE5BFRM   GET BILL FORM RECORD?                        
         BZ    *+12                NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BNE   *+14                YES - ALWAYS GET BFORM!                      
         CLC   SBBMKT,STABKMKT     TEST MARKET CHANGE                           
         BE    RS70                                                             
         MVC   SBBMKT,STABKMKT     YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   RS70                                                             
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
RS70     CLC   SBBSTA,STABKSTA     TEST STATION CHANGE                          
         BNE   *+16                                                             
         CLI   GOODSTA,C'N'        NO-SKIP STATION IF IT'S NOT GOOD             
         BE    RS90                                                             
         B     RS78                                                             
         NI    SBINDS,255-SBINETBL YES-                                         
         MVI   GOODSTA,C'N'                                                     
         CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK REQUEST                
         BNE   RS71                                                             
         CLC   SBBSTA(2),STABKSTA                                               
         BE    RS74                YES-GET THE NETWORK                          
*                                                                               
RS71     MVC   SBBSTA,STABKSTA     SET STATION                                  
         GOTOR AMSUNPK,PARM,(X'80',STABKMKT),WORK,WORK+4                        
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         XC    SBNETWK,SBNETWK                                                  
         CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK REQUEST                
         BNE   RS76                                                             
* IF STABKSTA+2 >= 5, IT IS A NETWORK ID. ELSE IT IS MEDIA CODE                 
         CLI   STABKSTA+2,5        YES-TEST STATION BELONGS TO NETWORK          
***         BH    RS74                YES-FIND THE NETWORK                      
         BNL   RS74                 YES - FIND THE NETWORK (YSFI)               
         LA    R1,CNTWKTAB         NO-TEST IF STATION IS A NETWORK              
         LA    R0,128                                                           
RS71A    CLI   0(R1),0             TEST EOT                                     
         BE    RS71B                                                            
         CLC   0(4,R1),SBSTA                                                    
         BE    RS72                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,RS71A                                                         
RS71B    CLI   SBQMED,C'C'         IT'S NOT A NETWORK-TEST MED=COMBINED         
         BNE   RS74                                                             
         MVI   SBMED,C'T'          YES-THEN MEDIA IS TV                         
         B     RS76                                                             
*                                                                               
RS72     MVC   SBNETWK,SBSTA       SET NETWORK=STATION                          
         OC    STABKMKT,STABKMKT   TEST MKT=0                                   
         BNZ   *+8                                                              
         OI    SBINDS,SBINETBL     YES-THEN THIS IS NETWORK BILLING             
         MVI   SBMED,C'N'          MEDIA IS N                                   
         B     RS75                                                             
*                                                                               
* IF STABKSTA+2 >= B0, IT IS CANADIAN CABLE                                     
RS74     CLI   STABKSTA+2,X'B0'                                                 
         BNL   RS72                                                             
         LLC   R1,STABKSTA+2       GET LAST BYTE OF STATION                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,CNTWKTAB(R1)     INDEX INTO NETWORK TABLE                     
         MVC   SBNETWK,0(R1)       SET THE NETWORK                              
         MVI   SBMED,C'N'          MEDIA IS N                                   
*                                                                               
RS75     OC    QBNET,QBNET         TEST NETWORK FILTER                          
         BZ    *+14                                                             
         CLC   SBNETWK,SBQNET      YES-COMPARE NETWORK TO FILTER                
         BNE   RS90                NOT EQUAL - SKIP THIS RECORD                 
         CLC   SBBSTA,STABKSTA     TEST SAME STATION, DIFFERENT NETWORK         
         BE    RS76                                                             
         MVC   SBBSTA,STABKSTA     YES-SET THE STA, BUT NO STA FIRST            
         B     RS78                                                             
*                                                                               
RS76     MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
RS78     CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   RS80                NO                                           
         CLC   SBSTYPE,SBQSTATY    YES-CHECK STATION TYPE                       
         BNE   RS90                (ASSUMES HOOK SET TYPE)                      
*                                                                               
RS80     MVI   GOODSTA,C'Y'             GOOD STATION                            
         MVC   IODA,SBRECDA             GET THE BILL RECORD                     
         GOTOR AIO,IOSPTFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    RS90                                                             
*                                                                               
* HOOK TO USER                                                                  
*                                                                               
         MVI   SBMODE,SBPROCBL                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
*                                                                               
RS90     LA    R2,IOKEY                                                         
         TM    SBIOFLAG,SBNOIO     TEST NO IO EXECUTED IN HOOK TO USER          
         BO    RS92                                                             
         TM    SBIOFLAG,SBRDDEL                                                 
         BNZ   RS91                                                             
         GOTOR AIO,IOSPTDIR+IOHI   NO-REESTABLISH READ SEQUENCE                 
         B     RS92                                                             
RS91     GOTOR AIO,IOSPTDIR+IOHID                                               
*                                                                               
RS92     DS    0H                                                               
         TM    SBIOFLAG,SBRDDEL                                                 
         BNZ   RS93                                                             
         GOTOR AIO,IOSPTDIR+IOSQ   READ NEXT BILL RECORD                        
         B     RS15                                                             
RS93     GOTOR AIO,IOSPTDIR+IOSQD                                               
         B     RS15                                                             
*                                                                               
RS94     CLI   SBQMED,C'C'         TEST MEDIA=COMBINED                          
         BNE   RSX                                                              
         CLI   MEDCMED,C'T'        YES-TEST JUST PROCESSED MEDIA T              
         BNE   RSX                                                              
         MVI   MEDCMED,C'N'        YES-NOW PROCESS MEDIA N                      
         MVI   SBMED,C'N'                                                       
         B     RS00                                                             
*                                                                               
RSX      J     EQEXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
         DROP  R6,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES (LOW ORDER 2 BYTES) SET FROM IO EQUATES *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSAV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)           *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSAV *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
         SPACE 1                                                                
IOEX     NMOD1 IOWORKX-IOWORKD,**IOEX**,CLEAR=YES                               
         USING IOWORKD,RC                                                       
         ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
*                                                                               
         TM    SBIOFLAG,SBSKDEL    NEVER READ DELETED RECS IF SET               
         BNZ   *+16                                                             
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
*                                                                               
         LA    R1,IO1+IO2          ESTABLISH I/O AREA ADDRESS                   
         N     R1,IOCTRL                                                        
         BZ    IOEX4                                                            
         SRL   R1,6                R1=I/O AREA NUMBER                           
         CLM   R1,1,IONUM                                                       
         BNH   *+6                                                              
         DC    H'0'                I/O AREA NUMBER INVALID                      
         SLL   R1,2                                                             
         L     R1,AIO1-4(R1)                                                    
         STCM  R1,15,IOADDR        SET ADDRESS OF I/O AREA                      
IOEX4    LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX6                                                            
         OC    IOFILE,IOFILE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX20                                                           
IOEX6    SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LA    R0,10                                                            
         CR    R1,R0                                                            
         BNH   *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
         USING FILTABD,RE                                                       
IOEX8    CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX8                                                            
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX10                                                           
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX20                                                           
IOEX10   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)                                                   
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     *+16                                                             
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX10                                                           
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IOEX12   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX12                                                           
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
*                                                                               
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX14                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX16                                                           
         OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX14                                                           
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN-OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         LLC   R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         BRAS  RE,IOEX                                                          
         BE    IOEX14              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
IOEX14   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,IOSWAP           SWITCH FILES IF NECESSARY                    
         GOTOR CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK             
         BRAS  RE,IOSWAP           SWITCH BACK                                  
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         B     IOEXX               EXIT TO CALLER                               
IOEX16   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX20                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IOFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
IOEX18   BRAS  RE,IOSWAP           SWITCH FILES IF NECESSARY                    
         GOTOR CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                   
         BRAS  RE,IOSWAP           SWITCH BACK                                  
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS        TEST ANY ERRORS FOUND                        
         BZ    *+12                                                             
         TM    IOERR,IOEDEL        TEST DELETED RECORD FOUND                    
         BZ    IOEXX               NO - EXIT WITH ERROR                         
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEXX               NO - EXIT                                    
         LLC   R1,IOFILKL          YES - EXTRACT DISK ADDRESS                   
         LLC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         B     IOEXX                                                            
IOEX20   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,IOSWAP           SWITCH FILES IF NECESSARY                    
         GOTOR CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         BRAS  RE,IOSWAP           SWITCH BACK                                  
         MVC   IOERR,8(R1)                                                      
*                                                                               
IOEXX    CLI   SBQTRACE,C'Y'                                                    
         BNE   *+8                                                              
         BRAS  RE,IOTRACE                                                       
         MVI   IOQ,1                                                            
         TM    IOERR,IOERRS                                                     
         BZ    IOEXXX                                                           
         MVI   IOQ,2                                                            
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOEXXX                                                           
         MVI   IOQ,0                                                            
IOEXXX   CLI   IOQ,1               SET CONDITION CODE FOR CALLER                
*                                                                               
         J     EXIT                                                             
*                                                                               
* IF ALTERNATE STATION FILE SPECIFIED, SWITCH TSYS IN UTL TO READ IT            
* AND SWITCH TSYS BACK AFTERWARDS                                               
*                                                                               
IOSWAP   DS    0H                                                               
         CLC   IOFILNM,=C'STATION'                                              
         BNER  RE                                                               
         TM    SBIOFLAG,SBXFILE    CROSS FILE READ FLAG?                        
         BZR   RE                  JUST AN EXTRA SAFETY                         
         CLI   SBSTAFSE,0                                                       
         BER   RE                                                               
         ICM   RF,15,SBAUTL                                                     
         XC    SBSTAFSE,4(RF)                                                   
         XC    4(1,RF),SBSTAFSE                                                 
         XC    SBSTAFSE,4(RF)                                                   
         BR    RE                                                               
*                                                                               
IOTRACE  NTR1                                ** IO TRACE ROUTINE **             
         CLC   IOFILNM,=C'XSPDIR '                                              
         BE    IT1                                                              
         CLC   IOFILNM,=C'XSPFIL '                                              
         BE    IT1                                                              
         CLC   IOFILNM,=C'SPTDIR '                                              
         BE    IT1                                                              
         CLC   IOFILNM,=C'SPTFIL '                                              
         BE    IT1                                                              
         CLC   IOFILNM,=C'STATION'                                              
         BNE   ITX                                                              
IT1      CLI   IOCMDNO,IOHI                                                     
         BNE   IT2                                                              
         LA    RE,IOKEYSAV                                                      
         ST    RE,IOTR1                                                         
         MVI   IOTR1,32                                                         
         LA    RE,IOKEY                                                         
         ST    RE,IOTR2                                                         
         MVI   IOTR2,40                                                         
         B     IT8                                                              
IT2      CLI   IOCMDNO,IOSQ                                                     
         BNE   IT4                                                              
         LA    RE,IOKEY                                                         
         ST    RE,IOTR1                                                         
         MVI   IOTR1,32                                                         
         LA    RE,IOKEY                                                         
         ST    RE,IOTR2                                                         
         MVI   IOTR2,40                                                         
         B     IT8                                                              
IT4      CLI   IOCMDNO,IOGET                                                    
         BNE   IT6                                                              
         LA    RE,IOKEY+14                                                      
         CLC   IOFILNM,=C'XSPFIL '                                              
         BNE   *+8                                                              
         LA    RE,IOKEY+36                                                      
         ST    RE,IOTR1                                                         
         MVI   IOTR1,4                                                          
         L     RE,IOADDR                                                        
         ST    RE,IOTR2                                                         
         MVI   IOTR2,16                                                         
         B     IT8                                                              
IT6      CLI   IOCMDNO,IOGET                                                    
         BNE   IT7                                                              
         LA    RE,IOKEY                                                         
         ST    RE,IOTR1                                                         
         MVI   IOTR1,13                                                         
         L     RE,IOADDR                                                        
         ST    RE,IOTR2                                                         
         MVI   IOTR2,16                                                         
IT7      CLI   IOCMDNO,IORD                                                     
         BNE   ITX                                                              
         LA    RE,IOKEYSAV                                                      
         ST    RE,IOTR1                                                         
         MVI   IOTR1,32                                                         
         LA    RE,IOKEY                                                         
         ST    RE,IOTR2                                                         
         MVI   IOTR2,40                                                         
*                                                                               
IT8      OC    SBPRINT,SBPRINT                                                  
         BZ    ITX                                                              
         MVC   P,BLANKS                                                         
         MVC   P(6),IOCMDNM                                                     
         MVC   P+8(6),IOFILNM                                                   
         LA    R3,P+16                                                          
         LLC   R0,IOTR1                                                         
         L     R4,IOTR1                                                         
         CLC   IOFILNM,=C'STATION'                                              
         BNE   IT10                                                             
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         AR    R3,R1                                                            
         B     IT15                                                             
IT10     GOTOR CHEXOUT,PARM,(R4),(R3),(R0),=C'TOG'                              
         A     R3,PARM+16                                                       
IT15     LA    R3,2(R3)                                                         
         LLC   R0,IOTR2                                                         
         L     R4,IOTR2                                                         
         CLC   IOFILNM,=C'STATION'                                              
         BNE   IT20                                                             
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         B     IT30                                                             
IT20     GOTOR CHEXOUT,PARM,(R4),(R3),(R0),=C'TOG'                              
IT30     GOTOR SBPRINT,PARM,PASA,=C'BL01'                                       
*                                                                               
ITX      J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,RELO                                                          
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         MVI   ONLINE,C'Y'         SET ONLINE FLAG                              
         TM    SBINDS,SBIONLIN     TEST FORCE ONLINE PROCESSING                 
         BO    SP2                                                              
         GOTOR CGETFACT,PARM,0                                                  
         L     R1,0(R1)                                                         
         TM    FATFLAG-FACTSD(R1),1                                             
         BZ    SP2                                                              
         MVI   ONLINE,C'N'                                                      
*                                                                               
SP2      MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
         MVI   XFF,X'FF'                                                        
         MVC   XFF+1(L'XFF-1),XFF                                               
*                                                                               
         MVC   SBABUFFS,=C'*BUFFERS'                                            
         MVC   SBMODEQ,=C'***MODE='                                             
         MVC   SBQAREA,=C'REQUESTS'                                             
         MVC   SBDATA,=C'**DATA**'                                              
         MVC   SBBVALS,=C'*BINVALS'                                             
*                                                                               
         MVC   AIO1(12),SBAIO1     SET IO AREAS FROM BLOCK                      
         MVI   IONUM,3             SET NUMBER OF I/O AREAS                      
*                                                                               
*                                  SET RECORD AREA ADDRESSES                    
         ICM   RE,15,SBACLTRC                                                   
         BNZ   *+8                                                              
         LA    RE,CLTREC           CLIENT RECORD                                
         ST    RE,ACLTREC                                                       
*                                                                               
*                                  SET T00A PHASE ADDRESSES                     
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,RELO                                                          
         LA    R3,COREFACS         R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAXIMUM NUMBER OF PHASES                  
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,PARM                                                          
         L     RF,CCALLOV                                                       
SP10     ICM   R0,1,0(R2)                                                       
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SP10                                                          
*                                  SET SPOTIO FACILITY ADDRESSES                
SP11     LA    R0,CONADDRN         R0=NUMBER OF CONTROLLER ADDRESSES            
         LTR   R0,R0                                                            
         BZ    INITNEQ                                                          
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         L     RF,=A(CONADDRS)                                                  
         A     RF,RELO                                                          
         L     R1,0(RF,RE)                                                      
         A     R1,RELO             RELOCATE AND STORE IN W/S                    
         ST    R1,CONFACS(RE)                                                   
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                  FIND FILE TABLE ENTRY (FOR IO)               
         L     R1,AFILTAB          R1 = A(FILE NAMES TABLE)                     
         LA    R1,3(R1)                                                         
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
*                                                                               
         OC    SBQTODAY,SBQTODAY   GETTODAY'S DATE COMPRESSED                   
         BZ    SP16                                                             
         GOTOR CDATCON,PARM,SBQTODAY,(2,SBBTODAY)                               
*                                                                               
         TM    SBQREAD,SBQRDBH     TEST READING BILL HEADERS                    
         BZ    SP16                                                             
         OC    SBQBHIST,SBQBHIST   YES-CONVERT DATE FILTERS                     
         BZ    SP12                                                             
         GOTOR (RF),(R1),(2,SBQBHIST),(0,IVCST)                                 
         GOTOR (RF),(R1),(2,SBQBHIEN),(0,IVCEN)                                 
*                                                                               
SP12     OC    SBQBHRST,SBQBHRST                                                
         BZ    SP14                                                             
         GOTOR (RF),(R1),(2,SBQBHRST),(0,RUNST)                                 
         GOTOR (RF),(R1),(2,SBQBHREN),(0,RUNEN)                                 
*                                                                               
SP14     OC    SBQBHDST,SBQBHDST                                                
         BZ    SP16                                                             
         GOTOR (RF),(R1),(2,SBQBHDST),(3,DUEST)                                 
         GOTOR (RF),(R1),(2,SBQBHDEN),(3,DUEEN)                                 
*                                                                               
*                                  SET EXTRACTED DATA FIELDS                    
SP16     MVC   SBMED,SBQMED                                                     
         CLI   SBQMED,C'*'         TEST MEDIA T AND R REQUEST                   
         BNE   *+8                                                              
         MVI   SBMED,C'T'          YES- START WITH T                            
*                                                                               
INITEQ   CR    RB,RB                                                            
         B     *+6                                                              
INITNEQ  LTR   RB,RB                                                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO LOAD BILL HEADERS RECORDS TO BUFFER               *         
*                                                                     *         
***********************************************************************         
GOCSHR   NTR1  BASE=*,LABEL=*                                                   
         ICM   R4,15,SBACSHRC      POINT TO CASHIER CONTROL BLOCK               
         BNZ   GCSINITX               ALREADY SET                               
*                                                                               
         MVC   SBACASHR,VCASHIER   PASS ON DDCASHIER ADDRESS                    
*                                                                               
         LHI   R4,CSHIERC-WORKD    SET CONTROL BLOCK ADDRESS                    
         LA    R4,WORKD(R4)                                                     
         ST    R4,SBACSHRC                                                      
*                                                                               
         USING CSHIERD,R4          ESTABLISH AREA                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZE DDCASHIER CONTROL BLOCK                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GCSINIT  DS    0H                                                               
*                                                                               
         CLI   ONLINE,C'Y'         IF OFF-LINE GET VUTL                         
         BE    GOCSHRX                NOT VALID ONLINE                          
*                                                                               
         XC    CSHIERD(CSHIERL),CSHIERD INIT CONTROL BLOCK                      
*                                                                               
         MVI   CSHACT,CSHINIQ      SET TO INITIALIZE                            
*                                                                               
         MVC   CSHAGYCH,SBAGY      SET AGENCY ALPHA                             
         MVC   CSHMED,SBMED        SET MEDIA                                    
         MVI   CSHSYS,CSHSPTQ      SET TO SPOT SYSTEM                           
*                                                                               
         MVC   CSHBLLL,=Y(256)     MAX BILL HEADER RECORD LENGTH                
         MVI   CSHBLLKL,L'BKEY     BILL RECORD KEY LENGTH                       
*                                                                               
         MVC   CSHMAX,=F'2000'     MAX NUMBER OF BILL RECORDS                   
*                                                                               
         OI    CSHCTL,CSHCBLLQ     INDICATE BILL DATA WANTED                    
*                                                                               
         TM    SBQREAD2,SBQRD2CA   CHECK IF CASH APPLIED DATA NEEDED            
         BNO   *+8                                                              
         OI    CSHCTL,CSHCCSHQ                                                  
*                                                                               
         MVC   CSHBLLA,SBAIO2      SET A(BILLREC)                               
         MVC   CSHCLTA,SBACLTRC    SET A(CLTREC)                                
*                                                                               
         L     RF,SBCOMFAC         COMFACS ADDRESS                              
         MVC   CSHDMGRA,CDATAMGR-COMFACSD(RF)   DATAMGR ADDRESS                 
         MVC   CSHGTPRA,CGETPROF-COMFACSD(RF)   GETPROF ADDRESS                 
         MVC   CSHDATCA,CDATCON-COMFACSD(RF)    DATCON  ADDRESS                 
*                                                                               
         MVC   CSHTSARA,VTSAROFF   PASS V(TSAROFF)                              
         MVC   CSHUTLA,SBAUTL      PASS A(UTL)                                  
*                                                                               
         GOTOR VCASHIER,PARM,CSHIERD   INIT DDCASHIER                           
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GCSINITX DS    0H                                                               
*                                                                               
*        ADD BILL RECORD TO BUFFER                                              
*                                                                               
         MVI   CSHACT,CSHADDQ      SET ACTION TO ADD                            
*                                                                               
         MVC   CSHBLLA,SBAIO1      SET A(BILL RECORD)                           
*                                                                               
         GOTOR VCASHIER,PARM,CSHIERD    ADD BILL RECORD TO BUFFER               
*                                                                               
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOCSHRX  DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
*                  READ CLEARANCE STATUS RECORDS                      *         
* THIS ROUTINE WAS PART OF EXTRA BUT EXTRA CODE MADE USE OF 3RD BASE  *         
* REGISTER (RC) WHICH WAS CLOBBERED BY THE NMOD1 MACRO  AKAT 7/15/03  *         
***********************************************************************         
READCLS  NTR1  BASE=*,LABEL=*                                                   
         ICM   R3,15,SBACHKTB      CHECK FOR A(CHECK TABLE)                     
         BZ    RCX                                                              
***      TM    FIRSTSW,FSTA        STATION FIRST?                               
***      BZ    *+10                NO                                           
***      XC    4(4,R3),4(R3)       N'RECORDS IN TABLE SO = 0                    
         BRAS  RE,STAFRST          STATION FIRST?                               
         BNE   *+10                NO - FOUND THIS STA IN THE TABLE             
         XC    4(4,R3),4(R3)       N'RECS IN TABLE = 0 (CLEAR TABLE)            
         L     R4,4(R3)            R4=ENTRY COUNT                               
         L     R5,8(R3)            R5=MAX N'ENTRIES                             
         LA    R3,12(R3)           R3=START OF TABLE                            
*                                                                               
         MVC   SVIOKEY,IOKEY       SAVE BUY RECORD KEY                          
         XC    CALLSAVE,CALLSAVE   9D ELEMENT DISPLACEMENT IN BUYREC            
*                                                                               
RC00     BRAS  RE,BUYSTA           WANT TO PROCESS STATION IN FULL?             
         BNE   RCX                 NO, HAVE IT ALREADY                          
*                                                                               
         MVC   HALF,SBBMKT                                                      
         CLI   SBSTA,C'0'          TEST CABLE                                   
         BL    RC01                                                             
         NI    FULL+2,X'80'        BETTER WAY OF REMOVING NETWORK BITS          
         B     RC01A                                                            
*                                                                               
RC01     OC    SBNETWK,SBNETWK     TEST CANADIAN NETWORK                        
         BZ    RC01A                                                            
         XC    HALF,HALF                                                        
         MVC   FULL(3),SBBCNTWK                                                 
*                                                                               
RC01A    XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            CLEARANCE STATUS RECORD KEY                  
         USING CLRSTATD,R2                                                      
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,SBBAGYMD                                                
         CLI   SBQMED,C'C'         TEST MEDIA = COMBINED                        
         BNE   RC01B                                                            
         NI    CLSKAGMD,X'F0'      YES-ALTER MEDIA TO T OR N                    
         CLI   SBMED,C'T'                                                       
         BNE   *+12                                                             
         OI    CLSKAGMD,X'01'                                                   
         B     RC01B                                                            
         OI    CLSKAGMD,X'03'                                                   
*                                                                               
RC01B    MVC   CLSKCLT,SBBCLT                                                   
         MVC   CLSKMKT,HALF                                                     
         MVC   CLSKSTA,FULL                                                     
*                                                                               
         CLI   NINE_D,C'Y'         HAVE STA FROM 9D ELEM?                       
         BNE   RC01C               NO                                           
         MVC   SVSTA,FULL          STA FROM 9D ELEMENT                          
         GOTOR GETSTREC            DID WE GET THE STATION'S MARKET?             
         BNE   *+10                NO                                           
         MVC   CLSKMKT,SVMKT       YES - USE THIS MARKET TO READ CLRST          
         MVC   SVSTA,SBBSTA        RESTORE THE CURRENT STATION                  
         GOTOR GETSTREC                                                         
*                                                                               
RC01C    LA    R1,IOSPTDIR+IOHID                                                
         B     RC4                                                              
*                                                                               
RC2      LA    R1,IOSPTDIR+IOSQD   READ SEQUENTIAL                              
*                                                                               
RC4      DS    0H                                                               
         GOTOR AIO                                                              
         CLC   IOKEY(CLSKDATE-CLSKEY),IOKEYSAV  TEST SAME STATION               
         BNE   RC11                SEE IF WE HAVE X'9D' ELEM IN BUY REC         
*                                                                               
         GOTOR (RF),IOSPTFIL+IOGET+IO3  YES-GET THE RECORD                      
         L     R2,SBAIO3                                                        
         LA    R1,CLSELEMS         SCAN FOR CLEARANCE ELEMENTS                  
         SR    R0,R0                                                            
*                                                                               
RC6      CLI   0(R1),0                                                          
         BE    RC10                                                             
         CLI   0(R1),1                                                          
         BNE   RC8                                                              
         USING CLSTEL01,R1                                                      
*                                                                               
         CLC   CLSTCLRD,SBBQSTP    CLEARANCE BEFORE REQ START?                  
         BL    RC8                  YES - SKIP ELEMENT                          
*                                                                               
RC7      XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING CHKTABD,R6                                                       
         CLC   =C'VOID',CLSTCHK    HAVE A VOID CHECK?                           
         BNE   *+12                NO                                           
         TM    SBEFLAG4,SBE4VOID   YES - HAVE NOVOID OPTION SET?                
         BNZ   RC8                 YES - SKIP THIS ELEMENT                      
*                                                                               
         MVC   CHCLDT,CLSTCLRD     ADD ENTRY - CLEARANCE DATE                   
         MVC   CHCLSEQ,CLSTCLSQ                SEQUENCE NUMBER                  
         MVC   CHSTA,CLSKSTA                   STATION                          
         MVC   CHCHKNUM,CLSTCHK                CHECK NUMBER                     
         MVC   CHCHKDT,CLSTCHDT                CHECK DATE                       
         MVC   CHSTATUS,CLSTSTAT               STATUS                           
         MVC   CHPDREP,CLSTREPT                REPTYPE & PAYEE                  
         MVC   CHDISKA,IOKEY+14                D/A                              
*                                                                               
         CLI   CLSTEL01+1,CL01ELLN IF ELEM LEN L.T. CURRENT EQU,                
         BL    *+10                 THERE IS NO CLSTBKDT                        
         MVC   CHBNKDT,CLSTBKDT     BANK CLEARED DATE                           
         DROP  R6                                                               
*                                                                               
         LA    R6,CHKTABL                                                       
         ST    R6,PARM+12                                                       
         LR    R6,R1               SAVE OFF POINTER TO CLEARANCE ELEM           
         GOTOR SBABINSR,PARM,(1,WORK),(R3),(R4),,(0,6),(R5)                     
         OC    1(3,R1),1(R1)       TABLE FULL?                                  
         BNZ   *+6                 NO                                           
         DC    H'0'                                                             
*        CLI   0(R1),1             RECORD FOUND?                                
*        BE    *+6                 NO, SHOULD NOT ADD DUPLICATES                
*        DC    H'0'                                                             
         L     R4,8(R1)            NUMBER OF RECS (UPDATED BY BINSRCH)          
         LR    R1,R6               RESTORE POINTER TO CLEARANCE ELEM            
*                                                                               
RC8      IC    R0,1(R1)            NEXT ELEMENT                                 
         AR    R1,R0                                                            
         B     RC6                                                              
*                                                                               
RC10     LA    R2,IOKEY            READ NEXT RECORD                             
***         GOTOR (RF),IOSPTDIR+IOSQD                                           
***         B     RC4                                                           
         B     RC2                 SFC!!!                                       
*                                                                               
RC11     L     R1,SBACHKTB                                                      
         ST    R4,4(R1)            UPDATE N'RECORDS IN TABLE                    
         CLI   NINE_D,C'Y'         JUST READ CLRST W/STA FROM 9D ELEM?          
         BE    RC00                YES READ NEXT 9D ELEM OR DEFAULT STA         
*                                                                               
RCX      L     R3,SBACHKTB         SAVE N'RECORDS IN TABLE                      
         ST    R4,4(R3)                                                         
         MVC   IOKEY,SVIOKEY       RESTORE BUY RECORD KEY                       
*                                                                               
         J     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CHECK THE BUY RECORD FOR 9D ELEMENTS (CALL LETTER CHANGES) & READ   *         
* THE CLEARANCE STATUS RECORD W/THAT STA TO GET THE PROPER CHECK DATA *         
*                                                                     *         
* ON ENTRY: 1) SBAIO1   = A(BUY RECORD)                               *         
*           2) CALLSAVE = DISPLACEMENT TO ELEM AFTER LAST 9D ELEM     *         
*                                                                     *         
* ON EXIT:  1) FULL     = PACKED STATION*                             *         
*           2) CALLSAVE = DISPLACEMENT TO ELEM AFTER LAST 9D ELEM     *         
***********************************************************************         
BUYSTA   NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL(3),SBBSTA      DEFAULT STATION                              
         MVI   NINE_D,C'N'         DID NOT FIND ANY 9D ELMS IN BUY REC          
         OC    SBNETWK,SBNETWK     CANADIAN NETWORK?                            
         BNZ   BS20                YES                                          
         L     R2,SBAIO1           AIO AREA BUY RECORD IS IN                    
         SR    R6,R6                                                            
         ICM   R6,3,CALLSAVE       ELEM AFTER LAST 9D ELEMENT (IF ANY)          
         LA    R2,24(R6,R2)        FIRST ELEM OR WHERE WE LAST LEFT OFF         
*                                                                               
BS00     CLI   0(R2),0             END OF BUY RECORD?                           
         BE    BS20                YES, DONE                                    
         CLI   0(R2),X'9D'         FOUND A CALL LETTER CHANGE ELEMENT?          
         BNE   BS10                NO, LOOK AGAIN                               
*                                                                               
         USING SFXELEM,R2                                                       
         TM    FIRSTSW,FSTA        STATION FIRST?                               
         BNZ   BS01                YES, NO NEED TO CHECK THE CHECK TAB          
***                                                                             
* MAKE SURE THAT THE STATION IN FULL IS NOT IN THE TABLE ALREADY                
***                                                                             
         ICM   R3,15,SBACHKTB      CHECK FOR A(CHECK TABLE)                     
         LA    R4,12(R3)           START OF TABLE                               
         L     R5,4(R3)            N' ENTRIES ALREADY IN TABLE                  
         L     R3,8(R3)            MAX N'ENTRIES                                
         LA    R1,CHKTABL                                                       
         ST    R1,PARM+12                                                       
         GOTOR SBABINSR,PARM,(0,SFXSTA),0(R4),(R5),,(0,3),(R3)                  
         CLI   0(R1),1             DID WE FIND THE STATION?                     
         BNE   BSXNO               YES, DON'T PROCESS THE STATION AGAIN         
*                                                                               
BS01     CLC   SFXDATE,SBBQSTP     CHANGE CALL LETTERS BEFORE REQ START         
         BL    BS10                YES, IGNORE THIS                             
*                                                                               
BS05     MVC   FULL(3),SFXSTA      CHECK CLEAR STATUS WITH OLD STATION          
         LLC   R0,1(R2)            LENGTH TO NEXT ELEMENT                       
         AR    R6,R0                                                            
         STCM  R6,3,CALLSAVE       DISPLACEMENT TO NEXT ELEMENT                 
         MVI   NINE_D,C'Y'         INDICATE THAT WE FOUND A 9D ELEM             
         B     BS20                                                             
         DROP  R2                                                               
*                                                                               
BS10     LLC   R0,1(R2)            LENGTH TO NEXT ELEMENT                       
         AR    R2,R0               BUMP                                         
         AR    R6,R0                                                            
         B     BS00                                                             
*                                                                               
BS20     CLI   NINE_D,C'Y'         JUST FIND A 9D ELEMENT?                      
         BE    BSXYES              YES, PROCESS THIS STATION                    
         TM    FIRSTSW,FSTA        STATION FIRST?                               
         BNZ   BSXYES              YES                                          
***                                                                             
* IT'S POSSIBLE TO HAVE FSTA TURNED OFF AND *NOT* HAVE READ THE CLRST           
* RECORD. ANY TIME WE FILTER OUT THE BUY FOR A STATION AND GO TO RB790          
* WE CAN HAVE MODE FSTA BUT TURN IT OFF FOR THE NEXT BUY IF IT'S THE            
* SAME STATION SO A BETTER TEST WOULD BE TO SEE IF THERE IS DATA IN             
* CHECK TABLE FOR THIS STATION                                                  
***                                                                             
         ICM   R3,15,SBACHKTB      CHECK FOR A(CHECK TABLE)                     
         LA    R4,12(R3)           START OF TABLE                               
         L     R5,4(R3)            N' ENTRIES ALREADY IN TABLE                  
         L     R3,8(R3)            MAX N'ENTRIES                                
         LA    R1,CHKTABL                                                       
         ST    R1,PARM+12                                                       
         GOTOR SBABINSR,PARM,(0,SBBSTA),0(R4),(R5),,(0,3),(R3)                  
         CLI   0(R1),1             DID WE FIND THE STATION?                     
         BNE   BSXNO               YES, DON'T PROCESS THE STATION AGAIN         
*                                                                               
BSXYES   CR    RB,RB                                                            
         B     *+6                                                              
BSXNO    LTR   RB,RB                                                            
         J     EXIT                                                             
***********************************************************************         
* IT'S POSSIBLE TO HAVE FSTA TURNED OFF AND *NOT* HAVE READ THE CLRST *         
* RECORD. ANY TIME WE FILTER OUT THE BUY FOR A STATION AND GO TO RB790*         
* WE CAN HAVE MODE FSTA BUT TURN IT OFF FOR THE NEXT BUY IF IT'S THE  *         
* SAME STATION SO A BETTER TEST WOULD BE TO SEE IF THERE IS DATA IN   *         
* CHECK TABLE FOR THIS STATION                                        *         
***********************************************************************         
STAFRST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R3,15,SBACHKTB      CHECK FOR A(CHECK TABLE)                     
         LA    R4,12(R3)           START OF TABLE                               
         L     R5,4(R3)            N' ENTRIES ALREADY IN TABLE                  
         L     R3,8(R3)            MAX N'ENTRIES                                
         LA    R1,CHKTABL                                                       
         ST    R1,PARM+12                                                       
         GOTOR SBABINSR,PARM,(0,SBBSTA),0(R4),(R5),,(0,3),(R3)                  
         CLI   0(R1),1             DID WE FIND THE STATION?                     
         BNE   SFNO                YES - DO NOT CLEAR THE N'ENTRIES             
*                                                                               
SFYES    CR    RB,RB               NO - THIS IS STATION FIRST!                  
         B     *+6                 RETURN CC EQU TO INDICATE FSTA               
SFNO     LTR   RB,RB               RETURN CC NEQ TO INDICATE NOT FSTA           
         J     EXIT                EXIT WITH CC CODE                            
***********************************************************************         
* READ OM RECORDS                                                               
***********************************************************************         
READOM   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,READFLT          READ FLT RECS & BUFFER VALID FLIGHTS         
*                                                                               
         MVI   SBBPRD,0                                                         
         MVI   SBBEST,0                                                         
         XC    SBBSTA,SBBSTA                                                    
         XC    SVSTA,SVSTA                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING DOKEY,R2                                                         
         MVI   DCKTYPE,DCKTYPQ     X'0D'                                        
         MVI   DCKSUBTY,DCKSTYPQ   X'B5'                                        
         MVC   DCKAGMD,SBBAGYMD    A/M                                          
         MVC   DCKCLT,SBBCLT       CLIENT                                       
         CLI   SBQBPRD,0           SINGLE PRODUCT REQUEST?                      
         BE    ROM05               NO, STOP FILLING IN THE KEY                  
         MVC   DCKPRD,SBQBPRD      YES, FILL IN THE PRODUCT                     
         MVC   DCKEST,SBQEST       START ESTIMATE                               
         OC    QBSTA,QBSTA         STATION FILTER?                              
         BZ    ROM05               NO, STOP FILLING IN THE KEY                  
         MVC   DCKSTA,QBSTA        YES, FILL IN THE STATION                     
*                                                                               
ROM05    LA    R1,IOSPTDIR+IOHI                                                 
         B     *+8                                                              
*                                                                               
ROM10    LA    R1,IOSPTDIR+IOSQ                                                 
         GOTOR AIO                                                              
*                                                                               
         CLC   IOKEY(DCKPRD-DOKEY),IOKEYSAV                                     
         BNE   ROMEQU                                                           
***                                                                             
* TEST THE PRODUCT/PRODUCT GROUP                                                
***                                                                             
ROM15    CLI   SBQBPRD,0           SINGLE PRODUCT REQUEST?                      
         BNE   ROM15A              NO                                           
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP FILTERING?                     
         BNH   ROM20               NO                                           
         CLI   DCKPRD,X'FF'        POL IN KEY?                                  
         BNL   ROMEQU              YES - FINISHED                               
         LLC   RE,DCKPRD           NO - FIND PRODUCT GROUP                      
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP     HAVE A PRODUCT GROUP?                        
         BZ    ROM100              NO, BUMP TO NEXT PRODUCT                     
         B     ROM20                                                            
         DROP  R1                                                               
*                                                                               
ROM15A   CLC   SBQBPRD,DCKPRD      PRD FILTER MATCHES KEY?                      
         BE    ROM20               YES                                          
         BL    ROMEQU              PASSED THE FILTER IN KEY...DONE              
         MVC   DCKPRD,SBQBPRD      SKIP TO REQ PRODUCT                          
         XC    DCKEST(7),DCKEST    CLEAR EVERYTHING AFTER PRD                   
         B     ROM05               AND READHI                                   
***                                                                             
* TEST THE ESTIMATE                                                             
***                                                                             
ROM20    LLC   RE,DCKPRD           PRODUCT IN KEY                               
         BCTR  RE,0                                                             
         ICM   R1,15,SBAESTTB      ANY ESTIMATE TABLE?                          
         BNZ   ROM20A              YES                                          
         OC    ESTTAB,ESTTAB       ANY VALID EST FOR THIS PRODUCT?              
         BZ    ROM100              NO, SKIP TO NEXT PRODUCT                     
*                                                                               
         LLC   RE,DCKEST           ESTIMATE IN KEY                              
         LA    RE,ESTTAB(RE)       INDEX INTO ESTIMATE ROW                      
         B     ROM20B                                                           
*                                                                               
ROM20A   SLL   RE,8                PRD X 256                                    
         AR    RE,R1               INDEX INTO EST ROW FOR THIS PRODUCT          
         OC    0(256,RE),0(RE)     ANY VALID ESTIMATES?                         
         BZ    ROM100              NO - SKIP TO NEXT PRODUCT                    
*                                                                               
         LLC   RF,DCKEST           ESTIMATE IN KEY                              
         BCTR  RF,0                                                             
         AR    RE,RF               INDEX INTO ETIMATE ROW                       
ROM20B   CLI   0(RE),0             IS THIS ESTIMATE ACTIVE?                     
         BE    ROM80               NO, SKIP TO NEXT ESTIMATE                    
*                                                                               
         ICM   R1,15,SBAESTBF                                                   
         BNZ   *+14                                                             
         MVC   SBDPTMEN,0(RE)                                                   
         B     ROM21                                                            
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R1,RF                                                            
         USING ESTBUFFD,R1                                                      
         MVC   SBDPTMEN,EBDPTMEN   SET DAYPART MENU                             
         MVC   SBESTFLT,EBFILT     SET ESTIMATE FILTERS                         
         MVC   SBESTSR,EBSREP      SET ESTIMATE'S SPECIAL REP                   
         MVC   SBRTLSCH,EBRTLSCH   SET ESTIMATE'S RETAIL SCHEME CODE            
***      MVC   SBUE1FLD,EBUFLD1                                                 
***      MVC   SBUE2FLD,EBUFLD2                                                 
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
         DROP  R1                                                               
***                                                                             
* TEST THE STATION                                                              
***                                                                             
ROM21    OC    QBSTA,QBSTA         ANY STATION FILTER                           
         BZ    ROM21AA             NO                                           
         CLC   QBSTA,DCKSTA        HAVE THE STATION WE WANT?                    
         BNE   ROM70               NO                                           
*                                                                               
ROM21AA  MVC   SVSTA,DCKSTA        YES-GET STATION DETAILS                      
         GOTOR AGETSTA                                                          
         BNE   ROM70                                                            
         GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BNE   ROM70                                                            
         XC    STAFILT,STAFILT     CLEAR STAFILT                                
         MVC   STAFILT+2(3),DCKSTA PASS STATION                                 
         GOTOR ACBLFILT            PASSED ANY CABLE FILTERS?                    
         BNE   ROM70               NO - SKIP TO NEXT STATION                    
***                                                                             
* WE JUST CHANGED STATION, GET THE IMPLIED MARKET                               
***                                                                             
         GOTOR GETSTREC            GET STATION'S MARKET                         
         BNE   ROM70                                                            
*                                                                               
         CLC   SBBMKT,SVMKT        NEW MARKET?                                  
         BE    ROM22               NO                                           
*                                                                               
         MVC   SBBMKT,SVMKT                                                     
*                                                                               
         OC    QBMKT,QBMKT         HAVE A MARKET FILTER?                        
         BZ    ROM21A              NO                                           
         CLC   QBMKT,SVMKT         MATCH ON MKT?                                
         BE    ROM21A              YES                                          
         XC    SBBMKT,SBBMKT                                                    
         B     ROM70               NO, NEXT STATION                             
*                                                                               
ROM21A   SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB           4 CHAR MKT NUMBER                            
*                                                                               
         CLI   SBQMGR,0            DOING MARKET GROUPS?                         
         BE    ROM21C              NO                                           
         GOTOR ACHKMGRP                                                         
         BE    ROM21B                                                           
         XC    SBBMKT,SBBMKT       CLEAR THIS IF MGROUP DID NOT PASS!           
         B     ROM70                                                            
*                                                                               
ROM21B   MVC   SBBMGR,HALF                                                      
*                                                                               
ROM21C   GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   ROM22                                                            
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
*        JNE   NEQEXIT                                                          
***                                                                             
* TEST PRODUCT CHANGE                                                           
***                                                                             
ROM22    CLC   DCKPRD,SBBPRD       TEST PRODUCT CHANGE                          
         BE    ROM24A                                                           
         MVC   SBBPRD,DCKPRD       SET PRODUCT                                  
         L     RF,ACLTREC          FIND PRODUCT IN CLTHDR                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
ROM23    CLC   SBBPRD,3(RF)                                                     
         BE    ROM24                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    ROM23                                                            
         DC    H'0'                                                             
*                                                                               
ROM24    MVC   SBPRD,0(RF)         PRODUCT                                      
         LLC   RE,SBBPRD           SET PRODUCT GROUP                            
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    ROM24A                                                           
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBBPGR,PBGROUP                                                   
         OC    SBBPGR,SBBPGR                                                    
         BNZ   ROM24A                                                           
         MVC   SBBPGR,=X'9999'                                                  
***                                                                             
* TEST ESTIMATE CHANGE                                                          
***                                                                             
ROM24A   CLC   SBBEST,DCKEST       TEST ESTIMATE CHANGE                         
         BE    ROM25                                                            
         MVC   SBBEST,DCKEST       YES - SET ESTIMATE                           
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
ROM25    TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    *+8                         NO                                   
         BRAS  RE,ESTUSERF                 YES - GET EST USER FIELDS            
***                                                                             
* TEST STATION CHANGE                                                           
***                                                                             
         NI    FIRSTSW,255-FSTA                                                 
         CLC   SBBSTA,DCKSTA       TEST STATION CHANGE                          
         BE    ROM25B                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),DCKSTA                                                  
         GOTOR AMSUNPK,PARM,(X'80',DUB),WORK,WORK+4                             
         CLI   WORK+4,C'0'         TEST NEW AND OLD STATIONS ARE CABLE          
         BL    ROM25A                                                           
         CLI   SBSTA,C'0'                                                       
         BL    ROM25A                                                           
         ICM   RE,7,SBBSTA         YES-REMOVE NETWORK BITS FROM BOTH            
         SRL   RE,7                                                             
         SLL   RE,7                                                             
         ICM   RF,7,DCKSTA                                                      
         SRL   RF,7                                                             
         SLL   RF,7                                                             
         CR    RE,RF               TEST HEADEND CHANGE                          
         BE    *+8                 NO-NOT STATION FIRST                         
*                                                                               
ROM25A   OI    FIRSTSW,FSTA        STATION FIRST                                
*                                                                               
         MVC   SBBSTA,DCKSTA       SET STATION                                  
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO                                                            
         BNE   ROMNEQ                                                           
*                                                                               
ROM25B   CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   ROM30               NO                                           
         CLC   SBSTYPE,SBQSTATY    YES-CHECK STATION TYPE                       
         BE    ROM30               (ASSUMES HOOK SET TYPE)                      
         GOTOR AIO,IOSPTDIR+IOHID  RE-ESTABLISH READ SEQUENCE                   
         B     ROM10               READ SEQ                                     
***                                                                             
* TEST THE PIGGY                                                                
***                                                                             
ROM30    CLI   DCKPRD2,0           PIGGY?                                       
         BE    ROM30A              NO                                           
         TM    SBQPIND2,SBQPIGNO   YES- SUPPRESS PIGGYBACKS?                    
         BZ    ROM30B              NO, PROCEED                                  
         B     ROM10               YES, DON'T WANT THIS RECORD                  
*                                                                               
ROM30A   TM    SBQPIND,SBQPIGS     DID WE WANT PIGGIES?                         
         BO    ROM10               YES, DON'T WANT THIS RECORD                  
         CLI   SBQBPRD2,0          DO WE HAVE A PIGGY FILTER?                   
         BNE   ROM10               YES, DON'T WANT THIS RECORD                  
         B     ROM40                                                            
*                                                                               
ROM30B   CLI   SBQBPRD2,0          PIGGY FILTER?                                
         BE    ROM40               NO                                           
         CLC   SBQBPRD2,DCKPRD2    PIGGY MATCH?                                 
         BNE   ROM10               NO, DON'T WANT THIS RECORD                   
***                                                                             
* CHECK VALID FLIGHT                                                            
***                                                                             
ROM40    CLI   DCKFLTNM,0          HAVE A FLIGHT?                               
         BE    ROM50               NO                                           
         ICM   RE,15,SBAFLTAB      A(FLIGHT TABLE)                              
         LLC   R1,DCKEST           ESTIMATE                                     
         BCTR  R1,0                                                             
         MHI   R1,16                                                            
         AR    RE,R1               INDEX INTO EST ROW                           
         OC    0(255,RE),0(RE)     ANY FLIGHT VALID FOR THIS EST?               
         BNZ   ROM40A              YES                                          
         MVI   DCKFLTNM,X'FF'      NEXT RECORD                                  
         B     ROM05                                                            
*                                                                               
ROM40A   LLC   R1,DCKFLTNM         FLIGHT NUMBER                                
         BCTR  R1,0                                                             
         AR    RE,R1               INDEX INTO FLIGHT                            
         CLI   0(RE),1             FLIGHT VALID FOR REQ PERIOD?                 
         BNE   ROM10               NO, READ SEQ                                 
*                                                                               
ROM50    GOTOR AIO,IOSPTFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    ROM05                                                            
*                                                                               
         MVI   SBMODE,SBPROCOM     PROCESS ORDER MANAGER RECORDS                
         BRAS  RE,GO               HOOK TO USER                                 
         BNE   ROMNEQ                                                           
         GOTOR AIO,IOSPTDIR+IOHID  RE-ESTABLISH READ SEQUENCE                   
         B     ROM10               READ SEQ                                     
***                                                                             
* BUMP TO NEXT STATION                                                          
***                                                                             
ROM70    OC    QBSTA,QBSTA         NEXT STATION                                 
         BZ    ROM75                                                            
         CLI   DCKSTA,X'E8'        TEST CABLE                                   
         BNL   ROM75               YES - DO ALL STATIONS THIS MKT               
*                                                                               
         CLC   DCKSTA,QBMKTSTA+2      WANT TO BUMP TO A STATION?                
         BH    ROM80                  NO, BUMP TO NEXT ESTIMATE                 
*                                                                               
         MVC   DCKSTA,QBMKTSTA+2      BUMP TO THIS STATION                      
         XC    DCKPRD2(3),DCKPRD2     AND CLEAR EVERYTHING AFTER STA            
         B     ROM05                                                            
*                                                                               
ROM75    XR    R1,R1                                                            
         ICM   R1,7,DCKSTA                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,7,DCKSTA                                                      
         XC    DCKPRD2(3),DCKPRD2     AND CLEAR EVERYTHING AFTER STA            
         B     ROM05                                                            
***                                                                             
* BUMP TO NEXT ESTIMATE                                                         
***                                                                             
ROM80    CLC   DCKEST,SBQEST       EST IN KEY >= EST START?                     
         BNL   ROM85               YES                                          
         MVC   DCKEST,SBQEST       NO - MOVE IN START EST                       
         XC    DCKSTA(6),DCKSTA    CLEAR EVERYTHING AFTER                       
         B     ROM05               AND READHI                                   
*                                                                               
ROM85    CLC   DCKEST,SBQESTND     EST IN KEY > EST END?                        
         BH    ROM100              YES, BUMP TO NEXT PRODUCT                    
         MVC   DCKSTA(6),XFF       NO - BUMP TO NEXT ESTIMATE                   
         B     ROM05               AND READHI                                   
***                                                                             
* BUMP TO NEXT PRODUCT                                                          
***                                                                             
ROM100   CLI   DCKPRD,X'FF'        POL PRODUCT?                                 
         BE    ROMEQU              YES, WE ARE DONE                             
         CLI   SBQBPRD,0           SINGLE PRODUCT REQUEST?                      
         BNE   ROMEQU              YES, WE ARE DONE                             
         MVI   DCKEST,X'FF'                                                     
         MVC   DCKEST+1(6),DCKEST  BUMP TO NEXT PRODUCT                         
         B     ROM05                                                            
*                                                                               
ROMEQU   CR    RB,RB                                                            
         B     *+6                                                              
ROMNEQ   LTR   RB,RB                                                            
         J     EXIT                                                             
***********************************************************************         
* READ THE FLIGHT RECORDS FOR THIS A/M/CLT/PRD AND ALL POSSIBLE EST   *         
***********************************************************************         
READFLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,SBAFLTAB       CLEAR FLIGHT TABLE                          
         L     RF,=F'4080'          L' FLIGHT TABLE                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ICM   R3,15,SBAFLTAB                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING DFLKEY,R2            DARE FLIGHT KEY                             
         MVI   DFLKTYP,DFLKTYPQ     X'0D'                                       
         MVI   DFLKSUB,DFLKSUBQ     X'38'                                       
         MVC   DFLKAGMD,SBBAGYMD    A/M                                         
         MVC   DFLKCLT,SBBCLT       CLIENT                                      
         MVC   DFLKPRD,=C'POL'      PRODUCT POL                                 
         MVC   DFLKEST,SBQEST       ESTIMATE START                              
*                                                                               
         GOTOR AIO,IOSPTDIR+IOHID                                               
         B     RF10                                                             
*                                                                               
RF05     GOTOR AIO,IOSPTDIR+IOSQ                                                
*                                                                               
RF10     CLC   IOKEY(DFLKEST-DFLKEY),IOKEYSAV                                   
         BNE   RFX                                                              
         CLC   DFLKEST,SBQESTND     EST IN RECORD HIGHER THAN EST END?          
         BH    RFX                  YES - WE ARE DONE                           
*                                                                               
         GOTOR AIO,IOSPTFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    RF50                                                             
*                                                                               
         L     R2,SBAIO1                                                        
         LLC   R1,DFLKEST           EST NUM SO WE CAN INDEX INTO TABLE          
         BCTR  R1,0                 FLT ST AT 1 WHILE TABLE ST AT 0             
         MHI   R1,16                INDEX INTO ESTIMATE N ROW                   
         AR    R1,R3                                                            
*                                                                               
         LA    R6,DFLEL                                                         
         USING DFFLTEL,R6                                                       
*                                                                               
RF15     CLI   0(R6),0              DONE WITH THIS RECORD?                      
         BE    RF50                 YES                                         
         CLI   0(R6),DFFLTELQ       X'05' FLIGHT ELEMENT?                       
         BNE   RF20                 NO                                          
         CLC   DFFLTSTR,SBBQST      FLIGHT ST DT BEFORE REQ ST DT?              
         BL    RF20                 YES, IGNORE                                 
         CLC   DFFLTSTR,SBBQEND     FLIGHT ST DT AFTER REQ END DT?              
         BH    RF20                 YES, IGNORE                                 
         LLC   R5,DFFLTNUM          FLIGHT NUMBER                               
         BCTR  R5,0                                                             
         AR    R5,R1                INDEX INTO ESTIMATE COLUMN                  
         MVI   0(R5),1              INDICATE THIS FLIGHT IS VALID               
*                                                                               
RF20     LLC   R5,1(R6)             BUMP TO NEXT ELEMENT                        
         AR    R6,R5                                                            
         B     RF15                                                             
*                                                                               
RF50     LA    R2,IOKEY                                                         
         GOTOR AIO,IOSPTDIR+IOHID   RE-READ THE RECORD                          
         B     RF05                 AND READ NEXT FLIGHT RECORD                 
*                                                                               
RFX      J     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* READ FOR MARKET GROUP EQUIVALENCE REC                               *         
* ON ENTRY, R5 = A(SNVHDELD)                                          *         
*           SBAIO1 = INVOICE RECORD                                             
* ON EXIT, STAFILT(2) = EQV MARKET OR NULLS                           *         
***********************************************************************         
         USING SNVHDELD,R5                                                      
K        USING STERECD,IOKEY                                                    
*                                                                               
MGREQ    NTR1  BASE=*,LABEL=*                                                   
         L     RF,SBAIO1           SAVE KEY OF CURRENT INVOICE REC              
         MVC   SVIOKEY,0(RF)                                                    
         XC    IOKEY,IOKEY                                                      
         MVC   K.STEKTYP(2),=X'0D44'                                            
         MVC   K.STEKAGMD,SBBAGYMD                                              
         MVC   K.STEKCLT,SBBCLT                                                 
         XC    WORK,WORK                                                        
         L     RF,SBAIO1                                                        
         MVC   WORK+2(3),SNVKSTA-SNVKEY(RF)                                     
         GOTOR AMSUNPK,PARM,(X'80',WORK),WORK+4,WORK+8                          
         MVC   K.STEKSTA,WORK+8                                                 
         CLI   K.STEKSTA,C'0'      TEST CABLE STATION                           
         BNL   *+16                                                             
         CLI   K.STEKSTA+4,C' '                                                 
         BH    *+8                                                              
         MVI   K.STEKSTA+4,C'T'                                                 
*                                                                               
         L     R2,SBAIO2           CHECK IF WE ALREADY HAVE THIS REC            
         CLC   K.STEKEY,0(R2)                                                   
         BE    MGREQ20                                                          
*                                                                               
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   K.STEKEY,IOKEYSAV                                                
         BE    MGREQ10                                                          
         MVC   K.STEKEY,IOKEYSAV                                                
         MVC   K.STEKCLT,XFF       TRY ALL CLT                                  
*                                                                               
         CLC   K.STEKEY,0(R2)      CHECK IF WE ALREADY HAVE THIS REC            
         BE    MGREQ20                                                          
*                                                                               
         GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   K.STEKEY,IOKEYSAV                                                
         BNE   MGREQX                                                           
         DROP  K                                                                
*                                                                               
MGREQ10  GOTOR AIO,IOSPTFIL+IOGET+IO2                                           
*                                                                               
         USING STEEL03,R2                                                       
MGREQ20  AHI   R2,24               POINT AT FIRST EL                            
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         BRAS  RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
MGREQ30  BRAS  RE,NEXTEL                                                        
         BNE   MGREQX                                                           
         CLC   STEMGID,SNVHDCON                                                 
         BNE   MGREQ30                                                          
         PACK  WORK(3),SNVHDCON+1(5)                                            
         CLC   STEMGRP,WORK                                                     
         BNE   MGREQ30                                                          
         MVC   STAFILT(2),STEMGMKT                                              
*                                                                               
MGREQX   MVC   IOKEY,SVIOKEY                                                    
         GOTOR AIO,IOXSPDIR+IOHI                                                
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO GET A STATION RECORD                                           
* INPUT  : SVSTA=PACKED STATION                                                 
* OUTPUT : STATION RECORD IN AIO2                                               
*          SVMKT=STATION'S BINARY MARKET                                        
*          CC EQ - OK                                                           
*          CC NE - STATION NOT FOUND                                            
***********************************************************************         
GETSTREC NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(L'IOKEY+L'IOKEYSAV),IOKEY  SAVE KEY AND KEYSAVE             
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SVSTA                                                   
         GOTOR AMSUNPK,PARM,DUB,FULL,SBSTA                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         IC    R1,SBQMED                                                        
         CLI   SBQMED,C'*'                                                      
         BNE   *+8                                                              
         IC    R1,SBMED                                                         
         STC   R1,STAKMED                                                       
         MVC   STAKCALL,SBSTA                                                   
         CLI   STAKCALL+4,C'L'     T & X NORMALLY HAVE BAND OF T & X            
         BE    ST0                 BUT LEAVE BANDS D AND L ALONE                
         CLI   STAKCALL+4,C'D'     DIGITAL REQUEST?                             
         BE    ST0                 YES - LEAVE IT BE                            
         CLI   SBMED,C'R'                                                       
         BE    *+8                                                              
         STC   R1,STAKCALL+4                                                    
ST0      MVC   STAKAGY,SBAGY                                                    
         OC    SBSTAAGY,SBSTAAGY   USE ALT STA FILE?                            
         BZ    *+10                                                             
         MVC   STAKAGY,SBSTAAGY                                                 
         MVC   STAKCLT,SBCLT       TRY CLIENT SPECIFIC                          
         GOTOR AIO,IORD+IOSTAFIL+IO2                                            
         BE    ST2                                                              
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   STAKCLT,=C'000'     NO-REMOVE CLIENT                             
         BASR  RE,RF                                                            
         BE    ST2                                                              
         MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         J     NEQEXIT                                                          
*                                                                               
ST2      L     R2,AIO2                                                          
         PACK  DUB,SMKT            EXTRACT THE MARKET                           
         CVB   RE,DUB                                                           
         STCM  RE,3,SVMKT                                                       
*                                                                               
STX      MVC   IOKEY(L'IOKEY+L'IOKEYSAV),WORK  RESTORE KEYS                     
         J     EQEXIT                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO PROCESS OFFICE FILTERS                                         
*          CC EQ - PROCESS THIS CLIENT                                          
*          CC NE - SKIP THIS CLIENT                                             
* DUB USAGE:                                                                    
* DUB+0(2) = 2 CHAR OFFICE ON CLT REC                                           
*    +2(2) = 2 CHAR STARTING OFFICE                                             
*    +4(2) = 2 CHAR ENDING OFFICE                                               
*                                                                               
***********************************************************************         
         USING CLTHDRD,R2                                                       
O        USING OFFICED,WORK                                                     
*                                                                               
OFCFILT  NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB                                                          
         XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAUTH,SBTWAACS                                               
         MVC   O.OFCLMT,SBTWAACS                                                
         MVC   O.OFCAGY,SBAGY                                                   
         MVC   O.OFCOFC,COFFICE                                                 
         MVC   O.OFCCLT2,SBBCLT                                                 
         OI    O.OFCINDS,OFCI2CSC                                               
         L     RF,SBCOMFAC                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         USING MASTD,RF                                                         
         MVC   O.OFCSECD,MCASECRT                                               
         DROP  RF                                                               
         GOTOR VOFFICER,PARM,(C'2',WORK),(0,SBCOMFAC)                           
         TM    O.OFCINDS,OFCINOLA                                               
         BNZ   *+12                NO ERROR IF NO 2 CHAR OFFICES                
         CLI   0(R1),0                                                          
         BNE   OFCFNEQ                                                          
         MVC   DUB(2),O.OFCOFC2    SAVE OFF 2 CHARACTER OFFICE                  
*                                                                               
         CLI   SBQCLT+1,C'-'       TEST 'ALL BUT'                               
         BE    OFCF20                                                           
         MVC   O.OFCOFC,SBQCLT+1                                                
         XC    O.OFCOFC2,O.OFCOFC2                                              
         GOTOR VOFFICER,PARM,(C'2',WORK),(0,SBCOMFAC)                           
         TM    O.OFCINDS,OFCINOLA                                               
         BNZ   *+12                NO ERROR IF NO 2 CHAR OFFICES                
         CLI   0(R1),0                                                          
         BNE   OFCFNEQ                                                          
         MVC   DUB+2(2),O.OFCOFC2  SAVE OFF 2 CHARACTER OFFICE                  
*                                                                               
OFCF20   CLI   SBQCLT+2,0                                                       
         BE    OFCF30                                                           
         MVC   O.OFCOFC,SBQCLT+2                                                
         XC    O.OFCOFC2,O.OFCOFC2                                              
         GOTOR VOFFICER,PARM,(C'2',WORK),(0,SBCOMFAC)                           
         TM    O.OFCINDS,OFCINOLA                                               
         BNZ   *+12                NO ERROR IF NO 2 CHAR OFFICES                
         CLI   0(R1),0                                                          
         BNE   OFCFNEQ                                                          
         MVC   DUB+4(2),O.OFCOFC2  SAVE OFF 2 CHARACTER OFFICE                  
*                                                                               
OFCF30   CLI   SBQCLT+1,C'-'       TEST 'ALL BUT'                               
         BE    OFCF40                                                           
         CLC   DUB(2),DUB+2                                                     
         BE    OFCFEQ              OK                                           
         BL    OFCFNEQ                                                          
         CLI   SBQCLT+2,0          TEST RANGE                                   
         BNH   OFCFNEQ             NO                                           
         CLC   DUB(2),DUB+4                                                     
         BH    OFCFNEQ                                                          
         B     OFCFEQ              OK                                           
*                                                                               
OFCF40   CLC   DUB(2),DUB+4        'ALL BUT'                                    
         BE    OFCFNEQ                                                          
         B     OFCFEQ                                                           
*                                                                               
OFCFEQ   J     EQEXIT                                                           
OFCFNEQ  J     NEQEXIT                                                          
         EJECT                                                                  
*                                                                               
ESTUSERF NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,SBEHOOK                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         J     EXIT                                                             
*                                                                               
TSARHIGH NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(4),2(R4)                                                    
         L     R2,SBTSARBF                                                      
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         ST    R4,TSAREC                                                        
         GOTOR VTSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         BO    TSARHNEQ            YES, SET CC NOT EQU                          
         CLC   WORK(4),2(R4)       MATCH ON KEY?                                
         JE    EQEXIT              YES                                          
*                                                                               
TSARHNEQ LA    RE,TSARKEYL+2(R4)   START CLEARING STA BUFFER HERE               
         LA    RF,SBUFMAX          CLEAR IT FOR THIS MANY BYTES                 
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         J     NEQEXIT                                                          
*                                                                               
TSARADD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,SBTSARBF                                                      
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAADD     SET TO ADD A RECORD                          
         ST    R4,TSAREC           A(VARIABLE LENGTH RECORD)                    
         GOTOR VTSAROFF,(R2)       ADD THE RECORD                               
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    EXIT                NO                                           
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         JE    EXIT                YES, IGNORE IT                               
         DC    H'0'                                                             
***********************************************************************         
* PUT VARIABLE LENGTH ELEMENT INTO THE STATION BUFFER                 *         
* PARMS  R4     = A(STA BUFFER)                                       *         
*        R5     = A(DATA) TO BE MOVED TO ELEMENT                      *         
*        HALF   = ELEMENT CODE                                        *         
*        HALF+1 = MAX ELEMENT LENGTH                                  *         
***********************************************************************         
PUTELEM  NTR1  BASE=*,LABEL=*                                                   
         USING STABUFFD,R4                                                      
         XR    RF,RF                                                            
         LA    R1,STBELEM          START OF ELEMENTS IN STA BUFFER              
*                                                                               
PELEM10  CLI   0(R1),0             END OF STATION BUFFER?                       
         BE    PELEM20             YES                                          
         IC    RF,1(R1)            LENGTH OF ELEMENT                            
         AR    R1,RF               BUMP PAST THIS ELEMENT                       
         B     PELEM10             AND CHECK FOR END OF STATION BUFFER          
*                                                                               
PELEM20  MVC   0(1,R1),HALF        ELEMENT CODE                                 
         IC    RF,HALF+1           MAX LENGTH OF ELEMENT                        
         BCTR  RF,0                -1                                           
         LA    R2,0(RF,R5)         R2 = LAST CHAR OF DATA                       
         IC    RF,HALF+1           MAX LENGTH OF ELEMENT                        
         LA    RE,MSFREQ                                                        
         CR    RE,R5               TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES - SAVE MSFREQ,MSSTATE & MSMOWNER         
         CLI   HALF,STADD3         TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES                                          
         CLI   HALF,SMADD3         TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES                                          
*                                                                               
PELEM25  CLI   0(R2),X'40'         HAVE ANY DATA                                
         BH    PELEM30             YES                                          
         BCTR  R2,0                DECREMENT DATA POINTER                       
         BCT   RF,PELEM25          DECREMENT DATA LENGTH POINTER                
         DC    H'0'                POINTING TO STORAGE BEFORE DATA              
*                                                                               
PELEM30  AHI   RF,2                DATA + ELEM CODE + ELEM LENGTH               
         STC   RF,1(R1)            STORE ELEMENT LENGTH                         
         SHI   RF,3                - ELEM CODE - ELEM LEN - 1 FOR EX            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),0(R5)       ** EXECUTED **                               
*                                                                               
PEXIT    J     EXIT                                                             
*                                                                               
RDBILL1  NTR1  BASE=*,LABEL=*                                                   
         CLI   SBQMED,C'C'         TEST MEDIA = COMBINED                        
         BNE   *+12                                                             
         MVI   MEDCMED,C'T'        YES-PROCESS MEDIA T FIRST                    
         MVI   SBMED,C'T'                                                       
         CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK                        
         BNE   RS6                                                              
         LA    R0,CNTWKTAB         YES-BUILD CANADIAN NETWORK SEQUENCE          
         LA    R1,128*4            CLEAR TABLE                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    IOKEY,IOKEY             NUMBERS TABLE                            
         LA    R2,IOKEY                                                         
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'   READ NETWORK DEFINITION RECORDS              
         MVC   NDEFKAGY,SBAGY                                                   
*                                                                               
RS2      LA    R2,IOKEY                                                         
         MVC   NDEFKCLT(5),XFF                                                  
*                                                                               
RS4      GOTOR AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(NDEFKNET-NDEFRECD),IOKEYSAV                                
         BNE   RS6                                                              
         GOTOR (RF),IOSPTFIL+IOGET+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R4,NDEFEL                                                        
         SR    R0,R0               FIND NETWORK DEFINITION ELEMENT              
*                                                                               
RS5      CLI   0(R4),0                                                          
         BE    RS2                 IGNORE IF ELEMENT MISSING                    
         CLI   0(R4),2                                                          
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RS5                                                              
         LLC   RF,2(R4)            GET NETWORK BITS                             
         BCTR  RF,0                                                             
         SLL   RF,2                X4                                           
         LA    RF,CNTWKTAB(RF)     POINT TO SLOT                                
         MVC   0(4,RF),NDEFKNET    MOVE IN THE NETWORK                          
         B     RS2                                                              
*                                                                               
RS6      J     EXIT                                                             
***********************************************************************         
* READ AND BUFFER ALL BPCT RECORDS FOR THIS CLIENT                              
***********************************************************************         
BPCT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SBQREAD2,SBQRD2BP   READ BPCT RECORDS?                           
         BZ    BPX                 NOPE                                         
         ICM   R3,15,SBACHKTB      HAVE A(CHECK TABLE)                          
         BZ    BPX                 NOPE                                         
         XC    4(4,R3),4(R3)       N'RECORDS IN TABLE SO = 0                    
         XR    R4,R4               R4=ENTRY COUNT                               
         L     R5,8(R3)            R5=MAX N'ENTRIES                             
         LA    R3,12(R3)           R3=START OF TABLE                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING BPCRECD,R2                                                       
         MVI   BPCKTYP,BPCKTYQQ    X'0E'                                        
         MVI   BPCKSUB,BPCKSUBQ    X'0D'                                        
         MVC   BPCKAM,SBBAGYMD     A/M                                          
         MVC   BPCKCLT,SBBCLT      CLIENT                                       
*                                                                               
         GOTOR AIO,IOXSPDIR+IOHI   READ HIGH                                    
         B     BP00B                                                            
*                                                                               
BP00A    GOTOR AIO,IOXSPDIR+IOSQD  READ SEQ                                     
*                                                                               
BP00B    CLC   IOKEY(BPCKPRD-BPCKEY),IOKEYSAV                                   
         BNE   BP00H                                                            
*                                                                               
         GOTOR AIO,IOXSPFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1             BPCT RECORD                                  
         LA    R6,BPCEL-BPCKEY(R6) A(FIRST ELEMENT)                             
*                                                                               
BP00C    CLI   0(R6),0             END OF RECORD?                               
         BE    BP00A               YES, READ SEQ                                
         CLI   0(R6),PCTELQ        FOUND X'10' ELEMENT?                         
         BE    BP00E               YES                                          
BP00D    LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     BP00C                                                            
*                                                                               
         USING PCTELEM,R6                                                       
BP00E    XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING BPCTTABD,R1                                                      
         MVC   BPCTAM,BPCKAM       A/M                                          
         MVC   BPCTCLT,BPCKCLT     CLIENT                                       
***                                                                             
* UGH - FIND BINARY PRD FROM 3 CHAR PRD                                         
***                                                                             
         L     RF,ACLTREC          FIND PRODUCT IN CLTHDR                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
BP00F    CLC   BPCKPRD,0(RF)       MATCH ON 3 CHAR PRD?                         
         BE    BP00G               YES                                          
         LA    RF,4(RF)            NO - BUMP TO NEXT PRD IN CLIST               
         CLI   0(RF),C' '          END OF CLIST?                                
         BH    BP00F               NO                                           
         DC    H'0'                YES - INVALID PRD IN RECORD                  
*                                                                               
BP00G    MVC   BPCTPRD,3(RF)       PRODUCT                                      
         MVC   BPCTEST,BPCKEST     ESTIMATE                                     
         MVC   BPCTMON,PCTMONTH                                                 
         NI    PCTPCT,X'FF'-X'80'  TURN OFF ZERO FLAG                           
         MVC   BPCTPCT,PCTPCT      PERCENTAGE TO 1 DECIMAL PLACE                
         DROP  R1,R2,R6                                                         
*                                                                               
         GOTOR SBABINSR,PARM,(1,WORK),(R3),(R4),BPCTTABL,(0,7),(R5)             
         OC    1(3,R1),1(R1)       TABLE FULL?                                  
         BNZ   *+6                 NO                                           
         DC    H'0'                                                             
         L     R4,8(R1)            NUMBER OF RECS (UPDATED BY BINSRCH)          
         B     BP00D               NEXT ELEMENT                                 
*                                                                               
BP00H    ICM   R3,15,SBACHKTB      A(CHECK TABLE)                               
         ST    R4,4(R3)                                                         
*                                                                               
BPX      J     EXIT                                                             
*                                                                               
ESTFILT  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDRD,R2          R2 = ESTIMATE HEADER                         
         LA    R1,3                3 FILTER VALUES TO LOOP THROUGH              
         LA    RE,SBQESFLT         REQUESTED ESTIMATE FILTERS                   
         LA    RF,EPROF            ESTIMATE FILTERS ON ESTIMATE REC             
*                                                                               
ESTF10   CLI   0(RE),C'*'          FILTER = *?                                  
         JE    EQEXIT              YES - ACCEPT ANYTHING SO WE PASSED           
         TM    0(RE),X'40'         NEGATIVE FILTER?                             
         JZ    EQEXIT              YES - WE TEST THIS LATER                     
         CLI   0(RE),C' '          SPACE?                                       
         JE    ESTF20              YES - IGNORE                                 
         CLC   0(1,RE),0(RF)       FILTER MATCHES EST FILTER?                   
         JE    EQEXIT              YES - WE HAVE A MATCH                        
         CLC   0(1,RE),1(RF)       FILTER MATCHES EST FILTER+1?                 
         JE    EQEXIT              YES - WE HAVE A MATCH                        
         CLC   0(1,RE),2(RF)       FILTER MATCHES EST FILTER+2?                 
         JE    EQEXIT              YES - WE HAVE A MATCH                        
*                                                                               
ESTF20   LA    RE,1(RE)            BUMP TO NEXT EST FILTER                      
         BCT   R1,ESTF10           LOOP BACK AND CHECK THIS FILTER              
         J     NEQEXIT             NO MATCH FOUND - FILTER EST OUT!             
*                                                                               
ESTFILTN NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDRD,R2          R2 = ESTIMATE HEADER                         
         LA    R1,3                3 FILTER VALUES TO LOOP THROUGH              
         LA    RE,SBQESFLT         REQUESTED ESTIMATE FILTERS                   
         LA    RF,EPROF            ESTIMATE FILTERS ON ESTIMATE REC             
*                                                                               
ESTFN10  CLI   0(RE),C'*'          FILTER = *?                                  
         JE    EQEXIT              YES - ACCEPT ANYTHING SO WE PASSED           
         CLI   0(RE),C' '          SPACE?                                       
         JE    ESTFN20             YES - IGNORE                                 
         TM    0(RE),X'40'         NEGATIVE FILTER?                             
         BNZ   ESTFN20             NO                                           
*                                                                               
         MVC   BYTE,0(RE)          MOVE NEGATIVE FILTER TO BYTE                 
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEG FILTER MATCHES EST FILTER?               
         JE    NEQEXIT             NO - THIS ESTIMATE PASSED                    
         CLC   BYTE,1(RF)          NEG FILTER MATCHES EST FILTER+1?             
         JE    NEQEXIT             NO - THIS ESTIMATE PASSED                    
         CLC   BYTE,2(RF)          NEG FILTER MATCHES EST FILTER+2?             
         JE    NEQEXIT             NO - THIS ESTIMATE PASSED                    
*                                                                               
ESTFN20  LA    RE,1(RE)            BUMP TO NEXT EST FILTER                      
         BCT   R1,ESTFN10          LOOP BACK AND CHECK THIS FILTER              
         J     EQEXIT              NO NEG MATCH FOUND - ESTIMATE OK!            
*                                                                               
         EJECT                                                                  
* READ STATION LOCKIN RECORDS                                                   
*                                                                               
READSLK  NTR1  BASE=*,LABEL=*                                                   
         MVI   SBBPRD,0                                                         
         MVI   SBBPRD2,0                                                        
         MVI   SBBEST,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    SVMKTSTA,SVMKTSTA                                                
         XC    IOKEY,IOKEY                                                      
*                                                                               
         LA    R2,IOKEY            STATION LOCKIN HEADER KEY                    
         USING SLKRECD,R2                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SBBAGYMD                                                
         MVC   SLKKCLT,SBBCLT                                                   
         LA    R4,SLKKMKT-SLKKEY-1                                              
         OC    QBMKT,QBMKT                                                      
         BZ    SL2                                                              
         MVC   SLKKMKT,QBMKT                                                    
         CLC   QBMKTST,QBMKTND                                                  
         BNE   SL2                                                              
         LA    R4,L'SLKKMKT(R4)                                                 
         OC    SBQSTA,SBQSTA                                                    
         BZ    SL2                                                              
         CLC   =C'ALL',SBQSTA                                                   
         BE    SL2                                                              
         MVC   SLKKSTA,QBSTA                                                    
         LA    R4,L'SLKKSTA(R4)                                                 
         CLI   SBQBPRD,0                                                        
         BE    SL2                                                              
         CLI   SBQBPRD,FF                                                       
         BE    SL2                                                              
         MVC   SLKKPRD,SBQBPRD                                                  
         LA    R4,L'SLKKPRD(R4)                                                 
         CLI   SBQBPRD2,0                                                       
         BE    SL2                                                              
         MVC   SLKKPRD2,SBQBPRD2                                                
         LA    R4,L'SLKKPRD2(R4)                                                
         MVC   SLKKEST,SBQEST                                                   
         CLC   SBQEST,SBQESTND                                                  
         BNE   SL2                                                              
         LA    R4,L'SLKKEST(R4)                                                 
*                                                                               
SL2      GOTOR AIO,IOXSPDIR+IOHID                                               
*                                                                               
SL4      EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   SLX                                                              
         MVC   SBRECDA,IODA                                                     
*                                                                               
         TM    SLKKIND,SLKKIPSV    SKIP PASSIVES                                
         BO    SL62                                                             
*                                                                               
         MVC   SVMKT,SLKKMKT       SAVE THE MARKET                              
         CLC   SBQMGRF,BLANKS      TEST MARKET GROUP FILTER                     
         BNH   SL6                                                              
         OC    SBPGRPEX(2),SBPGRPEX  AND NO PRODUCT GROUP EXCEPTION             
         BNZ   SL6                                                              
         GOTOR ACHKMGRP            YES-CHECK THE MARKET                         
         BE    SL6                                                              
         MVC   SLKKSTA(6),XFF      MKT NOT IN MKTGRP - READ NEXT MKT            
         B     SL2                                                              
*                                                                               
SL6      CLC   SVSTA,SLKKSTA       TEST STATION CHANGE                          
         BE    SL7                                                              
         MVC   SVSTA,SLKKSTA       YES-GET STATION DETAILS                      
         GOTOR AGETSTA                                                          
         BNE   SL8                                                              
*                                                                               
         MVC   STAFILT,SLKKMKT                                                  
         GOTOR ACBLFILT                                                         
         BNE   SL8                 SKIP TO NEXT STATION                         
*                                                                               
SL7      GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BE    SL10                                                             
*                                                                               
SL8      MVC   SLKKPRD,XFF         SKIP TO NEXT STATION                         
         B     SL2                                                              
*                                                                               
SL10     CLC   SBQMGRF,BLANKS      TEST MARKET GROUP FILTER                     
         BNH   SL12                                                             
         OC    SBPGRPEX(2),SBPGRPEX  AND PRODUCT GROUP EXCEPTION                
         BZ    SL12                                                             
         MVC   SVPRD,SLKKPRD       YES-CHECK FOR VALID MKTGRP                   
         GOTOR ACHKMGRP                                                         
         BNE   SL34                MKT NOT IN MKTGRP - READ NEXT PRD            
*                                                                               
SL12     CLC   SBQPGRF,BLANKS      TEST PRODUCT GROUP FILTER                    
         BNH   SL14                                                             
         LLC   RE,SLKKPRD          YES-CHECK PRODUCT'S IN GROUP                 
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    SL34                SKIP TO NEXT PRODUCT                         
         DROP  R1                                                               
*                                                                               
SL14     ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   SL16                YES                                          
         LLC   RE,SLKKEST                                                       
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE ESTIMATE                         
         BNE   SL18                YES                                          
         B     SL32                NO-SKIP TO NEXT ESTIMATE                     
*                                                                               
SL16     LLC   RE,SLKKPRD          PRODUCT                                      
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    RE,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   R0,SLKKEST          ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    SL32                NO-SKIP TO NEXT ESTIMATE                     
*                                                                               
SL18     ICM   R5,15,SBAESTBF      GET ESTIMATE DETAILS                         
         BNZ   *+14                                                             
         MVC   SBDPTMEN,0(RE)                                                   
         B     SL20                                                             
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R5,RF                                                            
         USING ESTBUFFD,R5                                                      
         MVC   SBDPTMEN,EBDPTMEN   SET DAYPART MENU                             
         MVC   SBESTFLT,EBFILT     SET ESTIMATE FILTERS                         
         MVC   SBRTLSCH,EBRTLSCH   SET RETAIL SCHEME CODE                       
         GOTOR VRCPACK,PARM,(C'U',EBSREP),SBSREP                                
         DROP  R5                                                               
*                                                                               
SL20     CLI   SBQDPT,0            TEST DAYPART FILTER                          
         BE    SL22                                                             
         GOTOR AFILTDPT,SLKKDPT    YES                                          
         BE    SL24                                                             
         MVC   SLKKLEN(3),XFF                                                   
         B     SL2                                                              
*                                                                               
SL22     CLI   SBQMASDP,0          TEST MASTER DAYPART FILTER                   
         BE    SL24                                                             
         LA    R1,SLKKDPT          YES-                                         
         GOTOR ACHKMDPT                                                         
         BNE   SL30                DAYPART REJECTED - NEXT DAYPART              
*                                                                               
SL24     CLI   SBQLEN,0            TEST LENGTH FILTER                           
         BE    SL40                                                             
         LLC   RE,SLKKLEN                                                       
         LLC   RF,SLKKLEN2                                                      
         AR    RE,RF                                                            
         CLM   RE,1,SBQLEN                                                      
         BNE   SL60                NOT EQUAL - READ NEXT RECORD                 
         B     SL40                                                             
*                                                                               
SL30     MVC   SLKKLEN(3),XFF      NEXT DAYPART                                 
         B     SL2                                                              
*                                                                               
SL32     MVC   SLKKDPT(4),XFF      NEXT ESTIMATE                                
         B     SL2                                                              
*                                                                               
SL34     MVC   SLKKPRD2(6),XFF     NEXT PRODUCT                                 
         B     SL2                                                              
*                                                                               
*                                                                               
SL40     CLC   SBBPRD,SLKKPRD      TEST PRODUCT CHANGE                          
         BE    SL46                                                             
         OI    FLAG2,FLGETPW                                                    
         MVC   SBBPRD,SLKKPRD                                                   
         L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
SL42     CLC   SBBPRD,3(RF)                                                     
         BE    SL44                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    SL42                                                             
         B     SL34                PRD NOT IN CLTHDR - READ NEXT PRD            
*                                                                               
SL44     MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    SL46                                                             
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         OC    SBBPGR,SBBPGR                                                    
         BNZ   SL46                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
SL46     CLC   SBBEST,SLKKEST      TEST ESTIMATE CHANGE                         
         BE    SL47                                                             
         OI    FLAG2,FLGETPW                                                    
         MVC   SBBEST,SLKKEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
SL47     CLI   SBQMGR,0            TEST  MARKET GROUPS                          
         BE    SL48                                                             
         CLC   SBBMKT,SVMKT        YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    SL48                                                             
         MVC   SVPRD,SLKKPRD                                                    
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
SL48     CLC   SBBMKT,SVMKT        TEST MARKET CHANGE                           
         BE    SL50                                                             
         OI    FLAG2,FLGETPW                                                    
         MVC   SBBMKT,SVMKT        YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   SL50                                                             
         MVI   SBMODE,SBPROCMK     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
SL50     CLC   SBBSTA,SVSTA        TEST STATION CHANGE                          
         BE    SL54                                                             
         CLI   CNETWK,C'Y'         YES-TEST CANADIAN NETWORK REQUEST            
         BNE   SL52                                                             
         CLC   SBBSTA(2),SVSTA                                                  
         BE    SL54                                                             
*                                                                               
SL52     MVC   SBBSTA,SVSTA        SET STATION                                  
         GOTOR AMSUNPK,PARM,(X'80',SVMKTSTA),WORK,WORK+4                        
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
SL54     GOTOR AIO,IOXSPFIL+IOGET+IO1   GET THE LOCKIN RECORD                   
*                                                                               
         TM    SBEFLAG,SBEWIPW     SKIP UNLESS EXTRACTING PW DATA               
         BZ    SL56                                                             
         TM    FLAG2,FLGETPW       KEY CHANGE?                                  
         BZ    SL56                 NO                                          
*                                                                               
* BUILD PW KEY IF NEEDED                                                        
         XC    SVKEY,SVKEY                                                      
         LA    RF,SVKEY                                                         
         USING PWRECD,RF                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,SBBAGYMD    AGY/MD                                       
         MVC   PWKCLT,SBBCLT       CLIENT                                       
         MVC   PWKPRD,SBBPRD       PRODUCT                                      
         MVC   PWKEST,SBBEST       ESTIMATE                                     
         MVC   PWKMKT,SBBMKT       MARKET                                       
         DROP  RF                                                               
*                                                                               
         OI    FLAG2,SKIPSTA       DON'T READ PW STA REC                        
         GOTOR AGETPWM                                                          
*                                                                               
SL56     MVC   SBDPTCD,SLKKDPT     SET DAYPART CODE                             
*                                                                               
         MVI   SBMODE,SBPROCSL     HOOK TO USER                                 
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
SL60     GOTOR AIO,IOXSPDIR+IOHID  READ THE NEXT LOCKIN RECORD                  
*                                                                               
SL62     GOTOR AIO,IOXSPDIR+IOSQD                                               
         B     SL4                                                              
*                                                                               
SLX      J     EQEXIT                                                           
         EJECT                                                                  
*                                                                               
READPW   NTR1  BASE=*,LABEL=*                                                   
         MVI   SBBEST,0                                                         
         MVI   SBBPRD,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SVSTA,SVSTA                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PWRECD,R2                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,SBBAGYMD                                                 
         MVC   PWKCLT,SBBCLT                                                    
         LA    R4,PWKPRD-PWFKEY-1                                               
         CLI   SBQBPRD,0                                                        
         BE    PW10                                                             
         CLI   SBQBPRD,FF                                                       
         BE    PW10                                                             
         MVC   PWKPRD,SBQBPRD                                                   
         LA    R4,L'PWKPRD(R4)                                                  
         CLC   SBQEST,SBQESTND                                                  
         BNE   PW10                                                             
         MVC   PWKEST,SBQEST                                                    
         LA    R4,L'PWKEST(R4)                                                  
         MVC   PWKMKT,QBMKT                                                     
         OC    QBMKT,QBMKT                                                      
         BZ    PW10                                                             
         CLC   QBMKTST,QBMKTND                                                  
         BNE   PW10               MARKET RANGE                                  
         LA    R4,L'PWKMKT(R4)                                                  
*                                                                               
PW10     LA    R2,IOKEY                                                         
         GOTOR AIO,IOSPTDIR+IOHI                                                
*                                                                               
PW15     MVC   SBRECDA,IODA        SAVE DISK ADDRESS                            
         EX    R4,*+8              R4 = KEY COMPARE LENGTH - 1                  
         B     *+10                                                             
         CLC   PWFKEY(0),IOKEYSAV                                               
         BNE   PWX                                                              
*                                                                               
         TM    SBIOFLAG,SBRDPWS    DON'T SKIP STATION LEVEL RECS                
         BNZ   *+14                                                             
         OC    PWKSTA,PWKSTA       SKIP STATION LEVEL RECS                      
         BNZ   PW45                GET NEXT MKT                                 
*                                                                               
         CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BE    PW18                                                             
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BE    PW20                YES - RETURN ALL PRODUCTS                    
         CLC   SBQBPRD,PWKPRD      NO - FILTER ON PRODUCT                       
         BE    PW20                                                             
         BL    PWX                                                              
         MVC   PWKPRD,SBQBPRD      SKIP TO REQ PRODUCT                          
         XC    PWKEST(7),PWKEST                                                 
         B     PW10                                                             
*                                                                               
PW18     CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   PW20                                                             
         LLC   RE,PWKPRD           FIND PRDGRP                                  
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    PW55                                                             
         DROP  R1                                                               
*                                                                               
PW20     ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   PW22                YES                                          
         LLC   RE,PWKEST                                                        
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE ESTIMATE                         
         BNE   PW30                YES                                          
         LR    R1,RE               NO-SKIP TO NEXT ACTIVE ESTIMATE              
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         LLC   R3,PWKEST                                                        
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     PW55                NEXT PRODUCT                                 
         STC   R3,PWKEST           NEXT ACTIVE ESTIMATE                         
         MVC   PWKMKT,QBMKT                                                     
         MVC   PWKSTA,QBSTA                                                     
         B     PW10                YES                                          
*                                                                               
PW22     LLC   RE,PWKPRD           PRODUCT                                      
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    RE,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   R0,PWKEST           ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    PW50                NO                                           
*                                                                               
PW30     OC    QBMKTST(4),QBMKTST  TEST MARKET START/END SPECIFIED              
         BNZ   PW31                YES                                          
         OC    QBMKT,QBMKT         TEST REQUEST FOR MKT 0 (CAN NTWK)            
         BZ    PW32                YES - DO NOT TEST MARKET                     
PW31     CLC   PWKMKT,QBMKTST      TEST MARKET WITHIN RANGE                     
         BL    *+18                                                             
         CLC   PWKMKT,QBMKTND                                                   
         BH    PW50                DONE WITH MARKET - NEXT ESTIMATE             
         B     PW32                                                             
         MVC   PWKMKT,QBMKTST      SKIP TO MARKET START                         
         MVC   PWKSTA,QBSTA                                                     
         B     PW10                                                             
*                                                                               
PW32     CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP FILTERING              
         BNH   PW60                NO                                           
         MVC   SVMKT,PWKMKT                                                     
         MVC   SVPRD,PWKPRD                                                     
         GOTOR ACHKMGRP                                                         
         BE    PW60                                                             
*                                                                               
PW45     MVC   PWKSTA(4),XFF       NEXT MARKET                                  
         B     PW10                                                             
*                                                                               
PW50     MVC   PWKMKT(6),XFF       NEXT ESTIMATE                                
         ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   PW10                YES                                          
         LLC   R1,PWKEST           NO-FIND NEXT ACTIVE ESTIMATE                 
         LA    R1,1(R1)                                                         
         LR    R3,R1                                                            
         LA    R1,ESTTAB(R1)                                                    
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     PW55                                                             
         STC   R3,PWKEST                                                        
         MVC   PWKMKT,QBMKT                                                     
         MVC   PWKSTA,QBSTA                                                     
         B     PW10                                                             
*                                                                               
PW55     CLI   SBQBPRD,0           NEXT PRODUCT                                 
         BE    *+12                                                             
         CLI   SBQBPRD,FF                                                       
         BNE   PWX                                                              
         MVC   PWKEST(7),XFF                                                    
         B     PW10                                                             
*                                                                               
PW60     CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   PW61                NO                                           
         CLI   SBEBYPRD,C'Y'       YES-TEST TO SPLIT OUT THE PRODUCTS           
         BE    PW61                YES                                          
         CLI   SBBPRD,FF           NO-FORCE PRODUCT=POL                         
         BE    PW65                                                             
         MVI   SBBPRD,FF                                                        
         B     PW62                                                             
*                                                                               
PW61     CLC   PWKPRD,SBBPRD       TEST PRODUCT CHANGE                          
         BE    PW65                                                             
         MVC   SBBPRD,PWKPRD                                                    
*                                                                               
PW62     L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
PW63     CLC   SBBPRD,3(RF)                                                     
         BE    PW64                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    PW63                                                             
         B     PW55                PER TSMY                                     
*                                                                               
PW64     MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    PW65                                                             
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         MVC   SBPRDINT,PBINT-PRDBUFFD(R1)                                      
         MVC   SBUP1FLD,PBUFLD1-PRDBUFFD(R1)                                    
         MVC   SBUP2FLD,PBUFLD2-PRDBUFFD(R1)                                    
         OI    SBEUDEF,SBEUPCOM    TELL CALLER TO GET PRD UCOMS                 
         OC    SBBPGR,SBBPGR                                                    
         BNZ   *+10                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
PW65     CLC   SBBEST,PWKEST       TEST ESTIMATE CHANGE                         
         BE    PW66                                                             
         MVC   SBBEST,PWKEST                                                    
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
PW66     CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    PW68                                                             
         CLC   SBBMKT,PWKMKT       YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    PW68                                                             
         MVC   SVMKT,PWKMKT        YES-SET THE MARKET GROUP                     
         MVC   SVPRD,PWKPRD                                                     
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
PW68     CLC   SBBMKT,PWKMKT       TEST MARKET CHANGE                           
         BE    PW80                                                             
         MVC   SBBMKT,PWKMKT       YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   PW80                                                             
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
PW80     MVC   IODA,SBRECDA             GET THE PW RECORD                       
         GOTOR AIO,IOSPTFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    PW90                                                             
*                                                                               
* HOOK TO USER                                                                  
*                                                                               
         MVI   SBMODE,SBPROCWP                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
PW90     LA    R2,IOKEY                                                         
         GOTOR AIO,IOSPTDIR+IOHI   REESTABLISH READ SEQUENCE                    
         GOTOR AIO,IOSPTDIR+IOSQ   READ NEXT PW RECORD                          
         B     PW15                                                             
*                                                                               
PWX      J     EQEXIT                                                           
*                                                                               
         EJECT                                                                  
READMSR  NTR1  BASE=*,LABEL=*                                                   
         XC    INVST(4),INVST                                                   
         XC    INVENMOS,INVENMOS                                                
         MVC   INVSTMOS,XFF                                                     
         OC    SBNDATES,SBNDATES   TEST DATES PASSED                            
         BZ    MSR1                                                             
*                                                                               
         ICM   R2,15,SBADATE                                                    
         BZ    MSR1                                                             
         GOTOR CDATCON,PARM,(2,0(R2)),(0,DUB)  YES-GET BRDCST MNTH STRT         
         GOTOR VGETBROD,(R1),(1,DUB),WORK,CGETDAY,CADDAY  OF START DATE         
         GOTOR CADDAY,(R1),WORK,WORK,7   (DATE+7 WILL BE IN RIGHT MON)          
         MVC   WORK+4(2),=C'01'          1ST OF MONTH                           
         GOTOR CDATCON,(R1),(0,WORK),(2,DUB)                                    
         MVC   INVSTMOS,DUB                                                     
         XC    INVSTMOS,XFF          COMPLEMENT                                 
*                                                                               
         L     R1,SBNDATES                                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R2,R1                                                            
         GOTOR CDATCON,PARM,(2,2(R2)),(0,DUB)  GET BROADCAST MONTH STRT         
         GOTOR VGETBROD,(R1),(1,DUB),WORK,CGETDAY,CADDAY  OF END DATE           
         GOTOR CADDAY,(R1),WORK,WORK,7   (DATE+7 WILL BE IN RIGHT MON)          
         MVC   WORK+4(2),=C'01'         1ST OF MONTH                            
         GOTOR CDATCON,(R1),(0,WORK),(2,DUB)                                    
         MVC   INVENMOS,DUB                                                     
         XC    INVENMOS,XFF          COMPLEMENT                                 
*                                                                               
MSR1     MVI   SBBEST,0                                                         
         MVI   SBBPRD,0                                                         
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    SVSTA,SVSTA                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING MSRKEYD,R2                                                       
         MVI   MSRKTYPE,MSRKTYPQ                                                
         MVI   MSRKSUB,MSRKSUBQ                                                 
         MVC   MSRKAM,SBBAGYMD                                                  
         MVC   MSRKCLT,SBBCLT                                                   
         LA    R4,MSRKPRD-MSRKEY-1                                              
         CLI   SBQBPRD,0                                                        
         BE    MSR10                                                            
         CLI   SBQBPRD,FF                                                       
         BE    MSR10                                                            
         MVC   MSRKPRD,SBQBPRD                                                  
         LA    R4,L'MSRKPRD+L'MSRKPRD2(R4)                                      
         CLC   SBQEST,SBQESTND                                                  
         BNE   MSR10                                                            
         MVC   MSRKEST,SBQEST                                                   
         LA    R4,L'MSRKEST(R4)                                                 
         MVC   MSRKMKT,QBMKT                                                    
         OC    QBMKT,QBMKT                                                      
         BZ    MSR10                                                            
         CLC   QBMKTST,QBMKTND                                                  
         BNE   MSR10              MARKET RANGE                                  
         LA    R4,L'MSRKMKT(R4)                                                 
         OC    SBQSTA,SBQSTA                                                    
         BZ    MSR10                                                            
         CLC   =C'ALL',SBQSTA                                                   
         BE    MSR10                                                            
         OC    QBSTACAN,QBSTACAN                                                
         BNZ   MSR10                                                            
         MVC   MSRKSTA,QBSTA                                                    
         LA    R4,L'MSRKSTA(R4)                                                 
         MVC   MSRKMOS,INVENMOS                                                 
*                                                                               
MSR10    LA    R2,IOKEY                                                         
         GOTOR AIO,IOXSPDIR+IOHI                                                
*                                                                               
MSR15    MVC   SBRECDA,IODA        SAVE DISK ADDRESS                            
         EX    R4,*+8              R4 = KEY COMPARE LENGTH - 1                  
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   MSRX                                                             
*                                                                               
MSR16    CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BE    MSR18                                                            
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BE    MSR20               YES - RETURN ALL PRODUCTS                    
         CLC   SBQBPRD,MSRKPRD     NO - FILTER ON PRODUCT                       
         BE    MSR20                                                            
         BL    MSRX                                                             
         MVC   MSRKPRD,SBQBPRD     SKIP TO REQ PRODUCT                          
         XC    MSRKPRD2(MSRDSTAT-MSRKPRD2),MSRKPRD2                             
         B     MSR10                                                            
*                                                                               
MSR18    CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   MSR20                                                            
         LLC   RE,MSRKPRD          FIND PRDGRP                                  
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP                                                  
         BZ    MSR55                                                            
         DROP  R1                                                               
*                                                                               
MSR20    ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   MSR22               YES                                          
         LLC   RE,MSRKEST                                                       
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ACTIVE ESTIMATE                         
         BNE   MSR30               YES                                          
         LR    R1,RE               NO-SKIP TO NEXT ACTIVE ESTIMATE              
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         LLC   R3,MSRKEST                                                       
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     MSR55               NEXT PRODUCT                                 
         STC   R3,MSRKEST          NEXT ACTIVE ESTIMATE                         
         MVC   MSRKMKT,QBMKT                                                    
         MVC   MSRKSTA,QBSTA                                                    
         MVC   MSRKMOS,INVENMOS                                                 
         XC    MSRKMINK(MSRDSTAT-MSRKMINK),MSRKMINK                             
         B     MSR10               YES                                          
*                                                                               
MSR22    LLC   RE,MSRKPRD          PRODUCT                                      
         CLI   SBQBPRD,FF          TEST FOR 'POL' REQUEST                       
         BNE   *+8                                                              
         LA    RE,255              YES-LOOK UNDER 'POL' ESTIMATES               
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZICM  R0,MSRKEST,1        ESTIMATE                                     
         BZ    MSR50               SKIP EST 0 RECS                              
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    MSR50               NO                                           
         ICM   R5,15,SBAESTBF      YES-GET ESTIMATE DETAILS                     
         BZ    MSR30                                                            
         LLC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MHI   RF,ESTBUFFL                                                      
         AR    R5,RF                                                            
         USING ESTBUFFD,R5                                                      
         MVC   SBESTFLT,EBFILT                                                  
         MVC   SBRTLSCH,EBRTLSCH                                                
***      MVC   SBUE1FLD,EBUFLD1                                                 
***      MVC   SBUE2FLD,EBUFLD2                                                 
         OI    SBEUDEF,SBEUECOM    TELL CALLER TO GET EST UCOMS                 
         GOTOR VRCPACK,PARM,(C'U',EBSREP),SBSREP                                
         DROP  R5                                                               
*                                                                               
MSR30    OC    QBMKTST(4),QBMKTST  TEST MARKET START/END SPECIFIED              
         BNZ   MSR31               YES                                          
         OC    QBMKT,QBMKT         TEST REQUEST FOR MKT 0 (CAN NTWK)            
         BZ    MSR32               YES - DO NOT TEST MARKET                     
MSR31    CLC   MSRKMKT,QBMKTST     TEST MARKET WITHIN RANGE                     
         BL    *+18                                                             
         CLC   MSRKMKT,QBMKTND                                                  
         BH    MSR50               DONE WITH MARKET - NEXT ESTIMATE             
         B     MSR32                                                            
         MVC   MSRKMKT,QBMKTST     SKIP TO MARKET START                         
         MVC   MSRKSTA,QBSTA                                                    
         MVC   MSRKMOS,INVENMOS                                                 
         XC    MSRKMINK(MSRDSTAT-MSRKMINK),MSRKMINK                             
         B     MSR10                                                            
*                                                                               
MSR32    CLC   SBQMGRF,BLANKS      TEST FOR MARKET GROUP FILTERING              
         BNH   MSR34               NO                                           
         MVC   SVMKT,MSRKMKT                                                    
         MVC   SVPRD,MSRKPRD                                                    
         GOTOR ACHKMGRP                                                         
         BNE   MSR45               MARKET NOT IN MKTGRP - READ NEXT MKT         
*                                                                               
MSR34    MVC   STAFILT,MSRKMKT                                                  
         GOTOR ACBLFILT                                                         
         BNE   MSR44               SKIP TO NEXT STATION                         
*                                                                               
MSR37    SR    R0,R0                                                            
         ICM   R0,7,SVSTA                                                       
         SR    RF,RF                                                            
         ICM   RF,7,MSRKSTA                                                     
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BE    MSR37A               NFC (NO CABLE)                              
         CLI   MSRKSTA,X'E8'                                                    
         BL    MSR37A                                                           
         N     R0,=X'00FFFF80'                                                  
         N     RF,=X'00FFFF80'                                                  
*                                                                               
MSR37A   CR    R0,RF               TEST FOR RIGHT STATION                       
         BE    MSR38                                                            
         MVC   SVSTA,MSRKSTA       YES-GET STATION DETAILS                      
         GOTOR AGETSTA                                                          
         BNE   MSR44                                                            
*                                                                               
MSR38    GOTOR ACHKSTA             CHECK STATION FILTERS                        
         BNE   MSR44                                                            
*                                                                               
MSR40    OC    INVENMOS,INVENMOS   TEST HAVE A END MOS                          
         BZ    MSR41                                                            
         CLC   MSRKMOS,INVENMOS    YES-TEST MONTH AFTER END                     
         BNL   MSR41                                                            
         MVC   MSRKMOS,INVENMOS    YES-ADVANCE TO END                           
         XC    MSRKMINK(MSRDSTAT-MSRKMINK),MSRKMINK                             
         B     MSR10                                                            
*                                                                               
MSR41    CLC   INVSTMOS,XFF        TEST HAVE A START MONTH                      
         BE    MSR60                                                            
         CLC   MSRKMOS,INVSTMOS    YES-TEST BEFORE START MONTH                  
         BNH   MSR60                                                            
         B     MSR44               NEXT STATION                                 
*                                                                               
MSR44    MVC   MSRKMOS(MSRDSTAT-MSRKMOS),XFF   NEXT STATION                     
         B     MSR10                                                            
*                                                                               
MSR45    MVC   MSRKSTA(MSRDSTAT-MSRKSTA),XFF   NEXT MARKET                      
         B     MSR10                                                            
*                                                                               
MSR50    MVC   MSRKMKT(MSRDSTAT-MSRKMKT),XFF   NEXT ESTIMATE                    
         ICM   R1,15,SBAESTTB      TEST PRODUCT/EST BUFFER PASSED               
         BNZ   MSR10               YES                                          
         LLC   R1,MSRKEST          NO-FIND NEXT ACTIVE ESTIMATE                 
         LA    R1,1(R1)                                                         
         LR    R3,R1                                                            
         LA    R1,ESTTAB(R1)                                                    
         LA    RE,1                                                             
         LA    RF,ESTTAB+L'ESTTAB-1                                             
         CLI   0(R1),0                                                          
         BNE   *+16                                                             
         LA    R3,1(R3)                                                         
         BXLE  R1,RE,*-12                                                       
         B     MSR55                                                            
         STC   R3,MSRKEST                                                       
         MVC   MSRKMKT,QBMKT                                                    
         MVC   MSRKSTA,QBSTA                                                    
         MVC   MSRKMOS,INVENMOS                                                 
         XC    MSRKMINK(MSRDSTAT-MSRKMINK),MSRKMINK                             
         B     MSR10                                                            
*                                                                               
MSR55    CLI   SBQBPRD,0           NEXT PRODUCT                                 
         BE    *+12                                                             
         CLI   SBQBPRD,FF                                                       
         BNE   MSRX                                                             
         MVC   MSRKPRD2(MSRDSTAT-MSRKPRD2),XFF                                  
         B     MSR10                                                            
*                                                                               
*                                                                               
MSR60    CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   MSR61               NO                                           
         CLI   SBEBYPRD,C'Y'       YES-TEST TO SPLIT OUT THE PRODUCTS           
         BE    MSR61               YES                                          
         CLI   SBBPRD,FF           NO-FORCE PRODUCT=POL                         
         BE    MSR65                                                            
         MVI   SBBPRD,FF                                                        
         B     MSR62                                                            
*                                                                               
MSR61    CLC   MSRKPRD,SBBPRD      TEST PRODUCT CHANGE                          
         BE    MSR65                                                            
         MVC   SBBPRD,MSRKPRD                                                   
*                                                                               
MSR62    L     RF,ACLTREC                                                       
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
MSR63    CLC   SBBPRD,3(RF)                                                     
         BE    MSR64                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    MSR63                                                            
         B     MSR55                                                            
*                                                                               
MSR64    MVC   SBPRD,0(RF)                                                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         ICM   R1,15,SBAPRDBF                                                   
         BZ    MSR65                                                            
         AR    R1,RE                                                            
         MVC   SBBPGR,PBGROUP-PRDBUFFD(R1)                                      
         MVC   SBPRDNM,PBNAME-PRDBUFFD(R1)                                      
         MVC   SBPRDINT,PBINT-PRDBUFFD(R1)                                      
         MVC   SBUP1FLD,PBUFLD1-PRDBUFFD(R1)                                    
         MVC   SBUP2FLD,PBUFLD2-PRDBUFFD(R1)                                    
         OI    SBEUDEF,SBEUPCOM    TELL CALLER TO GET PRD UCOMS                 
         OC    SBBPGR,SBBPGR                                                    
         BNZ   *+10                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
MSR65    CLC   SBBEST,MSRKEST      TEST ESTIMATE CHANGE                         
         BE    MSR66                                                            
         MVC   SBBEST,MSRKEST                                                   
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
MSR66    TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    *+8                         NO                                   
         BRAS  RE,ESTUSERF                 YES - GET EST USER FIELDS            
*                                                                               
         CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    MSR68                                                            
         CLC   SBBMKT,MSRKMKT      YES-TEST NEW MARKET                          
         BNE   *+14                                                             
         OC    SBPGRPEX(2),SBPGRPEX    OR THERE ARE PRDGRP EXCEPTIONS           
         BZ    MSR68                                                            
         MVC   SVMKT,MSRKMKT       YES-SET THE MARKET GROUP                     
         MVC   SVPRD,MSRKPRD                                                    
         GOTOR ACHKMGRP                                                         
         MVC   SBBMGR,HALF                                                      
*                                                                               
MSR68    CLC   SBBMKT,MSRKMKT      TEST MARKET CHANGE                           
         BE    MSR70                                                            
         MVC   SBBMKT,MSRKMKT      YES-SET MARKET DETAILS                       
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBMKT,DUB                                                        
         GOTOR AGETMKT             GET THE MARKET RECORD                        
         BNE   MSR70                                                            
         MVI   SBMODE,SBPROCMK                                                  
         BRAS  RE,GO               HOOK TO USER                                 
         JNE   NEQEXIT                                                          
*                                                                               
MSR70    CLC   SBBSTA,MSRKSTA      TEST STATION CHANGE                          
         BNE   *+16                                                             
         CLI   GOODSTA,C'N'        NO-SKIP STATION IF IT'S NOT GOOD             
         BE    MSR90                                                            
         B     MSR78                                                            
         MVI   GOODSTA,C'N'                                                     
         CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK REQUEST                
         BNE   MSR71                                                            
         CLC   SBBSTA(2),MSRKSTA                                                
         BE    MSR74               YES-GET THE NETWORK                          
*                                                                               
MSR71    MVC   SBBSTA,MSRKSTA      SET STATION                                  
         GOTOR AMSUNPK,PARM,(X'80',MSRKMKT),WORK,WORK+4                         
         MVC   SBSTA,WORK+4                                                     
         MVC   SBCBLNET,WORK+9                                                  
         XC    SBNETWK,SBNETWK                                                  
         CLI   CNETWK,C'Y'         TEST CANADIAN NETWORK REQUEST                
         BNE   MSR76                                                            
* IF MSRKSTA+2 >= 5, IT IS A NETWORK ID. ELSE IT IS MEDIA CODE                  
         CLI   MSRKSTA+2,5         YES-TEST STATION BELONGS TO NETWORK          
         BH    MSR74               YES-FIND THE NETWORK                         
         LA    R1,CNTWKTAB         NO-TEST IF STATION IS A NETWORK              
         LA    R0,128                                                           
MSR71A   CLI   0(R1),0             TEST EOT                                     
         BE    MSR71B                                                           
         CLC   0(4,R1),SBSTA                                                    
         BE    MSR72                                                            
         LA    R1,4(R1)                                                         
         BCT   R0,MSR71A                                                        
MSR71B   CLI   SBQMED,C'C'         IT'S NOT A NETWORK-TEST MED=COMBINED         
         BNE   MSR74                                                            
         MVI   SBMED,C'T'          YES-THEN MEDIA IS TV                         
         B     MSR76                                                            
*                                                                               
MSR72    MVC   SBNETWK,SBSTA       SET NETWORK=STATION                          
         MVI   SBMED,C'N'          MEDIA IS N                                   
         B     MSR75                                                            
*                                                                               
MSR74    LLC   R1,MSRKSTA+2        GET LAST BYTE OF STATION                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,CNTWKTAB(R1)     INDEX INTO NETWORK TABLE                     
         MVC   SBNETWK,0(R1)       SET THE NETWORK                              
         MVI   SBMED,C'N'          MEDIA IS N                                   
*                                                                               
MSR75    OC    QBNET,QBNET         TEST NETWORK FILTER                          
         BZ    *+14                                                             
         CLC   SBNETWK,SBQNET      YES-COMPARE NETWORK TO FILTER                
         BNE   MSR90               NOT EQUAL - SKIP THIS RECORD                 
         CLC   SBBSTA,MSRKSTA      TEST SAME STATION, DIFFERENT NETWORK         
         BE    MSR76                                                            
         MVC   SBBSTA,MSRKSTA      YES-SET THE STA, BUT NO STA FIRST            
         B     MSR78                                                            
*                                                                               
MSR76    MVI   SBMODE,SBPROCST                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
MSR78    CLI   SBQSTATY,C' '       TEST STATION TYPE FILTER                     
         BNH   MSR80               NO                                           
         CLC   SBSTYPE,SBQSTATY    YES-CHECK STATION TYPE                       
         BNE   MSR90               (ASSUMES HOOK SET TYPE)                      
*                                                                               
MSR80    MVI   GOODSTA,C'Y'             GOOD STATION                            
         MVC   IODA,SBRECDA             GET THE MSR RECORD                      
         GOTOR AIO,IOXSPFIL+IOGET+IO1                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    MSR90                                                            
*                                                                               
* HOOK TO USER                                                                  
*                                                                               
         MVI   SBMODE,SBPROCMS                                                  
         BRAS  RE,GO                                                            
         JNE   NEQEXIT                                                          
*                                                                               
*                                                                               
MSR90    LA    R2,IOKEY                                                         
         TM    SBIOFLAG,SBMINSK    SKIP REST OF MINIO SET?                      
         BZ    *+14                                                             
         MVC   MSRKMINK,XFF                                                     
         B     *+12                                                             
         TM    SBIOFLAG,SBNOIO     TEST NO IO EXECUTED IN HOOK TO USER          
         BO    MSR92                                                            
         GOTOR AIO,IOXSPDIR+IOHI   NO-REESTABLISH READ SEQUENCE                 
*                                                                               
MSR92    DS    0H                                                               
         NI    SBIOFLAG,X'FF'-(SBNOIO+SBMINSK)                                  
         GOTOR AIO,IOXSPDIR+IOSQ   READ NEXT BILL RECORD                        
         B     MSR15                                                            
*                                                                               
MSRX     J     EQEXIT                                                           
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*************************************************************                   
*                                                                               
* BASELESS CODE                                                                 
*                                                                               
*************************************************************                   
         SPACE                                                                  
EQBR     LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NEBR     LTR   RE,R0                                                            
         BR    RE                                                               
*                                                                               
EQEXIT   CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQEXIT  LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         JZ    NEXTELX                                                          
         AR    R2,R0                                                            
NEXTEL2  CLI   0(R2),0                                                          
         JE    NEXTELX                                                          
         CLC   0(1,R2),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R2),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
*                                                                               
SETMASK  LTR   R1,R1                                                            
         JP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,GRPMASKS-2                                                    
         LA    RF,2(RF)                                                         
         BRCT  R1,*-4                                                           
         LH    R1,0(RF)                                                         
         BR    RE                                                               
*                                                                               
* HOOK TO USER                                                                  
* RETURN CONDITION CODE EQ - NORMAL RETURN                                      
*                       NE - STOP SPOTIO NOW                                    
*                                                                               
GO       LR    R1,RE              SAVE RE                                       
         MVC   SBIOKEY,IOKEY      PASS KEY                                      
         BRAS  RE,GO2                                                           
         CLI   SBMODE,SBSTOP      TEST RETURN MODE SET TO STOP                  
         JNE   *+8                                                              
         LTR   RE,R1                                                            
         BR    RE                                                               
         LR    RE,R1                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
GO2      NTR1  BASE=*,LABEL=*                                                   
         L     RF,SBIOHOOK                                                      
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         J     EXIT                                                             
*                                                                               
         DROP                                                                   
         ORG   *+256                                                            
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
TROFC    DC    C'16',X'FF'                                                      
NEOFC    DC    C'BYLEMCRIUGS',X'FF'                                             
GBOFC    DC    C'1',X'FF'                                                       
*                                                                               
GRPMASKS DC    X'0FFF'             FOR BREAK LEN = 3                            
         DC    X'00FF'                             2                            
         DC    X'000F'                             1                            
*                                                                               
* HARD CODED STATION GROUP FOR EIX                                              
         SPACE 1                                                                
ESGR1BK  DC    CL12'EASI'                                                       
ESGR2BK  DC    CL12' '                                                          
ESGR1LN  DC    X'03'                                                            
ESGR2LN  DC    X'03'                                                            
EBSGR    DC    X'1000'                                                          
ESGR1NM  DC    CL24'STATIONS'                                                   
ESGR2NM  DC    CL24' '                                                          
*                                                                               
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
*                                                                               
         EJECT                                                                  
*                                                                               
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSPOOL)                                                      
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QCASHIER)                                                    
         DC    AL1(QTSAROFF)                                                    
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QBLDMGN)                                                     
PHASESN  EQU   *-PHASES                                                         
         SPACE 1                                                                
CONADDRS DS    0F                  ** SPOTIO FACILITIES **                      
         DC    V(EQVRD)                                                         
         DC    V(MEDGET)                                                        
         DC    V(DPTRD)                                                         
         DC    V(GETBROAD)                                                      
         DC    V(COVAIL)                                                        
         DC    A(FILTAB)                                                        
         DC    A(SYSTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    A(IOEX)                                                          
         DC    A(IOEX)                                                          
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         EJECT                                                                  
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                               
*                                                                               
FILTAB   DS    0X                                                               
*                                  ** SPOT SYSTEM FILES **                      
FILSPT   DC    X'02',AL2(FILSPTX-*)                                             
*                                                                               
         DC    AL1(1),C'SPTDIR '   MUST BE FIRST TABLE ENTRY                    
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(2,13,01),AL2(18)                                             
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(2),C'SPTFIL '   MUST BE SECOND TABLE ENTRY                   
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(1,13,24),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(3),C'STATION'                                                
         DC    AL1(FILIIS,0)                                                    
         DC    AL1(0,17,01),AL2(117)                                            
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(4),C'XSPDIR '                                                
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(5,32,04),AL2(40)                                             
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(5),C'XSPFIL '                                                
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(4,32,42),AL2(4000)                                           
         DC    XL5'00'                                                          
*                                                                               
*                                                                               
FILSPTX  DC    AL1(EOT)                                                         
*                                                                               
FILTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
* TABLE OF GLOBAL SYSTEM FILES (FILE NUMBERS 10 THRU 15)                        
*                                                                               
SYSTAB   DS    0X                                                               
*                                                                               
         DC    AL1(11),C'CTFILE '  CONTROL FILE                                 
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
SYSTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* SYSTEM FILE COMMANDS TABLE                                                    
*                                                                               
CMDTAB   DS    0X                                                               
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIVL+FILIIS,0),AL2(CMDISX-*)                               
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(EOT)                                                         
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX-*)                                      
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
CMDDAX   DC    AL1(EOT)                                                         
*                                                                               
CMDTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IOWORKD  DSECT                     ** IO S/R LOCAL W/S **                       
IODUB    DS    D                   GENERAL WORK AREA                            
IOCTRL   DS    XL4                 I/O COMMAND WORD                             
IOTR1    DS    F                   TRACE WORD 1                                 
IOTR2    DS    F                   TRACE WORD 2                                 
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 COMMAND NAME                                 
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOWORKX  EQU   *                                                                
         EJECT                                                                  
*                                  ** GLOBAL EQUATES **                         
EOT      EQU   0                   END-OF-TABLE INDICATOR                       
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
*                                  ** I/O FILE EQUATES **                       
IOFILES  EQU   X'0F00'             RESERVED FOR CONTROLLER USE                  
IOSPTDIR EQU   X'0100'             I/O TO SPOT DIRECTORY                        
IOSPTFIL EQU   X'0200'             I/O TO SPOT FILE                             
IOSTAFIL EQU   X'0300'             I/O TO STATION FILE                          
IOXSPDIR EQU   X'0400'             I/O TO XSPOT DIR                             
IOXSPFIL EQU   X'0500'             I/O TO XSPOT FILE                            
IOCTFILE EQU   X'0B00'             I/O TO CONTROL FILE                          
         SPACE 1                                                                
*                                  ** I/O AREA EQUATES **                       
IO1      EQU   X'0040'             I/O AREA 1 TO BE USED FOR I/O                
IO2      EQU   X'0080'             I/O AREA 2 TO BE USED FOR I/O                
IO3      EQU   X'00C0'             I/O AREA 3 TO BE USED FOR I/O                
         SPACE 1                                                                
*                                  ** I/O COMMAND EQUATES **                    
IOCMNDS  EQU   X'000F'             RESERVED FOR CONTROLLER USE                  
IOLOCK   EQU   X'0010'             READ FOR UPDATE                              
IORDEL   EQU   X'0020'             READ DELETED RECORDS                         
IOHI     EQU   X'0001'             DMRDHI                                       
IOHID    EQU   IOHI+IORDEL         DMRDHI (FOR DELETES)                         
IOHIUP   EQU   IOHI+IOLOCK         DMRDHI (FOR UPDATE)                          
IOHIUPD  EQU   IOHI+IOLOCK+IORDEL  DMRDHI (FOR UPDATE & DELETES)                
IORD     EQU   X'0002'             DMREAD                                       
IORDD    EQU   IORD+IORDEL         DMREAD (FOR DELETES)                         
IORDUP   EQU   IORD+IOLOCK         DMREAD (FOR UPDATE)                          
IORDUPD  EQU   IORD+IOLOCK+IORDEL  DMREAD (FOR UPDATE & DELETES)                
IOSQ     EQU   X'0003'             DMRSEQ                                       
IOSQD    EQU   IOSQ+IORDEL         DMRSEQ (FOR DELETES)                         
IOSQUP   EQU   IOSQ+IOLOCK         DMRSEQ (FOR UPDATE)                          
IOSQUPD  EQU   IOSQ+IOLOCK+IORDEL  DMRSEQ (FOR UPDATE & DELETES)                
IOGET    EQU   X'0004'             GETREC                                       
IOGETRUP EQU   IOGET+IOLOCK        GETREC (FOR UPDATE)                          
IOADD    EQU   X'0005'             DMADD                                        
IOADDREC EQU   X'0005'             ADDREC                                       
IOWRITE  EQU   X'0006'             DMWRT                                        
IOPUTREC EQU   X'0006'             PUTREC                                       
IOUNLOCK EQU   X'0007'             DMUNLK                                       
         EJECT                                                                  
FILTABD  DSECT                     ** I/O FILE TABLE **                         
FILNUM   DS    XL1                 FILE NUMBER (IOFILES EQUATE)                 
FILNAME  DS    CL7                 FILE NAME                                    
FILINDS  DS    XL1                 FILE INDICATORS - 1                          
FILIVL   EQU   X'80'               FILE RECORDS ARE VARIABLE LENGTH             
FILIDA   EQU   X'40'               FILE IS A DIRECT ACCESS FILE                 
FILIIS   EQU   X'20'               FILE IS AN INDEX SEQUENTIAL FILE             
FILINDS2 DS    XL1                 FILE INDICATORS - 2                          
FILIDI   EQU   X'80'               FILE IS D/A WITH I/S INDEX                   
FILIID   EQU   FILIDI              FILE IS I/S INDEX TO A D/A FILE              
FILNUM2  DS    XL1                 D/A OR I/S FILE NUMBER                       
FILKEYL  DS    XL1                 D/A OR I/S KEY LENGTH                        
FILCTLL  DS    XL1                 FIXED LENGTH I/S CONTROL LENGTH              
FILDISP  EQU   FILCTLL             DISPLACEMENT TO FIRST RECORD ELEMENT         
FILMAXL  DS    XL2                 MAXIMUM RECORD LENGTH                        
         DS    XL5                 SPARE                                        
FILTABL  EQU   *-FILTABD                                                        
         SPACE 1                                                                
CMDTABD  DSECT                     ** I/O COMMAND TABLE **                      
CMDNAME  DS    CL7                 COMMAND NAME                                 
CMDNUMB  DS    XL1                 COMMAND NUMBER (SEE IOCMNDS)                 
CMDINDS  DS    XL1                 COMMAND INDICATORS - 1                       
CMDIDARQ EQU   X'80'               DISK ADDRESS REQUIRED FOR I/O                
CMDIDAXC EQU   X'40'               CLEAR DISK ADDRESS BEFORE I/O                
CMDIDADD EQU   X'20'               DISK ADDRESS RETURNED FROM I/O               
CMDINDS2 DS    XL1                 COMMAND INDICATORS - 2                       
CMDTABL  EQU   *-CMDTABD                                                        
*                                                                               
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
RELO     DS    A                                                                
PARM     DS    8F                                                               
WORK     DS    CL256                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
USERRD   DS    A                   USER REGISTER 13                             
STAWORK  DS    XL32                STAPACK WORK AREA                            
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
AXTRA    DS    0F                  EXTENTION ROUTINE ADDRESSES                  
AREADINF DS    A                                                                
ASETUSER DS    A                                                                
AGETPGR  DS    A                                                                
AGETMGR  DS    A                                                                
AGETMKT  DS    A                                                                
AGETSGR  DS    A                                                                
AGETSTA  DS    A                                                                
AGETDPTB DS    A                                                                
AGETBILF DS    A                                                                
ACHKEST  DS    A                                                                
ACHKMGRP DS    A                                                                
ACHKSTA  DS    A                                                                
ACHKMDPT DS    A                                                                
AFILTDPT DS    A                                                                
AMSPACK  DS    A                                                                
AMSUNPK  DS    A                                                                
ACBLFILT DS    A                                                                
AGETALPH DS    A                                                                
AGETPWM  DS    A                                                                
AGETPWS  DS    A                                                                
ARDBILL  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
AFILNTRY DS    A                                                                
ACLTREC  DS    A                                                                
AGLMED   DS    A                                                                
ACLTEL   DS    A                                                                
AXSPILL  DS    A                                                                
*                                                                               
QCGRLEN  DS    F                                                                
QSGRLEN  DS    F                                                                
*                                                                               
ONLINE   DS    CL1                                                              
THREE    DS    XL3                                                              
BYTE     DS    X                                                                
FLAG     DS    X                                                                
LMASPRD  DS    CL2                 PREV BDMASPRD                                
SBDRPROF DS    CL16                                                             
POLUDEF1 DS    CL(L'PUSER1)        SAVED POL UDEF1                              
POLUDEF2 DS    CL(L'PUSER2)        SAVED POL UDEF2                              
*                                                                               
FLAG2    DS    X                                                                
FLGETPW  EQU   X'80'               EXTRACT PW MKT & STA REC                     
FLGTPWS  EQU   X'40'               EXTRACT PW STATION REC                       
STPNODIE EQU   X'20'               DON'T DIE ON STAPACK ERROR                   
SKIPSTA  EQU   X'10'               DON'T READ PW STA LEVEL REC                  
FLWASPOL EQU   X'08'               SBPRD WAS POL, SET BACK                      
FLRDBH2  EQU   X'04'               THIS IS NEW READBH PASS                      
FLSTA    EQU   X'02'               SAME STATION                                 
*UNUSED  EQU   X'01'                                                            
*                                                                               
INDS     DS    X                                                                
IONUM    DS    X                                                                
FRSTEST  DS    C                                                                
ESTFOUND DS    C                                                                
ESTBFENT DS    X                                                                
CANADA   DS    C                                                                
COUNTRY  DS    C                                                                
MARKET0  DS    C                                                                
CNETWK   DS    C                                                                
GOODSTA  DS    C                                                                
PRD2ONLY DS    C                                                                
INVST    DS    XL2                                                              
INVEN    DS    XL2                                                              
INVSTMOS DS    XL2                                                              
INVENMOS DS    XL2                                                              
GLMEDS   DS    CL4                                                              
BSTA     DS    XL3                                                              
FIRSTSW  DS    XL1                                                              
FSTA     EQU   X'80'                                                            
FEST     EQU   X'40'                                                            
MEDCMED  DS    CL1                                                              
SVKEY    DS    XL13                                                             
BLANKS   DS    CL133                                                            
XFF      DS    XL16                                                             
*                                                                               
IVCST    DS    CL6                 BILL HEADER DATE FILTERS                     
IVCEN    DS    CL6                                                              
RUNST    DS    CL6                                                              
RUNEN    DS    CL6                                                              
DUEST    DS    XL3                                                              
DUEEN    DS    XL3                                                              
*                                                                               
PASA     DS    X                                                                
P        DS    CL132                                                            
*                                                                               
BILLFORM DS    0CL5                DEFAULT BILL FORMULA                         
BILLBASE DS    X                                                                
BILLCOM  DS    XL4                                                              
         SPACE 1                                                                
*                                  ** REQUEST FILTERS **                        
QPGRLEN  DS    XL2                                                              
QPNAME   DS    CL20                                                             
QDEMOS   DS    CL(20*3)                                                         
STAFILT  DS    XL5                                                              
QBMKT    DS    XL2                                                              
QBMKTST  DS    XL2                                                              
QBMKTND  DS    XL2                                                              
QBMKTSTA DS    XL5                                                              
         ORG   QBMKTSTA+2                                                       
QBSTA    DS    XL3                                                              
QBSTACAN DS    XL3                                                              
QBNET    DS    XL3                                                              
QNETBITS DS    XL1                                                              
QMGRLEN  DS    XL2                                                              
QCPPGUID DS    CL1                                                              
ESTTAB   DS    XL256                                                            
         SPACE 1                                                                
*                                  ** SAVED VALUES **                           
SVPRD    DS    X                                                                
SVMKTSTA DS    0XL5                                                             
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
SVIOKEY  DS    XL40                                                             
SVBAGYMD DS    XL1                                                              
CALLSAVE DS    XL2                 DISPLACEMENT TO 9D ELEM IN BUY REC           
NINE_D   DS    XL1                                                              
SVSBSTA  DS    CL5                 SAVED SBSTA                                  
THISSTA  DS    XL3                                                              
STAMKALF DS    XL3                 STATION'S ALPHA MARKET                       
* NOTE * NOTE * TSARKEYL & NPSTACDQ ARE ALSO DEFINED IN SPWRIWORKD              
TSARKEYL EQU   31                  MAX KEY LEN                                  
NPSTACDQ EQU   17                  STATION BUFFER, LEN=SBUFMAX                  
*                                                                               
IOAREAST DS    0F                  ** I/O CONTROLLER VARIABLES **               
IOADDR   DS    A                   I/O AREA ADDRESS                             
IOFILE   DS    CL7                 FILE NAME                                    
IOCMND   DS    CL7                 DATAMGR COMMAND                              
IOERR    DS    XL1                 I/O ERROR RETURN BYTE                        
IOEEOF   EQU   X'80'               END-OF-FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
IOERRS   EQU   X'FF'               RESERVED FOR CONTROLLER USE                  
IOKEY    DS    CL40                ACTUAL KEY VALUE                             
IOKEYSAV DS    CL40                SAVED ACTUAL KEY VALUE (BEFORE I/O)          
IODA     DS    XL4                 DISK ADDRESS OF D/A RECORD                   
IOWORK   DS    XL96                D/A LINKED FILES WORK AREA                   
IOAREAND EQU   *                                                                
*                                                                               
PWLKEY   DS    CL13                LAST PW KEY                                  
MSFORMAT DS    CL3                 MSTREET FORMAT FOR KEY                       
MSOWNER  DS    CL5                 MSTREET OWNER FOR KEY                        
MSPARENT DS    CL5                 MSTREET PARENT FOR KEY                       
MSFREQ   DS    CL5                 MSTREET FREQUENCY                            
MSSTATE  DS    CL2                 MSTREET STATE                                
MSMOWNER DS    CL1                 MSTREET OWNER                                
MSUNIQID DS    CL6                 MSTREET UNIQUE ID                            
MSMOWNF  DS    CL1                 MINORITY OWNERSHIP IS FEMALE                 
MSFCCM   DS    CL1                 QUALIFIED FCC MINORITY                       
MSFCCMF  DS    CL1                 FCC MINORITY OWNERSHIP IS FEMALE             
SVSYSCD  DS    CL3                 SYSCODE FOR OM RECS                          
*                                                                               
         SPACE 1                                                                
COREFACS DS    0A                  ** T00A PHASE ROUTINES **                    
VCLPACK  DS    V                                                                
VCLUNPK  DS    V                                                                
VOFFICER DS    V                                                                
VSPOOL   DS    V                                                                
VSTAPACK DS    V                                                                
VCASHIER DS    V                   V(DDCASHIER)                                 
VTSAROFF DS    V                   V(TSAROFF)                                   
VRCPACK  DS    V                   V(RCPACK)                                    
VBLDMGN  DS    V                   V(BLDMGN)                                    
         SPACE 1                                                                
CONFACS  DS    0F                  ** SPOTIO FACILITIES **                      
VEQVRD   DS    V                                                                
VMEDGET  DS    V                                                                
VDPTRD   DS    V                                                                
VGETBROD DS    V                                                                
VCOVAIL  DS    V                                                                
AFILTAB  DS    A                                                                
ASYSTAB  DS    A                                                                
ACMDTAB  DS    A                                                                
AIO      DS    A                                                                
VIO      DS    A                                                                
         SPACE 1                                                                
AIOAREAS DS    0F                  ** IO AREA ADDRESSES **                      
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
         SPACE 1                                                                
*                                  ** RECORD AREAS **                           
CLTREC   DS    (CLTHDRL)X                                                       
         SPACE 1                                                                
*                                  ** CANADIAN NETWORK TABLE **                 
CNTWKTAB DS    XL(128*4)                                                        
         SPACE 1                                                                
*                                  ** BILL FORMULA TABLE **                     
BILFOTAB DS    255XL6                                                           
*                                                                               
         DS    0D                                                               
CSHIERC  DS    XL(CSHIERL)         DDCASHIER CONTROL BLOCK                      
*                                                                               
         DS    0D                                                               
CGRPREC  DS    2000X               CLIENT GROUP RECORD (FOR OFFLINE             
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
* SPSTAPACKD                       *** STAPACK CONTROL BLOCK ***                
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*                                                                               
SBLOCKD  DSECT                     ** SPOTBLOCK AREA **                         
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENCLT                                                                      
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENPRD                                                                      
         PRINT OFF                                                              
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENPRG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENEST                                                                      
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBUY                                                                      
*        PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENGOAL                                                                     
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSTAB                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENINV                                                                      
         PRINT OFF                                                              
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSNV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENSNV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSTEQ                                                                     
        PRINT OFF                                                               
       ++INCLUDE SPGENSTEQ                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
*                                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENAGY                                                                      
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSTA                                                                      
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENGRP                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENGRP                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBILL                                                                     
         PRINT OFF                                                              
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENXLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENXLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENDAYPT                                                                    
         PRINT OFF                                                              
DPTHDRD  DSECT                                                                  
       ++INCLUDE SPGENDAYPT                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENEQU                                                                      
         PRINT OFF                                                              
EQUHDRD  DSECT                                                                  
       ++INCLUDE SPGENEQU                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENDMN                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENNDEF                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENNDEF                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENPGEST                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENPGEST                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENCLRST                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENCLRST                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENINFO                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENINFO                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENWIPW                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENWIPW                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMSR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMSR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBGR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBGR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDCASHIERD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCASHIERD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
* CTGENRAD                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENRAD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
* SPGENDRORD/SPGENDRFLT/DDTSARD/SPGENBPCT/SPMGADN                               
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRFLT                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPGENBPCT                                                      
       ++INCLUDE SPMGADN                                                        
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPOTIO    06/17/19'                                      
         END                                                                    
*********************************************************************           
* UPDATE HISTORY:                                                   *           
* 07DEC10 91 AKT -- MAKE SURE ALL OM RECS ARE DELETED FROM TSAR BUFF*           
* 19OCT10 90 AKT -- BUFFER THE CLIENT/PRODUCT MAIN PST FIELDS       *           
*                -- DONT COUNT X'22/23' ELEMS AS PBD IF NO OVERRIDES*           
* 17AUG10 89 AKT -- SUPPORT NEW OR & NON-POSITIONAL EST FILT OPTION *           
* 25MAR10 88 AKT -- MEDIA FRAMEWORKS SUPPORT                        *           
* 25JAN10 86 AKT -- SET STBTYPE DIFFERENTLY FOR CANADIAN STATIONS   *           
* 11JAN10 85 AKT -- DON'T DIE IF STAPERR=X'04' (INVALID CBL NTWK)   *           
* 01MAY09 84 AKT -- LOOK UP TV MSTREET REC W/BAND L IF T NOT FOUND  *           
* 15APR09 83 AKT -- SUPPORT NEW MTRADE=Y/N OPTION FOR BILL HEADERS  *           
* 14JAN09 82 AKT -- SUPPORT NEW BFORM OPTION                        *           
* 16MAY08 81 AKT -- STRIP NETWORK BITS PROPERLY FOR ORDERS          *           
* 05MAY08 80 AKT -- FIX RANGE FILTER LOGIC                          *           
* 20APR08 79 AKT -- SET PGROUP PROPERLY WHEN PROCESSING ESTIMATES   *           
* 14APR08 78 AKT -- ONLY PROCESS OM RECS ONCE PER SYSCODE           *           
* 22FEB07 77 AKT -- FIX UDEF=POL BUG                                *           
* 09JAN07 76 AKT -- FIX GM BILLING RESTRUCTURE BUG                  *           
* 11JAN07 75 AKT -- TURN OFF ZERO FLAG ON BILLING PERCENT           *           
* 09JAN07 74 AKT -- FIX OMFLIGHT BUG                                *           
* 20DEC07 73 AKT -- GM BILLING RESTRUCTURE SUPPORT                  *           
* 20DEC07 72 AKT -- BUFFER OM RECS ON EST CHANGE FOR OM/BUY MERGE   *           
* 14DEC07 71 AKT -- SET CASH/TRADE FOR BUYS PROPERLY                *           
* 30NOV07 70 AKT -- MERGE OM AND BUY DATA                           *           
* 12JUL07 67 AKT -- ADD NEW FIELDS TO THE STATION BUFFER AND ONLY   *           
*                -- BUFFER THINGS WE ACTUALLY FIND FOR THEIR LENGTH *           
* 21JUN07 68 AKT -- FIX MKT LIMIT ACCESS BUG IN OFFICER CALL        *           
* 09MAY07 67 AKT -- CALL OFFICER TO HANDLE SECURITY FOR US          *           
* 31JAN07 66 AKT -- FIX MGREQ BUG AS PER ANN                        *           
* 10NOV06 65 AKT -- TSAR OFF STATION BUFFER ENTRIES AND DON'T SAVE  *           
*                -- NETWORK BITS WHEN ADDING TO STA BUFF            *           
* 05OCT06 64 AKT -- SUPPORT NEW ERATE FILTER                        *           
* 03OCT06 62 AKT -- CALL MSUNPK WITH X'80' IN PROCOM                *           
* 24AUG06 61 AKT -- ESTIMATE USER FIELDS NOW IN TSAR (NOT EST BUFF) *           
* 14AUG06 60 AKT -- CANADIAN AFFILIATE IS SCANNTWK, NOT SNETWRK     *           
* 19MAY06 59 AKT -- FILTER LOCKED ESTIMATES BASED ON FILTER OPTION  *           
* 11MAY06 58 AKT -- HONOR MGROUP FILTER FOR BILL HEADER KEYWORDS    *           
* 26APR06 57 AKT -- FIX MGROUP FILTER LOGIC FOR OM KEYWORDS         *           
* 09MAR06 56 AKT -- FIX SBSTA BUG FOR BUYS                          *           
* 02JAN06 55 AKT -- FIX 2 CHAR MGROUP BUG FOR BILL HEADER           *           
* 16NOV05 54 AKT -- SUPPRESS VOID CHECKS IF NOVOID OPTION IS SET    *           
* 12SEP05 53 AKT -- FIX CDATE/ACTDATE BUG FOR GOALS                 *           
* 20JUL05 52 EFJ -- 2 CHARACTER OFFICE CODE SUPPORT                 *           
* 09AUG05 51 AKT -- CDATE FILTER SUPPORTED FOR GOALS & BILLHEADER   *           
*                   ACTIVDATE FILTER SUPPORTED FOR BUYS AND GOALS   *           
*                   COMMENT OUT CODE FOR USER ID MVMFI AS PER EJ    *           
* 29JUL05 50 AKT -- FILL IN SBQBPRD FOR ALL MEDIA SINGLE PRD REQUEST*           
* 26JUL05 49 AKT -- IF WE HAVE A STATION CALL LETTER CHANGE MAKE    *           
*                -- SURE WE HAVE CORRECT MKT FOR CLRST RECORD SINCE *           
*                -- THEY MAY HAVE RUN A MKTFIX ON TOP OF THE STAFIX!*           
* 15JUN05 48 AKT -- FIX BUY-ID BUGS FOR INVOICES                    *           
* 07MAR05 47 EFJ -- DON'T CALL NEXTSTA FOR INVOICES IN WRONG MKT    *           
* 23FEB05 46 AKT -- HONOR STATION FILTER FOR OM KEYWORDS            *           
* 19JAN05 45 AKT -- CLEAR SBBSTA IF NEXT STA FORCED FOR BILL RECS   *           
* 17NOV04 44 EFJ -- FULL GETSTA EXTRACT IF FLAG SET                 *           
*                -- MOVE GETSTREC OUT OF COMMON                     *           
* 02NOV04 43 AKT -- DO NOT LOOK FOR PASSIVE SGROUP WITH MEDIA T     *           
* 04OCT04 42 AKT -- OM KEYWORD ROUTINE SHOULD EXTRACT MGOUPS        *           
* 28JUL04 41 AKT -- GET MSTREET PARENT NAME FROM PARENT CODE        *           
*                -- AND READ MSTREET RECORDS BY STATION             *           
* 22JUN04 40 EFJ -- GET CORRECT MARKET FOR BUY-ID CLIENTS FOR INV'S *           
* 20JUL04 39 AKT -- DO NOT SET SBCBLNET IN PROCOM                   *           
* 08JUL04 38 AKT -- EXTRACT OM DATA                                 *           
* 09JUN04 37 AKT -- NEW OWNER AND FORMAT KEY FORMAT FOR MSTREET     *           
*                   AND BUG FIX ON HOW KEY IS COMPARED AFTER READ   *           
* 13MAY04 36 AKT -- SUPPORT NEW ETYPE AND BILL HEADER FILTERS       *           
* 10MAY04 35 EFJ -- FIX NEW CABLE TESTS FOR CANADA                  *           
* 03APR04 34 AKT -- CLEAR SBEUDEF OF PRD/EST/MCOM BETWEEN CLIENTS   *           
* 02APR04 33 AKT -- CHKTAB MUST USE SBNETWK AS STA IF IT IS SET     *           
* 29MAR04 32 EFJ -- ADIOS SPGENESTN                                 *           
* 29MAR04 31 EFJ -- NEW CABLE TESTS                                 *           
* 16DEC03 30 PWE -- EXTRACT CLIENT PST CODES (CPST)                 *           
* 18NOV03 29 AKT -- READ ALL MARKETS ON AN ALL CLIENT REQUEST       *           
* 17NOV03 28 AKT -- SAVE RS1/2CALL FOR CANADIAN SOFT DEMOS          *           
* 31OCT03 27 AKT -- FIX MGROUP AND CGROUP X'40' BUGS                *           
* 22OCT03 26 PWE -- FIX READCLS FOR CANADIAN NETWORK                *           
* 20OCT03 25 AKT -- MSTREET DATA TO STA BUFFER                      *           
* 09OCT03 24 EFJ -- REMOVE EBUFCRDT FROM ESTBUFF                    *           
* 09OCT03 23 AKT -- TEST MSTREET FLAG IN STATION TO GO TO FSTA      *           
* 15SEP03 22 EFJ -- SUPPORT FOR ECREATE KWD (INCOMPAT W/PW)         *           
* 21AUG03 21 AKT -- DONT DIE ON DUP ENTRY FROM BINSRCH IN READCLS   *           
* 05AUG03 20 AKT -- DONT CLOBBER CHECK TAB WHEN UPDATING N'RECS!    *           
* 28JUL03 19 AKT -- FIX BINSRCH BUG BY PUTTING STATION FIRST        *           
* 25JUL03 18 AKT -- CHECK CALL LETTER CHANGE IN BUY FOR CHECK TAB   *           
* 29MAY03 17 AKT -- SAVE NTI CODE IN STA BUFFER                     *           
* 18MAR03 16 AKT -- READ CANADIAN STA RECS (PASSIVE) WITH AN N      *           
* 06MAR03 15 EFJ -- FILTER ON POST BUY DEMOS                        *           
*                -- FIX TEST FOR CANADIAN CABLE!                    *           
* 22NOV02 14 EFJ -- NEVER READ FOR DELETES IF SBSKDEL IS SET        *           
* 18NOV02 13 EFJ -- FIX CLIENT EXCLUDE CODE                         *           
* 01NOV02 12 BOB -- OPTION TO READ BILLS BEFORE BUYS                *           
* 23OCT02 11 EFJ -- -S FILTER                                       *           
* 04OCT02 10 EFJ -- FILTER EASI INVOICES                            *           
* 13SEP02 09 EFJ -- SKIP ENTIRE INVOICE SET IF SBMODE=SBSKIP        *           
* 09JUL02 07 EFJ -- CHANGE ETYPE FILTER AGAIN                       *           
* 02JUL02 06 EFJ -- FIX STATION NAME FOR CABLE                      *           
*                -- CHANGE ETYPE FILTER                             *           
* 10JUN02 05 EFJ -- BONUS FILTER                                    *           
*                -- CLTACC KEYWORD                                  *           
* 07JUN02 04 EFJ -- SPECIAL SECURITY FOR MVMFI                      *           
* 28MAY02 03 EFJ -- SUPPORT ETYPE FILTER                            *           
* 17MAY02 02 EFJ -- LEVEL RESET                                     *           
*                -- SUPPORT RCPACK                                  *           
*-------------------------------------------------------------------*           
*                                                                   *           
* 06MAY02 93 EFJ -- FIX MARKET TEST FOR MARKET 0!                   *           
* 30APR02 92 EFJ -- FIX CANADIAN NETWORK BUG                        *           
* 11APR02 91 EFJ -- FIX STATION BILLS FOR NEW CANADIAN CABLE!       *           
*                -- CHANGE ALL BAL/BALR TO BRAS/BASR                *           
* 12MAR02 90 EFJ -- FIX ESTHDR LENGTHS!                             *           
* 15JAN02 89 EFJ -- EXTRACT BANK CLEARED DATE FROM CLRST REC        *           
* 19NOV01 88 EFJ -- CLIENT LIST SUPPORT                             *           
*                -- READSLK NOT TESTING CABLE FILTERS               *           
* 14SEP01 87 EFJ -- CANADIAN NETWORKS                               *           
*                -- CHANGE *+4 TO *+8 FOR IDF                       *           
* 27AUG01 86 EFJ -- SUPPORT MARKET LEVEL USER COMMENTS (UCOM)       *           
* 09AUG01 85 EFJ -- REPORT MEDIA N WITH MEDIA * FOR US AGY TR       *           
* 23JUL01 84 EFJ -- SET RSM IN GETMKT                               *           
* 25MAY01 83 EFJ -- UNDO L73 FROM 15APR96!!! - CAN ONLY SUPPORT 256 *           
* 25APR01 82 EFJ -- FIX STATION GROUPS FOR CABLE                    *           
* 08MAR01 81 EFJ -- READBILL DOESN'T FILTER ON CABLE STAS TOO WELL  *           
* 02JAN01 80 EFJ -- SUPPORT COST2                                   *           
* 21NOV00 79 EFJ -- COPY ALL CK CODE FOR JM                         *           
* 06OCT00 78 EFJ -- SUPPORT USER COMMENT RECORDS                    *           
* 14SEP00 77 EFJ -- NEW STATION LOCKIN RECORDS ON XSPFIL            *           
* 26JUN00 76 EFJ -- FIX NETWORK FILTER FOR MKT 0 BUYS               *           
* 07JUN00 75 EFJ -- SUPPORT MANUAL BILLING FILTERS                  *           
* 31MAR00 74 BOB -- BUFFER BILL RECORDS                             *           
* 10MAR00 73 EFJ -- BROKE SPILL ON LEV 71                           *           
* 02MAR00 72 EFJ -- SUPPORT BHEDATE FILTER                          *           
* 23FEB00 71 EFJ -- FIX BUG IN BUY SKIP READ                        *           
* 04FEB00 70 BOB -- USE MBRCLT FOR 'ALL' CLIENT MARKET GROUPS       *           
* 17NOV99 69 EFJ -- GET PAID REP FROM CLEARANCE STATUS REC          *           
* 24AUG99 68 EFJ -- CHANGE WIM TRADE TEST FOR DIY TRADE CLIENT      *           
* 09JUL99 67 EFJ -- SUPORT UP FRONT READ OF BH RECS TO BUILD TABLE  *           
* 02JUL99 66 EFJ -- ADD NEW OFFICE 'S' TO NEOFC FOR DDB XFILE       *           
* 30JUN99 65 EFJ -- FIX CBL FILE CODE FOR INV'S (NOT FULLY CORRECT) *           
* 07JUN99 64 EFJ -- DON'T BUILD PRD/EST TAB IF SBEFLAG2=SBEPETAB    *           
* 10MAY99 63 EFJ -- SKIP DELETED BUYS IF SBSKDEL SET                *           
* 05MAY99 62 EFJ -- SUPPORT NETWORK FILTER FOR INVOICES             *           
* 08APR99 61 EFJ -- CAN'T USE FUCKING R0                            *           
* 07APR99 60 EFJ -- MORE SPOT LIMIT ACCESS                          *           
* 27JAN99 57 EFJ -- SUPPORT WESTERN TRADE BUYS FOR PW               *           
* 28OCT98 56 EFJ -- FUCKING RETARD                                  *           
* 27OCY98 55 EFJ -- SUPPORT ORIGINAL COST BUYS                      *           
* 21OCT98 54 EFJ -- ALWAYS GET STA DETAILS FOR MED R & AFFID DEMOS  *           
* 16OCT98 53 EFJ -- FIX GETALPH BUG                                 *           
* 24SEP98 52 EFJ -- FILTER BUYS ON PROGRAM                          *           
* 09SEP98 51 EFJ -- ONLY READ CLEARANCE RECS FROM STDT FORWARD      *           
* 03SEP98 50 EFJ -- OPTION TO INCLUDE STATION LEVEL RECS IN READPW  *           
* 03SEP98 49 EFJ -- SECOND COST STUFF FOR BILLING                   *           
* 10AUG98 48 EFJ -- ONLY READ PW STA ONCE FOR NETWORK               *           
* 10JUL98 47 EFJ -- CROSS EST PW REPORTING                          *           
*                -- FIX IOTRACE FOR READ CALL                       *           
* 01JUL98 46 EFJ -- FIX INVOICE FOR CANADIAN NETWORK                *           
* 29MAY98 45 EFJ -- NEW PW READ MODE (SBPROCWP)                     *           
* 14MAY98 44 EFJ -- RDBILL CABLE FIX                                *           
* 12MAY98 43 EFJ -- MSR CABLE FILTER NOT QUITE RIGHT                *           
* 11MAY98 42 EFJ -- REMOVE TEMP FIX                                 *           
* 11MAY98 41 EFJ -- TEMP FIX FOR CABLE CONV FUCK UP                 *           
* 06MAY98 40 EFJ -- READ B1 PROFILE                                 *           
* 07APR98 39 EFJ -- NEW CABLE FILTER CODE                           *           
* 25MAR98 38 EFJ -- NOP OLD INVOICE SUPPORT                         *           
* 28JAN98 37 EFJ -- INCLUDE OFFC B FOR NE FILE                      *           
* 27JAN98 36 EFJ -- CHANGE OFFC EXCLUSIONS TO INCLUSIONS            *           
* 09JAN98 35 EFJ -- ADD NEW OFFC EXCLUSIONS TO XFILE FOR TR         *           
* 01DEC97 34 EFJ -- SET SBPRD & SBBPRD FOR PROCPW & PROCP2 IF POL   *           
* 22AUG97 33 EFJ -- FIX CABLE FILTERS FOR NEW INVOICES              *           
* 21AUG97 32 EFJ -- L26 STILL ISN'T QUITE RIGHT                     *           
* 15AUG97 31 EFJ -- L26 STILL ISN'T QUITE RIGHT                     *           
* 15AUG97 30 EFJ -- SUPPORT EXCLUDE CODES FOR CLT RECS              *           
* 15AUG97 29 EFJ -- DON'T DIE ON MISSING PGRP - SKIP CLT            *           
* 14AUG97 28 EFJ -- L26 WASN'T QUITE RIGHT...                       *           
* 12AUG97 27 EFJ -- ADD NEW OFFC EXCLUSIONS TO XFILE FOR TR         *           
* 11AUG97 26 EFJ -- FIX STATION GROUP REPORTING FOR INVOICE RECS    *           
* 15JUL97 25 EFJ -- READ SPECIAL MEDIA Z EQV REC FOR XFILE          *           
* 10JUL97 24 EFJ -- DON'T READ PW FOR SPILL MKTS                    *           
* 08JUL97 23 EFJ -- FIX MSR ALL- ALL/ READ BUG                      *           
* 25JUN97 21 EFJ -- FOR PW CABLE STA READ, DROP NETWORK             *           
* 22MAY97 20 EFJ -- WTF WAS I THINKING...                           *           
* 20MAY97 19 EFJ -- GET PW MKT REC IN READSLK TO GET DATES ONLY     *           
*                -- MOVE READSLK TO EXTRA                           *           
* 13MAR97 18 EFJ -- FIX GETSTREC TO WORK WITH LOW POWER STA'S       *           
* 28JAN97 17 EFJ -- HOOK TO USER WITH PROCPW EVEN IF NO PW MKT REC  *           
*                   TO CLEAR OUT DATE FIELDS                        *           
* 27DEC96 16 EFJ -- SUPPORT CLT GROUP LIMIT ACCESS                  *           
*                -- SQUEEZE A FEW MORE BYTES                        *           
* 18DEC96 15 EFJ -- SUPPORT SPECIAL MKT REPORTING FOR XFILE         *           
* 05DEC96 14 EFJ -- FILTER BUYS FOR SRC COMMENTS                    *           
* 05DEC96 13 EFJ -- RENUMBER READBUYS                               *           
* 07NOV96 12 EFJ -- SUPPORT CABLE FILTERS FOR NEW INVOICES          *           
* 05NOV96 11 EFJ -- MORE CABLE AGG CHANGES                          *           
* 24OCT96 10 EFJ -- SUPPORT BUY CREATION DATE FILTER                *           
* 23OCT96 09 EFJ -- SET SBBUYCH FLAGS                               *           
* 25SEP96 08 EFJ -- RE-READ MKT REC IF STATIONS MKT HAS CHANGED     *           
* 10SEP96 07 EFJ -- NOP BUYING GUIDLINE STATUS CODE                 *           
* 03SEP96 06 EFJ -- SUPPORT READING BUYING GUIDELINE STATUS RECORDS *           
* 29AUG96 05 EFJ -- REMOVE OFFICE R FROM NEOFC LIST                 *           
* 21AUG96 04 EFJ -- FIX BUG READING PW RECS                         *           
* 16AUG96 03 EFJ -- NEED TO HAVE ALT AGY CODE FOR ALT STAFILE!      *           
*                -- SKIP CLIENTS IF READING ACROSS FILES            *           
*                -- STATION'S MARKET IN STABUFF                     *           
*                -- USE STATION'S MKT IF READING ACROSS FILES       *           
* 06JUL96 02 EFJ -- PUT SF RANGE FILTER BACK TO WHAT IT WAS         *           
* 19JUL96 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
*                                                                   *           
* 19JUL96 87 EFJ -- DON'T DIE ON MISSING ACTIVE MKGRP POINTER       *           
* 16JUL96 86 EFJ -- SUPPORT READING ALTERNATE STATION FILE          *           
* 16JUL96 85 EFJ -- FIX BUG IN BUY TIME RANGE FILTER                *           
* 15JUL96 84 EFJ -- OPTION TO SKIP STA BIL RECS W/CUR=X'01'         *           
* 27JUN96 83 EFJ -- SUPPORT BUY TIME RANGE FILTER                   *           
* 26JUN96 82 EFJ -- DON'T SKIP CABLE FILTERS FOR MKTGRPS            *           
* 24JUN96 81 EFJ -- FILTER INVOICES BY PAID/UNPAID                  *           
*                -- PUT ALL INVOICE READ FLAGS IN SBEINV            *           
* 19JUN96 80 EFJ -- XSPFIL RECS ARE 4000 BYTES, NOT 2000            *           
* 17JUN96 79 EFJ -- CHECK SINGLE CLT REQ WHEN SKIPPING CLT...       *           
* 04JUN96 78 MFH -- RIP 31MAY96 (77)                                *           
* 31MAY96 77 EFJ -- SKIP POL POINTERS IF ALL PRDS & EST FILTER      *           
* 29MAY96 76 EFJ -- ALLOW APPL TO SKIP CLIENTS                      *           
* 29APR96 75 EFJ -- SUPPORT READING MATCHING STATUS RECORDS         *           
* 24APR96 74 EFJ -- FIX BUG SKIPPING CLEARANCE STATUS RECS          *           
* 15APR96 73 EFJ -- SUPPORT LARGER ESTBUF                           *           
* 11APR96 72 EFJ -- SKIP CLT CC FOR AGY CK CLIENT ALL REQUEST       *           
* 10APR96 71 EFJ -- FILL IN UDEF STUFF FROM ESTBUF IN PRDEST        *           
* 09APR96 70 EFJ -- SUPPORT INVOICE MIDNIGHT CLEARANCE FILTER       *           
* 02APR96 69 EFJ -- FIX INVOICE AUDIT CODE                          *           
* 02APR96 68 EFJ -- PROCESS PW MKT BEFORE STA WHEN NEW MKT          *           
* 25JAN96 67 EFJ -- INVOICE AUDIT                                   *           
* 27DEC95 66 EFJ -- SUPPORT CLT CODE AS AAN                         *           
* 14DEC95 65 EFJ -- SFI!  FIX BUG IN PREV FIX                       *           
* 11DEC95 64 EFJ -- FILL IN SBBMPRD                                 *           
*                -- MOVE BUY GETREC EARLIER (NEED BDMASPRD 4 FSTA)  *           
* 05DEC95 63 EFJ -- USE SBQBKTYP IN GETALPH                         *           
* 24OCT95 62 EFJ -- SUPPORT READING INVOICE RECORDS                 *           
* 20OCT95 61 EFJ -- DON'T SEND *OFFICE LIMIT TO OFFICER             *           
* 12OCT95 60 EFJ -- SKIP CLIENTS NOT VALID FOR OFFICE FOR CLT GROUP *           
* 14SEP95 59 EFJ -- FIX TIME FILTERS - DAY IS FROM 0600-0559        *           
* 30AUG95 58 EFJ -- FIX BUG IN READCLS STAPACK CALL                 *           
* 21AUG95 57 EFJ -- CLEAR SBAESTBF WHEN CLEARING SBAESTTB           *           
* 27JUN95 56 EFJ -- SUPPORT CLTPCT KEYWORD                          *           
* 19JUN95 55 EFJ -- REMOVE AFFID TIME FILTER (NOW IN WRI01)         *           
*                -- DON'T DIE ON MGR NOT FOUND IF MEDIA=*           *           
* 01JUN95 54 EFJ -- PASS FLAG IN HOB OF SBACURCH IF GETPW CALLED    *           
*                   FROM READGOLS                                   *           
* 12MAY95 53 EFJ -- FILL IN UDEF STUFF FROM ESTBUF IN READBUYS      *           
* 12MAY95 52 EFJ -- PW MAREKT FIX FIX                               *           
* 25APR95 51 EFJ -- FIX BUG WITH E ESTIMATES AND PW                 *           
* 17APR95 50 MH  -- FORCE NEXT ESTIMATE IF NO DATA                  *           
* 17APR95 49 EFJ -- READ PW DATA WHEN PROCESSING GOALS              *           
* 07APR95 48 EFJ -- SUPPORT FILTERING BUYS ON AFFID TIME            *           
* 06APR95 47 EFJ -- CHANGE STAPACK TO NOT DIE IF FLAG SET           *           
* 29MAR95 46 MH  -- ANOTHER CANAD NET BILLING BUG !                 *           
* 28MAR95 45 EFJ -- CALL PROCP2 AFTER BREAKING OUT X'06' ELEM       *           
*                -- SEPERATE CALLS FOR MKT & STA PW RECS            *           
* 27MAR95 44 MH  -- FIX BUG READING CAN NTWK BILL RECS              *           
* 27MAR95 43 EFJ -- PASS A(PW REC) INSTEAD OF A(ELEM)               *           
* 21MAR95 42 EFJ -- MORE PFW SHIT                                   *           
* 14MAR95 41 EFJ -- SUPPORT WIPW OVERRIDE $$$                       *           
* 03MAR95 40 EFJ -- MORE PW SUPPORT                                 *           
* 17FEB95 39 GEP -- SUPPORT NEW INVOICE RECORDS                     *           
* 17FEB95 38 SMP -- SUPPORT NEW BDCIND EQUATES                      *           
* 30JAN95 37 EFJ -- ENLARGE CNTWKTAB TO 128 ENTRIES                 *           
*                -- NETWORK NOW FULL BYTE, NOT 5 BITS               *           
* 27JAN95 36 EFJ -- REMOVE ABOVE FIX                                *           
*                -- FIX FILTERING BY NET PROBLEM FOR CANADA         *           
* 24JAN95 35 EFJ -- TEMP FIX FOR CANADA BAD BUYS                    *           
* 17JAN95 34 EFJ -- PASS DELETED BILL RECS IF FLAG SET (THANKS MEL) *           
*                -- MOVE CODE TO EXTRA                              *           
* 03JAN95 33 EFJ -- YASF PW BUG                                     *           
* 29DEC94 32 EFJ -- SF BUG - CALLING GETPW WHEN SHOULDN'T BE        *           
* 22DEC94 31 EFJ -- SUPPORT OPTION TO INCLUDE/EXCLUDE RETAIL        *           
*                   CONTROL BILLING                                 *           
*                -- SQUEEZE OUT A FEW MORE BYTES                    *           
* 12DEC94 30 EFJ -- SUPPORT TSAROFF                                 *           
*                -- READ WIPW DATA INTO SBPWAREA                    *           
*                -- DON'T TEST PRD IN KEY FOR SBEPWCH - USE MASPRD  *           
* 12DEC94 29 EFJ -- DON'T PASS DELETED BILL RECS (PER MHER)         *           
* 08DEC94 28 EFJ -- DON'T DIE ON ALPHAMKT REC NOT FOUND             *           
*                -- USE IO2 FOR ALPHAMKT REC                        *           
* 05DEC94 27 EFJ -- DON'T USE L'MKTREC TO READ MKTREC               *           
* 30NOV94 26 EFJ -- YASF PW BUG                                     *           
* 08NOV94 25 EFJ -- FIX MEDIA * BUG ACROSS ALL CLT'S                *           
* 07NOV94 24 EFJ -- NEW ENTRY IN STABUFF FOR RADIO DEMO SUPPORT     *           
*                -- CODE TO GET RTG SVC MKT# FROM ALPHAMKT REC      *           
* 27OCT94 23 EFJ -- FIX SF 'B  *+'  BUG                             *           
* 20OCT94 22 EFJ -- SAVE A(STABUFF ENTRY) ON GETSTA                 *           
*                -- NEW ENTRIES IN STABUFF                          *           
*                -- FIX LENGTH OF WORK (AND STUFF THAT DOES L'WORK) *           
* 11OCT94 21 EFJ -- SUPPORT DMA OVERRIDE                            *           
* 27SEP94 20 EFJ -- FILTER FOR 'E' (CONTROL) TYPE ESTIMATES         *           
* 23SEP94 19 EFJ -- RESTORE KEY AFTER READING STA REC *ALWAYS*      *           
* 22SEP94 18 EFJ -- UPDATE IOTRACE TO ACCEPT STAFIL CALLS           *           
* 21SEP94 17 EFJ -- IO2 WAS BAD CHOICE FOR READING 3E REC           *           
* 12SEP94 16 EFJ -- SUPPORT FOR STAPACK (INTERCEPT MSPACK/MSUNPK)   *           
* 02SEP94 15 EFJ -- CODE TO SUPPORT EASI STA LIST FROM CTFIL 3E REC *           
* 31AUG94 14 EFJ -- FIX BUG FOR READING LOCK $$ FOR PW              *           
* 14JUL94 13 EFJ -- SUPPORT WESTERN PROFIT WITHIN KEYWORDS          *           
* 24JUN94 12 EFJ -- INCLUDE MEDIA 'X' IN THE COMBO FOR MEDIA '*'    *           
* 09JUN94 11 EFJ -- STOP DUMP IN RH8 FROM BAD BH PROD RECS          *           
*                -- SAME FOR RS63                                   *           
* 19MAY94 10 TCS -- MOVE EIX STATION LIST TO BOOK SPEIXSTA          *           
* 15MAR94 09 EFJ -- DON'T DIE ON DELETED INV REC (PATCHED 15MAR)    *           
* 11MAR94 08 TCS -- FIX BUG IN READING CLIENT RECORD FOR ONLINE     *           
* 10MAR94 07 TCS -- ADD HARD CODED EIX STATION GROUP                *           
* 22FEB94 06 TCS -- ADD NETWORK TO CANADIAN MEDIA=* REQUEST         *           
* 15FEB94 05 EFJ -- INFOMERCIAL RE-WORK                             *           
* 20JAN94 04 TCS -- CLEAR DAYPART TABLES BUFFER FOR MEDIA=*         *           
* 18JAN94 03 TCS -- ENSURE STATION GROUP IS SET IF IT'S NEEDED      *           
* 12JAN94 02 EFJ -- DON'T PASS DELETED INFOMERCIAL RECS             *           
*                -- REMOVE REFERENCE TO SBQROINF                    *           
* 05JAN94 00 EFJ -- HISTORY LOST.  LEVEL RESET.  ADD 'A' TO PHASE   *           
*                                                                   *           
*********************************************************************           
