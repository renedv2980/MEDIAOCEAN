*          DATA SET REDEMUP    AT LEVEL 131 AS OF 08/06/18                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T00A08A                                                                  
*INCLUDE DEINMKT                                                                
         TITLE 'T00A08 - REPPAK DEMO UPGRADE ROUTINES'                          
***********************************************************************         
*  HISTORY OF CHANGES:                                                *         
***********************************************************************         
*  JUL31/91 (BU ) --- INCORPORATE TEST FOR NEW FORMAT DEMO OVERRIDE   *         
*                                                                     *         
*  AUG09/94 (RZ)  --- SUPPORT MULTIPLE IUN DISPLACEMENT TABLES        *         
*                                                                     *         
*  JUN01/95 (RZ)  --- SUPPORT IMPRESSION BASED UPGRADES               *         
*                                                                     *         
*  DEC06/95 (RZ)  --- SUPPORT DAY/TIME LIST HPT LOOKUPS               *         
*                                                                     *         
*  JUL19/95 (RZ)  --- SUPPORT IMP BASED UPGRADES (66)                 *         
*                                                                     *         
*  JAN14/97 (RZ)  --- SUPPORT RIB,RDB PARAMETERS (137)                *         
*                                                                     *         
*  MAR21/97 (RZ)  --- RESET LEVEL NUMBERS                             *         
*                     DECIMAL PRECISION UPGRADES                      *         
*                                                                     *         
*  APR08/98 (MTA) --- SUPPORT NHTI FILE                               *         
*                     NOTE: INDEXFF N/A FOR NHTI EXITS IN GETHPT RTN  *         
*                                                                     *         
*  JUN08/00 (GL)  --- FEATURE REQUEST FROM REPPAK:                    *         
*                       FOR UPGRADES PUT,HUT,HPT,MMU,PINDEX,PAVG,SAVG,*         
*                       AND SINDEX, WE WANT TO KEEP THE ORIGINAL HOMES*         
*                       SHARE THAT WAS PROVIDED ON THE TAPE           *         
*                       THE DEC/90 & JUN/91 'S0' FORMULAE WERE ALSO   *         
*                       CHANGED TO ALLOW DIRECT TRANSFERS             *         
*                                                                     *         
*  JUN22/00 (GL)  --- THE SHARES CALCULATIONS IN THE 'GETYRS' ROUTINE *         
*                       WERE CHANGED TO USE THE ORIGINAL HOMES SHARES *         
*                                                                     *         
* NOV23/04 (BPOO)  - FOR MMU UGRADE WHEN WE ARE GETTING PUTS FOR      *         
*                    P BOOKS - DEFAULT TO SAME YEAR DIARY IF P BOOK   *         
*                    DOESNT EXIST                                     *         
*                                                                     *         
* JAN07/11 (SKUI) --- SUPPORT FOR NEW INVENTORY KEY                   *         
***********************************************************************         
DEMUP    RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DEMUPDX-DEMUPD,**DEMUP,RA,R9,CLEAR=YES,RR=RE                     
         USING DEMUPD,RC           RC=A(W/S)                                    
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
**       LM    R7,R9,0(R1)                                                      
         LM    R7,R8,0(R1)                                                      
         MVC   FROMTYPE,0(R1)      SAVE 'FROM' RECORD TYPE                      
         MVC   ASAVE,12(R1)        SAVE A(USER SAVE AREA)                       
         XC    ASHARES,ASHARES     SAVE A(PASSED SHARES)                        
         MVC   ASHARES,16(R1)      SAVE A(PASSED SHARES)                        
***      USING COMFACSD,R9         R9=A(COMFACS)                                
         L     RF,8(R1)            RF=A(COMFACS)                                
         ST    RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOMTH,CDEMOMTH                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VDEMAINT,CDEMAINT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VDEMTABS,CDEMTABS                                                
         MVC   DATAMGR,CDATAMGR                                                 
                                                                                
         DROP  RF                                                               
         BRAS  RE,INIT2                                                         
*                                                                               
DEM2     LR    R1,R7                                                            
         LA    RE,PRFRSTEL-PRKEY                                                
         SR    R1,RE                                                            
         ST    R1,ARECORD                                                       
         LA    RE,PRRLEN-PRKEY(R1)                                              
         ST    RE,ARECLEN                                                       
         MVC   FILEVALS,PAVVALS                                                 
         CLC   0(3,R1),=C'QNN'      NETWORK RECORD?                             
         JE    DEM8                                                             
         CLI   0(R1),C'P'          TEST FOR PAV DEMO RECORD                     
         JE    DEM8                                                             
         MVC   FILEVALS,TPTVALS                                                 
         CLI   0(R1),C'T'          TEST FOR TIME PERIOD                         
         JE    DEM8                                                             
         LR    R1,R7                                                            
         LA    RE,RINVPEL-RINVREC                                               
         SR    R1,RE                                                            
         ST    R1,ARECORD                                                       
         LA    RE,RINVLEN-RINVD(R1)                                             
         ST    RE,ARECLEN                                                       
         MVC   FILEVALS,INVVALS                                                 
         CLI   0(R1),RINVKTYQ      TEST FOR REP INVENTORY RECORD                
         JE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETELS           GET ELEMENT ADDRESSES                        
         ICM   R1,15,ABOOKEL       TEST IF BOOK ELEMENT PRESENT                 
         JNZ   DEM8                NO - ADD ONE                                 
         MVC   WORK(7),BOOKEL      START WITH DEFAULT BOOK ELEMENT              
         CLI   TAPEOPT,C'Y'                                                     
         JNE   *+10                                                             
         MVC   WORK+5(2),=X'5A0B'                                               
         GOTO1 VHELLO,DMCB,(C'P',FILENAME),ARECORD,WORK,0                       
         J     DEM8                                                             
         SPACE 1                                                                
ERROR    CLI   DBERROR,0           TEST FOR DEMO ERRORS                         
         BER   RE                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
                                                                                
EXIT     L     RE,UPDB          SAVE ERROR CODE IF WE ENCOUNTERED               
         OR    RE,RE            A PROBLEM                                       
         BZ    *+18                                                             
         CLI   DBERROR,0                                                        
         BE    *+10                                                             
         MVC   DBERROR-DBLOCK(L'DBERROR,RE),DBERROR                             
         XIT1                      EXIT FROM ROUTINE/MODULE                     
         EJECT                                                                  
***********************************************************************         
* FOR INVENTORY RECORDS IN IUN FORMAT - EXPLODE INPUT RECORD INTO     *         
* IUNREC VIA DEMOUT.                                                  *         
*                                                                     *         
* FOR NON IUN FORMAT RECORDS - GET UNIVERSE & OLD RATINGS LINE FROM   *         
* INPUT RECORD, GET OLD H/P/T LINE FROM THE DEMO FILES (ONLY IF OLD   *         
* DATA IS NOT PRESENT ON RECORD).                                     *         
*                                                                     *         
* HOMES SHARES VALUES WILL BE CALCULATED IF NECCESSARY.               *         
***********************************************************************         
         SPACE 1                                                                
DEM8     BAS   RE,GETELS           GET A(INPUT RECORD ELEMENTS)                 
*                                  SET INDEX=FF FLAG                            
         MVI   INDEXFF,C'N'                                                     
         ICM   R1,15,AUPGDEL                                                    
         USING RAVLNEL,R1                                                       
*                                                                               
         MVI   BOOKI,0             SET UP FOR                                   
         XC    BOOKLIST,BOOKLIST   A LIST OF BOOKS                              
         MVC   BYTE,RAVLNTYP                                                    
         NI    BYTE,B'11011111'    ZAP INVENTORY BIT                            
         CLI   BYTE,X'0B'          AND TEST NEW TYPE UPGS                       
         JE    *+8                                                              
         CLI   BYTE,X'0C'                                                       
         JE    *+8                                                              
         CLI   BYTE,X'0D'                                                       
         JE    *+8                                                              
         CLI   BYTE,X'0E'                                                       
         JNE   DEM8D                                                            
         MVI   Y4FSW,0                                                          
         TM    RAVLNTYP,X'20'                                                   
         JZ    *+8                                                              
         MVI   Y4FSW,C'I'                                                       
         CLI   RAVLNCAT,C'M'                                                    
         JNE   *+12                                                             
         MVI   Y4MSW,C'Y'                                                       
         J     *+12                                                             
         CLI   RAVLNCAT,C'N'                                                    
         JNE   DEM8C                                                            
         ZIC   RE,1(R1)                                                         
         SH    RE,=H'7'                                                         
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   BOOKLIST(8),6(R1)   4BOOK MAX                                    
         SR    RF,RF                                                            
         LA    RE,BOOKLIST(RF)                                                  
         CLC   0(2,RE),=XL2'00'                                                 
         JE    *+12                                                             
         LA    RF,2(RF)                                                         
         J     *-18                                                             
         SRL   RF,1                                                             
         STC   RF,Y4SYRS                                                        
         J     DEM8D                                                            
*                                                                               
DEM8C    MVC   BOOKLIST(2),RAVLNOP1   START BOOK/NUMBER OF BOOKS                
         MVC   Y4SYRS(1),RAVLNOP2+1                                             
*                                                                               
DEM8D    MVC   RAVTMPTY,RAVLNTYP                                                
         TM    RAVLNTYP,X'C0'                                                   
         JNM   *+8                                                              
         XI    RAVTMPTY,X'40'                                                   
*                                                                               
         OI    RAVLNTYP,0     <----FOR TESTING                                  
         CLI   RAVLNTYP,X'44'                                                   
         JE    *+12                                                             
         CLI   RAVLNTYP,4                                                       
         JNE   DEM9                                                             
         CLI   RAVLNOP1,X'FF'                                                   
         JNE   DEM9                                                             
         MVI   INDEXFF,C'Y'                                                     
         MVC   HPTWT,RAVLNOP2      SAVE WEIGHT/A(DBLOCK)/TYPE                   
         MVC   ADBLOCK,RAVLNOP3                                                 
         MVC   INDEXTYP,RAVLNCAT                                                
         MVC   INDEXFIL,RAVLNBKS                                                
         DROP  R1                                                               
*                                                                               
DEM9     L     R1,VCOMFACS                                                      
         L     R1,CT00AD0-COMFACSD(R1)  R1=A(MASTER DISP. TABLE)                
         LA    R1,16(R1)           GO PAST HEADER                               
*                                  LOCATE IUN DISP. TABLE ENTRY                 
         USING DSPHDRD,R1                                                       
DEM10    CLC   0(2,R1),=XL2'00'    TEST E-O-T                                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DSPFILE,C'I'        MATCH ON INTERNAL FILE CODE                  
         JE    *+16                                                             
         ICM   RE,7,DSPAET                                                      
         LA    R1,1(RE,R1)         BUMP TO NEXT TABLE HEADER                    
         J     DEM10                                                            
*                                                                               
         CLI   TAPEOPT,C'Y'        IMPRESSION BASED UPGRADES                    
         JE    DEM11                                                            
         CLC   DSPSBOOK,=X'B0F4'   BOOK BASE DISP TABLE                         
         JE    DEM11                                                            
         ICM   RE,7,DSPAET                                                      
         LA    R1,1(RE,R1)         BUMP TO NEXT TABLE HEADER                    
         J     DEM10                                                            
*                                                                               
DEM11    LA    R1,DSPHDRLN(R1)     BUMP PAST HEADER                             
         ST    R1,ADISPTAB         SAVE A(FIRST DEMO ENTRY)                     
         DROP  R1                                                               
*                                  GET OLD RATING LINE VALUES                   
DEM12    MVC   DBFILE,FILEDEM      BUILD DBLOCK FOR LOOKUP/EXPLODE              
         MVC   DBAREC,ARECORD                                                   
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBTAPEP,TAPEOPT     IMPRESSION/RATING PRECISION                  
         MVC   DBTIMCHG,UPTIMCHG                                                
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
*        CLI   FILETYPE,C'I'                                                    
*        JNE   *+12                                                             
*        MVI   DBSELMED,C'U'       IF INV FILE, DEFAULT TO 'U'                  
*        J     *+8                                                              
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
         ICM   R1,15,ABOOKEL                                                    
         JZ    DEM14                                                            
         CLC   RIBFILE-RIBELEM(3,R1),=C'NHN'                                    
         JNE   DEM12INV                                                         
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         L     R1,ARECORD                                                       
         MVC   DBSELSTA,PMSTAT-PMKEY(R1)   PRG RECD,GET STATION                 
         CLI   0(R1),C'P'                                                       
         JNE   *+10                                                             
         MVC   DBSELSTA,PRSTAT-PRKEY(R1)   TP RECD,GET STATN                    
*                                                                               
*                                  IUN FORMAT IS EXPLODE VIA DEMOUT             
DEM12INV CLI   RIBFILE-RIBELEM(R1),C'I'                                         
         JNE   DEM16                                                            
         GOTO1 VGETIUN,DMCB,(9,DBLOCK),IUNREC                                   
         MVI   IUNFLAG,C'Y'        SET IUN RECORD FLAG                          
*--->    OC    UOQHOMES,UOQHOMES   TEST IF QHOMES PRESENT                       
*--->    JZ    DEM18               NO - GO GET H/P/T LINE                       
         J     DEM20               ELSE ASSUME OLD H/P/T LINE PRESENT           
*                                  DEMO FORMAT CALLS DEMOUT FOR VALUES          
DEM14    CLI   FILETYPE,C'P'       NO BOOK ELEMENT - MUST BE PAV DEMO           
         JNE   DEM15                                                            
         L     R1,ARECORD                                                       
         USING PRKEY,R1                                                         
         MVC   DBINTFIL(2),PRCODE  SET FILE/MEDIA/SOURCE/BOOK FROM KEY          
         MVC   DBACTSRC,PRSRC                                                   
         MVC   DBACTBK,PRBOOK                                                   
         J     DEM16                                                            
         DROP  R1                                                               
*                                  TIME PERIOD DEMO FORMAT                      
DEM15    CLI   FILETYPE,C'T'                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ADBLOCK          FIX DBLOCK FOR TIME PERIOD                   
         MVC   DBINTFIL(2),DBINTFIL-DBLOCK(R1)                                  
         MVC   DBSELBK,DBSELBK-DBLOCK(R1)                                       
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
*                                  CALCULATE UNIVERSE LINE                      
DEM16    LA    R3,1                SET CALL TYPE FOR GETIUN                     
         XC    NETSHRV,NETSHRV                                                  
         CLI   INDEXFF,C'Y'                                                     
         JNE   DEM16F                                                           
         CLI   DBSELMED,C'N'                                                    
         JNE   DEM16F                                                           
         L     R1,ASHARES                                                       
         MVC   HOMSHR(HOMSHRLN),0(R1)    WERE WE PASSED SHRS BY CALLER          
         OC    HOMSHR(HOMSHRLN),HOMSHR                                          
         JNZ   DEM16F                                                           
         GOTO1 VDEMOUT,DMCB,(C'L',NETSHRS),DBLOCK,NETSHRV,0                     
*                                                                               
*                                                                               
DEM16F   CLI   INDEXFIL,C'T'       TEST FOR TIME PERIOD                         
         JE    *+8                                                              
         LA    R3,4                                                             
         GOTO1 VGETIUN,DMCB,((R3),DBLOCK),OLDUNV                                
*                                  INDEX=FF HAS SHARES PASSED                   
         L     R1,ASHARES                                                       
         CLI   INDEXFF,C'Y'                                                     
         JNE   DEM17                                                            
         MVC   HOMSHR(HOMSHRLN),0(R1)                                           
         OC    HOMSHR(HOMSHRLN),HOMSHR                                          
         JNZ   DEM18                                                            
*                                  CALL DEMOUT TO GET HOMES SHARES              
DEM17    DS    0H                                                               
         CLI   DBSELMED,C'N'                                                    
         JNE   *+14                                                             
         MVC   HOMSHR(HOMSHRLN),NETSHRV  WE'VE ALREADY LOOK THEM UP             
         J     DEM18               SO JUST SLOT AND EXIT                        
         L     R1,ADISPTAB                                                      
         LA    R2,((HOMSHR-IUNREC)/4)*5(R1)                                     
         LA    R3,((HOMSHRX-IUNREC)/4)*5(R1)                                    
         GOTO1 VDEMOUT,DMCB,(C'M',(R2)),DBLOCK,HOMSHR,(R3)                      
*                                  GET OLD H/P/T LINE IF REQUIRED               
DEM18    MVI   HPTFLAG,C'O'        SET TO GET OLD H/P/T LINE                    
         LA    R1,=H'500'                                                       
         CLI   INDEXFF,C'Y'        TEST FOR INDEX=FF CALL                       
         JNE   *+8                                                              
         BAS   RE,GETHPT           GET OLD H/P/T VALUES                         
*                                  ROUND OLD VALUES                             
DEM20    CLI   INDEXFF,C'Y'                                                     
         JE    DEM100                                                           
         MVC   INDEX,=F'10000'                                                  
*                                                                               
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,OLDRTG                        
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDHPT,0,OLDHPT                        
*                                                                               
*        GOTO1 CALC,P1,OLDRTG,0,OLDRTG                                          
*        GOTO1 CALC,P1,OLDHPT,0,OLDHPT                                          
*                                                                               
         XC    INDEX,INDEX         CLEAR INDEX VALUE                            
**       J     DEM40                                                            
         EJECT                                                                  
**DEM40    BAS   RE,GETSHR           GET OLD SHARES                             
GET40    GOTO1 VSUBR01,P1,('GETSHRQ',(RC))                                      
         MVI   HPTFLAG,C'N'        SET TO GET NEW H/P/T LINE                    
*                                  IF FROM RECORDS WERE INVENTORY               
*                                  USE NEW RATINGS FOR UPGRADE                  
         CLI   FROMTYPE,C'I'                                                    
         JNE   DEM42                                                            
         MVC   SAVRTG(OLDRTGLN),OLDRTG                                          
         MVC   OLDRTG(OLDRTGLN),NEWRTG                                          
*                                                                               
DEM42    L     R3,AUPGDEL                                                       
         USING RAVLNEL,R3          R3=A(UPGRADE ELEMENT)                        
*                                  SPECIAL CODE FOR TIME COMBOS                 
         ICM   R1,15,AINVCEL                                                    
         JZ    DEM50                                                            
         TM    RINVCTYP-RINVCEL(R1),X'80'                                       
         JZ    DEM50                                                            
         LA    R1,=H'500'                                                       
         BAS   RE,GETHPT           GET NEW H/P/T LINE                           
*                                  SET NEW RATINGS=OLD RATINGS                  
         MVC   NEWRTG(OLDRTGLN),OLDRTG                                          
         J     DEM100                                                           
         EJECT                                                                  
DEM50    CLI   RAVLNTYP,X'42'      DEC PRECISION INPUT                          
         JE    *+12                                                             
         CLI   RAVLNTYP,2                                                       
         JNE   DEM60                                                            
         OC    RAVLNOP2,RAVLNOP2   TEST IF SHARE PRESENT                        
         JZ    *+14                                                             
         CLC   RAVLNOP2,=H'1000'   TEST SHARE VALUE PRESENT                     
         JL    DEM52               HIGH IS A BOOK -- NOT A SHARE                
         SPACE 2                                                                
**********************************************************************          
* RATING ONLY UPGRADE                                                *          
* ADJRTG = RTGINX * OLDRTG                                           *          
* OUTPUT = OLDRTG / OLDHPT / OLDRTG / NEWHPT                         *          
* OVERRIDE RATING - CALCULATE SHARE OVERRIDE                         *          
**********************************************************************          
         SPACE 2                                                                
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU            NORMALIZE UNIVERSES                          
*                                  CREATE HOMES OVERRIDE                        
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET NEW RHOMES                               
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  COMPUTE HOMES INDEX                          
         L     R0,UORHOMES                                                      
         CLI   TAPEOPT,C'Y'        IMP BASEED                                   
         JNE   UPRTG2                                                           
         LR    RF,R0               IMP BASED                                    
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
*                                                                               
UPRTG2   BAS   RE,GETINDEX                                                      
*                                  GET ADJUSTED RATINGS                         
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*                                  CALCULATE SHARE OVERRIDE                     
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET NEW RATING HOMES                         
         TM    RAVLNTYP,X'C0'                                                   
         JM    *+8                                                              
         MH    R1,=H'10'           SCALING FOR DIVISION TO DERIVE SHARE         
         L     R0,UOPHOMES         OLD HUT                                      
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPRTG3                                                           
         LR    RF,R0               IMP BASED                                    
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
UPRTG3   TM    RAVLNTYP,X'C0'                                                   
         JNM   *+8                                                              
         MH    R1,=H'10'           SCALING FOR DIVISION TO DERIVE SHARE         
         BAS   RE,GETVALUE                                                      
         MVC   DUB(2),SHOMES                                                    
         TM    RAVLNTYP,X'C0'                                                   
         JM    *+8                                                              
         MH    R1,=H'10'           SCALING FOR OVERRIDE ELEMENT                 
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER                                                       
         XC    HOMSHR(HOMSHRLN),HOMSHR  FORCE A CALCULATION                     
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
**********************************************************************          
* RATING/SHARE UPGRADE                                               *          
* ADJRTG = RTGINX * OLDRTG                                           *          
* OUTPUT = ADJRTG / ADJHPT / ADJRTG / NEWHPT                         *          
* OVERRIDE RATING/SHARE - CALCULATE HUT OVERRIDE                     *          
**********************************************************************          
         SPACE 2                                                                
DEM52    LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU            NORMALIZE UNIVERSES                          
*                                  CREATE HOMES OVERRIDE                        
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET NEW RHOMES                               
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),RHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  COMPUTE HOMES INDEX                          
         L     R0,UORHOMES                                                      
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPRTSH2                                                          
         LR    RF,R0               IMP BASED                                    
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
*                                                                               
UPRTSH2  BAS   RE,GETINDEX                                                      
*                                  CALCULATE ADJ RATINGS                        
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*                                  CREATE SHARE OVERRIDE                        
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP2       GET NEW SHARE                                
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  CALCULATE NEW HUT OVERRIDE                   
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       NEW RATING                                   
         MH    R1,=H'10'            1 DEC PREC. ADJ ALSO                        
         LH    R0,DUB+2            NEW SHARE                                    
         BAS   RE,GETVALUE                                                      
         MVC   DUB(2),PHOMES                                                    
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         STH   R1,DUB+2                                                         
         STH   R1,SAVHUT                                                        
         BAS   RE,ADDOVER                                                       
*        XC    HOMSHR(HOMSHRLN),HOMSHR  FORCE A CALCULATION                     
* ADJUST THE PUTS/TOTS                                                          
         MVC   DUB(4),INDEX        SAVE CURRENT INDEX                           
         SR    R0,R0                                                            
         LH    R1,SAVHUT           NEW HUT                                      
         CLI   TAPEOPT,C'Y'        IMP BASED                                    
         JNE   UPRTSH4                                                          
         L     R1,UNPHOMES         GETS ADJUSTED BY UNIVERSE                    
         MH    R1,=H'100'                                                       
         D     R0,UOUHOMES                                                      
         SR    R0,R0                                                            
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         LR    R0,R1               SLOT PROPERLY                                
         LH    R1,SAVHUT           RESTORE NEW                                  
         J     *+8                                                              
UPRTSH4  L     R0,UNPHOMES                                                      
         BAS   RE,GETINDEX                                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*&&DO                                                                           
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWTOTS,0,NEWTOTS                      
*&&                                                                             
         MVC   INDEX(4),DUB                                                     
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
         SPACE 2                                                                
DEM60    CLI   RAVLNTYP,6          TEST HPT UPGRADE                             
         JE    DEM60X                                                           
         CLI   RAVLNTYP,X'46'      TEST HPT UPGRADE                             
         JE    DEM60X                                                           
         CLI   RAVLNTYP,3                                                       
         JE    *+12                                                             
         CLI   RAVLNTYP,X'43'                                                   
         JNE   DEM80                                                            
         CLI   RAVLNCAT,C'P'       TEST PUT UPGRADE                             
         JNE   DEM62                                                            
         SPACE 2                                                                
**********************************************************************          
* PUT UPGRADE                                                        *          
* NEWHPT = BOOK VALUES                                               *          
* ADJRTG = OLDSHR * NEWHPT                                           *          
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                         *          
**********************************************************************          
         SPACE 2                                                                
DEM60X   LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT           GET NEW HPT                                  
         CLI   RAVLNTYP,X'46'      TEST HPT UPGRADE                             
         JE    *+12                                                             
         CLI   RAVLNTYP,6          TEST HPT UPGRADE                             
         JNE   UPHUTU                                                           
         CLI   RAVLNCAT,C'V'       VALUES GIVEN                                 
         JE    UPYP2                                                            
         CLC   RAVLNOP3,=H'227'    MMU (SPECIAL CASE)                           
         JE    UPYP4                                                            
         CLC   RAVLNOP2,=H'227'    MMU (SPECIAL CASE)                           
         JE    UPYP4                                                            
         J     UPYP3               INDEX GIVEN                                  
         SPACE 1                                                                
UPYP2    DS    0C                                                               
* INDEX HPTS BY VALUE IN RAVLNOP3 FOR HPV UPGRADE *                             
         L     R1,UNPHOMES                                                      
         LR    R0,R1               SET UP FOR INDEX                             
         CLI   TAPEOPT,C'Y'        IMP BASED - NEED TO CALC OLD                 
         JNE   UPYP2A                                                           
         LR    RF,R1                                                            
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    LUHOMES,LUHOMES                                                  
         JZ    *+12                                                             
         D     RE,LUHOMES                                                       
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF               SET UP FOR INDEX                             
         SR    R1,R1                                                            
UPYP2A   ICM   R1,3,RAVLNOP3                                                    
         BAS   RE,GETINDEX                                                      
         J     UPYP3A                                                           
*                                                                               
         SPACE 1                                                                
UPYP3    SR    R0,R0               SET INDEX VALUE                              
         ICM   R0,3,RAVLNOP3                                                    
         MH    R0,=H'100'          SCALE TO 2 DEC PLACES                        
         ST    R0,INDEX                                                         
UPYP3A   GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*        GOTO1 CALC,P1,NEWHPT,0,NEWHPT                                          
         J     UPHUTU                                                           
         SPACE 1                                                                
UPYP4    DS    0C                                                               
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
UPHUTU   BRAS  RE,NORMU            NORMALIZE UNIVERSES                          
*                                                                               
*   CALCULATE NEW RATINGS                                  *                    
*   NOTE PARAM 1 ROUNDED BY CALC SO OLDSHR MUST BE PARAM 2 *                    
         SPACE 1                                                                
DEM61    GOTO1 VSUBR01,P1,('PUTSHRQ',(RC))                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,OLDSHR,ADJRTG                   
*        GOTO1 CALC,P1,NEWHPT,OLDSHR,ADJRTG                                     
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         XC    INDEX,INDEX                                                      
         MVI   PUTUPGD,C'Y'                                                     
         CLI   PUTSHRD,C'Y'                                                     
         JNE   DEM100                                                           
         MVC   OLDRTG(NEWRTGLN),NEWRTG                                          
         MVC   SAVRTG(NEWRTGLN),NEWRTG                                          
         MVC   OLDHPT(NEWHPTLN),NEWHPT                                          
         J     DEM100                                                           
         EJECT                                                                  
DEM62    OC    RAVLNOP2,RAVLNOP2   TEST IF SHARE PRESENT                        
         JZ    *+14                                                             
         CLC   RAVLNOP2,=H'1000'   TEST SHARE GIVEN                             
         JL    DEM66               YES                                          
         SPACE 2                                                                
**********************************************************************          
* HUT ONLY UPGRADE                                                   *          
* NEWRTG(HMS) = OLDSHR * NEWHUT                                      *          
* ADJRTG      = RTGINX * OLDRTG                                      *          
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / OLDHPT                         *          
* OVERRIDE HUT - CALCULATE RATING OVERRIDE                           *          
**********************************************************************          
         SPACE 2                                                                
         CLC   RAVLNOP1,=H'1000'   DETERMINE WHICH HPT LINE                     
         JH    *+16                                                             
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU                                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET HUT INDEX                                
         CLC   RAVLNOP1,=H'1000'   TEST HUT = INDEX OR BOOK                     
         JL    DEM64                                                            
*                                                                               
*                                  BOOK - GET NEW HUT VALUE                     
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         OC    DUALTRMK,DUALTRMK   PUNT AND ADJUST UNIVS IF                     
         BZ    *+8                  A MARKET OVERRIDE IS PRESENT                
         BRAS  RE,NORMM                                                         
         L     R1,UNPHOMES                                                      
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         JNE   DEM63                                                            
         LR    RF,R1               IMP BASED                                    
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    LUHOMES,LUHOMES                                                  
*&&DO                                                                           
*--------------------------------   REPLACE THIS CODE TO GET THE                
         JZ    *+12             |   SAME PUTS FROM PUT UPGRADES                 
         D     RE,UOUHOMES      |                                               
         D     RE,LUHOMES       |                                               
         J     *+8              |                                               
*--------------------------------                                               
*&&                                                                             
         JZ    DEM62A                                                           
         L     R5,LUHOMES                                                       
         AHI   R5,5                                                             
         DR    RE,R5                                                            
         J     *+8                                                              
*                                                                               
DEM62A   D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R1,RF                                                            
         MVC   DUB(2),PHOMES                                                    
         J     DEM65                                                            
*                                                                               
DEM63    BAS   RE,ADJPUT                                                        
         TM    RAVLNTYP,X'C0'                                                   
         JNM   *+8                                                              
         MH    R1,=H'10'                                                        
*                                  CREATE OVERRIDE ELEMENT                      
DEM64    MVC   DUB(2),PHOMES                                                    
         TM    RAVLNTYP,X'C0'                                                   
         JM    *+8                                                              
         MH    R1,=H'10'                                                        
DEM65    STH   R1,DUB+2                                                         
         STH   R1,SAVHUT                                                        
         BAS   RE,ADDOVER                                                       
*                                  CALCULATE NEW RTGHMS AND OVRD ELEM           
         L     R0,UOSHOMES         GET OLD HOMES SHARE                          
         MR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DUB+2            STORE NEW RATING HOMES                       
         MVC   DUB(2),RHOMES                                                    
         BAS   RE,ADDOVER          SCREWS UP SHARE IF HERE                      
*                                                                               
         L     R0,UORHOMES         GET OLD RATING HOMES                         
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPHUT3                                                           
         LR    RF,R0               CALC IMP BASED RATING XX.X                   
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
UPHUT2A  D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
*                                                                               
UPHUT3   BAS   RE,GETINDEX                                                      
*                                                                               
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*          DATA SET SPDEMUPA   AT LEVEL 029 AS OF 10/28/99                      
* ADJUST THE PUTS/TOTS                                                          
         MVC   DUB(4),INDEX        SAVE CURRENT INDEX                           
         SR    R0,R0                                                            
         LH    R1,SAVHUT           NEW HUT                                      
         CLI   TAPEOPT,C'Y'        IMP BASED                                    
         JNE   UPHUT4                                                           
         L     R1,UNPHOMES         GETS ADJUSTED BY UNIVERSE                    
         MH    R1,=H'100'                                                       
*---------------------------                                                    
***      D     R0,UOUHOMES |                                                    
*---------------------------                                                    
         L     R5,UOUHOMES        NEW CODE TO ROUND UP UOUHOMES                 
         AHI   R5,5               PUT UPGRADES AND ADJUST PUTS                  
         DR    R0,R5              PROPERLY                                      
*                                                                               
         SR    R0,R0                                                            
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         LR    R0,R1               SLOT PROPERLY                                
         LH    R1,SAVHUT           RESTORE NEW                                  
*                                                                               
         J     *+8                                                              
UPHUT4   L     R0,UNPHOMES                                                      
         BAS   RE,GETINDEX                                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*&&DO                                                                           
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWTOTS,0,NEWTOTS                      
*&&                                                                             
         MVC   INDEX(4),DUB                                                     
*        GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*        GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWTOTS,0,NEWTOTS                      
         CLC   RAVLNOP1,=H'1000'   TEST HUT = INDEX OR BOOK                     
         JL    *+8                                                              
         BRAS  RE,NORMU                                                         
         CLI   TAPEOPT,C'Y'             IF IMP-BASED, KEEP                      
         JE    *+10                      ORIG HOME SHRS FOR HUT UPGD            
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
DEM66    OC    RAVLNOP1,RAVLNOP1   TEST HUT PRESENT                             
         JZ    DEM70                                                            
         SPACE 2                                                                
**********************************************************************          
* HUT/SHARE UPGRADE                                                  *          
* NEWRTG = NEWHUT * NEWSHR(HMS)                                      *          
* ADJRTG = RTGINX * OLDRTG                                           *          
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                         *          
* OVERRIDE HUT/SHARE - CALCULATE RATING OVERRIDE                     *          
**********************************************************************          
         SPACE 2                                                                
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU                                                         
*                                  CREATE HUT OVERRIDE ELEMENT                  
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET HUT INDEX                                
         CLC   RAVLNOP1,=H'1000'   TEST HUT = INDEX OR BOOK                     
         JL    UPHUT7                                                           
         L     R1,UNPHOMES         BOOK - GET NEW HUT VALUE                     
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPHUT6                                                           
*                                                                               
         L     R1,UNPHOMES         BOOK - GET NEW HUT VALUE                     
         SR    R0,R0                                                            
         MH    R1,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
*-------------------------------                                                
**       JZ    *+12            |                                                
*        D     R0,UOUHOMES     |                                                
*-------------------------------                                                
         JZ    UPHUT5                                                           
         L     RF,UOUHOMES         NEW CODE TO ROUND UP UOUHOMES                
         AHI   RF,5                TO GET THE SAME PUT RESULTS AS               
         SR    RE,RE               PUT UPGRADES AND ADJUST PUTS                 
         DR    R0,RF               PROPERLY                                     
*                                                                               
         J     *+8                                                              
UPHUT5   D     R0,=F'1'                                                         
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
UPHUT6   TM    RAVLNTYP,X'C0'                                                   
         JM    *+8                                                              
         BAS   RE,ADJPUT                                                        
UPHUT7   MVC   DUB(2),PHOMES                                                    
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'                                                        
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         ST    R1,DUB+4            SAVE NEW HUT                                 
*                                  CREATE SHARE OVERRIDE ELEMENT                
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP2       GET NEW SHARE                                
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
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
*          DATA SET SPDEMUP    AT LEVEL 064 AS OF 04/07/95                      
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPHUT8                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
UPHUT8   BAS   RE,GETINDEX                                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*          DATA SET SPDEMUPA   AT LEVEL 029 AS OF 10/28/99                      
* ADJUST THE PUTS/TOTS                                                          
         MVC   DUB(4),INDEX        SAVE CURRENT INDEX                           
         SR    R0,R0                                                            
         L     R1,DUB+4            NEW HUT                                      
         CLI   TAPEOPT,C'Y'        IMP BASED                                    
         JNE   UPHUT9                                                           
         L     R1,UNPHOMES         GETS ADJUSTED BY UNIVERSE                    
         MH    R1,=H'100'                                                       
*---------------------------                                                    
*****    D     R0,UOUHOMES |                                                    
*---------------------------                                                    
         L     RF,UOUHOMES         NEW CODE TO ROUND UP UOUHOMES                
         AHI   RF,5                TO GET THE SAME PUT RESULTS AS               
         SR    RE,RE               PUT UPGRADES AND ADJUST PUTS                 
         DR    R0,RF               PROPERLY                                     
         SR    R0,R0                                                            
*                                                                               
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         LR    R0,R1               SLOT PROPERLY                                
         L     R1,DUB+4            RESTORE NEW                                  
         J     *+8                                                              
UPHUT9   L     R0,UNPHOMES                                                      
         BAS   RE,GETINDEX                                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*&&DO                                                                           
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWTOTS,0,NEWTOTS                      
*&&                                                                             
         MVC   INDEX(4),DUB                                                     
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
**********************************************************************          
* SHARE ONLY UPGRADE                                                 *          
* NEWRTG = NEWSHR * OLDHUT                                           *          
* ADJRTG = RTGINX * OLDRTGS                                          *          
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                         *          
* OVERRIDE SHARE - CALCULATE RATING OVERRIDE                         *          
**********************************************************************          
         SPACE 2                                                                
DEM70    LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU                                                         
*                                  CREATE SHARE OVERRIDE                        
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP2       GET NEW SHARE                                
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                  CALCULATE RATING HOMES OVERRIDE              
         ICM   R1,3,RAVLNOP2       RESTORE SHARE                                
         TM    RAVLNTYP,X'C0'      DECIMAL PRECISION                            
         JM    *+8                                                              
         MH    R1,=H'10'           SCALE                                        
         L     R0,UOPHOMES         OLD HUT                                      
*          DATA SET SPDEMUP    AT LEVEL 064 AS OF 04/07/95                      
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPSHR2                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
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
*          DATA SET SPDEMUP    AT LEVEL 064 AS OF 04/07/95                      
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPSHR4                                                           
         LR    RF,R0               CONVERT IMPS TO RATING XX.X                  
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LR    R0,RF                                                            
UPSHR4   BAS   RE,GETINDEX         RATING INDEX                                 
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
DEM80    CLI   RAVLNTYP,X'44'                                                   
         JE    *+12                                                             
         CLI   RAVLNTYP,4                                                       
         JNE   DEM85                                                            
         SPACE 2                                                                
**********************************************************************          
* INDEX UPGRADE                                                      *          
* ADJRTG = INDEX * OLDRTG                                            *          
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                         *          
* CALCULATE SHARE OVERRIDE                                           *          
**********************************************************************          
         SPACE 2                                                                
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
         CLC   RAVLNOP1,=H'100'                                                 
         JNE   DEM82                                                            
*                                  SPECIAL CODE FOR INDEX=100                   
         MVC   NEWRTG(NEWRTGLN),OLDRTG                                          
         BRAS  RE,NORMU            NORMALIZE                                    
*                                                                               
DEM80A   GOTO1 VHELLO,DMCB,(C'D',FILENAME),(X'05',ARECORD),0                    
         XC    INDEX,INDEX                                                      
         J     DEM100                                                           
*                                                                               
DEM82    SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1       GET INDEX VALUE                              
         MH    R1,=H'100'          INDEX VALUE MUST BE TO 2 DEC. PLCS.          
         ST    R1,INDEX                                                         
*                                                                               
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*                                                                               
         L     R1,UNRHOMES         NEW RATING HOMES                             
         MH    R1,=H'10'           SCALING                                      
         L     R0,UOPHOMES         OLD HUT                                      
         BAS   RE,GETVALUE         CALCULATE NEW HOMES SHARE                    
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER                                                       
         BRAS  RE,NORMU                                                         
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
DEM85    CLI   RAVLNTYP,X'48'                                                   
         JE    *+12                                                             
         CLI   RAVLNTYP,8                                                       
         JNE   DEM86                                                            
         SPACE 2                                                                
**********************************************************************          
* 3 YEAR AVERAGE FOR SHARES                                          *          
* ADJSHR = AVG OF 3 YEAR SHARES                                      *          
* OUTPUT = ADJRTG / ADJHPT / NEWRTG / NEWHPT                         *          
* CALCULATE 3 YEAR SHARE                                             *          
**********************************************************************          
         SPACE 2                                                                
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT                                                        
*        BAS   RE,GETYRS                                                        
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         J     DEM100                                                           
*                                                                               
         EJECT                                                                  
DEM86    CLI   RAVLNTYP,X'49'                                                   
         JE    *+12                                                             
         CLI   RAVLNTYP,9                                                       
         JNE   DEM87                                                            
         SPACE 2                                                                
**********************************************************************          
* MMU UPGRADE                                                        *          
* PUT UPGRADE WITH INDEX OF 2 YEARS PUT                              *          
**********************************************************************          
         SPACE 2                                                                
* CHECK RAVLNOP2                                                                
* IF ITS A STATION ITS NOT NUMERIC IT MUST BE A STATION                         
* WHICH INDICATES THIS IS "LPM" UPGRADE                                         
         CLI   RAVLNOP2,X'C1'           ALPHA?                                  
         BL    *+12                                                             
         CLI   RAVLNOP2,X'E9'                                                   
         BNH   DEM87LPM                                                         
         CLC   =X'9999',RAVLNOP3        LPS UPGRADE IDENTIFIER?                 
         BE    DEM87LPS                                                         
         OC    OLDRTG(OLDRTGLN),OLDRTG                                          
         JNZ   *+10                                                             
         MVC   OLDRTG(OLDRTGLN),NEWRTG                                          
*                                                                               
         MVI   HPTFLAG,C'N'                                                     
         LA    R1,RAVLNOP1                                                      
         MVC   SVLPMBT,DBBTYPE                                                  
         BAS   RE,GETHPT                                                        
         MVC   DBBTYPE,SVLPMBT                                                  
         BRAS  RE,NORMU                                                         
* MOVE THE FOLLOWING MMU CODE TO SUBR01 TO CREATE ADDRESSIBILITY                
*&&DO                                                                           
*        BAS   RE,GETYRS                                                        
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4ASW,C'N'                                                       
         MVC   MMUBOOK,RAVLNOP2                                                 
         MVC   SAVRTG(OLDRTGLN),OLDRTG                                          
         MVI   MMUPASS,1                                                        
         MVC   DBSELBK,MMUBOOK                                                  
         GOTO1 VSUBR01,DMCB,('LPMCHKQ',(RC))                                    
*                                                                               
         CLI   DBBTYPE,C'P' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         CLI   DBBTYPE,C'I' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         MVI   MMUPASS,0                                                        
*                                                                               
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
*&&                                                                             
***      GOTO1 VSUBR01,P1,('UPMMUQ',(RC))                                       
         GOTO1 VSUBR02,P1,('UPMMUQ',(RC))                                       
         J     DEM100                                                           
*                                                                               
DEM87    MVC   BYTE,RAVLNTYP                                                    
         NI    BYTE,B'11011111'    ZAP INVENTORY BYTE                           
         CLI   BYTE,11                                                          
         JE    *+12                                                             
         CLI   BYTE,12                                                          
         JNE   DEM88                                                            
         MVI   Y4SSW,C'S'                                                       
         CLI   BYTE,12                                                          
         JE    *+8                                                              
         MVI   Y4SSW,C'P'                                                       
         J     DEM87AVG                                                         
*                                                                               
         SPACE 2                                                                
**********************************************************************          
* LPM UPGRADE - INDEX PUT AND SHARE BOOK VALUES WITH                 *          
* SINDEX = LPM SHARE/DIARY SHARE                                     *          
* PINDEX=LPM PUTS / DIARY PUTS                                       *          
* APPLY INDEXES TO PUT UPGRADED NUMBERS                              *          
**********************************************************************          
         SPACE 2                                                                
DEM87LPM DS    0C                                                               
         LA    R1,RAVLNOP1                                                      
         BAS   RE,GETHPT           GET NEW HPT                                  
         BRAS  RE,NORMU            NORMALIZE UNIVERSES                          
         GOTO1 VSUBR01,P1,('LPMSIDXQ',(RC))                                     
         J     DEM100                                                           
DEM87LPS DS    0C                                                               
         GOTO1 VSUBR01,P1,('LPSPIDXQ',(RC))                                     
         J     DEM100                                                           
**********************************************************************          
* P OR S AVERAGE UPGRADE                                             *          
* ADJSHR = AVG OF N PERIOD PUTS/SHARES                               *          
* OUTPUT = ADJRTG / ADJHPT / NEWRTG / NEWHPT                         *          
* CALCULATE N PERIOD INDEX                                           *          
**********************************************************************          
         SPACE 2                                                                
DEM87AVG LA    R1,RAVLNOP1                                                      
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         J     DEM100                                                           
         SPACE 2                                                                
DEM88    CLI   BYTE,13                                                          
         JE    *+12                                                             
         CLI   BYTE,14                                                          
         JNE   DEM90                                                            
         MVI   Y4SSW,C'S'                                                       
         MVI   Y4ASW,C'Y'                                                       
         CLI   BYTE,14                                                          
         JE    *+8                                                              
         MVI   Y4SSW,C'P'                                                       
         SPACE 2                                                                
**********************************************************************          
* P OR S AVERGAE UPGRADES  PAB,PAQ,PAY,SAB,SAQ,SAY                   *          
**********************************************************************          
*                                                                               
**       CLI   YRSAVTYP,0      PAB,SAB,PAQ,SAQ,PAY,SAY                          
**       BE    GETYRS1A                                                         
* NEED TO CALL GETHPT FOR PAB/PAQ                                               
* PROPOSER SAVG PUTS BREAKS IF WE CALL GETHPT AGAIN                             
         GOTO1 VSUBR01,DMCB,('GETHMKTQ',(RC))                                   
                                                                                
*DEM89   BAS   RE,GETHPT      CALL TO GET SAVRMKT NEEDED FOR PAB/PAQ/SA         
*                                                                               
*                                                                               
****     LA    R1,RAVLNOP1                                                      
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         J     DEM100                                                           
         EJECT                                                                  
DEM90    CLI   RAVTMPTY,C'A'                                                    
         JNL   *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* DEMO UPGRADE                                                        *         
* INDEX  = NEWDEMO / OLDDEMO                                          *         
* ADJRTG = INDEX * OLDRTG                                             *         
* OUTPUT = OLDRTG / OLDHPT / ADJRTG / NEWHPT                          *         
* OVERRIDE NEWDEMO - CALCULATE SHARE/HOMES RATING OVERRIDE            *         
***********************************************************************         
         SPACE 2                                                                
*                                  MAKE NEWRTG=OLDRTG ON RECORD                 
         MVC   NEWRTG(NEWRTGLN),OLDRTG                                          
         LA    R1,(IUNEND-IUNREC)/4                                             
         STCM  R1,3,DBNUMVLS       SET DBNUMVLS TO MAXIMUM                      
         SLL   R1,2                                                             
         MOVE  (IO,(R1)),IUNREC    MOVE VALUES TO SAVE AREA                     
         MVC   WORK(10),OFORMAT    BUILD DEMAINT OUTPUT FORMAT BLOCK            
         CLI   TAPEOPT,C'Y'        TEST IF IMP. BASED                           
         JNE   *+10                                                             
         MVC   WORK+7(2),=X'5A0B'                                               
         MVC   WORK(3),FILEDEM                                                  
         GOTO1 VDEMAINT,DMCB,=C'REP',DBLOCK,IO,WORK                             
         BAS   RE,ERROR                                                         
         BAS   RE,GETELS           GET NEW A(RECORD ELEMENTS)                   
*                                  GET OLDDEMO VALUE INTO INDEX                 
         GOTO1 VDEMOUT,DMCB,(C'D',RAVTMPTY-1),DBLOCK,INDEX                      
         XC    NEWRTG(NEWRTGLN),NEWRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
*                                                                               
         LA    R1,BOOKZ                                                         
         BAS   RE,GETHPT                                                        
         BRAS  RE,NORMU                                                         
*                                                                               
         L     R1,INDEX            GET INDEX VALUE                              
         SR    R0,R0                                                            
         LR    R0,R1                                                            
         ICM   R1,3,RAVLNOP1                                                    
         CLI   RAVLNTYP,C'A'       1 DEC UPG PREC - DONT ADJ                    
         JL    *+16                                                             
         CLI   RAVLNTYP,C'R'                                                    
         JNE   *+8                                                              
         MH    R1,=H'10'                                                        
         BAS   RE,GETINDX2                                                      
*                                  CREATE DEMO OVERRIDE ELEMENT                 
         SR    R1,R1                                                            
         ICM   R1,3,RAVLNOP1                                                    
         CLI   RAVLNTYP,C'A'       1 DEC UPG PREC - DONT ADJ                    
         JL    *+16                                                             
         CLI   RAVLNTYP,C'R'                                                    
         JNE   *+8                                                              
         MH    R1,=H'10'                                                        
         MVC   DUB(2),RAVTMPTY                                                  
         STH   R1,DUB+2                                                         
         BAS   RE,ADDOVER                                                       
*                                                                               
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),OLDRTG,0,ADJRTG                        
*        GOTO1 CALC,P1,OLDRTG,0,ADJRTG                                          
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         MVC   NEWRTG(RTGLN),OLDRTG                                             
         MVI   INDEXUPG,C'Y'                                                    
*                                  CREATE SHARE OVERRIDE                        
         L     R1,UOSHOMES                                                      
         M     R0,INDEX                                                         
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
         MVC   DUB(2),SHOMES                                                    
         STH   R1,DUB+2                                                         
*        BAS   RE,ADDOVER <-------------                                        
*                                  CALCULATE NEW RATING HOMES                   
         CLC   RAVTMPTY(2),RHOMES  CREATE RATING OVERRIDE                       
         JE    DEM100              UNLESS RHOMES WAS INPUT DEMO                 
         L     RF,UOPHOMES                                                      
         CLI   TAPEOPT,C'Y'                                                     
         JNE   UPDEM3                                                           
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         OC    UOUHOMES,UOUHOMES                                                
         JZ    *+12                                                             
         D     RE,UOUHOMES                                                      
         J     *+8                                                              
         D     RE,=F'1'                                                         
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
         MVI   INDEXUPG,C'Y'                                                    
         J     DEM100                                                           
         EJECT                                                                  
***********************************************************************         
* IUN RECORD VALUES HAVE NOW BEEN ADJUSTED BY UPGRADE ROUTINES.       *         
* REPLACE OLD DEMO ELEMENTS WITH IUN FORMAT DEMO ELEMENTS.            *         
* CREATE OVERRIDE ELEMENTS FOR ANY VALUE THAT WAS PASSED FROM CALLER. *         
* BOOK ELEMENT IS FORCED TO JUN/82 IF PUT UPGRADE TOOK PLACE.         *         
***********************************************************************         
         SPACE 1                                                                
DEM100   CLI   FROMTYPE,C'I'       RESTORE SAVED RATING VALUES                  
         JNE   *+10                                                             
         MVC   OLDRTG(OLDRTGLN),SAVRTG                                          
         CLI   IUNFLAG,C'Y'        TEST FOR IUN RECORD PASSED                   
         JE    DEM101                                                           
         CLI   INDEXFF,C'Y'        NO - TEST FOR INDEX=FF CALL                  
         JNE   *+10                YES - EQUATE NEW TO OLD RATINGS              
         MVC   NEWRTG(NEWRTGLN),OLDRTG                                          
DEM101   XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR DEMAINT CALL                
         MVC   DBFILE,FILEDEM                                                   
         MVC   DBAREC,ARECORD                                                   
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         LA    R1,(IUNEND-IUNREC)/4                                             
         STCM  R1,3,DBNUMVLS       SET DBNUMVLS TO MAXIMUM                      
         MVC   WORK(10),OFORMAT    BUILD DEMAINT OUTPUT FORMAT BLOCK            
         CLI   INDEXUPG,C'Y'       INDEX UPGRADE - RATING BASED                 
         JNE   *+10                                                             
         MVC   WORK+7(2),BOOK8312                                               
*                                                                               
         CLI   TAPEOPT,C'Y'        TEST IF IMP. BASED                           
         JNE   DEM101A                                                          
         MVC   WORK+7(2),=X'5A0B'  YES - SET BOOK TO NOV/90                     
         CLI   INDEXUPG,C'Y'       INDEX UPGRADE TAPE BASED                     
         JNE   *+10                                                             
         MVC   WORK+7(2),BOOK9012                                               
*                                                                               
DEM101A  CLI   FILETYPE,C'I'                                                    
         JE    *+10                                                             
         MVC   WORK(3),PAVVALS+1   FORCE BOOK ELEMENT TO RECORD                 
         CLI   PUTUPGD,C'Y'        TEST IF PUT UPGRADE DONE                     
         JNE   DEM101B                                                          
*-->     MVC   WORK+7(2),=X'5206'  YES - SET BOOK TO JUN/82                     
         MVC   WORK+7(2),=X'5406'  YES - SET BOOK TO JUN/84                     
         CLI   TAPEOPT,C'Y'        TEST IF IMP. BASED                           
         JNE   DEM101B                                                          
         MVC   WORK+7(2),=X'5B06'  YES - SET BOOK TO JUN/91                     
DEM101B  GOTO1 VDEMAINT,DMCB,=C'REP',DBLOCK,IUNREC,WORK                         
         XC    WORK(12),WORK       INITIALIZE WORKAREA                          
         BAS   RE,ERROR                                                         
         MVC   WORK(2),=X'DE0C'    BUILD ELEMENT (NEW FORMAT)                   
         GOTO1 VHELLO,DMCB,(C'D',FILENAME),(WORK,ARECORD),0                     
*                                  CREATE DEMO OVERRIDE ELEMENTS                
         SR    R0,R0                                                            
         ICM   R0,1,OVERLIST       R0=NUMBER OF OVERRIDE VALUES                 
         JZ    DEM104                                                           
         LA    R2,OVERLIST+1       R2=A(OVERRIDE LIST)                          
*                                  ADD OVERRIDE ELEMENTS                        
DEM102   EQU   *                                                                
         MVC   WORK+4(2),0(R2)     INSERT DEMO TYPE + NUMBER                    
         MVC   WORK+10(2),2(R2)    INSERT DEMO VALUE                            
         MVI   WORK+7,X'81'        SET RATINGS PRECISION                        
         CLI   WORK+4,C'R'         TEST RATING OR IMPRESSION                    
         JE    DEM103                                                           
         CLI   WORK+4,C'P'         TEST RATING OR IMPRESSION                    
         JE    DEM103                                                           
         CLI   WORK+4,C'S'         TEST RATING OR IMPRESSION                    
         JE    DEM103                                                           
         MVI   WORK+7,X'42'        SET IMPRESSION PRECISION                     
DEM103   EQU   *                                                                
         GOTO1 VHELLO,DMCB,(C'P',FILENAME),ARECORD,WORK,0                       
         LA    R2,4(R2)                                                         
         BCT   R0,DEM102                                                        
*                                  ADD INDEX VALUE OVERRIDE ELEMENT             
DEM104   OC    INDEX,INDEX         TEST FOR INDEX                               
         JZ    DEM106                                                           
         MVI   WORK+1,7                                                         
         MVC   WORK+2(2),NDXDEMO                                                
         MVC   WORK+4(3),INDEX+1   3 BYTE INDEX VALUE                           
         GOTO1 VHELLO,DMCB,(C'P',FILENAME),ARECORD,WORK,0                       
*                                                                               
DEM106   L     R1,ARECLEN                                                       
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         BCTR  R0,0                                                             
         STCM  R0,3,0(R1)                                                       
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
**********************************************************************          
* SUBROUTINE TO COMPUTE INTEGER INDEX VALUE                          *          
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                *          
* RETURN RESULT IN R1.                                               *          
**********************************************************************          
         SPACE 1                                                                
GETVALUE MH    R1,=H'100'                                                       
         LR    RF,R0               SAVE OLD VALUE                               
         SRL   R0,1                HALVE                                        
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
*                                                                               
         LTR   RF,RF               HANDLE ZERO DIVISOR                          
         JNZ   *+10                                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         DR    R0,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUBROUTINES TO COMPUTE INDEX VALUE TO 2 DECIMAL PLACES             *          
* ENTRY AT GETINDEX IS USED TO TAKE VALUES AND ROUND BEFORE CALC     *          
* ENTRY AT GETINDX2 ASSUMES ROUNDED VALUES PASSED                    *          
*                                                                    *          
* R0 HAS OLD VALUE - R1 HAS NEW VALUE                                *          
* RETURN RESULT IN R1 AND IN 'INDEX'.                                *          
**********************************************************************          
         SPACE 1                                                                
GETINDEX ST    R0,DMCB                                                          
         SR    R0,R0                                                            
         ST    R1,DMCB+4                                                        
*                                                                               
         SR    R0,R0                                                            
         L     R1,DMCB                                                          
         ST    R1,DMCB                                                          
*                                                                               
         LM    R0,R1,DMCB          PICK UP ROUNDED VALUES                       
*                                                                               
GETINDX2 XC    INDEX,INDEX                                                      
         MH    R1,=H'10000'                                                     
         LTR   RF,R0               SAVE OLD VALUE                               
         BZR   RE                                                               
         SRL   R0,1                                                             
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         DR    R0,RF                                                            
         ST    R1,INDEX                                                         
         BR    RE                                                               
*                                                                               
         SPACE 1                                                                
*                                  ADJUST PUT VALUE FOR OVERRIDE                
ADJPUT   SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OLD OR NEW H/P/T VALUES FROM TIME PERIOD FILE.       *         
* ON ENTRY HPTFLAG=C'O' FOR OLD H/P/T OR C'N' FOR NEW H/P/T WITH      *         
* R1=A(BOOK OPTION).                                                  *         
* EXIT WITH H/P/T VALUES AT OLDHPT OR NEWHPT.                         *         
***********************************************************************         
         SPACE 1                                                                
GETHPT   NTR1  WORK=(R8,STNHPTLN)                                               
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         ICM   RE,15,UPIMS                                                      
GETHPIMS LTR   RE,RE                                                            
         JZ    GETHAFFX                                                         
         USING DBXAFFD,RE                                                       
         CLC    0(4,RE),=C'IMS '   IN MKT SHARE LIST OF AFFL                    
         JE    *+12                                                             
         L     RE,DBXAFFNX                                                      
******** J     *-20                                                             
         J     GETHPIMS                                                         
         LA    RE,DBXAFFL                                                       
         ST    RE,AAFFLST          SAVE ADDR OF AFFL LIST FOR STNHPT            
         DROP  RE                                                               
         LR    R2,R1               PASS INPUT PARMS                             
         GOTO1 VSUBR01,DMCB,('STNHPTS',(RC)),(R2)                               
         CLI   DBERROR,0           IF NO ERRORS, EXIT ELSE DEFAULT              
         JE    EXIT                 TO REG LK UP W/OUT AN IMS                   
         LR    R1,R2               RESTORE PARM LIST                            
****     J     GETHAFFX                                                         
*                                                                               
GETHAFFX DS    0H                                                               
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVI   FORCESW,C'N'        INITIALIZE SPECIAL SURVEY SWITCH             
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         MVC   DBFILE,TPTVALS+1                                                 
         CLC   0(2,R1),=H'1000'    TEST IF BOOK PASSED                          
         JNH   *+10                                                             
*        MVC   DBSELBK,RAVLNOP1    YES - SET SELECTED BOOK                      
         MVC   DBSELBK,0(R1)       YES - SET SELECTED BOOK                      
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
*        CLI   FILETYPE,C'I'                                                    
*        JNE   *+12                                                             
*        MVI   DBSELMED,C'U'       IF INV FILE, DEFAULT TO 'U'                  
*        J     *+8                                                              
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
*                                  VALIDATE STATION TO GET MARKET               
         MVI   DBFUNCT,DBVLSTBK                                                 
*                                                                               
         CLI   INDEXFF,C'Y'        TEST FOR INDEX=FF DAY/TIME CALL              
         JNE   GETHPT1                                                          
         CLI   INDEXTYP,C'D'                                                    
         JNE   GETHPT1                                                          
         L     R1,ADBLOCK          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBSELMED,DBSELMED-DBLOCK(R1)                                     
         MVC   DBSELBK,DBSELBK-DBLOCK(R1)                                       
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         J     GETHPT6                                                          
*                                                                               
GETHPT1  L     R2,ARECORD                                                       
         CLI   FILETYPE,C'I'       TEST FOR INVENTORY FILE                      
         JNE   GETHPT4                                                          
         CLI   HPTFLAG,C'O'        AND OLD H/P/T VALUES REQUIRED                
         JNE   GETHPT2                                                          
*                                                                               
*                                  EXTRACT STATION/BOOK/SOURCE FROM             
*                                  INVENTORY RECORD KEY/ELEMENTS                
         USING RINVD,R2                                                         
*                                                                               
GETHPT1E ICM   R3,15,AINVFREL      TEST IF FROM BOOK ELEMENT FOUND              
         JZ    GETHPT2                                                          
RFR      USING RINVFREL,R3                                                      
         MVC   DBSELSTA,RFR.RINVFRST YES - EXTRACT VALUES FROM ELEMENT          
         MVI   DBSELSRC,C'A'       ARB                                          
         TM    RFR.RINVFRBK,X'40'  NSI                                          
         JZ    *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         TM    RFR.RINVFRBK,X'41'  SRC                                          
         JNO   *+8                                                              
         MVI   DBSELSRC,C'S'                                                    
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+12                                                             
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DUB(2),RFR.RINVFRBK+1                                            
         MVC   DUB+2(1),RFR.RINVFRBT                                            
         J     GETHPT6                                                          
*                                                                               
GETHPT2  MVC   DBSELSTA,RINVKSTA   EXTRACT STATION/SRC/BOOK FROM KEY            
*                                                                               
*                                                                               
*  ANY SPILL MARKET LINK ?                                                      
         OC    RINVLINK,RINVLINK                                                
         BZ    GETHPT2E                                                         
         ZICM  RE,RINVLINK,(15)                                                 
GETHPT2B CLC   =C'SMKT',0(RE)                                                   
         BE    GETHPT2C                                                         
         ZICM  RE,4(RE),(15)                                                    
         OR    RE,RE                                                            
         BZ    GETHPT2E                                                         
         B     GETHPT2B                                                         
GETHPT2C MVC   DBSELMK,8(RE)                                                    
*                                                                               
GETHPT2E DS    0H                                                               
*&&DO                                                                           
         L     RF,=A(SVCLST)                                                    
         A     RF,RELO                                                          
GETHPT3  CLI   0(RF),X'FF'         END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                *****INVALID KEY SOURCE*********             
         CLC   2(1,RF),RINVKSRC    FIND REAL SOURCE                             
         JE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         J     GETHPT3                                                          
         MVC   DBSELSRC,0(RF)      AND SET IN DBLOCK                            
*&&                                                                             
         MVC   DBSELSRC,RINVKRSR   AND SET IN DBLOCK                            
*                                                                               
         MVC   DUB(2),RINVKBK                                                   
         CLI   HPTFLAG,C'N'        TEST FOR READING NEW HPTS                    
         JNE   GETHPT6             NO                                           
         ICM   R3,15,AINVFREL                                                   
         JZ    *+10                                                             
         MVC   DUB+2(1),RFR.RINVFRBT  TRY TO GET NEW HPTS FOR SPEC SURV         
         J     GETHPT6             FIRST                                        
         DROP  R2,RFR                                                           
*                                  EXTRACT VALUES FROM PAV DEMO KEY             
         USING PMKEY,R2                                                         
GETHPT4  CLI   FILETYPE,C'P'       TEST FOR PAV FILE                            
         JNE   GETHPT5                                                          
         CLC   =C'QNN',0(R2)       TEST NHT PRG RECD                            
         JNE   GETHPT5             NO, ASSUME PRKEY FRMT                        
         MVC   DBSELSTA,PMSTAT                                                  
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DUB(2),PMBOOK                                                    
         MVC   DUB+2(1),PMBTYP                                                  
         J     GETHPT6                                                          
         DROP  R2                                                               
*                                                                               
GETHPT5  DS    0H                  NON-NETWORK TV RECS IN PRKEY FRMT            
         USING PRKEY,R2                                                         
         MVC   DBSELSTA,PRSTAT                                                  
         MVC   DBSELSRC,PRSRC                                                   
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+8                                                              
         MVI   DBSELSRC,C'H'                                                    
         MVC   DUB(2),PRBOOK                                                    
         MVC   DUB+2(1),PRBTYP                                                  
*                                                                               
GETHPT6  DS    0H                                                               
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+12               ==================================            
         CLI   INDEXFF,C'Y'        INDEX FF N/A FOR NHTI FILE   !!!             
         JE    EXIT               ===================================           
*                                                                               
         OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         JNZ   *+16                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DBBTYPE,DUB+2                                                    
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
         MVC   DUB+2(1),DBBTYPE    AND BOOK TYPE                                
*                                                                               
         ICM   R1,15,AUPGDEL                                                    
         USING RAVLNEL,R1                                                       
         CLI   RAVLNBT,0           TEST FOR BOOK TYPE IN UPGRADE EL             
         JE    GETHPT7             NO                                           
         MVC   DBBTYPE,RAVLNBT     YES-IT TAKES PRECEDENCE OVER                 
         MVC   DUB+2(1),RAVLNBT    EVERYTHING ELSE                              
         MVI   FORCESW,C'Y'        MUST FIND HPT'S IN SPECIAL SURVEY BK         
*                                                                               
         CLI   RAVLNBT,C'A'      PARENT OF SAT USES NORMAL BOOK FOR HPT         
         BE    GETHPT6F                                                         
         CLI   HPTFLAG,C'N'        TEST FOR READING NEW HPTS                    
         JNE   GETHPT7             NO                                           
         CLI   RAVLNBT,C'O'        AND OLYMPIC                                  
         JNE   GETHPT7             NO                                           
*                                                                               
GETHPT6F MVI   DBBTYPE,0           USE NORMAL BOOK                              
         MVI   DUB+2,0                                                          
         DROP  R1                                                               
*                                                                               
GETHPT7  DS    0H                                                               
         MVI   ZCELLBTY,0                                                       
* CHECK TO SEE IF WE ARE DEALING WITH LIVE ONLY BOOK                            
         GOTO1 VSUBR02,DMCB,('CHKLIVMQ',(RC))                                   
*                                                                               
         CLI   ROLLFLAG,C'Y'                                                    
         BNE   GETHPT7B                                                         
         GOTO1 VSUBR01,DMCB,('SETOVBTQ',(RC)),(R2)                              
                                                                                
*                                                                               
GETHPT7B CLI   DBSELSTA+4,C'H'     NHTI FILE LK UP?                             
         JNE   GETHPT7A                                                         
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'H'                                                    
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
GETHPT7A LA    R0,2                R0=LOOP COUNT                                
         CLI   INDEXFF,C'Y'        TEST FOR INDEX=FF CALL                       
         JNE   GETHPT8                                                          
         L     R1,ASAVE                                                         
         CLC   0(5,R1),=XL5'00'    TEST IF FIRST TIME                           
         JE    GETHPT8                                                          
         CLC   0(2,R1),DBSELBK     NO - TEST IF BOOK CHANGED                    
         JNE   GETHPT8                                                          
         CLC   2(1,R1),DBBTYPE     NOW TEST FOR CHANGE IN BK TYPE               
         JNE   GETHPT8                                                          
         MVC   DBSELRMK,3(R1)      SET MARKET NUMBER                            
*                                                                               
         CLC   DBSELSTA(4),=C'WWWB' FORCE TO CHARLOTTE                          
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'117'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'KAKW' FORCE TO AUSTIN                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'235'                                                 
*                                                                               
         J     GETHPT11                                                         
*                                  CALL DEMAND TO VALIDATE STATION/BOOK         
GETHPT8  DS    0C                                                               
* FOR WIRED USTV NSI - GRAB NEW HPT FROM STANDARD                               
         CLI   DBSELMED,C'T'                                                    
         BNE   *+8                                                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   *+8                                                              
         CLI   HPTFLAG,C'N'                                                     
         BNE   GETHPT8B                                                         
*                                                                               
         OC    DBSELMK,DBSELMK     CABLE SPILL                                  
         BNZ   GETHPT8B            DONT SWITCH                                  
*                                                                               
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
         CLI   DBBTYPE,BOOKTYPE_L1   STANDARD LIVE+1                            
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_C1   DMA CABLE LIVE+1                           
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_W1   WIRED CABKE LIVE+1                         
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_STANDARD                                        
         CLI   DBBTYPE,BOOKTYPE_H1   HISPANIC CABLE LIVE+1                      
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_H                                               
         CLI   DBBTYPE,BOOKTYPE_O1   OLYMPICE LIVE+1                            
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_O                                               
*                                                                               
GETHPT8B MVC   SVBKTYPE,DBBTYPE                                                 
         GOTO1 VSUBR02,DMCB,('FORCELPQ',(RC))                                   
*                                                                               
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
*                                                                               
         MVC   DBBTYPE,SVBKTYPE    RESTORE BOOKTYPE                             
*                                                                               
         OC    DUALTRMK,DUALTRMK   MARKET OVERRIDE PRESENT                      
         BZ    *+20                                                             
         MVC   DBSELRMK,DUALTRMK   JUST SET IT AND CONTINUE                     
         MVC   DBACTRMK,DUALTRMK                                                
         B     GETHPT10                                                         
*                                                                               
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   GETHPT8D            NO - EXTRTACT MARKET NUMBER                  
         MVC   DBSELRMK,DBKEY+(BSRMKT-BSKEY)                                    
         CLI   DBSELSRC,C'N'       FIX NSI FUCK UPS FOR ATL/COLOSPR             
         JNE   GETHPT10                                                         
         CLI   DBBTYPE,0           LEAVE IT ALONE IF BOOK TYPE                  
         JE    *+12                                                             
         CLI   DBBTYPE,C' '        LEAVE IT ALONE IF BOOK TYPE                  
         JNE   GETHPT10                                                         
         CLC   DBSELRMK,=H'124'    ATLANTA                                      
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'168'                                                 
         CLC   DBSELRMK,=H'299'    COLO SPRINGS TA                              
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'352'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'WWWB'                                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'117'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'KAKW' FORCE TO AUSTIN                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'235'                                                 
*                                                                               
         J     GETHPT10                                                         
*                                                                               
GETHPT8D DS    0H                                                               
* IF DBERROR = NOTFOUND CHECK TO SEE IF WE ARE PROCESSING ZEROCELL              
* BOOKTYPES 1-4.  IF SO, REREAD FOR NON ZEROCELL.                               
         CLI   DBERROR,X'10'                                                    
         BNE   GETHPT8H                                                         
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
* CHECK IF WE ARE ASKING FOR ZERO CELL DATA                                     
* IF SO IT WILL SET THE BOOKTYPE TO NON ZEROCELL LOOKUP                         
         CLI   ZCELLBTY,0         DID WE ALREADY DO THE RELOOKUP?               
         BNE   GETHPT8H                                                         
         GOTO1 VSUBR02,DMCB,('NONZEROQ',(RC))                                   
         CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
         BNE   GETHPT8            ZERO CELL - REREAD FOR NON ZEROCELL           
* RESTORE FROM SAVED BOOKTYPE AT CHKLIVEZ                                       
GETHPT8H MVC   DBBTYPE,ZSVBKTYP                                                 
*                                                                               
         CLI   HPTFLAG,C'O'        TEST OLD H/P/T LINE REQUIRED                 
         JE    EXIT                                                             
         CLI   DBERROR,X'10'       TEST FOR NOT FOUND                           
         JNE   GETHPT9                                                          
         BCT   R0,*+8              YES - TEST EST BOOK PROCESSED                
         J     GETHPT9                                                          
*                                                                               
*       BEN -  IF NO PEOPLE METER FOUND THEN TRY STANDARD SAME YEAR             
*               DONT GO BACK TO PREVIOUS YEARS DIARY.                           
*                                                                               
         ICM   R1,15,AUPGDEL                                                    
         USING RAVLNEL,R1                                                       
         CLI   RAVLNTYP,X'09'      ONLY FOR MMU                                 
         JNE   GETHPT8C                                                         
         CLI   DBBTYPE,C'I'        IF PEOPLE METER HISPANIC                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'P'        IF PEOPLE METER WAS NOT FOUND                
         BNE   *+12                READ STANDARD -SAME BOOK                     
         MVI   DBBTYPE,0           IF NOT FOUND                                 
         B     GETHPT8                                                          
         DROP  R1                                                               
*                                                                               
GETHPT8C ZIC   R1,DBSELBK          NO - DECREMENT BOOK & TRY AGAIN              
         BCTR  R1,0                                                             
         STC   R1,DBSELBK                                                       
         J     GETHPT8                                                          
*                                  STA/BK NOT FOUND - BOOK TYPE CODE            
GETHPT9  CLI   DUB+2,0             TEST FOR BOOK TYPE PRESENT                   
         JE    GETHPT9A            NO                                           
         CLI   DBBTYPE,0           TEST FOR READ ON NORMAL BOOK                 
         JE    EXIT                YES-VALIDATION FAILED SO EXIT                
         CLI   DBBTYPE,C'O'        ALLOW DEFAULT FOR OLYMPIC ONLY.              
         JE    *+12                                                             
         CLI   FORCESW,C'Y'        TEST FOR FORCING SPEC SURV ONLY              
         JE    EXIT                YES-EXIT                                     
         MVI   DBBTYPE,0           NO-TRY UNDER NORMAL BOOK AGAIN               
         MVC   DBSELBK,DUB         RESTORE ORIGINAL BOOK                        
         LA    R0,2                RESET COUNTER                                
         J     GETHPT8                                                          
*                                  STA/BOOK NOT FOUND TRY STATION ONLY          
GETHPT9A MVI   DBFUNCT,DBVLST                                                   
*                                                                               
         OC    DUALTRMK,DUALTRMK   MARKET OVERRIDE ACTIVE                       
         BZ    *+14                                                             
         MVC   DBACTRMK,DUALTRMK   MARKET OVERRIDE - DON'T CHECK STAT           
         B     GETHPT9C                                                         
*                                                                               
         MVC   SVBKTYPE,DBBTYPE                                                 
         GOTO1 VSUBR02,DMCB,('FORCELPQ',(RC))                                   
*                                                                               
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         MVC   DBBTYPE,SVBKTYPE                                                 
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   EXIT                                                             
GETHPT9C MVC   DBSELRMK,DBACTRMK   NO - EXTRACT MARKET NUMBER                   
*                                                                               
         CLC   DBSELSTA(4),=C'WWWB'                                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'117'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'KAKW' FORCE TO AUSTIN                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'235'                                                 
*                                                                               
         MVC   DBSELBK,DUB         RESTORE SELECTED BOOK VALUE                  
*                                  SAVE MARKET/BOOK IF INDEX=FF CALL            
GETHPT10 CLI   INDEXFF,C'Y'                                                     
         JNE   GETHPT11                                                         
         L     R1,ASAVE                                                         
         MVC   0(2,R1),DBSELBK                                                  
         MVC   2(1,R1),DBBTYPE                                                  
         MVC   3(2,R1),DBSELRMK                                                 
         B     GETHPT11A                                                        
*                                                                               
GETHPT11 MVI   DBFUNCT,DBGETTOT                                                 
*                                                                               
GETHPT11A CLC   DBSELRMK,=H'106'    DEAL WITH  BOSTON PPM DATA                  
         JNE   GETHPT11B                                                        
         CLC   DBSELBK,=X'6604'                                                 
         JH    GETHPT11B                                                        
         CLC   DBSELBK,=X'650A'                                                 
         JL    GETHPT11B                                                        
         MVI   DBBTYPE,C'P'                                                     
         J     GETHPT11X                                                        
GETHPT11B DS 0C                                                                 
         XC    UPLPMSD(4),UPLPMSD  CLEAR THE LPM DATES                          
         MVI   LPMHSP,C'N'                                                      
         CLI   DBBTYPE,C'B'              LEAVE BLACK SURVEY ALONE               
         BE    GETHPT11X                                                        
*&&DO                                                                           
         CLI   DBBTYPE,C'H'              CONTROL HISPANIC SEPARATELY            
         BE    UPLPMHSP                                                         
         CLI   DBBTYPE,C'I'                                                     
         BE    UPLPMHSP                                                         
*&&                                                                             
                                                                                
         GOTO1 VDEMTABS,DMCB,LPMSTEND  GET A(LPM DUAL DATA DATES)               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING LPMSETD,RE                                                       
GETHT11C CLI   0(RE),X'FF'         EOT?                                         
         BE    UPLPMSET                                                         
         CLC   DBBTYPE,LPMSEBKT    SAME BOOKTYPE?                               
         BNE   GETHP11D                                                         
         CLC   DBSELRMK,LPMSETMK   SAME MARKET                                  
         BNE   GETHP11D                                                         
         MVC   UPLPMSD(4),LPMSTART                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   LPMHSP,C'Y'                                                      
         B     UPLPMSET                                                         
GETHP11D AR    RE,R0                                                            
         B     GETHT11C                                                         
         DROP  RE                                                               
*&&DO                                                                           
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
         CLC   DBSELRMK,=H'104'          PHL                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
         CLC   DBSELRMK,=H'111'          WAS                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
*                                                                               
         CLC   DBSELRMK,=H'105'          DET                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   DBSELRMK,=H'223'          DALLAS FT WORTH                        
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   DBSELRMK,=H'168'          ATLANTA                                
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'6A056A05'                                          
*                                                                               
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
****     B     UPLPMSET                                                         
*&&                                                                             
UPLPMSET OC    UPLPMSD,UPLPMSD                                                  
         BZ    GETHPT11X                                                        
         CLI   DBBTYPE,C'I'                                                     
         BE    GNYHPT1                                                          
         CLI   DBBTYPE,C'P'        KILL PPM PRIOR                               
         BNE   GNYHPT2                                                          
                                                                                
GNYHPT1  CLC   DBSELBK,UPLPMSD     COMPARE TO LPM START                         
         BNL   GNYHPT2              IF LESS THE USE REGULAR                     
         MVI   DBBTYPE,0           DEFAULT DIARY                                
         CLI   LPMHSP,C'Y'                                                      
         BNE   GETHPT11X                                                        
         MVI   DBBTYPE,C'H'        DEFAULT HISPANIC                             
         B     GETHPT11X                                                        
*                                                                               
GNYHPT2  DS    0C                                                               
                                                                                
GNYHPT4  DS    0C                                                               
         CLC   DBSELBK,UPLPMED      IF IT'S WITHIN CURRENCY PERIOD              
         BH    GETHPT11X                                                        
         CLC   DBSELBK,UPLPMSD                                                  
         BL    GETHPT11X                                                        
         MVI   DBBTYPE,C'P'         USE THE LPM HPT DATA                        
         CLI   LPMHSP,C'Y'                                                      
         BNE   GETHPT11X                                                        
         MVI   DBBTYPE,C'I'                                                     
                                                                                
GETHPT11X DS 0C                                                                 
         OC    DUALTRMK,DUALTRMK   CHECK FOR MARKET OVERRRIDE                   
         BZ    *+16                 FROM STATION RECORD                         
         MVC   DBSELRMK,DUALTRMK                                                
         MVC   DBACTRMK,DUALTRMK                                                
*                                                                               
         L     R1,AIO2           BUILD A DUMMY DEMO RECORD IN IO2               
         XC    0(50,R1),0(R1)                                                   
         LA    RE,PRFRSTEL-PRKEY+1                                              
         STCM  RE,3,PRRLEN-PRKEY(R1)                                            
*                                  TEST FOR INDEX=FF DAY/TIME CALL              
         CLI   INDEXFF,C'Y'                                                     
         JNE   GETHPT1A                                                         
         CLI   INDEXTYP,C'D'                                                    
         JNE   GETHPT1A                                                         
         L     R1,ADBLOCK          YES - GET DAY/TIME FROM USER DBLOCK          
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         J     GETHPT16                                                         
*                                                                               
GETHPT1A CLI   FILETYPE,C'I'       TEST FOR INVENTORY RECORD                    
         JNE   GETHPT14                                                         
*                                  EXTRACT LOOKUP DATA FROM INV RECORD          
         MVI   DYTMELCD,X'09'      MULTI HUT DAY/TIME                           
         ICM   R3,15,AIMHEL                                                     
         JNZ   ALTDYTM              YES - USE THESE                             
         MVI   DYTMELCD,X'CE'      ROVER ALT/PUT                                
         ICM   R3,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         JZ    GETHPT12             NO - ACCEPT USER INPUT                      
ALTDYTM  DS    0C         NOTE - BOTH CAN USE RINVZEL FOR DAY/TIME              
         USING RINVZEL,R3                                                       
         OC    RINVZDAY(5),RINVZDAY                                             
         JZ    GETHPT12                                                         
         MVC   DBSELDAY,RINVZDAY   YES - EXTRACT DAY/TIME FROM ELEMENT          
         MVC   DBSELTIM,RINVZTIM                                                
         SR    R1,R1                                                            
         ICM   R1,3,DBSELTIM                                                    
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'                                                      
         JNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         CLC   DBSELTIM+2(2),=C'CC'                                             
         JNE   *+8                                                              
         STCM  R1,3,DBSELTIM+2                                                  
* POSSIBLE MULTI DAY/TIME ELEMENTS                                              
         XC    DYTMLIST,DYTMLIST   INITIALIZE LIST                              
         ZIC   R1,1(R3)                                                         
         AR    R1,R3                                                            
         CLC   0(1,R1),DYTMELCD    MULTIPLE DAY/TIME ELEMENTS                   
         JNE   GETHPT16                                                         
         LA    RF,DYTMLIST         GET BY ANY PREV EXTENSIONS                   
         L     RE,DBEXTEND                                                      
         LTR   RE,RE                                                            
         JZ    BLDDTL                                                           
         CLC   4(4,RE),=XL4'00'                                                 
         JE    *+12                                                             
         L     RE,4(RE)                                                         
         J     *-14                                                             
         ST    RF,4(RE)            LINK TO THIS EXTENSION                       
         J     *+8                                                              
BLDDTL   ST    RF,DBEXTEND                                                      
         MVC   DYTMLIST(4),=C'DYTM' BUILD THE DAY/TIME LIST                     
         LA    R1,DYTMLIST                                                      
         LA    R1,9(R1)                                                         
BLDDTL2  MVC   0(1,R1),RINVZDAY                                                 
         MVC   1(4,R1),RINVZTIM                                                 
         ZIC   R0,1(R3)                                                         
         LA    R1,5(R1)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),DYTMELCD                                                 
         JE    BLDDTL2                                                          
         J     GETHPT16                                                         
*                                  EXTRACT DAY/TIME FROM KEY                    
GETHPT12 L     R2,ARECORD                                                       
RKEY     USING RINVD,R2                                                         
         MVC   DUB(1),RKEY.RINVKDAY                                             
         IC    R1,DUB                                                           
         SLL   R1,4                                                             
         STC   R1,DUB                                                           
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,GETDAY1          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),RKEY.RINVKQTR                                             
         MVC   DUB+1(1),RKEY.RINVKLEN                                           
         GOTO1 VSUBR01,DMCB,('INVTIMQ',(RC))   CNV TIME TO MLTY                 
*        BAS   RE,GETIME1          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
         J     GETHPT16                                                         
         DROP  RKEY                                                             
*                                  EXTRACT DAY/TIME FROM PAV RECORD             
GETHPT14 L     R2,ARECORD                                                       
         CLC   0(3,R2),=C'QNN'     NETWORK RECD MAY BE PMKEY FMT                
         JNE   GTHPT14B                                                         
         L     R3,ANETRTEL         NETWORK RUN TIME ELEM                        
         USING NTELEM,R3                                                        
         MVC   DBSELDAY,NTDAY      ALREADY IN DEMAND'S BIT FORMAT               
         MVC   DUB(1),NTSQH                                                     
         J     GTHPT14C                                                         
         DROP  R3                                                               
         USING PRKEY,R2                                                         
GTHPT14B MVC   DUB(1),PRDW                                                      
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,GETDAY2          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),PRSTIM                                                    
*                                                                               
GTHPT14C MVI   DUB+1,2                                                          
         ICM   R3,15,AQTRHREL                                                   
         JZ    GETHPT15                                                         
         IC    R0,PHDUR-PHELEM(R3)                                              
         SRL   R0,1                ROUND TO NEXT LOWEST EVEN QTR HOUR           
         SLL   R0,1                                                             
         STC   R0,DUB+1                                                         
GETHPT15 DS    0H                                                               
         GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))  PUT  DAY IN DEMAND FMT            
*        BAS   RE,GETIME2          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
*                                                                               
GETHPT16 XC    WORK(20),WORK       BUILD DEMOMATH FORMAT BLOCK                  
         LA    R1,DBLOCK                                                        
         ST    R1,WORK                                                          
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),WORK+8                                                
         MVC   WORK+14(1),DBACTSRC                                              
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         OC    DBSELAGY,DBSELAGY                                                
         JNZ   *+10                                                             
         MVC   DBSELAGY,UPAGY                                                   
         XC    DUB(4),DUB          DBDIVSOR COUNTER                             
         MVC   SAVRMKT,DBSELRMK                                                 
*                                                                               
*                                  CALL DEMAND TO GET H/P/T DATA                
*                                  RECORDS ARE PROCESSED AT GETHPT18            
*                                                                               
*  IF ONE OF THE BOOKS IS LIVE ZERO CELL THEN THE CURRENT BOOK WILL             
*  BE ADJUSTED TO THE CORRECT PARALLEL ZEROCELL BOOKTYPE                        
GETHPT17 MVI   ZCELLBTY,0                                                       
*******  GOTO1 VSUBR02,DMCB,('CHKLIVEQ',(RC))                                   
*******  GOTO1 VSUBR02,DMCB,('NONZEROQ',(RC))                                   
*******  GOTO1 VSUBR02,DMCB,('CHKNHUTQ',(RC))                                   
         GOTO1 VSUBR02,DMCB,('CHKLEXPQ',(RC))                                   
* FORCE TRANSPARENCY CODE FOR LIVE ONLY-LIVE+                                   
         MVI   DBVOPT,X'40'                                                     
*                                                                               
* FOR CABLE SPILL WEHAVE TO SWITCH BOOKTYPE                                     
         OC    DBSELMK,DBSELMK     CABLE SPILL                                  
         BZ    GETHP17A            NO CABLE ALREADY SWITCHED ABOVE              
*                                                                               
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
*                                                                               
GETHP17A GOTO1 VDEMAND,DMCB,DBLOCK,GETHPT18                                     
* IF DBERROR = NOTFOUND CHECK TO SEE IF WE ARE PROCESSING ZEROCELL              
* BOOKTYPES 1-4.  IF SO, REREAD FOR NON ZEROCELL.                               
         CLI   DBERROR,X'10'                                                    
         BNE   GETHP17H                                                         
* CHECK IF WE ARE ASKING FOR ZERO CELL DATA                                     
* IF SO IT WILL SET THE BOOKTYPE TO NON ZEROCELL LOOKUP                         
* TOOK OUT NHUT CODE - ONLY DOING NIELSEN EXPANSION                             
*&&DO                                                                           
         CLI   NHUTFLAG,C'Y'      DID WE ALREADY DO THE RELOOKUP?               
         BNE   GETHP17H                                                         
         MVI   NHUTFLAG,C'N'      DID WE ALREADY DO THE RELOOKUP?               
*&&                                                                             
         CLI   NEXPFLAG,C'Y'      DID WE ALREADY DO THE RELOOKUP?               
         BNE   GETHP17H                                                         
         MVI   NEXPFLAG,C'N'      DID WE ALREADY DO THE RELOOKUP?               
         MVC   DBBTYPE,ZSVBKTYP                                                 
         B     GETHP17A                                                         
*******  GOTO1 VSUBR02,DMCB,('NONZEROQ',(RC))                                   
*******  GOTO1 VSUBR02,DMCB,('CHKNHUTQ',(RC))                                   
*******  CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
*******  BNE   GETHP17A           ZERO CELL - REREAD FOR NON ZEROCELL           
* RESTORE FROM SAVED BOOKTYPE AT CHKLIVEZ                                       
GETHP17H MVC   DBBTYPE,ZSVBKTYP                                                 
*                                                                               
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         JNE   EXIT                NO - EXIT ON ANY ERROR SETTING               
         OC    DUB(4),DUB          NOTHING FOUND                                
         JZ    EXIT                                                             
         MVI   DBERROR,0                                                        
         J     GETHPT20                                                         
*                                  ADD DEMO RECORD INTO COMPOSITE               
GETHPT18 NTR1                                                                   
         L     R1,DBAQUART         IGNORE 2 WEEK ONLY DATA                      
         TM    QHWKS-QHELEM(R1),X'10'                                           
         JNZ   EXIT                                                             
         L     RF,AIO2                                                          
         GOTO1 VGETIUN,DMCB,(2,DBLOCK),(RF)                                     
         LA    R0,(OLDHPTX-OLDHPT)/4                                            
         L     R4,AIO2                                                          
         LA    R5,(OLDHPTX-OLDHPT)(R4)                                          
GETHPTML L     RF,0(R4)                                                         
         MH    RF,DBFACTOR                                                      
         A     RF,0(R5)                                                         
         ST    RF,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,GETHPTML                                                      
         LH    R1,DBFACTOR                                                      
         A     R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUALTRMK,DUALTRMK   MARKET OVERRIDE ADJUST THEM                  
         BNZ   *+12                                                             
         CLI   HPTFLAG,C'O'        OLD HPT HAS UNIV AREA                        
         JE    EXIT                                                             
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         JNE   EXIT                                                             
*                   SAVE NEW UNIV FOR IMP BASED UPGRADES                        
         L     RE,AIO2                                                          
         LA    RF,LUNV-IUNREC                                                   
         AR    RF,RE                                                            
         PRINT GEN                                                              
         GOTO1 VGETIUN,DMCB,(5,DBLOCK),(RF)                                     
         L     RE,AIO2                                                          
         LA    RF,LUNV-IUNREC                                                   
         AR    RF,RE                                                            
         LA    R4,LUNV                                                          
         MVC   0(LUNVX-LUNV,R4),0(RF)                                           
         PRINT NOGEN                                                            
         J     EXIT                                                             
         SPACE 2                                                                
*                                  CALL DEMOUT TO CALCULATE H/P/T LINE          
GETHPT20 LA    R4,OLDHPT           R4=A(OLD/NEW HPT LINE)                       
         CLI   HPTFLAG,C'O'                                                     
         JE    *+8                                                              
         LA    R4,NEWHPT                                                        
         L     RF,AIO2                                                          
         LA    RF,(OLDHPTX-OLDHPT)(RF)                                          
         MVC   0(OLDHPTX-OLDHPT,R4),0(RF)                                       
         LA    R0,(OLDHPTX-OLDHPT)/4                                            
GETHPTDV L     RF,0(R4)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DUB                                                           
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,GETHPTDV                                                      
*                                  ROUND NEW H/P/T LINE VALUES                  
         CLI   HPTFLAG,C'N'                                                     
         JNE   GETHPT21                                                         
         L     R0,INDEX            SAVE INDEX VALUE                             
         MVC   INDEX,=F'10000'                                                  
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*        GOTO1 CALC,P1,NEWHPT,0,NEWHPT                                          
         ST    R0,INDEX            RESTORE INDEX VALUE                          
*                                  FOR INDEX=FF XPLY BY WEIGHTING               
GETHPT21 CLI   INDEXFF,C'Y'                                                     
         JNE   GETHPT24                                                         
         LA    R1,OLDHPT                                                        
         LA    R0,(OLDHPTX-OLDHPT)/4                                            
GETHPT22 L     RF,0(R1)                                                         
         MH    RF,HPTWT                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,GETHPT22                                                      
*                                  RESET ORIGINAL DBLOCK VALUES                 
GETHPT24 XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,ARECORD                                                   
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBFILE,FILEDEM                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
**       STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         J     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
         EJECT                                                                  
* GET ADDRESSES OF USEFUL ELEMENTS ON INPUT RECORD                              
*                                                                               
GETELS   L     R1,AFRSTEL                                                       
         SR    R0,R0                                                            
GETELS2  CLI   0(R1),0             TEST E-O-R                                   
         BER   RE                                                               
         CLI   0(R1),X'03'         FROM BOOK ELEMENT                            
         JNE   *+12                                                             
         ST    R1,AINVFREL                                                      
         J     GETELS4                                                          
         CLI   0(R1),X'20'         QUARTER HOUR ELEMENT                         
         JNE   *+12                                                             
         ST    R1,AQTRHREL                                                      
         J     GETELS4                                                          
         CLI   0(R1),X'22'         NETWORK RUNTIME ELEMENT                      
         JNE   *+12                                                             
         ST    R1,ANETRTEL                                                      
         J     GETELS4                                                          
         CLI   0(R1),X'5E'         BOOK FORMAT ELEMENT                          
         JNE   *+12                                                             
         ST    R1,ABOOKEL                                                       
         J     GETELS4                                                          
         CLI   0(R1),X'CD'         CODE ELEMENT                                 
         JNE   *+12                                                             
         ST    R1,AINVCEL                                                       
         J     GETELS4                                                          
         OC    AIMHEL(4),AIMHEL    FIRST ONLY - CAN BE MULTIPLE                 
         JNZ   GETELS3                                                          
         CLI   0(R1),X'09'         MULTI HUT DAY/TIME ELEMENT                   
         JNE   *+12                                                             
         ST    R1,AIMHEL                                                        
         J     GETELS4                                                          
GETELS3  OC    AINVZEL(4),AINVZEL  FIRST ONLY - CAN BE MULTIPLE                 
         JNZ   GETELS4                                                          
         CLI   0(R1),X'CE'         DAY/TIME ELEMENT                             
***      JNE   *+12                                                             
         JNE   *+8                                                              
         ST    R1,AINVZEL                                                       
*****    J     GETELS4                                                          
GETELS4  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     GETELS2                                                          
         EJECT                                                                  
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
         EJECT                                                                  
* CALCULATE ORIGINAL SHARE VALUES FROM OLD RATINGS & OLD H/P/T LINE             
*&&DO                                                                           
GETSHR   NTR1                                                                   
         SR    RE,RE               RE=INDEX REGISTER                            
         LA    RF,OLDRTGLN/4       RF=NUMBER OF SHARE VALUES                    
GETSHR2  L     R0,OLDRTG(RE)       (RATING*100+(PUT/2))/PUT=SHARE               
         MH    R0,=H'1000'                                                      
         L     R1,OLDHPT(RE)                                                    
         LTR   R1,R1               TEST FOR ZERO PUT                            
         JZ    GETSHR4                                                          
         SRL   R1,1                                                             
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         D     R0,OLDHPT(RE)                                                    
GETSHR4  ST    R1,OLDSHR(RE)       SET SHARE & BUMP TO NEXT                     
         LA    RE,4(RE)                                                         
         BCT   RF,GETSHR2                                                       
*                                  RESTORE HOMES SHARE VALUES (BOOK)            
         MVC   OLDSHR+UORHOMES-OLDRTG(HOMSHRLN),HOMSHR                          
*                                  NOW ROUND THE SHARES YET AGAIN               
         J     EXIT                                                             
*&&                                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
*-->MAT  DC    C'INVUIUN',X'520500'                                             
OFORMAT  DC    C'INVUIUN',X'530B00'                                             
BOOK8312 DC    AL1(83,12)        FOR SHARE RECALC INDEX UPG(RTG BASED)          
BOOK9012 DC    AL1(90,12)        FOR SHARE RECALC INDEX UPG(IMP BASED)          
INVVALS  DC    C'IINVREPFILE '                                                  
         DS    0H                                                               
TPTVALS  DC    C'TTP DEMFILE '     (MUST BE HALFWORD ALIGNED FOR LARL)          
PAVVALS  DC    C'PPAVPAVFILE '                                                  
BOOKZ    DC    X'0000'                                                          
*                                                                               
RHOMES   DC    C'R',AL1(1)                                                      
PHOMES   DC    C'P',AL1(1)                                                      
SHOMES   DC    C'S',AL1(1)                                                      
*---->   DC    C'&&',AL1(255)      INDEX DEMO MODIFIER/NUMBER                   
NDXDEMO  DC    C'&&',AL1(254)      INDEX DEMO MODIFIER/NUMBER                   
         EJECT                                                                  
*                                  CONVERT INTERNAL DAY TO DEMAND FMT           
*                                  DEFAULT BOOK ELEMENT                         
BOOKEL   DC    X'5E07',C'IUN',X'530B'                                           
NETSHRS  DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
*                                  CONVERT INV DURATION INTO QTR HOURS          
* NOTE - THE TIMES IN THE TABLE BELOW ARE THE MOST RESTRICTIVE                  
* TIMES DESCRIBED BY THE CODE. IN OTHER WORDS A CODE OF 1 WHICH                 
* REPRESENTS 1/2-1 HOUR PROGRAMS WILL LOOK UP HUTS/PUTS FOR 1/2 HOUR            
* THIS CAUSES A LOT OF GRIEF IF THIS TABLE IS USED. IT'S BETTER TO USE          
* THE TIMES FROM THE X'CE' ELEMENT.                                             
         DS    0H                                                               
DURTAB   DS    0XL2                (MUST BE HALFWORD-ALIGNED FOR LARL)          
         DC    X'00',AL1(02)                                                    
         DC    X'01',AL1(02)                                                    
         DC    X'02',AL1(04)                                                    
         DC    X'03',AL1(06)                                                    
         DC    X'04',AL1(08)                                                    
         DC    X'05',AL1(10)                                                    
         DC    X'06',AL1(14)                                                    
         DC    X'07',AL1(20)                                                    
         DC    X'08',AL1(28)                                                    
         DC    X'09',AL1(32)                                                    
         DC    X'FF',AL1(02)                                                    
       ++INCLUDE RESVCTABN                                                      
         SPACE 2                                                                
         DROP  RB,RA,R9                                                         
*                                                                               
* NORMALIZE NEW HUTS/PUTS TO SHARE UNIVERSE                                     
NORMU    NTR1  BASE=*,LABEL=*                                                   
         CLI   TAPEOPT,C'Y'                                                     
         JNE   NORMUX                                                           
         CLC   OLDUNV(OLDUNVLN),LUNV  NO CHANGE - DON'T NEED INDEX              
         JE    NORMUX                                                           
         OC    LUNV(20),LUNV       NO NEW - DON'T NEED INDEX                    
         JZ    NORMUX                                                           
*      ************************************************                         
         LA    R0,IUNDEMS          CALCULATE UNIVERSE INDEX                     
         LA    R1,LUNV                                                          
         LA    R7,UPUIDX                                                        
         LA    R8,OLDUNV                                                        
                                                                                
NORMU1   L     RF,0(R8)            FIRST CALC INDEX                             
         M     RE,=F'20000'        OLD UNIVERSE/NEW UNIVERSE                    
         CLC   0(4,R1),=XL4'00'                                                 
         JE    *+12                                                             
         D     RE,0(R1)                                                         
         J     *+8                                                              
         L     RF,=F'20000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R7)                                                         
         LA    R8,4(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMU1                                                        
*     *************************************************                         
         LA    R0,IUNDEMS          CALCULATE ADJUSTED HUTS/PUTS                 
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
NORMUX   XIT1  ,                                                                
*                                                                               
* NORMALIZE SHARE UNIVERSE TO HPT UNIVERSE (DUALTRMK<>0)                        
NORMM    NTR1  BASE=*,LABEL=*                                                   
         CLI   TAPEOPT,C'Y'                                                     
         JNE   NORMMX                                                           
         CLC   OLDUNV(OLDUNVLN),LUNV  NO CHANGE - DON'T NEED INDEX              
         JE    NORMMX                                                           
         OC    LUNV(20),LUNV       NO NEW - DON'T NEED INDEX                    
         JZ    NORMMX                                                           
*      ************************************************                         
         LA    R0,IUNDEMS          CALCULATE UNIVERSE INDEX                     
         LA    R8,LUNV                                                          
         LA    R7,UPUIDX                                                        
         LA    R1,OLDUNV                                                        
                                                                                
NORMM1   L     RF,0(R8)            FIRST CALC INDEX                             
         M     RE,=F'20000'        NEW UNIVERSE/OLD UNIVERSE                    
         CLC   0(4,R1),=XL4'00'                                                 
         JE    *+12                                                             
         D     RE,0(R1)                                                         
         J     *+8                                                              
         L     RF,=F'20000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R7)                                                         
         LA    R8,4(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMM1                                                        
*     *************************************************                         
         LA    R0,IUNDEMS          CALCULATE ADJUSTED HUTS/PUTS                 
         LA    R1,OLDHPT                                                        
         LA    R7,UPUIDX                                                        
NORMM2   L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMM2                                                        
*                                                                               
         LA    R0,IUNDEMS          CALCULATE ADJUSTED HUTS/PUTS                 
         LA    R1,OLDRTG                                                        
         LA    R7,UPUIDX                                                        
NORMM2R  L     RF,0(R1)                                                         
         M     RE,0(R7)                                                         
         SLDL  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R7,4(R7)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,NORMM2R                                                       
         MVC   OLDUNV(OLDUNVLN),LUNV  NO CHANGE - DON'T NEED INDEX              
*                                                                               
NORMMX   XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
INIT2    NTR1  BASE=*,LABEL=*                                                   
         ST    R7,AFRSTEL                                                       
         LA    R8,0(R8)            REMOVE H.O.B. (PREC CNTRL FLAG)              
         ST    R8,AUPGDEL                                                       
         MVI   PUTUPGD,C'N'                                                     
         MVI   IUNFLAG,C'N'                                                     
         MVI   UPTIMCHG,0                                                       
         MVI   Y4ASW,X'00'                                                      
         MVI   Y4MSW,X'00'                                                      
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4SYRS,2                                                         
         MVI   MMUPASS,0                                                        
         MVI   ROLLFLAG,C'N'                                                    
         CLI   8(R1),X'FA'         CHECK OV ROLLING AVG                         
         BNE   *+8                                                              
         MVI   ROLLFLAG,C'Y'                                                    
         L     RF,=A(SUBR01)                                                    
         A     RF,RELO                                                          
         ST    RF,VSUBR01                                                       
         L     RF,=A(SUBR02)                                                    
         A     RF,RELO                                                          
         ST    RF,VSUBR02                                                       
         L     R0,=V(DEINMKT)                                                   
         A     R0,RELO                                                          
         ST    R0,VDEINMKT                                                      
         LR    RF,RC                                                            
         AH    RF,=Y(IO2-DEMUPD)                                                
         ST    RF,AIO2             GET ADDRESSABILITY TO IO2                    
         LR    RF,RC                                                            
         AH    RF,=Y(IO3-DEMUPD)                                                
         ST    RF,AIO3             GET ADDRESSABILITY TO IO2                    
         LARL  RE,DURTAB                                                        
         ST    RE,ADURTAB                                                       
         LARL  RE,TPTVALS                                                       
         ST    RE,ATPTVALS                                                      
         L     RF,12(R1)           PICK UP THE REP ID                           
         XC    UPIMS,UPIMS                                                      
*                                                                               
         CLC   0(4,RF),=C'RID='    JUST THE AGENCY                              
         JNE   P1X                                                              
         MVC   UPAGY(2),4(RF)                                                   
         XC    0(8,RF),0(RF)                                                    
         J     P4X                                                              
*                                                                               
P1X      CLC   0(4,RF),=C'RI2='    AGENCY AND TIME CHANGE                       
         JNE   P2X                                                              
         MVC   UPAGY(2),4(RF)                                                   
         MVC   UPTIMCHG(1),6(RF)                                                
         XC    0(8,RF),0(RF)                                                    
         J     P4X                                                              
*                                                                               
P2X      CLC   0(4,RF),=C'RDB='    A VALID DBLOCK                               
         JNE   P3X                                                              
         L     RE,4(RF)                                                         
         MVC   UPAGY(2),DBSELAGY-DBLOCK(RE)                                     
         MVC   UPTIMCHG(1),DBTIMCHG-DBLOCK(RE)                                  
         MVC   UPDB(4),4(RF)                                                    
         XC    0(8,RF),0(RF)                                                    
         J     P4X                                                              
*                                                                               
P3X      CLC   0(4,RF),=C'RI3='    AGENCY  AND  TIME CHANGE, IMS                
         JNE   P4X                                                              
         MVC   UPAGY(2),4(RF)                                                   
         MVC   UPTIMCHG(1),6(RF)                                                
         MVC   UPIMS(4),8(RF)      LIST OF IMS AFFILIATES                       
         XC    0(8,RF),0(RF)                                                    
****     J     P4X                                                              
*                                                                               
P4X      DS    0H                                                               
**********************************************************                      
         CLI   4(R1),C'I'                                                       
         JNE   *+8                                                              
         MVI   TAPEOPT,C'Y'        IMP BASED UPGRADES                           
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
**********************************************************                      
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A29' REGETIUN                              
         MVC   VGETIUN,0(R1)                                                    
*                                  SET FILE VALUES FOR INPUT RECORD             
* SET UP ANY ALTERNATE MARKETS                                                  
         XC    DUALTRMK,DUALTRMK                                                
         OC    UPAGY,UPAGY       ANY REP GIVEN                                  
         BZ    FNDSELX            NO - DON'T READ STATION RECORD                
         B     *+10              NOOP TO TEST ALT MKT LOOKUP                    
         MVC   DUALTRMK,=H'104'                                                 
* FIND THE STATION                                                              
         LR    R1,R7                                                            
         LA    RE,PRFRSTEL-PRKEY                                                
         SR    R1,RE                                                            
         CLC   0(3,R1),=C'QNN'   NETWORK DATA SOURCE                            
         JE    INITNET                                                          
         CLI   0(R1),C'P'        PAV DATA SOURCE                                
         BE    INITPAV                                                          
         CLI   0(R1),C'T'        TIME PERIOD DATA SOURCE                        
         BE    INITPAV                                                          
         LR    R1,R7             INVENTORY FILE SOURCE                          
         LA    RE,RINVPEL-RINVREC                                               
         SR    R1,RE                                                            
         MVC   DBSELSTA,RINVKSTA-RINVKEY(R1)                                    
         B     *+10                                                             
INITPAV  MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
         B     *+10                                                             
INITNET  MVC   DBSELSTA,PRSTAT-PRKEY(R1)                                        
*                                                                               
         LA    R1,KEY            BUILD A DUMMY DEMO RECORD IN IO2               
         XC    0(40,R1),0(R1)                                                   
         USING RSTAREC,R1                                                       
         MVI   RSTAKEY,X'02'                                                    
         MVC   RSTAKREP,UPAGY                                                   
         MVC   RSTAKSTA,DBSELSTA                                                
         CLI   RSTAKSTA+4,C'1'                                                  
         JE    *+8                                                              
         CLI   RSTAKSTA+4,C'T'                                                  
         JNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,         X        
               0,0                                                              
         CLI   DMCB+8,X'10'                                                     
         JE    FNDSELX                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RF,AIO3                                                          
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,(RF),        X        
               DMWORK,0                                                         
         PRINT NOGEN                                                            
         L     R1,AIO3                                                          
         LA    RE,RSTAELEM                                                      
RSTAD    USING RSTADMEL,RE                                                      
         SR    RF,RF                                                            
FNDSEL   CLI   1(RE),0                                                          
         BE    FNDSELX                                                          
         CLI   0(RE),X'11'                                                      
         BE    FNDSEL2                                                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     FNDSEL                                                           
*                                                                               
FNDSEL2  CLI   RSTAD.RSTADMCD,0                                                 
         BNE   FNDSELX                                                          
         MVC   DUALTRMK,RSTAD.RSTADMCD+1                                        
*        MVI   TAPEOPT,C'N'              FORCE IT TO RATINGS BASED              
FNDSELX  DS    0C                                                               
*                                                                               
         DROP  R1,RSTAD                                                         
* THIS FOLLOWING SETS UP YRSAVTYP WHICH IS USED TO INDICATE                     
* WE ARE DOING SOME SORT OF SAVG OR PAVG UPGRADE                                
* WHICH WILL USE THE EXTENDED DOUBLE WORD BUFFERS                               
* ZAP INVENTORY BYTE TO MAKE SURE WHICH UPGRADE WE ARE PROCESSING               
*                                                                               
         ICM   R1,15,AUPGDEL                                                    
         USING RAVLNEL,R1                                                       
         MVC   BYTE,RAVLNTYP                                                    
         NI    BYTE,B'11011111'                                                 
         MVI   YRSAVTYP,0                                                       
*                                                                               
         CLI   BYTE,13                                                          
         BE    *+8                                                              
         CLI   BYTE,14                                                          
         BNE   INITAVX                                                          
         MVI   YRSAVTYP,C'G'         DEFAULT SAVG/PAVG - SET TO G               
         CLI   RAVLNOP2,C'Q'         PAQ/SAQ/PAY/SAY/PAB/SAB                    
         BE    *+8                                                              
         CLI   RAVLNOP2,C'B'         PAQ/SAQ/PAY/SAY/PAB/SAB                    
         BE    *+8                                                              
         CLI   RAVLNOP2,C'Y'                                                    
         BNE   *+10                                                             
         MVC   YRSAVTYP,RAVLNOP2                                                
         DROP  R1                                                               
INITAVX  DS    0C                                                               
* FUDGE                                                                         
         MVC   DBSELSTA,=C'WUABT'                                               
*                                                                               
*                                                                               
         XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
         DROP  RC                                                               
*********************************************************************           
* SUBR01 - SUBROUTINE BLOCK 01                                                  
*********************************************************************           
SUBR01   NMOD1 0,**SR01**,RA,RR=RE                                              
         L     RC,0(R1)                                                         
         USING DEMUPD,RC                                                        
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
INVTIMQ  EQU   (INVTIM#-*)/4+1                                                  
GETTIMQ  EQU   (GETTIM#-*)/4+1                                                  
CALCQ    EQU   (CALC#-*)/4+1                                                    
STNHPTS  EQU   (STNHPT#-*)/4+1                                                  
PUTSHRQ  EQU   (PUTSHR#-*)/4+1                                                  
LPMCHKQ  EQU   (LPMCHK#-*)/4+1                                                  
LPMSIDXQ EQU   (LPMSIDX#-*)/4+1                                                 
GETSHRQ  EQU   (GETSHR#-*)/4+1                                                  
LPSPIDXQ EQU   (LPSPIDX#-*)/4+1                                                 
*UPMMUQ   EQU   (UPMMU#-*)/4+1                                                  
SETOVBTQ EQU   (SETOVBT#-*)/4+1                                                 
GETHMKTQ EQU   (GETHMKT#-*)/4+1                                                 
         SPACE 1                                                                
GETYRS#  J     GETYRS              GET MULTI BOOK INDEXES                       
GETDAY#  J     GETDAY              CONVERT DAY TO DEMAND FMT                    
INVTIM#  J     INVTIM              CONVERT INVENTORY TIME TO MILTY              
GETTIM#  J     GETTIME             CONVERT QHR TO MILITARY ST & END             
CALC#    J     CALC                CALCULATES NEW ROW VALUES                    
STNHPT#  J     STNHPT              PROCESS IN MARKET SHARES                     
PUTSHR#  J     PUTSHR              PROCESS IN MARKET SHARES                     
LPMCHK#  J     LPMCHK              CHECK FOR LPM PARALLEL PERIODS               
LPMSIDX# J     LPMSINDX            LPM UPGRADE SHARE INDEX                      
GETSHR#  J     GETSHR                                                           
LPSPIDX# J     LPSPINDX            LPM UPGRADE PUT INDEX                        
*UPMMU#   J     UPMMU               MMU CODE                                    
SETOVBT# J     SETOVBKT            SET OVERNIGHTS ROLLING AVG PUT BTYPE         
GETHMKT# J     GETHMKT             GET HOME MARKET                              
         EJECT                                                                  
*********************************************************************           
*GETYRS - ?                                                                     
*********************************************************************           
GETYRS   L     RE,AIO2                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
*                                                                               
         L     R2,AUPGDEL                                                       
         USING RAVLNEL,R2                                                       
*                                                                               
         CLC   =X'9999',RAVLNOP3       LPS UPGRADE                              
         BNE   GTYRS001                                                         
         XC    BOOKLIST,BOOKLIST                                                
         MVC   BOOKLIST(2),RAVLNOP2                                             
         MVC   BOOKLIST+2(2),RAVLNOP1    LPS SEASONAL TRENDING BOOKS            
                                                                                
GTYRS001 DS    0C                                                               
*                                                                               
*                                                                               
*    FOR SAQ/PAQ  WE NEED TO CREATE THE BOOKLIST                                
         CLI   YRSAVTYP,0      PAB,SAB,PAQ,SAQ,PAY,SAY                          
         BE    GETYRS1A                                                         
         CLI   YRSAVTYP,C'G'   PAVG,SAVG DONT SET UP SPECIAL BOOKLIST           
         BE    GETYRS1A                                                         
         CLI   YRSAVTYP,C'Y'   PAVG,SAVG DONT SET UP SPECIAL BOOKLIST           
         BE    GETYRS1A                                                         
         XC    BOOKLIST,BOOKLIST                                                
         CLI   YRSAVTYP,C'B'   PAB/SAB                                          
         BNE   *+14                                                             
         MVC   BOOKLIST(2),RAVLNOP1                                             
         B     GETYRS1A                                                         
         CLI   YRSAVTYP,C'Q'   PAQ/SAQ                                          
         BNE   GETYRS1A                                                         
*                                                                               
         ZIC   RF,RAVLNOP1     GRAB YEAR                                        
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
* NOW QTRBKTAB HAS LATEST AND EARLIEST 4 QTRS BOOKS POSSIBLE                    
* DEPENDING ON THE REQUEST                                                      
         LA    RE,QTRBKTAB                                                      
         CLC   RAVLNOP1,0(RE)  COMPARE START BOOK TO RANGE IN TABLE             
         BL    *+12                                                             
         AHI   RE,2                                                             
         B     *-14                                                             
         ICM   R0,1,RAVLNOP2+1 NUMBER OF QTRS                                   
         SHI   RE,2                                                             
         LA    RF,BOOKLIST                                                      
*                                                                               
GTYRS002 DS    0C                                                               
         MVC   0(2,RF),0(RE)   CREATE BOOKLIST                                  
         AHI   RF,2                                                             
         SHI   RE,2                                                             
         BCT   R0,GTYRS002                                                      
GETYRS1A  DS   0C                                                               
         DROP  R2                                                               
*                                                                               
         XC    TOTHMSHR,TOTHMSHR                                                
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVI   FORCESW,C'N'        INITIALIZE SPECIAL SURVEY SWITCH             
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         OC    UPAGY,UPAGY         SET AGY IF GIVEN                             
         JZ    *+10                                                             
         MVC   DBSELAGY,UPAGY                                                   
*        MVC   DBFILE,TPTVALS+1                                                 
         L     RF,ATPTVALS                                                      
         MVC   DBFILE(L'DBFILE),1(RF)                                           
*        CLI   FILETYPE,C'I'                                                    
*        JNE   *+12                                                             
*        MVI   DBSELMED,C'U'       IF INV FILE, DEFAULT TO 'U'                  
*        J     *+8                                                              
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
         CLC   0(2,R1),=H'1000'    TEST IF BOOK PASSED                          
         JNH   *+10                                                             
*        MVC   DBSELBK,RAVLNOP1    YES - SET SELECTED BOOK                      
         MVC   DBSELBK,0(R1)       YES - SET SELECTED BOOK                      
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
**       STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
*                                  VALIDATE STATION TO GET MARKET               
GETYRS1  L     R2,ARECORD                                                       
         CLI   FILETYPE,C'I'       TEST FOR INVENTORY FILE                      
         JNE   GETYRS4                                                          
*                                  EXTRACT STATION/BOOK/SOURCE FROM             
*                                  INVENTORY RECORD KEY/ELEMENTS                
         USING RINVD,R2                                                         
         ICM   R3,15,AINVFREL      TEST IF FROM BOOK ELEMENT FOUND              
         JZ    GETYRS2                                                          
RFR      USING RINVFREL,R3                                                      
         MVC   DBSELSTA,RFR.RINVFRST  YES - EXTRACT VALUES FROM ELEMENT         
*                                                                               
         MVI   DBSELSRC,C'A'       ARB                                          
*                                                                               
         TM    RFR.RINVFRBK,X'40'  NSI                                          
         JZ    *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         TM    RFR.RINVFRBK,X'41'  SRC                                          
         JNO   *+8                                                              
         MVI   DBSELSRC,C'S'                                                    
*                                                                               
         MVC   DUB(2),RFR.RINVFRBK+1                                            
         MVC   DUB+2(1),RFR.RINVFRBT                                            
         J     GETYRS6                                                          
*                                                                               
GETYRS2  MVC   DBSELSTA,RINVKSTA   EXTRACT STATION/SRC/BOOK FROM KEY            
*&&DO                                                                           
         L     RF,=A(SVCLST)                                                    
         A     RF,RELO                                                          
GETYRS3  CLI   0(RF),X'FF'         END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                *****INVALID KEY SOURCE*********             
         CLC   2(1,RF),RINVKSRC    FIND REAL SOURCE                             
         JE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         J     GETYRS3                                                          
         MVC   DBSELSRC,0(RF)      AND SET IN DBLOCK                            
*&&                                                                             
         MVC   DBSELSRC,RINVKRSR   AND SET IN DBLOCK                            
*                                                                               
         MVC   DUB(2),RINVKBK                                                   
         ICM   R3,15,AINVFREL                                                   
         JZ    *+10                                                             
         MVC   DUB+2(1),RFR.RINVFRBT  TRY TO GET NEW HPTS FOR SPEC SURV         
         J     GETYRS6             FIRST                                        
         DROP  R2,RFR                                                           
*                                  EXTRACT VALUES FROM PAV DEMO KEY             
         USING PRKEY,R2                                                         
GETYRS4  MVC   DBSELSTA,PRSTAT                                                  
         MVC   DBSELSRC,PRSRC                                                   
         MVC   DUB(2),PRBOOK                                                    
         MVC   DUB+2(1),PRBTYP                                                  
*                                                                               
GETYRS6  OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         JNZ   *+16                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DBBTYPE,DUB+2                                                    
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
         MVC   DUB+2(1),DBBTYPE    AND BOOK TYPE                                
*                                                                               
         ICM   R1,15,AUPGDEL                                                    
         USING RAVLNEL,R1                                                       
         CLI   RAVLNBT,0           TEST FOR BOOK TYPE IN UPGRADE EL             
         JE    GETYRS7             NO                                           
         MVC   DBBTYPE,RAVLNBT     YES-IT TAKES PRECEDENCE OVER                 
         MVC   DUB+2(1),RAVLNBT    EVERYTHING ELSE                              
         MVI   FORCESW,C'Y'        MUST FIND HPT'S IN SPECIAL SURVEY BK         
         DROP  R1                                                               
*                                                                               
GETYRS7  DS    0C                                                               
GETYRS10 DS    0C                                                               
*                                                                               
GETYRS11 MVI   DBFUNCT,DBGETDEM                                                 
         CLI   Y4FSW,C'I'                                                       
         JNE   IVTSTX                                                           
         MVC   DBFILE(3),=C'IUN'                                                
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSTYPE,C'N'        ALWAYS REGULAR BOOK                          
         L     R2,ARECORD                                                       
         USING RINVD,R2                                                         
         MVC   DBSELPR4(4),RINVKINV                                             
*        MVC   DBSELAGY,UPAGY                                                   
         L     R1,AIO2                                                          
         LA    R1,IUNWRK-IO2(R1)                                                
*        LA    R1,IUNWRK-IO2                                                    
*        LA    R1,IO2(R1)                                                       
         ST    R1,DBEXTEND                                                      
         XC    0(L'IUNWRK,R1),0(R1)                                             
         MVC   0(4,R1),=C'RINV'                                                 
         DROP  R2                                                               
IVTSTX   L     R1,AIO2             BUILD A DUMMY DEMO RECORD IN IO2             
         XC    0(50,R1),0(R1)                                                   
         LA    RE,PRFRSTEL-PRKEY+1                                              
         STCM  RE,3,PRRLEN-PRKEY(R1)                                            
*                                                                               
         CLI   FILETYPE,C'I'       TEST FOR INVENTORY RECORD                    
         JNE   GETYRS14                                                         
         CLI   Y4FSW,C'I'          IF FROM INV. ALREADY SET UP                  
         JE    GETYRS16                                                         
*                                  EXTRACT LOOKUP DATA FROM INV RECORD          
         ICM   R3,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         JZ    GETYRS12                                                         
RZEL     USING RINVZEL,R3                                                       
         OC    RZEL.RINVZDAY(5),RZEL.RINVZDAY                                   
         JZ    GETYRS12                                                         
         MVC   DBSELDAY,RZEL.RINVZDAY YES - EXTRACT DAY/TIME FROM ELEM          
         MVC   DBSELTIM,RZEL.RINVZTIM                                           
         SR    R1,R1                                                            
         ICM   R1,3,DBSELTIM                                                    
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'                                                      
         JNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         CLC   DBSELTIM+2(2),=C'CC'                                             
         JNE   *+8                                                              
         STCM  R1,3,DBSELTIM+2                                                  
* POSSIBLE MULTI DAY/TIME ELEMENTS                                              
         XC    DYTMLIST,DYTMLIST   INITIALIZE LIST                              
         ZIC   R1,1(R3)                                                         
         AR    R1,R3                                                            
         CLI   0(R1),X'CE'         MULTIPLE DAY/TIME ELEMENTS                   
         JNE   GETYRS16                                                         
         LA    RF,DYTMLIST         GET BY ANY PREV EXTENSIONS                   
         L     RE,DBEXTEND                                                      
         LTR   RE,RE                                                            
         JZ    YBLDTL                                                           
         CLC   4(4,RE),=XL4'00'                                                 
         JE    *+12                                                             
         L     RE,4(RE)                                                         
         J     *-14                                                             
         ST    RF,4(RE)            LINK TO THIS EXTENSION                       
         J     *+8                                                              
YBLDTL   ST    RF,DBEXTEND                                                      
         MVC   DYTMLIST(4),=C'DYTM' BUILD THE DAY/TIME LIST                     
         LA    R1,DYTMLIST                                                      
         LA    R1,9(R1)                                                         
YBLDTL2  MVC   0(1,R1),RZEL.RINVZDAY                                            
         MVC   1(4,R1),RZEL.RINVZTIM                                            
         ZIC   R0,1(R3)                                                         
         LA    R1,5(R1)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'CE'                                                      
         JE    YBLDTL2                                                          
         J     GETYRS16                                                         
         DROP  RZEL                                                             
*                                  EXTRACT DAY/TIME FROM KEY                    
GETYRS12 L     R2,ARECORD                                                       
         USING RINVD,R2                                                         
         MVC   DUB(1),RINVKDAY                                                  
         IC    R1,DUB                                                           
         SLL   R1,4                                                             
         STC   R1,DUB                                                           
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,G1TDAY1          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),RINVKQTR                                                  
         MVC   DUB+1(1),RINVKLEN                                                
         GOTO1 VSUBR01,DMCB,('INVTIMQ',(RC))   CNV TIME TO MLTY                 
*        BAS   RE,G1TIME1          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
         J     GETYRS16                                                         
         DROP  R2                                                               
*                                  EXTRACT DAY/TIME FROM PAV RECORD             
GETYRS14 L     R2,ARECORD                                                       
         USING PRKEY,R2                                                         
         MVC   DUB(1),PRDW                                                      
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,G1TDAY2          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),PRSTIM                                                    
         MVI   DUB+1,2                                                          
         ICM   R3,15,AQTRHREL                                                   
         JZ    GETYRS15                                                         
         IC    R0,PHDUR-PHELEM(R3)                                              
         SRL   R0,1                ROUND TO NEXT LOWEST EVEN QTR HOUR           
         SLL   R0,1                                                             
         STC   R0,DUB+1                                                         
*        BAS   RE,G1TIME2          CONVERT TIME TO DEMAND FORMAT                
GETYRS15 GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))   CNV TIME TO MLTY                 
         MVC   DBSELTIM,DUB+4                                                   
*                                                                               
GETYRS16 XC    WORK(20),WORK       BUILD DEMOMATH FORMAT BLOCK                  
         ST    R7,SAVER7                                                        
         LA    R1,DBLOCK                                                        
         ST    R1,WORK                                                          
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),WORK+8                                                
         MVC   WORK+14(1),DBACTSRC                                              
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         CLI   DBSELMED,0                                                       
         JNE   *+8                                                              
*        CLI   FILETYPE,C'I'                                                    
*        JNE   *+16                                                             
*        MVI   DBSELMED,C'U'       IF INV FILE, DEFAULT TO 'U'                  
*        J     *+8                                                              
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         MVI   DBTPTT,C'P'                                                      
         MVC   DBSELBK(2),=X'600B' FORCE TO NOV/96 FOR NOW                      
*                                                                               
         LA    R1,STATNOL              NEW ORLEANS                              
GETYRS16A CLI   0(R1),X'FF'                                                     
         JE    GETYRS16B                                                        
         CLC   DBSELSTA(4),0(R1)                                                
         JE    *+12                                                             
         LA    R1,4(R1)                                                         
         J     GETYRS16A                                                        
         MVC   DBSELBK(2),=X'6102' FORCE TO FEB/97                              
GETYRS16B DS   0C                                                               
*                                                                               
         LA    R1,STATMEM              MEMPHIS                                  
GETYRS16C CLI   0(R1),X'FF'                                                     
         JE    GETYRS16D                                                        
         CLC   DBSELSTA(4),0(R1)                                                
         JE    *+12                                                             
         LA    R1,4(R1)                                                         
         J     GETYRS16C                                                        
         MVC   DBSELBK(2),=X'6105' FORCE TO MAY/97                              
GETYRS16D DS   0C                                                               
*                                                                               
         LA    R1,STATNASH             NASHVILLE                                
GETYRS16E CLI   0(R1),X'FF'                                                     
         JE    GETYRS16F                                                        
         CLC   DBSELSTA(4),0(R1)                                                
         JE    *+12                                                             
         LA    R1,4(R1)                                                         
         J     GETYRS16E                                                        
         MVC   DBSELBK(2),=X'610A' FORCE TO OCT/97                              
GETYRS16F DS   0C                                                               
*                                                                               
*                                                                               
         LA    R1,STATJAK              JACKSONVILLE                             
GETYRS16G CLI   0(R1),X'FF'                                                     
         JE    GETYRS16H                                                        
         CLC   DBSELSTA(4),0(R1)                                                
         JE    *+12                                                             
         LA    R1,4(R1)                                                         
         J     GETYRS16G                                                        
         MVC   DBSELBK(2),=X'6207' FORCE TO JUL/98                              
GETYRS16H DS   0C                                                               
*                                                                               
         LA    R1,STATBIR    BIRMINGHAM/LAS VAGAS/PROVIDENCE/RALEIGH            
GETYRS16I CLI   0(R1),X'FF'                                                     
         JE    GETYRS16J                                                        
         CLC   DBSELSTA(4),0(R1)                                                
         JE    *+12                                                             
         LA    R1,4(R1)                                                         
         J     GETYRS16I                                                        
         MVC   DBSELBK(2),=X'620B' FORCE TO NOV/98                              
GETYRS16J DS   0C                                                               
         OC    MMUBOOK,MMUBOOK     MMU UPGRADE                                  
         JZ    GETYRS16K                                                        
         MVC   DBSELBK,MMUBOOK     OVERRIDES ALL                                
         MVI   Y4SYRS,2                                                         
         XC    BOOKLIST,BOOKLIST                                                
*                                                                               
*                                                                               
GETYRS16K DS   0C                                                               
*                                                                               
         MVC   Y4SYRS2,Y4SYRS                                                   
*                                                                               
         OC    BOOKLIST(2),BOOKLIST ANY BOOK LIST                               
         JZ    *+10                                                             
         MVC   DBSELBK(2),BOOKLIST        SET INITIAL BOOK                      
         OC    BOOKLIST+2(2),BOOKLIST+2   ZAP IF SB ONLY                        
         JNZ   *+10                                                             
         XC    BOOKLIST(2),BOOKLIST                                             
*                                                                               
GETYRS16Q DS   0C                                                               
         ZIC   R0,Y4SYRS           NUMBER OF YEARS TO INDEX                     
         CH    R0,=H'2'            NEED AT LEAST 2                              
         JL    GETYRS24            OTHERWISE JUST EXIT                          
         XC    DUB(4),DUB          DBDIVSOR COUNTER                             
*                                  CALL DEMAND TO GET H/P/T DATA                
*                                  RECORDS ARE PROCESSED AT GETYRS40            
         PRINT GEN                                                              
GETYRS17 OC    BOOKLIST(2),BOOKLIST BOOK LIST GIVEN                             
         JZ    GTYRS17B                                                         
         ZIC   RE,BOOKI             YES - INDEX THROUGH IT                      
         LR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BOOKI                                                         
         SLL   RE,1                                                             
         LA    RE,BOOKLIST(RE)                                                  
         CLC   0(2,RE),=X'0000'                                                 
         JE    GETYRS20                                                         
         MVC   DBSELBK,0(RE)                                                    
GTYRS17B DS    0C                                                               
         MVI   HOOKFLAG,C'N'                                                    
         GOTO1 VSUBR01,DMCB,('LPMCHKQ',(RC))                                    
*                                                                               
         CLI   YRSAVTYP,C'B'   PAB/SAB                                          
         BE    GTYRS17C                                                         
         CLI   YRSAVTYP,C'G'   PAVG/SAVG                                        
         BE    GTYRS17C                                                         
****     OC    UPLPMSD,UPLPMSD                                                  
****     BNZ   GTYRS17C                                                         
         CLC   =AL2(FEB_09),DBSELBK  THERE IS NO FEB09 BOOK                     
         BNE   GTYRS17C                                                         
         MVC   DBSELBK,=AL2(MAR_09)  FORCE TO MAR09                             
*                                                                               
GTYRS17C GOTO1 VSUBR02,DMCB,('CHKLIVMQ',(RC))                                   
*                                                                               
*                                                                               
******   GOTO1 VSUBR02,DMCB,('CHKLIVEQ',(RC))                                   
*******  GOTO1 VSUBR02,DMCB,('NONZEROQ',(RC))                                   
******   GOTO1 VSUBR02,DMCB,('CHKNHUTQ',(RC))                                   
         GOTO1 VSUBR02,DMCB,('CHKLEXPQ',(RC))                                   
         MVI   ZCELLBTY,0                                                       
*                                                                               
* FORCE TRANSPARENCY CODE FOR LIVE ONLY-LIVE+                                   
         MVI   DBVOPT,X'40'                                                     
GTYRS17D GOTO1 VDEMAND,DMCB,DBLOCK,GETYRS40                                     
*        MVC   DBBTYPE,SVLPMBT     NOW RESTORE ORIGINAL BTYP                    
* CHK FOR ZERO CELL BOOKTYPE                                                    
         CLI   DBERROR,X'10'       IF NOT FOUND                                 
         BNE   GTYRS17E                                                         
* TOOK OUT NHUT STUFF ONLY DOING NIELSEN EXPANSION                              
*&&DO                                                                           
         CLI   NHUTFLAG,C'Y'                                                    
         BNE   GTYRS17E                                                         
         MVI   NHUTFLAG,C'N'                                                    
*&&                                                                             
         CLI   NEXPFLAG,C'Y'                                                    
         BNE   GTYRS17E                                                         
         MVI   NEXPFLAG,C'N'                                                    
         MVC   DBBTYPE,ZSVBKTYP   RESET THE BOOKTYPE BACK FROM CHKLIVEZ         
         B     GTYRS17D                                                         
******   CLI   ZCELLBTY,0          ONLY DO IT ONCE                              
******   BNE   GTYRS17E                                                         
******   GOTO1 VSUBR02,DMCB,('NONZEROQ',(RC))                                   
******   GOTO1 VSUBR02,DMCB,('CHKNHUTQ',(RC))                                   
******   CLI   ZCELLBTY,X'FF'     IF ORIGINAL BOOKTYPE WAS                      
******   BNE   GTYRS17D           ZERO CELL - REREAD FOR NON ZEROCELL           
                                                                                
GTYRS17E MVC   DBBTYPE,ZSVBKTYP   RESET THE BOOKTYPE BACK FROM CHKLIVEZ         
         PRINT NOGEN                                                            
         CLI   YRSAVTYP,C'B'       SAB/PAB ?                                    
         BNE   *+12                IF RECORD NOT FOUND THEN GRAB                
         CLI   DBERROR,X'10'       NEXT BOOK                                    
         BE    SLOTSHR3X                                                        
*                                                                               
*                                                                               
*SAQ/PAQ/SAVG/PAVG REMAINING BOOKS IF CURRENT BOOK LOOKUP FAILS                 
         CLI   YRSAVTYP,C'G'       SAVG/PAVG                                    
         BE    *+8                                                              
         CLI   YRSAVTYP,C'Q'       SAQ/PAQ ?                                    
         BNE   GTYRS17H                                                         
         CLI   DBERROR,X'80'                                                    
         BNE   *+14                                                             
         OC    DUB(4),DUB          NOTHING FOUND                                
         BNZ   GTYRS17H                                                         
         L     R7,SAVER7                                                        
         MVI   DBERROR,0                                                        
         ZIC   RE,Y4SYRS2                                                       
         SHI   RE,1                                                             
         STC   RE,Y4SYRS2                                                       
         B     PABLOOPX                                                         
*                                                                               
GTYRS17H CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         JNE   EXIT1               NO - EXIT ON ANY ERROR SETTING               
         OC    DUB(4),DUB          NOTHING FOUND                                
         JZ    EXIT1                                                            
         MVI   DBERROR,0                                                        
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         LA    RF,Y2SHR-Y1SHR                                                   
         MR    RE,RE                                                            
         L     RE,AIO2                                                          
         LA    RE,Y1SHR-IO2(RE)                                                 
*        LA    RE,IO2(RE)                                                       
         AR    RE,RF               POINT TO SHARE SLOT                          
         LA    R4,NEWRTG-OLDUNV                                                 
         LA    R5,NEWHPT-OLDUNV                                                 
         L     R6,AIO2                                                          
         AR    R4,R6               RATINGS                                      
         AR    R5,R6               PUTS                                         
         LA    R1,OLDRTGLN/4                                                    
*                                                                               
         CLI   Y4SSW,C'S'          SHARES NEED PRECISION                        
         JE    SLOTSHR              THIS IS CONFUSING BUT NEEDED                
*                                                                               
         CLI   DBTAPEP,C'Y'        IF IMP BASED                                 
         JNE   SLOTSHR                                                          
         LA    R1,OLDRTGLN/4                                                    
         SRL   R1,1                ONLY RATING/PUT                              
         LR    RF,R6                                                            
CALCPCT  DS    0H                  GET RATING/PUT PERCENTS                      
         CLC   0(4,RF),=XL4'00'     SO USER CAN TRACK WHATS GOING ON            
         JE    CALCPCT2                                                         
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
         L     R7,0(R5)            PUTS                                         
         M     R6,=F'10'                                                        
         SLDA  R6,1                                                             
         D     R6,0(RF)                                                         
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,0(R5)                                                         
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
         LA    R1,OLDRTGLN/4                                                    
*                                                                               
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
         JNZ   *+10                                                             
         SR    R7,R7                                                            
         J     *+6                                                              
         DR    R6,R8                                                            
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(RE)            SAVE IN YEAR SLOT                            
         CLI   Y4SSW,C'P'          DOING A PUT INDEX                            
         JNE   *+10                                                             
         MVC   0(4,RE),0(R5)       SAVE THE PUTS - NOT THE SHARES               
         LA    R7,4                SET UP NEXT SLOT                             
         AR    R4,R7                                                            
         AR    R5,R7                                                            
         AR    RE,R7                                                            
         BCT   R1,SLOTSHR          DO 'EM ALL                                   
*                                                                               
         CLI   Y4SSW,C'S'          ONLY IF WE'RE SAVING THE SHARES              
         JNE   SLOTSHR3X                                                        
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
         XC    DUB,DUB                                                          
         L     RE,AIO2                 RESET FOR NEXT YEAR                      
         L     RF,=F'2000'                                                      
         XCEF                                                                   
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
PABLOOP  STC   RE,DBSELBK+1                                                     
         BCT   R0,GTYRS17B                                                      
         J     GETYRS20                                                         
*                                                                               
PABLOOPX EQU   *                                                                
**                                                                              
         ZIC   RE,DBSELBK          DECREMENT THE YEAR                           
         BCTR  RE,0                                                             
         STC   RE,DBSELBK                                                       
         BCT   R0,GETYRS17         REPEAT FOR N PREV YEARS                      
         J     GETYRS20                                                         
MMULOOP  CLI   MMUPASS,2           SECOND PASS                                  
         BE    GETYRS20             YES - EXIT                                  
         MVI   MMUPASS,2            NO  - SET FOR IT                            
         BCTR  R0,0                DECREMENT SLOT POINTER                       
         CLI   DBBTYPE,C'I'        IF NOT HISPANIC LPM                          
         BNE   MMULOOP2             SET TO REG BOOK                             
         MVI   DBBTYPE,C'H'        OTHERWISE TO HISPANIC REG                    
         B     GTYRS17B                                                         
*                                                                               
MMULOOP2 MVI   DBBTYPE,0           RESET TO DIARY BOOK                          
         B     GTYRS17B                                                         
*                                                                               
*       CALCULATE THE YEAR TO YEAR INDEXES                                      
GETYRS20 L     RE,AIO2                                                          
         LA    RE,Y1SHR-IO2(RE)                                                 
*        LA    RE,IO2(RE)                                                       
         ST    RE,DMCB                                                          
         ST    RE,DMCB+8           SAVE SUM AREA ADDR                           
         LR    R5,RE               SET SUM AREA                                 
         LA    RF,Y2SHR-Y1SHR(RE)                                               
         MVI   DMCB+4,1            FIRST TIME SWITCH                            
*        LA    R0,3                                                             
         ZIC   R0,Y4SYRS           NUMBER OF YEARS                              
         BCTR  R0,0                NUMBER OF INDEX PASSES                       
         LA    R1,(Y2SHR-Y1SHR)/4                                               
GETYRS21 LA    R6,2000             CALC THE YR TO YR INDEXES                    
         L     R7,0(RF)                                                         
         CLI   Y4ASW,C'Y'          DOING AVERAGE NOT INDEX                      
         JE    GETYRA21                                                         
         MR    R6,R6                                                            
         CLC   0(4,RE),=XL4'00'                                                 
         JNE   *+12                                                             
         LA    R7,2000                                                          
         J     *+8                                                              
         D     R6,0(RE)                                                         
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         CLI   DMCB+4,1            IF FIRST TIME                                
         JE    GETYRM21             SAVE THE VALUE                              
GETYRA21 DS    0C                                                               
         CLI   Y4MSW,C'Y'          EITHER SUM OR MULTIPLY THE                   
         JNE   *+12                 INDEXES OR VALUES                           
         MH    R7,2(R5)            MULT FOR GEOMETRIC AVERAGES                  
         J     *+8                                                              
         A     R7,0(R5)            SUM THE INDEXES                              
GETYRM21 ST    R7,0(R5)                                                         
         LA    R5,4(R5)            SUM AREA                                     
         LA    RE,4(RE)            DENOMINATOR                                  
         LA    RF,4(RF)            NUMERATOR                                    
         BCT   R1,GETYRS21                                                      
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
         ZIC   R7,Y4SYRS           SET UP DIVISOR YEARS                         
*                                                                               
*   PAQ/SAQ TAKE LESSER OF Y4SYRS2 AND Y4SYRS                                   
         CLI   YRSAVTYP,C'G'       PAVG/SAVG                                    
         BE    *+8                                                              
         CLI   YRSAVTYP,C'Q'       SAQ/PAQ ?                                    
         BNE   GETYRN21                                                         
         CLC   Y4SYRS,Y4SYRS2                                                   
         BNH   GETYRN21                                                         
         OC    Y4SYRS2,Y4SYRS2     EXIT IF NOTHING FOUND                        
         BZ    EXIT1                                                            
         ZIC   R7,Y4SYRS2                                                       
*                                                                               
GETYRN21 CLI   Y4ASW,C'Y'          AVERAGES OK                                  
         JE    *+6                                                              
         BCTR  R7,0                INDEXES 1 LESS                               
         ST    R7,DMCB                                                          
*                                                                               
         CLI   Y4MSW,C'Y'          GEOMETRIC INDEX                              
         JNE   GETYRS22                                                         
         LA    R6,1                                                             
         J     *+8                                                              
         MH    R6,=H'1000'         DETERMINE THE DIVISOR                        
         BCT   R7,*-4                                                           
         ST    R6,DMCB                                                          
*                                                                               
GETYRS22 L     R7,0(RE)            SUM OF INDEXES                               
         SR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,DMCB             DIVIDE BY YEARS                              
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         ST    R7,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,GETYRS22                                                      
         SPACE 2                                                                
         CLI   Y4SSW,C'P'                                                       
         JE    GETYRS30                                                         
         SPACE 2                                                                
*        NOW INDEX THE CURRENT VALUES TO THIS                                   
*        1. CALCULATE THE OLD SHARE                                             
*        2. INDEX THE OLD SHARE                                                 
*        3. OLD PUT X INDEXED SHARE = OLD RATING/IMP                            
*        3. NEW PUT X INDEXED SHARE = NEW RATING/IMP                            
*                                                                               
         L     RE,AIO2                                                          
         LA    RE,Y1SHR-IO2(RE)                                                 
*        LA    RE,Y1SHR-IO2                                                     
*        LA    RE,IO2(RE)                                                       
         MVC   OLDRTG(OLDRTGLN),SAVRTG                                          
         L     RF,AIO2                                                          
         AHI   RF,(OLDRTG-IUNREC)               RF-->OLD RTG WORK AREA          
         LA    R8,(OLDHPT-OLDRTG)(RF)           R8-->OLD HPT WORK AREA          
         MVC   0(OLDRTGLN,RF),OLDRTG                                            
         MVC   0(OLDHPTLN,R8),OLDHPT                                            
         MVC   (DISPHOM*4)(HOMSHRLN,RF),HOMSHR  FUDGE TO USE ORIGINAL           
         LHI   R1,1000                                                          
         STCM  R1,15,((DISPHOM+0)*4)(R8)         HOMES SHARES IN                
         STCM  R1,15,((DISPHOM+1)*4)(R8)         INDEXING                       
         STCM  R1,15,((DISPHOM+2)*4)(R8)         ANDECALCULATING                
         LA    R0,(OLDRTGX-OLDRTG)/4                                            
         SR    R1,R1                                                            
*                                                                               
GETYRS23 CLI   Y4ASW,C'Y'          AVERAGE NOT INDEX                            
         JNE   *+12                                                             
         L     R7,0(RE)            SET RAW VALUE                                
         J     GETYRA23                                                         
*                                                                               
         SR    R6,R6                                                            
         LHI   R7,2000                                                          
         M     R6,0(RF)            OLDRTG                                       
         CLC   0(4,R8),=XL4'00'                                                 
         JNE   *+12                                                             
         LA    R7,1                                                             
         J     *+8                                                              
         D     R6,0(R8)            OLDHPT (SHARE CALC)                          
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         SR    R6,R6                                                            
         M     R6,0(RE)            NOW INDEX IT                                 
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
GETYRA23 ST    R7,DMCB             SAVE INDEXED OR AVG SHARE                    
         SR    R6,R6                                                            
         M     R6,OLDHPT(R1)       OLDHPT X INDEXED SHARE                       
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,OLDRTG(R1)       OLDRTG (BASED ON INDEXED SHARE)              
                                                                                
         SR    R6,R6                                                            
         L     R7,DMCB             INDEXED SHARE                                
         M     R6,NEWHPT(R1)       NEWHPT X INDEXED SHAREÙ                     
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,NEWRTG(R1)       NEW RATING (BASED ON INDEXED SHR)            
         LA    R7,4                BUMP TO NEXT FIELDS                          
         AR    RE,R7                                                            
         AR    RF,R7                                                            
         AR    R8,R7                                                            
         AR    R1,R7                                                            
         BCT   R0,GETYRS23                                                      
*                                                                               
*&&DO                                                                           
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
*&&                                                                             
         L     RE,AIO2                                                          
         LA    RE,(Y1SHR-IO2)(RE)                                               
         AHI   RE,(DISPHOM*4)       RE-->INDICES FOR HOMES SHARES               
         LA    RF,HOMSHR            RF-->ORIG HOMES SHARE NEED INDEXING         
         LHI   R0,HOMSHRLN/4                                                    
                                                                                
GETYRS23SH DS  0H                                                               
         L     R7,0(RE)             ASSUME 0(RE) = AVG SHR VALUE                
         CLI   Y4ASW,C'Y'                                                       
         JE    GETYRA23SH           IF IT IS, THEN JUST SEED IT IN              
                                                                                
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
         MVC   SAVRTG(OLDRTGLN),OLDRTG BECAUSE IT GETS RESET                    
*                                  RESET ORIGINAL DBLOCK VALUES                 
GETYRS24 XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,ARECORD                                                   
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBFILE,FILEDEM                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
****     STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         J     EXIT1                                                            
         SPACE 2                                                                
*        NOW INDEX THE CURRENT VALUES TO THIS                                   
*        1. INDEX THE OLD PUT                                                   
*        3. OLD PUT X INDEXED SHARE = OLD RATING/IMP                            
*        3. NEW PUT X INDEXED SHARE = NEW RATING/IMP                            
*                                                                               
GETYRS30 L     RE,AIO2                                                          
         LA    RE,Y1SHR-IO2(RE)                                                 
*        LA    RE,Y1SHR-IO2                                                     
*        LA    RE,IO2(RE)                                                       
         LA    R3,OLDUNV                                                        
         LA    RF,OLDRTG                                                        
         LA    R8,OLDHPT                                                        
         LA    R1,NEWHPT                                                        
         LA    R4,NEWRTG                                                        
         LA    R0,(OLDRTGX-OLDRTG)/4                                            
         MVC   OLDRTG(OLDRTGLN),SAVRTG                                          
GETYRS33 L     R7,0(RF)            OLDRTG                                       
         LA    R6,2000                                                          
         MR    R6,R6                                                            
         CLC   0(4,R8),=XL4'00'                                                 
         JNE   *+12                                                             
         LA    R7,1                                                             
         J     *+8                                                              
         D     R6,0(R8)            OLDHPT (SHARE CALC)                          
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,DMCB             SAVE  SHARE                                  
*                                                                               
         CLI   Y4ASW,C'Y'          DOING AVERAGE                                
         JNE   *+12                                                             
         L     R7,0(RE)            YES - JUST REPLACE IT                        
         J     GETYRA33                                                         
*                                                                               
         L     R7,0(R1)            NEWHPT                                       
         SR    R6,R6                                                            
         BAS   R2,GYRSITOR                                                      
         M     R6,0(RE)            NOW INDEX IT                                 
         A     R7,=F'500'                                                       
         D     R6,=F'1000'                                                      
GETYRA33 BAS   R2,GYRSRTOI                                                      
         ST    R7,0(R1)            INDEXED HPT                                  
         L     R6,DMCB             SHARE                                        
         MR    R6,R6                                                            
         SLDA  R6,1                                                             
         D     R6,=F'1000'                                                      
         A     R7,=F'1'                                                         
         SRL   R7,1                                                             
         ST    R7,0(R4)            NEW RATING (BASED ON INDEXED PUT)            
         LA    R7,4                BUMP TO NEXT FIELDS                          
         AR    RE,R7                                                            
         AR    RF,R7                                                            
         AR    R8,R7                                                            
         AR    R1,R7                                                            
         AR    R4,R7                                                            
         AR    R3,R7                                                            
         BCT   R0,GETYRS33                                                      
*                                                                               
         MVC   SAVRTG(OLDRTGLN),OLDRTG BECAUSE IT GETS RESET                    
*                                                                               
         J     GETYRS24                                                         
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
         DS    0H                                                               
*                                  ADD DEMO RECORD INTO COMPOSITE               
GETYRS40 NTR1                                                                   
*----->  L     R1,DBAQUART         IGNORE 2 WEEK ONLY DATA                      
*----->  TM    QHWKS-QHELEM(R1),X'10'                                           
*----->  JNZ   EXIT1                                                            
         MVI   HOOKFLAG,C'Y'                                                    
         CLC   DBFILE,=C'IUN'      INV IS DIFFERENT                             
         JE    GETYRSMI                                                         
         L     RF,AIO2                                                          
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),(RF)                                     
         LA    R0,(OLDHPTX-OLDRTG)/4                                            
         L     R4,AIO2                                                          
         LA    R4,(OLDRTG-OLDUNV)(R4)  POINT TO CURR RATINGS                    
**       LA    R4,IO2+(OLDRTG-OLDUNV)  POINT TO CURR RATINGS                    
         LA    R5,NEWRTG-OLDRTG        AND OFFSET TO SUMMED RATINGS             
         AR    R5,R4                                                            
GETYRSML L     RF,0(R4)                                                         
         MH    RF,DBFACTOR                                                      
         A     RF,0(R5)                                                         
         ST    RF,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,GETYRSML                                                      
         LH    R1,DBFACTOR                                                      
         A     R1,DUB                                                           
         ST    R1,DUB                                                           
*                                                                               
GETYRSHS DS    0H                  GET ORIGINAL HOME SHARES FROM RECD           
         MVC   SVDBACBK,DBACTBK                                                 
         L     R4,AIO2                                                          
         AHI   R4,(HOMSHR-IUNREC)   R4-->OUTPUT AREA                            
         XC    0(HOMSHRLN,R4),0(R4)                                             
         OC    DBAQUART,DBAQUART                                                
         JZ    GETYRSSHX                                                        
         GOTO1 VDEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,(R4),0                        
         MVC   DBACTBK,SVDBACBK                                                 
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
         J     EXIT1                                                            
         SPACE 1                                                                
*                                                                               
GETYRSMI DS    0H                                                               
         MVC   DBFACTOR,=H'1'      BECAUSE ONLY 1 INV RECORD                    
         L     RF,AIO2                                                          
         GOTO1 VGETIUN,DMCB,(9,DBLOCK),(RF)                                     
         LH    R1,DBFACTOR                                                      
         A     R1,DUB                                                           
         ST    R1,DUB                                                           
         CLI   Y4SSW,C'S'          TRANSFORM FOR SHARE INDEX                    
         JNE   EXIT1                                                            
         L     R4,AIO2                 SHARES BASED ON OLD VALUES               
IUN      USING IUNREC,R4                                                        
*                                                                               
         L     R1,IUN.UOPHOMES     ADJUST FOR OVERRIDEN HOMES SHR               
         SR    R0,R0                                                            
         M     R0,IUN.HOMSHR       OLD PUT X OLD SHARE                          
         SLDL  R0,1                                                             
         D     R0,=F'1000'                                                      
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,IUN.UORHOMES     EQUALS OLD RATING                            
*                                                                               
         MVC   IUN.NEWRTG(RTGLN),IUN.OLDRTG                                     
         MVC   IUN.NEWIMPS(RTGLN),IUN.OLDIMP                                    
         MVC   IUN.NEWHPT(RTGLN),IUN.OLDHPT                                     
         MVC   IUN.NEWTOTS(RTGLN),IUN.OLDTOT                                    
         DROP  IUN                                                              
*        J     EXIT1                                                            
         J     GETYRSHS            GO BACK TO GET ORIGINAL HOME SHARE           
         SPACE 1                                                                
EXIT1    L     RE,UPDB              SAVE OFF DBERROR  WHEN THERES A             
         OR    RE,RE                PROBLEM                                     
         BZ    *+18                                                             
         CLI   DBERROR,0                                                        
         BE    *+10                                                             
         MVC   DBERROR-DBLOCK(L'DBERROR,RE),DBERROR                             
         XIT1                      EXIT FROM ROUTINE/MODULE                     
         EJECT                                                                  
*********************************************************************           
* GETDAY - CONVERT INTERNAL DAY CODE INTO DEMAND DAY CODE FORMAT                
*********************************************************************           
*                                                                               
GETDAY   DS    0H                                                               
         LA    R1,DAYTAB           LOOKUP DAY IN TABLE                          
         NI    DUB,X'F0'                                                        
GETDAY10 CLI   0(R1),X'FF'                                                      
         JE    GETDAY20                                                         
         CLC   0(1,R1),DUB                                                      
         JE    GETDAY20                                                         
         LA    R1,2(R1)                                                         
         J     GETDAY10                                                         
GETDAY20 MVC   DUB+4(1),1(R1)                                                   
         J     EXIT1               RETURN TO CALLER                             
*                                                                               
DAYTAB   DC    X'007C1040202030104008500460027001FF7F'                          
QTRBKTBQ DC    X'000200050007000B000200050007000BFFFF'                          
*                                                                               
         SPACE 1                                                                
*******************************************************************             
*INVTIM-  CONVERT INVENTORY DURATION INTO QUARTER HOURS                         
*         FALL INTO GETTIME ROUTINE                                             
*******************************************************************             
*                                                                               
INVTIM   TM    DUB+1,X'F0'                                                      
         BO    *+8                                                              
         MVI   DUB+1,2                                                          
         NI    DUB+1,X'0F'         TURN OFF HIGH NIBBLE                         
         L     R1,=A(DURTAB)           LOOKUP DURATION IN TABLE                 
         A     R1,RELO                                                          
INVTIM5  CLI   0(R1),X'FF'                                                      
         JE    INVTIM10                                                         
         CLC   0(1,R1),DUB+1                                                    
         JE    INVTIM10                                                         
         LA    R1,L'DURTAB(R1)                                                  
         J     INVTIM5                                                          
INVTIM10 MVC   DUB+1(1),1(R1)      SET NUMBER OF QUARTER HOURS                  
*                                                                               
         J     GETTIME                                                          
         SPACE 1                                                                
         EJECT                                                                  
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
         J     EXIT1               RETURN TO CALLER                             
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
         JNH   *+8                                                              
         SH    R1,=H'2400'         YES - SUBTRACT 24 HOURS                      
         BR    RE                                                               
         SPACE 1                                                                
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO CALCULATE NEW ROW VALUES                             *          
* P1=IN1,P2=IN2,P3=OUT                                               *          
* IF P2=0, P1 IS MULTIPLIED BY 'INDEX'                               *          
* IF P2 NOT 0, P1 VALUES ARE MULTIPLIED BY P2 VALUES                 *          
**********************************************************************          
         SPACE 1                                                                
CALC     DS    0H                                                               
         L     R1,PARM01           PT TO INPUT PARM LIST                        
         LM    R4,R6,4(R1)         R4=IN1,R5=IN2,R6=OUT                         
*        LM    R4,R6,P1            R4=IN1,R5=IN2,R6=OUT                         
         SR    RE,RE               CLEAR INDEX REG                              
         LA    RF,OLDRTGLN/4       SET BCT COUNT                                
         LTR   R5,R5               TEST 2 ROWS SPECIFIED                        
         JNZ   CALC10                                                           
*                                                                               
* ONE ROW SPECIFIED - MULTIPLY IT BY VALUE IN INDEX *                           
*                                                                               
CALC2    L     R1,0(R4,RE)                                                      
         SR    R0,R0                                                            
         M     R0,INDEX                                                         
*&&DO                                                                           
         A     R1,=F'5000'         ROUND TO 1 DEC. PRECISION                    
*&&                                                                             
         AL    R1,=F'5000'         ROUND TO 1 DEC. PRECISION                    
         BC    12,*+8              IF RESULTED IN A CARRY,                      
         AHI   R0,1                 REFLECT THAT IN THE HIGHER REGISTER         
         D     R0,=F'10000'                                                     
         ST    R1,0(R6,RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,CALC2                                                         
         J     EXIT1                                                            
*                                                                               
* TWO ROWS SPECIFIED - MULTIPLY CORRESPONDING VALUES *                          
*                                                                               
CALC10   L     R1,0(R4,RE)                                                      
         SR    R0,R0                                                            
         M     R0,0(R5,RE)                                                      
         AH    R1,=H'500'          ROUND TO 1 DEC. PRECISION                    
         D     R0,=F'1000'                                                      
         ST    R1,0(R6,RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,CALC10                                                        
         J     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INDEX SHARES FOR PUT/SHARE UPGRADE                       *         
* EXIT WITH PUTSHRD = Y IF DONE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RAVLNEL,R3                                                       
PUTSHR   DS    0H                                                               
         LA    R0,IUNDEMS*2        CALCULATE NEW RATINGS                        
*                                                                               
         MVI   PUTSHRD,C'N'                                                     
         OC    RAVLNOP4,RAVLNOP4   PUT/SHARE INDEX                              
         JZ    PUTSHRI                                                          
         SR    R1,R1                                                            
         LR    RE,R1                                                            
         ICM   R1,3,RAVLNOP4       GET SHARE INDEX                              
         LR    RF,R1                                                            
         CLI   RAVLNCAT,C'V'       VALUE INPUT                                  
         JE    PUTSHRIV                                                         
         MH    R1,=H'10'           AND ADJUST FOR ROUTINE PRECISION             
         J     PUTSHRI2                                                         
PUTSHRI  DS    0C                                                               
         CLC   RAVLNOP2,=H'1000'   IF IT'S A BOOK IGNORE                        
         JNL   EXIT1                                                            
*                                                                               
         OC    RAVLNOP2,RAVLNOP2   ANY NEW SHARE                                
         JZ    EXIT1               NO - ADJUST RATINGS/IMPS ONLY                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,RAVLNOP2       GET NEW SHARE                                
PUTSHRIV SR    RE,RE               VALUE ENTERED IN RAVLNOP4                    
         M     RE,=F'2000'                                                      
         OC    HOMSHR,HOMSHR       STOP DIVIDE BY ZERO                          
         BNZ   *+10                                                             
         MVC   HOMSHR(4),=F'1'                                                  
         D     RE,HOMSHR           INDEX TO OLD SHARE                           
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         LR    R1,RF                                                            
PUTSHRI2 LA    R0,IUNDEMS*2        INDEX ALL OLD SHARES                         
         LA    R4,OLDSHR                                                        
UPPSH    SR    RE,RE                                                            
         L     RF,0(R4)            REALLY OLD RATINGS(MAY BE IN IMPS)           
* NEW YORK EXCEEDS BINARY LIMIT - MUST DO IN PACKED                             
         CVD   RF,DUB              RATING TO PACKED                             
         ZAP   DPWORK(16),DUB                                                   
         CVD   R1,DUB              INDEX TO PACKED                              
         MP    DPWORK,=P'10'       ADJ FOR ROUND                                
         MP    DPWORK,DUB          INDEX IT                                     
         DP    DPWORK,=PL8'1000'                                                
         ZAP   DUB,DPWORK(8)                                                    
         AP    DUB,=P'5'           ROUND IT                                     
         ZAP   DPWORK(16),DUB                                                   
         DP    DPWORK,=PL8'10'                                                  
         ZAP   DUB,DPWORK(8)                                                    
         CVB   RF,DUB              GET IT IN BINARY                             
*                                                                               
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
         OC    RAVLNOP4,RAVLNOP4                                                
         JNZ   UPPSH3                                                           
         ICM   RF,3,RAVLNOP2       NOW REPLACE OVERRIDEN SHARE                  
         ST    RF,HOMSHR                                                        
UPPSH3   MVI   PUTSHRD,C'Y'                                                     
         J     EXIT1                                                            
* RESTORE PREVIOUS R3 USING STATEMENT                                           
         DROP  R3                                                               
         USING RINVZEL,R3                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OLD OR NEW H/P/T VALUES FROM TIME PERIOD FILE.       *         
* ON ENTRY HPTFLAG=C'O' FOR OLD H/P/T OR C'N' FOR NEW H/P/T WITH      *         
* R1=A(BOOK OPTION).                                                  *         
* EXIT WITH H/P/T VALUES AT OLDHPT OR NEWHPT.                         *         
***********************************************************************         
         SPACE 1                                                                
STNHPT   DS    0H                                                               
         USING STNHPTWK,R8         R8=A(LOCAL W/S)                              
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVI   FORCESW,C'N'        INITIALIZE SPECIAL SURVEY SWITCH             
         XC    STNBK,STNBK                                                      
         L     R1,PARM01                                                        
         ICM   RE,15,4(R1)                                                      
         JZ    *+20                                                             
         CLC   0(2,RE),=X'2000'    TEST IF BOOK PASSED                          
         JNH   *+10                                                             
         MVC   STNBK,0(RE)         YES - SET SELECTED BOOK                      
*                                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
*        CLI   FILETYPE,C'I'                                                    
*        JNE   *+12                                                             
*        MVI   DBSELMED,C'U'       IF INV FILE, DEFAULT TO 'U'                  
*        J     *+8                                                              
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
         MVC   DBSELBK,STNBK                                                    
*                                                                               
         CLI   INDEXFF,C'Y'        TEST FOR INDEX=FF DAY/TIME CALL              
         JNE   STNHPT1                                                          
         CLI   INDEXTYP,C'D'                                                    
         JNE   STNHPT1                                                          
         L     R1,ADBLOCK          YES - GET VALUES FROM USER DBLOCK            
         MVC   DBSELSTA,DBSELSTA-DBLOCK(R1)                                     
         MVC   DBSELSRC,DBSELSRC-DBLOCK(R1)                                     
         MVC   DBSELMED,DBSELMED-DBLOCK(R1)                                     
         MVC   DBSELBK,DBSELBK-DBLOCK(R1)                                       
         MVC   DBBTYPE,DBBTYPE-DBLOCK(R1)                                       
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         J     STNHPT6                                                          
*                                                                               
STNHPT1  L     R2,ARECORD                                                       
         CLI   FILETYPE,C'I'       TEST FOR INVENTORY FILE                      
         JNE   STNHPT4                                                          
         CLI   HPTFLAG,C'O'        AND OLD H/P/T VALUES REQUIRED                
         JNE   STNHPT2                                                          
*                                  EXTRACT STATION/BOOK/SOURCE FROM             
*                                  INVENTORY RECORD KEY/ELEMENTS                
RKEY     USING RINVD,R2                                                         
         ICM   R3,15,AINVFREL      TEST IF FROM BOOK ELEMENT FOUND              
         JZ    STNHPT2                                                          
RFR      USING RINVFREL,R3                                                      
         MVC   DBSELSTA,RFR.RINVFRST  YES - EXTRACT VALUES FROM ELEMENT         
         MVI   DBSELSRC,C'A'       ARB                                          
         TM    RFR.RINVFRBK,X'40'  NSI                                          
         JZ    *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         TM    RFR.RINVFRBK,X'41'  SRC                                          
         JNO   *+8                                                              
         MVI   DBSELSRC,C'S'                                                    
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+12                                                             
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DUB(2),RFR.RINVFRBK+1                                            
         MVC   DUB+2(1),RFR.RINVFRBT                                            
         J     STNHPT6                                                          
*                                                                               
STNHPT2  MVC   DBSELSTA,RKEY.RINVKSTA EXTRACT STATION/SRC/BOOK FROM KEY         
*&&DO                                                                           
         L     RF,=A(SVCLST)                                                    
         A     RF,RELO                                                          
STNHPT3  CLI   0(RF),X'FF'         END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                *****INVALID KEY SOURCE*********             
         CLC   2(1,RF),RINVKSRC    FIND REAL SOURCE                             
         JE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         J     STNHPT3                                                          
         MVC   DBSELSRC,0(RF)      AND SET IN DBLOCK                            
*&&                                                                             
         MVC   DBSELSRC,RKEY.RINVKRSR   AND SET IN DBLOCK                       
*                                                                               
         MVC   DUB(2),RKEY.RINVKBK                                              
         CLI   HPTFLAG,C'N'        TEST FOR READING NEW HPTS                    
         JNE   STNHPT6             NO                                           
         ICM   R3,15,AINVFREL                                                   
         JZ    *+10                                                             
         MVC   DUB+2(1),RFR.RINVFRBT  TRY TO GET NEW HPTS FOR SPEC SURV         
         J     STNHPT6             FIRST                                        
         DROP  R2,RFR                                                           
*                                  EXTRACT VALUES FROM PAV DEMO KEY             
         USING PMKEY,R2                                                         
STNHPT4  CLI   FILETYPE,C'P'       TEST FOR PAV FILE                            
         JNE   STNHPT5                                                          
         CLC   =C'QNN',0(R2)       TEST NHT PRG RECD                            
         JNE   STNHPT5             NO, ASSUME PRKEY FRMT                        
         MVC   DBSELSTA,PMSTAT                                                  
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVC   DUB(2),PMBOOK                                                    
         MVC   DUB+2(1),PMBTYP                                                  
         J     STNHPT6                                                          
         DROP  R2                                                               
*                                                                               
STNHPT5  DS    0H                  NON-NETWORK TV RECS IN PRKEY FRMT            
         USING PRKEY,R2                                                         
         MVC   DBSELSTA,PRSTAT                                                  
         MVC   DBSELSRC,PRSRC                                                   
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+8                                                              
         MVI   DBSELSRC,C'H'                                                    
         MVC   DUB(2),PRBOOK                                                    
         MVC   DUB+2(1),PRBTYP                                                  
*                                                                               
STNHPT6  DS    0H                                                               
         CLI   DBSELSTA+4,C'H'     IF NHT FILE, SOURCE IS 'H'                   
         JNE   *+12               ==================================            
         CLI   INDEXFF,C'Y'        INDEX FF N/A FOR NHTI FILE   !!!             
         JE    EXIT1              ===================================           
*                                                                               
         OC    DBSELBK,DBSELBK     TEST IF BOOK PASSED                          
         JNZ   *+16                                                             
         MVC   DBSELBK,DUB         NO - SET FROM INPUT RECORD                   
         MVC   DBBTYPE,DUB+2                                                    
         MVC   DUB(2),DBSELBK      SAVE SELECTED BOOK VALUE                     
         MVC   DUB+2(1),DBBTYPE    AND BOOK TYPE                                
*                                                                               
         ICM   R1,15,AUPGDEL                                                    
RAVL     USING RAVLNEL,R1                                                       
         CLI   RAVL.RAVLNBT,0      TEST FOR BOOK TYPE IN UPGRADE EL             
         JE    STNHPT7             NO                                           
         MVC   DBBTYPE,RAVL.RAVLNBT    YES-IT TAKES PRECEDENCE OVER             
         MVC   DUB+2(1),RAVL.RAVLNBT   EVERYTHING ELSE                          
         MVI   FORCESW,C'Y'        MUST FIND HPT'S IN SPECIAL SURVEY BK         
         DROP  RAVL                                                             
*                                                                               
STNHPT7  DS    0H                                                               
         CLI   DBSELSTA+4,C'H'     NHTI FILE LK UP?                             
         JNE   STNHPT7A                                                         
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'H'                                                    
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
STNHPT7A LA    R0,2                R0=LOOP COUNT                                
         CLI   INDEXFF,C'Y'        TEST FOR INDEX=FF CALL                       
         JNE   STNHPT8                                                          
         L     R1,ASAVE                                                         
         CLC   0(5,R1),=XL5'00'    TEST IF FIRST TIME                           
         JE    STNHPT8                                                          
         CLC   0(2,R1),DBSELBK     NO - TEST IF BOOK CHANGED                    
         JNE   STNHPT8                                                          
         CLC   2(1,R1),DBBTYPE     NOW TEST FOR CHANGE IN BK TYPE               
         JNE   STNHPT8                                                          
         MVC   DBSELRMK,3(R1)      SET MARKET NUMBER                            
*                                                                               
         CLC   DBSELSTA(4),=C'WWWB'                                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'117'                                                 
*                                                                               
         CLC   DBSELSTA(4),=C'KAKW' FORCE TO AUSTIN                             
         JNE   *+10                                                             
         MVC   DBSELRMK,=H'235'                                                 
*                                                                               
         J     STNHPT11                                                         
*                                                                               
STNHPT8  DS    0H                                                               
         MVC   DBTAPEP,TAPEOPT                                                  
         LA    RE,STNSTLST                                                      
         USING DBXMSD,RE                                                        
         XC    DBXMSD(20),DBXMSD                                                
         MVC   DBXMSID,=C'MSTA'                                                 
         LA    R0,DBXMSTA                                                       
*                                                                               
         OC    DBSELAGY,DBSELAGY                                                
         JNZ   *+10                                                             
         MVC   DBSELAGY,UPAGY                                                   
         GOTO1 VDEINMKT,DMCB,DBLOCK,AAFFLST,(R0)                                
         LA    RE,STNSTLST                                                      
         OC    DBXMSTA,DBXMSTA                                                  
         JZ    STNHPTNX            NO STATIONS TO LOOK UP-- ERROR               
         ST    RE,DBEXTEND                                                      
         DROP  RE                                                               
*                                                                               
STNHPT10 CLI   INDEXFF,C'Y'                                                     
         JNE   STNHPT11                                                         
         L     R1,ASAVE                                                         
         MVC   0(2,R1),DBSELBK                                                  
         MVC   2(1,R1),DBBTYPE                                                  
         MVC   3(2,R1),DBSELRMK                                                 
*                                                                               
STNHPT11 L     R1,AIO2           BUILD A DUMMY DEMO RECORD IN IO2               
         XC    0(50,R1),0(R1)                                                   
         LA    RE,PRFRSTEL-PRKEY+1                                              
         STCM  RE,3,PRRLEN-PRKEY(R1)                                            
*                                  TEST FOR INDEX=FF DAY/TIME CALL              
         CLI   INDEXFF,C'Y'                                                     
         JNE   STNHPT1A                                                         
         CLI   INDEXTYP,C'D'                                                    
         JNE   STNHPT1A                                                         
         L     R1,ADBLOCK          YES - GET DAY/TIME FROM USER DBLOCK          
         MVC   DBSELDAY,DBSELDAY-DBLOCK(R1)                                     
         MVC   DBSELTIM,DBSELTIM-DBLOCK(R1)                                     
         J     STNHPT16                                                         
*                                                                               
STNHPT1A CLI   FILETYPE,C'I'       TEST FOR INVENTORY RECORD                    
         JNE   STNHPT14                                                         
*                                  EXTRACT LOOKUP DATA FROM INV RECORD          
         ICM   R3,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         JZ    STNHPT12                                                         
         USING RINVZEL,R3                                                       
         OC    RINVZDAY(5),RINVZDAY                                             
         JZ    STNHPT12                                                         
         MVC   DBSELDAY,RINVZDAY   YES - EXTRACT DAY/TIME FROM ELEMENT          
         MVC   DBSELTIM,RINVZTIM                                                
         SR    R1,R1                                                            
         ICM   R1,3,DBSELTIM                                                    
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'                                                      
         JNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         CLC   DBSELTIM+2(2),=C'CC'                                             
         JNE   *+8                                                              
         STCM  R1,3,DBSELTIM+2                                                  
* POSSIBLE MULTI DAY/TIME ELEMENTS                                              
         XC    DYTMLIST,DYTMLIST   INITIALIZE LIST                              
         ZIC   R1,1(R3)                                                         
         AR    R1,R3                                                            
         CLI   0(R1),X'CE'         MULTIPLE DAY/TIME ELEMENTS                   
         JNE   STNHPT16                                                         
         LA    RF,DYTMLIST         GET BY ANY PREV EXTENSIONS                   
         L     RE,DBEXTEND                                                      
         LTR   RE,RE                                                            
         JZ    SBLDDTL                                                          
         CLC   4(4,RE),=XL4'00'                                                 
         JE    *+12                                                             
         L     RE,4(RE)                                                         
         J     *-14                                                             
         ST    RF,4(RE)            LINK TO THIS EXTENSION                       
         J     *+8                                                              
SBLDDTL  ST    RF,DBEXTEND                                                      
         MVC   DYTMLIST(4),=C'DYTM' BUILD THE DAY/TIME LIST                     
         LA    R1,DYTMLIST                                                      
         LA    R1,9(R1)                                                         
SBLDDTL2 MVC   0(1,R1),RINVZDAY                                                 
         MVC   1(4,R1),RINVZTIM                                                 
         ZIC   R0,1(R3)                                                         
         LA    R1,5(R1)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'CE'                                                      
         JE    SBLDDTL2                                                         
         J     STNHPT16                                                         
*                                  EXTRACT DAY/TIME FROM KEY                    
STNHPT12 L     R2,ARECORD                                                       
RKEY     USING RINVD,R2                                                         
         MVC   DUB(1),RKEY.RINVKDAY                                             
         IC    R1,DUB                                                           
         SLL   R1,4                                                             
         STC   R1,DUB                                                           
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,GETDAY1          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),RKEY.RINVKQTR                                             
         MVC   DUB+1(1),RKEY.RINVKLEN                                           
         GOTO1 VSUBR01,DMCB,('INVTIMQ',(RC))   CNV TIME TO MLTY                 
*        BAS   RE,GETIME1          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
         J     STNHPT16                                                         
         DROP  RKEY                                                             
*                                  EXTRACT DAY/TIME FROM PAV RECORD             
STNHPT14 L     R2,ARECORD                                                       
         CLC   0(3,R2),=C'QNN'     NETWORK RECD MAY BE PMKEY FMT                
         JNE   STNPT14B                                                         
         L     R3,ANETRTEL         NETWORK RUN TIME ELEM                        
         USING NTELEM,R3                                                        
         MVC   DBSELDAY,NTDAY      ALREADY IN DEMAND'S BIT FORMAT               
         MVC   DUB(1),NTSQH                                                     
         J     STNPT14C                                                         
         DROP  R3                                                               
         USING PRKEY,R2                                                         
STNPT14B MVC   DUB(1),PRDW                                                      
         GOTO1 VSUBR01,DMCB,('GETDAYQ',(RC))   CNV DAY IN DMD FMT               
*        BAS   RE,GETDAY2          CONVERT DAY TO DEMAND FORMAT                 
         MVC   DBSELDAY,DUB+4                                                   
         MVC   DUB(1),PRSTIM                                                    
*                                                                               
STNPT14C MVI   DUB+1,2                                                          
         ICM   R3,15,AQTRHREL                                                   
         JZ    STNHPT15                                                         
         IC    R0,PHDUR-PHELEM(R3)                                              
         SRL   R0,1                ROUND TO NEXT LOWEST EVEN QTR HOUR           
         SLL   R0,1                                                             
         STC   R0,DUB+1                                                         
STNHPT15 GOTO1 VSUBR01,DMCB,('GETTIMQ',(RC))   CNV TIME TO MLTY                 
*        BAS   RE,GETIME2          CONVERT TIME TO DEMAND FORMAT                
         MVC   DBSELTIM,DUB+4                                                   
*                                                                               
STNHPT16 XC    WORK(20),WORK       BUILD DEMOMATH FORMAT BLOCK                  
         LA    R1,DBLOCK                                                        
         ST    R1,WORK                                                          
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),WORK+8                                                
         MVC   WORK+14(1),DBACTSRC                                              
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
         OC    DBSELAGY,DBSELAGY                                                
         JNZ   *+10                                                             
         MVC   DBSELAGY,UPAGY                                                   
         XC    DUB(4),DUB          DBDIVSOR COUNTER                             
*                                                                               
*                                                                               
STNHPT17 XC    STNSTNBK(9),STNSTNBK                                             
         MVI   STNSTCNT,0          AND # STN'S COUNTER                          
         XC    STNUNIV(LENVALS),STNUNIV                                         
         XC    STNRTIMP(LENVALS*2),STNRTIMP                                     
         XC    STNHPUTS(LENVALS*2),STNHPUTS                                     
* FORCE TRANSPARENCY CODE FOR LIVE ONLY-LIVE+                                   
         MVI   DBVOPT,X'40'                                                     
         GOTO1 VDEMAND,DMCB,DBLOCK,STNHK                                        
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         JNE   STNHPTNX            NO - EXIT ON ANY ERROR SETTING               
         OC    DUB(4),DUB          NOTHING FOUND                                
         JZ    STNHPTNX                                                         
         MVI   DBERROR,0                                                        
*                                                                               
         XC    HOMSHR(HOMSHRLN),HOMSHR                                          
*                                                                               
         CLI   STNSTCNT,0                                                       
         JE    STNHPT20                                                         
         SR    R1,R1                                                            
         ICM   R1,15,DUB            DIVIDE DBDIVSOR BY # STATIONS HIT           
         SR    R0,R0                                                            
         SR    R2,R2                                                            
         IC    R2,STNSTCNT                                                      
         DR    R0,R2                                                            
         STCM  R1,15,DUB                                                        
*                                                                               
*                                  ADD DEMO RECORD INTO COMPOSITE               
STNHPT20 LA    R4,OLDHPT           R4=A(OLD/NEW HPT LINE)                       
         CLI   HPTFLAG,C'O'                                                     
         JE    *+8                                                              
         LA    R4,NEWHPT                                                        
         MVC   0(OLDHPTX-OLDHPT,R4),STNHPUTS                                    
         LA    R0,(OLDHPTX-OLDHPT)/4                                            
STNHPTDV L     RF,0(R4)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DUB                                                           
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,STNHPTDV                                                      
*                                  ROUND NEW H/P/T LINE VALUES                  
         CLI   HPTFLAG,C'N'                                                     
         JNE   STNHPT21                                                         
         L     R0,INDEX            SAVE INDEX VALUE                             
         MVC   INDEX,=F'10000'                                                  
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,0,NEWHPT                        
*        GOTO1 CALC,P1,NEWHPT,0,NEWHPT                                          
         ST    R0,INDEX            RESTORE INDEX VALUE                          
*                                                                               
         CLI   HPTFLAG,C'O'                                                     
         JE    *+10                                                             
         MVC   OLDHPT(LENVALS*2),NEWHPT                                         
*                                  FOR INDEX=FF XPLY BY WEIGHTING               
STNHPT21 CLI   INDEXFF,C'Y'                                                     
         JNE   STNHPT24                                                         
         LA    R1,OLDHPT                                                        
         LA    R0,(OLDHPTX-OLDHPT)/4                                            
STNHPT22 L     RF,0(R1)                                                         
         MH    RF,HPTWT                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,STNHPT22                                                      
*                                  RESET ORIGINAL DBLOCK VALUES                 
STNHPT24 XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,ARECORD                                                   
         MVC   DBAQUART,AFRSTEL                                                 
         MVC   DBFILE,FILEDEM                                                   
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBTIMCHG,UPTIMCHG                                                
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         J     STNHPTX                                                          
*                                                                               
STNHPTNX MVI   DBERROR,X'10'       NO STNS FOUND                                
*                                                                               
STNHPTX  J     EXIT1                                                            
         EJECT                                                                  
**********************************************************************          
*STNHK - DEMAND HOOK TO PROCESS DEMO RECDS                                      
**********************************************************************          
STNHK    NTR1                                                                   
         L     R1,DBAQUART         IGNORE 2 WEEK ONLY DATA                      
         TM    QHWKS-QHELEM(R1),X'10'                                           
         JNZ   EXIT1                                                            
         XC    STNUNIV(LENVALS),STNUNIV                                         
         XC    STNRTIMP(LENVALS*2),STNRTIMP                                     
         LA    RF,STNUNIV                                                       
         GOTO1 VGETIUN,DMCB,(1,DBLOCK),STNUNIV  GET RTGS & IMPS                 
         LA    R0,IUNDEMS*2        LOOP THRU ALL RTGS AND IMPS                  
         LA    R4,STNRTIMP         WGTH RTGS & IMPS                             
         LA    R5,STNHPUTS         ACCUM AND SAVE THEM IN HPTS AREA             
STNHK10  L     RF,0(R4)                                                         
         MH    RF,DBFACTOR                                                      
         A     RF,0(R5)                                                         
         ST    RF,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,STNHK10                                                       
         LH    R1,DBFACTOR                                                      
         A     R1,DUB                                                           
         ST    R1,DUB                                                           
         CLI   HPTFLAG,C'O'        OLD HPT HAS UNIV AREA                        
         JE    STNHK20                                                          
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         JNE   STNHK20                                                          
*                              SAVE NEW UNIV FOR IMP BASED UPGRADES             
         LA    R4,LUNV                                                          
         MVC   0(LUNVX-LUNV,R4),STNUNIV                                         
*                                                                               
STNHK20  LA    RE,STNSTNBK         UPDATE STN-HIT TABLE                         
         LA    R0,STNSTNQ          MAX TABLE SIZE                               
STNHK25  CLI   0(RE),0             EOT?                                         
         JE    STNHK28             UPDATE TABLE IF NOT FOUND                    
         CLC   DBSELSTA,0(RE)      MATCH STATION AND BOOK                       
         JNE   *+14                                                             
         CLC   DBACTBK,5(RE)       SAME BOOK?                                   
         JE    STNHKX              MATCH, NO UPDATE NECC                        
         LA    RE,7(RE)                                                         
         BCT   R0,STNHK25                                                       
         DC    H'0'                REACHED MAX STN SIZE                         
STNHK28  MVC   0(5,RE),DBSELSTA                                                 
         MVC   5(2,RE),DBACTBK                                                  
         XC    7(9,RE),7(RE)       CLEAR FOR NEXT ENTRY                         
         ZIC   RE,STNSTCNT         UPDATE NUMBER STATIONS IN TABLE              
         LA    RE,1(RE)                                                         
         STC   RE,STNSTCNT                                                      
*                                                                               
STNHKX   J     EXIT1                                                            
         EJECT                                                                  
*********************************************************************           
* OVERRIDE BOOK TYPE FOR PUT LOOKUP IF WE ARE IN A PARALLEL PERIOD  *           
*********************************************************************           
LPMCHK   DS    0H                                                               
         XC    UPLPMSD(4),UPLPMSD  CLEAR THE LPM DATES                          
         MVI   LPMHSP,C'N'                                                      
         CLI   DBBTYPE,C'B'            LEAVE BLACK SURVEY ALONE                 
         BE    LPMCHKX                                                          
*                                                                               
         CLI   MMUPASS,2                                                        
         BNE   LPMCHK2                                                          
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
         B     LPMCHKX                                                          
*                                                                               
* CALL DEDEMTABS TO GET LPM START-END TABLE                                     
*                                                                               
LPMCHK2  GOTO1 VDEMTABS,DMCB,LPMSTEND  GET A(LPM DUAL DATA DATES)               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
         USING LPMSETD,RE                                                       
LPMCHK3  CLI   0(RE),X'FF'         EOT?                                         
         BE    LPMCKSET                                                         
         CLC   DBBTYPE,LPMSEBKT    SAME BOOKTYPE?                               
         BNE   LPMCHK5                                                          
         CLC   SAVRMKT,LPMSETMK    SAME MARKET                                  
         BNE   LPMCHK5                                                          
         MVC   UPLPMSD(4),LPMSTART                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   LPMHSP,C'Y'                                                      
         B     LPMCKSET                                                         
LPMCHK5  AR    RE,R0                                                            
         B     LPMCHK3                                                          
         DROP  RE                                                               
************************************************************                    
*&&DO                                                                           
LPMCHK2  CLI   DBBTYPE,C'H'              CONTROL HISPANIC SEPARATELY            
         BE    LPMCKHSP                                                         
         CLI   DBBTYPE,C'I'                                                     
         BE    LPMCKHSP                                                         
*                                                                               
         CLC   SAVRMKT,=H'101'           NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   SAVRMKT,=H'403'           LA                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68056807'                                          
         CLC   SAVRMKT,=H'202'           CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         CLC   SAVRMKT,=H'407'          SF                                      
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'680A680C'                                          
         CLC   SAVRMKT,=H'111'          WAS                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
         CLC   SAVRMKT,=H'104'          PHL                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'69056906'                                          
*                                                                               
         CLC   SAVRMKT,=H'105'          DET                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   SAVRMKT,=H'223'          DALLAS FT WORTH                         
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'690B690B'                                          
         CLC   SAVRMKT,=H'168'          ATLANTA                                 
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'6A056A05'                                          
*                                                                               
         B     LPMCKSET                                                         
*                                                                               
*                                                                               
LPMCKHSP CLC   SAVRMKT,=H'101'           NY                                     
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68046808'                                          
         CLC   SAVRMKT,=H'202'           CHI                                    
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'68076809'                                          
         CLC   SAVRMKT,=H'407'          SF                                      
         BNE   *+10                                                             
         MVC   UPLPMSD(4),=X'680A680C'                                          
         OC    UPLPMSD,UPLPMSD                                                  
         BZ    LPMCKSET                                                         
         MVI   LPMHSP,C'Y'                                                      
****     B     LPMCKSET                                                         
*&&                                                                             
***************************************************************                 
LPMCKSET OC    UPLPMSD,UPLPMSD                                                  
         BZ    LPMCHKX                                                          
         CLI   DBBTYPE,C'I'                                                     
         JE    LPMHPT1                                                          
         CLI   DBBTYPE,C'P'        KILL PPM PRIOR                               
         JNE   LPMHPT2                                                          
                                                                                
LPMHPT1  CLC   DBSELBK,UPLPMSD     COMPARE TO LPM START                         
         JNL   LPMHPT2              IF LESS THE USE REGULAR                     
         MVI   DBBTYPE,0           DEFAULT DIARY                                
         CLI   LPMHSP,C'Y'                                                      
         JNE   LPMCHKX                                                          
         MVI   DBBTYPE,C'H'        DEFAULT HISPANIC                             
         J     LPMCHKX                                                          
*                                                                               
LPMHPT2  DS    0C                                                               
                                                                                
LPMHPT4  DS    0C                                                               
         CLC   DBSELBK,UPLPMED      IF IT'S WITHIN CURRENCY PERIOD              
         JH    LPMCHKX                                                          
         CLC   DBSELBK,UPLPMSD                                                  
         JL    LPMCHKX                                                          
         MVI   DBBTYPE,C'P'         USE THE LPM HPT DATA                        
         CLI   LPMHSP,C'Y'                                                      
         JNE   LPMCHKX                                                          
         MVI   DBBTYPE,C'I'                                                     
****     J     LPMCHKX                                                          
LPMCHKX  XIT1                                                                   
                                                                                
******************************************************************              
* LPM SHARE INDEX ROUTINE WHICH                                                 
* PRETTY MUCH DOES AN MMU UPGRADE WITH A FORCED STATION MARKET                  
* INDEX = LPM BOOK / DIARY BOOK                                                 
******************************************************************              
LPMSINDX DS    0H                                                               
         ICM   RE,15,AUPGDEL                                                    
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,X'09'                                                   
         BNE   LPMSID04                                                         
         DROP  RE                                                               
*                                                                               
***********************************************************                     
*   THIS BLOCK ONLY USED FOR LPM SYNTAX UPGRADE                                 
*   ELSE WE MUST BE USING OLD PUT SYNTAX BRANCH AROUND                          
*                                                                               
*                                                                               
*   CALCULATE NEW RATINGS                                  *                    
*   NOTE PARAM 1 ROUNDED BY CALC SO OLDSHR MUST BE PARAM 2 *                    
         SPACE 1                                                                
****     GOTO1 VSUBR01,P1,('PUTSHRQ',(RC))                                      
         GOTO1 VSUBR01,P1,('CALCQ',(RC)),NEWHPT,OLDSHR,ADJRTG                   
*        GOTO1 CALC,P1,NEWHPT,OLDSHR,ADJRTG                                     
         MVC   NEWRTG(NEWRTGLN),ADJRTG                                          
         XC    INDEX,INDEX                                                      
         MVI   PUTUPGD,C'Y'                                                     
*                                                                               
* AFTER PUT UPGRADE ADD NEW CODE TO CALCULATE PINDEX AND SINDEX                 
* FROM WNBC AND NY FOR NOW WHERE INDEX =LPM/DIARY                               
*                                                                               
***********************************************************                     
LPMSID04 DS    0C                                                               
                                                                                
         OC    OLDRTG(OLDRTGLN),OLDRTG                                          
         JNZ   *+10                                                             
         MVC   OLDRTG(OLDRTGLN),NEWRTG                                          
                                                                                
         ICM   RE,15,AUPGDEL      IF USING OLD PUT FORMULA                      
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,X'09'                                                   
         BNE   LPMSHARE           SKIP VALIDATING MKTS FOR NOW                  
         DROP  RE                                                               
*                                                                               
* CALL DEMAND TO GET MARKET NUMBER FOR STATION                                  
*                                                                               
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVC   DBTAPEP,TAPEOPT                                                  
         L     RE,ATPTVALS                                                      
         MVC   DBFILE(L'DBFILE),1(RE)                                           
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
*                                  VALIDATE STATION TO GET MARKET               
****     MVI   DBFUNCT,DBVLST                                                   
         MVI   DBFUNCT,DBVLSTBK                                                 
         ICM   RE,15,AUPGDEL                                                    
         USING RAVLNEL,RE                                                       
         MVC   DBSELSTA(4),RAVLNOP2 RAVLNOP2,3 HAS STATION IN IT                
         MVC   DBSELBK,RAVLNOP1                                                 
         DROP  RE                                                               
         MVI   DBSELSTA+4,C'T'                                                  
*                                  CALL DEMAND TO VALIDATE STATION/BOOK         
         MVC   SVBKTYPE,DBBTYPE                                                 
         GOTO1 VSUBR02,DMCB,('FORCELPQ',(RC))                                   
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         MVC   DBBTYPE,SVBKTYPE                                                 
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   LPMSIDX             IF ERROR JUST EXIT                           
***      MVC   DBSELRMK,DBKEY+(SBRMKT-SBKEY)                                    
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
LPMSID06 CLI   0(RE),X'FF'              IN TABLE                                
         BE    LPMSIDX                  IF NOT IN LPM TABLE EXIT                
***      CLC   0(2,RE),DBKEY+(SBRMKT-SBKEY)                                     
         CLC   0(2,RE),DBKEY+(BSRMKT-BSKEY)                                     
         BE    LPMSID08                                                         
         AR    RE,R0                                                            
         B     LPMSID06                                                         
*                                                                               
LPMSID08 MVC   LPMSBOOK,2(RE)          SET SHARE INDEX BOOK FROM TABLE          
**       MVC   LPMSRMKT,DBKEY+(SBRMKT-SBKEY)   SHARE INDEX HOME MARKET          
         MVC   LPMSRMKT,DBKEY+(BSRMKT-BSKEY)   SHARE INDEX HOME MARKET          
*                                                                               
         LR    RE,RF                   CHECK PUT MKT IS ALSO IN TABLE           
         L     RF,AUPGDEL                                                       
         USING RAVLNEL,RF                                                       
         OC    RAVLNOP4,RAVLNOP4       PUT INDEX MARKET IS OPTIONAL             
         BNZ   LPMSID09                IF NOT GIVEN THEN DO PUT INDEX           
         MVC   LPMPBOOK,LPMSBOOK       OFF SHARE INDEX BOOK                     
         MVC   LPMPRMKT,LPMSRMKT       SET PUT INDEX MKT = SHARE INDEX          
         B     LPMSHARE                                                         
*                                      MKT                                      
LPMSID09 CLI   0(RE),X'FF'                                                      
         BE    LPMSIDX                 IF NOT IN LPM TABLE EXIT                 
         CLC   0(2,RE),RAVLNOP4                                                 
         BE    LPMSID10                                                         
         AR    RE,R0                                                            
         B     LPMSID09                                                         
LPMSID10 MVC   LPMPBOOK,2(RE)          SET PUT INDEX BOOK FROM TABLE            
         MVC   LPMPRMKT,RAVLNOP4       SET PUT INDEX MARKET                     
*                                                                               
**LPMSID11 MVC   DBSELRMK,DBKEY+(SBRMKT-SBKEY)   SHARE INDEX HOME MKT           
         DROP  RF                                                               
************************************************                                
*  NOW CALCULATE  SHARE INDEX AND ADJUST SHARES*                                
************************************************                                
                                                                                
LPMSHARE MVI   BOOKI,0             SET UP FOR                                   
         MVI   Y4SSW,C'S'                                                       
         MVI   MMUPASS,1                                                        
         MVC   MMUBOOK,LPMSBOOK                                                 
         MVC   DBSELBK,MMUBOOK                                                  
         MVC   SAVRMKT,LPMSRMKT                                                 
         L     RE,AUPGDEL                                                       
         USING RAVLNEL,RE                                                       
         MVC   SVPBOOK,RAVLNOP1                                                 
         MVC   RAVLNOP1,MMUBOOK                                                 
         LA    R1,RAVLNOP1                                                      
         DROP  RE                                                               
                                                                                
         L     RF,ARECORD                                                       
         USING RINVD,RF                                                         
         ZICM  RE,AINVFREL,(15)     FORCE TO WABC FOR NOW                       
RFR      USING RINVFREL,RE                                                      
         BZ    *+20                                                             
         MVC   SAVSTA,RFR.RINVFRST  SAVE OFF THE STATION IN CASE                
         MVC   RFR.RINVFRST,DBSELSTA                                            
         B     *+16                                                             
         MVC   SAVSTA,RINVKSTA     SAVE OFF THE STATION IN CASE                 
         MVC   RINVKSTA,DBSELSTA   DBSELSTA SHOULD HAVE UPGRADE STATION         
         DROP  RFR,RF                                                           
                                                                                
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
                                                                                
************************************************                                
* CALCULATE THE PINDEX AND ADJUST PUTS         *                                
************************************************                                
LPMPUT   DS    0C                                                               
         MVI   PUTUPGD,C'Y'                                                     
                                                                                
LPMPUT08 MVI   Y4SSW,C'P'                                                       
         MVI   Y4ASW,C'N'                                                       
         MVC   MMUBOOK,LPMPBOOK                                                 
         MVI   BOOKI,0             SET UP FOR                                   
                                                                                
         MVI   MMUPASS,1                                                        
         MVC   DBSELBK,MMUBOOK                                                  
                                                                                
         ICM   RE,15,AUPGDEL                                                    
         USING RAVLNEL,RE                                                       
         MVC   RAVLNOP1,MMUBOOK   THIS MUST BE THE BAD STATMENT                 
         LA    R1,RAVLNOP1                                                      
         SR    RF,RF               YES - SET STATION TO NNNNT                   
         ICM   RF,3,LPMPRMKT                                                    
         CVD   RF,DUB                                                           
         UNPK  DBSELSTA(4),DUB                                                  
         OI    DBSELSTA+3,X'F0'                                                 
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   SAVRMKT,LPMPRMKT    PUT INDEX MARKET                             
         DROP  RE                                                               
                                                                                
         L     RF,ARECORD                                                       
         USING RINVD,RF                                                         
         ZICM  RE,AINVFREL,(15)     DBSELSTA SHOULD BE EQUAL TO                 
RFR      USING RINVFREL,RE          MKT#  IN UPGRADE FORMULA                    
         BZ    *+14                                                             
         MVC   RFR.RINVFRST,DBSELSTA                                            
         B     *+10                                                             
         MVC   RINVKSTA,DBSELSTA                                                
         DROP  RFR,RF                                                           
*                                                                               
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
*****************************************************************               
* BEFORE WE EXIT- RESTORE THE INVENTORY ELEMENT OR KEY FIELD                    
* WE MUCKED AROUND WITH ABOVE IN CASE IT NEEDS TO BE IN SYNC WHEN               
* WE GET BACK TO WHERE EVER WE CAME FROM                                        
* ***************************************************************               
         L     RF,ARECORD                                                       
         USING RINVD,RF                                                         
         ZICM  RE,AINVFREL,(15)                                                 
RFR      USING RINVFREL,RE                                                      
         BZ    *+14                                                             
         MVC   RFR.RINVFRST,SAVSTA                                              
         B     *+10                                                             
         MVC   RINVKSTA,SAVSTA                                                  
         DROP  RFR,RF                                                           
         L     RE,AUPGDEL                 RESTORE                               
         USING RAVLNEL,RE                                                       
         MVC   RAVLNOP1,SVPBOOK                                                 
         DROP  RE                                                               
                                                                                
LPMSIDX  CLI   PUTSHRD,C'Y'                                                     
         JNE   LPMSIDXX                                                         
         ICM   RE,15,AUPGDEL                                                    
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,X'09'                                                   
         BNE   LPMSIDXX                                                         
         DROP  RE                                                               
         MVC   OLDRTG(NEWRTGLN),NEWRTG                                          
         MVC   SAVRTG(NEWRTGLN),NEWRTG                                          
         MVC   OLDHPT(NEWHPTLN),NEWHPT                                          
************************************************                                
LPMSIDXX XIT1                                                                   
*                                                                               
*                                                                               
******************************************************************              
* LPS - LPS PUT INDEX UPGRADE                                                   
******************************************************************              
LPSPINDX DS    0H                                                               
* CALL DEMAND TO GET MARKET NUMBER FOR STATION                                  
*                                                                               
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVC   DBTAPEP,TAPEOPT                                                  
         L     RE,ATPTVALS                                                      
         MVC   DBFILE(L'DBFILE),1(RE)                                           
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
***      STCM  R9,15,DBCOMFCS                                                   
         MVC   DBCOMFCS,VCOMFACS                                                
         ICM   RE,15,AUPGDEL                                                    
         USING RAVLNEL,RE                                                       
         MVC   DBSELBK,RAVLNOP1                                                 
         DROP  RE                                                               
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
*                                  VALIDATE STATION TO GET MARKET               
**       MVI   DBFUNCT,DBVLST                                                   
         MVI   DBFUNCT,DBVLSTBK                                                 
         L     R2,ARECORD                                                       
         CLI   FILETYPE,C'I'       TEST FOR INVENTORY FILE                      
         JNE   LPSPDX4                                                          
*                                  EXTRACT STATION/BOOK/SOURCE FROM             
*                                  INVENTORY RECORD KEY/ELEMENTS                
         USING RINVD,R2                                                         
         ICM   R3,15,AINVFREL      TEST IF FROM BOOK ELEMENT FOUND              
         JZ    LPSPDX2                                                          
RFR      USING RINVFREL,R3                                                      
         MVC   DBSELSTA,RFR.RINVFRST  YES - EXTRACT VALUES FROM ELEMENT         
         J     LPSPDX6                                                          
         DROP  RFR                                                              
*                                                                               
LPSPDX2  MVC   DBSELSTA,RINVKSTA   EXTRACT STATION/SRC/BOOK FROM KEY            
         J     LPSPDX6             FIRST                                        
         DROP  R2                                                               
*                                  EXTRACT VALUES FROM PAV DEMO KEY             
         USING PRKEY,R2                                                         
LPSPDX4  MVC   DBSELSTA,PRSTAT                                                  
         DROP  R2                                                               
LPSPDX6  DS    0C                                                               
*                                                                               
*                                  CALL DEMAND TO VALIDATE STATION/BOOK         
         MVC   SVBKTYPE,DBBTYPE                                                 
         GOTO1 VSUBR02,DMCB,('FORCELPQ',(RC))                                   
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         MVC   DBBTYPE,SVBKTYPE                                                 
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   LPSPIDXX            IF ERROR JUST EXIT                           
***      MVC   SAVRMKT,DBKEY+(SBRMKT-SBKEY)                                     
         MVC   SAVRMKT,DBKEY+(BSRMKT-BSKEY)                                     
         MVI   PUTUPGD,C'Y'                                                     
                                                                                
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4ASW,C'N'                                                       
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
LPSPIDXX XIT1                                                                   
*                                                                               
GETSHR   DS    0C                                                               
         SR    RE,RE               RE=INDEX REGISTER                            
         LA    RF,OLDRTGLN/4       RF=NUMBER OF SHARE VALUES                    
GETSHR2  L     R0,OLDRTG(RE)       (RATING*100+(PUT/2))/PUT=SHARE               
         MH    R0,=H'1000'                                                      
         L     R1,OLDHPT(RE)                                                    
         LTR   R1,R1               TEST FOR ZERO PUT                            
         JZ    GETSHR4                                                          
         SRL   R1,1                                                             
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         D     R0,OLDHPT(RE)                                                    
GETSHR4  ST    R1,OLDSHR(RE)       SET SHARE & BUMP TO NEXT                     
         LA    RE,4(RE)                                                         
         BCT   RF,GETSHR2                                                       
*                                  RESTORE HOMES SHARE VALUES (BOOK)            
         MVC   OLDSHR+UORHOMES-OLDRTG(HOMSHRLN),HOMSHR                          
*                                  NOW ROUND THE SHARES YET AGAIN               
         XIT1                                                                   
*&&DO                                                                           
* MMU UPGRADE CODE                                                              
UPMMU    DS    0C                                                               
*                                                                               
         L     R3,AUPGDEL                                                       
         USING RAVLNEL,R3                                                       
*        BAS   RE,GETYRS                                                        
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4ASW,C'N'                                                       
         MVC   MMUBOOK,RAVLNOP2                                                 
         MVC   SAVRTG(OLDRTGLN),OLDRTG                                          
         MVI   MMUPASS,1                                                        
         MVC   DBSELBK,MMUBOOK                                                  
         GOTO1 VSUBR01,DMCB,('LPMCHKQ',(RC))                                    
*                                                                               
         CLI   DBBTYPE,C'P' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         CLI   DBBTYPE,C'I' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         MVI   MMUPASS,0                                                        
*                                                                               
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         XIT1                                                                   
*&&                                                                             
*  SET OVERNIGHTS ROLLING AVERGAE PUT BOOKTYPE                                  
SETOVBKT DS    0C                                                               
         GOTO1 VDEMTABS,DMCB,ROLLPBKT  GET A(BOOKTYPE TABLE)                    
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
*                                                                               
*                                                                               
SETOVB10 CLI   0(RE),X'FF'              IN TABLE                                
         BE    SETOVBX                                                          
         CLC   DBSELBK,0(RE)                                                    
         BL    SETOVB20                                                         
         CLC   DBBTYPE,2(RE)                                                    
         BNE   SETOVB20                                                         
         MVC   DBBTYPE,3(RE)                                                    
         B     SETOVBX                                                          
SETOVB20 AR    RE,R0                                                            
         B     SETOVB10                                                         
                                                                                
SETOVBX  XIT1                                                                   
*                                                                               
GETHMKT  DS    0H                                                               
         L     R3,AUPGDEL                                                       
RAVL     USING RAVLNEL,R3                                                       
         XC    DBLOCK,DBLOCK       BUILD DBLOCK FOR NEW H/P/T LOOKUP            
         XC    DUB,DUB             CLEAR BOOK VALUES AREA                       
         MVC   DBTAPEP,TAPEOPT                                                  
         L     RE,ATPTVALS                                                      
         MVC   DBFILE(L'DBFILE),1(RE)                                           
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVC   DBSELBK,RAVL.RAVLNOP1                                            
         MVC   DBCOMFCS,VCOMFACS                                                
         MVI   DBSELMED,C'T'       DEFAULT THE MEDIA TO USTV                    
*                                  VALIDATE STATION TO GET MARKET               
         MVI   DBFUNCT,DBVLSTBK                                                 
         DROP  RAVL                                                             
*                                                                               
         L     R2,ARECORD                                                       
*                                  EXTRACT STATION/BOOK/SOURCE FROM             
*                                  INVENTORY RECORD KEY/ELEMENTS                
RKEY     USING RINVD,R2                                                         
         ICM   R3,15,AINVFREL      TEST IF FROM BOOK ELEMENT FOUND              
         JZ    GETHMK2                                                          
RFR      USING RINVFREL,R3                                                      
         MVC   DBSELSTA,RFR.RINVFRST  YES - EXTRACT VALUES FROM ELEMENT         
         J     GETHMK6                                                          
*                                                                               
GETHMK2  MVC   DBSELSTA,RKEY.RINVKSTA EXTRACT STATION/SRC/BOOK FROM KEY         
         J     GETHMK6             FIRST                                        
         DROP  RKEY,RFR                                                         
*                                  EXTRACT VALUES FROM PAV DEMO KEY             
         USING PRKEY,R2                                                         
GETHMK4  MVC   DBSELSTA,PRSTAT                                                  
         DROP  R2                                                               
GETHMK6  DS    0C                                                               
*                                                                               
*                                  CALL DEMAND TO VALIDATE STATION/BOOK         
         MVC   SVBKTYPE,DBBTYPE                                                 
         GOTO1 VSUBR02,DMCB,('FORCELPQ',(RC))                                   
         GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
         MVC   DBBTYPE,SVBKTYPE                                                 
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   GETHMKTX            IF ERROR JUST EXIT                           
***      MVC   SAVRMKT,DBKEY+(SBRMKT-SBKEY)                                     
         MVC   SAVRMKT,DBKEY+(BSRMKT-BSKEY)                                     
GETHMKTX XIT1                                                                   
                                                                                
******************************************************************              
* NEW ORLEANS                                                                   
STATNOL  DC    C'WDSU'                                                          
         DC    C'WGNO'                                                          
         DC    C'WHNO'                                                          
         DC    C'WLAE'                                                          
         DC    C'WNOL'                                                          
         DC    C'WUPL'                                                          
         DC    C'WVUE'                                                          
         DC    C'WWL '                                                          
         DC    C'WYES'                                                          
         DC    X'FF'                                                            
*                                                                               
* MEMPHIS                                                                       
STATMEM  DC    C'WHBQ'                                                          
         DC    C'WKNO'                                                          
         DC    C'WLMT'                                                          
         DC    C'WMC '                                                          
         DC    C'WPTY'                                                          
         DC    C'WREG'                                                          
         DC    C'WBUY'                                                          
         DC    X'FF'                                                            
* NASHVILLE                                                                     
STATNASH DC    C'WDCN'                                                          
         DC    C'WKRN'                                                          
         DC    C'WNAB'                                                          
         DC    C'WPGD'                                                          
         DC    C'WSMV'                                                          
         DC    C'WTVF'                                                          
         DC    C'WUXP'                                                          
         DC    C'WZTV'                                                          
         DC    X'FF'                                                            
* JACKSONVILLE JUL/98                                                           
STATJAK  DC    C'WAWS'                                                          
         DC    C'WJCT'                                                          
         DC    C'WJWB'                                                          
         DC    C'WJXT'                                                          
         DC    C'WJXX'                                                          
         DC    C'WTEV'                                                          
         DC    C'WTLV'                                                          
         DC    X'FF'                                                            
*                                                                               
* BIRMINGHAM NOV/98                                                             
STATBIR  DC    C'WABM'                                                          
         DC    C'WBIQ'                                                          
         DC    C'WBMA'                                                          
         DC    C'WBRC'                                                          
         DC    C'WIAT'                                                          
         DC    C'WJRD'                                                          
         DC    C'WPXH'                                                          
         DC    C'WTTO'                                                          
         DC    C'WVTM'                                                          
*                                                                               
* LAS VEGAS NOV/98                                                              
STATLV   DC    C'KBLR'                                                          
         DC    C'KFBT'                                                          
         DC    C'KINC'                                                          
         DC    C'KLAS'                                                          
         DC    C'KLVX'                                                          
         DC    C'KTNV'                                                          
         DC    C'KVBC'                                                          
         DC    C'KVVU'                                                          
         DC    C'KVWB'                                                          
*                                                                               
* PROVIDENCE NOV/98                                                             
STATPRO  DC    C'WJAR'                                                          
         DC    C'WLNE'                                                          
         DC    C'WLWC'                                                          
         DC    C'WNAC'                                                          
         DC    C'WPRI'                                                          
         DC    C'WPXQ'                                                          
         DC    C'WSBE'                                                          
*                                                                               
* RALEIGH NOV/98                                                                
STATRAL  DC    C'WFPX'                                                          
         DC    C'WKFT'                                                          
         DC    C'WLFL'                                                          
         DC    C'WNCN'                                                          
         DC    C'WRAL'                                                          
         DC    C'WRAZ'                                                          
         DC    C'WRDC'                                                          
         DC    C'WRPX'                                                          
         DC    C'WTVD'                                                          
         DC   X'FF'                                                             
                                                                                
                                                                                
DEMOSHR  DS    0XL3                                                             
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
         DROP  RA                                                               
         EJECT                                                                  
         LTORG                                                                  
*********************************************************************           
* SUBR02 - SUBROUTINE BLOCK 01                                                  
*********************************************************************           
SUBR02   NMOD1 0,**SR02**,RA,RR=RE                                              
         L     RC,0(R1)                                                         
         USING DEMUPD,RC                                                        
         ST    R1,PARM02                                                        
         ST    RE,RELO3            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
CHKLIVEQ EQU   (CHKLIVE#-*)/4+1                                                 
NONZEROQ EQU   (NONZERO#-*)/4+1                                                 
CHKLIVMQ EQU   (CHKLIVM#-*)/4+1                                                 
FORCELPQ EQU   (FORCELP#-*)/4+1                                                 
UPMMUQ   EQU   (UPMMU#-*)/4+1                                                   
CHKNHUTQ EQU   (CHKNHUT#-*)/4+1                                                 
CHKLEXPQ EQU   (CHKLEXP#-*)/4+1                                                 
         SPACE 1                                                                
CHKLIVE# J     CHKLIVEZ            CHK IF ANY BOOKS -LIVE ZERO CELL             
NONZERO# J     NONZEROC            IF NO ZEROCELL FOUND- SET BKTYPES            
CHKLIVM# J     CHKLIVEM            CHK LIVE ONLY BOOK FOR BOOKTYPE              
FORCELP# J     FORCELVP            CHK LIVE ONLY BOOK FOR BOOKTYPE              
UPMMU#   J     UPMMU               MMU CODE                                     
CHKNHUT# J     CHKNHUT             CHK IF ANY BOOKS -NEW HUT BOOKS              
CHKLEXP# J     CHKLEXPB            EXPANSION BOOK                               
         EJECT                                                                  
*                                                                               
*******************************************************************             
* ROUTINE TO CHECK IF ANY OF THE UPGRADE BOOKS IS LIVE ZERO CELL                
* JUL06 OR LATER                                                                
* IF ONE OF THE  BOOKS IS LIVE ZERO CELL THEN ALL THE OTHER BOOKS               
* PRIOR TO JUL06 WILL READ ZERO CELL BOOKTYPE (1-4)                             
********************************************************************            
CHKLIVEZ DS    0C                                                               
         L     R3,AUPGDEL                                                       
RAVL     USING RAVLNEL,R3                                                       
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
* LOOK AT ALL THE BOOKS PARAMETER                                               
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLIVEX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKLIVEX                                                         
         CLC   RAVL.RAVLNOP1,=X'6A07'   ARE ANY BOOKS JUL06 OR LATER?           
         BL    *+14                 HAS TO BE VALID BETWEEN JUL06               
         CLC   RAVL.RAVLNOP1,=X'9999'   AND 9999                                
         BL    CHKLIVEY                                                         
         CLC   RAVL.RAVLNOP2,=X'6A07'   ARE ANY BOOKS JUL06 OR LATER?           
         BL    *+14                 HAS TO BE VALID BETWEEN JUL06               
         CLC   RAVL.RAVLNOP2,=X'9999'   AND 9999                                
         BL    CHKLIVEY                                                         
         CLC   RAVL.RAVLNOP3,=X'6A07'   ARE ANY BOOKS JUL06 OR LATER?           
         BL    *+14                 HAS TO BE VALID BETWEEN JUL06               
         CLC   RAVL.RAVLNOP3,=X'9999'   AND 9999                                
         BL    CHKLIVEY                                                         
         CLC   RAVL.RAVLNOP4,=X'6A07'   ARE ANY BOOKS JUL06 OR LATER?           
         BL    *+14                 HAS TO BE VALID BETWEEN JUL06               
         CLC   RAVL.RAVLNOP4,=X'9999'   AND 9999                                
         BL    CHKLIVEY                                                         
         DROP  RAVL                                                             
                                                                                
         CLC   DBSELBK,=X'690A'                                                 
         BL    CHKLIVEX                                                         
         CLC   DBSELBK,=X'6A05'                                                 
         BH    CHKLIVEX                                                         
* CHECK IF SHARE BOOK INFO IS PASSED INTO RINVZEL                               
         ICM   RE,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         USING RINVZEL,RE                                                       
         MVC   SHRBK,RINVZBK+1                                                  
         DROP  RE                                                               
         OC    SHRBK,SHRBK         IF SHARE BOOK PASSED IN CHECK                
         BZ    CHKLIVEY            AGAINST IT                                   
         CLC   SHRBK,=X'6A07'      IF SHARE BOOK IS LIVE ZEROCELL               
         BL    CHKLIVEX            THEN ALWAYS READ ZEROCELL BOOK               
* WITHIN THE PARALLEL PERIOD       FIRST.                                       
         CLI   DBBTYPE,0                                                        
         BNE   *+8                                                              
         MVI   DBBTYPE,C'1'                                                     
         CLI   DBBTYPE,C'H'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'2'                                                     
         B     CHKLIVEX                                                         
*                                                                               
* IF ANY OF THE BOOKS USED IS JUL06 OR LATER THEN IF THE CURRENT BOOK           
* BEING LOOKED UP IS PRIOR TO JUL06 THEN SET THE BOOKTYPE TO SEARCH FOR         
* PARALLEL ZEROCELL BOOKTYPE                                                    
*                                                                               
CHKLIVEY CLC   DBSELBK,=X'6A07'                                                 
         BNL   CHKLIVEX                                                         
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
*                                                                               
*******************************************************************             
* EXPANSION DATA FROM NOV14-NOV15 PUTS AND SHARE BOOK  JAN16 ON                 
********************************************************************            
CHKLEXPB DS    0C                                                               
         L     R3,AUPGDEL                                                       
RAVL     USING RAVLNEL,R3                                                       
         MVI   NEXPFLAG,C'N'                                                    
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
* LOOK AT ALL THE BOOKS PARAMETER                                               
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLEXPX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKLEXPX                                                         
         CLC   RAVL.RAVLNOP1,=AL2(NOV_14)                                       
         BL    *+14                                                             
         CLC   RAVL.RAVLNOP1,=AL2(NOV_15)                                       
         BNH   CHKLEXPY                                                         
         CLC   RAVL.RAVLNOP2,=AL2(NOV_14)                                       
         BL    *+14                                                             
         CLC   RAVL.RAVLNOP2,=AL2(NOV_15)                                       
         BNH   CHKLEXPY                                                         
         CLC   RAVL.RAVLNOP3,=AL2(NOV_14)                                       
         BL    *+14                                                             
         CLC   RAVL.RAVLNOP3,=AL2(NOV_15)                                       
         BNH   CHKLEXPY                                                         
         CLC   RAVL.RAVLNOP4,=AL2(NOV_14)                                       
         BL    *+14                                                             
         CLC   RAVL.RAVLNOP4,=AL2(NOV_15)                                       
         BNH   CHKLEXPY                                                         
         DROP  RAVL                                                             
         B     CHKLEXPX                                                         
*&&DO                                                                           
**       CLC   DBSELBK,=X'690A'                                                 
**       BL    CHKLEXPX                                                         
**       CLC   DBSELBK,=X'6A05'                                                 
**       BH    CHKLEXPX                                                         
         CLC   DBSELBK,=AL2(JAN_16)                                             
         BL    CHKLEXPX                                                         
* CHECK IF SHARE BOOK INFO IS PASSED INTO RINVZEL                               
         ICM   RE,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         USING RINVZEL,RE                                                       
         MVC   SHRBK,RINVZBK+1                                                  
         DROP  RE                                                               
         OC    SHRBK,SHRBK         IF SHARE BOOK PASSED IN CHECK                
         BZ    CHKLEXY2            AGAINST IT                                   
         CLC   SHRBK,=AL2(JAN_16)  IF SHARE BOOK IS LIVE ZEROCELL               
         BL    CHKLEXPX            THEN ALWAYS READ ZEROCELL BOOK               
         B     CHKEXY2                                                          
*&&                                                                             
*                                                                               
* IF ANY OF THE BOOKS USED IS JUL06 OR LATER THEN IF THE CURRENT BOOK           
* BEING LOOKED UP IS PRIOR TO JUL06 THEN SET THE BOOKTYPE TO SEARCH FOR         
* PARALLEL ZEROCELL BOOKTYPE                                                    
*                                                                               
CHKLEXPY CLC   DBSELBK,=AL2(JAN_16)                                             
         BNL   CHKLEXPX                                                         
         MVI   NEXPFLAG,C'Y'                                                    
*                                                                               
CHKLEXY2 CLI   DBBTYPE,C'L'                                                     
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
                                                                                
         CLI   DBBTYPE,C'U'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YU                                              
         CLI   DBBTYPE,BOOKTYPE_C3                                              
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YD                                              
         CLI   DBBTYPE,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_YC                                              
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
CHKLEXPX XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
NONZEROC DS    0H                                                               
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
NOZEROX  XIT1                                                                   
*                                                                               
***************************************************************                 
* ROUTINE TO CHECK LIVE ONLY BOOKTYPES AND SEE WHICH BOOKTYPES                  
* WE SHOULD READ FOR.                                                           
***************************************************************                 
CHKLIVEM DS    0C                                                               
         MVI   CHKLFLAG,0                                                       
         B     CHKLVM00                                                         
FORCELVP DS    0C                                                               
         MVI   CHKLFLAG,X'01'                                                   
*                                                                               
CHKLVM00 CLI   DBSELMED,C'T'                                                    
         BNE   CHKLIVMX                                                         
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKLIVMX                                                         
***      CLC   DBSELBK,=X'6A0B'      NOV06 AND AFTER                            
****     BNL   CHKLIVMX                                                         
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
CHKLVM60 DS    0C                                                               
         CLI   CHKLFLAG,X'01'        FORCE LIVE+ BOOKTYPE ?                     
         BE    CHKLVM70                                                         
         CLC   DBSELBK,LEFFBOOK      IF BOOK IS EFF BOOK OR LATER               
         BNL   CHKLIVMX              THEN KEEP BOOKTYPE                         
*                                    ELSE                                       
CHKLVM70 DS    0C                                                               
         MVC   DBBTYPE,LVPLUSBT      USE LIVE PLUS BOOKTYPE                     
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
*                                                                               
*                                                                               
* MMU UPGRADE CODE                                                              
UPMMU    DS    0C                                                               
*                                                                               
         L     R3,AUPGDEL                                                       
         USING RAVLNEL,R3                                                       
*        BAS   RE,GETYRS                                                        
         MVI   Y4SSW,C'P'                                                       
         MVI   Y4ASW,C'N'                                                       
         MVC   MMUBOOK,RAVLNOP2                                                 
         MVC   SAVRTG(OLDRTGLN),OLDRTG                                          
         MVI   MMUPASS,1                                                        
         MVC   DBSELBK,MMUBOOK                                                  
         GOTO1 VSUBR01,DMCB,('LPMCHKQ',(RC))                                    
*                                                                               
         CLI   DBBTYPE,C'P' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         CLI   DBBTYPE,C'I' RESET MMUPASS IF NO LPM                             
         BE    *+8                                                              
         MVI   MMUPASS,0                                                        
*                                                                               
         GOTO1 VSUBR01,DMCB,('GETYRSE',(RC))                                    
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
*******************************************************************             
* ROUTINE TO CHECK IF ANY OF THE UPGRADE BOOKS IS LIVE ZERO CELL                
* JUL06 OR LATER                                                                
* IF ONE OF THE  BOOKS IS LIVE ZERO CELL THEN ALL THE OTHER BOOKS               
* PRIOR TO JUL06 WILL READ ZERO CELL BOOKTYPE (1-4)                             
********************************************************************            
CHKNHUT  DS    0C                                                               
         L     R3,AUPGDEL                                                       
RAVH     USING RAVLNEL,R3                                                       
         MVI   NHUTFLAG,C'N'                                                    
         MVC   SVSELDAY,DBSELDAY                                                
         MVC   SVSELTIM,DBSELTIM                                                
* LOOK AT ALL THE BOOKS PARAMETER                                               
         CLI   DBSELSRC,C'N'                                                    
         BNE   CHKNHUTX                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   CHKNHUTX                                                         
* ANY BOOKS EQUAL TO OR LATER THEN JAN/14                                       
         CLC   RAVH.RAVLNOP1,=AL2(JAN_14)                                       
         BL    *+14                 HAS TO BE VALID BETWEEN JAN14               
         CLC   RAVH.RAVLNOP1,=X'9999'   AND 9999                                
         BL    CHKNHUTY                                                         
         CLC   RAVH.RAVLNOP2,=AL2(JAN_14)                                       
         BL    *+14                 HAS TO BE VALID BETWEEN JAN14               
         CLC   RAVH.RAVLNOP2,=X'9999'   AND 9999                                
         BL    CHKNHUTY                                                         
         CLC   RAVH.RAVLNOP3,=AL2(JAN_14)                                       
         BL    *+14                 HAS TO BE VALID BETWEEN JAN14               
         CLC   RAVH.RAVLNOP3,=X'9999'   AND 9999                                
         BL    CHKNHUTY                                                         
         CLC   RAVH.RAVLNOP4,=AL2(JAN_14)                                       
         BL    *+14                 HAS TO BE VALID BETWEEN JAN14               
         CLC   RAVH.RAVLNOP4,=X'9999'   AND 9999                                
         BL    CHKNHUTY                                                         
         DROP  RAVH                                                             
                                                                                
* CHECK IF SHARE BOOK INFO IS PASSED INTO RINVZEL                               
         ICM   RE,15,AINVZEL       TEST DAY/TIME ELEMENT FOUND                  
         BZ    CHKNHUTX                                                         
         USING RINVZEL,RE                                                       
         MVC   SHRBK,RINVZBK+1                                                  
         DROP  RE                                                               
         OC    SHRBK,SHRBK         IF LATEST BOOK CONSIDER AFTER 2014           
         BZ    CHKNHUTY                                                         
         CLC   SHRBK,=AL2(JAN_14)                                               
         BL    CHKNHUTX            HAS TO BE VALID BETWEEN JAN14                
         CLC   SHRBK,=X'9999'              AND 9999                             
         BL    CHKNHUTY                                                         
         B     CHKNHUTX                                                         
*                                                                               
* IF THE CURRENT BOOK USED IS BETWEEN JAN13 AND DEC13                           
* CONVERT THE BOOKTYPE TO THE NEW HUT BOOKTYPE                                  
*                                                                               
CHKNHUTY CLC   DBSELBK,=AL2(JAN_13)                                             
         BL    CHKNHUTX                                                         
         CLC   DBSELBK,=AL2(DEC_13)                                             
         BNH   CHKNHTY2                                                         
*                                                                               
CHKNHTY2 MVI   NHUTFLAG,C'Y'                                                    
         MVC   ZSVBKTYP,DBBTYPE    SAVE BOOKTYPE COMING IN                      
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
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
DEMUPD   DSECT                                                                  
ADURTAB  DS    F                                                                
ATPTVALS DS    F                                                                
VSUBR01  DS    F                                                                
VSUBR02  DS    F                                                                
VDEINMKT DS    F                                                                
RELO2    DS    F                                                                
RELO3    DS    F                                                                
MYBASE2  DS    F                                                                
PARM01   DS    F                                                                
PARM02   DS    F                                                                
SAVER7   DS    F                                                                
DATAMGR  DS    F                                                                
VDATAMGR DS    F                                                                
VCOMFACS DS    V                                                                
VDEMOUT  DS    V                                                                
VHELLO   DS    V                                                                
VDEMAINT DS    V                                                                
VCALLOV  DS    V                                                                
VDEMAND  DS    V                                                                
VDEMOMTH DS    V                                                                
VDEMTABS DS    V                                                                
*                                                                               
SAVEMORE DS    D                                                                
BYTE     DS    C                                                                
Y4ASW    DS    C                                                                
Y4MSW    DS    C                                                                
Y4SSW    DS    C                                                                
Y4FSW    DS    C                   UPGRADE FILE SOURCE                          
*                                   0 = DEMO FILES                              
*                                   I = INVENTORY                               
MMUBOOK  DS    CL2                 METERED MARKET UPG PUT START                 
LPMSBOOK DS    CL2                 SHARE INDEX BOOK FOR LPM UPGRADE             
LPMPBOOK DS    CL2                 PUT INDEX BOOK FOR LPM UPGRADE               
LPMSRMKT DS    XL2                 SHARE INDEX HOME MARKET                      
LPMPRMKT DS    XL2                 PUT INDEX HOME MARKET                        
YRSAVTYP DS    C                   TYPE OF AVG UPGRADE                          
HOOKFLAG DS    C                                                                
NHUTFLAG DS    C                                                                
NEXPFLAG DS    C                                                                
Y4SYRS   DS    C                                                                
Y4SYRS2  DS    C                                                                
BOOKI    DS    C                   BOOK INDEX                                   
BOOKLIST DS    CL10                4 BOOKS + 00 END                             
STNBK    DS    XL2                 BOOK PARM PASSED                             
SAVHUT   DS    H                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
P1       EQU   DMCB                                                             
P2       EQU   DMCB+4                                                           
P3       EQU   DMCB+8                                                           
P4       EQU   DMCB+12                                                          
DPWORK   DS    XL16                                                             
WORK     DS    XL64                                                             
INDEX    DS    F                   CALCULATION INDEX FACTOR                     
NETSHRV  DS    XL12                NETWORK HOMES SHARE VALUES                   
*                                  ADDRESS DIRECTORY                            
RELO     DS    A                   PROGRAM RELOCATION FACTOR                    
VGETIUN  DS    V                   V(GETIUN)                                    
AIO2     DS    A                   A(IO2)                                       
AIO3     DS    A                   A(IO3)                                       
AFRSTEL  DS    A                   A(FIRST ELEMENT)                             
AUPGDEL  DS    A                   A(UPGRADE ELEMENT)                           
ARECORD  DS    A                   A(INPUT RECORD)                              
ARECLEN  DS    A                   A(RECORD LENGTH)                             
AINVFREL DS    A                   A(XFER FROM ELEMENT)                         
AQTRHREL DS    A                   A(QTR HOUR ELEMENT)                          
AIMHEL   DS    A                   A(MULTI HUT ELEMENT)                         
ABOOKEL  DS    A                   A(BOOK ELEMENT)                              
AINVCEL  DS    A                   A(CODE ELEMENT)                              
AINVZEL  DS    A                   A(DAY/TIME ELEMENT)                          
ANETRTEL DS    A                   A(NETWORK PROGRAM RUN TIME ELEMENT)          
ASAVE    DS    A                   A(USE SUPPLIED SAVE AREA)                    
ASHARES  DS    A                   A(USER SUPPLIED SHARES)                      
ADBLOCK  DS    A                   A(USER DBLOCK)                               
ADISPTAB DS    A                   A(MASTER DISP. TABLE)                        
TAPEOPT  DS    X                   IMPR/RATING BASED CALCULATIONS               
RAVTMPZR DS    X                                                                
RAVTMPTY DS    CL2                 MODIFIED TYPE                                
FILEVALS DS    0CL12               INPUT FILE VALUES                            
FILETYPE DS    C                   INTERNAL FILE (I=INV,P=PAV)                  
FILEDEM  DS    CL3                 DBLOCK FILE NAME                             
FILENAME DS    CL8                 HELLO FILE NAME                              
DYTMELCD DS    X                   DAY/TIME LIST BUILD ELEMENT                  
PUTSHRD  DS    C                   C'Y'=THIS IS A PUT/SHARE UPGRADE             
PUTUPGD  DS    C                   C'Y'=THIS IS A PUT UPGRADE                   
HPTFLAG  DS    C                   O=GET OLD H/P/T, N=GET NEW H/P/T             
IUNFLAG  DS    C                   C'Y'=IUN FORMAT RECORD PASSED                
FORCESW  DS    C                   C'Y'=READ FOR SPECIAL SURVEY ONLY            
OVERLIST DS    XL21                LIST OF DEMO OVERRIDE VALUES                 
FROMTYPE DS    C                   C'I'=INV TO INV UPGRADE                      
INDEXFF  DS    C                   C'Y'=INDEX=FF CALL                           
INDEXTYP DS    C                   TYPE OF INDEX=FF CALL                        
INDEXFIL DS    C                   FILE TYPE                                    
INDEXUPG DS    C                   INDEX UPGRADE DONE                           
CHKLFLAG DS    X                                                                
HPTWT    DS    H                   OLD H/P/T WEIGHTING FOR INDEX=FF             
UPAGY    DS    CL2                 2 CHAR REP ID                                
UPTIMCHG DS    C                   TIME CHANGE INDICATOR                        
UPDB     DS    F                   APPLICATION DBLOCK                           
UPIMS    DS    F                   IN MARKET SHARE LIST OF AFFLIATES            
AAFFLST  DS    A                   ADDR OF AFFL LST FOR IMS                     
UPLPMSD  DS    XL2                 LPM PARALELL START FOR MARKET                
UPLPMED  DS    XL2                 LPM PARALELL END FOR MARKET                  
LPMHSP   DS    C                   INDICATE LPM HISPANIC                        
ROLLFLAG DS    C                   ROLLING AVG FLAG                             
SAVERE   DS    F                   SAVE REGISTER RE                             
SVSELDAY DS    CL(L'DBSELDAY)                                                   
SVSELTIM DS    CL(L'DBSELTIM)                                                   
ZCELLBTY DS    C                                                                
ZSVBKTYP DS    C                                                                
SVBKTYPE DS    C                                                                
SVLPMBT  DS    C                   SAVE BTYP SO WE CAN RESTORE IT               
SHRBK    DS    XL2                                                              
SAVRMKT  DS    XL2                 RATING SERVICE MARKET                        
SAVSTA   DS    CL(L'DBSELSTA)      SAVE STATION CALL LETTER                     
SVPBOOK  DS    XL(L'DBACTBK)                                                    
SVDBACBK DS    XL(L'DBACTBK)       SAVE AREA FOR DBACTBK                        
         DS    0F                                                               
TOTHMSHR DS     XL(HOMSHRLN)       COMPOSITE AREA FOR HOME SHARES               
DUALTRMK DS    CL2                                                              
KEY      DS    CL48                                                             
KEYSAVE  DS    CL48                                                             
DMWORK   DS    12F                                                              
DYTMLIST DS    CL250               MULTI DAY/TIME LIST FOR DEMAND               
         SPACE 1                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
OLDSHR   DS    (IUNDEMS*2)F        ORIGINAL SHARES                              
SAVRTG   DS    (IUNDEMS*2)F        SAVED OLD RATING VALUES                      
MMUPASS  DS    X                   PASS NUMBER FOR MMU                          
QTRBKTAB DS    XL18                                                             
         EJECT                                                                  
*****************************************************************               
*                                                               *               
IUNREC   DS    0F    ***  RECORD LAYOUT FOR UPGRADES  ***       *               
*                                                               *               
*****************************************************************               
*                                                               *               
*        ORIGINAL VALUES                                        *               
*                                                               *               
*****************************************************************               
*                                                               *               
OLDUNV   DS    (IUNDEMS)F          UNIVERSES                    *               
         ORG   OLDUNV+(DISPHOM*4)                                               
UOUHOMES DS    F                                                *               
         ORG                                                                    
OLDUNVX  DS    0F                                               *               
OLDUNVLN EQU   *-OLDUNV                                         *               
*                                                               *               
*****************************************************************               
*                                                               *               
OLDRTG   DS    (IUNDEMS)F          RATINGS                      *               
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                *               
         ORG                                                                    
RTGLN    EQU   *-OLDRTG                                                         
OLDIMP   DS    (IUNDEMS)F          IMPRESSIONS                  *               
         ORG   OLDIMP+(DISPHOM*4)                                               
UOTHOMES DS    F                                                *               
         ORG                                                                    
OLDRTGX  DS    0F                                               *               
OLDRTGLN EQU   *-OLDRTG                                         *               
*                                                               *               
*****************************************************************               
*                                                               *               
OLDHPT   DS    (IUNDEMS)F          HUTS/PUTS                    *               
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                *               
         ORG                                                                    
OLDTOT   DS    (IUNDEMS)F          TSA TOTALS                   *               
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                *               
         ORG                                                                    
OLDHPTX  DS    0F                                               *               
OLDHPTLN EQU   *-OLDHPT                                         *               
*                                                               *               
*****************************************************************               
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*        NEW VALUES                                             *               
*                                                               *               
*****************************************************************               
*                                                               *               
NEWRTG   DS    (IUNDEMS)F          RATINGS                      *               
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                *               
         ORG                                                                    
NEWIMPS  DS    (IUNDEMS)F          IMPRESSIONS                  *               
         ORG   NEWIMPS+(DISPHOM*4)                                              
UNTHOMES DS    F                                                *               
         ORG                                                                    
NEWRTGX  DS    0F                                               *               
NEWRTGLN EQU   *-NEWRTG                                         *               
*                                                               *               
*****************************************************************               
*                                                               *               
NEWHPT   DS    (IUNDEMS)F          HUTS/PUTS                    *               
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                *               
         ORG                                                                    
NEWTOTS  DS    (IUNDEMS)F          TSA TOTALS                   *               
         ORG   NEWTOTS+(DISPHOM*4)                                              
UNQHOMES DS    F                                                *               
         ORG                                                                    
NEWHPTX  DS    0F                                               *               
NEWHPTLN EQU   *-NEWHPT                                         *               
*                                                               *               
*****************************************************************               
*                                                               *               
*        OTHER VALUES                                           *               
*                                                               *               
*****************************************************************               
*                                                               *               
HOMSHR   DS    0F                  ORIGINAL HOMES SHARES        *               
UOSHOMES DS    F                                                *               
         DS    2F                                               *               
HOMSHRX  DS    0F                                               *               
HOMSHRLN EQU   *-HOMSHR                                         *               
*                                                               *               
*****************************************************************               
*                                                               *               
LUNV     DS    (IUNDEMS)F          LOONEYVERSES                                 
         ORG   LUNV+(DISPHOM*4)                                                 
LUHOMES  DS    F                                                                
         ORG                                                                    
LUNVX    EQU   *                                                                
IUNEND   DS    0F                                               *               
*                                                               *               
*****************************************************************               
UPUIDX   DS    (IUNDEMS)F          OLD/NEW UNIVERSE INDEX                       
*                                                                               
IO       DS    (IUNEND-IUNREC)C                                                 
         ORG   IO                                                               
         DS    2000C               ALLOW FOR 2000 BYTE NETWK RECDS              
         ORG   IO                                                               
ADJHPT   DS    (IUNDEMS*2)F        ADJUSTED HUTS/PUTS/TOTS                      
ADJRTG   DS    (IUNDEMS*2)F        ADJUSTED RATINGS                             
         ORG                                                                    
IO2      DS    2000C                                                            
IO3      DS    1000C                                                            
*                                                                               
Y1SHR    DS    (IUNDEMS*2)F        YEAR 1 SHARES FOR INDEXING                   
Y2SHR    DS    (IUNDEMS*2)F        YEAR 2 SHARES FOR INDEXING                   
Y3SHR    DS    (IUNDEMS*2)F        YEAR 3 SHARES FOR INDEXING                   
Y4SHR    DS    (IUNDEMS*2)F        YEAR 4 SHARES FOR INDEXING                   
IUNWRK   DS    CL200               DEMAND IUN AREA                              
*                                                                               
*                                                                               
DEMUPDX  EQU   *                                                                
         SPACE 2                                                                
IUNDEMS  EQU   32                                                               
DISPHOM  EQU   20                  FULLWORD DISPLACEMENT TO HOMES CELL          
LENVALS  EQU   IUNDEMS*4                                                        
         EJECT                                                                  
STNHPTWK DSECT                     ** STNHPT S/R LOCAL W/S **                   
STNSTNQ  EQU   50                  MAX STNS TABLES CAN HANDLE                   
STNSTLST DS    XL((5*STNSTNQ)+10)                                               
STNSTNBK DS    XL((7*STNSTNQ)+10)  STN/BK FOUND IN STNHK                        
STNSTCNT DS    XL1                 TOT NUMBER STATIONS FOUND                    
STNUNIV  DS    (IUNDEMS*1)F        UNIVS FROM GETIUN CALL IN STNHK              
STNRTIMP DS    (IUNDEMS*2)F        RTGS & IMPS "         "     "                
STNHPUTS DS    (IUNDEMS*2)F        HUTS & PUT ACCUMULATOR                       
STNHPTLN EQU   *-STNHPTWK                                                       
*                                                                               
* DEDBEXTRAD                                                                    
       ++INCLUDE DEDBEXTRAD                                                     
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* REGENINV                                                                      
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* REGENAVL                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* REGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131REDEMUP   08/06/18'                                      
         END                                                                    
