*          DATA SET DEDEM04    AT LEVEL 082 AS OF 01/03/13                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041325.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PROCESS USING(WARN(15))                                                        
*PHASE T21B04B                                                                  
         TITLE 'DEDEM04 - $DEM LIST MARKETS FOR A BOOK'                         
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                               
* -------- --- ----   ------------------------------------------------*         
* SEP01/02 060 BPOO - ARB MARKET                                      *         
* JAN09/01 059 BPOO - SUPPORT RADAR FILE                              *         
* NOV04/00 057 BPOO - SUPPORT USERID AUTHORIZATION                    *         
* NOV02/00 056 BPOO - MAKE DEM TABLES INTO PHASE DEM81                *         
* SEP22/00 055 BPOO - falink stuff...change screen one line lower     *         
* Jun20/00 053 GLEE - Change versioning scheme to support PC vrsn stmp*         
*                                                                     *         
* Mar08/00 052 GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Sep13/99 051 GLEE - Set CC upon exiting DEMPROC mode                *         
*                                                                     *         
* Feb11/99 050 GLEE - Implement version (extract) dates for DEM32     *         
*                                                                     *         
* JUN10/98 006 BPOO - Support for DEM32 session                                 
*                                                                               
* Nov20/95 005 GLEE - Support for STEREO session                                
*                                                                               
* Aug11/95 004 GLEE - Display alpha market codes as well                        
*                                                                               
* Jun09/95 003 GLEE - Clean up                                                  
*                                                                               
*  ??????   ?   ??  - HISTORY UNKNOWN                                           
***********************************************************************         
         EJECT                                                                  
DEM04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM4**,RA                                                    
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
*                                                                               
         ST    RD,DM04W_RD                                                      
                                                                                
         DS    0H                  SET UP ADCONS                                
         LH    R1,=Y(DISPTAB-DEM04)                                             
         LA    R1,DEM04(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM04_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM04(RE)        RE=A(TABLE)                                  
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM04_10                                                      
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
         MVC   LINE1+10(25),=C'DID ALL READS, NO POSTING'                       
         BE    EXIT                                                             
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                AND EXIT                                     
*                                                                               
YES      SR    R9,R9                                                            
NO       LTR   R9,R9                                                            
EXIT     XMOD1 1                                                                
                                                                                
*                                                                               
XITMODL  DS    0H                                                               
         LHI   R0,-1               EXIT/W CC LOW                                
         J     XITMODCR                                                         
XITMODCR DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9                                                            
         J     XITMOD                                                           
XITMOD   DS    0H                                                               
         L     RD,DM04W_RD                                                      
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE                                                 
*                                                                               
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
         MVC   DEMHD1(L'HEADMKT1),HEADMKT1                                      
         MVC   DEMHD2(L'HEADMKT2),HEADMKT2                                      
         LA    R0,BINMNAM-BINRECD  DISPLACEMENT TO KEY                          
         LA    RE,BINEND-BINRECD   L'KEY                                        
         LA    RF,BINEND-BINRECD   L'RECORD                                     
         CLI   OPTLIST,C'N'        TEST LIST IN ALPHA SEQUENCE                  
         BNE   *+12                                                             
         LA    R0,BINMRKT-BINRECD                                               
         LA    RE,BINEND-BINMRKT                                                
         ST    RE,BINLKEY          SET BINRCH PARMS                             
         STC   R0,BINDKEY                                                       
         ST    RF,BINLREC                                                       
         B     EXIT                                                             
                                                                                
                                                                                
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   TSKEYL,L'TDRAMNAM   ASSUME LIST IN ALPHA SEQUENCE                
         CLI   OPTLIST,C'N'                                                     
         BNE   *+8                                                              
         MVI   TSKEYL,L'TDRNMRKT    WRONG, LIST IN NUMERIC SEQUENCE             
                                                                                
         LA    R0,TDRLENQ+2                                                     
         STH   R0,TSRECL           SET MAX LENGTH OF RECORD                     
                                                                                
         MVI   ANYDATA,C'N'        ASSUME NO DATA FOR THIS REQUEST              
*                                                                               
DEMHD20  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
*                                                                               
DEMPROC  LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
*                                                                               
* SEE IF BOOK IS PROVIDED. IF BOOK PROVIDED THEN CALL OLD DEGET ROUTINE         
         OC    BKS,BKS                                                          
         BNZ   DMPRC03X                                                         
         CLI   DBMED,C'N'           NOT APPLICABLE FOR NETWORK, THOUGH          
         BE    DMPRC03X                                                         
         MVI   DBFNC,DBGETAMB                                                   
         CLI   DBMED,C'U'                                                       
         BNE   DMPRC03X                                                         
         MVI   FERN,160             MISSING BOOK FOR COUNTY COVERAGE            
         MVI   APMODE,FORCEEND                                                  
         J     XITMODL                                                          
DMPRC03X EQU   *                                                                
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1                                                  
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBFUNCT,DBFNC                                                    
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELBK,BKS                                                      
         MVC   DBBTYPE,BKS+3                                                    
         XC    MARKETN,MARKETN     CLEAR N'MARKETS                              
*                                  CALL DEMAND TO READ RECORDS                  
*                                                                               
* radar just fudge in the market for now                                        
         CLI   DBSRC,C'R'                                                       
         BNE   DMPRC05                                                          
         XC    MARNAME,MARNAME                                                  
         MVC   MARNAME(14),=C'RADAR MARKET 1'                                   
         MVC   MARKET,=X'0001'                                                  
         MVC   MARALF,=C'USA'                                                   
         BAS   RE,DEMHOOK                                                       
         B     DMPRC07X                                                         
*                                                                               
DMPRC05  DS    0H                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK,0                                   
                                                                                
*                                                                               
DMPRC06  DS    0H                                                               
         CLI   DBSELMED,C'C'                                                    
         BNE   DMPRC06X                                                         
         CLI   DBSELSRC,C'A'                                                    
         BNE   DMPRC06X                                                         
         CLI   DBBTYPE,C'W'                                                     
         BE    DMPRC06X                                                         
         CLI   DBERROR,EOF                                                      
         BNE   DMPRC06X                                                         
*                                                                               
         OC    DBSELBK,DBSELBK                                                  
         BZ    DMPRC06X                                                         
*                                                                               
         MVC   YMDSV,DBSELBK                                                    
*        XC    YMDSV,=X'FFFF'                                                   
         MVI   YMDSV+2,15                                                       
         GOTO1 VDATCON,DMCB,(3,YMDSV),(0,DATESV)                                
         GOTO1 VGETBROD,DMCB,(X'01',DATESV),WORKSV,VGETDAY,VADDAY               
         MVC   DATESV,WORKSV+6                                                  
         GOTO1 VNSIWEEK,DMCB,DATESV,(1,VGETDAY),VADDAY,VDATCON                  
         MVC   DBSELBK+1(1),DMCB                                                
         MVC   DBSELBK(1),DMCB+4                                                
*        XC    DBSELBK,=X'FFFF'                                                 
DMPRC06B MVI   DBBTYPE,C'W'                                                     
         MVI   WKMNFLG,C'Y'                                                     
         B     DMPRC05                                                          
*                                                                               
DMPRC06X OC    BKS,BKS                                                          
         BNZ   DMPRC07X                                                         
         MVC   MARKETN,BINSOFAR+2                                               
         L     R1,BINSOFAR                                                      
         BCTR  R1,0                                                             
         TM    DEMFLAG1,DF1STERO                                                
         BZ    *+10                                                             
         SR    R1,R1                                                            
         ICM   R1,3,TSARBLCK+(TSPRECN-TSARD)                                    
         STCM  R1,3,MARKETN                                                     
DMPRC07X EQU   *                                                                
                                                                                
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DMPRC10              POST THE STEREO WAY                         
                                                                                
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         XC    BINMRKT,BINMRKT                                                  
         XC    BINMNAM,BINMNAM                                                  
         MVC   BINMNAM+L'BINMNAM-2(2),MARKETN                                   
         GOTO1 APOST                                                            
         B     DMPRCX                                                           
                                                                                
DMPRC10  DS    0H                  SPECIAL POSTING FOR STEREO                   
         CLI   ANYDATA,C'Y'         IF THERE WERE NO DATA,                      
         BNE   DMPRCX                SKIP THIS PART                             
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL              MOVE IN LENGTH OF RECORD             
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,TDRRTMSG                                                 
         LA    R1,TDRAMNAM+L'TDRAMNAM-L'MARKETN                                 
         CLI   OPTLIST,C'N'                                                     
         BNE   *+8                                                              
         LA    R1,TDRNMNAM+L'TDRNMNAM-L'MARKETN                                 
         MVC   0(L'MARKETN,R1),MARKETN     MOVE IN # OF MARKETS                 
         GOTO1 APOST                                                            
                                                                                
         DS    0H                  POST COLUMN HEADERS                          
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,TDRRTHDR                                                 
         MVI   TDRHHLIN,1           1ST HEADLINE                                
         GOTO1 APOST                                                            
         MVI   TDRHHLIN,2           2ND HEADLINE                                
         GOTO1 (RF)                                                             
         B     DMPRCX                                                           
                                                                                
         DROP  R2,R4                                                            
*                                                                               
DMPRCX   DS    0H                  POST RECORD                                  
*&&DO                                                                           
         B     EXIT                                                             
*&&                                                                             
         B     EXITE                                                            
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
DEMHOOK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                  SEARCH RECORD FOR NAME ELEMENT               
* RADAR WE ARE FUDGING FOR NOW TO GET MARKET OUT THERE                          
         CLI   DBSRC,C'R'                                                       
         BE    DEMHOOK4                                                         
*                                                                               
         L     R1,DBAREC                                                        
         LA    R1,DMFRSTEL-DMKEY(R1)                                            
         USING DMELEM,R1           R1=A(FIRST ELEMENT)                          
         SR    RE,RE                                                            
*                                                                               
         CLI   DBSRC,C'A'                                                       
         BNE   DEMHOOK2                                                         
         CLI   DBFIL,C'T'                                                       
         BNE   DEMHOOK2                                                         
         CLI   DBMED,C'T'                                                       
         BNE   DEMHOOK2                                                         
         CLC   =X'6601',DBACTBK    <JAN02  NOT VALID FOR ARB,TP                 
         BH    DEMHOOKX                                                         
*                                                                               
***DEMHOOK2 DS    0H                                                            
*                                                                               
DEMHOOK2 CLI   DMELEM,0            TEST E-O-R                                   
         BE    DEMHOOK6                                                         
*                                                                               
         CLI   DMELEM,DMECODEQ     TEST MARKET NAME ELEMENT                     
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     DEMHOOK2                                                         
*                                  EXTRACT MARKET NAME FROM ELEMENT             
         MVC   MARNAME,SPACES                                                   
         MVC   MARKET,DMMNO                                                     
************************************************************                    
         L     RE,ADEMFIL                                                       
         CLC   =C'LPM',8(RE)          LOCAL PEOPLE METER SHOULD                 
         BNE   DEMHK2A                ONLY SHOW BOSTON                          
*                                                                               
         OC    CDEMTABS,CDEMTABS                                                
         BNZ   DEMHKLP1                                                         
         LHI   R0,QDEMTABS                                                      
         ICM   R0,B'1110',=X'D9000A'                                            
         GOTO1 VCALLOV,DMCB,0,(R0)                                              
         MVC   CDEMTABS,0(R1)                                                   
*                                                                               
DEMHKLP1 GOTO1 CDEMTABS,DMCB,FUSNENDP                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)                                                         
         USING FUSENDPD,RE                                                      
DEMHKLP2 CLC   =X'FFFF',0(RE)                                                   
         BE    DEMHOOKX                                                         
         CLC   MARKET,FUSNMKT                                                   
         BE    DEMHKLP3                                                         
         AR    RE,R0                                                            
         B     DEMHKLP2                                                         
         DROP  RE                                                               
*                                     LPM                                       
DEMHKLP3 CLC   MARKET,=H'101'         NY                                        
         BNE   DEMHK2C                                                          
         CLC   DBACTBK,=X'6818'       >=JUN0504 ALLOW NY TO BE LPM              
         BL    DEMHOOKX                                                         
***      CLI   DBBTYPE,C'P'           >=06/05/04 NY IS PEOPLE METER             
***      BNE   DEMHOOKX               DATA ELSE DONT LIST STANDARD              
         B     DEMHK2C                                                          
*****&&DO                                                                       
DEMHKBOS CLC   MARKET,=H'106'         BOS                                       
**       BNE   DEMHOOKX                                                         
         BNE   DEMHKLA                                                          
         B     DEMHK2C                                                          
DEMHKLA  CLC   MARKET,=H'403'         LA                                        
         BNE   DEMHKCHI                                                         
         B     DEMHK2C                                                          
DEMHKCHI CLC   MARKET,=H'202'         CHICAGO                                   
         BE    DEMHK2C                                                          
         CLC   MARKET,=H'407'        SAN FRANCISCO                              
         BE    DEMHK2C                                                          
         CLC   MARKET,=H'104'        PHL                                        
         BE    DEMHK2C                                                          
         CLC   MARKET,=H'111'        WAS DC                                     
         BE    DEMHK2C                                                          
         CLC   MARKET,=H'105'        DET                                        
         BE    DEMHK2C                                                          
         CLC   MARKET,=H'223'        DALLAS                                     
         BE    DEMHK2C                                                          
         B     DEMHOOKX                                                         
*****&&                                                                         
************************************************************                    
DEMHK2A  DS    0H                                                               
         L     RE,ADEMFIL             EEKLY TIME PERIOD OTHERWISE               
         CLC   =C'WTP',8(RE)          SHOW EVRYTHING BUT BOSTON                 
         BNE   DEMHK2C                                                          
         CLC   MARKET,=H'106'                                                   
         BE    DEMHOOKX                                                         
         CLI   DBBTYPE,C'P'           NO BOOKTYPE P FOR WEEKLY METERED          
         BE    DEMHOOKX               ALL WEEKLY PEOPLE METER IS LPM            
         CLC   MARKET,=H'403'         LA  POST AUG0704 GOES TO LPM              
         BNE   DEMHK2C                NOT WTP                                   
         CLC   DBACTBK,=X'6821'       <=AUG0504 ALLOW LA TO BE WTP              
         BNL   DEMHOOKX                                                         
                                                                                
DEMHK2C  DS    0H                                                               
         CLI   DBSELMED,C'N'          NET DOESNT CALL DEFINE                    
         BE    *+8                                                              
         CLI   DBSELMED,C'U'                                                    
         BNE   DEMHK2P                                                          
         MVC   WORK,SPACES                                                      
***      GOTO1 VDEFINE,DMCB,=C'CTY#',DBLOCK,WORK                                
**       MVC   MARKET,WORK+0                                                    
**       MVC   WORK,SPACES                                                      
**       GOTO1 VDEFINE,DMCB,=C'CTYN',DBLOCK,WORK                                
**       MVC   MARNAME,WORK+0                                                   
         L     RF,DBAREC                                                        
         MVC   MARKET,DMMINOR-DMKEY(RF)                                         
*                                                                               
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         B     DEMHK2S                                                          
DEMHK2D  CLI   DMELEM,0            TEST E-O-R                                   
         BE    DEMHK2S                                                          
*                                                                               
         CLI   DMELEM,DMECODEQ     TEST MARKET NAME ELEMENT                     
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     DEMHK2D                                                          
*                                  EXTRACT MARKET NAME FROM ELEMENT             
         MVC   MARNAME,SPACES                                                   
         L     RF,DBAREC                                                        
         MVC   MARKET,DMMINOR-DMKEY(RF)                                         
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         B     DEMHK2S                                                          
DEMHK2P  MVC   WORK,SPACES                                                      
*                                                                               
         GOTO1 VDEFINE,DMCB,=C'MNAME',DBLOCK,WORK                               
         MVC   MARKET,WORK+0                                                    
         MVC   MARNAME,WORK+2                                                   
DEMHK2S  DS    0H                                                               
*&&DO                                                                           
*                                                                               
         CLI   DBSELMED,C'U'                                                    
         BNE   *+14                                                             
         L     RF,DBAREC                                                        
         MVC   MARKET,DMMINOR-DMKEY(RF)                                         
*                                                                               
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         DROP  R1                                                               
*&&                                                                             
DEMHOOK3 MVC   WORK,SPACES                                                      
         CLI   DBMED,C'W'           WEEKLY FUDGED TO TP USTV                    
         BE    *+8                                                              
         CLI   DBMED,C'O'           OVERNIGHTS FUDGED TO TP USTV                
         BNE   *+8                                                              
         MVI   DBINTMED,C'T'                                                    
         CLI   DBSRC,C'F'           FUSION FUDGE TO NSI                         
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         MVC   MARALF,WORK                                                      
         CLI   DBMED,C'W'                                                       
         BNE   *+8                                                              
         MVI   DBINTMED,C'W'                                                    
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   DBINTMED,C'O'       RESET                                        
         CLI   DBSRC,C'F'                                                       
         BNE   *+8                                                              
         MVI   DBACTSRC,C'F'       RESET                                        
                                                                                
         OC    OPTSTRT(2),OPTSTRT  TEST IF NAME FILTER WANTED                   
         BZ    DEMHOOK4                                                         
         ZIC   R1,OPTSTRT                                                       
         EX    R1,*+8              TEST IF THIS MARKET QUALIFIES                
         B     *+10                                                             
         CLC   MARNAME(0),OPTSTRT+1                                             
         BNE   DEMHOOK6                                                         
*                                  BUILD BINSRCH RECORD & POST                  
DEMHOOK4 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHK4A              POST THE STEREO WAY                         
                                                                                
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         MVC   BINMNAM,MARNAME                                                  
         MVC   BINMRKT,MARKET                                                   
         MVC   BINALFM,MARALF                                                   
         GOTO1 APOST                                                            
         DROP  R1                                                               
         B     DEMHOOK6                                                         
                                                                                
DEMHK4A  DS    0H                  POSTING FOR STEREO                           
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         MVC   0(2,R2),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
                                                                                
         LA    RE,TDRAMNAM         ASSUME LIST BY ALPHA SEQUENCE                
         LA    RF,TDRAMRKT                                                      
         CLI   OPTLIST,C'N'                                                     
         BNE   *+12                                                             
         LA    RE,TDRNMNAM         LIST BY NUMERIC SEQUENCE                     
         LA    RF,TDRNMRKT                                                      
         MVC   0(L'MARNAME,RE),MARNAME  MOVE MARKET NAME AND NUMBER             
         MVC   0(L'MARKET,RF),MARKET     INTO TSAR DEMO RECORD                  
                                                                                
         MVC   TDRALF,MARALF       MOVE IN ALPHA MARKET CODE                    
         MVI   TDRRTYP,TDRRTMKT                                                 
                                                                                
         GOTO1 APOST                                                            
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
                                                                                
         B     DEMHOOK6                                                         
         DROP  R3,R4                                                            
*                                  BUMP N'ACTIVE MARKETS                        
DEMHOOK6 SR    R1,R1                                                            
         ICM   R1,3,MARKETN                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MARKETN                                                     
*                                  RETURN TO DEMAND                             
DEMHOOKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*                                                                               
DEMLINE  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DEMLIN20             YES, DO STEREO LOGIC                        
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINMNAM,X'FF'       IGNORE DUMMY E-O-F RECORDS                   
         BE    EXIT                                                             
*                                                                               
         OC    BINMRKT,BINMRKT     TEST N'MARKETS ENTRY                         
         BNZ   DEMLINE2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,BINMNAM+L'BINMNAM-2                                         
         EDIT  (R1),(4,LINE1+15),ALIGN=LEFT                                     
*                                                                               
         LA    R1,LINE1+16                                                      
         AR    R1,R0                                                            
*&&DO                                                                           
         MVC   0(L'ACTVMESS,R1),ACTVMESS                                        
*&&                                                                             
         LA    RE,L'ACTVMESS-1                                                  
         LA    RF,ACTVMESS                                                      
         OC    BKS,BKS                                                          
         BNZ   *+12                                                             
         LA    RE,L'ACTVMES2-1                                                  
         LA    RF,ACTVMES2                                                      
         EXMVC RE,0(R1),0(RF)                                                   
         B     EXIT                                                             
*                                  MARKET ENTRY                                 
DEMLINE2 EDIT  (B2,BINMRKT),(4,LINE1+1)                                         
                                                                                
         MVC   LINE1+9(L'BINALFM),BINALFM                                       
         MVC   LINE1+15(L'BINMNAM),BINMNAM                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
DEMLIN20 DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
                                                                                
* 1st record is always a message record telling # of markets,                   
* Subsequent records has the following format:                                  
*   Mkt# (STSPOLST) Alpha Mkt (STSPOLST) Mkt Name                               
                                                                                
         L     R2,ASTIOBUF                                                      
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
         DROP  R1                                                               
                                                                                
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32?                              
         BO    DMLN200                                                          
*                                                                               
         CLI   TDRRTYP,TDRRTMKT    DOES RECD HAVE MARKET INFO?                  
         BE    DEMLIN30                                                         
         CLI   TDRRTYP,TDRRTMSG    DOES RECD HAVE LIST MESSAGE?                 
         BE    DEMLIN25                                                         
         CLI   TDRRTYP,TDRRTHDR    DOES RECD HAVE HEADER COLUMNS?               
         BE    DEMLIN50                                                         
         CLI   TDRRTYP,TDRRTXFF                                                 
         BE    DEMLINX                                                          
         DROP  R5                                                               
         DC    H'0'                                                             
*                                                                               
DEMLIN25 DS    0H                  DO N'MARKETS ACTIVE MESSAGE                  
         USING TSDEMRCD,R5                                                      
         LA    RF,TDRAMNAM+L'TDRAMNAM-L'MARKETN                                 
         CLI   OPTLIST,C'N'                                                     
         BNE   *+8                                                              
         LA    RF,TDRNMNAM+L'TDRNMNAM-L'MARKETN                                 
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVC   1(L'ACTVMESS,R2),ACTVMESS                                        
         LA    R2,L'ACTVMESS(R2)                                                
         B     DEMLIN95                                                         
         DROP  R5                                                               
*                                                                               
DEMLIN30 DS    0H                  DO MKT NAME/MKT #/ALPHA MKT ENTRY            
         USING TSDEMRCD,R5                                                      
                                                                                
         MVC   MARNAME,TDRAMNAM    ASSUME LIST BY ALPHA SEQUENCE                
         MVC   MARKET,TDRAMRKT                                                  
         CLI   OPTLIST,C'N'                                                     
         BNE   *+16                                                             
         MVC   MARNAME,TDRNMNAM    LIST BY NUMERIC SEQUENCE                     
         MVC   MARKET,TDRNMRKT                                                  
         MVC   MARALF,TDRALF                                                    
                                                                                
         ZICM  R1,MARKET,(3)                                                    
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
                                                                                
         MVI   0(R2),STSPOLST      FLD SEPARATOR (LISTS USE SEMICOLON)          
         LA    R2,1(R2)                                                         
         MVC   0(L'MARALF,R2),MARALF                                            
         LA    R2,L'MARALF(R2)                                                  
                                                                                
         MVI   0(R2),STSPOLST      FLD SEPARATOR (LISTS USE SEMICOLON)          
         LA    R2,1(R2)                                                         
         MVC   0(L'MARNAME,R2),MARNAME                                          
         LR    R0,R2                                                            
         LA    R2,L'MARNAME-1(R2)                                               
DEMLIN32 CR    R0,R2                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),C' '                                                       
         BH    DEMLIN33                                                         
         BCT   R2,DEMLIN32                                                      
                                                                                
DEMLIN33 DS    0H                                                               
         LR    RE,R0               RE-->START OF MARKET NAME                    
         LR    RF,R2               RF-->LAST BYTE OF MARKET NAME                
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF = L(MARKET NAME)                          
DEMLN33A CLI   0(RE),STSPOLST      IF SEPARATOR IN MARKET NAME,                 
         BNE   *+8                                                              
         MVI   0(RE),C','           REPLACE IT W/ COMMA                         
         LA    RE,1(RE)                                                         
         BCT   RF,DEMLN33A                                                      
         B     DEMLIN95                                                         
         DROP  R5                                                               
*                                                                               
DEMLIN50 DS    0H                  DO COLUMN HEADERS                            
         USING TSDEMRCD,R5                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'HEADMKT1),HEADMKT1                                        
         CLI   TDRHHLIN,1                                                       
         BE    *+10                                                             
         MVC   WORK(L'HEADMKT2),HEADMKT2                                        
                                                                                
         MVI   WORK+6,STSPOLST      FUDGE IN THE SEPARATORS                     
         MVI   WORK+13,STSPOLST                                                 
                                                                                
         LA    R1,L'HEADMKT1-1                                                  
         CLI   TDRHHLIN,1                                                       
         BE    *+8                                                              
         LA    R1,(L'HEADMKT2-11)-1  (LEAVES OUT THE DASHES)                    
         EXMVC R1,0(R2),WORK                                                    
         AR    R2,R1                                                            
         B     DEMLIN95                                                         
         DROP  R5                                                               
*                                                                               
DEMLIN95 DS    0H                                                               
         MVI   1(R2),STSPIKEY                                                   
         LA    R2,2(R2)                                                         
         S     R2,ASTIOBUF                                                      
         STH   R2,IODATALN                                                      
         B     DEMLINX                                                          
*                                                                               
DMLN200  DS    0H                                                               
         MVI   GOSUBN,D32DL#                                                    
         GOTO1 AGOSUB                                                           
*                                                                               
DEMLINX  DS    0H                                                               
         B     EXIT                                                             
         DROP  RB,RA                                                            
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
         DS    0H                                                               
GOSUB    NTR1  BASE=*,LABEL=N                                                   
*                                                                               
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
*                                                                               
         MVC   ASUBRTN,ASUBR01                                                  
         B     GOSUBGO                                                          
*                                                                               
GOSUBGO  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,GOSUBN                                                        
         GOTO1 ASUBRTN,(R1)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
*******************************************************                         
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HEADMKT1 DC    C'MARKET  ALPHA  MARKET NAME'                                    
HEADMKT2 DC    C'NUMBER   MKT   -----------'                                    
         SPACE 1                                                                
ACTVMESS DC    C'ACTIVE MARKETS FOR REQUESTED BOOK'                             
ACTVMES2 DC    C'ACTIVE MARKETS FOR REQUESTED FILE/SOURCE'                      
*                                                                               
*                                                                               
         DROP  R7,R8,R9,RB                                                      
         EJECT                                                                  
*********************************************************                       
*=============SUBROUTINE POOL ==========================                        
* AT ENTRY ,                                                                    
* R9 --->DEMWRKD                                                                
* R8 ---> TWA                                                                   
* R7 --->DEMTMPD                                                                
* R1 ---> EQUATED SUBROUTINE NUMBER                                             
SUBR01Q  EQU   (((*-DEM04+4095)/4096)*4096)                                     
         ORG   DEM04+SUBR01Q                                                    
SUBR01   NMOD1 0,**0404**                                                       
         USING DEMWRKD,R9                                                       
         USING DEMTWAD,R8                                                       
         USING DEMTMPD,R7                                                       
*                                                                               
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
*                                                                               
D32DL#   EQU   (R01_01-*)/4+1      DEMLINE PROCESSING FOR DEM32                 
D32MK#   EQU   (R01_02-*)/4+1      DOWNLOAD MARKET LIST                         
OKDWNLD# EQU   (R01_03-*)/4+1      OKAY TO DOWNLAD ?                            
GADLD#   EQU   (R01_04-*)/4+1      GET A(DOWNLOAD DATA TABLES)                  
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R01_02   B     D32DLMK             DOWNLOAD MARKET LIST                         
R01_03   B     OKDOWNLD            OKAY TO DOWNLOAD?                            
R01_04   B     GADLDTAB            GET A(DOWNLOAD DATA TABLE)                   
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
YES_STE  SR    R9,R9                                                            
NO_STE   LTR   R9,R9                                                            
XIT_STE  XIT1                                                                   
*********************************************************                       
*          DATA SET DEDEM02A   AT LEVEL 093 AS OF 07/02/98                      
*-------------------- GET A(DOWNLOAD DATA TABLES) --------------------*         
                                                                                
* Gets address of corresponding download data entry                             
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
* At exit,                                                                      
*   ADLDNTRY = A(download data table entry)                                     
                                                                                
GADLDTAB DS    0H                                                               
         ICM   R2,15,ADLDTABS      R2-->DOWNLOAD DATA TABLES                    
         BZ    GADLDX                                                           
         USING DLDTABD,R2                                                       
                                                                                
*                                                                               
GADLD012 DS    0H                                                               
         CLI   DLDTLEN,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DLDTFIL,DBFIL                                                    
         BNE   GADLD018                                                         
         CLC   DLDTSRC,DBSRC                                                    
         BNE   GADLD018                                                         
         CLC   DLDTMED,DBMED                                                    
         BNE   GADLD018                                                         
         CLC   DLDTRTYP,TMPRTYP                                                 
         BNE   GADLD018                                                         
         CLC   DLDTVRSN,D32PCVER                                                
         BH    GADLD018                                                         
         B     GADLD019                                                         
*                                                                               
GADLD018 DS    0H                  BUMP TO NEXT DOWNLOAD DATA ENTRY             
         ZIC   R0,DLDTLEN                                                       
         AR    R2,R0                                                            
         B     GADLD012                                                         
GADLD019 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     GADLDX                                                           
                                                                                
*                                                                               
GADLDX   DS    0H                                                               
         ST    R2,ADLDNTRY                                                      
         J     EXIT                                                             
         DROP  R2                                                               
         TITLE 'DEDEM02 - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--OKD+        
               L#)'                                                             
*------------------------- OKAY TO DOWNLOAD? -------------------------*         
                                                                                
* See if data can be downloaded to PC                                           
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
*   FALEMPC  = element map code                                                 
*   FALDMPC  = data    map code                                                 
* At exit,                                                                      
*   CC set to eql if data can be downloaded                                     
*   CC set to neq if otherwise                                                  
                                                                                
OKDOWNLD DS    0H                                                               
         MVI   GOSUBN,GADLD#                                                    
         GOTO1 AGOSUB                                                           
         ICM   R2,15,ADLDNTRY                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  R2-->DOWNLOAD DATA TABLE ENTRY               
         LA    R3,DLDTFIXL(R2)                                                  
         SR    R0,R0                                                            
*                                                                               
OKDL022  DS    0H                  BUMP TO SECTION FOR ELEMENT MAP CODE         
         CLI   0(R3),0                                                          
         BE    OKDLXN                                                           
         CLC   FALEMPC,1(R3)                                                    
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     OKDL022                                                          
*                                                                               
         LA    R4,3(R3)                                                         
OKDL025  DS    0H                                                               
         OC    0(L'FALDMPC,R4),0(R4)                                            
         BZ    OKDLXN                                                           
         CLC   0(L'FALDMPC,R4),FALDMPC                                          
         BE    *+12                                                             
         LA    R4,2(R4)                                                         
         B     OKDL025                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     OKDLXY                                                           
                                                                                
*                                                                               
OKDLXN   DS    0H                                                               
         J     NO                                                               
OKDLXY   DS    0H                                                               
         J     YES                                                              
*********************************************************                       
D32DEMLN  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   TMPRTYP,TDRRTYP                                                 
          CLI   TDRRTYP,TDRRTMKT                                                
          BE    D32DL100                                                        
          B     D32DLX                                                          
D32DL100  DS    0H                                                              
          MVI   GOSUBN,D32MK#                                                   
          GOTO1 AGOSUB                                                          
D32DLX    B     XIT_STE                                                         
******************* DEM32 DOWNLOAD MARKET LIST *******                          
D32DLMK   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   FALEMPC,=Y(FMHBKMK)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                  ALPHA MARKET                                
          MVC   FALDMPC,=Y(FMDBKMKAMKT)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRALF),TDRALF                                           
          LA    R0,WORK                                                         
          LA    R1,3                                                            
          BAS   RE,D32TPAD                                                      
*                                                                               
          DS    0H                  NUMERIC MARKET                              
          MVC   FALDMPC,=Y(FMDBKMKNMKT)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRAMRKT),TDRAMRKT  SSUME LIST BY ALPHA SEQ              
          CLI   OPTLIST,C'N'                                                    
          BNE   *+10                                                            
          MVC   WORK(L'TDRNMRKT),TDRNMRKT                                       
          ZICM  R1,WORK,2                                                       
          EDIT  (R1),(4,WORK),ALIGN=LEFT                                        
          LA    R0,WORK                                                         
          LA    R1,4                                                            
          BAS   RE,D32TPAD                                                      
*                                                                               
          DS    0H                  MARKET NAME                                 
          MVC   FALDMPC,=Y(FMDBKMKMNAM)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRAMNAM),TDRAMNAM  SSUME LIST BY ALPHA SEQ              
          CLI   OPTLIST,C'N'                                                    
          BNE   *+10                                                            
          MVC   WORK(L'TDRNMNAM),TDRNMNAM                                       
          LA    R0,WORK                                                         
          LA    R1,30                                                           
          BAS   RE,D32TPAD                                                      
*                                                                               
          B     XIT_STE                                                         
*                                                                               
          DROP  R5                                                              
*                                                                               
D32TPAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
          XIT1                                                                  
*                                                                               
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(GOSUB-DEM04),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM04),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(DLDATTAB-DEM04),AL2(ADLDTABS-DEMTMPD)                        
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
*                                                                               
*          DATA SET DEDEM02A   AT LEVEL 093 AS OF 07/02/98                      
DLDATTAB    DS  0D                                                              
*                                                                               
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
ANYDATA  DS    CL1                 ANY DATA FOR THIS REQUEST? (Y/N)             
DM04W_RD DS    A                                                                
         ORG                                                                    
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
MARKETN  DS    XL2                 N'ACTIVE MARKETS FOR BOOK                    
MARKET   DS    XL2                 MARKET NUMBER                                
MARNAME  DS    CL30                MARKET NAME                                  
MARALF   DS    CL3                 ALPHA MARKET CODE                            
TMPRTYP  DS    XL(L'TDRRTYP)       TEMP STORAGE FOR TSAR DEM RECD TYP           
GOSUBN   DS    XL1                 SUB-ROUTINE NUMBER                           
ADLDNTRY DS    A                                                                
ASUBRTN  DS    A                                                                
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUBR01 POOL INTERFACE)                     
ADLDTABS DS    A                                                                
CDEMTABS DS    A                   A(DEMTABS)                                   
*                                                                               
YMDSV    DS    XL3                                                              
DATESV   DS    CL6                                                              
WORKSV   DS    XL12                                                             
WKMNFLG  DS    CL1                                                              
*                                                                               
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINMNAM  DS    CL30                MARKET NAME                                  
BINMRKT  DS    XL2                 MARKET NUMBER                                
BINALFM  DS    CL3                 ALPHA MARKET CODE                            
BINEND   EQU   *                                                                
                                                                                
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  TSAR DEMO RECORD KEY                         
TDRRTYP  DS    XL1                  RECORD TYPE                                 
                                                                                
TDRAKEY  DS    0C                  LIST BY ALPHA (MKT NAME) SEQUENCE            
TDRAMNAM DS    CL(L'MARNAME)        MARKET NAME                                 
TDRAMRKT DS    XL(L'MARKET)         MARKET NUMBER                               
         ORG   TDRAKEY                                                          
TDRNKEY  DS    0C                  LIST BY NUMERIC (MKT #) SEQUENCE             
TDRNMRKT DS    XL(L'MARKET)         MARKET NUMBER                               
TDRNMNAM DS    CL(L'MARNAME)        MARKET NAME                                 
         ORG   TDRNKEY                                                          
TDRHKEY  DS    0C                  KEY FOR RECD CONTAINING HDR INFO             
TDRHHLIN DS    XL1                  HEADLINE NUMBER                             
         ORG                                                                    
                                                                                
TDRDATA  DS    0X                  DATA                                         
TDRALF   DS    CL(L'MARALF)         ALPHA MARKET CODE                           
                                                                                
TDRLENQ  EQU   *-TSDEMRCD                                                       
                                                                                
* TDRRTYP can have the following values:                                        
TDRRTMSG EQU   X'40'               RECORD CONTAINS INFO MESSAGE                 
TDRRTHDR EQU   X'80'               RECORD SIGNIFIES TO OUTPUT COL HDR           
TDRRTMKT EQU   X'C0'               RECORD CONTAINS MARKET INFORMATION           
TDRRTXFF EQU   XFF                 (RESERVED FOR DEM00)                         
*                                                                               
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL1                 L(ENTRY)                                     
DLDTFMS  DS    0CL(L'DBFIL+L'DBSRC+L'DBMED)                                     
DLDTFIL  DS     CL(L'DBFIL)         FILE                                        
DLDTSRC  DS     CL(L'DBSRC)         SOURCE                                      
DLDTMED  DS     CL(L'DBMED)         MEDIA                                       
DLDTRTYP DS    XL(L'TDRRTYP)      TSAR DEMO RECORD TYPE                         
DLDTVRSN DS    XL(L'D32PCVER)      STEREO DEM EXTRACT VERSION                   
DLDTFIXL EQU   *-DLDTABD                                                        
                                                                                
DLDTDATA DS    0X                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082DEDEM04   01/03/13'                                      
         END                                                                    
         EJECT                                                                  
