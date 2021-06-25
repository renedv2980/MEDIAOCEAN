*          DATA SET DEDEM03    AT LEVEL 136 AS OF 10/23/17                      
*PHASE T21B03E,*                                                                
         SPACE 1                                                                
* ACTN=SPILLTO  - LISTS MARKETS A STATION SPILLS TO                             
*                                                                               
* ACTN=STATION  - LISTS ACTIVE STATIONS FOR A MARKET                            
*                                                                               
* ACTN=SYSCODE  - LIST SYSCODE INFORMATION FOR FUSION                           
*                                                                               
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* NOV28/01 105 BPOO - FIX SPILL AND HOME INDICATOR FOR DEM32          *         
* MAR28/01 104 BPOO - SPILL CANADIAN ALL BOOKS (FUDGE TO NOV00)       *         
* JAN11/01 102 BPOO - SUPPORT RADAR FILE                              *         
* NOV07/00 101 BPOO - SUPPORT BBM WEEKLY - PASS BOOKTYPE TO AD32TBK   *         
* NOV04/00 100 BPOO - SUPPORT USERID AUTHORIZATION                    *         
* NOV02/00 099 BPOO - MAKE DEM TABLES INTO SEPERATE PHASE DEM81       *         
* SEP22/00 098 BPOO - falink stuff...change screen one line lower     *         
* Jul12/00 097 GLEE - Skip DBGETMK call USTV/NSI/Cable spill markets  *         
*              GLEE - Append "/(spill mkt)" for DEM16 downloads       *         
*                                                                     *         
* Jun20/00 096 GLEE - Change versioning scheme to support PC vrsn stmp*         
*                                                                     *         
* Mar08/00 095 GLEE - Use new TSAR I/O area for TSAR routines                   
*              GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Jan05/00 094 GLEE - Kill booktype if booktype is x'05' (NSI 2A-6A)  *         
*                                                                     *         
* Dec07/99 093 GLEE - Force affil = "N/A" for BBM Canada (many I/Os)  *         
*                                                                     *         
* Sep13/99 092 GLEE - Set CC upon exiting phase                       *         
*                                                                     *         
* Apr01/99 091 GLEE - More test for spill using the spill market      *         
*                                                                     *         
* Mar23/99 090 GLEE - Bug fixes                                       *         
*                                                                     *         
* Mar22/99 089 GLEE - For media=N source=H, manual download "booktype"*         
*                      to DEM32                                       *         
*              GLEE - Use RA as second base register in main NMOD area*         
*                                                                     *         
* Mar05/99 088 GLEE - Test for spill using the spill market           *         
*                                                                     *         
* Feb11/99 087 GLEE - Support for "DEM32" style of translating books  *         
*                                                                     *         
* Jan27/99 086 GLEE - In DEM32 session, download "" for booktype "\"  *         
*                      (for optimized version, LVL=085, only)                   
*                                                                     *         
* Jan18/99 085 GLEE - More optimization for DEM32 ACTN=STATION dwnload*         
*                                                                     *         
* Jan12/99 084 GLEE - Set up SUBR02                                   *         
*                                                                     *         
* Jan12/99 083 GLEE - Kill booktype if book is prior to a certain date*         
*                                                                     *         
* Dec23/98 082 GLEE - Optimize DEM32 station download                 *         
*                                                                     *         
* Dec01/98 081 GLEE - FALINK break/resume for buffer overflows        *         
*                                                                     *         
* Nov24/98 080 GLEE - Fix bug in GETAFFL routine                      *         
*                                                                     *         
* Nov17/98 079 GLEE - Suppress cable for NSI PAV                      *         
*                                                                     *         
* Nov05/98 078 GLEE - Check for public station using C'P ', not C'P'  *         
*                                                                     *         
* Nov04/98 077 GLEE - Optimize download for ACTN=STATION for DEM32    *         
*                                                                     *         
* Jul16/97 013 GLEE - Initialize  AFFILS  field (to spaces)           *         
*                                                                     *         
* Mar13/97 012 GLEE - If spill, pass "WABC/PHL" back to STEREO        *         
*                                                                     *         
* Nov20/95 011 GLEE - Support for STEREO                              *         
*                                                                     *         
* Aug15/95 010 GLEE - Prepatory stage for STEREO support              *         
*                                                                     *         
* Aug15/95 009 GLEE - Display alpha market codes on screen            *         
*                                                                     *         
* Jul20/95 008 GLEE - Handle scenario when invalid alpha mkt inputted.*         
*                                                                     *         
* Feb14/95 007 GLEE - support alpha market inputs in Stations field   *         
*                                                                     *         
*  ??????   ?   ??  - HISTORY UNKNOWN                                 *         
***********************************************************************         
T21B03   TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS'                          
DEM03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DM03WRKL,**DEM3**,RA,CLEAR=YES                                   
         USING DM03WRKD,RC                                                      
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
*                                                                               
         ST    RD,DM03W_RD                                                      
         ST    RB,MYBASE1                                                       
*                                  HANDLE CONTROLLER MODE SETTINGS              
         DS    0H                  SET UP ADCONS                                
         LH    R1,=Y(DISPTAB-DEM03)                                             
         LA    R1,DEM03(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM03_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM03(RE)        RE=A(TABLE)                                  
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM03_10                                                      
*                                                                               
* SET BOOK CUTOFF TO 5 YEARS PRIOR TO TODAY'S DATE                              
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+8,F'-5'                             
         GOTO1 VDATCON,DMCB,(0,WORK+8),(3,FULL)                                 
         MVC   BKCUTOFF,FULL                                                    
*                                                                               
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
         CLI   APMODE,APMBREAK     TASKS TO DO ON A TRANSACTION BREAK           
         BE    DEMBREAK                                                         
         CLI   APMODE,APMRESUM     TASKS TO DO ON A TRANSACTION RESUME          
         BE    DEMRESUM                                                         
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
         J     EXIT                 AND EXIT                                    
                                                                                
YES      SR    R9,R9                                                            
NO       LTR   R9,R9                                                            
EXIT     XMOD1 1                                                                
                                                                                
*                                                                               
XITMOD   DS    0H                  EXITS TO DEM03 CALLER                        
         L     RD,DM03W_RD                                                      
         XMOD1                                                                  
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE FOR DEMO LOOK-UPS                               
*                                                                               
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
         CLI   ACTN,STATION                                                     
         BNE   DEMHEAD2                                                         
         MVC   DEMHD1(L'HEADINS1),HEADINS1                                      
         MVC   DEMHD2(L'HEADINS2),HEADINS2                                      
         LA    R0,BINMNAM-BINRECD  DISPLACEMENT TO KEY                          
         LA    RE,BINEND2-BINRECD  L'KEY                                        
         LA    RF,BINEND2-BINRECD  L'RECORD                                     
         CLI   OPTLIST,C'N'        TEST LIST IN NUMERIC SEQUENCE                
         BNE   DEMHEAD4                                                         
         LA    R0,BINRMKT-BINRECD                                               
         LA    RE,BINEND2-BINRMKT                                               
         B     DEMHEAD4                                                         
*                                                                               
DEMHEAD2 CLI   ACTN,SPILLTO                                                     
         BNE   DEMHEAD3                                                         
         MVC   DEMHD1(L'HEADOUT1),HEADOUT1                                      
         MVC   DEMHD2(L'HEADOUT2),HEADOUT2                                      
         LA    R0,BINMNAM-BINRECD  DISPLACEMENT TO KEY                          
         LA    RE,BINEND1-BINRECD  L'KEY                                        
         LA    RF,BINEND1-BINRECD  L'RECORD                                     
         CLI   OPTLIST,C'N'        TEST LIST IN NUMERIC SEQUENCE                
         BNE   DEMHEAD4                                                         
         LA    R0,BINRMKT-BINRECD                                               
         LA    RE,BINEND1-BINRMKT                                               
         B     DEMHEAD4                                                         
*                                  SET BINSRCH PARMS                            
DEMHEAD3 CLI   ACTN,SYSCODE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DEMHD1(L'HEADSYS1),HEADSYS1                                      
         MVC   DEMHD2(L'HEADSYS2),HEADSYS2                                      
         CLC   =X'6C01',BKS                                                     
         BH    DEMHD3C                                                          
         MVC   DEMHD1(L'HEADSYS3),HEADSYS3                                      
         MVC   DEMHD2(L'HEADSYS4),HEADSYS4                                      
         B     DEMHD3C                                                          
DEMHD3A  DS    0H                                                               
DEMHD3C  DS    0H                                                               
*                                                                               
         LA    R0,BINMSON-BINRECD  DISPLACEMENT TO KEY                          
         LA    RE,BINEND3-BINRECD  L'KEY                                        
         LA    RF,BINEND3-BINRECD  L'RECORD                                     
         CLI   OPTLIST,C'S'        TEST LIST BY SYSCODE                         
         BE    DEMHD3D                                                          
         CLI   OPTLIST,C'M'        LIST BY MSO NAME                             
         BE    DEMHEAD4                                                         
*                                                                               
DEMHD3D  DS    0H                  LIST BY SYSCODE                              
         LA    R0,BINSYSC-BINRECD                                               
         LA    RE,BINEND3-BINSYSC                                               
*                                                                               
DEMHEAD4 ST    RE,BINLKEY                                                       
         STC   R0,BINDKEY                                                       
         ST    RF,BINLREC                                                       
         B     EXIT                                                             
*                                                                               
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS                
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
*                                                                               
*                                                                               
* CHECK WHAT VERSION OF DEM32 WE ARE - IF VERSION IS LESS THAN XTRCVER9         
* THEN WE WILL FORCE SPILLTO ACTION TO PROCESS SYSCODE LISTING BECAUSE          
* WE DONT HAVE PC SUPPORT FOR THE SYSCODE LISTS YET                             
*                                                                               
         CLC   D32PCVER,=AL4(XTRCVER9)                                          
         BNL   DEMHD11                                                          
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BZ    DEMHD11                                                          
         CLI   DBSRC,C'F'                                                       
         BNE   DEMHD11                                                          
         CLI   ACTN,SPILLTO                                                     
         BNE   DEMHD11                                                          
         MVI   DBFNC,DBGETSYS                                                   
         MVI   ACTN,SYSCODE                                                     
DEMHD11  DS    0H                                                               
*                                                                               
         MVI   ANYDATA,C'N'        ASSUME NO DATA FOR THIS REQUEST              
         CLI   ACTN,STATION                                                     
         BE    DEMHD12                                                          
         CLI   ACTN,SPILLTO                                                     
         BE    DEMHD14                                                          
         CLI   ACTN,SYSCODE                                                     
         BE    DEMHD15                                                          
         DC    H'0'                                                             
*                                                                               
DEMHD12  DS    0H                  ACTN=STATION                                 
         MVI   TSKEYL,TDRSIKYL                                                  
         LA    R0,TDRSIRCL+2                                                    
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+12                                                             
         MVI   TSKEYL,TDRI2KYL                                                  
         LH    R0,LTSIOREC         R1=L(TSAR I/O AREA)                          
         B     DEMHD16                                                          
*                                                                               
DEMHD14  DS    0H                  ACTN=SPILLTO                                 
         MVI   TSKEYL,TDRSOKYL                                                  
         LA    R0,TDRSORCL+2                                                    
         B     DEMHD16                                                          
DEMHD15  DS    0H                  ACTN=SYSCODE                                 
         MVI   TSKEYL,TDRI3KYL                                                  
         LA    R0,TDRI3RCQ+2                                                    
         B     DEMHD16                                                          
*                                                                               
DEMHD16  DS    0H                                                               
         STH   R0,TSRECL                                                        
*                                                                               
DEMHD20  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
*                                                                               
DEMPROC  DS    0H                                                               
*                                                                               
         CLI   DBMED,C'N'          NETWORK MKT VALIDATES W/IN DEMAND            
         BE    DP07                                                             
         CLI   DBMED,C'U'                                                       
         BE    DP07                                                             
*                                                                               
*&&DO                                                                           
DP05     LA    R3,KEY              BUILD KEY FOR MARKET NAME RECORD             
         USING DMKEY,R3            R3=A(KEY)                                    
         XC    DMKMAJOR,DMKMAJOR                                                
         MVI   DMCODE,DMCODEQU                                                  
         MVC   DMMEDIA,DBMED                                                    
         CLI   DBMED,C'W'          WEEKLY FUDGE TO USTV                         
         BE    *+8                                                              
         CLI   DBMED,C'O'          OVERNIGHTS FUDGE TO USTV                     
         BNE   *+8                                                              
         MVI   DMMEDIA,C'T'                                                     
         MVC   DMSRC,DBSRC                                                      
         CLI   DBSRC,C'F'          FUSION FUDGE TO READ NSI                     
         BNE   *+8                                                              
         MVI   DMSRC,C'N'                                                       
         MVI   IOFLAG,DIR+READ+DEM                                              
         GOTO1 AIO                 READ MARKET NAME DIRECTORY RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
DP07     DS    0H                  NEED TO TRANSLATE ALPHA MKT?                 
         XC    KMARKET,KMARKET     KMARKET WILL CONTAIN MKT #                   
         CLI   DBFNC,DBGETSYS      TRANSLATE FOR SYSCODE ACTION TOO             
         BE    *+8                                                              
         CLI   DBFNC,DBGETMS           DID USER ASK FOR THIS FNCTION?           
         BNE   DP10                     NO, DON'T TRANSLATE                     
         CLI   DBFNC,DBGETSYS      SYSCODE LISTING- IF ONLY STATION             
         BNE   DP08                FIELD FILLED IN AND ITS NON NUMERIC          
***      B     DP09                                                             
         TM    STAS,X'F0'          IT MUST BE THE MKT ALPHA ENTERED             
         BO    DP09                                                             
                                                                                
DP7B     OC    ALFMKTS,ALFMKTS                                                  
         BNZ   DP09                                                             
         MVC   DUB(L'ALFMKTS),STAS                                              
         B     DP09A                                                            
                                                                                
DP08     OC    STAS,STAS               DO WE HAVE MKT ALREADY?                  
         BNZ   DP10                     YES, DON'T TRANSLATE                    
*                                                                               
DP09     OC    ALFMKTS,ALFMKTS         IS THERE AN ALPHA MKT?                   
         BZ    DP10                     NO, DON'T TRANSLATE                     
         MVC   DUB(L'ALFMKTS),ALFMKTS                                           
DP09A    BAS   RE,TRAMKT                                                        
         OC    DUB+3(2),DUB+3      DUB+3(2) HAS BINARY MARKET #                 
         BZ    DP20                 IF NO MKT #, DON'T DO ANYTHING              
         MVC   KMARKET,DUB+3        ELSE, GO PROCESS IT                         
*                                  INITIALIZE DBLOCK FOR DEMO READING           
DP10     DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA2                                                  
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBFILE,DBFIL                                                     
*                                                                               
         MVC   DBFUNCT,DBFNC                                                    
         CLI   DBFNC,DBGETSYS      SYSCODE LISTING- IF ONLY STATION             
         BNE   *+10                                                             
         MVC   DBSELSYC,SYSCODES                                                
         CLI   ACTN,STATION        IF ACTION=STATION,                           
         BNE   DP12B                                                            
         OC    BKS,BKS              AND IF NO BOOKS ENTERRED,                   
         BNZ   DP12B                                                            
         CLI   DBMED,C'N'           AND MEDIA IS NOT NETWORK,                   
         BE    DP12B                                                            
         MVI   DBFUNCT,DBGETASB     GET ALL STTNS/BOOKS                         
DP12B    EQU   *                                                                
*                                                                               
         CLI   ACTN,SPILLTO        IF ACTION=SPILL,                             
         BNE   DP12D                                                            
         OC    BKS,BKS              AND IF NO BOOKS ENTERRED,                   
         BNZ   DP12D                                                            
* canadian spill if no books = nov00 for now                                    
*                                                                               
         CLI   DBMED,C'C'                                                       
         BNE   DP12C                                                            
         MVC   BKS(2),=X'640B'                                                  
         MVC   DBFUNCT,DBFNC                                                    
         B     DP12D                                                            
*                                                                               
DP12C    DS    0H                                                               
         CLI   DBMED,C'N'           AND MEDIA IS NOT NETWORK,                   
         BE    DP12D                                                            
         CLI   DBMED,C'C'           AND MEDIA IS NOT NETWORK,                   
         BE    DP12D                                                            
         MVI   DBFUNCT,DBGETASM     GET ALL SPILL MKTS ACROSS ALL BOOKS         
DP12D    EQU   *                                                                
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELSTA,STAS                                                    
         OC    SYSCODES,SYSCODES    IF SYSCODE -NUMERIC NUMBER ENTERED          
         BZ    *+10                 DONT SET DBSELSTA                           
         XC    DBSELSTA,DBSELSTA    HMM THIS BREAKS SYSCODE LOOKUP              
         MVC   DBSELBK,BKS                                                      
         MVC   DBBTYPE,BKS+3                                                    
         CLI   DBFNC,DBGETMS        DID USER ASK FOR THIS FNCTION?              
         BNE   DP15                  NO, DON'T SET DBSELMRK                     
         OC    DBSELSTA,DBSELSTA    DBSELSTA ALL NULLS?                         
         BNZ   DP15                  NO, DON'T SET DBSELMRK                     
         MVC   DBSELRMK,KMARKET                                                 
*                                  CALL DEMAND TO READ RECORDS                  
DP15     DS    0H                                                               
         CLI   DBFNC,DBGETSYS      SYSCODE LSITING SET NUMERIC                  
         BNE   DP15X               MARKET                                       
         MVC   DBSELRMK,KMARKET                                                 
* FOR SYCODE LISTING IN STAS IS FILLED IN AND KMARKET NOT SET- ASSUME           
* ITS A MARKET ENTERED ONLY IF BOTH FILLED IN THEN IT IS STAT/MKT               
* IF ONLY NUMERIC FILLED IN ITS SYSCODE                                         
         OC    ALFMKTS,ALFMKTS                                                  
         BNZ   *+10                                                             
         XC    DBSELSTA,DBSELSTA                                                
*                                                                               
*                                                                               
DP15X    GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK,0                                   
         CLI   DBMED,C'U'          COUNTY COVERAGE                              
         BE    DP16X                                                            
         CLI   DBMED,C'N'                                                       
         BE    DP16X                                                            
         CLI   ACTN,STATION        IF ACTN=STATION,                             
         BNE   *+8                                                              
         BAS   RE,GETAFFL           GET AFFILIATIONS FOR STATIONS               
DP16X    EQU   *                                                                
*                                                                               
DP20     DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   FOR LIST ACTIONS UNDER A STEREO              
*&&DO                                                                           
         BZ    EXIT                 SESSION, 1ST RECD S/B A MSG RECD            
*&&                                                                             
         BZ    DP028X               SESSION, 1ST RECD S/B A MSG RECD            
         CLI   ANYDATA,C'Y'        IF THERE WERE NO DATA,                       
*&&DO                                                                           
         BNE   EXIT                 EXIT NOW                                    
*&&                                                                             
         BNE   DP028X                                                           
                                                                                
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         ZIC   R1,TSKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    2(0,R2),2(R2)       CLEAR TSAR DEMO RECORD KEY                   
         LA    R0,2+1+1(R1)                                                     
         STH   R0,0(R2)            SET RECORD LENGTH                            
         LA    R1,2+1(R1,R2)                                                    
         MVI   0(R1),0             MOVE IN DUMMY DATA                           
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
                                                                                
         DS    0H                                                               
         MVI   TDRRTYP,TDRRTMSG    POST MESSAGE RECORD                          
         GOTO1 APOST                                                            
                                                                                
         MVI   TDRRTYP,TDRRTHDR    POST COLUMN HEADER RECORDS                   
         MVI   TDRHDLIN,1           1ST HEADLINE                                
         GOTO1 (RF)                                                             
         MVI   TDRHDLIN,2           2ND HEADLINE                                
         GOTO1 (RF)                                                             
                                                                                
         MVI   TDRRTYP,TDRRTINI    POST INITIALIZATION RECORD                   
         GOTO1 (RF)                                                             
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   DP028X                                                           
         CLI   ACTN,STATION                  ACTN=STATION,                      
         BNE   DP028X                                                           
         CLC   D32PCVER,=AL4(XTRCVER2)       AND USING EXTRACT VER > 2,         
         BL    DP028X                                                           
         MVI   TDRRTYP,TDRRTDRQ              POST LAST STTN RECORD              
         MVI   TDRI2RMK,XFF                                                     
         GOTO1 (RF)                                                             
DP028X   EQU   *                                                                
         DROP  R2,R4                                                            
         B     EXITE                                                            
         EJECT                                                                  
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
DEMHOOK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                                                               
         CLI   DBSRC,C'F'           FOR FUSION                                  
         BNE   DEMHOOK1                                                         
         CLI   ACTN,SYSCODE                                                     
         BNE   DEMHOOK1                                                         
         CLI   DBRECTYP,DBRECSYS                                                
         BNE   DEMHOOKX                                                         
         B     DEMHK100                                                         
DEMHOOK1 DS    0H                                                               
*                                                                               
         MVC   MARNAME,SPACES                                                   
         MVC   MARNAME(11),=C'**UNKNOWN**'                                      
         LA    R1,DBKEY                                                         
         USING BSKEY,R1                                                         
         CLI   DBRECTYP,DBRECSM                                                 
         BNE   DEMHOOK2                                                         
         MVC   MARKET,BSRMKT       EXTRACT MARKET                               
         MVC   KMARKET,BSKMKT                                                   
         MVC   KBTYP,BSBTYP                                                     
         MVC   KBOOK,BSBOOK                                                     
         XC    KBOOK,=X'FFFF'                                                   
*                                                                               
         CLI   DBMED,C'U'                                                       
         BNE   DMHOOK1C                                                         
         MVC   STACALL,BSSTAT                                                   
         CLI   STACALL,X'D4'      NOT INTERESTED IN MARKET SPILLS               
         BNE   DMHOOK1C                                                         
         TM    STACALL+1,X'F0'                                                  
         BO    DEMHOOKX                                                         
*                                                                               
DMHOOK1C CLI   DBMED,C'T'          IF MEDIA IS USTV,                            
         BNE   DMHOOKGX                                                         
         CLI   KBTYP,X'05'          AND IT'S NSI 2A-6A DATA,                    
         BE    DEMHOOKX              IGNORE THIS RECORD                         
         CLI   KBTYP,C'C'           AND BOOKTYPE IS CABLE,                      
         BNE   DMHOOKGX                                                         
         CLC   FILE,=C'PAV'         AND FILE REQUESTED IS PAV,                  
         BE    DEMHOOKX             IGNORE THIS RECORD                          
DMHOOKGX EQU   *                                                                
         B     DEMHOOK3                                                         
*        CLI   DBSELMED,C'N'                                                    
*        BE    DEMHOOK3                                                         
*        CLI   DBSELMED,C'U'                                                    
*        BE    DEMHOOK3                                                         
*        B     DEMHOOK4                                                         
*                                                                               
         USING MLKEY,R1                                                         
DEMHOOK2 CLI   DBRECTYP,DBRECMS                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SAMEDMAF,C'N'                                                    
         CLC   MARKET,MLRMKT       IF ITS THE SAME DMA WE DONT NEED             
         BNE   *+8                 TO READ THE DEMO RECORD FOR INFO             
         MVI   SAMEDMAF,C'Y'                                                    
*                                                                               
         MVC   MARKET,MLRMKT       EXTRACT MARKET & STATION                     
*                                                                               
         MVC   KMARKET,MLKMKT                                                   
         MVC   STACALL,MLSTAT                                                   
* FUSION PASSES BACK THE STATION CALL LETTER REPRESENTATION IN                  
* DBACTSTA.                                                                     
*                                                                               
         CLI   DBSRC,C'F'                                                       
         BNE   DEMHOOK2A                                                        
         CLI   DBFUNCT,DBGETASB     GET ALL STTNS/BOOKS                         
         BE    *+10                                                             
         MVC   STACALL,DBACTSTA                                                 
DEMHOOK2A EQU  *                                                                
*                                                                               
         MVC   KBOOK,MLBOOK                                                     
         CLI   DBMED,C'N'                                                       
         BE    *+10                                                             
         XC    KBOOK,=X'FFFF'                                                   
         MVC   KBTYP,MLBTYP                                                     
         CLI   DBFUNCT,DBGETASB   IGNORE BOOKTYPES - IF ALL STATIONS            
         BNE   DHOOK2B            FOR ALL BOOKS UNDER A MARKET                  
         CLI   KBTYP,C'F'         IGNORE BOOKTYPES F AND Y FOR NOW              
         BE    DEMHOOKX                                                         
         CLI   KBTYP,C'Y'                                                       
         BE    DEMHOOKX                                                         
DHOOK2B  DS    0H                                                               
*                                                                               
         CLI   DBMED,C'O'          ONLY SHOW OVERNIGHTS STARTING                
         BNE   *+14                DEC0604                                      
         CLC   KBOOK,OVCUTOFF                                                   
         BL    DEMHOOKX                                                         
*                                                                               
         CLC   KBOOK,BTCUTOFF      IF BOOK IS PRIOR TO BKTYP CUT-OFF,           
         BNL   *+8                                                              
         MVI   KBTYP,0              KILL THE BOOKTYPE                           
*****  CHECK DDS TERMINAL- FOR FUSION FILE                                      
*****  CHECK DDS TERMINAL- ONLY ALLOW UNMERIC STATIONS FOR DDS                  
*****  ELSE WE COULDNT TRANSLATE THE CODES                                      
*                                                                               
         CLI   DBSRC,C'F'         FUSION ALLOW NUMERIC CALL LETTERS             
         BNE   DHOOK2D            ONLY FOR DDS TERMINALS                        
         L     RF,ATWA                                                          
         CLI   (TWAOFFC-TWAD)(RF),C'*'                                          
         BE    DHOOK2F                                                          
*                                                                               
DHOOK2D  TM    STACALL,X'F0'       NOT INTERESTED IN MARKET SPILLS              
         BO    DEMHOOKX                                                         
DHOOK2F  CLI   DBMED,C'T'          IF MEDIA IS USTV,                            
         BNE   DHOOK2GX                                                         
* ONLY APPLY TO BOOK CUTOFF IF THEY WANT ALL BOOKS/STATS                        
         CLI   DBFUNCT,DBGETASB     GET ALL STTNS/BOOKS                         
         BNE   *+14                                                             
         CLC   KBOOK,BKCUTOFF      IF BOOK IS PRIOR TO BOOK CUT-OFF,            
         BL    DEMHOOKX                                                         
         CLI   KBTYP,X'05'          AND IT'S NSI 2A-6A DATA,                    
         BE    DEMHOOKX              IGNORE THIS RECORD                         
         CLI   KBTYP,C'C'           AND BOOKTYPE IS CABLE,                      
         BNE   DHOOK2GX                                                         
         CLC   FILE,=C'PAV'         AND FILE REQUESTED IS PAV,                  
         BE    DEMHOOKX             IGNORE THIS RECORD                          
DHOOK2GX EQU   *                                                                
*                                                                               
*&&DO                                                                           
         CLI   DBSELMED,C'N'                                                    
         BE    DEMHOOK3                                                         
         CLI   DBMED,C'T'          IF MEDIA IS USTV,                            
         BNE   DHKSPILX                                                         
         CLI   DBSRC,C'N'           AND SOURCE IS NIELSEN,                      
         BNE   DHKSPILX                                                         
         CLI   KBTYP,C'C'           AND BOOKTYPE IS CABLE,                      
         BNE   DHKSPILX                                                         
         B     DEMHOOK4             SKIP SPILL STTN TEST AND CONTINUE           
DHKSPILX EQU   *                                                                
         OC    MLKMKT,MLKMKT       TEST IF A SPILL STATION                      
         BZ    DEMHOOK4                                                         
         DROP  R1                                                               
*&&                                GET SPILL MARKET FROM SPILL STATION          
DEMHOOK3 DS    0H                                                               
*^^TEST                                                                         
         CLI   ACTN,STATION                                                     
         BNE   DEMHK03B                                                         
         CLI   DBMED,C'U'         COUNTY COVERAGE -READ THE FILE                
         BE    DEMHK03B           FOR COUNTY/MARKET INFO                        
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DEMHOOKA                                                         
DEMHK03B DS    0H                                                               
*^^EOTEST                                                                       
* TAKE OUT READ FOR NOW *TEST*                                                  
         CLI   DBMED,C'U'          ONLY COUNTY COVERAGE COMPARE                 
         BNE   DEMHK3C             IF DMA CHANGED                               
         CLI   SAMEDMAF,C'Y'                                                    
         BNE   DEMHK3C                                                          
         MVC   MARNAME,SVMARNAM                                                 
         B     DEMHOOKA                                                         
DEMHK3C  LA    R5,DBLOCK2          POINT TO SECOND DBLOCK                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA                                                   
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBFILE,DBFIL                                                     
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELMED,DBMED                                                   
                                                                                
*        CLI   DBSRC,C'F'         FUSION                                        
*        BE    *+8                                                              
*        CLI   DBSELMED,C'O'      OVERNIGHTS                                    
*        BNE   *+10                                                             
         MVC   DBSELRMK,MARKET                                                  
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELSTA,STACALL    SET SPILL STATION                            
* COUNTY COVERAGE STUFF NEEDS THIS                                              
         CLI   DBMED,C'U'                                                       
         BNE   DEMHK03D                                                         
         MVC   DBSELRMK,KMARKET                                                 
         MVC   DBSELBK,KBOOK                                                    
         MVC   DBBTYPE,KBTYP                                                    
         CLI   STACALL,X'D4'      NOT INTERESTED IN MARKET SPILLS               
         BNE   DEMHK03D                                                         
         TM    STACALL+1,X'F0'                                                  
         BO    DEMHOOKX          FOR COUNTY COVERAGE                            
*                                                                               
DEMHK03D LA    R1,DBLOCK1                                                       
         CLI   DBSELMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   DBSELBK(2),DBSELBK-DBLOCKD(R1)                                   
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
*                                                                               
TESTING  DS    0H                                                               
*                                                                               
         CLI   DBSELMED,C'U'                                                    
         BE    *+10                                                             
         MVC   MARKET,DBACTRMK                                                  
         LR    R1,R5               PT TO DBLOCK JUST GOT BACK                   
         LA    R5,DBLOCK1                                                       
         OI    DBMODE,DBMDSEQ      SET READ SEQUENCE BROKEN                     
         CLI   DBERROR-DBLOCKD(R1),0                                            
         BNE   DEMHOOKA                                                         
         B     DEMHOOK6                                                         
         DROP  R5                                                               
*&&DO                              READ MARKET NAME FILE RECORD                 
DEMHOOK4 OC    MARKET,MARKET       TEST FOR HOME MARKET                         
         BZ    DEMHOOKX                                                         
*                                                                               
*  RADAR STIFF ALPHA MARKET IS STILL MUCKED UP-FUDGE FOR NOW                    
         CLI   DBSRC,C'R'                                                       
         BNE   *+14                                                             
         MVC   MARRALF,=C'USA'                                                  
         B     DEMHOOKX                                                         
*                                                                               
*^^TEST                                                                         
         CLI   ACTN,STATION                                                     
         BNE   *+12                                                             
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DEMHOOKA                                                         
*^^EOTEST                                                                       
         CLI   DBMED,C'U'         COUNTY COVERAGE DIDNT BUILT DMKEY FOR         
         BE    *+10               READING MARKET RECORD BEFORE.                 
         MVC   DMMINOR,MARKET                                                   
         MVI   IOFLAG,FIL+READ+DEM                                              
         GOTO1 AIO                                                              
         BNE   DEMHOOKA                                                         
*=======CALL DEFINE FOR MARKET NAME                                             
*                                                                               
*&&                                                                             
DEMHOOK6 MVC   WORK,SPACES                                                      
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         L     RE,AIOAREA                                                       
         ST    RE,DBAREC                                                        
         MVI   DBRECTYP,DBRECMK       FUDGE RECTYPE FOR DEFINE                  
         XC    SVMARNAM,SVMARNAM                                                
         CLI   DBSELMED,C'U'                                                    
         BE    DEMHK6A                                                          
         GOTO1 VDEFINE,MYDMCB,=C'MNAME',DBLOCK1,WORK                            
         MVC   MARKET,WORK+0                                                    
         MVC   MARNAME,WORK+2                                                   
         B     DEMHK6D                                                          
DEMHK6A  DS    0H                                                               
         CLI   ACTN,SPILLTO                                                     
         BE    DEMHK6B                                                          
         GOTO1 VDEFINE,MYDMCB,=C'DMA#',DBLOCK1,WORK                             
         MVC   MARKET,WORK+0                                                    
         GOTO1 VDEFINE,MYDMCB,=C'DMAN',DBLOCK1,WORK                             
         MVC   MARNAME(26),WORK+0                                               
         MVC   SVMARNAM,MARNAME                                                 
         B     DEMHK6D                                                          
DEMHK6B  DS    0H                                                               
         GOTO1 VDEFINE,MYDMCB,=C'CTY#',DBLOCK1,WORK                             
         MVC   MARKET,WORK+0                                                    
         GOTO1 VDEFINE,MYDMCB,=C'CTYN',DBLOCK1,WORK                             
         MVC   MARNAME(16),WORK+0                                               
         MVC   SVMARNAM(16),WORK+0                                              
DEMHK6D  DS    0H                                                               
         DROP  R5                                                               
***************************************************************                 
* RADAR SINCED WE FUDGED THE MARKET NAME RECORD STUFF- FUDGE HERE TOO           
         CLI   DBSRC,C'R'                                                       
         BNE   *+16                                                             
         MVC   MARNAME,SPACES                                                   
         MVC   MARNAME(14),=C'RADAR MARKET 1'                                   
*                                  GET RATING SVCE ALPHA MKT CODE               
DEMHOOKA DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVC   MARRALF,SPACES                                                   
         CLI   DBSELMED,C'N'                                                    
         BE    DEMHOOKC                                                         
         SR    R0,R0               SAVE MKT NUMBERS IN R0                       
         ICM   R0,12,DBSELMK        H.O.2.B = SPILL MARKET#                     
         ICM   R0,3,DBSELRMK        L.O.2.B = RTG SVCE MKT#                     
         XC    DBSELMK,DBSELMK                                                  
         MVC   DBSELRMK,MARKET                                                  
         MVC   WORK,SPACES                                                      
*                                                                               
*  RADAR STIFF ALPHA MARKET IS STILL MUCKED UP-FUDGE FOR NOW                    
         CLI   DBSRC,C'R'                                                       
         BNE   DEMHOOKB                                                         
         MVC   MARRALF,=C'USA'                                                  
         B     DEMHOOKC                                                         
*                                                                               
DEMHOOKB DS    0H                                                               
         CLI   DBMED,C'W'          WEEKLYS READ USTV                            
         BE    *+8                                                              
         CLI   DBMED,C'O'          OVERNIGHTS READ USTV                         
         BNE   *+8                                                              
         MVI   DBINTMED,C'T'                                                    
         CLI   DBSRC,C'F'          FUSION READ NSI                              
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         CLI   DBMED,C'W'          RESET BACK  WEEKLY                           
         BNE   *+8                                                              
         MVI   DBINTMED,C'W'                                                    
         CLI   DBMED,C'O'          RESET BACK TP OVERNIGHT                      
         BNE   *+8                                                              
         MVI   DBINTMED,C'O'                                                    
         CLI   DBSRC,C'F'         RESET BACK TO FUSION                          
         BNE   *+8                                                              
         MVI   DBACTSRC,C'F'                                                    
         MVC   MARRALF,WORK                                                     
         STCM  R0,12,DBSELMK        RESTORE SPILL MARKET#                       
         STCM  R0,3,DBSELRMK        RESTORE RTG SVCE MKT#                       
         DROP  R5                                                               
*                                  BUILD BINSRCH RECORD & POST                  
DEMHOOKC DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHK10              POST THE STEREO WAY                         
                                                                                
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         MVC   BINMNAM,MARNAME                                                  
         MVC   BINRMKT,MARKET                                                   
         MVC   BINKMKT,KMARKET                                                  
         MVC   BINRALF,MARRALF                                                  
         MVC   BINSTAT,STACALL                                                  
         MVC   BINBTYP,KBTYP                                                    
         MVC   BINBOOK,KBOOK                                                    
         GOTO1 APOST                                                            
         DROP  R1                                                               
         B     DEMHOOKX                                                         
*                                                                               
DEMHK10  DS    0H                  POSTING FOR STEREO                           
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         MVC   0(2,R2),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         MVI   TMPRTYP,TDRRTDRQ                                                 
         MVI   TDRRTYP,TDRRTDRQ                                                 
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'N'          IF MEDIA=NETWORK,                            
         BNE   DEMHK10G                                                         
         CLI   DBSRC,C'H'           AND SOURCE=NIELSEN HISPANIC,                
         BNE   DEMHK10G                                                         
         L     R6,ABTPTBNH          GET SET TO MANUAL DL BOOKTYPES              
DEMHK10G EQU   *                                                                
                                                                                
DEMHK10LP DS   0H                  START FOR NHT BKTYP MANUAL DL LOOP           
         CLI   ACTN,STATION                                                     
         BE    DEMHK10A                                                         
         CLI   ACTN,SPILLTO                                                     
         BE    DEMHK10B                                                         
         DC    H'0'                                                             
                                                                                
DEMHK10A DS    0H                  STEREO POSTING FOR ACTN=STATION              
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   DEMHK10AB                                                        
         MVI   GOSUBN,D32PST#                                                   
         GOTO1 AGOSUB                                                           
****************************************************************                
         BE    DEMHK10C                                                         
*                                                                               
         B     DEMHOOKX                                                         
*                                                                               
DEMHK10AB DS   0H                                                               
         MVC   TDRSIRMK,MARKET                                                  
         MVC   TDRSIKMK,KMARKET                                                 
         MVC   TDRSIBTP,KBTYP                                                   
         MVC   TDRSISTA,STACALL                                                 
         MVC   TDRSIBOK,KBOOK                                                   
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+10                                                             
         XC    TDRSIBOK,=X'FFFF'                                                
*                                                                               
         B     DEMHK10C                                                         
                                                                                
DEMHK10B DS    0H                  STEREO POSTING FOR ACTN=SPILLTO              
         MVC   TDRSORMK,MARKET                                                  
         MVC   TDRSOKMK,KMARKET                                                 
         MVC   TDRSOBTP,KBTYP                                                   
         MVC   TDRSOMNA,MARNAME                                                 
         MVC   TDRSORAM,MARRALF                                                 
         MVC   TDRSOBOK,KBOOK                                                   
         B     DEMHK10C                                                         
*                                                                               
DEMHK10C DS    0H                                                               
         GOTO1 APOST                                                            
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'N'          IF MEDIA=NETWORK,                            
         BNE   DEMHK10M                                                         
         CLI   DBSRC,C'H'           AND SOURCE=NIELSEN HISPANIC,                
         BNE   DEMHK10M                                                         
         AHI   R6,L'BTPTBNHT        GET NEXT BOOKTYPE                           
                                                                                
         CLI   0(R6),XFF            IF NO MORE BOOKTYPES TO GO,                 
         BE    DEMHK10M              EXIT LOOP                                  
         MVC   KBTYP,0(R6)                                                      
         B     DEMHK10LP                                                        
DEMHK10M EQU   *                                                                
                                                                                
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
         B     DEMHOOKX                                                         
         DROP  R2,R4                                                            
*                                                                               
* ACTION = SYSCODE FOR FUSION                                                   
*                                                                               
DEMHK100 DS    0H                                                               
         L     R2,AIOAREA2                                                      
*&&DO                                                                           
         CLC   BKS(2),=X'6C01'                                                  
         BL    DEMHK102                                                         
         OC    STAS,STAS           IF NO STATION SPECIFIED                      
         BZ    DEMHK102            USE OLD BTF RECORDS                          
*                                                                               
         TM    STAS,X'F0'                                                       
         BO    DEMHK102                                                         
*&&                                                                             
         USING DFUEKEY,R2                                                       
         CLC   =C'GTF',0(R2)                                                    
         BE    *+14                                                             
         CLC   =C'BTF',0(R2)                                                    
         BE    DEMHK102                                                         
*                                                                               
**       OC     ALFMKTS,ALFMKTS      ONLY NEW GTF RECS IF BOTH STA/MKT          
**       BZ     DEMHK102             ENTERED                                    
**       OC     SYSCODES(L'SYSCODES),SYSCODES                                   
**       BNZ    DEMHK101                                                        
         OC     OPTSTRT,OPTSTRT                                                 
         BZ     DMHK100C                                                        
         ZIC    R1,OPTSTRT                                                      
DMHK100B EX     R1,*+8                                                          
         B      *+10                                                            
         PACK   DUB,OPTSTRT+1(0)                                                
         CVB    R0,DUB                                                          
         STCM   R0,3,STARTSYS                                                   
*                                                                               
         CLC   STARTSYS,DFUESYSC   START LIST AT OPTION INPUT                   
         BH    DEMHOOKX                                                         
*                                                                               
*                                                                               
*                                                                               
DMHK100C CLC   =C'ALL',STAS         WE WANT ALL STATIONS FOR MKT ?              
         BE    DEMHK101                                                         
         TM    STAS,X'F0'           NUMERIC STATION ENTERED                     
         BNL   DEMHK101                                                         
* IF SYSCODE LEVEL LOOKUP  SPECIFIED AND NEW GTF RECORDS RETURNED               
* THEN DONT CHECK STATION- WE WANT ALL STATION THAT MATCH THE SYSCODE           
*                                                                               
         CLC   DFUESTN,STAS                                                     
         BNE   DEMHOOKX                                                         
*                                                                               
DEMHK101 MVC   MYSYSCDE,DFUESYSC   SYSCODE                                      
         MVC   SUBBASE+1(L'DFUEAIUE),DFUEAIUE    AD INSERTABLE                  
         MVC   MSOUNIV+1(L'DFUECAUE),DFUECAUE   CARRIAGE UNIVERSE EST           
         MVC   MARRALF,ALFMKTS                                                  
         MVC   DMAWSYS,DFUEFLGS                                                 
         OC    SYSCODES(L'SYSCODES),SYSCODES                                    
         BZ    *+14                                                             
         CLC   MYSYSCDE,SYSCODES   COMPARE SYSCODE NUMBER                       
         BNE   DEMHOOKX                                                         
         MVC   MSONAME(3),=C'N/A'                                               
         MVC   SYSCNAME(3),=C'N/A'                                              
         MVC   STACALL,DFUESTN                                                  
* TRANSLATE NUMERIC TO ALPHA MKT                                                
*                                                                               
         LA    R5,DBLOCK2         POINT TO SECOND DBLOCK                        
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIOAREA                                                   
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         CLI   DBMED,C'W'          WEEKLY READ USTV                             
         BE    *+8                                                              
         CLI   DBMED,C'O'          OVERNIGHTS READ UST                          
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         CLI   DBSRC,C'F'          FUSION READ NSI                              
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBFUNCT,DBCNVN2A                                                 
         MVC   DBSELRMK,DFUEMKT                                                 
*                                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         OI    DBMODE,DBMDSEQ      SET READ SEQUENCE BROKEN                     
         MVC   MARRALF,DBSELALF                                                 
*                                                                               
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         B     DEMHK150                                                         
         DROP  R5                                                               
*                                                                               
*  OLD BTF RECORDS                                                              
*                                                                               
*                                                                               
         USING DSYKEY,R2                                                        
DEMHK102 LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         MVC   MYSYSCDE,DSYSYSCD    SYSCODE NUMBER                              
*                                                                               
* FOR BTF RECORDS- WE HAVE NEW BTFS AND OLD BTFS                                
* THEY HAVE DIFFERENT ELEMENTS                                                  
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R2,DSYFRST-DSYKEY(R2)                                            
         USING FSCELEM,R2           R1=A(FIRST ELEMENT)                         
DEMHK105 CLI   FSCELEM,0            TEST E-O-R                                  
         BE    DEMHK150                                                         
         CLI   FSCELEM,FAUCODEQ     NEW UE ELEMENT                              
         BNE   DEMHK110                                                         
         USING FAUELEM,R2                                                       
         MVC   MSONAME,FAUMSONM                                                 
         MVC   SYSCNAME,FAUSYSNM                                                
         MVC   MARRALF,STAS                                                     
         MVI   NEWBTF,C'Y'                                                      
         MVC   BTFMKT,FAUMKT#                                                   
         MVC   SUBBASE,=C'N/A'     SUBSCRIBER BASE                              
         MVC   MSOUNIV,=C'N/A'     UNIVERSE                                     
* CHECK NEW DMA FLAG                                                            
         CLI   FAULEN,FAULENEQ                                                  
         BNE   DEMHK108                                                         
         MVC   DMAWSYS,FAUFLAGS                                                 
*                                                                               
DEMHK108 B     DEMHK125                                                         
         DROP  R2                                                               
*                                                                               
*                                                                               
         USING FSCELEM,R2           R1=A(FIRST ELEMENT)                         
DEMHK110 CLI   FSCELEM,FSCCODEQ     SUBSCRIBER BASE ELEMENT?                    
         BNE   DEMHK120                                                         
         MVI   NEWBTF,C'N'                                                      
***      CLC   FSCSBASE,FSCUNIV    IF SUBSCRIBER BASE > UNIV                    
***      BH    DEMHOOKX            THEN SUPRESS IT                              
         MVC   SUBBASE,FSCSBASE    SUBSCRIBER BASE                              
         MVC   MSOUNIV,FSCUNIV     UNIVERSE                                     
         B     DEMHK140                                                         
         DROP  R2                                                               
         USING FSCELEM,R2           R1=A(FIRST ELEMENT)"                        
DEMHK120 CLI   FSCELEM,FANCODEQ     FUSION ALPHA NAME ELEMENT ?                 
         BNE   DEMHK140                                                         
         USING FANELEM,R2                                                       
         MVC   MSONAME,FANMSONM                                                 
         MVC   SYSCNAME,FANSYSNM                                                
***      MVC   MARNMKT,             NUMERIC MARKET CODE WHEN WE GET IT          
         MVC   MARRALF,FANMKTCD     MARKET ALPHA CODE                           
****     MVC   STACALL(3),=C'N/A'                                               
*                                                                               
* FOLLOWING CHECK FILTERS EITHER BY ALPHA MARKET                                
* OR BY A SYSCODE NUMBER TO DISPLAY                                             
* IF THERES A NUMERIC SYSCODES IN BUFFER SYSCODES THAT MEANS                    
* A NUMERIC SYSCODES FILTER WAS APPLIED                                         
* ELSE AN ALPHA MKT WAS SELECTED                                                
*                                                                               
DEMHK125 OC    SYSCODES(L'SYSCODES),SYSCODES                                    
         BZ    DEMHK135                                                         
         CLC   MYSYSCDE,SYSCODES   COMPARE SYSCODE NUMBER                       
         BNE   DEMHOOKX                                                         
         B     DEMHK150                                                         
*                                                                               
********************************************************************            
**  CHECK PC VERSION -IF LOW VERSION - SPILL ACTION = SYSCODE                   
**  COMPARE TO STAS INSTEAD BECAUSE WE ARE USING THE SPILL ACTION FOR           
**  SYSCODES RIGHT NOW FOR PC , THE MARKET NAME IS WOULD BE STORE IN            
**  FIELD STAS AS WOULD THE SPILL ACTION                                        
*                                                                               
DEMHK135 TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BZ    DEMHK136                                                         
         CLC   D32PCVER,=AL4(XTRCVER9)                                          
         BNL   DEMHK136                                                         
         L     RE,ADEMACT                                                       
         CLC   =C'SPILL',8(RE)                                                  
         BNE   DEMHK136                                                         
**       CLC   MARRALF,STAS         COMPARE ON ALPHA MARKET                     
**       BNE   DEMHOOKX             FOR NOW TILL WE HAVE NUMERIC MKT            
         CLC   KMARKET,FANMKT#      COMPARE ON NUMERIC MKT #                    
         BNE   DEMHOOKX                                                         
         B     DEMHK150                                                         
DEMHK136 DS    0H                                                               
********************************************************************            
         CLI   NEWBTF,C'Y'                                                      
         BNE   DEMHK138                                                         
         CLC   KMARKET,BTFMKT       COMPARE NUMERIC MSO MARKET                  
         BE    DEMHK150             WITH NUMERIC MKT ASKED                      
         B     DEMHOOKX                                                         
DEMHK138 CLC   KMARKET,FANMKT#      COMPARE NUMERIC MSO MARKET                  
         BE    DEMHK150             WITH NUMERIC MKT ASKED                      
***      CLC   DBSELRMK,FANMKT#      COMPARE NUMERIC MSO MARKET                 
***      BNE   DEMHOOKX             WITH NUMERIC MKT ASKED                      
*        CLC   MARRALF,ALFMKTS      COMPARE ON ALPHA MARKET                     
*        BE    DEMHK150                                                         
*        CLC   MARRALF,STAS         COMPARE ON ALPHA MARKET                     
*        BE    DEMHK150             FOR NOW TILL WE HAVE NUMERIC MKT            
         B     DEMHOOKX                                                         
DEMHK140 DS    0H                                                               
         ZIC   RE,FANLEN                                                        
         AR    R2,RE                                                            
         B     DEMHK105                                                         
         DROP  R2                                                               
************ COMPARE IF MAINFRAME OF DEM32  TRANSACTION*********                
DEMHK150 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DEMHK200                                                         
* MAINFRAME TRANSACTION                                                         
         MVC   BINSBASE,SUBBASE     SUBSCRIBER BASE                             
         MVC   BINSUNV,MSOUNIV     UNIVERSE                                     
         MVC   BINMSON,MSONAME                                                  
         MVC   BINSYSN,SYSCNAME                                                 
         MVC   BINSYSMK,MARRALF                                                 
         MVC   BINSYSC,MYSYSCDE    SYSCODE NUMBER                               
         MVC   BINSSTAT,STACALL     CALL LETTERS                                
         MVI   BINDMAW,C'W'                                                     
         TM    DMAWSYS,X'80'       DMA/WIRED                                    
         BZ    *+8                                                              
         MVI   BINDMAW,C'D'                                                     
         B     DEMHK450                                                         
*                                                                               
DEMHK200 DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         MVC   0(2,R2),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
***      MVI   TMPRTYP,TDRRSYSQ                                                 
***      MVI   TDRRTYP,TDRRSYSQ                                                 
         MVI   TMPRTYP,TDRRTDRQ                                                 
         MVI   TDRRTYP,TDRRTDRQ                                                 
         MVC   TDRI3SCD,MYSYSCDE                                                
         MVC   TDRI3MSO,MSONAME                                                 
         MVC   TDRI3AMK(L'MARRALF),MARRALF                                      
***      MVC   TDRI3NMK,MARNMKT                                                 
         MVC   TDRI3SBS,SUBBASE                                                 
         MVC   TDRI3UNV,MSOUNIV                                                 
         CLC   =C'N/A',TDRI3SBS                                                 
         BNE   *+10                                                             
         XC    TDRI3SBS,TDRI3SBS                                                
         CLC   =C'N/A',TDRI3UNV                                                 
         BNE   *+10                                                             
         XC    TDRI3UNV,TDRI3UNV                                                
         MVC   TDRI3SYN,SYSCNAME                                                
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
*                                                                               
DEMHK450 DS    0H                                                               
         GOTO1 APOST                                                            
         B     DEMHOOKX                                                         
         DROP  R1,R2                                                            
*                                                                               
*                                  RETURN TO DEMAND                             
DEMHOOKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*======================= TRANSLATE ALPHA MARKET ======================*         
                                                                                
* Calls DEMAND to translate an alpha market into a numeric market               
* At entry, DUB(3)   = alpha market padded w/ spaces                            
* At exit,  DUB+3(2) = numeric market                                           
                                                                                
TRAMKT   NTR1                                                                   
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
*                                                                               
*  RADAR STUFF,JUST FUDGE MARKET STUFF FOR NOW....MARKET IS 1                   
         CLI   DBSRC,C'R'                                                       
         BNE   TRAMK10                                                          
         CLC   =C'USA',DUB                                                      
         BNE   TRAMK10                                                          
         MVC   DUB+3(2),=X'0001'                                                
         B     TRAX                                                             
*                                                                               
TRAMK10  DS    0H                                                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIOAREA2                                                  
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         CLI   DBMED,C'W'          WEEKLY READ USTV                             
         BE    *+8                                                              
         CLI   DBMED,C'O'          OVERNIGHTS READ UST                          
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         CLI   DBSRC,C'F'          FUSION READ NSI                              
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELALF,DUB                                                     
*                                                                               
         CLI   DUB+3,X'F0'                                                      
         BL    *+14                                                             
         MVI   DBFUNCT,DBCNVN2A                                                 
         MVC   DBSELRMK,DUB+3                                                   
*                                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
                                                                                
         MVC   DUB+3(2),DBSELRMK   MARKET # RETURNED IN DBSELRMK                
         CLI   DBMED,C'W'                                                       
         BNE   TRAMK30                                                          
         CLC   DUB+3(2),=X'00A8'   168 = ATL                                    
         BNE   *+10                FOR WEEKLY IT WAS NEVER CHANGED TO           
         MVC   DUB+3(2),=X'007C'   168- KEEP USING 124                          
TRAMK30  CLI   DBERROR,0           USE IT IF NO ERROR                           
         BE    TRAX                                                             
         XC    DUB+3(2),DUB+3       ELSE, RETURN NULLS                          
         DROP  R5                                                               
                                                                                
TRAX     DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== GET AFFILIATIONS =========================*         
                                                                                
* This routine is for ACTN=STATION only.  It picks out all the stations         
*  in the sort buffer and calls DEMAND and DEFINE to get their                  
*  affiliates.                                                                  
                                                                                
         DS    0H                                                               
GETAFFL  NTR1                                                                   
*   ALPHA MKT MARKET STILL MUCKED UP- KEEP ON FUDGING                           
*&&DO                                                                           
         CLI   DBSRC,C'R'                                                       
         BNE   *+14                                                             
         MVC   AFFILS,=CL5'N/A  '     FORCE AFFIL = "N/A"                       
         B     GAFX                                                             
*&&                                                                             
         XC    RECNUM,RECNUM                                                    
         CLI   DBMED,C'N'          N/A FOR NETWORK                              
         BNE   *+10                                                             
         SR    R3,R3                                                            
         B     GAFX                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         XC    PREVSTTN,PREVSTTN                                                
         MVC   AFFILS,SPACES                                                    
                                                                                
*                                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         XC    DBLOCK,DBLOCK       SET FIXED VALUES INTO DBLOCK                 
         MVC   DBFILE,=C'TP '            FILE,                                  
****     LA    R0,IOAREA1                                                       
****     ST    R0,DBAREC                 A(I/O AREA),                           
         MVC   DBAREC,AIOAREA1                                                  
         MVI   DBFUNCT,DBGETDEM          FUNCTION,                              
         MVC   DBCOMFCS,AFAC             A(COMFACS),                            
         MVC   DBSELSRC,DBSRC            SOURCE,                                
         MVC   DBSELBK,BKS               BOOK,                                  
         MVC   DBSELMED,DBMED            MEDIA                                  
         MVC   DBSELAGY,AGYALPH          AGENCY CODE,                           
         MVI   DBSELDAY,X'40'            DAY,                                   
         MVC   DBSELTIM,=AL2(0800,0815)  AND TIMES,                             
                                                                                
*                                                                               
GAF20    DS    0H                  GET (BINSRCH/TSAR) RECORD                    
         BAS   RE,GETSREC          R3-->RECORD ON RETURN                        
         OR    R3,R3               IF NULLS,                                    
         BZ    GAFX                 E-O-F REACHED                               
                                                                                
         LA    RF,GAF22                                                         
         TM    DEMFLAG1,DF1STERO                                                
         BZ    *+8                                                              
         LA    RF,GAF24                                                         
         BR    RF                                                               
                                                                                
GAF22    DS    0H                  EXTRACT INFO FROM BINSRCH RECORD             
         USING BINRECD,R3                                                       
         MVC   STACALL,BINSTAT                                                  
         MVC   KMARKET,BINKMKT                                                  
         MVC   KBOOK,BINBOOK                                                    
         MVC   KBTYP,BINBTYP                                                    
         B     GAF40                                                            
         DROP  R3                                                               
                                                                                
GAF24    DS    0H                  EXTRACT INFO FROM TSAR DEMO RECORD           
         LA    R3,2(R3)             BUMP PAST LENGTH BYTES                      
         USING TSDEMRCD,R3                                                      
         CLI   TDRRTYP,TDRRTDRQ     ONLY INTERESTED IN MKT INFO RECDS           
         BNE   GAF50                                                            
*                                                                               
         DS    0H                                                               
         MVC   STACALL,TDRSISTA                                                 
         MVC   KMARKET,TDRSIKMK                                                 
         MVC   KBOOK,TDRSIBOK                                                   
         MVC   KBTYP,TDRSIBTP                                                   
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   GAF28X                                                           
         CLI   TDRI2RMK,XFF        IF THIS CONDITION IS TRUE,                   
         BE    GAF50                SKIP THIS (DUMMY) RECORD                    
         MVC   STACALL,TDRI2STA                                                 
         MVC   KMARKET,TDRI2KMK                                                 
         MVC   KBOOK(1),TDRI2BKY                                                
         MVC   KBTYP,TDRI2BTP                                                   
*                                                                               
* APPLY VERSION CONTROL.  NEW DEM32 NEVER PASSES BOOK LIST TO                   
* PC  ALONG WITH THE STATION LIST DOWNLOAD                                      
         CLC   D32PCVER,=AL4(XTRCVERA)                                          
         BNH   GAF25                                                            
* EXCESSIVE IOS- CABLE DONT HAVE AFFILATION TO DONT BOTHER                      
* READING FOR THEM.                                                             
         CLI   KBTYP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                            
         BE    *+8                                                              
         CLI   KBTYP,BOOKTYPE_C3  LIVE+3 DMA CABLE                              
         BE    *+8                                                              
         CLI   KBTYP,BOOKTYPE_LS  LIVE+SD DMA CABLE                             
         BE    *+8                                                              
         CLI   KBTYP,BOOKTYPE_WS  LIVE+3D WIRED CABLE                           
         BE    *+8                                                              
         CLI   KBTYP,BOOKTYPE_HS  LIVE+SD HISPANIC                              
         BE    *+8                                                              
         CLI   KBTYP,BOOKTYPE_HT  HISPANIC TEST                                 
         BE    *+8                                                              
         CLI   KBTYP,C'Z'         LIVE ONLY WIRED CABLE                         
         BE    *+8                                                              
         CLI   KBTYP,C'U'         LIVE ONLY DMA CABLE                           
         BE    *+8                                                              
         CLI   KBTYP,C'W'                                                       
         BE    *+8                                                              
         CLI   KBTYP,C'C'                                                       
         BE    *+8                                                              
         CLI   KBTYP,C'P'                                                       
         BE    *+8                                                              
         CLI   KBTYP,C'B'                                                       
         BE    *+8                                                              
         CLI   KBTYP,C'H'         SKIP AFFILIATES LOOKUP FOR CABLES             
         BE    *+8                                                              
         CLI   KBTYP,C'J'                                                       
         BNE   GAF24A                                                           
         OC    TDRI2KMK,TDRI2KMK  CHECK SPILL                                   
         BZ    *+14                                                             
         MVC   AFFILS,=CL5'N/A'                                                 
         B     GAF44X                                                           
*                                                                               
GAF24A   XC    KBOOK,KBOOK                                                      
         XC    KBTYP,KBTYP                                                      
         B     GAF40                                                            
                                                                                
*                                                                               
GAF25    MVC   MYDATDSP,=Y(TDRI2KYL)                                            
         MVI   MYELCODE,TDREMWNQ                                                
         LR    R6,R3                HOLD ONTO ADDRESS IN R3                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KBOOK+1(1),2(R3)                                                 
         XC    KBOOK,=X'FFFF'                                                   
         LR    R3,R6                RESTORE ADDRESS BACK INTO R3                
GAF28X   EQU   *                                                                
         B     GAF40                                                            
         DROP  R3                                                               
*                                                                               
GAF40    DS    0H                  SET DYNAMIC VALUES INTO DBLOCK               
         CLC   PREVSTTN,STACALL     IF SAME AS PREVIOUS STATION,                
         BE    GAF44X                DON'T READ FILE AGAIN                      
         MVC   PREVSTTN,STACALL                                                 
         MVC   DBSELSTA,STACALL          STATION CALL LETTERS,                  
         MVC   DBSELMK,KMARKET           SPILL MARKET,                          
         MVC   DBSELBK,KBOOK             BOOK,                                  
****** THIS DOESNT SEEM TO BE WORKING CORRECTLY ******                          
** WHY DONT WE JUST READ ALL BOOKS POSTED IN TSAR RECORDS???*****               
         OC    BKS,BKS                    IF NO BOOKS ENTERRED,                 
         BNZ   GAF41                                                            
         CLI   TDRI2XPL-TSDEMRCD(R3),X'E0'     IF XTRA SPILL                    
         BE    GAF41                                                            
         XC    DBSELBK,DBSELBK             USE LATEST BOOK FOR AFFILS           
*                                                                               
*                                                                               
GAF41    DS    0H                                                               
         MVC   DBBTYPE,KBTYP             BOOK TYPE,                             
         XC    DBACTUAL,DBACTUAL                                                
         MVC   AFFILS,SPACES       INITIALIZE TO BLANKS                         
*                                                                               
         DS    0H                  QUICK FIX FOR BBM CANADA MANY I/Os           
         CLI   DBMED,C'C'           IF CANADA,                                  
         BNE   GAF42TCAX                                                        
         CLI   DBSRC,C'A'           BBM,                                        
         BNE   GAF42TCAX                                                        
         MVC   AFFILS,=CL5'N/A'     FORCE AFFIL = "N/A"                         
         B     GAF44X                                                           
GAF42TCAX EQU  *                                                                
         CLI   DBSRC,C'F'           FOR FUSION READ NSI AFFILIATES              
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,GAFHK,0                                     
         CLI   DBSRC,C'F'           FOR FUSION WE READ NSI AFFILIATES           
         BNE   *+8                  RESET BACK TO FUSION                        
         MVI   DBSELSRC,C'N'                                                    
GAF44X   EQU   *                                                                
*                                                                               
*                                                                               
                                                                                
         LA    RF,BINAFFL-BINRECD(R3)                                           
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BZ    GAF46M                                                           
         LA    RF,TDRI2KYL+2(R3)                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    GAF46M                                                           
         LA    RF,TDRSIAFL-TSDEMRCD(R3)                                         
GAF46M   EQU   *                                                                
*                                                                               
*                                                                               
GAF48    DS    0H                                                               
*                                                                               
         CLC   =C'KFRE',STACALL             FRESNO CHANGES                      
         BNE   GAF49                                                            
         CLC   =X'01D2',MARKET                                                  
         CLC   KBOOK,=X'6502'                                                   
         BL    GAF49                                                            
         MVC   AFFILS(1),=C'W'                                                  
GAF49    DS    0H                                                               
*&&DO                                                                           
         CLC   =C'KMTF',STACALL                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         MVC   0(L'AFFILS,RF),AFFILS   MOVE AFFILIATIONS INTO SORT RCD          
         TM    DEMFLAG1,DF1STERO       NEED TO PUT RECD BACK INTO TSAR          
         BZ    GAF50                                                            
         MVI   MYTSRBLK+(TSACTN-TSARD),TSAPUT                                   
         GOTO1 VTSAR,MYTSRBLK          (USE THE TSAR BLOCK IN GETSREC)          
*                                                                               
GAF50    DS    0H                  GET SET TO GET NEXT RECORD                   
         LH    R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,RECNUM                                                        
                                                                                
         B     GAF20                                                            
*                                                                               
GAFX     DS    0H                                                               
         B     EXIT                                                             
                                                                                
*                                                                               
** GET (SORT) RECORD **                                                         
*                                                                               
* R3 will contain A(record) on exit.                                            
*                                                                               
         DS    0H                                                               
GETSREC  NTR1                                                                   
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    GSR20                GET TSAR DEMO RECORD                        
*                                                                               
         DS    0H                  GETTING BINSRCH RECORD                       
         SR    R2,R2                                                            
         L     R3,BINLREC                                                       
         LH    R1,RECNUM                                                        
         MR    R2,R1               R3 = DISPL. TO NEXT RECORD                   
         A     R3,BINATAB          R3-->NEXT RECORD                             
         USING BINRECD,R3                                                       
         CLI   BINMNAM,X'FF'       IF E-O-F RECORD,                             
         BNE   *+6                                                              
         SR    R3,R3                DON'T PASS BACK AN ADDRESS                  
         B     GSRX                                                             
         DROP  R3                                                               
*                                                                               
GSR20    DS    0H                  GETTING TSAR DEMO RECORD                     
         LA    R4,MYTSRBLK                                                      
         USING TSARD,R4                                                         
         MVC   TSARD(TSARDL),TSARBLCK                                           
         MVI   TSACTN,TSAGET                                                    
         LA    R0,MYTSRREC                                                      
         ST    R0,TSAREC                                                        
         MVC   TSACOM,AFAC                                                      
         LH    R1,RECNUM                                                        
         LA    R1,1(R1)            TSAR RECORDS ARE ONE-BASED                   
         STH   R1,TSRNUM                                                        
         GOTO1 VTSAR,TSARD                                                      
         SR    R3,R3                                                            
         TM    TSERRS,TSEEOF                                                    
         BO    *+8                                                              
         ICM   R3,7,TSAREC+1                                                    
         B     GSRX                                                             
         DROP  R4                                                               
                                                                                
GSRX     DS    0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
** DEMAND HOOK TO EXTRACT AFFILIATIONS **                                       
*                                                                               
         DS    0H                                                               
GAFHK    NTR1                                                                   
         CLI   DBSRC,C'R'                                                       
         BNE   *+14                                                             
         MVC   AFFILS,=CL5'N/A  '     FORCE AFFIL = "N/A"                       
         B     GAFHKX                                                           
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'AFFL',DBLOCK,WORK                              
         MVC   AFFILS,WORK                                                      
GAFHKX   B     EXIT                                                             
         DROP  R5                                                               
***********************************************************************         
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*                                                                               
DEMLINE  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DEMLIN20             YES, DO STEREO LOGIC                        
         CLI   ACTN,SYSCODE                                                     
         BE    DEMLINE4                                                         
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINMNAM,X'FF'       IGNORE DUMMY E-O-F RECORDS                   
         BE    EXIT                                                             
         LA    R2,LINE1                                                         
         CLI   ACTN,STATION                                                     
         BNE   DEMLINE2                                                         
         MVC   LINE1+1(L'BINSTAT),BINSTAT                                       
         CLI   LINE1+5,C'T'        DON'T SHOW PARENT INDICATORS                 
         BNE   *+8                                                              
         MVI   LINE1+5,C' '                                                     
         CLI   BINBTYP,X'E0'                                                    
         BNE   *+8                                                              
         MVI   LINE1+6,C'#'                                                     
         MVC   LINE1+9(L'BINAFFL),BINAFFL                                       
         EDIT  (B2,BINRMKT),(4,LINE1+17)                                        
         MVC   LINE1+24(L'BINRALF),BINRALF                                      
         MVC   LINE1+31(L'BINMNAM),BINMNAM                                      
*&&DO                                                                           
         MVC   LINE1+60(L'BINBTYP),BINBTYP                                      
*&&                                                                             
         B     EXIT                                                             
*                                                                               
DEMLINE2 EDIT  (B2,BINRMKT),(4,LINE1+1)                                         
         LA    R2,LINE1+8                                                       
         MVC   0(L'BINRALF,R2),BINRALF                                          
         LA    R2,LINE1+15                                                      
         CLI   BINBTYP,X'E0'       XTRA SPILL STUFF                             
         BNE   *+12                                                             
         MVI   0(R2),C'#'                                                       
         LA    R2,1(R2)                                                         
         OC    BINKMKT,BINKMKT                                                  
         BNZ   DEMLINE3                                                         
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
DEMLINE3 MVC   0(L'BINMNAM,R2),BINMNAM                                          
*&&DO                                                                           
         LA    R2,L'BINMNAM+3(R2)                                               
         MVC   0(L'BINBTYP,R2),BINBTYP                                          
*&&                                                                             
         B     EXIT                                                             
*                                                                               
DEMLINE4 DS    0H                                                               
*** OUTPUT CODE FOR FUSION SYSCODE LISTING                                      
         L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINMSON,X'FF'       IGNORE DUMMY E-O-F RECORDS                   
         BE    EXIT                                                             
         LA    R2,LINE1                                                         
         MVC   LINE1+34(15),BINMSON                                             
         EDIT  (B2,BINSYSC),(4,LINE1),ALIGN=RIGHT,ZERO=NOBLANK                  
         MVC   LINE1+8(L'BINSYSN),BINSYSN                                       
         OC    BINSYSMK,BINSYSMK                                                
         BZ    *+10                                                             
         MVC   LINE1+55(L'BINSYSMK),BINSYSMK                                    
         MVC   LINE1+50(L'BINSSTAT-1),BINSSTAT                                  
         OC    BINSSTAT,BINSSTAT                                                
         BNZ   *+10                                                             
         MVC   LINE1+50(3),=C'N/A'                                              
*                                                                               
         MVC   LINE1+6(1),BINDMAW                                               
*                                                                               
         CLC   =C'N/A',BINSBASE                                                 
         BNE   DEMLIN10                                                         
         MVC   LINE1+59(3),=C'N/A'                                              
         CLC   =C'N/A',BINSUNV                                                  
         BNE   *+14                                                             
         MVC   LINE1+70(3),=C'N/A'                                              
         B     EXIT                                                             
DEMLIN10 EDIT  (B4,BINSBASE),(10,LINE1+59),ALIGN=LEFT,ZERO=BLANK                
         EDIT  (B4,BINSUNV),(10,LINE1+70),ALIGN=LEFT,ZERO=BLANK                 
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
DEMLIN20 DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
                                                                                
         L     R2,ASTIOBUF                                                      
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32?                              
         BO    DMLN200                                                          
*                                                                               
         CLI   TDRRTYP,TDRRTDRQ    DOES RECORD HAVE REQUESTED DATA?             
         BE    DEMLIN30                                                         
         CLI   TDRRTYP,TDRRTMSG    DOES RECORD HAVE MESSAGE INFO?               
         BE    DEMLIN25                                                         
         CLI   TDRRTYP,TDRRTHDR    DOES RECORD HAVE COLUMN HEADER INFO?         
         BE    DEMLIN60                                                         
         CLI   TDRRTYP,TDRRTINI    DEM16 SESSION DOESN'T NEED TO INIT           
         BE    DEMLINX                                                          
         CLI   TDRRTYP,TDRRTXFF                                                 
         BE    DEMLINX                                                          
         DROP  R5                                                               
         DC    H'0'                                                             
*                                                                               
DEMLIN25 DS    0H                  MESSAGE FOR LIST                             
         MVI   0(R2),C' '           PASS DUMMY MSG TO STEREO (THIS              
         LA    R2,1(R2)             ACTION HAS NO MSG)                          
         B     DEMLIN95                                                         
*                                                                               
DEMLIN30 DS    0H                  LIST REQUESTED DATA                          
         USING TSDEMRCD,R5                                                      
         CLI   ACTN,STATION                                                     
         BE    DEMLIN32                                                         
         CLI   ACTN,SPILLTO                                                     
         BE    DEMLIN34                                                         
         DC    H'0'                                                             
                                                                                
DEMLIN32 DS    0H                  OUTPUT FOR ACTN=STATION                      
         MVC   0(L'TDRSISTA,R2),TDRSISTA                                        
         CLI   L'TDRSISTA-1(R2),C'T'     DON'T SHOW PARENT INDICATORS           
         BNE   *+8                                                              
         MVI   L'TDRSISTA-1(R2),C' '                                            
         LR    R0,R2                      REMOVE ANY TRAILING BLANKS            
         LA    R2,L'TDRSISTA-1(R2)                                              
         BAS   RE,REMSPC                   AFTER STATION CALL LETTERS           
*&&DO                                                                           
         CLI   TDRSIBTP,X'E0'            EXTRA SPILL STUFF                      
         BNE   DEMLN32M                                                         
*&&                                                                             
         OC    TDRSIKMK,TDRSIKMK         FOR SPILL MARKETS                      
         BZ    DEMLN32G                                                         
         MVI   0(R2),C'/'                 SET TO PUT "/(MKT CODE)"              
         LA    R2,1(R2)                                                         
         OC    ALFMKTS,ALFMKTS            IF NO ALPHA MARKET,                   
         BZ    DEMLN32E                                                         
         CLC   ALFMKTS,SPACES                                                   
         BE    DEMLN32E                    PASS BACK NUMERIC MKT CODE           
         MVC   0(L'ALFMKTS,R2),ALFMKTS    ELSE, MOVE IN ALPHA MARKET            
         LR    R0,R2                                                            
         LA    R2,L'ALFMKTS-1(R2)                                               
         BAS   RE,REMSPC                   AND REMOVE TRAILING BLANKS           
         B     DEMLN32G                                                         
DEMLN32E EDIT  (B2,TDRSIKMK),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         AR    R2,R0                                                            
*&&DO                                                                           
DEMLN32G MVI   0(R2),C'#'                                                       
*&&                                                                             
DEMLN32G EQU   *                                                                
         CLI   TDRSIBTP,X'E0'            EXTRA SPILL STUFF                      
         BNE   DEMLN32M                                                         
         MVI   0(R2),C'#'                                                       
         LA    R2,1(R2)                                                         
DEMLN32M EQU   *                                                                
         BAS   RE,INSSEP                                                        
                                                                                
         MVC   0(L'TDRSIAFL,R2),TDRSIAFL                                        
         LR    R0,R2               IN CASE WE CALL REMSPC                       
         LA    R2,L'TDRSIAFL(R2)                                                
                                                                                
         CLC   TDRSIAFL,SPACES     IF AFFIL IS ALL BLANKS,                      
         BE    *+10                 LEAVE BLANKS THERE                          
         BCTR  R2,0                                                             
         BAS   RE,REMSPC                                                        
         BAS   RE,INSSEP                                                        
         EDIT  (B2,TDRSIRMK),(4,0(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
*&&DO                                                                           
         BAS   RE,INSSEP                                                        
                                                                                
         MVC   0(L'TDRSIRAM,R2),TDRSIRAM                                        
         LR    R0,R2               IN CASE WE CALL REMSPC                       
         LA    R2,L'TDRSIRAM(R2)                                                
         CLC   TDRSIRAM,SPACES     IF ALPHA MKT IS ALL BLANKS,                  
         BE    *+10                 LEAVE BLANKS THERE                          
         BCTR  R2,0                                                             
         BAS   RE,REMSPC                                                        
         BAS   RE,INSSEP                                                        
                                                                                
         MVC   0(L'TDRSIMNA,R2),TDRSIMNA                                        
         LR    R0,R2                                                            
         LA    R2,L'TDRSIMNA-1(R2)                                              
         BAS   RE,REMSPC           REMOVE TRAILING BLANKS                       
         BAS   RE,REPDLM           REPLACE DELIMITER                            
*&&                                                                             
         B     DEMLIN95                                                         
                                                                                
DEMLIN34 DS    0H                  OUTPUT FOR ACTN=SPILLTO                      
         EDIT  (B2,TDRSORMK),(4,0(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
         BAS   RE,INSSEP                                                        
                                                                                
         MVC   0(L'TDRSORAM,R2),TDRSORAM                                        
         LR    R0,R2               IN CASE WE CALL REMSPC                       
         LA    R2,L'TDRSORAM(R2)                                                
         CLC   TDRSORAM,SPACES     IF ALPHA MKT IS ALL BLANKS,                  
         BE    *+10                 LEAVE BLANKS THERE                          
         BCTR  R2,0                                                             
         BAS   RE,REMSPC                                                        
         BAS   RE,INSSEP                                                        
                                                                                
         CLI   TDRSOBTP,X'E0'                                                   
         BNE   *+12                                                             
         MVI   0(R2),C'#'                                                       
         LA    R2,1(R2)                                                         
         OC    TDRSOKMK,TDRSOKMK                                                
         BNZ   *+12                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(L'TDRSOMNA,R2),TDRSOMNA                                        
         LR    R0,R2                                                            
         LA    R2,L'TDRSOMNA-1(R2)                                              
         BAS   RE,REMSPC           REMOVE TRAILING BLANKS                       
         BAS   RE,REPDLM           REPLACE DELIMITER                            
         B     DEMLIN95                                                         
         DROP  R5                                                               
*                                                                               
DEMLIN60 DS    0H                  COLUMN HEADERS                               
         USING TSDEMRCD,R5                                                      
         MVC   WORK,SPACES                                                      
         CLI   ACTN,STATION                                                     
         BE    DEMLIN63                                                         
         CLI   ACTN,SPILLTO                                                     
         BE    DEMLIN66                                                         
         DC    H'0'                                                             
                                                                                
DEMLIN63 DS    0H                   COLUMN HEADERS FOR STATION LIST             
         MVC   WORK(L'HEADINS1),HEADINS1                                        
         CLI   TDRHDLIN,1                                                       
         BE    *+10                                                             
         MVC   WORK(L'HEADINS2),HEADINS2                                        
                                                                                
         MVI   WORK+07,STSPOLST    FUDGE IN THE SEPARATORS                      
         MVI   WORK+14,STSPOLST                                                 
         MVI   WORK+22,STSPOLST                                                 
         MVI   WORK+29,STSPOLST                                                 
                                                                                
         LA    R1,WORK             REMOVE DASHES                                
         LA    R0,L'HEADINS1                                                    
         CLI   0(R1),C'-'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
                                                                                
         LA    R1,L'HEADINS1-1     SET LENGTH FOR MOVE TO BUFFER                
         CLI   TDRHDLIN,1                                                       
         BE    *+8                                                              
         LA    R1,(L'HEADINS2-11)-1                                             
                                                                                
         B     DEMLIN68                                                         
                                                                                
DEMLIN66 DS    0H                   COLUMN HEADERS FOR SPILL MKT LIST           
         MVC   WORK(L'HEADOUT1),HEADOUT1                                        
         CLI   TDRHDLIN,1                                                       
         BE    *+10                                                             
         MVC   WORK(L'HEADOUT2),HEADOUT2                                        
                                                                                
         MVI   WORK+06,STSPOLST    FUDGE IN THE SEPARATORS                      
         MVI   WORK+12,STSPOLST                                                 
                                                                                
         LA    R1,WORK             REPLACE EQUAL SIGN W/ DASH                   
         LA    R0,L'HEADOUT1                                                    
         CLI   0(R1),C'='                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
                                                                                
         LA    R1,L'HEADOUT1-1     SET LENGTH FOR MOVE TO BUFFER                
         CLI   TDRHDLIN,1                                                       
         BE    *+8                                                              
         LA    R1,(L'HEADOUT2-11)-1                                             
                                                                                
         B     DEMLIN68                                                         
                                                                                
DEMLIN68 DS    0H                                                               
         EXMVC R1,0(R2),WORK                                                    
         LA    R2,1(R2,R1)                                                      
         B     DEMLIN95                                                         
         DROP  R5                                                               
*                                                                               
DEMLIN95 DS    0H                                                               
         MVI   0(R2),STSPIKEY      MOVE IN "END OF KEY" SEPARATOR               
         LA    R2,1(R2)                                                         
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
         DROP  R4                                                               
                                                                                
                                                                                
* Little routine to insert field separator into output buffer.                  
*  R2-->next output location in the output buffer.                              
                                                                                
INSSEP   DS    0H                                                               
         MVI   0(R2),STSPOLST      LIST ACTNS USE SEMICOLONS                    
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to remove trailing blanks.  R0=start & R2=end of fld.          
*  R2-->next output location in the output buffer.                              
                                                                                
REMSPC   DS    0H                                                               
         CR    R0,R2                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,REMSPC                                                        
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to replace STSPOLST w/ ','.  R0=start & R2=end of fld.         
*  R1 & RF are clobbered in this routine.                                       
                                                                                
REPDLM   DS    0H                  REPLACE DELIMITER                            
         LR    R1,R0                                                            
         LR    RF,R2                                                            
         SR    RF,R1                                                            
REPDLM10 CLI   0(R1),STSPOLST                                                   
         BNE   *+8                                                              
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BCT   RF,REPDLM10                                                      
         BR    RE                                                               
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (APMODE=APMBREAK)+        
               '                                                                
***********************************************************************         
*========================== APMODE=APMBREAK ==========================*         
                                                                                
DEMBREAK DS    0H                                                               
         DS    0H                  SEE WHO CALLED FOR A BREAK                   
         TM    BRKFLAG1,BF1FALNK    FALINK?                                     
         BO    DMBRKF                YEP                                        
         B     DMBRKX                                                           
                                                                                
*                                                                               
DMBRKF   DS    0H                                                               
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         MVC   AS2EMPC,FALEMPC                                                  
         MVC   AS2DMPC,FALDMPC                                                  
         MVC   AS2STMOD,STMODE                                                  
         DROP  RF                                                               
                                                                                
*                                                                               
DMBRKX   DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (APMODE=APMRESUM)+        
               '                                                                
***********************************************************************         
*========================== APMODE=APMRESUM ==========================*         
                                                                                
DEMRESUM DS    0H                                                               
         DS    0H                  SEE WHO CALLED FOR A RESUME                  
         TM    RSMFLAG1,RF1FALNK    FALINK?                                     
         BO    DMRSMF                YEP                                        
         B     DMRSMX                                                           
                                                                                
*                                                                               
** RESUME CALLED BY FALINK **                                                   
*                                                                               
DMRSMF   DS    0H                                                               
         L     R6,AAPSAV2                                                       
         USING APSAV2D,R6                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   AS2STMOD,STMOUTQ                                                 
         BE    DMRSMFO                                                          
         B     DMRSMFX                                                          
                                                                                
*                                                                               
*** STMODE=STMOUTQ WHEN BREAK CALLED BY FALINK ***                              
*                                                                               
DMRSMFO  DS    0H                                                               
         OI    AS2RSMF,AS2RFFLK                                                 
         DROP  R6                                                               
                                                                                
*                                                                               
DMRSMFX  DS    0H                                                               
         B     DMRSMX                                                           
                                                                                
*                                                                               
DMRSMX   DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS'                          
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
         DS    0H                                                               
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
*                                                                               
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
*                                                                               
         DS    0H                                                               
         L     RE,=A(SUBRTNTB-DEM03)                                            
         LA    RE,DEM03(RE)                                                     
                                                                                
GOSUB22  DS    0H                                                               
         CLI   0(RE),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   GOSUBN,0(RE)                                                     
         BL    *+12                                                             
         LA    RE,L'SUBRTNTB(RE)                                                
         B     GOSUB22                                                          
*                                                                               
         ZICM  RF,1(RE),(3)                                                     
         LA    RF,DEM03(RF)                                                     
                                                                                
*                                                                               
GOSUBGO  DS    0H                                                               
         ST    RC,ALOCWRK                                                       
         SR    R1,R1                                                            
         IC    R1,GOSUBN                                                        
         GOTO1 (RF),(R1)                                                        
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ GETEL MACRO ============================*         
         GETEL R3,MYDATDSP,MYELCODE                                             
                                                                                
***********************************************************************         
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HEADOUT1 DC    C'MARKET  ALPHA  MARKET NAME (*=HOME MARKET) (#=XTRA SPI+        
               LL)'                                                             
HEADOUT2 DC    C'NUMBER   MKT   -----------'                                    
HEADINS1 DC    C'STATION  AFFIL  MARKET  ALPHA  MARKET NAME'                    
HEADINS2 DC    C'-------  -----  NUMBER   MKT   -----------'                    
         SPACE 1                                                                
*EADSYS1 DC    C'MSO NAME        SYSCODE  SYSCODE NAME              ALP         
*              HA  SUB-BASE   UNIVERSE'                                         
*EADSYS2 DC    C'--------        -------  ------------               MK         
*              T   --------   --------'                                         
HEADSYS1 DC    C'SYSCODE SYSCODE NAME              MSO NAME        STAT+        
                MKT SUB-BASE   UNIVERSE'                                        
HEADSYS2 DC    C'------- ------------              --------        ----+        
                --- --------   --------'                                        
HEADSYS3 DC    C'SYSCODE SYSCODE NAME              MSO NAME        STAT+        
                MKT   AIUE    CARR UNIV'                                        
HEADSYS4 DC    C'------- ------------              --------        ----+        
                --- --------  ---------'                                        
         SPACE 1                                                                
BTCUTOFF DC    X'5A07'             KILL BOOKTYPE PRIOR TO JUL90                 
**BKCUTOFF DC    X'5E0A'             KILL BOOK PRIOR TO OCT94                   
**BKCUTOFF DC    X'630A'             CUTOFF  BOOK PRIOR TO OCT99                
OVCUTOFF DC    X'6832'             OVERNIGHT AFTER DEC0604                      
*                                                                               
R00#     EQU   1                                                                
         DROP  R7,R8,R9,RB,RC                                                   
         EJECT                                                                  
***********************************************************************         
*=============SUBROUTINE POOL ==========================                        
* AT ENTRY ,                                                                    
* R9 --->DEMWRKD                                                                
* R8 ---> TWA                                                                   
* R7 --->DEMTMPD                                                                
* R1 ---> EQUATED SUBROUTINE NUMBER                                             
SUBR01Q  EQU   (((*-DEM03+4095)/4096)*4096)                                     
         ORG   DEM03+SUBR01Q                                                    
SUBR01   NMOD1 0,**0301**                                                       
         USING DEMWRKD,R9                                                       
         USING DEMTWAD,R8                                                       
         USING DEMTMPD,R7                                                       
*                                                                               
         L     RC,ALOCWRK                                                       
         USING DM03WRKD,RC                                                      
*                                                                               
         AHI   R1,-(R00#)                                                       
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
*                                                                               
D32DL#   EQU   (R01_01-*)/4+R00#      DEMLINE PROCESSING FOR DEM32              
D32BKST# EQU   (R01_02-*)/4+R00#      DOWNLOAD BOOKS FOR STATION                
D32MKST# EQU   (R01_05-*)/4+R00#      DOWNLOAD MARKETS FOR STATION              
D32PST#  EQU   (R01_06-R01_00)/4+R00# POSTING FOR DEM32                         
**D32SYSC# EQU   (R01_07-R01_00)/4+R00# SYSCODE LISTING DOWNLOAD                
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R01_02   B     D32BKST             DOWNLOAD BOOKS FOR STATION                   
R01_05   B     D32MKST             DOWNLOAD MARKETS FOR STATION                 
R01_06   B     D32POST             POSTING FOR DEM32                            
**R01_07   B     D32SYSCD                 DOWNLOAD SYSCODE                      
R01#     EQU   (*-R01_00)/4+R00#                                                
         DC    H'0'                                                             
YES_STE  SR    R9,R9                                                            
NO_STE   LTR   R9,R9                                                            
XIT_STE  XIT1                                                                   
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--D32DL#)'         
*-------------------- DOWNLOAD ROUTINES FOR DEM32 --------------------*         
D32DEMLN  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   PUFLLOO,0           INITIALIZE "LAST LEFT OFF" FLAGS             
         MVI   PUFLLOO2,0                                                       
                                                                                
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         TM    AS2RSMF,AS2RFFLK    IF FALINK RESUME,                            
         BZ    D32DL019                                                         
         MVI   PUFLLOO,C'N'         NEED TO RE-ESTABLISH MAP CODES              
         MVI   PUFLLOO2,C'N'         "    "       "      MTH/WK #               
D32DL019 EQU   *                                                                
         DROP  RF                                                               
                                                                                
*                                                                               
         DS    0H                                                               
          MVC   TMPRTYP,TDRRTYP                                                 
          CLI   TDRRTYP,TDRRTDRQ      DOES  REC HAVE REQUESTED DATA?            
          BE    D32DL100                                                        
***       CLI   TDRRTYP,TDRRSYSQ      SYSCODE DATA?                             
***       BE    D32DL100                                                        
          CLI   TDRRTYP,TDRRTINI   THIS GIVES US A CHANCE TO DO INIT            
          BE    D32DL200                                                        
          B     D32DLX                                                          
                                                                                
*                                                                               
D32DL100  DS    0H                                                              
          XC    MARNAME,MARNAME                                                 
          XC    MARRALF,MARRALF                                                 
*                                                                               
          LA    RF,DBLOCK1                                                      
          USING DBLOCKD,RF                                                      
          XC    DBLOCK,DBLOCK                                                   
          MVC   DBAREC,AIOAREA                                                  
          MVC   DBCOMFCS,AFAC                                                   
          MVC   DBFILE,DBFIL                                                    
          MVI   DBFUNCT,DBGETMK                                                 
          MVC   DBSELMED,DBMED                                                  
          MVC   DBSELSRC,DBSRC                                                  
          MVC   DBSELSTA,STACALL    SET SPILL STATION                           
          DROP  RF                                                              
                                                                                
*                                                                               
          CLI   ACTN,STATION                                                    
          BE    D32STAT                                                         
          CLI   ACTN,SPILLTO                                                    
          BE    D32SPILL                                                        
          CLI   ACTN,SYSCODE                                                    
          BE    D32SYSCDE                                                       
          DC    H'0'                                                            
*                                                                               
D32STAT   MVI   GOSUBN,D32BKST#                                                 
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
D32SPILL  MVI   GOSUBN,D32MKST#                                                 
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
D32SYSCDE MVI   GOSUBN,D32SYSC#                                                 
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
          EJECT                                                                 
D32DL200  DS    0H                                                              
          MVI   GOSUBN,CIOB#                                                    
          GOTO1 AGOSUB                                                          
                                                                                
*                                                                               
D32DLX    B     XIT_STE                                                         
                                                                                
                                                                                
D32DLOKD NTR1                                                                   
         MVI   GOSUBN,OKDL#                                                     
         GOTO1 AGOSUB                                                           
         J     EXIT                                                             
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--D32BKST#+        
               )'                                                               
*----------- DEM32 DOWNLOAD BOOKS FOR STATIONS IN A MARKET -----------*         
                                                                                
D32BKST   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
                                                                                
*                                                                               
          DS    0H                 CHECK EXTRACT VERSIONS                       
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   D32BS100                                                        
          CLC   D32PCVER,=AL4(XTRCVER2)                                         
          BNL   D32BS100                                                        
          CLC   D32PCVER,=AL4(XTRCVER1)                                         
          BNL   D32BS010                                                        
          J     EXITL                                                           
                                                                                
*                                                                               
D32BS010  DS    0H                                                              
          LR    R3,R5                                                           
          MVC   MYDATDSP,=Y(TDRI2KYL)                                           
          MVI   MYELCODE,TDREMWNQ                                               
          BRAS  RE,GETEL                                                        
          BE    *+6                                                             
          DC    H'0'                                                            
          ZIC   R4,1(R3)            R3-->MTH/WK # ELEMENT                       
          SH    R4,=H'2'            R4 = LOOP COUNTER                           
          BP    *+6                                                             
          DC    H'0'                                                            
          LA    R6,2(R3)            R6-->MTH/WK #s                              
                                                                                
          CLI   NBKS,0              IF AT LEAST ONE BOOK GIVEN,                 
          BE    *+8                                                             
          LA    R4,1                 SET UP TO NOT PASS BOOKS DOWN              
                                                                                
*                                                                               
D32BKST12 DS    0H                                                              
          CLI  PUFLLOO2,C'N'       RE-ESTABLISHED MTH/WK # YET?                 
          BNE  D32BS012G            EITHER "YES" OR "N/A"                       
          L    RF,AAPSAV2                                                       
          CLC  (AS2MWN-APSAV2D)(,RF),0(R6)                                      
          BNE  D32BSBUMP                                                        
          MVI  PUFLLOO2,C'Y'                                                    
D32BS012G EQU  *                                                                
*                                                                               
          MVC   FALEMPC,=Y(FMHBKST)                                             
                                                                                
          CLI  PUFLLOO,C'N'        IS "P/U FROM LAST LEFT OFF" NOT OK?          
          BNE  D32BS012K                                                        
          L    RF,AAPSAV2                                                       
          USING APSAV2D,RF                                                      
          CLC  AS2EMPC,FALEMPC      TRY TO RESUME FROM LAST OFF?                
          BE   *+6                                                              
          DC   H'0'                                                             
          DROP RF                                                               
          B    D32BS012M                                                        
D32BS012K EQU  *                                                                
                                                                                
          SR    R1,R1                                                           
          GOTO1 ADM32SET            SETELEM FOR EVERY BOOK                      
D32BS012M EQU  *                                                                
                                                                                
*                                                                               
          DS    0H                          STATION CODE                        
          MVC   FALDMPC,=Y(FMDBKSTSTA)                                          
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS040X                                                       
                                                                                
          MVC   STACALL,TDRI2STA                                                
          MVC   KMARKET,TDRI2KMK                                                
          MVC   KBTYP,TDRI2BTP                                                  
          MVI   GOSUBN,FSTN#       FORMAT STATION INTO  WORK2                   
          GOTO1 AGOSUB                                                          
                                                                                
          DS    0H                                                              
          LA    R0,WORK2                                                        
          LH    R1,HALF                                                         
          BAS   RE,D32BSAD                                                      
D32BS040X EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                          BOOK                                
          CLI   NBKS,0                       IF BOOK(S) IN REQUEST,             
          BNE   D32BS045X                     DON'T PASS IT (THEM) BACK         
                                                                                
          LA    R2,WORK                                                         
          MVC   FALDMPC,=Y(FMDBKSTBOOK)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS045X                                                       
                                                                                
          MVC   DUB+0(1),TDRI2BKY            YEAR                               
          MVC   DUB+1(1),0(R6)               MONTH/WEEK NUMBER                  
          XC    DUB+0(2),=X'FFFF'                                               
          MVI   GOSUBN,TBK#                                                     
          GOTO1 AGOSUB                                                          
          LA    R0,WORK                                                         
          LH    R1,HALF                                                         
          BAS   RE,D32BSAD                                                      
D32BS045X EQU   *                                                               
*                                                                               
          DS    0H                          BOOKTYPE                            
          CLI   NBKS,0                       IF BOOK(S) IN REQUEST,             
          BNE   D32BS050X                     DON'T PASS BOOKTYPE BACK          
                                                                                
          LA    R2,WORK                                                         
          MVC   FALDMPC,=Y(FMDBKSTBTYP)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS050X                                                       
                                                                                
          MVC   WORK(1),TDRI2BTP                                                
          CLI   WORK,X'E0'                   IF XTRA SPILL,                     
          BNE   *+8                                                             
          MVI   WORK,0                        KILL BOOKTYPE                     
                                                                                
                                                                                
          LA    R0,WORK                                                         
          LA    R1,1                                                            
          BAS   RE,D32BSAD                                                      
D32BS050X EQU   *                                                               
                                                                                
*                                           AFFILIATION                         
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTAFF)                                          
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS065X                                                       
                                                                                
          MVC   WORK,SPACES                                                     
          LR    R3,R5                                                           
          MVI   MYELCODE,TDREAFFQ                                               
          BRAS  RE,GETEL                                                        
          BE    *+6                                                             
          DC    H'0'                                                            
          ZIC   R1,1(R3)                    R3-->AFFILIATIONS ELEMENT           
          SH    R1,=Y(2+1)                                                      
          EXMVC R1,WORK,2(R3)                                                   
          LA    R0,WORK                                                         
          LA    R1,1(R1)                                                        
          BAS   RE,D32BSAD                                                      
D32BS065X EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                 FLAGS                                        
          MVC   FALDMPC,=Y(FMDBKSTFLG1)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS079X                                                       
                                                                                
          MVI   WORK2,0             WORK2(1) WILL CONTAIN FLAGS                 
*                                                                               
          DS    0H                  SPILL STATION?                              
          MVC   FALDMPC,=Y(FMDBKSTSPLL)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS071X                                                       
                                                                                
          CLI   DBSRC,C'F'            FUSION ALSO DONT FLAG AS                  
          BE    D32BS071X             SPILL STATION                             
          CLI   DBSRC,C'N'           IF NIELSEN,                                
          BNE   D32BS071M                                                       
          CLI   DBMED,C'T'            USTV,                                     
          BNE   D32BS071M                                                       
          CLI   TDRI2BTP,BOOKTYPE_C3  LIVE+3 DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_LS  LIVE+SD CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_WS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'         LIVE ONLY WIRED                           
          BE    *+8                                                             
          CLI   TDRI2BTP,C'U'         LIVE ONLY DMA                             
          BE    *+8                                                             
          CLI   TDRI2BTP,C'3'         ZERO CELL CABLE                           
          BE    *+8                                                             
          CLI   TDRI2BTP,C'4'         ZERO CELL WIRED CABLE                     
          BE    *+8                                                             
          CLI   TDRI2BTP,C'W'         WIRED CABLE                               
          BE    *+8                                                             
          CLI   TDRI2BTP,C'C'         AND BOOKTYPE IS "CABLE",                  
          BNE   D32BS071M                                                       
          B     D32BS071X             DON'T FLAG AS SPILL STATION               
D32BS071M EQU   *                                                               
          OC    TDRI2KMK,TDRI2KMK   SPILL STATION?                              
          BZ    *+8                                                             
          OI    WORK2,FMDBKSTF1SP                                               
D32BS071X EQU   *                                                               
*                                                                               
          DS    0H                  PUBLIC STATION?                             
          MVC   FALDMPC,=Y(FMDBKSTPUBL)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS072X                                                       
                                                                                
          CLC   =C'P ',2(R3)         R3-->AFFILIATIONS ELEMENT                  
          BNE   *+8                                                             
          OI    WORK2,FMDBKSTF1PU                                               
D32BS072X EQU   *                                                               
*                                                                               
          DS    0H                  CABLE STATION?                              
          MVC   FALDMPC,=Y(FMDBKSTCABL)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS073X                                                       
                                                                                
          CLI   DBSRC,C'F'         FUSION ALWAYS FLAG AS CABLE                  
          BE    *+8                                                             
          CLI   TDRI2BTP,C'4'      ZERO CELL WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'3'      ZERO CELL CABLE                              
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_C3  LIVE+3 DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_LS  LIVE+SD CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_WS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'      LIVE ONLY WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'U'      LIVE ONLY DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,C'W'      WIRED                                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'C'                                                   
          BNE   D32BS073X                                                       
          OC    TDRI2KMK,TDRI2KMK   SPILL STATION?                              
          BZ    *+8                                                             
          OI    WORK2,FMDBKSTF1CA   ONLY IF SPILL FLAG AS CABLE                 
D32BS073X EQU   *                                                               
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTFLG1)                                         
          LA    R0,WORK2                                                        
          SR    R1,R1                                                           
          BAS   RE,D32BSAD                                                      
D32BS079X EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                                                              
          CLI   PUFLLOO,C'N'       OKAY FROM LAST LEFT OFF?                     
          BNE   *+6                 EITHER "YES" OR "N/A"                       
          DC    H'0'                                                            
*                                                                               
D32BSBUMP DS    0H                                                              
          LA    R6,1(R6)            BUMP TO NEXT MTH/WK #                       
          BCT   R4,D32BKST12                                                    
                                                                                
*                                                                               
          DS    0H                                                              
          B     D32BSX                                                          
          EJECT                                                                 
D32BS100  DS    0H                 USING EXTRACT VERSION  XTRCVER2              
          MVC   FALEMPC,=Y(FMHBKST)                                             
          CLI   STPRVOVL+(SPOVSETE-SPVOVALD),C'Y'                               
          BE    D32BS105X                                                       
                                                                                
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
          MVI   STPRVOVL+(SPOVSETE-SPVOVALD),C'Y'                               
D32BS105X EQU   *                                                               
                                                                                
*                                                                               
** DELIMITERS **                                                                
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTDLMT)     DELIMITERS                          
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS109                                                        
*                                                                               
          DS    0H                                                              
          LA    RF,STPRVOVL                                                     
          USING SPVOVALD,RF                                                     
          CLI   SPOVDLMT+0,STSPIKEY                                             
          BE    D32BS109                                                        
          MVI   SPOVDLMT+0,STSPIKEY                                             
          DROP  RF                                                              
                                                                                
          LA    R0,STPRVOVL+(SPOVDLMT-SPVOVALD)                                 
          LA    R1,L'SPOVDLMT                                                   
          BAS   RE,D32BSAD                                                      
D32BS109  EQU   *                                                               
          MVC   MYD32DLM,STPRVOVL+(SPOVDLMT-SPVOVALD)                           
                                                                                
*                                                                               
** "GROUP" #1 DATA **                                                           
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTGRP1)     GROUP #1 DATA                       
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS129                                                        
                                                                                
*                                                                               
          DS    0H                                                              
SPSB      USING TSDEMRCD,(STPRVOVL+(SPOVSTBT-SPVOVALD))                         
          CLC   SPSB.TDRKEY(L'SPOVSTBT),TDRKEY  DID STTN/KMK/BTYP CHNG?         
          BE    D32BS129                         NO, DON'T REPEAT GRP#1         
*                                                                               
          DS    0H                 DOWNLOAD GROUP #2 STUFF IN I/O BUFF          
          ZICM  R1,IODATALN,(3)     GET L(DATA IN I/O BUFFER)                   
          BZ    D32BS113X           DON'T DOWNLOAD IF NO DATA                   
                                                                                
          MVC   FALDMPC,=Y(FMDBKSTGP2A)     GROUP #2 DATA (W/ DEM32 BK)         
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   *+10                                                            
          MVC   FALDMPC,=Y(FMDBKSTGRP2)     GROUP #2 DATA                       
          L     R0,ASTIOBUF                                                     
          BAS   RE,D32BSAD                                                      
                                                                                
          MVI   GOSUBN,CIOB#       CLEAR STEREO I/O BUFFER                      
          GOTO1 AGOSUB                                                          
D32BS113X EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                                                              
          CLI   TDRI2RMK,XFF       DID WE REACH "LAST" STATION?                 
          BE    D32BSX              YEP, NO MORE STTNS TO DOWNLOAD              
*                                                                               
          CLC   SPSB.TDRI2RMK,TDRI2RMK     DID RTG SVCE MRKT CHANGE?            
          BE    *+14                        NO                                  
          MVC   SPSB.TDRKEY(L'SPOVSTBT),TDRKEY                                  
          B     D32BS120                                                        
                                                                                
          CLC   SPSB.TDRI2STA,TDRI2STA     DID STATION CHANGE?                  
          BE    *+14                        NO                                  
          MVC   SPSB.TDRKEY(L'SPOVSTBT),TDRKEY                                  
          B     D32BS120                                                        
                                                                                
          CLC   SPSB.TDRI2KMK,TDRI2KMK     DID SPILL MARKET CHANGE?             
          BE    *+14                        NO                                  
          MVC   SPSB.TDRKEY(L'SPOVSTBT),TDRKEY                                  
          B     D32BS120                                                        
                                                                                
          CLC   SPSB.TDRI2BTP,TDRI2BTP     DID BOOKTYPE CHANGE?                 
          BE    *+14                        NO                                  
          MVC   SPSB.TDRKEY(L'SPOVSTBT),TDRKEY                                  
          B     D32BS130                                                        
                                                                                
          DC    H'0'                                                            
          DROP  SPSB                                                            
                                                                                
*                                                                               
D32BS120  DS    0H                                                              
          L     R2,ASTIOBUF        USE  R2  AS OUTPUT AREA POINTER              
                                                                                
*                                                                               
          DS    0H                                                              
          MVC   STACALL,TDRI2STA                                                
          MVC   KMARKET,TDRI2KMK                                                
          MVC   KBTYP,TDRI2BTP                                                  
          MVI   GOSUBN,FSTN#       FORMAT STATION INTO  WORK2                   
          GOTO1 AGOSUB                                                          
                                                                                
          DS    0H                                                              
          LR    R0,R2                                                           
          LH    R1,HALF                                                         
          LA    RE,WORK2                                                        
          LR    RF,R1                                                           
          MVCL  R0,RE              MOVE FORMATTED STATION TO OUTPUT             
          LR    R2,R0              R2-->NEXT OUTPUT AREA                        
*                                                                               
          MVC   0(1,R2),MYD32DLM                                                
          AHI   R2,1                                                            
                                                                                
*                                           AFFILIATION                         
          DS    0H                                                              
          LR    R3,R5                                                           
          MVI   MYELCODE,TDREAFFQ                                               
          BRAS  RE,GETEL                                                        
          BE    *+6                                                             
          DC    H'0'                                                            
          ZIC   R1,1(R3)                    R3-->AFFILIATIONS ELEMENT           
          AHI   R1,-(2+1)                                                       
          EXMVC R1,0(R2),2(R3)                                                  
          LA    R2,1(R2,R1)                                                     
*                                                                               
D32BS123  EQU  *                                                                
          MVC   0(1,R2),MYD32DLM                                                
          AHI   R2,1                                                            
                                                                                
*                                                                               
          DS    0H                 FLAGS                                        
          DS    0H                  SPILL STATION?                              
          MVI   WORK2+0,C'N'                                                    
*                                                                               
D32BS124  CLI   DBSRC,C'F'           FUSION DONT FLAG AS SPILL                  
          BE    D32BS126X                                                       
          CLI   DBSRC,C'N'           IF NIELSEN,                                
          BNE   D32BS126M                                                       
          CLI   DBMED,C'T'            USTV,                                     
          BNE   D32BS126M                                                       
          CLI   TDRI2BTP,C'4'         ZERO CELL WIRED                           
          BE    *+8                                                             
          CLI   TDRI2BTP,C'3'         ZERO CELL CABLE                           
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_C3  LIVE+3 DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_LS  LIVE+SD CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_WS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'      LIVE ONLY WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'U'      LIVE ONLY DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,C'W'         AND BOOKTYPE IS "WIRED CABLE",            
          BE    *+8                                                             
          CLI   TDRI2BTP,C'C'         AND BOOKTYPE IS "CABLE",                  
          BNE   D32BS126M                                                       
          B     D32BS126X             DON'T FLAG AS SPILL STATION               
D32BS126M EQU   *                                                               
          OC    TDRI2KMK,TDRI2KMK   SPILL STATION?                              
          BZ    D32BS126X                                                       
*&&DO                                                                           
          CLI   DBMED,C'C'          CANADA CHECK FOR A FEW EXCEPTIONS           
          BNE   D32BS126O                                                       
*                                                                               
          OC    CDEMTABS,CDEMTABS                                               
          BNZ   D32BS126N                                                       
          LHI   R0,QDEMTABS                                                     
          ICM   R0,B'1110',=X'D9000A'                                           
          GOTO1 VCALLOV,DMCB,0,(R0)                                             
          MVC   CDEMTABS,0(R1)                                                  
*                                                                               
D32BS126N GOTO1 CDEMTABS,DMCB,HOMESTA                                           
          ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                     
          BNZ   *+6                                                             
          DC    H'0'                BAD TABLEID PASSED                          
          L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                
          USING HOMESTAD,RE                                                     
*                                                                               
HOME10    CLC   =X'FFFF',0(RE)                                                  
          BE    D32BS126O                                                       
          CLC   HOMEMKT,TDRI2KMK                                                
          BNE   HOME20                                                          
          CLC   HOMECALL(4),TDRI2STA                                            
          BE    D32BS126X                                                       
HOME20    AHI   RE,HOMESTAQ                                                     
          B     HOME10                                                          
*&&                                                                             
D32BS126O MVI   WORK2+0,C'Y'                                                    
D32BS126X EQU   *                                                               
                                                                                
          DS    0H                  PUBLIC STATION?                             
          MVI   WORK2+1,C'N'                                                    
          CLC   =C'P ',2(R3)         R3-->AFFILIATIONS ELEMENT                  
          BNE   *+8                                                             
          MVI   WORK2+1,C'Y'                                                    
                                                                                
          DS    0H                  CABLE STATION?                              
          MVI   WORK2+2,C'N'                                                    
*                                                                               
          CLI   DBMED,C'O'                                                      
          BE    *+8                                                             
          CLI   DBMED,C'T'                                                      
          BNE   D32BS127                                                        
          CLI   DBSRC,C'N'                                                      
          BNE   D32BS127                                                        
          CLI   TDRI2BTP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_C3  LIVE+3 DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_LS  LIVE+SD CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_WS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HT  HISPANIC TEST                             
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'      LIVE ONLY WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'U'      LIVE ONLY DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,C'W'                                                   
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'                                                   
          BE    *+8                                                             
          CLI   TDRI2BTP,C'B'       BLACK HAS CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,C'J'       LIVE ONLY HISPANIC HAS CABLE                
          BE    *+12                                                            
          CLI   TDRI2BTP,C'H'       HISPANIC HAS CABLE                          
          BNE   D32BS127                                                        
          OC    TDRI2KMK,TDRI2KMK                                               
          BZ    D32BS127                                                        
          MVI   WORK2+2,C'Y'        HISPANIC SPILL = CABLE                      
          MVI   WORK2,C'N'                                                      
*                                                                               
D32BS127  CLI   DBSRC,C'F'          FUSION ALWAYS FLAG AS CABLE                 
          BE    *+8                                                             
          CLI   TDRI2BTP,C'C'                                                   
          BNE   *+8                                                             
          MVI   WORK2+2,C'Y'                                                    
*                                                                               
          CLI   DBMED,C'T'          WIRED CABLE FLAG AS CABLE                   
          BNE   D32BS128                                                        
          CLI   TDRI2BTP,C'4'                                                   
          BE    *+8                                                             
          CLI   TDRI2BTP,C'3'                                                   
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_W3  LIVE+3 WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_C3  LIVE+3 DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_LS  LIVE+SD CABLE                             
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_WS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,BOOKTYPE_HS  LIVE+SD WIRED CABLE                       
          BE    *+8                                                             
          CLI   TDRI2BTP,C'Z'      LIVE ONLY WIRED CABLE                        
          BE    *+8                                                             
          CLI   TDRI2BTP,C'U'      LIVE ONLY DMA CABLE                          
          BE    *+8                                                             
          CLI   TDRI2BTP,C'W'                                                   
          BNE   D32BS128                                                        
                                                                                
          OC    TDRI2KMK,TDRI2KMK                                               
          BZ    *+8                                                             
          MVI   WORK2+2,C'Y'                                                    
D32BS128  EQU  *                                                                
*                                                                               
          MVC   0(3,R2),WORK2+0                                                 
          AHI   R2,3                                                            
                                                                                
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTGRP1)     GROUP #1 DATA                       
          LR    R1,R2                                                           
          L     R0,ASTIOBUF                                                     
          SR    R1,R0                                                           
          BAS   RE,D32BSAD                                                      
                                                                                
          MVI   GOSUBN,CIOB#       CLEAR STEREO I/O BUFFER                      
          GOTO1 AGOSUB                                                          
D32BS129  EQU   *                                                               
                                                                                
*                                                                               
** "GROUP #2" DATA **                                                           
*                                                                               
D32BS130  DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBKSTGP2A)     GROUP #2 DATA (W/ DEM32 BK)         
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   *+10                                                            
          MVC   FALDMPC,=Y(FMDBKSTGRP2)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BS159                                                        
*                                                                               
          CLI   NBKS,0                       IF BOOK(S) IN REQUEST,             
          BNE   D32BS159                      DON'T PASS GROUP #2 BACK          
* NO BOOK LIST FOR LATEST VERSION OF DEM32                                      
          CLC   D32PCVER,=AL4(XTRCVERA)                                         
          BH    D32BS159                                                        
*                                                                               
                                                                                
*                                                                               
*** BUILD "GROUP #2" DATA ***                                                   
*                                                                               
          DS    0H                 SET UP BOOK POINTER & COUNTER                
          LR    R3,R5                                                           
          MVC   MYDATDSP,=Y(TDRI2KYL)                                           
          MVI   MYELCODE,TDREMWNQ                                               
          BRAS  RE,GETEL                                                        
          BE    *+6                                                             
          DC    H'0'                                                            
          ZIC   R4,1(R3)            R3-->MTH/WK # ELEMENT                       
          SH    R4,=H'2'            R4 = LOOP COUNTER                           
          BP    *+6                                                             
          DC    H'0'                                                            
          LA    R6,2(R3)            R6-->MTH/WK #s                              
                                                                                
          CLI   NBKS,0              IF AT LEAST ONE BOOK GIVEN,                 
          BE    *+8                                                             
          LA    R4,1                 SET UP TO NOT PASS BOOKS DOWN              
                                                                                
*                                                                               
D32BS139  DS    0H                  DOWNLOAD BOOKTYPE                           
          L     R2,ASTIOBUF                                                     
          ZICM  R0,IODATALN,(3)                                                 
          BZ    *+10                                                            
          AR    R2,R0                                                           
          B     D32BS139M                                                       
                                                                                
*  IF BBM AND TP REQUEST (MONTHLY) NO BOOKTYPE W                                
          CLI   DBSRC,C'A'                                                      
          BNE   D32BS139O                                                       
          L     RE,ADEMFIL                                                      
          CLC   =C'WTP',8(RE)                                                   
*******   BE    *+8            WTP ALSO DONT PASS BOOKTYPES                     
          B     D32BS139M      TP NO BOOKTYPES                                  
D32BS139O DS    0H                                                              
*                                                                               
          MVC   0(L'TDRI2BTP,R2),TDRI2BTP  ALWAYS 1ST ITEM IN GRP#2             
          CLI   0(R2),X'E0'                                                     
          BNE   *+8                                                             
          MVI   0(R2),X'00'                                                     
          OC    0(L'TDRI2BTP,R2),0(R2)     IF NO BOOKTYPE,                      
          BZ    *+14                        DON'T BUMP OUTPUT POINTER           
          OC    0(L'TDRI2BTP,R2),SPACES                                         
          AHI   R2,L'TDRI2BTP                                                   
D32BS139M EQU   *                                                               
                                                                                
*                                                                               
D32BS140  DS    0H                          BOOK                                
          MVC   DUB+0(1),TDRI2BKY            YEAR                               
          MVC   DUB+1(1),0(R6)               MONTH/WEEK NUMBER                  
          XC    DUB+0(2),=X'FFFF'                                               
                                                                                
* NEW CODE  HERE                                                                
************************************************                                
          CLI   DBMED,C'C'                                                      
          BNE   D32BS1430                                                       
          L     RE,ADEMFIL                                                      
          CLC   =C'WTP',8(RE)                                                   
          BE    D32BS1413                                                       
*  TP CODE                                                                      
          CLI   DBSRC,C'A'                                                      
          BE    D32BS1410                                                       
          CLC   DUB,=X'6001'                                                    
          BNL   D32BS1416                                                       
          B     D32BS1430                                                       
*                                                                               
D32BS1410 CLI   TDRI2BTP,C'W'    IF REGULAR MONTHLY BOOK                        
          BNE   D32BS1430                                                       
          B     D32BS1416        IF WEEKLY BOOK FOR TP REQUEST                  
*                                TRANSLATE TO MONTHLY                           
*  WTP CODE                                                                     
D32BS1413 DS    0H                                                              
***       XC    DUB(2),=X'FFFF'                                                 
          CLI   DBSRC,C'A'                                                      
          BE    D32BS1414                                                       
          CLC   DUB,=X'6001'     IF WTP REQUEST AND WEEKLY DATA                 
          BL    D32BSX           ELSE BYPASS MONTHYL DATA                       
          MVI   DUB+2,C'W'       TRANSLATION ROUTINE ONLY TRANSLATE             
*                                BOOK TO WEEKLY FORMAT IF BKTYPE IS             
*                                SET TO W FOR CANADIAN                          
          B     D32BS1430        THEN JUST DOWNLOAD                             
D32BS1414 CLI   TDRI2BTP,C'W'    IF REGULAR MONTHLY BOOK                        
          BNE   D32BSX                                                          
          B     D32BS1430        IF WEEKLY BOOK FOR TP REQUEST                  
*                                TRANSLATE TO MONTHLY                           
D32BS1416 DS    0H                                                              
* THIS PART TRANSLATES THE WEEKLY BOOK TO MONTHLY FORMAT                        
*                                                                               
          GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(BYTE,VGETDAY),VADDAY,VDATCON          
          ZICM  R1,DMCB+1,(7)                                                   
          BNZ   *+6                                                             
          DC    H'0'                                                            
          MVC   DUB2(6),0(R1)                                                   
          GOTO1 VDATCON,DMCB,(X'80',DUB2),(3,FULL2)                             
          MVC   DUB(2),FULL2                                                    
          MVI   DUB+2,0         INDICATE ITS NOT WEEKLY                         
*                                                                               
D32BS1430 DS     0H                                                             
***       XC    DUB(2),=X'FFFF'                                                 
***********************************************                                 
                                                                                
          CLI   TDRI2BTP,C'W'                                                   
          BNE   *+8                                                             
          MVI   DUB+2,C'W'                                                      
          CLI   DBSRC,C'A'                                                      
          BNE   D32BS1432                                                       
          L     RE,ADEMFIL                                                      
          CLC   =C'WTP',8(RE)                                                   
          BE    D32BS1432                                                       
          MVI   DUB+2,0         IF MONTHLY REQUEST DONT TRANSLATE AS            
D32BS1432 DS    0H              WEEKLY                                          
*                                                                               
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   D32BS1433                                                       
          MVI   GOSUBN,TBK#                                                     
          GOTO1 AGOSUB                                                          
          B     D32BS143X                                                       
D32BS1433 DS    0H                                                              
          GOTO1 AD32TBK                                                         
D32BS143X EQU   *                                                               
                                                                                
          LA    R0,1(R2)                                                        
          S     R0,ASTIOBUF                                                     
          AH    R0,HALF                  R0 = NEW L(DATA IN I/O BUFFER)         
          CHI   R0,255                   CAN THIS BOOK FIT INTO BUFFER?         
          BNH   D32BS145X                 NO, DOWNLOAD STUFF IN BUFFER          
                                                                                
          MVC   FALDMPC,=Y(FMDBKSTGP2A)     GROUP #2 DATA (W/ DEM32 BK)         
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   *+10                                                            
          MVC   FALDMPC,=Y(FMDBKSTGRP2)                                         
          L     R0,ASTIOBUF                                                     
          LR    R1,R2                                                           
          S     R1,ASTIOBUF                                                     
          BAS   RE,D32BSAD                                                      
          MVI   GOSUBN,CIOB#             CLEAR STEREO I/O BUFFER                
          GOTO1 AGOSUB                                                          
          B     D32BS139                                                        
D32BS145X EQU   *                                                               
*                                                                               
          MVC   0(1,R2),MYD32DLM                                                
          AHI   R2,1                                                            
*                                                                               
          LH    R1,HALF                                                         
          BCTR  R1,0                                                            
          EXMVC R1,0(R2),WORK                                                   
          LA    R2,1(R2,R1)                                                     
*                                                                               
          DS    0H                                                              
          LA    R6,1(R6)            BUMP TO NEXT MTH/WK #                       
          BCT   R4,D32BS140                                                     
                                                                                
*                                                                               
          DS    0H                                                              
          S     R2,ASTIOBUF                                                     
          STH   R2,IODATALN                                                     
*                                                                               
D32BS159  EQU   *                                                               
          B     D32BSX                                                          
                                                                                
*                                                                               
D32BSX    DS    0H                                                              
          L     RF,AAPSAV2                                                      
          USING APSAV2D,RF                                                      
          NI    AS2RSMF,XFF-AS2RFFLK                                            
          DROP  RF                                                              
*                                                                               
          J     EXITE                                                           
          DROP  R5                                                              
                                                                                
                                                                                
*                                                                               
** HELPER ROUTINE TO DOWNLOAD THE DATA **                                       
*                                                                               
D32BSAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
                                                                                
          TM    BRKFLAG1,BF1FALNK  DID FALINK CALL FOR A BREAK?                 
          BZ    D32BSADG            NOPE                                        
          L     RF,AAPSAV2          YES, NEED TO SAVE LOCAL VARIABLES           
          USING APSAV2D,RF                                                      
          MVC   AS2MWN,0(R6)         REMEMBER MONTH/WEEK #                      
          DROP  RF                                                              
          J     XITMOD              EXIT DEM03 NOW                              
D32BSADG  EQU   *                                                               
*                                                                               
          J     EXIT                                                            
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--D32MKST#+        
               )'                                                               
*----------------- DEM32 DOWNLOAD MARKETS FOR STATION ----------------*         
                                                                                
D32MKST   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   FALEMPC,=Y(FMHMKST)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
                                                                                
*                                                                               
          DS    0H                          ALPHA MARKET                        
          MVC   FALDMPC,=Y(FMDMKSTAMKT)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32MS029                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRSORAM),TDRSORAM                                       
          LA    R0,WORK                                                         
          LA    R1,3                                                            
          CLC   SPACES(3),WORK                                                  
          BNL   D32MS028                                                        
          BAS   RE,D32MSAD                                                      
          B     D32MS029                                                        
* IF NO ALPHA MARKET THEN PRINT OUT MARKET NUMBER                               
D32MS028  MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRSORMK),TDRSORMK                                       
          OC    TDRSORMK,TDRSORMK                                               
          BNZ   *+10                                                            
          MVC   WORK(L'TDRSOKMK),TDRSOKMK                                       
          ZICM  R1,WORK,2                                                       
          EDIT  (R1),(4,WORK),ALIGN=LEFT                                        
          LR    R1,R0                                                           
          LA    R0,WORK                                                         
          BAS   RE,D32MSAD                                                      
*                                                                               
D32MS029  EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                          NUMERIC MARKET                      
          MVC   FALDMPC,=Y(FMDMKSTNMKT)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32MS039                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRSORMK),TDRSORMK                                       
          OC    TDRSORMK,TDRSORMK                                               
          BNZ   *+10                                                            
          MVC   WORK(L'TDRSOKMK),TDRSOKMK                                       
          ZICM  R1,WORK,2                                                       
          EDIT  (R1),(4,WORK),ALIGN=LEFT                                        
          LR    R1,R0                                                           
          LA    R0,WORK                                                         
          BAS   RE,D32MSAD                                                      
D32MS039  EQU   *                                                               
                                                                                
*                                                                               
          MVC   FALDMPC,=Y(FMDMKSTMNAM)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32MS049                                                        
                                                                                
          MVC   WORK,SPACES                                                     
* ATTACH HOME OR SPILL INDICATOR                                                
*                                                                               
          LA    RE,WORK                                                         
          CLI   TDRSOBTP,X'E0'                                                  
          BNE   *+12                                                            
          MVI   0(RE),C'#'                SPILL                                 
          AHI   RE,1                                                            
          OC    TDRSOKMK,TDRSOKMK                                               
          BNZ   *+12                                                            
          MVI   0(RE),C'*'                HOME                                  
          AHI   RE,1                                                            
*                                                                               
**        MVC   WORK(L'TDRSOMNA),TDRSOMNA                                       
          MVC   0(L'TDRSOMNA,RE),TDRSOMNA                                       
          LA    R0,WORK                                                         
          LA    R1,30                                                           
          BAS   RE,D32MSAD                                                      
D32MS049  EQU   *                                                               
                                                                                
*                                                                               
          MVC   FALDMPC,=Y(FMDMKSTMREL)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32MS059                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          CLI   TDRSOBTP,X'E0'                                                  
          BNE   *+8                                                             
          MVI   WORK,C'S'                                                       
          OC    TDRSOKMK,TDRSOKMK                                               
          BNZ   *+8                                                             
          MVI   WORK,C'H'                                                       
          LA    R0,WORK                                                         
          LA    R1,1                                                            
          BAS   RE,D32MSAD                                                      
D32MS059  EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                                                              
          J     EXIT                                                            
          DROP  R5                                                              
                                                                                
                                                                                
*                                                                               
** HELPER ROUTINE TO DOWNLOAD THE DATA **                                       
*                                                                               
D32MSAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
                                                                                
          TM    BRKFLAG1,BF1FALNK  DID FALINK CALL FOR A BREAK?                 
          JNZ   XITMOD              EXIT DEM03 NOW                              
*                                                                               
          J     EXIT                                                            
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--D32PST#)+        
               '                                                                
*--------------------------- DEM32 POSTING ---------------------------*         
                                                                                
* Builds TSAR DEMO RECORD under a DEM32 session.  The actual posting            
*  occurs upon exiting this routine with an equal condition code.               
* At entry,                                                                     
*   DUB+0(2) = book                                                             
* At exit,                                                                      
*   CC set to equal if record successfully built,                               
*   CC set to not equal otherwise                                               
                                                                                
D32POST  DS    0H                                                               
         CLI   ACTN,STATION                                                     
         BE    D32PST020                                                        
         B     D32PSTXN                                                         
                                                                                
*                                                                               
D32PST020 DS   0H                                                               
         CLI   TMPRTYP,TDRRTDRQ                                                 
         BE    D32PST100                                                        
         B     D32PSTXN                                                         
                                                                                
                                                                                
*                                                                               
* ACTN=STATION, TDRRTYP=TDRRTDRQ *                                              
*                                                                               
D32PST100 DS   0H                                                               
*                                                                               
*         CLC   =C'KMSG',STACALL             FRESNO CHANGES                     
*         BNE   D32PST101                                                       
*         CLC   KBOOK,=X'6502'                                                  
*         BL    D32PST101                                                       
*         MVC   STACALL(4),=C'KFRE'                                             
D32PST101 DS    0H                                                              
*                                                                               
         CLI   DBMED,C'C'           CANADIAN                                    
         BNE   D32PST102                                                        
         CLI   DBSRC,C'A'           BBM                                         
         BNE   D32PST102                                                        
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   D32PST102                                                        
**       CLI   MLBTYP,C'W'                                                      
**       BNE   DEMHOOKX                                                         
         CLI   KBOOK,X'65'         BBM WEEKLY TORONTO STARTS                    
         BH    D32PST102           NOV04/02  01 DATA IS TEST DATA               
                                                                                
         CLC   =X'144F',KMARKET                                                 
         BE    D32PSTXN                                                         
         CLC   =X'144F',MARKET                                                  
         BE    D32PSTXN                                                         
                                                                                
D32PST102 DS    0H                                                              
*                                                                               
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
         LA    R6,TSARKEY                                                       
         USING TSDEMRCD,R6                                                      
         MVC   TDRRTYP,TMPRTYP                                                  
*        MVC   TDRI2RMK,MARKET                                                  
         MVC   TDRI2STA,STACALL                                                 
         MVC   TDRI2KMK,KMARKET                                                 
         MVC   TDRI2BTP,KBTYP                                                   
*        MVC   TDRI2BKY,KBOOK                                                   
*        XI    TDRI2BKY,XFF                                                     
*                                                                               
*                                                                               
*  SPECIAL CASE ??? WHAT THE HECK IS BOOKTYPE 'E0' ?                            
*                                                                               
         CLI   ACTN,STATION                                                     
         BNE   D32PST105                                                        
         CLI   TDRI2BTP,X'E0'                                                   
         BNE   *+12                                                             
         MVI   TDRI2BTP,0                                                       
         MVI   TDRI2XPL,X'E0'                                                   
*                                                                               
*                                                                               
*&&DO                                                                           
         CLI   ACTN,STATION                                                     
         BNE   D32PST105                                                        
*                                                                               
         OC    BKS,BKS              AND IF NO BOOKS ENTERRED,                   
         BNZ   D32PST105                                                        
         MVI   TDRI2BTP,0                                                       
*&&                                                                             
*                                                                               
D32PST105 DS    0H                                                              
*                                                                               
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 AGETTDR                                                          
         BE    D32PST110                                                        
*                                                                               
         LA    RF,TSARBLCK         RECORD NOT FOUND                             
         USING TSARD,RF                                                         
         ZICM  RE,TSAREC+1,(7)                                                  
         DROP  RF                                                               
                                                                                
         DS    0H                  BUILD A SKELETON RECORD                      
         LA    R1,2(RE)                                                         
         USING TSDEMRCD,R1                                                      
         MVC   TDRKEY(TDRI2KYL),TSARKEY     KEY                                 
*                                                                               
         CLI   DBMED,C'U'                                                       
         BE    D32PST108                                                        
*                                                                               
         MVI   TDRKEY+TDRI2KYL+0,TDREAFFQ   AFFILIATIONS ELEMENT                
         MVI   TDRKEY+TDRI2KYL+1,7                                              
         MVC   TDRKEY+TDRI2KYL+2(5),SPACES                                      
         MVI   TDRKEY+TDRI2KYL+7,TDREMWNQ   MONTH/WEEK # ELEMENT                
         MVI   TDRKEY+TDRI2KYL+8,2                                              
         LA    R0,TDRI2KYL+7+2+2                                                
         STCM  R0,3,0(RE)                   TOTAL RECORD LENGTH                 
         B     D32PST110                                                        
*                                                                               
* DOING THIS FOR COUNTY COVERAGE                                                
* BECAUSE AFFILATIONS WILL CONTAIN COUNTY NAME                                  
*                                                                               
D32PST108 DS    0H                                                              
         MVI   TDRKEY+TDRI2KYL+0,TDREAFFQ   AFFILIATIONS ELEMENT                
         MVI   TDRKEY+TDRI2KYL+1,18                                             
         MVC   TDRKEY+TDRI2KYL+2(16),MARNAME                                    
         MVI   TDRKEY+TDRI2KYL+18,TDREMWNQ   MONTH/WEEK # ELEMENT               
         MVI   TDRKEY+TDRI2KYL+19,2                                             
         DROP  R1                                                               
         LA    R0,TDRI2KYL+18+2+2                                               
         STCM  R0,3,0(RE)                   TOTAL RECORD LENGTH                 
                                                                                
*                                                                               
D32PST110 DS   0H                                                               
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         SR    R6,R6                                                            
         ICM   R6,7,TSAREC+1          R6-->TSAR DEMO RECORD                     
         DROP  RF                                                               
                                                                                
         MVC   MYDATDSP,=Y(TDRI2KYL)  DISPL TO 1ST ELEMENT                      
                                                                                
         L     R0,ASTBUFFR                                                      
         LH    R1,LSTBUFFR                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                  CLEAR BUFFER FOR NEW ELEMENTS             
                                                                                
*                                                                               
** AFFILIATIONS ELEMENT **                                                      
*                                                                               
         DS    0H                                                               
         LA    R3,2(R6)                                                         
         MVI   MYELCODE,TDREAFFQ                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,MYDMCB,(C'P',=C'CORETAB'),ASTBUFFR,(R3),0                 
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  DONT PASS BOOKS WITH STATION LIST FOR LATEST VERSION OF DEM32                
          CLC   D32PCVER,=AL4(XTRCVERA)                                         
          BH    D32PST138                                                       
*                                                                               
** MONTH/WEEK NUMBER ELEMENT **                                                 
*                                                                               
         DS    0H                                                               
         LA    R3,2(R6)                                                         
         MVI   MYELCODE,TDREMWNQ                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                                                               
         MVC   BYTE,KBOOK+1                                                     
         XI    BYTE,XFF             BYTE = MTH/WK # TO INSERT INTO ELEM         
*                                                                               
         XC    WORK2,WORK2          WORK2 = TEMP STORAGE FOR NEW ELEM           
         LA    R2,2(R3)             R2-->MTH/WK #s IN EXISTING ELEM             
         ZIC   R1,1(R3)                                                         
         SH    R1,=H'2'                                                         
         BZ    D32PST134                                                        
*                                                                               
D32PST132 DS   0H                                                               
         CLC   0(1,R2),BYTE        FIND "BREAK" POINT                           
         BNL   *+12                (PUTTING BOOKS IN REV CHRONO ORDER)          
         LA    R2,1(R2)                                                         
         BCT   R1,D32PST132                                                     
*                                                                               
D32PST134 DS   0H                                                               
         ZIC   R1,1(R3)                                                         
         LR    R0,R3               R0-->"TOP" PORTION OF ELEMENT                
         LR    RF,R2                                                            
         SR    RF,R0               RF = L("TOP" PORTION OF ELM) TO MOVE         
         LA    RE,WORK2            RE-->DESTINATION                             
         MVCL  RE,R0               MOVE "TOP" PORTION TO NEW ELEM               
                                                                                
         MVC   0(1,RE),BYTE        INSERT NEW MONTH/WEEK NUMBER                 
         LA    RE,1(RE)                                                         
                                                                                
         LTR   RF,R1               RF = L("BOTTOM" PRTN OF ELM) TO MOVE         
         BZ    D32PST137           IF ZERO, NO "BOTTOM" PORTION TO MOVE         
         LR    R0,R2                                                            
         CLC   0(1,R2),BYTE                                                     
         BH    *+10                (PUTTING BOOKS IN REV CHRONO ORDER)          
         LA    R0,1(R2)            R0-->"BOTTOM" PORTION OF ELEMENT             
         BCTR  RF,0                                                             
         MVCL  RE,R0               MOVE "BOTTOM" PORTION TO NEW ELEM            
D32PST137 EQU  *                                                                
                                                                                
         DS    0H                  CALCULATE L(NEW ELEMENT)                     
         LA    R0,WORK2                                                         
         SR    RE,R0                                                            
         STC   RE,WORK2+1                                                       
         GOTO1 VHELLO,MYDMCB,(C'P',=C'CORETAB'),ASTBUFFR,WORK2,0                
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
D32PST138 EQU  *                                                                
                                                                                
*                                                                               
** BUILD NEW TSAR DEMO RECORD **                                                
*                                                                               
         DS    0H                                                               
         LR    R0,R6                                                            
         LH    R1,SVMXRLEN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                CLEAR TSAR DEMO RECORD AREA                 
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R3,2(R6)                                                         
         USING TSDEMRCD,R3                                                      
         MVC   TDRKEY(TDRI2KYL),TSARKEY    KEY                                  
         L     RE,ASTBUFFR                                                      
         ZICM  R1,0(RE),(3)                                                     
         SH    R1,=Y(2+1)                                                       
         LA    R0,TDRKEY+TDRI2KYL                                               
         LR    RF,R1                                                            
         LA    RE,2(RE)                                                         
         MVCL  R0,RE                       ELEMENTS                             
         SR    R0,R6                                                            
         STCM  R0,3,0(R6)                  TOTAL RECORD LENGTH                  
         DROP  R3                                                               
                                                                                
*                                                                               
         B     D32PSTXY                                                         
                                                                                
*                                                                               
D32PSTXN DS    0H                                                               
         J     NO                                                               
*                                                                               
D32PSTXY DS    0H                                                               
         J     YES                                                              
*&&DO                                                                           
*----------------- DEM32 DOWNLOAD SYSCODES FOR MARKET ----------------*         
                                                                                
D32SYSCD  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          CLC   D32PCVER,=AL4(XTRCVER9)                                         
          BL    D32SY200                                                        
                                                                                
*   THE FOLLOWING SECTION IS FOR THE NEW BUILD FOR FUSION                       
*   WHICH DEALS WITH A NEW TRANSACTION = SYSCODE                                
*   FUDGE ON TRANSACTION DOWNLOAD TO BE SPILL TRANSACTION FOR NOW               
*   SO WE CAN GIVE THE PC SOME TIME BEFORE THEY CAN GIVE US A NEW               
*   SYSCODE LISTING                                                             
*                                                                               
          MVC   FALEMPC,=Y(FMHSYSCD)                                            
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                          MSO NAME                            
          MVC   FALDMPC,=Y(FMDDMSONAME)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY029                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3MSO),TDRI3MSO                                       
          LA    R0,WORK                                                         
          LA    R1,L'TDRI3MSO                                                   
          BAS   RE,D32SYAD                                                      
          B     D32SY029                                                        
D32SY029  EQU   *                                                               
                                                                                
D32SY039  EQU   *                                                               
*     SYSCODE                                                                   
          MVC   FALDMPC,=Y(FMDDSYSCODE)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY049                                                        
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SCD),TDRI3SCD                                       
          LA    R1,WORK                                                         
          LA    R1,L'TDRI3SCD                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
D32SY049  EQU   *                                                               
                                                                                
*                                                                               
          MVC   FALDMPC,=Y(FMDDSYSNAME)      SYSCODE NAME                       
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY059                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SYN),TDRI3SYN                                       
          LA    R1,L'TDRI3SYN                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY059  EQU   *                                                               
          MVC   FALDMPC,=Y(FMDDSYSAMKT)      ALPHA MKT                          
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY069                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3AMK),TDRI3AMK                                       
          LA    R1,L'TDRI3AMK                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY069  EQU   *                                                               
                                                                                
          MVC   FALDMPC,=Y(FMDDSYSSBAS)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY079                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SBS),TDRI3SBS                                       
          LA    R1,L'TDRI3SBS                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
                                                                                
D32SY079  EQU   *                                 UNIVERSE                      
          MVC   FALDMPC,=Y(FMDDSYSUNIV)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY089                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3UNV),TDRI3UNV                                       
          LA    R1,L'TDRI3UNV                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY089  EQU   *                                 UNIVERSE                      
          DS    0H                                                              
          J     EXIT                                                            
*                                                                               
D32SY200  DS    0H                                                              
*  THE FOLLOWING SECTION IS FOR MIMICKING THE MAPCODES OF A SPILLTO             
*  TRANSACTION BECAUSE THE PC DOESNT HAVE SUPPORT FOR A SYSCODE LIST            
*                                                                               
          MVC   FALEMPC,=Y(FMHMKST)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
                                                                                
          MVC   FALDMPC,=Y(FMDMKSTAMKT)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY259                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SYN),TDRI3SYN                                       
          LA    R1,L'TDRI3SYN                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY249  EQU   *                                                               
                                                                                
*     SYSCODE                                                                   
          MVC   FALDMPC,=Y(FMDMKSTNMKT)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY259                                                        
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SCD),TDRI3SCD                                       
          LA    R1,WORK                                                         
          EDIT  (B2,(R1)),(4,WORK),ALIGN=LEFT                                   
          LA    R1,4                                                            
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
                                                                                
*                                                                               
D32SY259  EQU   *                                                               
*                                                                               
          DS    0H                          MSO NAME                            
          MVC   FALDMPC,=Y(FMDMKSTMNAM)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32SY269                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3MSO),TDRI3MSO                                       
          LA    R0,WORK                                                         
          LA    R1,L'TDRI3MSO                                                   
          BAS   RE,D32SYAD                                                      
          B     D32SY269                                                        
D32SY269  EQU   *                                                               
          J     EXIT                                                            
          DROP  R5                                                              
** HELPER ROUTINE TO DOWNLOAD THE DATA **                                       
*                                                                               
D32SYAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
                                                                                
          TM    BRKFLAG1,BF1FALNK  DID FALINK CALL FOR A BREAK?                 
          JNZ   XITMOD              EXIT DEM03 NOW                              
*                                                                               
          J     EXIT                                                            
*&&                                                                             
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--LTORG &&+        
               CONSTANTS)'                                                      
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(4096*2-SUBR01L+1)                                            
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR02)'                 
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR02Q  EQU   (((*-DEM03+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   DEM03+SUBR02Q                                                    
SUBR02   NMOD1 0,**0302**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM03WRKD,RC                                                      
*                                                                               
         AHI   R1,-(R01#)          SUBTRACT FOR SUB-RTN #1                      
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
                                                                                
FSTN#    EQU   (R02_01-R02_00)/4+R01#   FORMAT STATION                          
CIOB#    EQU   (R02_02-R02_00)/4+R01#   CLEAR STEREO I/O BUFFER                 
TBK#     EQU   (R02_03-R02_00)/4+R01#   TRANSLATE BOOK                          
GADLD#   EQU   (R02_04-R02_00)/4+R01#   GET A(DOWNLOAD DATA TABLES)             
OKDL#    EQU   (R02_05-R02_00)/4+R01#   OKAY TO DOWNLAD ?                       
D32SYSC# EQU   (R02_06-R02_00)/4+R01#   SYSCODE LISTING DOWNLOAD                
                                                                                
                                                                                
R02_00   DS    0H                                                               
R02_01   B     FMTSTTN                  FORMAT STATION                          
R02_02   B     CLRIOBUF                 CLEAR STEREO I/O BUFFER                 
R02_03   B     TRSLTBK                  TRANSLATE BOOK                          
R02_04   B     GADLDTAB                 GET A(DOWNLOAD DATA TABLE)              
R02_05   B     OKDOWNLD                 OKAY TO DOWNLOAD?                       
R02_06   B     D32SYSCD                 DOWNLOAD SYSCODE                        
R02#     EQU   (*-R02_00)/4+R01#                                                
         DC    H'0'                                                             
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR02--FSTN#)'          
*--------------------------- FORMAT STATION --------------------------*         
                                                                                
* Formats station and spill market, if any, to pass down to STEREO.             
* At entry,                                                                     
*   STACALL = station call letters                                              
*   KMARKET = spill market                                                      
*   KBOOK   = associated book                                                   
*   KBTYP   = associated booktype                                               
*   DBLOCK1 set up with DBAREC,DBCOMFCS,DBFILE,DBSELMED,DBSELSRC                
* At exit,                                                                      
*   WORK2 = formatted output                                                    
*   HALF  = L(formatted output)                                                 
                                                                                
FMTSTTN  DS    0H                                                               
         MVC   WORK2,SPACES                                                     
         LA    R2,WORK2                                                         
                                                                                
*                                                                               
** STATION CALL LETTERS **                                                      
*                                                                               
         DS    0H                                                               
         MVC   0(L'STACALL,R2),STACALL                                          
         LA    R2,L'STACALL-1(R2)                                               
         CLI   0(R2),C'T'                   DON'T SHOW PARENT INDICATR          
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
                                                                                
*                                                                               
         DS    0H                  POINT TO NEXT AVAILABLE OUTPUT AREA          
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
                                                                                
*                                                                               
** APPEND MARKET **                                                             
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'R'                    ALWAYS FOR RADIO                   
         BE    FSTN035                                                          
         CLI   DBSRC,C'N'                    ALWAYS FOR                         
         BNE   *+12                                                             
         CLI   KBTYP,C'C'                     NIELSEN CABLE                     
         BE    FSTN035                                                          
         OC    KMARKET,KMARKET                                                  
         BNZ   FSTN035                                                          
         B     FSTN039                                                          
*                                                                               
FSTN035  DS    0H                                                               
         OC    KMARKET,KMARKET                                                  
         BZ    FSTN035G                                                         
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELRMK,KMARKET                                                 
         XC    DBSELSTA,DBSELSTA                                                
         MVC   DBBTYPE,KBTYP                                                    
         CLI   DBMED,C'O'                     OVERNIGHT READ USTV               
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         CLI   DBSRC,C'F'                 FUSION FUDGE TO READ NSI              
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCK,DMHK2,0    GET ALPHA MKT & NAME              
         CLI   DBSRC,C'F'                  RESET BACK TO FUSION                 
         BNE   *+8                                                              
         MVI   DBSELSRC,C'F'                                                    
         DROP  R5                                                               
FSTN035G EQU   *                                                                
*                                                                               
         DS    0H                                                               
****     MVI   0(R2),C'/'                                                       
****     LA    R2,1(R2)                                                         
*                                                                               
         DS    0H                                                               
         OC    MARRALF,MARRALF                                                  
         BZ    FSTN036E                                                         
         CLC   MARRALF,SPACES                                                   
         BE    FSTN036E                                                         
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(L'MARRALF,R2),MARRALF                                          
         LA    R2,L'MARRALF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         B     FSTN039                                                          
                                                                                
FSTN036E DS    0H                                                               
         OC    KMARKET,KMARKET                                                  
         BZ    FSTN039                                                          
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B2,KMARKET),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R2,R0                                                            
FSTN039  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R0,WORK2                                                         
         SR    R2,R0                                                            
         STH   R2,HALF             SET L(FORMATTED STATION)                     
                                                                                
*                                                                               
FSTNX    DS    0H                                                               
         J     EXIT                                                             
                                                                                
                                                                                
*                                                                               
** DEMAND HOOK TO EXTRACT MARKET NAME **                                        
*                                                                               
* At entry,                                                                     
*   DBLOCK1 = area used for DBLOCK in the DEMAND call                           
                                                                                
DMHK2    NTR1                                                                   
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
*                                                                               
*  RADAR ALPHA MKT RECORD STILL MUCKED UP-KEEP FUDGING                          
         CLI   DBSRC,C'R'                                                       
         BNE   DMHK2A                                                           
         MVC   MARNMKT,=X'0001'                                                 
         MVC   MARNAME(14),=C'RADAR MARKET 1'                                   
         MVC   MARRALF,=C'USA'                                                  
         J     EXIT                                                             
*                                                                               
DMHK2A   DS    0H                                                               
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'MNAME',DBLOCK,WORK                             
         MVC   MARNMKT,WORK+0                                                   
         MVC   MARNAME,WORK+2                                                   
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 (RF),(R1),=C'NAMKT',DBLOCK,WORK                                  
         MVC   MARRALF,WORK                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         DROP  R5                                                               
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR02--CIOB#)'          
*---------------------- CLEAR STEREO I/O BUFFER ----------------------*         
                                                                                
CLRIOBUF DS    0H                                                               
         L     R0,ASTIOBUF                                                      
         LH    R1,LSTIOBUF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    IODATALN,IODATALN                                                
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
                                                                                
*                                                                               
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* Translates book into a printable format                                       
* At entry,                                                                     
*   DUB+0(2) = book                                                             
* At exit,                                                                      
*   WORK     = formatted book                                                   
*   HALF     = length of formatted book                                         
                                                                                
TRSLTBK  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'W'                                                       
         BE    TBK060                                                           
*                                                                               
         DS    0H                                                               
         MVI   STARTDAY,0          USE DEFAULT START DAY IN NSIWEEK             
         CLI   DBMED,C'C'           CHECK FOR CANADIEN                          
         BNE   TBK053X                                                          
         CLI   DBSRC,C'N'            NIELSEN                                    
         BNE   TBK053X                                                          
         CLC   DBFIL,=C'TP '         TIME PERIOD                                
         BNE   TBK053X                                                          
         CLC   DUB(2),=X'6001'       ON OR AFTER JAN/96                         
         BL    TBK053X                                                          
         MVI   STARTDAY,1           OVERRIDE START DAY TO MONDAY                
         B     TBK060                                                           
TBK053X  EQU   *                                                                
         B     TBK080              GO FORMAT AS MONTHLY BOOK                    
                                                                                
*                                                                               
TBK060   DS    0H                  FORMAT WEEKLY BOOK                           
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(STARTDAY,VGETDAY),VADDAY,     +        
               VDATCON                                                          
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB2(6),0(R1)                                                    
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(5,(R4))                               
         ZIC   R0,DMCB+4                                                        
         AR    R4,R0                                                            
         B     TBK100                                                           
                                                                                
*                                                                               
TBK080   DS    0H                  FORMAT MONTHLY BOOK                          
         GOTO1 VDATCON,DMCB,(X'83',DUB),(6,(R4))                                
         CLI   3(R4),C'/'          IF OUTPUT IS MMM/YY,                         
         BNE   *+12                                                             
         MVC   3(2,R4),4(R4)        THEN REMOVE THE '/'                         
         BCTR  R4,0                                                             
         ZIC   R0,DMCB+4                                                        
         AR    R4,R0                                                            
         B     TBK100                                                           
                                                                                
*                                                                               
TBK100   DS    0H                                                               
                                                                                
*                                                                               
TBKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF             SET LENGTH INTO HALF                         
*                                                                               
         J     EXIT                                                             
                                                                                
*-------------------- GET A(DOWNLOAD DATA TABLES) --------------------*         
                                                                                
* Gets address of corresponding download data entry                             
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
* At exit,                                                                      
*   ADLDNTRY = A(download data table entry)                                     
                                                                                
GADLDTAB DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R2,15,ADLDTABS      R2-->DOWNLOAD DATA TABLES                    
         BZ    GADLDX                                                           
         USING DLDTABD,R2                                                       
                                                                                
*                                                                               
GADLD012 DS    0H                  R2-->RECORD TYPE ENTRY                       
         OC    0(2,R2),0(R2)                                                    
         BZ    GADLDX                                                           
         CLC   3(1,R2),TMPRTYP     IS THIS THE ONE WE'RE LOOKING FOR?           
         BE    GADLD012X            YEP!                                        
         ZICM  R1,0(R2),(3)         NOT THE RIGHT RECORD TYPE                   
         AR    R2,R1                 BUMP TO NEXT ONE                           
         B     GADLD012                                                         
GADLD012X EQU  *                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R1,DLDTDSP                                                       
         DROP  R2                                                               
                                                                                
         LA    R3,0(R2,R1)                                                      
         USING DLDTABD,R3                                                       
                                                                                
*                                                                               
GADLD022 DS    0H                  R3-->FIL/SRC/MED ENTRY                       
         OC    DLDTLEN,DLDTLEN                                                  
         BZ    GADLDX                                                           
                                                                                
         CLC   DLDTFIL,SPACES                                                   
         BE    *+14                                                             
         CLC   DLDTFIL,DBFIL        MATCH ON FILE                               
         BNE   GADLD022G                                                        
                                                                                
         CLC   DLDTSRC,SPACES                                                   
         BE    *+14                                                             
         CLC   DLDTSRC,DBSRC        MATCH ON SOURCE                             
         BNE   GADLD022G                                                        
                                                                                
         CLC   DLDTMED,SPACES                                                   
         BE    *+14                                                             
         CLC   DLDTMED,DBMED        MATCH ON MEDIA                              
         BNE   GADLD022G                                                        
                                                                                
         B     GADLD022X            FOUND FILE/SOURCE/MEDIA                     
                                                                                
GADLD022G DS   0H                                                               
         ZICM  R1,DLDTLEN,(3)       NOT THE RIGHT FIL/SRC/MED                   
         AR    R3,R1                 BUMP TO NEXT ONE                           
         B     GADLD022                                                         
GADLD022X EQU  *                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R1,DLDTDSP                                                       
         DROP  R3                                                               
                                                                                
         LA    R4,0(R3,R1)                                                      
         USING DLDTABD,R4                                                       
                                                                                
*                                                                               
GADLD032 DS    0H                  R4-->ELEMENT MAP CODE ENTRY                  
         OC    DLDTLEN,DLDTLEN                                                  
         BZ    GADLDX                                                           
                                                                                
         CLC   DLDTEMPC,FALEMPC     MATCH ON ELEMENT MAP CODE                   
         BNE   GADLD032G                                                        
                                                                                
         B     GADLD032X            FOUND ELEMENT MAP CODE                      
                                                                                
GADLD032G DS   0H                                                               
         ZICM  R1,DLDTLEN,(3)       NOT THE RIGHT ELEMENT MAP CODE              
         AR    R4,R1                 BUMP TO NEXT ONE                           
         B     GADLD032                                                         
GADLD032X EQU  *                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R1,DLDTDSP                                                       
         DROP  R4                                                               
                                                                                
         LA    R5,0(R4,R1)                                                      
         USING DLDTABD,R5                                                       
                                                                                
*                                                                               
GADLD052 DS    0H                  R5-->ELEMENT MAP CODE ENTRY                  
         OC    DLDTLEN,DLDTLEN                                                  
         BZ    GADLDX                                                           
                                                                                
         CLC   DLDTVRSN,D32PCVER    FIND THE RIGHT EXTRACT VRSN DATE            
         BH    GADLD052G                                                        
                                                                                
         B     GADLD052X            FOUND RIGHT EXTRACT VERSION                 
                                                                                
GADLD052G DS   0H                                                               
         ZICM  R1,DLDTLEN,(3)       NOT THE RIGHT EXTRACT VERSION               
         AR    R5,R1                 BUMP TO NEXT ONE                           
         B     GADLD052                                                         
GADLD052X EQU  *                                                                
*                                                                               
         LR    R0,R5               POINT R0 TO ENTRY                            
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     GADLDX                                                           
                                                                                
*                                                                               
GADLDX   DS    0H                                                               
         ST    R0,ADLDNTRY                                                      
         J     EXIT                                                             
*                                                                               
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS (SUBR01--OKDL#)'          
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
         CLI   PUFLLOO,C'N'        OKAY FROM LAST LEFT OFF?                     
         BNE   OKDL010X             EITHER "YES" OR "N/A"                       
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         CLC   AS2EMPC,FALEMPC      MATCH ON ELEM MAP CODE AT BREAK             
         BNE   OKDLXN                                                           
         CLC   AS2DMPC,FALDMPC      MATCH ON DATA MAP CODE AT BREAK             
         BNE   OKDLXN                                                           
         DROP  RF                                                               
         MVI   PUFLLOO,C'Y'        NOW WE'RE OKAY FROM LAST LEFT OFF            
         B     OKDLXN               BUT DON'T DOWNLOAD THIS DATA TWICE          
OKDL010X EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,GADLD#                                                    
         GOTO1 AGOSUB                                                           
         ICM   R2,15,ADLDNTRY                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  R2-->DOWNLOAD DATA TABLE ENTRY               
         USING DLDTABD,R2                                                       
         ZIC   R3,DLDTDSP                                                       
         LA    R3,DLDTABD(R3)                                                   
         DROP  R2                                                               
         SR    R0,R0                                                            
*                                                                               
OKDL022  DS    0H                  BUMP TO SECTION FOR ELEMENT MAP CODE         
         OC    0(L'FALDMPC,R3),0(R3)                                            
         BZ    OKDLXN                                                           
         CLC   0(L'FALDMPC,R3),FALDMPC                                          
         BE    *+12                                                             
         AHI   R3,L'FALDMPC                                                     
         B     OKDL022                                                          
*                                                                               
         DS    0H                                                               
         B     OKDLXY                                                           
*                                                                               
OKDLXN   DS    0H                                                               
         J     NO                                                               
OKDLXY   DS    0H                                                               
         J     YES                                                              
*                                                                               
*                                                                               
*----------------- DEM32 DOWNLOAD SYSCODES FOR MARKET ----------------*         
                                                                                
D32SYSCD  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          CLC   D32PCVER,=AL4(XTRCVER9)                                         
          BL    D32SY200                                                        
                                                                                
*   THE FOLLOWING SECTION IS FOR THE NEW BUILD FOR FUSION                       
*   WHICH DEALS WITH A NEW TRANSACTION = SYSCODE                                
*   FUDGE ON TRANSACTION DOWNLOAD TO BE SPILL TRANSACTION FOR NOW               
*   SO WE CAN GIVE THE PC SOME TIME BEFORE THEY CAN GIVE US A NEW               
*   SYSCODE LISTING                                                             
*                                                                               
          MVC   FALEMPC,=Y(FMHSYSCD)                                            
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                          MSO NAME                            
          MVC   FALDMPC,=Y(FMDDMSONAME)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY029                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3MSO),TDRI3MSO                                       
          LA    R0,WORK                                                         
          LA    R1,L'TDRI3MSO                                                   
          BAS   RE,D32SYAD                                                      
          B     D32SY029                                                        
D32SY029  EQU   *                                                               
                                                                                
D32SY039  EQU   *                                                               
*     SYSCODE                                                                   
          MVC   FALDMPC,=Y(FMDDSYSCODE)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY049                                                        
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SCD),TDRI3SCD                                       
          LA    R1,WORK                                                         
          LA    R1,L'TDRI3SCD                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
D32SY049  EQU   *                                                               
                                                                                
*                                                                               
          MVC   FALDMPC,=Y(FMDDSYSNAME)      SYSCODE NAME                       
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY059                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SYN),TDRI3SYN                                       
          LA    R1,L'TDRI3SYN                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY059  EQU   *                                                               
          MVC   FALDMPC,=Y(FMDDSYSAMKT)      ALPHA MKT                          
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY069                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3AMK),TDRI3AMK                                       
          LA    R1,L'TDRI3AMK                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY069  EQU   *                                                               
                                                                                
          MVC   FALDMPC,=Y(FMDDSYSSBAS)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY079                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SBS),TDRI3SBS                                       
          LA    R1,L'TDRI3SBS                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
                                                                                
D32SY079  EQU   *                                 UNIVERSE                      
          MVC   FALDMPC,=Y(FMDDSYSUNIV)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY089                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3UNV),TDRI3UNV                                       
          LA    R1,L'TDRI3UNV                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY089  EQU   *                                 UNIVERSE                      
          DS    0H                                                              
          J     EXIT                                                            
*                                                                               
D32SY200  DS    0H                                                              
*  THE FOLLOWING SECTION IS FOR MIMICKING THE MAPCODES OF A SPILLTO             
*  TRANSACTION BECAUSE THE PC DOESNT HAVE SUPPORT FOR A SYSCODE LIST            
*                                                                               
          MVC   FALEMPC,=Y(FMHMKST)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
                                                                                
          MVC   FALDMPC,=Y(FMDMKSTAMKT)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY259                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SYN),TDRI3SYN                                       
          LA    R1,L'TDRI3SYN                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
*                                                                               
D32SY249  EQU   *                                                               
                                                                                
*     SYSCODE                                                                   
          MVC   FALDMPC,=Y(FMDMKSTNMKT)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY259                                                        
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3SCD),TDRI3SCD                                       
          LA    R1,WORK                                                         
          EDIT  (B2,(R1)),(4,WORK),ALIGN=LEFT                                   
          LA    R1,4                                                            
          LA    R0,WORK                                                         
          BAS   RE,D32SYAD                                                      
                                                                                
*                                                                               
D32SY259  EQU   *                                                               
*                                                                               
          DS    0H                          MSO NAME                            
          MVC   FALDMPC,=Y(FMDMKSTMNAM)                                         
          BAS   RE,D32DLOKD2                                                    
          BNE   D32SY269                                                        
                                                                                
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRI3MSO),TDRI3MSO                                       
          LA    R0,WORK                                                         
          LA    R1,L'TDRI3MSO                                                   
          BAS   RE,D32SYAD                                                      
          B     D32SY269                                                        
D32SY269  EQU   *                                                               
          J     EXIT                                                            
          DROP  R5                                                              
D32DLOKD2 NTR1                                                                  
          MVI   GOSUBN,OKDL#                                                    
          GOTO1 AGOSUB                                                          
          J     EXIT                                                            
** HELPER ROUTINE TO DOWNLOAD THE DATA **                                       
*                                                                               
D32SYAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
                                                                                
          TM    BRKFLAG1,BF1FALNK  DID FALINK CALL FOR A BREAK?                 
          JNZ   XITMOD              EXIT DEM03 NOW                              
*                                                                               
          J     EXIT                                                            
*                                                                               
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS' (SUBR02--LTORG &+        
               & CONSTANTS)'                                                    
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(X'1000'-SUBR02L+1)                                           
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'DEDEM03 - $DEM SPILL-OUT && SPILL-INS'                          
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(GOSUB-DEM03),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM03),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(DLDATTAB-DEM03),AL2(ADLDTABS-DEMTMPD)                        
         DC    AL2(BTPTBNHT-DEM03),AL2(ABTPTBNH-DEMTMPD)                        
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
SUBRTNTB DS    0XL(1+2)                                                         
         DC     AL1(R01#),AL2(SUBR01-DEM03)                                     
         DC     AL1(R02#),AL2(SUBR02-DEM03)                                     
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
*                                  TABLE OF NHT BOOKTYPES (CATEGORIES)          
BTPTBNHT DS    0CL(1)                                                           
         DC     X'00'               STANDARD                                    
         DC     C'5'                HISPANIC                                    
         DC     C'6'                ENGLISH                                     
         DC     C'7'                BILINGUAL                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*======================== DOWNLOAD DATA TABLES =======================*         
                                                                                
DLDATTAB      DS   0D                                                           
                                                                                
*                                                                               
** RECORD TYPE = TDRRTDRQ **                                                    
*                                                                               
DLDT1A        DS    0X                                                          
              DC     AL2(DLDT1AX-DLDT1A)                                        
              DC     AL1(DLDT1AG-DLDT1A)                                        
              DC     AL1(TDRRTDRQ)                                              
DLDT1AG       EQU   *                                                           
                                                                                
*                                                                               
*** FIL/SRC/MED = TP/NSI/USTV ***                                               
*                                                                               
DLDT1A2A      DS     0X                                                         
              DC      AL2(DLDT1A2AX-DLDT1A2A)                                   
              DC      AL1(DLDT1A2AG-DLDT1A2A)                                   
              DC      C'TP NT'              TP/NSI/USTV                         
DLDT1A2AG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2A3B    DS      0X                                                        
              DC       AL2(DLDT1A2A3BX-DLDT1A2A3B)                              
              DC       AL1(DLDT1A2A3BG-DLDT1A2A3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2A3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2A3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2A3B4CX-DLDT1A2A3B4C)                         
              DC        AL1(DLDT1A2A3B4CG-DLDT1A2A3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2A3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2A3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2A3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2A3B4BX-DLDT1A2A3B4B)                         
              DC        AL1(DLDT1A2A3B4BG-DLDT1A2A3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2A3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2A3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2A3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2A3B4AX-DLDT1A2A3B4A)                         
              DC        AL1(DLDT1A2A3B4AG-DLDT1A2A3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2A3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2A3B4AX EQU      *                                                        
                                                                                
DLDT1A2A3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2A3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2A3A    DS      0X                                                        
              DC       AL2(DLDT1A2A3AX-DLDT1A2A3A)                              
              DC       AL1(DLDT1A2A3AG-DLDT1A2A3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2A3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2A3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2A3A4AX-DLDT1A2A3A4A)                         
              DC        AL1(DLDT1A2A3A4AG-DLDT1A2A3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2A3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2A3A4AX EQU      *                                                        
                                                                                
DLDT1A2A3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2A3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2A3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2AX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = PAV/NSI/USTV ***                                              
*                                                                               
DLDT1A2B      DS     0X                                                         
              DC      AL2(DLDT1A2BX-DLDT1A2B)                                   
              DC      AL1(DLDT1A2BG-DLDT1A2B)                                   
              DC      C'PAVNT'              PAV/NSI/USTV                        
DLDT1A2BG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2B3B    DS      0X                                                        
              DC       AL2(DLDT1A2B3BX-DLDT1A2B3B)                              
              DC       AL1(DLDT1A2B3BG-DLDT1A2B3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2B3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2B3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2B3B4CX-DLDT1A2B3B4C)                         
              DC        AL1(DLDT1A2B3B4CG-DLDT1A2B3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2B3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2B3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2B3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2B3B4BX-DLDT1A2B3B4B)                         
              DC        AL1(DLDT1A2B3B4BG-DLDT1A2B3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2B3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2B3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2B3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2B3B4AX-DLDT1A2B3B4A)                         
              DC        AL1(DLDT1A2B3B4AG-DLDT1A2B3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2B3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2B3B4AX EQU      *                                                        
                                                                                
DLDT1A2B3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2B3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2B3A    DS      0X                                                        
              DC       AL2(DLDT1A2B3AX-DLDT1A2B3A)                              
              DC       AL1(DLDT1A2B3AG-DLDT1A2B3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2B3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2B3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2B3A4AX-DLDT1A2B3A4A)                         
              DC        AL1(DLDT1A2B3A4AG-DLDT1A2B3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2B3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2B3A4AX EQU      *                                                        
                                                                                
DLDT1A2B3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2B3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2B3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2BX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/NSI/WEEKLY ***                                             
*                                                                               
DLDT1A2C      DS     0X                                                         
              DC      AL2(DLDT1A2CX-DLDT1A2C)                                   
              DC      AL1(DLDT1A2CG-DLDT1A2C)                                   
              DC      C'TP NW'              TP/NSI/WEEKLY                       
DLDT1A2CG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2C3B    DS      0X                                                        
              DC       AL2(DLDT1A2C3BX-DLDT1A2C3B)                              
              DC       AL1(DLDT1A2C3BG-DLDT1A2C3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2C3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2C3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2C3B4CX-DLDT1A2C3B4C)                         
              DC        AL1(DLDT1A2C3B4CG-DLDT1A2C3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2C3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2C3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2C3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2C3B4BX-DLDT1A2C3B4B)                         
              DC        AL1(DLDT1A2C3B4BG-DLDT1A2C3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2C3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2C3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2C3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2C3B4AX-DLDT1A2C3B4A)                         
              DC        AL1(DLDT1A2C3B4AG-DLDT1A2C3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2C3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2C3B4AX EQU      *                                                        
                                                                                
DLDT1A2C3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2C3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2C3A    DS      0X                                                        
              DC       AL2(DLDT1A2C3AX-DLDT1A2C3A)                              
              DC       AL1(DLDT1A2C3AG-DLDT1A2C3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2C3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2C3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2C3A4AX-DLDT1A2C3A4A)                         
              DC        AL1(DLDT1A2C3A4AG-DLDT1A2C3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2C3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2C3A4AX EQU      *                                                        
                                                                                
DLDT1A2C3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2C3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2C3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2CX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/MFX/USTV ***                                               
*                                                                               
DLDT1A2D      DS     0X                                                         
              DC      AL2(DLDT1A2DX-DLDT1A2D)                                   
              DC      AL1(DLDT1A2DG-DLDT1A2D)                                   
              DC      C'TP MT'              TP/MFX/USTV                         
DLDT1A2DG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2D3B    DS      0X                                                        
              DC       AL2(DLDT1A2D3BX-DLDT1A2D3B)                              
              DC       AL1(DLDT1A2D3BG-DLDT1A2D3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2D3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2D3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2D3B4CX-DLDT1A2D3B4C)                         
              DC        AL1(DLDT1A2D3B4CG-DLDT1A2D3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2D3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2D3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2D3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2D3B4BX-DLDT1A2D3B4B)                         
              DC        AL1(DLDT1A2D3B4BG-DLDT1A2D3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2D3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2D3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2D3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2D3B4AX-DLDT1A2D3B4A)                         
              DC        AL1(DLDT1A2D3B4AG-DLDT1A2D3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2D3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2D3B4AX EQU      *                                                        
                                                                                
DLDT1A2D3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2D3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2D3A    DS      0X                                                        
              DC       AL2(DLDT1A2D3AX-DLDT1A2D3A)                              
              DC       AL1(DLDT1A2D3AG-DLDT1A2D3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2D3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2D3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2D3A4AX-DLDT1A2D3A4A)                         
              DC        AL1(DLDT1A2D3A4AG-DLDT1A2D3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2D3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2D3A4AX EQU      *                                                        
                                                                                
DLDT1A2D3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2D3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2D3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2DX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = PAV/MFX/USTV ***                                              
*                                                                               
DLDT1A2E      DS     0X                                                         
              DC      AL2(DLDT1A2EX-DLDT1A2E)                                   
              DC      AL1(DLDT1A2EG-DLDT1A2E)                                   
              DC      C'PAVMT'              PAV/MFX/USTV                        
DLDT1A2EG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2E3B    DS      0X                                                        
              DC       AL2(DLDT1A2E3BX-DLDT1A2E3B)                              
              DC       AL1(DLDT1A2E3BG-DLDT1A2E3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2E3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2E3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2E3B4CX-DLDT1A2E3B4C)                         
              DC        AL1(DLDT1A2E3B4CG-DLDT1A2E3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2E3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2E3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2E3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2E3B4BX-DLDT1A2E3B4B)                         
              DC        AL1(DLDT1A2E3B4BG-DLDT1A2E3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2E3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2E3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2E3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2E3B4AX-DLDT1A2E3B4A)                         
              DC        AL1(DLDT1A2E3B4AG-DLDT1A2E3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2E3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2E3B4AX EQU      *                                                        
                                                                                
DLDT1A2E3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2E3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2E3A    DS      0X                                                        
              DC       AL2(DLDT1A2E3AX-DLDT1A2E3A)                              
              DC       AL1(DLDT1A2E3AG-DLDT1A2E3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2E3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2E3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2E3A4AX-DLDT1A2E3A4A)                         
              DC        AL1(DLDT1A2E3A4AG-DLDT1A2E3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2E3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2E3A4AX EQU      *                                                        
                                                                                
DLDT1A2E3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2E3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2E3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2EX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/SRC/USTV ***                                               
*                                                                               
DLDT1A2F      DS     0X                                                         
              DC      AL2(DLDT1A2FX-DLDT1A2F)                                   
              DC      AL1(DLDT1A2FG-DLDT1A2F)                                   
              DC      C'TP ST'              TP/SRC/USTV                         
DLDT1A2FG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2F3B    DS      0X                                                        
              DC       AL2(DLDT1A2F3BX-DLDT1A2F3B)                              
              DC       AL1(DLDT1A2F3BG-DLDT1A2F3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2F3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2F3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2F3B4CX-DLDT1A2F3B4C)                         
              DC        AL1(DLDT1A2F3B4CG-DLDT1A2F3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2F3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2F3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2F3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2F3B4BX-DLDT1A2F3B4B)                         
              DC        AL1(DLDT1A2F3B4BG-DLDT1A2F3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2F3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2F3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2F3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2F3B4AX-DLDT1A2F3B4A)                         
              DC        AL1(DLDT1A2F3B4AG-DLDT1A2F3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2F3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2F3B4AX EQU      *                                                        
                                                                                
DLDT1A2F3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2F3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2F3A    DS      0X                                                        
              DC       AL2(DLDT1A2F3AX-DLDT1A2F3A)                              
              DC       AL1(DLDT1A2F3AG-DLDT1A2F3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2F3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2F3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2F3A4AX-DLDT1A2F3A4A)                         
              DC        AL1(DLDT1A2F3A4AG-DLDT1A2F3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2F3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2F3A4AX EQU      *                                                        
                                                                                
DLDT1A2F3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2F3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2F3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2FX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = PAV/SRC/USTV ***                                              
*                                                                               
DLDT1A2G      DS     0X                                                         
              DC      AL2(DLDT1A2GX-DLDT1A2G)                                   
              DC      AL1(DLDT1A2GG-DLDT1A2G)                                   
              DC      C'PAVST'              PAV/SRC/USTV                        
DLDT1A2GG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2G3B    DS      0X                                                        
              DC       AL2(DLDT1A2G3BX-DLDT1A2G3B)                              
              DC       AL1(DLDT1A2G3BG-DLDT1A2G3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2G3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2G3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2G3B4CX-DLDT1A2G3B4C)                         
              DC        AL1(DLDT1A2G3B4CG-DLDT1A2G3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2G3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2G3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2G3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2G3B4BX-DLDT1A2G3B4B)                         
              DC        AL1(DLDT1A2G3B4BG-DLDT1A2G3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2G3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2G3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2G3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2G3B4AX-DLDT1A2G3B4A)                         
              DC        AL1(DLDT1A2G3B4AG-DLDT1A2G3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2G3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2G3B4AX EQU      *                                                        
                                                                                
DLDT1A2G3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2G3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2G3A    DS      0X                                                        
              DC       AL2(DLDT1A2G3AX-DLDT1A2G3A)                              
              DC       AL1(DLDT1A2G3AG-DLDT1A2G3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2G3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2G3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2G3A4AX-DLDT1A2G3A4A)                         
              DC        AL1(DLDT1A2G3A4AG-DLDT1A2G3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2G3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2G3A4AX EQU      *                                                        
                                                                                
DLDT1A2G3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2G3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2G3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2GX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/ARB/RADIO ***                                              
*                                                                               
DLDT1A2H      DS     0X                                                         
              DC      AL2(DLDT1A2HX-DLDT1A2H)                                   
              DC      AL1(DLDT1A2HG-DLDT1A2H)                                   
              DC      C'TP AR'              TP/ARB/RADIO                        
DLDT1A2HG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2H3B    DS      0X                                                        
              DC       AL2(DLDT1A2H3BX-DLDT1A2H3B)                              
              DC       AL1(DLDT1A2H3BG-DLDT1A2H3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2H3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2H3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2H3B4CX-DLDT1A2H3B4C)                         
              DC        AL1(DLDT1A2H3B4CG-DLDT1A2H3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2H3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2H3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2H3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2H3B4BX-DLDT1A2H3B4B)                         
              DC        AL1(DLDT1A2H3B4BG-DLDT1A2H3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2H3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2H3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2H3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2H3B4AX-DLDT1A2H3B4A)                         
              DC        AL1(DLDT1A2H3B4AG-DLDT1A2H3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2H3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2H3B4AX EQU      *                                                        
                                                                                
DLDT1A2H3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2H3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2H3A    DS      0X                                                        
              DC       AL2(DLDT1A2H3AX-DLDT1A2H3A)                              
              DC       AL1(DLDT1A2H3AG-DLDT1A2H3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2H3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2H3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2H3A4AX-DLDT1A2H3A4A)                         
              DC        AL1(DLDT1A2H3A4AG-DLDT1A2H3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2H3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2H3A4AX EQU      *                                                        
                                                                                
DLDT1A2H3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2H3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2H3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2HX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/NHT/NETWORK ***                                            
*                                                                               
DLDT1A2J      DS     0X                                                         
              DC      AL2(DLDT1A2JX-DLDT1A2J)                                   
              DC      AL1(DLDT1A2JG-DLDT1A2J)                                   
              DC      C'TP HN'              TP/NHT/NETWORK                      
DLDT1A2JG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2J3B    DS      0X                                                        
              DC       AL2(DLDT1A2J3BX-DLDT1A2J3B)                              
              DC       AL1(DLDT1A2J3BG-DLDT1A2J3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2J3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2J3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2J3B4CX-DLDT1A2J3B4C)                         
              DC        AL1(DLDT1A2J3B4CG-DLDT1A2J3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2J3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2J3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2J3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2J3B4BX-DLDT1A2J3B4B)                         
              DC        AL1(DLDT1A2J3B4BG-DLDT1A2J3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2J3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2J3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2J3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2J3B4AX-DLDT1A2J3B4A)                         
              DC        AL1(DLDT1A2J3B4AG-DLDT1A2J3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2J3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2J3B4AX EQU      *                                                        
                                                                                
DLDT1A2J3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2J3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2J3A    DS      0X                                                        
              DC       AL2(DLDT1A2J3AX-DLDT1A2J3A)                              
              DC       AL1(DLDT1A2J3AG-DLDT1A2J3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2J3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2J3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2J3A4AX-DLDT1A2J3A4A)                         
              DC        AL1(DLDT1A2J3A4AG-DLDT1A2J3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2J3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2J3A4AX EQU      *                                                        
                                                                                
DLDT1A2J3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2J3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2J3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2JX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = PAV/NHT/NETWORK ***                                           
*                                                                               
DLDT1A2K      DS     0X                                                         
              DC      AL2(DLDT1A2KX-DLDT1A2K)                                   
              DC      AL1(DLDT1A2KG-DLDT1A2K)                                   
              DC      C'PAVHN'              PAV/NHT/NETWORK                     
DLDT1A2KG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2K3B    DS      0X                                                        
              DC       AL2(DLDT1A2K3BX-DLDT1A2K3B)                              
              DC       AL1(DLDT1A2K3BG-DLDT1A2K3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2K3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2K3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2K3B4CX-DLDT1A2K3B4C)                         
              DC        AL1(DLDT1A2K3B4CG-DLDT1A2K3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2K3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2K3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2K3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2K3B4BX-DLDT1A2K3B4B)                         
              DC        AL1(DLDT1A2K3B4BG-DLDT1A2K3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2K3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2K3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2K3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2K3B4AX-DLDT1A2K3B4A)                         
              DC        AL1(DLDT1A2K3B4AG-DLDT1A2K3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2K3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2K3B4AX EQU      *                                                        
                                                                                
DLDT1A2K3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2K3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2K3A    DS      0X                                                        
              DC       AL2(DLDT1A2K3AX-DLDT1A2K3A)                              
              DC       AL1(DLDT1A2K3AG-DLDT1A2K3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2K3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2K3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2K3A4AX-DLDT1A2K3A4A)                         
              DC        AL1(DLDT1A2K3A4AG-DLDT1A2K3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2K3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2K3A4AX EQU      *                                                        
                                                                                
DLDT1A2K3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2K3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2K3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2KX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/NSI/CANTV ***                                              
*                                                                               
DLDT1A2L      DS     0X                                                         
              DC      AL2(DLDT1A2LX-DLDT1A2L)                                   
              DC      AL1(DLDT1A2LG-DLDT1A2L)                                   
              DC      C'TP NC'              TP/NSI/CANTV                        
DLDT1A2LG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2L3B    DS      0X                                                        
              DC       AL2(DLDT1A2L3BX-DLDT1A2L3B)                              
              DC       AL1(DLDT1A2L3BG-DLDT1A2L3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2L3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2L3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2L3B4CX-DLDT1A2L3B4C)                         
              DC        AL1(DLDT1A2L3B4CG-DLDT1A2L3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2L3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2L3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2L3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2L3B4BX-DLDT1A2L3B4B)                         
              DC        AL1(DLDT1A2L3B4BG-DLDT1A2L3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2L3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2L3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2L3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2L3B4AX-DLDT1A2L3B4A)                         
              DC        AL1(DLDT1A2L3B4AG-DLDT1A2L3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2L3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2L3B4AX EQU      *                                                        
                                                                                
DLDT1A2L3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2L3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2L3A    DS      0X                                                        
              DC       AL2(DLDT1A2L3AX-DLDT1A2L3A)                              
              DC       AL1(DLDT1A2L3AG-DLDT1A2L3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2L3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2L3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2L3A4AX-DLDT1A2L3A4A)                         
              DC        AL1(DLDT1A2L3A4AG-DLDT1A2L3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2L3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2L3A4AX EQU      *                                                        
                                                                                
DLDT1A2L3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2L3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2L3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2LX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/BBM/CANTV ***                                              
*                                                                               
DLDT1A2M      DS     0X                                                         
              DC      AL2(DLDT1A2MX-DLDT1A2M)                                   
              DC      AL1(DLDT1A2MG-DLDT1A2M)                                   
              DC      C'TP AC'              TP/BBM/CANTV                        
DLDT1A2MG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2M3B    DS      0X                                                        
              DC       AL2(DLDT1A2M3BX-DLDT1A2M3B)                              
              DC       AL1(DLDT1A2M3BG-DLDT1A2M3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2M3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2M3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2M3B4CX-DLDT1A2M3B4C)                         
              DC        AL1(DLDT1A2M3B4CG-DLDT1A2M3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2M3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2M3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2M3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2M3B4BX-DLDT1A2M3B4B)                         
              DC        AL1(DLDT1A2M3B4BG-DLDT1A2M3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2M3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2M3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2M3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2M3B4AX-DLDT1A2M3B4A)                         
              DC        AL1(DLDT1A2M3B4AG-DLDT1A2M3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2M3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2M3B4AX EQU      *                                                        
                                                                                
DLDT1A2M3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2M3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2M3A    DS      0X                                                        
              DC       AL2(DLDT1A2M3AX-DLDT1A2M3A)                              
              DC       AL1(DLDT1A2M3AG-DLDT1A2M3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2M3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2M3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2M3A4AX-DLDT1A2M3A4A)                         
              DC        AL1(DLDT1A2M3A4AG-DLDT1A2M3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2M3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2M3A4AX EQU      *                                                        
                                                                                
DLDT1A2M3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2M3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2M3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2MX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*** FIL/SRC/MED = TP/ARB/USTV ***                                               
*                                                                               
DLDT1A2N      DS     0X                                                         
              DC      AL2(DLDT1A2NX-DLDT1A2N)                                   
              DC      AL1(DLDT1A2NG-DLDT1A2N)                                   
              DC      C'TP AT'              TP/ARB/USTV                         
DLDT1A2NG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2N3B    DS      0X                                                        
              DC       AL2(DLDT1A2N3BX-DLDT1A2N3B)                              
              DC       AL1(DLDT1A2N3BG-DLDT1A2N3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2N3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2N3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2N3B4CX-DLDT1A2N3B4C)                         
              DC        AL1(DLDT1A2N3B4CG-DLDT1A2N3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2N3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2N3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2N3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2N3B4BX-DLDT1A2N3B4B)                         
              DC        AL1(DLDT1A2N3B4BG-DLDT1A2N3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2N3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2N3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2N3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2N3B4AX-DLDT1A2N3B4A)                         
              DC        AL1(DLDT1A2N3B4AG-DLDT1A2N3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2N3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2N3B4AX EQU      *                                                        
                                                                                
DLDT1A2N3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2N3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2N3A    DS      0X                                                        
              DC       AL2(DLDT1A2N3AX-DLDT1A2N3A)                              
              DC       AL1(DLDT1A2N3AG-DLDT1A2N3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2N3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2N3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2N3A4AX-DLDT1A2N3A4A)                         
              DC        AL1(DLDT1A2N3A4AG-DLDT1A2N3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2N3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2N3A4AX EQU      *                                                        
                                                                                
DLDT1A2N3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2N3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2N3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2NX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*** FIL/SRC/MED = RTP/RAR/RADIO ***                                             
*                                                                               
DLDT1A2O      DS     0X                                                         
              DC      AL2(DLDT1A2OX-DLDT1A2O)                                   
              DC      AL1(DLDT1A2OG-DLDT1A2O)                                   
              DC      C'TP RR'              TP/ARB/RADIO                        
DLDT1A2OG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2O3B    DS      0X                                                        
              DC       AL2(DLDT1A2O3BX-DLDT1A2O3B)                              
              DC       AL1(DLDT1A2O3BG-DLDT1A2O3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2O3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2O3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2O3B4CX-DLDT1A2O3B4C)                         
              DC        AL1(DLDT1A2O3B4CG-DLDT1A2O3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2O3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2O3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2O3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2O3B4BX-DLDT1A2O3B4B)                         
              DC        AL1(DLDT1A2O3B4BG-DLDT1A2O3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2O3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2O3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2O3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2O3B4AX-DLDT1A2O3B4A)                         
              DC        AL1(DLDT1A2O3B4AG-DLDT1A2O3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2O3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2O3B4AX EQU      *                                                        
                                                                                
DLDT1A2O3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2O3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2O3A    DS      0X                                                        
              DC       AL2(DLDT1A2O3AX-DLDT1A2O3A)                              
              DC       AL1(DLDT1A2O3AG-DLDT1A2O3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2O3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2O3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2O3A4AX-DLDT1A2O3A4A)                         
              DC        AL1(DLDT1A2O3A4AG-DLDT1A2O3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2O3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2O3A4AX EQU      *                                                        
                                                                                
DLDT1A2O3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2O3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2O3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2OX     EQU    *                                                          
*                                                                               
****                                                                            
*** FIL/SRC/MED = CTP/NSI/USTV ***  COWORDTY COVERAGE                           
*                                                                               
DLDT1A2PA     DS     0X                                                         
              DC     AL2(DLDT1A2PAX-DLDT1A2PA)                                  
              DC     AL1(DLDT1A2PAG-DLDT1A2PA)                                  
****          DC      C'TP NU'              TP/NSI/USTV                         
              DC      C'CTPNU'             CTP/NSI/USTV                         
DLDT1A2PAG    EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2P3B    DS      0X                                                        
              DC     AL2(DLDT1A2P3BX-DLDT1A2P3B)                                
              DC     AL1(DLDT1A2P3BG-DLDT1A2P3B)                                
              DC     AL2(FMHBKST)         BOOKS FOR STATION                     
DLDT1A2P3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2P3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2P3B4CX-DLDT1A2P3B4C)                         
              DC        AL1(DLDT1A2P3B4CG-DLDT1A2P3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2P3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2P3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2P3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2P3B4BX-DLDT1A2P3B4B)                         
              DC        AL1(DLDT1A2P3B4BG-DLDT1A2P3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2P3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2P3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2P3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2P3B4AX-DLDT1A2P3B4A)                         
              DC        AL1(DLDT1A2P3B4AG-DLDT1A2P3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2P3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2P3B4AX EQU      *                                                        
                                                                                
DLDT1A2P3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2P3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2P3A    DS      0X                                                        
              DC       AL2(DLDT1A2P3AX-DLDT1A2P3A)                              
              DC       AL1(DLDT1A2P3AG-DLDT1A2P3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2P3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2P3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2P3A4AX-DLDT1A2P3A4A)                         
              DC        AL1(DLDT1A2P3A4AG-DLDT1A2P3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2P3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2P3A4AX EQU      *                                                        
                                                                                
DLDT1A2P3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2P3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2P3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2PAX    EQU    *                                                          
********************************************************                        
*                                                                               
*** FIL/SRC/MED = OTP/NSI/USTV ***  TP OVERNIGHTS                               
*                                                                               
DLDT1A2QA     DS     0X                                                         
              DC     AL2(DLDT1A2QAX-DLDT1A2QA)                                  
              DC     AL1(DLDT1A2QAG-DLDT1A2QA)                                  
              DC      C'TP NO'                                                  
DLDT1A2QAG    EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2Q3B    DS      0X                                                        
              DC     AL2(DLDT1A2Q3BX-DLDT1A2Q3B)                                
              DC     AL1(DLDT1A2Q3BG-DLDT1A2Q3B)                                
              DC     AL2(FMHBKST)         BOOKS FOR STATION                     
DLDT1A2Q3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2Q3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2Q3B4CX-DLDT1A2Q3B4C)                         
              DC        AL1(DLDT1A2Q3B4CG-DLDT1A2Q3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2Q3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2Q3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2Q3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2Q3B4BX-DLDT1A2Q3B4B)                         
              DC        AL1(DLDT1A2Q3B4BG-DLDT1A2Q3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2Q3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2Q3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2Q3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2Q3B4AX-DLDT1A2Q3B4A)                         
              DC        AL1(DLDT1A2Q3B4AG-DLDT1A2Q3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2Q3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2Q3B4AX EQU      *                                                        
                                                                                
DLDT1A2Q3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2Q3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2Q3A    DS      0X                                                        
              DC       AL2(DLDT1A2Q3AX-DLDT1A2Q3A)                              
              DC       AL1(DLDT1A2Q3AG-DLDT1A2Q3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2Q3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2Q3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2Q3A4AX-DLDT1A2Q3A4A)                         
              DC        AL1(DLDT1A2Q3A4AG-DLDT1A2Q3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2Q3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2Q3A4AX EQU      *                                                        
                                                                                
DLDT1A2Q3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2Q3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2Q3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2QAX    EQU    *                                                          
*                                                                               
         EJECT                                                                  
*********************************************************                       
*** FIL/SRC/MED = TP/FUS/USTV ***  TP FUSION                                    
*                                                                               
DLDT1A2RA     DS     0X                                                         
              DC     AL2(DLDT1A2RAX-DLDT1A2RA)                                  
              DC     AL1(DLDT1A2RAG-DLDT1A2RA)                                  
              DC      C'TP FT'                                                  
DLDT1A2RAG    EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2R3B    DS      0X                                                        
              DC     AL2(DLDT1A2R3BX-DLDT1A2R3B)                                
              DC     AL1(DLDT1A2R3BG-DLDT1A2R3B)                                
              DC     AL2(FMHBKST)         BOOKS FOR STATION                     
DLDT1A2R3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2R3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2R3B4CX-DLDT1A2R3B4C)                         
              DC        AL1(DLDT1A2R3B4CG-DLDT1A2R3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2R3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2R3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2R3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2R3B4BX-DLDT1A2R3B4B)                         
              DC        AL1(DLDT1A2R3B4BG-DLDT1A2R3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2R3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2R3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2R3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2R3B4AX-DLDT1A2R3B4A)                         
              DC        AL1(DLDT1A2R3B4AG-DLDT1A2R3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2R3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2R3B4AX EQU      *                                                        
                                                                                
DLDT1A2R3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2R3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*********************************************************                       
*** FIL/SRC/MED = TP/FUS/USTV ***  TP FUSION                                    
*                                                                               
DLDT1A2R3A    DS      0X                                                        
              DC       AL2(DLDT1A2R3AX-DLDT1A2R3A)                              
              DC       AL1(DLDT1A2R3AG-DLDT1A2R3A)                              
              DC       AL2(FMHMKST)        MARKETS FOR STATION                  
DLDT1A2R3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2R3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2R3A4AX-DLDT1A2R3A4A)                         
              DC        AL1(DLDT1A2R3A4AG-DLDT1A2R3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2R3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2R3A4AX EQU      *                                                        
                                                                                
DLDT1A2R3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2R3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
***** ELEMENT MAP CODE = FMHMKST *******                                        
*                                                                               
DLDT1A2S3A    DS      0X                                                        
              DC       AL2(DLDT1A2S3AX-DLDT1A2S3A)                              
              DC       AL1(DLDT1A2S3AG-DLDT1A2S3A)                              
              DC       AL2(FMHSYSCD)        SYSCODES FOR MARKET                 
DLDT1A2S3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2S3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2S3A4AX-DLDT1A2S3A4A)                         
              DC        AL1(DLDT1A2S3A4AG-DLDT1A2S3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2S3A4AG EQU      *                                                        
              DC        AL2(FMDDMSONAME)    MSO ANME                            
              DC        AL2(FMDDSYSCODE)    SYSCODE                             
              DC        AL2(FMDDSYSNAME)    SYSCODE NAME                        
              DC        AL2(FMDDSYSAMKT)    ALPHA MKT                           
              DC        AL2(FMDDSYSNMKT)    NUMERIC MKT                         
              DC        AL2(FMDDSYSSBAS)    SUBSCRIBER BASE                     
              DC        AL2(FMDDSYSUNIV)    UNIVERSE                            
              DC        AL2(0)                                                  
DLDT1A2S3A4AX EQU      *                                                        
                                                                                
DLDT1A2S3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2S3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2R3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2RAX    EQU    *                                                          
*                                                                               
*                                                                               
*********************************************************                       
*** FIL/SRC/MED = OPA/NSI/USTV ***  PAV OVERNIGHTS                              
*                                                                               
DLDT1A2T      DS     0X                                                         
              DC      AL2(DLDT1A2TX-DLDT1A2T)                                   
              DC      AL1(DLDT1A2TG-DLDT1A2T)                                   
              DC      C'PAVNO'              PAV/NSI/USTV                        
DLDT1A2TG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBKST ****                                            
*                                                                               
DLDT1A2T3B    DS      0X                                                        
              DC       AL2(DLDT1A2T3BX-DLDT1A2T3B)                              
              DC       AL1(DLDT1A2T3BG-DLDT1A2T3B)                              
              DC       AL2(FMHBKST)         BOOKS FOR STATION                   
DLDT1A2T3BG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2T3B4C  DS       0X                                                       
              DC        AL2(DLDT1A2T3B4CX-DLDT1A2T3B4C)                         
              DC        AL1(DLDT1A2T3B4CG-DLDT1A2T3B4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2T3B4CG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2T3B4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER2 *****                                          
*                                                                               
DLDT1A2T3B4B  DS       0X                                                       
              DC        AL2(DLDT1A2T3B4BX-DLDT1A2T3B4B)                         
              DC        AL1(DLDT1A2T3B4BG-DLDT1A2T3B4B)                         
              DC        AL4(XTRCVER2)                                           
DLDT1A2T3B4BG EQU      *                                                        
              DC        AL2(FMDBKSTGRP1)    GROUP #1 DATA                       
              DC        AL2(FMDBKSTGRP2)    GROUP #2 DATA                       
              DC        AL2(FMDBKSTDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2T3B4BX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2T3B4A  DS       0X                                                       
              DC        AL2(DLDT1A2T3B4AX-DLDT1A2T3B4A)                         
              DC        AL1(DLDT1A2T3B4AG-DLDT1A2T3B4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2T3B4AG EQU      *                                                        
              DC        AL2(FMDBKSTSTA)     STATION                             
              DC        AL2(FMDBKSTBOOK)    BOOK                                
              DC        AL2(FMDBKSTBTYP)    BOOK TYPE                           
              DC        AL2(FMDBKSTAFF)     AFFILIATION                         
              DC        AL2(FMDBKSTFLG1)    FLAGS                               
              DC        AL2(FMDBKSTSPLL)     SPILL?                             
              DC        AL2(FMDBKSTPUBL)     PUBLIC?                            
              DC        AL2(FMDBKSTCABL)     CABLE?                             
              DC        AL2(0)                                                  
DLDT1A2T3B4AX EQU      *                                                        
                                                                                
DLDT1A2T3B4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2T3BX   EQU     *                                                         
         EJECT                                                                  
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHMKST ****                                            
*                                                                               
DLDT1A2T3A    DS      0X                                                        
              DC       AL2(DLDT1A2T3AX-DLDT1A2T3A)                              
              DC       AL1(DLDT1A2T3AG-DLDT1A2T3A)                              
              DC       AL2(FMHMKST)         MARKETS FOR STATION                 
DLDT1A2T3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2T3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2T3A4AX-DLDT1A2T3A4A)                         
              DC        AL1(DLDT1A2T3A4AG-DLDT1A2T3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2T3A4AG EQU      *                                                        
              DC        AL2(FMDMKSTAMKT)    ALPHA MARKET                        
              DC        AL2(FMDMKSTNMKT)    NUMERIC MARKET                      
              DC        AL2(FMDMKSTMNAM)    MARKET NAME                         
              DC        AL2(FMDMKSTMREL)    MARKET'S RELATION TO STTN           
              DC        AL2(0)                                                  
DLDT1A2T3A4AX EQU      *                                                        
                                                                                
DLDT1A2T3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2T3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2T3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2TX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
* FOLLOWING IS THE END OF THE DOWNLOADABLE TABLES APPEND NEW                    
* TABLES FOR NEW FILE/SOURCES BEFORE THIS POINT                                 
DLDT1A2       DS     0X                                                         
              DC      AL2(0)                                                    
DLDT1AX       EQU   *                                                           
*                                                                               
         EJECT                                                                  
DLDT1         DS    0X                                                          
              DC     AL2(0)                                                     
DLDATTABX     EQU  *                                                            
***********************************************************************         
         EJECT                                                                  
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
ANYDATA  DS    CL1                 ANY DATA FOR THIS REQUEST? (Y/N)             
         ORG                                                                    
         EJECT                                                                  
DM03WRKD DSECT                                                                  
DM03W_RD DS    A                   DEM03'S RD                                   
DM03WRKX EQU   *                                                                
DM03WRKL EQU   DM03WRKX-DM03WRKD                                                
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
*                                                                               
MYDMCB   DS    6F                                                               
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
ALOCWRK  DS    A                   A(LOC WRK AREA)-USED B/W SUBRTN POOL         
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
MYDATDSP DS    H                   DATA DISPLACEMENT                            
MYELCODE DS    XL1                 ELEMENT CODE                                 
SAMEDMAF DS    CL1                 SAME DMA FLAG (Y/N)                          
NEWBTF   DS    C                   Y/N                                          
BTFMKT   DS    XL2                                                              
MARKET   DS    XL2                 MARKET NUMBER                                
KMARKET  DS    XL2                 SPILL MARKET NUMBER                          
KBTYP    DS    XL1                 BOOK TYPE                                    
KBOOK    DS    XL2                 BOOK                                         
STACALL  DS    CL5                 STATION CALL LETTERS                         
PREVSTTN DS    CL(L'STACALL)                                                    
AFFILS   DS    CL5                 STATION'S AFFILIATES                         
MARNAME  DS    CL30                MARKET NAME                                  
SVMARNAM DS    CL(L'MARNAME)       MARKET NAME                                  
MARRALF  DS    CL3                 RATING SVCE ALPHA MKT CODE                   
MARNMKT  DS    XL2                 NUMERIC MARKET                               
MSONAME  DS    CL(L'FANMSONM)      MSO NAME                                     
MYSYSCDE DS    XL2                 SYSCODE                                      
SYSCNAME DS    CL(L'FANSYSNM)      SYSCODE NAME                                 
SUBBASE  DS    F                   SUBSRIBER BASE                               
MSOUNIV  DS    F                   UNIVERSE                                     
DMAWSYS  DS    X                   DMA/WIRED                                    
TMPRTYP  DS    XL(L'TDRRTYP)       TEMP STORAGE FOR TSAR DEM RECD TYP           
GOSUBN   DS    X                   SUBROUTINE NUMBER                            
STARTDAY DS    XL1                 NSIWEEK START DAY (1-7)                      
STARTSYS DS    XL2                                                              
ADLDNTRY DS    A                                                                
ASUBRTN  DS    A                                                                
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUBR01 POOL INTERFACE)                     
ADLDTABS DS    A                                                                
CDEMTABS DS    A                   A(DEMTABS)                                   
ABTPTBNH DS    A                   A(BTPTPNHT)                                  
RECNUM   DS    H                   (BINSRCH/TSAR) RECORD NUMBER                 
MYD32DLM DS    CL(L'SPOVDLMT)      DELIMITERS IN DEM32 DOWNLOAD                 
*                                 PICK-UP FROM LAST LEFT OFF FLAGS              
PUFLLOO  DS    CL1                 RE-ESTABLISHED MAP CODES ?                   
PUFLLOO2 DS    CL1                 RE-ESTABLISHED MONTH/WEEK NO. ?              
*                                                                               
BKCUTOFF DS    XL2                 BOOK CUTOFF YY/MM                            
*                                                                               
DBLOCK2  DS    CL256               SECOND DBLOCK                                
         DS    0F                  (FULL-WORD ALIGN TSAR BLOCK)                 
MYTSRBLK DS    XL(TSARDL)          TSAR BLOCK FOR THIS APPLIC'S USE             
MYTSRREC DS    XL(TSDEMRCQ)        TSAR I/O AREA FOR THIS APPLIC'S USE          
                                                                                
                                                                                
DEMTMPL  EQU   *-DEMTMPD                                                        
         DS    0CL((APWORKX-APWORK)-DEMTMPL+1)                                  
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINMNAM  DS    CL30                MARKET NAME                                  
BINRMKT  DS    XL2                 MARKET NUMBER                                
BINKMKT  DS    XL2                 SPILL MARKET NUMBER                          
BINRALF  DS    CL3                 RATING SVCE ALPHA MKT CODE                   
BINBOOK  DS    XL2                 BOOK                                         
BINBTYP  DS    XL1                 BOOK TYPE                                    
BINEND1  EQU   *                                                                
BINSTAT  DS    CL5                 STATION (FOR SPILL-IN ONLY)                  
BINAFFL  DS    CL5                 STATION'S AFFILIATES                         
BINEND2  EQU   *                                                                
         ORG   BINRECD                                                          
BINMSON  DS    CL25                MSO NAME                                     
BINSYSC  DS    XL2                 SYSCODE                                      
BINSYSN  DS    CL25                SYSCODE NAME                                 
BINSYSMK DS    XL3                 MARKET CODE                                  
BINSSTAT DS    CL5                 STATION                                      
BINSYSMN DS    XL25                SYSCODE MKT NAME                             
BINSBASE DS    XL4                 SUBSCRIBER BASE                              
BINSUNV  DS    XL4                 UNIVERSE                                     
BINDMAW  DS    C                   DMA/WIRED                                    
BINEND3  EQU   *                                                                
         EJECT                                                                  
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  TSAR DEMO RECORD KEY                         
TDRRTYP  DS    XL1                  RECORD TYPE (SEE BELOW)                     
                                                                                
TDRSIREC DS    0C                  SPILL-IN (ACTN=STATION)                      
TDRSIRMK DS    XL2                  RATING SVCE MKT #                           
TDRSIKMK DS    XL2                  SPILL MKT #                                 
TDRSISTA DS    CL5                  STATION CALL LETTERS                        
TDRSIBOK DS    XL2                  BOOK                                        
TDRSIBTP DS    XL1                  BOOK TYPE                                   
TDRSIKYL EQU   *-TDRKEY            KEY LENGTH                                   
TDRSIAFL DS    CL5                 AFFILIATIONS                                 
         DS    CL11                EXTRA SPACE FOR COUNTY NAME                  
TDRCNAML EQU   *-TDRSIAFL                                                       
TDRSIRCL EQU   *-TSDEMRCD          RECORD LENGTH                                
                                                                                
         ORG   TDRSIREC                                                         
TDRI2REC DS    0X                  SPILL-IN (ACTN=STATION) FOR DEM32            
TDRI2RMK DS     XL2                 RATING SVCE MKT #                           
TDRI2STA DS     CL5                 STATION CALL LETTERS                        
TDRI2KMK DS     XL2                 SPILL MKT #                                 
TDRI2BTP DS     CL1                 BOOK TYPE                                   
TDRI2BKY DS     XL1                 BOOK YEAR                                   
*                                                                               
TDRI2XPL DS     CL1                INDICATOR IF RECOR IS XTRA SPILL             
*                          CUZ X'E0' SI TAKEN OUT OF BKTYPE IN TSAR             
*                          RECORDS FOR SORTING PURPOSES                         
*                                                                               
TDRI2KYL EQU   *-TDRKEY                                                         
                                                                                
         ORG   TDRSIREC                                                         
TDRSOREC DS    0C                  SPILL-OUT (ACTN=SPILL)                       
TDRSOMNA DS    CL30                 MARKET NAME                                 
TDRSOKYL EQU   *-TDRKEY            KEY LENGTH                                   
TDRSORMK DS    XL2                  RATING SVCE MKT #                           
TDRSORAM DS    CL3                  RATING SVCE ALPHA MKT CODE                  
TDRSOKMK DS    XL2                  SPILL MKT #                                 
TDRSOBTP DS    XL1                  BOOK TYPE                                   
TDRSOBOK DS    XL2                  BOOK                                        
TDRSORCL EQU   *-TSDEMRCD          RECORD LENGTH                                
                                                                                
         ORG   TDRSOREC                                                         
TDRHDREC DS    0C                  COLUMN HEADERS                               
TDRHDLIN DS    XL1                  HEADLINE NUMBER                             
                                                                                
         ORG                                                                    
TSDEMRCQ EQU   *-TSDEMRCD                                                       
*                                                                               
         ORG   TDRSIREC                                                         
TDRI3REC DS    0X                  MSO SYSCODE LISTINF FOR DEM32                
TDRI3SCD DS     XL2                 SYSCODE #                                   
TDRI3SYN DS     CL25                SYSCODE NAME                                
TDRI3MSO DS     CL25                MSONAME                                     
TDRI3AMK DS     CL5                 ALPHAMKT                                    
TDRI3KYL EQU   *-TDRKEY                                                         
TDRIMKNM DS     CL25                MARKER NAME                                 
TDRI3NMK DS     XL2                 NUMERIC MKT                                 
TDRI3SBS DS     XL4                 SUBSCRIBER BASE                             
TDRI3UNV DS     XL4                 UNIVERSE                                    
*                                                                               
*                                                                               
TDRI3RCQ EQU   *-TSDEMRCD                                                       
                                                                                
* TDRRTYP can have the following values:                                        
TDRRTINI EQU   X'10'               RECORD ALLOWS FOR INITIALIZATION             
TDRRTMSG EQU   X'40'               RECORD CONTAINS INFO MESSAGE                 
TDRRTHDR EQU   X'80'               RECORD SIGNIFIES TO OUTPUT COL HDR           
TDRRTDRQ EQU   X'C0'               RECORD CONTAINS MARKET INFORMATION           
*TDRRSYSQ EQU   X'D0'               RECORD CONTAINS SYSCODE INFO                
TDRRTXFF EQU   XFF                 (RESERVED FOR DEM00)                         
                                                                                
* Element codes for TSAR DEMO RECORD                                            
TDREAFFQ EQU   X'10'               AFFILIATION                                  
TDREMWNQ EQU   X'20'               MONTH/WEEK NUMBERS (REV CHRON ORDER)         
*                                                                               
*                                                                               
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL2                 L(ENTRY)                                     
DLDTDSP  DS    XL1                 DISPLACEMENT TO DATA                         
DLDTFIXL EQU   *-DLDTABD                                                        
*                                                                               
DLDTDATA DS    0X                                                               
                                                                                
         ORG   DLDTDATA                                                         
DLDTRTYP DS     XL(L'TDRRTYP)       TSAR DEMO RECORD TYPE                       
                                                                                
         ORG   DLDTDATA                                                         
DLDTFMS  DS     0CL(L'DBFIL+L'DBSRC+L'DBMED)                                    
DLDTFIL  DS      CL(L'DBFIL)         FILE                                       
DLDTSRC  DS      CL(L'DBSRC)         SOURCE                                     
DLDTMED  DS      CL(L'DBMED)         MEDIA                                      
                                                                                
         ORG   DLDTDATA                                                         
DLDTEMPC DS     XL(L'FALEMPC)       ELEMENT MAP CODE                            
                                                                                
         ORG   DLDTDATA                                                         
DLDTVRSN DS    XL(L'D32PCVER)       STEREO DEM EXTRACT VERSION                  
                                                                                
         ORG                                                                    
         EJECT                                                                  
APSAV2D  DSECT                                                                  
AS2EMPC  DS    XL(L'FALEMPC)       REMEMBER ELEMENT MAP CODE                    
AS2DMPC  DS    XL(L'FALDMPC)       REMEMBER DATA    MAP CODE                    
AS2STMOD DS    XL(L'STMODE)        REMEMBER STEREO MODE                         
AS2MWN   DS    XL1                 REMEMBER MONTH/WEEK #                        
AS2RSMF  DS    XL1                 RESUME FLAG                                  
AS2RFFLK EQU    X80                 RESUME FOR FALINK BREAK                     
APSAV2Q  EQU   *-APSAV2D                                                        
                                                                                
                                                                                
* DSECT TO COVER PREVIOUS VALUES OUTPUTTED TO STEREO                            
*                                                                               
SPVOVALD DSECT                                                                  
SPOVSETE DS    CL1                 SET ELEMENT YET?                             
SPOVDLMT DS    CL1                 DELIMITERS                                   
SPOVSTBT DS    XL((TDRI2BTP-TDRKEY)+L'TDRI2BTP) TDRKEY STTN/SPLL/BTYP           
SPOVSTA  DS    CL10                WABCT/PHI--CALL LETTERS(/SPILL MKT)          
SPOVAFF  DS    CL(L'AFFILS)        AFFILIATIONS                                 
SPOVFLG1 DS    XL1                 FLAGS                                        
SPVOVALQ EQU   *-SPVOVALD                                                       
         DS    0CL(L'STPRVOVL-SPVOVALQ+1)                                       
                                                                                
*FATWA                                                                          
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'136DEDEM03   10/23/17'                                      
         END                                                                    
