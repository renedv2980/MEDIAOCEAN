*          DATA SET DEDEM06    AT LEVEL 069 AS OF 06/15/11                      
*PHASE T21B06C,*                                                                
*INCLUDE NETUNBK                                                                
T21B06   TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION'                        
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* 02/06/03 059 BPOO - fix spill market canadian book list             *         
* MAR28/01 057 BPOO - BYPASS 6A-2A DATA IF BOOKTYPE = '05'            *         
* MAR13/01 056 BPOO - SHOW MONTHLY BOOKS FROM WEEKLYS AND             *         
*                     SEPERATE TP AND WTP BOOKS  FOR CANADA           *         
* JAN11/01 055 BPOO - SUPPORT RADAR FILE                              *         
* NOV06/00 054 BPOO - BBM WEEKLY                                      *         
* NOV04/00 053 BPOO - SUPPORT USERID AUTHORIZATION                    *         
* NOV02/00 052 BPOO - DEM TABLES MOVED TO DEM81 PHASE                 *         
* NOV01/00 051 BPOO - book '0000' mucking up download to pc           *         
* SEP22/00 050 BPOO - falink stuff...change screen one line lower     *         
* Jun20/00 049 GLEE - Change versioning scheme to support PC vrsn stmp*         
*                                                                     *         
* May24/00 048 GLEE - Some initial support for Network Cable TP file  *         
*                                                                     *         
* Mar08/00 047 GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Jan05/00 045 GLEE - Kill booktype if booktype is x'05' (NSI 2A-6A)  *         
*                                                                     *         
* Sep13/99 044 GLEE - Set CC upon exiting phase                       *         
*                                                                     *         
* Jul19/99 039 GLEE - Support both BBM monthly & weekly books         *         
*                                                                     *         
* May27/99 038 GLEE - Suppress Cable booktype for ACTN=NWSYNBKS       *         
*              GLEE - Suppress books prior to MAR94 for ACTN=NWSYNBKS *         
*                                                                     *         
* May14/99 037 GLEE - Kill booktype for DEM16 sessions since it never *         
*                      had booktypes until DEM32 came along           *         
*                                                                     *         
* May07/99 036 GLEE - Fix DEMLINE code to output dates for weekly bks *         
*                                                                     *         
* Apr09/99 035 GLEE - Support spill markets on mainframe sessions     *         
*                                                                     *         
* Mar18/99 034 GLEE - For media=N source=H, manual download "booktype"*         
*                      to DEM32                                       *         
*                                                                     *         
* Mar18/99 033 GLEE - For media=N, fudge TEL-H in if no station input *         
*                                                                     *         
* Mar08/99 032 GLEE - Kill booktype if booktype is extra spill        *         
*              GLEE - Change booktypes to uppercase                   *         
*                                                                     *         
* Feb11/99 031 GLEE - Kill booktype if book is prior to a certain date*         
*                                                                     *         
* Feb02/99 030 GLEE - Implement version (extract) dates for DEM32     *         
*                                                                     *         
* Nov17/98 029 GLEE - Suppress cable info for NSI PAV                 *         
*                                                                     *         
* Aug14/97 006 GLEE - New 4th parameter to NSIWEEK {A(DATCON)}        *         
*                                                                     *         
* Apr02/96 005 GLEE - Display proper book format for NSI Can TP       *         
*                                                                     *         
* Nov16/95 004 GLEE - Support for STEREO                              *         
*                                                                     *         
* Aug16/95 003 GLEE - Prepatory stage for STEREO support              *         
*                                                                     *         
* 10/21/94 002 GLEE - Support radio as well                           *         
*                                                                     *         
*  ??????   ?   ??  - HISTORY UNKNOWN                                 *         
***********************************************************************         
         EJECT                                                                  
DEM06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCWORKL,**DEM6**                                                
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         USING DM06WRKD,RC                                                      
*                                  HANDLE CONTROLLER MODE SETTINGS              
*                                                                               
         DS    0H                  SET UP ADCONS                                
         ST    RE,RELO06                                                        
         LH    R1,=Y(DISPTAB-DEM06)                                             
         LA    R1,DEM06(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM06_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM06(RE)        RE=A(TABLE)                                  
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM06_10                                                      
*                                                                               
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
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
*                                                                               
YES      SR    R9,R9                                                            
NO       LTR   R9,R9                                                            
EXIT     XMOD1 1                                                                
                                                                                
*                                                                               
XITMODH  DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     XITMODCR                                                         
                                                                                
XITMODL  DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     XITMODCR                                                         
                                                                                
XITMODE  DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     XITMODCR                                                         
                                                                                
XITMODCR DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     XITMOD               AND EXIT                                    
                                                                                
XITMOD   DS    0H                  EXITS TO DEM02 CALLER                        
         L     RD,DM06W_RD                                                      
         XMOD1                                                                  
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE                                                 
*                                                                               
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
         MVC   DEMHD1(L'HEADSTB1),HEADSTB1                                      
         MVC   DEMHD2(L'HEADSTB2),HEADSTB2                                      
         LA    R0,BINRECL                                                       
         ST    R0,BINLREC                                                       
         ST    R0,BINLKEY                                                       
         B     EXIT                                                             
                                                                                
                                                                                
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   TSKEYL,TDRKEYL      SET KEY LENGTH                               
                                                                                
         LA    R0,TDRRECL+2                                                     
         STH   R0,TSRECL           SET MAX LENGTH OF RECORD                     
*                                                                               
DEMHD20  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
*                                                                               
DEMPROC  LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1                                                  
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBFILE,DBFIL                                                     
                                                                                
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         DS    0H                  SET SPILL MARKET, IF ANY                     
         XC    SPILLMKT,SPILLMKT    INITIALIZE SPILL MARKET                     
                                                                                
         MVC   SPILLMKT,STAS+(STASPILL-STABKD)                                  
         OC    SPILLMKT,SPILLMKT    NUMERIC SPILL MKT INPUTTED?                 
         BNZ   DMPRC029              YEP, KEEP SPILL MKT AS NUMERIC             
                                                                                
         OC    ALFMKTS,ALFMKTS      ALPHA SPILL MKT INPUTTED?                   
         BNZ   *+14                  YEP, TRANSLATE TO NUMERIC                  
         CLC   ALFMKTS,SPACES                                                   
         BE    DMPRC029                                                         
                                                                                
         MVI   DBFUNCT,DBCNVA2N     TRANSLATE ALPHA TO NUMERIC MARKET           
         MVC   DBSELALF,ALFMKTS                                                 
* FOR FUSION READ NSI                                                           
         CLI   DBSRC,C'F'                                                       
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
* FOR OVERNIGHT READ MONTHLY NSI                                                
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0            ANY ERRORS?                                 
         BNE   *+10                                                             
         MVC   SPILLMKT,DBSELRMK     NO, SET NUMERIC MARKET AS SPILL            
         XC    DBSELRMK,DBSELRMK    CLEAR FIELD IN CASE 'TWAS FUDGED            
         MVI   DBERROR,0                                                        
* FOR FUSION WE READ NSI SO RESET BACK TO FUSION                                
         CLI   DBSRC,C'F'                                                       
         BNE   *+8                                                              
         MVI   DBSELSRC,C'F'                                                    
* FOR OVERNIGHT WE READ MONTHLY NSI SO RESET BACK TO OVERNIGHT                  
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   DBSELMED,C'O'                                                    
DMPRC029 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   DBFUNCT,DBFNC                                                    
         OC    STAS,STAS                                                        
         BNZ   DMPRC033X                                                        
         CLI   DBMED,C'N'           NOT APPLICABLE FOR NETWORK                  
         BE    DMPRC033X                                                        
         MVI   DBFUNCT,DBGETAMB     WRONG FUCKING CALL????                      
         CLI   DBMED,C'U'           COUNTY COVERAGE                             
         BNE   *+8                                                              
         MVI   DBFUNCT,DBGETAB      IM AN IDIOT                                 
DMPRC033X EQU  *                                                                
                                                                                
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELSTA,STAS                                                    
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
         DS    0H                                                               
         CLI   DBMED,C'N'          FOR MEDIA=NETWORK,                           
         BNE   DMPRC036X                                                        
         CLI   NSTAS,0              AND NO NETWORK INPUTTED,                    
         BNE   DMPRC036X                                                        
         CLI   DBSELSRC,C'H'                                                    
         BNE   *+10                                                             
         MVC   DBSELSTA,=C'TEL H'   FUDGE IN A NETWORK TO GET BOOKS             
         CLI   DBSELSRC,C'C'        IF NETWORK CABLE,                           
         BNE   *+10                                                             
         MVC   DBSELSTA,=C'AEN C'    FUDGE IN A CABLE NTWRK TO GET BKS          
DMPRC036X EQU  *                                                                
*                                  CALL DEMAND TO READ RECORDS                  
         GOTO1 ASETUSID                                                         
* NEW CODE BPOO                                                                 
*&&DO                                                                           
****     CLI   DBSRC,C'A'           BBM                                         
         CLI   DBMED,C'C'                                                       
         BNE   *+8                                                              
         MVI   DOWEEK+1,C'Y'        SET FLAG TO CONTROL THE WEEK                
         MVI   DOWEEK,X'FF'         AND MONTHLY PROCESSING                      
*                         SET FLAG TO DEFAULT LAST TIME PROCESSING              
*&&                                                                             
DMPRC037 DS    0H                                                               
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK,0                                   
*                                                                               
***      OC    STAS,STAS                                                        
***      BNZ   DMPRC038                                                         
*&&DO                                                                           
****     CLI   DBSRC,C'A'           BBM                                         
         CLI   DBMED,C'C'                                                       
         BNE   DMPRC038                                                         
         L     RE,ADEMFIL                                                       
***      CLC   =C'WTP',8(RE)       IF WEEKLY FILE DONT DO MONTHLY BOOKS         
***      BE    DMPRC038                                                         
         TM    DEMFLAG1,DF1STERO                                                
         BZ    DEMPRCX                                                          
                                                                                
         MVI   DOWEEK+1,C'N'       CHANGE FLAG TO MONTHLY BOOK PROCESS          
         CLI   DOWEEK,X'FF'        DONE WITH ALL BMM PROCESSING?                
         BNE   DMPRC037                                                         
***      BE    DMPRC038                                                         
***      CLI   DEMHKFLG,C'Y'       IF WE HAD ANY BOOKS AT ALL                   
***      BE    DMPRC037            IF NOT THE DOWEEK WOULDNT NEVER BEEN         
*&&                                 ABLE TO SET TO X'FF' FLAG'                  
DMPRC038 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO                                                
         BZ    DEMPRCX                                                          
*                                                                               
         DS    0H                  POST (DUMMY) MSG RECORD FOR STEREO           
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         MVI   TDRTYPE,TDRTMSGQ    RECORD TYPE                                  
         XC    TDRBOOK,TDRBOOK                                                  
         MVI   TDRDUMMY,0                                                       
         GOTO1 APOST                                                            
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   DP048X                                                           
         CLC   D32PCVER,=AL4(XTRCVER3)       AND USING EXTRACT VER >=3,         
         BL    DP048X                                                           
         MVI   TDRTYPE,TDRTBKQ               POST LAST STTN RECORD              
         MVI   TDRK2BTP,XFF                                                     
         GOTO1 APOST                                                            
DP048X   EQU   *                                                                
         DROP  R1,R2                                                            
*                                                                               
DEMPRCX  DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
DEMHOOK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                                                               
*                                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DEMHK20              YES, DO STEREO LOGIC                        
*                                                                               
         L     R2,DBAREC                                                        
         USING SBKEY,R2                                                         
***      CLI   DBMED,C'U'          COUNTY COVERAGE READS RUN                    
***      BE    DEMHK04             RECORDS                                      
         LA    R3,POSTLINE                                                      
         USING BINRECD,R3                                                       
**       CLI   SBBOOK,99           BYPASS BAD STATION RECORDS                   
**       BH    DEMHOOKX                                                         
         TM    SBBOOK,X'80'        BYPASS BAD STATION RECORDS                   
         BO    DEMHOOKX                                                         
                                                                                
*                                                                               
         DS    0H                                                               
                                                                                
         CLI   SBBTYP,X'05'        BYPASS 6A-2A DATA                            
         BE    DEMHOOKX                                                         
*                                                                               
DEMHK04  OC    SPILLMKT,SPILLMKT                                                
         BNZ   DEMHK08G                                                         
         CLI   SBHOME,C'H'         TEST RADIO HOME MARKET                       
         BE    *+14                 ACCEPT IF IT IS                             
         OC    SBKMKT,SBKMKT       IGNORE SPILL MARKETS                         
         BNZ   DEMHOOKX                                                         
         B     DEMHK08X                                                         
*                                                                               
DEMHK08G DS    0H                  USER ENTERRED A SPILL MARKET                 
         CLC   SBKMKT,SPILLMKT                                                  
         BNE   DEMHOOKX                                                         
DEMHK08X EQU   *                                                                
*                                                                               
         MVC   TMPBOOK,SBBOOK                                                   
         CLI   DBMED,C'O'          OVERNIGHTS                                   
         BNE   *+14                ONLY AFTER DEC0604                           
         CLC   TMPBOOK,=AL2(OVCUTOFF)                                           
         BL    DEMHOOKX                                                         
*                                                                               
* For now, booktype support on the MF is only for the new BBM weekly            
*  format.  Eventually, we should support it across the board.                  
DEMHK11M DS    0H                  HANDLE BOOKTYPE                              
         XC    TMPBTYP,TMPBTYP                                                  
         CLI   DBMED,C'C'           CANADIAN                                    
         BNE   DMHK012X                                                         
         CLI   DBSRC,C'A'           BBM                                         
         BNE   DMHK012X                                                         
         CLC   DBFIL,=C'TP '        TIME PERIOD                                 
         BNE   DMHK012X                                                         
*                                                                               
         DS    0H                  EXTRACT BOOKTYPE                             
         MVC   TMPBTYP,SBBTYP                                                   
                                                                                
         CLC   TMPBOOK,=AL2(BTCUTOFF) IF BK IS PRIOR TO BKTYP CUT-OFF,          
         BNL   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE                        
                                                                                
         CLI   TMPBTYP,X'E0'          IF XTRA SPILL,                            
         BNE   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE TOO                    
                                                                                
         L     RF,AUPCSETB                                                      
         TR    TMPBTYP,0(RF)          CHANGE BOOKTYPE TO UPPERCASE              
DMHK012X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'C'                                                       
         BNE   DMHK019                                                          
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BE    DMHK013                                                          
         B     DMHK015                                                          
DMHK013  DS    0H                                                               
         CLI   DBSRC,C'A'     BBM                                               
         BNE   DMHK014                                                          
         CLI   TMPBTYP,C'W'                                                     
         BNE   DEMHOOKX                                                         
         B     DMHK019                                                          
DMHK014  DS    0H             CSI                                               
         CLC   TMPBOOK,=X'6001'                                                 
         BL    DEMHOOKX                                                         
         B     DMHK019                                                          
* MONTHLY TP                                                                    
DMHK015  DS    0H        BBM                                                    
         CLI   DBSRC,C'A'                                                       
         BNE   DMHK016                                                          
         CLI   TMPBTYP,C'W'                                                     
         BE    DMHK16A                                                          
         B     DMHK019                                                          
DMHK016  DS    0H        CSI                                                    
         CLC   TMPBOOK,=X'6001'                                                 
         BNL   DMHK16A                                                          
         B     DMHK019                                                          
DMHK16A  DS    0H                                                               
*   CODE TO GET MONTHLY BOOKS FROM WEEKLIES                                     
*  CALL NSIWEEK TO GET WEEKLY BOOK TO MMDDYY FORMAT                             
*  THEN CALL DATCON - MONTHLY RESULT                                            
*  SAVE R1- POINING TO TSAR RECORD AND RESTORE AFTER EVERY                      
* TIME R1 GETS DESTROYED                                                        
*( GETTING MONTHLY BOOKS FROM WEEKLY BOOKS)                                     
*                                                                               
         ST    R1,FULL                                                          
         MVC   DUB(2),TMPBOOK     BOOK                                          
***      XC    DUB(2),=X'FFFF'    UNINVERT THE BOOK FOR NSIWEEK AND             
*                                                                               
         CLI   DBSRC,C'A'         BBM?                                          
         BE    *+18                                                             
*  CSI                                                                          
         CLC   DUB(2),=X'6001'    CSI ONLY BOOKS HIGHER THAN 1/96               
         BL    DMHK017            ARE WEEKLY ELSE JUST POST AS MONTH            
         B     *+12                                                             
         CLI   TMPBTYP,C'W'       IF PROCESSING MONTH AND DONT HAVE             
         BNE   DMHK017            BOOKTYPE W WE JUST POST AS REG MONTH          
*                                 DATCON CALL                                   
         DS    0H                 FORMAT WEEKLY BOOK                            
         MVI   BYTE,0                                                           
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         CLI   DBMED,C'C'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
*                                                                               
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(BYTE,VGETDAY),VADDAY,VDATCON           
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB2(6),0(R1)                                                    
*                                                                               
         GOTO1 VGETBROD,DMCB,(X'01',DUB2),TEMPBRO,VGETDAY,VADDAY                
         MVC   TEMPBRO(6),DUB2                                                  
         GOTO1 VADDAY,DMCB,TEMPBRO,TEMPBRO,6                                    
         CLC   TEMPBRO(6),TEMPBRO+6                                             
         BNE   DEMHOOKX                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(3,FULL2)                              
         L     R1,FULL              RESTORE TSAR RECORD                         
         MVC   BINBOOK,FULL2                                                    
***      XC    BINBOOK,=X'FFFF'     INVERT THE BOOK                             
*                                                                               
DMHK017  DS    0H                                                               
         MVI   BINBTYP,0                                                        
**       CLI   DBSRC,C'A'                                                       
**       BE    *+8                                                              
**       MVI   BINBTYP,C'W'                                                     
         GOTO1 APOST                                                            
         L     R1,FULL              RESTORE TSAR RECORD                         
         B     DEMHOOKX                                                         
***********                                                                     
*                                                                               
DMHK019  DS    0H                                                               
*&&DO                                                                           
         CLI   DBMED,C'C'                                                       
         BNE   DMHK019B                                                         
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   DEMHOOKX                                                         
         MVC   DUB(2),TMPBOOK                                                   
         XC    DUB(2),=X'FFFF'                                                  
         CLI   DBSRC,C'A'                                                       
         BNE   DMHK019A                                                         
         CLI   TMPBTYP,C'W'                                                     
         BNE   DEMHOOKX                                                         
         B     DMHK019B                                                         
*                                                                               
DMHK019A CLC   DUB(2),=X'6001'                                                  
         BL    DEMHOOKX                                                         
         MVI   TMPBTYP,C'W'      FORCE WEEKLY W BOOKTYPE FOR CSI                
*&&                                                                             
*                                                                               
DMHK019B MVC   BINBOOK,TMPBOOK                                                  
         MVC   BINBTYP,TMPBTYP                                                  
         GOTO1 APOST                                                            
         B     DEMHOOKX                                                         
*                                                                               
DEMHK20  DS    0H                  STEREO SESSION                               
         L     R2,DBAREC                                                        
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1                                                    
         MVC   0(2,R1),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R1,2(R1)                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         USING TSDEMRCD,R1                                                      
*                                                                               
         CLI   DBFUNCT,DBGETAB     GETAB USED BY COUTNY COVERAGE                
         BE    *+8                                                              
         CLI   DBFUNCT,DBGETAMB    WRONG FUCKING CALL????                       
         BNE   DEMHK26X                                                         
         MVC   TMPBOOK,DBACTBK      GET BOOK                                    
         MVC   TMPBTYP,DBBTYPE      AND BOOKTYPE FROM SOMEWHERE ELSE            
*                                                                               
         CLI   TMPBTYP,X'05'         BYPASS 6A-2A DATA                          
         BE    DEMHOOKX                                                         
                                                                                
         CLI   DBMED,C'T'          IF MEDIA IS USTV,                            
         BNE   DEMHK26G                                                         
         CLI   DBBTYPE,C'C'         AND BOOKTYPE IS CABLE,                      
         BNE   DEMHK26G                                                         
         CLC   FILE,=C'PAV'         AND FILE REQUESTED IS PAV,                  
         BE    DEMHOOKX             IGNORE THIS RECORD                          
         CLI   ACTN,NWSYNBKS        OR IF ACTION REQUESTED IS BOOKP,            
         BE    DEMHOOKX              IGNORE THIS RECORD                         
DEMHK26G EQU   *                                                                
* IF GETAMB CALL DONT INVERT                                                    
         CLI   DBFUNCT,DBGETAB     GETAB USED BY COUTNY COVERAGE                
         BNE   *+10                                                             
         XC    TMPBOOK,=X'FFFF'       INVERT BOOK BACK TO NORMAL                
*                                                                               
         DS    0H                  SEE IF WE SHOULD IGNORE RECORD               
         CLI   ACTN,NWSYNBKS        IF ACTION REQUESTED IS NOT BOOKP,           
         BNE   DEMHK26M              SKIP THIS CHECK                            
         CLI   DBSRC,C'N'           IF SOURCE IS NOT NSI,                       
         BNE   DEMHK26M              SKIP THIS CHECK                            
         CLI   DBMED,C'T'           IF MEDIA IS NOT USTV,                       
         BNE   DEMHK26M              SKIP THIS CHECK                            
         CLC   TMPBOOK,=AL2(NSCUTOFF) IF BK IS PRIOR TO NW/SYN CUT-OFF,         
         BL    DEMHOOKX                IGNORE THIS RECORD                       
DEMHK26M EQU   *                                                                
         CLI   DBMED,C'O'          OVERNIGHTS                                   
         BNE   *+14                ONLY AFTER DEC0604                           
         CLC   TMPBOOK,=AL2(OVCUTOFF)                                           
         BL    DEMHOOKX                                                         
*                                                                               
         CLC   TMPBOOK,=AL2(BTCUTOFF) IF BK IS PRIOR TO BKTYP CUT-OFF,          
         BNL   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE                        
*                                                                               
         CLI   TMPBTYP,X'E0'          IF XTRA SPILL,                            
         BNE   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE TOO                    
*                                                                               
         CLI   TMPBTYP,X'05'          IF X'05' BOOKTYPE (NSI 2A-6A),            
         BNE   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE TOO                    
*                                                                               
         L     RF,AUPCSETB                                                      
         TR    TMPBTYP,0(RF)          CHANGE BOOKTYPE TO UPPERCASE              
*                                                                               
         B     DMHKPOST                                                         
DEMHK26X EQU   *                                                                
         DROP  R1                                                               
                                                                                
         USING TSDEMRCD,R1                                                      
***      CLI   SBBOOK,99           BYPASS BAD STATION RECORDS                   
***      BH    DEMHOOKX                                                         
         TM    SBBOOK,X'80'        BYPASS BAD STATION RECORDS                   
         BO    DEMHOOKX                                                         
                                                                                
         DS    0H                                                               
         OC    SPILLMKT,SPILLMKT                                                
         BNZ   DEMHK45                                                          
         CLI   SBHOME,C'H'         TEST RADIO HOME MARKET                       
         BE    *+14                 ACCEPT IF IT IS                             
         OC    SBKMKT,SBKMKT       IGNORE SPILL MARKETS                         
         BNZ   DEMHOOKX                                                         
         B     DEMHK49                                                          
*                                                                               
DEMHK45  DS    0H                  USER ENTERRED A SPILL MARKET                 
         CLC   SBKMKT,SPILLMKT                                                  
***      BNE   DEMHOOKX                                                         
         BE    DEMHK49                                                          
         CLI   DBMED,C'R'                                                       
         BNE   DEMHOOKX                                                         
         CLC   DBSELALF,ALFMKTS                                                 
         BNE   DEMHOOKX                                                         
DEMHK49  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'T'          IF MEDIA IS USTV,                            
         BNE   DEMHK59                                                          
         CLI   SBBTYP,C'C'          AND BOOKTYPE IS CABLE,                      
         BNE   DEMHK59                                                          
         CLC   FILE,=C'PAV'         AND FILE REQUESTED IS PAV,                  
         BE    DEMHOOKX             IGNORE THIS RECORD                          
         CLI   ACTN,NWSYNBKS        OR IF ACTION REQUESTED IS BOOKP,            
         BE    DEMHOOKX              IGNORE THIS RECORD                         
DEMHK59  EQU   *                                                                
*                                                                               
         DS    0H                  SEE IF WE SHOULD IGNORE RECORD               
         CLI   ACTN,NWSYNBKS        IF ACTION REQUESTED IS NOT BOOKP,           
         BNE   DEMHK69               SKIP THIS CHECK                            
         CLI   DBSRC,C'N'           IF SOURCE IS NOT NSI,                       
         BNE   DEMHK69               SKIP THIS CHECK                            
         CLI   DBMED,C'T'           IF MEDIA IS NOT USTV,                       
         BNE   DEMHK69               SKIP THIS CHECK                            
         CLC   SBBOOK,=AL2(NSCUTOFF) IF BK IS PRIOR TO NW/SYN CUT-OFF,          
         BL    DEMHOOKX               IGNORE THIS RECORD                        
DEMHK69  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   TMPBOOK,SBBOOK      MOVE IN BOOK,                                
         MVC   TMPBTYP,SBBTYP       BOOK TYPE,                                  
*                                                                               
         CLI   DBMED,C'O'          OVERNIGHTS                                   
         BNE   *+14                ONLY AFTER DEC0604                           
         CLC   TMPBOOK,=AL2(OVCUTOFF)                                           
         BL    DEMHOOKX                                                         
*                                                                               
         CLC   TMPBOOK,=AL2(BTCUTOFF) IF BK IS PRIOR TO BKTYP CUT-OFF,          
         BNL   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE                        
*                                                                               
         CLI   TMPBTYP,X'E0'          IF XTRA SPILL,                            
         BNE   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE TOO                    
*                                                                               
         CLI   TMPBTYP,X'05'          IF X'05' BOOKTYPE (NSI 2A-6A),            
         BNE   *+8                                                              
         MVI   TMPBTYP,0               KILL THE BOOKTYPE TOO                    
*                                                                               
         L     RF,AUPCSETB                                                      
         TR    TMPBTYP,0(RF)          CHANGE BOOKTYPE TO UPPERCASE              
         DROP  R1                                                               
                                                                                
*                                                                               
DMHKPOST DS    0H                                                               
*&&DO                                                                           
*   new code to control canadien book processing                                
         CLI   DBMED,C'C'                                                       
         BNE   DEMHK26L                                                         
         MVI   DOWEEK,0                                                         
*                                                                               
DEMHK26J CLI   DOWEEK+1,C'Y'       IF WE ARE ONLY DOING WEEKLY                  
         BNE   DEMHK26K                                                         
*                                                                               
         CLI   DBSRC,C'A'    CSI                                                
         BE    DMHKBBM                                                          
* IF NOT BBM MUST BE CSI NOW                                                    
         CLC   TMPBOOK,=X'6401'                                                 
***      BNH   DEMHOOKX            IF LOWER THAN CUTOFF= MONTHLY                
         BL    DEMHK26L                                                         
         CLI   DOWEEK+1,C'N'                                                    
         BNE   DEMHOOKX                                                         
         MVI   DOWEEK,X'FF'                                                     
         B     DEMHOOKX                                                         
*                             BBM                                               
DMHKBBM  CLI   DBBTYPE,C'W'                                                     
         BE    DEMHK26L                                                         
         CLI   DOWEEK+1,C'N'                                                    
         BNE   DEMHOOKX                                                         
         MVI   DOWEEK,X'FF'                                                     
         B     DEMHOOKX                                                         
* THIS IS NOW PROCESSING REGULAR BOOKS FOR BBM                                  
*                                                                               
DEMHK26K DS    0H                                                               
         MVI   DOWEEK,X'FF'     SET BYTE TO BE DONE AFTER MONTHLY BKS           
         B     DEMHK26P                                                         
DEMHK26L DS    0H                                                               
         MVI   DOWEEK,0                                                         
DEMHK26P DS    0H                                                               
*&&                                                                             
         CLI   DBMED,C'N'          IF MEDIA=NETWORK,                            
         BNE   DMHKPSTAG                                                        
         CLI   DBSRC,C'H'           AND SOURCE=NIELSEN HISPANIC,                
         BNE   DMHKPSTAG                                                        
         L     R6,ABTPTBNH          GET SET TO MANUAL DL BOOKTYPES              
*                                                                               
DMHKPSTAG EQU  *                                                                
*                                                                               
         USING TSDEMRCD,R1                                                      
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM16 SESSION,                   
         BO    *+8                                                              
         MVI   TMPBTYP,0                     KILL BOOKTYPE                      
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   DMHKPSTB                                                         
                                                                                
         DS    0H                            NEED TO INVERT BOOK                
         XC    TMPBOOK,=X'FFFF'                                                 
*                                                                               
DMHKPSTA0 DS   0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   DMHKPSTB                                                         
                                                                                
         DS    0H                            NEED TO CHECK VERSIONS             
         CLC   D32PCVER,=AL4(XTRCVER3)                                          
         BNL   DMHKPSTD                                                         
         CLC   D32PCVER,=AL4(XTRCVER1)                                          
         BNL   DMHKPSTB                                                         
         B     DEMHOOKX                                                         
*                                                                               
DMHKPSTB DS    0H                   USING KEY #1                                
         MVC   TDRBOOK,TMPBOOK                                                  
         MVC   TDRBTYP,TMPBTYP                                                  
         B     DMHKPSTG                                                         
*                                                                               
DMHKPSTD DS    0H                   USING KEY #2                                
         MVC   TDRK2BK,TMPBOOK                                                  
         MVC   TDRK2BTP,TMPBTYP                                                 
         CLI   DBMED,C'U'                                                       
         BNE   DMHKP40                                                          
         L     RE,ASTATTAB                                                      
DMHKP30  CLC   =X'FFFF',0(RE)       IF STATECODE NOT IN TABLE                   
         BE    DEMHOOKX             THEN SOMETHING IS WRONG.                    
*                                   WE WILL IGNORE IT                           
         CLC   TMPBTYP,0(RE)                                                    
         BE    *+12                                                             
         AHI   RE,L'STACODE                                                     
         B     DMHKP30                                                          
         MVC   TDRK2BTP(2),1(RE)                                                
*                                                                               
DMHKP40  DS    0H                                                               
         CLI   DBMED,C'C'                                                       
         BNE   DMHKPSTG                                                         
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BE    DMHPSTF1                                                         
         B     DMHPSTF3                                                         
DMHPSTF1 DS    0H                                                               
         CLI   DBSRC,C'A'     BBM                                               
         BNE   DMHPSTF2                                                         
         CLI   TMPBTYP,C'W'                                                     
         BNE   DEMHOOKX                                                         
         B     DMHKPSTG                                                         
DMHPSTF2 DS    0H             CSI                                               
         CLC   TMPBOOK,=X'6001'                                                 
         BL    DEMHOOKX                                                         
         B     DMHKPSTG                                                         
* MONTHLY TP                                                                    
DMHPSTF3 DS    0H        BBM                                                    
         CLI   DBSRC,C'A'                                                       
         BNE   DMHPSTF5                                                         
         CLI   TMPBTYP,C'W'                                                     
         BE    DMHPSTF6                                                         
         B     DMHPSTF8                                                         
DMHPSTF5 DS    0H        CSI                                                    
         CLC   TMPBOOK,=X'6001'                                                 
         BNL   DMHPSTF6                                                         
         B     DMHPSTF8                                                         
DMHPSTF6 DS    0H                                                               
*   CODE TO GET MONTHLY BOOKS FROM WEEKLIES                                     
*  CALL NSIWEEK TO GET WEEKLY BOOK TO MMDDYY FORMAT                             
*  THEN CALL DATCON - MONTHLY RESULT                                            
*  SAVE R1- POINING TO TSAR RECORD AND RESTORE AFTER EVERY                      
* TIME R1 GETS DESTROYED                                                        
*( GETTING MONTHLY BOOKS FROM WEEKLY BOOKS)                                     
*                                                                               
         CLC   TMPBOOK,=X'FFFF'                                                 
         BE    DEMHOOKX                                                         
         ST    R1,FULL                                                          
         MVC   DUB(2),TMPBOOK     BOOK                                          
         XC    DUB(2),=X'FFFF'    UNINVERT THE BOOK FOR NSIWEEK AND             
*                                                                               
         CLI   DBSRC,C'A'         BBM?                                          
         BE    *+18                                                             
*  CSI                                                                          
         CLC   DUB(2),=X'6001'    CSI ONLY BOOKS HIGHER THAN 1/96               
         BL    DMHPSTF7           ARE WEEKLY ELSE JUST POST AS MONTH            
         B     *+12                                                             
         CLI   TMPBTYP,C'W'       IF PROCESSING MONTH AND DONT HAVE             
         BNE   DMHPSTF7           BOOKTYPE W WE JUST POST AS REG MONTH          
*                                 DATCON CALL                                   
         DS    0H                 FORMAT WEEKLY BOOK                            
         MVI   BYTE,0                                                           
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         CLI   DBMED,C'C'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(BYTE,VGETDAY),VADDAY,VDATCON           
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB2(6),0(R1)                                                    
*                                                                               
         GOTO1 VGETBROD,DMCB,(X'01',DUB2),TEMPBRO,VGETDAY,VADDAY                
         MVC   TEMPBRO(6),DUB2                                                  
         GOTO1 VADDAY,DMCB,TEMPBRO,TEMPBRO,6                                    
         CLC   TEMPBRO(6),TEMPBRO+6                                             
         BNE   DEMHOOKX                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(3,FULL2)                              
         L     R1,FULL              RESTORE TSAR RECORD                         
         MVC   TDRK2BK,FULL2                                                    
         XC    TDRK2BK,=X'FFFF'     INVERT THE BOOK                             
*                                                                               
DMHPSTF7 DS    0H                                                               
*                                                                               
DMHPSTF8 DS    0H                                                               
         CLC   TMPBOOK,=X'FFFF'                                                 
         BE    DEMHOOKX                                                         
         MVI   TDRTYPE,TDRTBKQ      RECORD TYPE,                                
         MVI   TDRDUMMY,0           AND A DUMMY VALUE                           
         MVI   TDRK2BTP,0                                                       
         GOTO1 APOST                                                            
         L     R1,FULL              RESTORE TSAR RECORD                         
         GOTO1 APOST                                                            
         L     R1,FULL              RESTORE TSAR RECORD                         
         B     DEMHOOKX                                                         
*******************************************************************             
DMHKPSTG DS    0H                                                               
*   FOR CANADIAN THIS IS WEEKLY BOOK POSTING                                    
*                                                                               
         CLI   DBMED,C'C'                                                       
         BNE   DMHKPSTI                                                         
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   DEMHOOKX                                                         
         MVC   DUB(2),TMPBOOK                                                   
         XC    DUB(2),=X'FFFF'                                                  
         CLI   DBSRC,C'A'                                                       
         BNE   DMHKPSTH                                                         
         CLI   TMPBTYP,C'W'                                                     
         BNE   DEMHOOKX                                                         
         B     DMHKPSTI                                                         
*                                                                               
DMHKPSTH CLC   DUB(2),=X'6001'                                                  
         BL    DMHKSKIP                                                         
         MVI   TDRK2BTP,C'W'      FORCE WEEKLY W BOOKTYPE FOR CSI               
*                                                                               
*                                                                               
DMHKPSTI DS    0H                                                               
         CLC   TMPBOOK,=X'FFFF'     BAD BOOK                                    
         BE    DEMHOOKX                                                         
*                                                                               
         MVI   TDRTYPE,TDRTBKQ      RECORD TYPE,                                
         MVI   TDRDUMMY,0           AND A DUMMY VALUE                           
***      MVC   SVPREWBK,TMPBOOK                                                 
         GOTO1 APOST                                                            
         DROP  R1                                                               
DMHKSKIP DS    0H                                                               
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'N'          IF MEDIA=NETWORK,                            
         BNE   DMHKPSTMX                                                        
         CLI   DBSRC,C'H'           AND SOURCE=NIELSEN HISPANIC,                
         BNE   DMHKPSTMX                                                        
         AHI   R6,L'BTPTBNHT        GET NEXT BOOKTYPE                           
                                                                                
         CLI   0(R6),XFF            IF NO MORE BOOKTYPES TO GO,                 
         BE    DMHKPSTMX             EXIT LOOP                                  
         MVC   TMPBTYP,0(R6)                                                    
         B     DMHKPSTA0                                                        
DMHKPSTMX EQU  *                                                                
         B     DEMHOOKX                                                         
         DROP  R4                                                               
*                                                                               
DEMHOOKX L     RE,SAVERE                                                        
*                                                                               
*                                                                               
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*                                                                               
DEMLINE  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DEMLIN20             YES, DO STEREO LOGIC                        
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINBOOK,X'FF'       IGNORE DUMMY E-O-F RECORDS                   
         BE    EXIT                                                             
                                                                                
         MVI   BYTE#2,0            BYTE#2 = NSIWEEK OVRRIDING START DAY         
                                                                                
         DS    0H                                                               
         CLI   DBMED,C'O'          CHECK WEEKLY TIME PERIOD                     
         BE    DEMLIN08                                                         
         CLI   DBMED,C'W'          CHECK WEEKLY TIME PERIOD                     
         BE    DEMLIN08                                                         
         CLI   DBMED,C'N'          NETWORK?                                     
         BNE   *+8                                                              
         CLI   DBSRC,C'C'          CABLE?                                       
         BE    DEMLIN08                                                         
         CLI   DBSRC,C'K'          POCKETPIECE?                                 
         BE    DEMLIN08                                                         
                                                                                
         DS    0H                                                               
         CLI   DBMED,C'C'          CHECK CANADIEN                               
         BNE   DEMLIN05                                                         
         CLI   DBSRC,C'N'           NIELSEN                                     
         BNE   DEMLIN05                                                         
         CLC   DBFIL,=C'TP '        TIME PERIOD                                 
         BNE   DEMLIN05                                                         
         CLC   BINBOOK,=X'6001'     AND BOOK ON OR AFTER JAN/96                 
         BL    DEMLIN05                                                         
         MVI   BYTE#2,1            OVERRIDE START DAY TO MONDAY                 
*                                                                               
*  CHECK TP SEE IF ADEMFIL IS WTP IF SO EVERYTHING IS IN WEEKLY                 
*  ELSE EVRYTHING IS IN MONTHLY                                                 
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   DEMLIN06                                                         
         B     DEMLIN08                                                         
DEMLIN05 EQU   *                                                                
                                                                                
         DS    0H                                                               
         CLI   DBMED,C'C'          CHECK CANADIAN                               
         BNE   DEMLIN06                                                         
*                                                                               
*                                                                               
         CLI   DBSRC,C'A'           BBM                                         
         BNE   DEMLIN06                                                         
         CLC   DBFIL,=C'TP '        TIME PERIOD                                 
         BNE   DEMLIN06                                                         
         CLI   BINBTYP,C'W'         BOOKTYPE="W"                                
         BL    DEMLIN06                                                         
         MVI   BYTE#2,1            OVERRIDE START DAY TO MONDAY                 
         B     DEMLIN08                                                         
DEMLIN06 EQU   *                                                                
                                                                                
         B     DEMLIN10                                                         
                                                                                
*                                                                               
DEMLIN08 DS    0H                  MMMDD/YY FORMAT                              
         CLI   DBMED,C'N'                                                       
         BNE   DEMLIN09                                                         
*                                                                               
         DS    0H                  MMMDD/YY FORMAT                              
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         LA    R1,DMCB                                                          
         GOTO1 VCALLOV,(R1),0                                                   
         ICM   RE,15,0(R1)                                                      
         XC    DMCB,DMCB                                                        
         ST    RE,DMCB+16          PASS A(GETBROAD) TO NETUNBK                  
         L     RF,=V(NETUNBK)                                                   
         A     RF,RELO06                                                        
         GOTO1 (RF),DMCB,(C'W',BINBOOK),DUB,VGETDAY,VADDAY,,                    
         B     DEMLIN9X                                                         
*                                                                               
DEMLIN09 DS    0H                  MMMDD/YY FORMAT                              
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE#2,1                                                         
         GOTO1 VNSIWEEK,DMCB,(C'D',BINBOOK),(BYTE#2,VGETDAY),VADDAY,   +        
               VDATCON                                                          
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(6),0(R1)                                                     
*                                                                               
DEMLIN9X DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(5,WORK)                                    
         MVC   LINE1+2(8),WORK                                                  
         LHI   R0,8                                                             
         B     DEMLIN19                                                         
                                                                                
*                                                                               
DEMLIN10 DS    0H                  MONTHLY FORMAT                               
         GOTO1 VDATCON,DMCB,(3,BINBOOK),(6,LINE1+2)                             
         LHI   R0,6                                                             
         B     DEMLIN19                                                         
                                                                                
*                                                                               
DEMLIN19 DS    0H                                                               
         CLI   BINBTYP,0                                                        
         BE    DEMLN19X                                                         
*                                                                               
         DS    0H                                                               
         LR    RF,R0                                                            
         LA    RF,LINE1+2(RF)                                                   
         MVI   0(RF),C'('                                                       
         MVC   1(L'BINBTYP,RF),BINBTYP                                          
         AHI   RF,L'BINBTYP+1                                                   
         MVI   0(RF),C')'                                                       
DEMLN19X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
DEMLIN20 DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
                                                                                
* For list functions, the 1st record to STEREO must be a message.               
*  If there are no messages, pass a dummy record of one blank.                  
                                                                                
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
         DROP  R1                                                               
                                                                                
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
         L     R2,ASTIOBUF                                                      
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DMLN200                                                          
*                                                                               
         DS    0H                  DETERMINE RECORD TYPE                        
         CLI   TDRTYPE,TDRTMSGQ     MESSAGE                                     
         BE    DEMLIN30                                                         
         CLI   TDRTYPE,TDRTBKQ      BOOK                                        
         BE    DEMLIN40                                                         
         DC    H'0'                                                             
*                                                                               
DEMLIN30 DS    0H                  MESSAGE RECORD                               
         MVI   0(R2),C' '           MOVE IN BLANK FOR DUMMY MSG                 
         LA    R2,1(R2)                                                         
         B     DMLIN100                                                         
*                                                                               
DEMLIN40 DS    0H                  BOOK RECORD                                  
         XC    DUB,DUB                                                          
         MVC   DUB(2),TDRBOOK                                                   
         MVI   BYTE,X'83'          BYTE = DATCON INPUT TYPE                     
         MVI   BYTE#2,0            BYTE#2 = NSIWEEK OVRRIDING START DAY         
                                                                                
         DS    0H                                                               
         CLI   DBMED,C'C'          CHECK CANADIEN                               
         BNE   DEMLN42X                                                         
         CLI   DBSRC,C'N'           NIELSEN                                     
         BNE   DEMLN42X                                                         
         CLC   DBFIL,=C'TP '        TIME PERIOD                                 
         BNE   DEMLN42X                                                         
         CLC   TDRBOOK,=X'6001'     AND BOOK ON OR AFTER JAN/96                 
         BL    DEMLN42X                                                         
         MVI   BYTE#2,1            OVERRIDE START DAY TO MONDAY                 
         B     DEMLIN50                                                         
DEMLN42X EQU   *                                                                
                                                                                
         B     DEMLIN52                                                         
                                                                                
*                                                                               
DEMLIN50 DS    0H                  WANT MMMDD/YY FORMAT                         
*&&DO                                                                           
         GOTO1 VNSIWEEK,DMCB,(C'D',TDRBOOK),(BYTE#2,VGETDAY),VADDAY,   +        
               VDATCON                                                          
*&&                                                                             
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE#2,1                                                         
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(BYTE#2,VGETDAY),VADDAY,VDATCON         
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(6),0(R1)                                                     
         MVI   BYTE,X'80'          BYTE = DATCON INPUT TYPE                     
         B     DEMLIN52                                                         
                                                                                
*                                                                               
DEMLIN52 DS    0H                                                               
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(X'83',TDRBOOK),(6,(R2)),0                          
*&&                                                                             
         GOTO1 VDATCON,DMCB,(BYTE,DUB),(5,(R2)),0                               
         ZIC   R0,DMCB+4                                                        
         B     DEMLIN60                                                         
                                                                                
DEMLIN60 DS    0H                  R0 = L(FORMATTED BOOK)                       
         AR    R2,R0                                                            
         B     DMLIN100                                                         
                                                                                
         DROP  R5                                                               
*                                                                               
DMLIN100 DS    0H                                                               
         MVI   0(R2),STSPIKEY                                                   
         LA    R2,1(R2)                                                         
         S     R2,ASTIOBUF                                                      
         STH   R2,IODATALN                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
DMLN200  DS    0H                                                               
         MVI   GOSUBN,D32DL#                                                    
         GOTO1 AGOSUB                                                           
         B     EXIT                                                             
         EJECT                                                                  
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
         ST    RC,ALOCWRK                                                       
         SR    R1,R1                                                            
         IC    R1,GOSUBN                                                        
         GOTO1 ASUBRTN,(R1)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HEADSTB1 DC    C'AVAILABLE'                                                     
HEADSTB2 DC    C'  BOOKS  '                                                     
         SPACE 1                                                                
         DROP  R7,R8,R9,RB,RC                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*========================== SUBROUTINE POOL ==========================*         
                                                                                
* AT ENTRY ,                                                                    
* R9 --->DEMWRKD                                                                
* R8 ---> TWA                                                                   
* R7 --->DEMTMPD                                                                
* R1 ---> EQUATED SUBROUTINE NUMBER                                             
SUBR01Q  EQU   (((*-DEM06+4095)/4096)*4096)                                     
         ORG   DEM06+SUBR01Q                                                    
SUBR01   NMOD1 0,**0601**                                                       
         USING DEMWRKD,R9                                                       
         USING DEMTWAD,R8                                                       
         USING DEMTMPD,R7                                                       
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM06WRKD,RC                                                      
*                                                                               
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
*                                                                               
D32DL#   EQU   (R01_01-*)/4+1      DEMLINE PROCESSING FOR DEM32                 
D32BK#   EQU   (R01_02-*)/4+1      DOWNLOAD MARKET LIST                         
OKDL#    EQU   (R01_03-*)/4+1      OKAY TO DOWNLAD ?                            
GADLD#   EQU   (R01_04-*)/4+1      GET A(DOWNLOAD DATA TABLES)                  
TBK#     EQU   (R01_05-*)/4+1      TRANSLATE BOOK                               
CIOB#    EQU   (R01_06-*)/4+1      CLEAR STEREO I/O BUFFER                      
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R01_02   B     D32DLBK             DOWNLOAD MARKET LIST                         
R01_03   B     OKDOWNLD            OKAY TO DOWNLOAD?                            
R01_04   B     GADLDTAB            GET A(DOWNLOAD DATA TABLE)                   
R01_05   B     TRSLTBK             TRANSLATE BOOK                               
R01_06   B     CLRIOBUF            CLEAR STEREO I/O BUFFER                      
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
YES_STE  SR    R9,R9                                                            
NO_STE   LTR   R9,R9                                                            
XIT_STE  XIT1                                                                   
*********************************************************                       
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--GADLD#+        
               )'                                                               
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
GADLD032 DS    0H                  R4-->ELEMENT MAP MODE ENTRY                  
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
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--OKDL#)+        
               '                                                                
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
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--D32DL#+        
               )'                                                               
*********************************************************                       
D32DEMLN  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   TMPRTYP,TDRTYPE                                                 
          CLI   TDRTYPE,TDRTBKQ                                                 
          BE    D32DL100                                                        
          CLI   TDRTYPE,TDRTMSGQ     MESSAGE                                    
          BE    D32DL200                                                        
          B     D32DLX                                                          
                                                                                
*                                                                               
D32DL100  DS    0H                                                              
          MVI   GOSUBN,D32BK#                                                   
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
                                                                                
                                                                                
*                                                                               
D32DL200  DS    0H                 TDRTYPE = TDRTMSGQ                           
          MVI   GOSUBN,CIOB#        USE THIS OPPORTUNITY TO INITIALIZE          
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
                                                                                
                                                                                
*                                                                               
D32DLX    B     XIT_STE                                                         
                                                                                
                                                                                
D32DLOKD NTR1                                                                   
         MVI   GOSUBN,OKDL#                                                     
         GOTO1 AGOSUB                                                           
         J     EXIT                                                             
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--D32BK#+        
               )'                                                               
*---------------------- DEM32 DOWNLOAD BOOK LIST ---------------------*         
D32DLBK   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
                                                                                
*                                                                               
          DS    0H                 CHECK EXTRACT VERSIONS                       
          CLC   D32PCVER,=AL4(XTRCVER3)                                         
          BNL   D32BK100                                                        
          CLC   D32PCVER,=AL4(XTRCVER1)                                         
          BNL   D32BK010                                                        
          J     EXIT                                                            
                                                                                
*                                                                               
D32BK010  DS    0H                                                              
          MVC   FALEMPC,=Y(FMHBOOK)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                  ALPHA MARKET                                
          MVC   FALDMPC,=Y(FMDFMBK)                                             
          MVC   WORK,SPACES                                                     
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),TDRBOOK                                                   
         XC    DUB(2),=X'FFFF'                                                  
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32BKAD                                                       
                                                                                
*                                                                               
         DS    0H                  BOOKTYPE                                     
*                                                                               
         MVC   FALDMPC,=Y(FMDBKTC)                                              
*                                                                               
         CLI   DBMED,C'C'                                                       
         BNE   D32BK98                                                          
         CLI   DBSRC,C'A'                                                       
         BNE   D32BK98                                                          
         L     RF,ADEMFIL                                                       
         CLC   =C'WTP',8(RF)                                                    
         BNE   D32BK98                                                          
         CLI   TDRBTYP,C'W'                                                     
         BE    D32BK99                                                          
*                                                                               
D32BK98  DS    0H                                                               
*                                                                               
         MVC   WORK(1),TDRBTYP                                                  
         CLI   WORK,C' '                                                        
         BNL   *+8                                                              
D32BK99  MVI   WORK,C' '                                                        
         LA    R0,WORK                                                          
         LA    R1,1                                                             
         BAS   RE,D32BKAD                                                       
*                                                                               
          B     XIT_STE                                                         
*                                                                               
         EJECT                                                                  
D32BK100  DS    0H                 USING EXTRACT VERSION  XTRCVER3              
          MVC   FALEMPC,=Y(FMHBOOK)                                             
                                                                                
                                                                                
          CLI   STPRVOVL+(SPOVSETE-SPVOVALD),C'Y'                               
          BE    D32BK105X                                                       
                                                                                
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
          MVI   STPRVOVL+(SPOVSETE-SPVOVALD),C'Y'                               
D32BK105X EQU   *                                                               
                                                                                
*                                                                               
** DELIMITERS **                                                                
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBOOKDLMT)     DELIMITERS                          
          BAS   RE,D32DLOKD                                                     
          BNE   D32BK109                                                        
*                                                                               
          DS    0H                                                              
          LA    RF,STPRVOVL                                                     
          USING SPVOVALD,RF                                                     
          CLI   SPOVDLMT+0,STSPIKEY                                             
          BE    D32BK109                                                        
          MVI   SPOVDLMT+0,STSPIKEY                                             
          DROP  RF                                                              
                                                                                
          LA    R0,STPRVOVL+(SPOVDLMT-SPVOVALD)                                 
          LA    R1,L'SPOVDLMT                                                   
          BAS   RE,D32BKAD                                                      
D32BK109  EQU   *                                                               
          MVC   MYD32DLM,STPRVOVL+(SPOVDLMT-SPVOVALD)                           
                                                                                
*                                                                               
** "GROUP #2" DATA **                                                           
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDBOOKGP2A)                                         
          BAS   RE,D32DLOKD                                                     
          BNE   D32BK159                                                        
                                                                                
*                                                                               
          DS    0H                                                              
**        CLI   DBMED,C'U'         COUNTY COVERAGE COMPARE                      
**        BNE   *+14               FOR 2 BYTE STATE CODE                        
**        CLC   TDRK2BTP(2),STPRVOVL+(SPOVBTYP-SPVOVALD)                        
**        B     *+10                                                            
          CLC   TDRK2BTP(2),STPRVOVL+(SPOVBTYP-SPVOVALD)                        
          BE    D32BK129                                                        
*                                                                               
          DS    0H                 DOWNLOAD GROUP #2 STUFF IN I/O BUFF          
          ZICM  R1,IODATALN,(3)     GET L(DATA IN I/O BUFFER)                   
          BZ    D32BK113X           DON'T DOWNLOAD IF NO DATA                   
                                                                                
          MVC   FALDMPC,=Y(FMDBOOKGP2A)     GROUP #2 DATA (W/ DEM32 BK)         
          L     R0,ASTIOBUF                                                     
          BAS   RE,D32BKAD                                                      
                                                                                
          MVI   GOSUBN,CIOB#       CLEAR STEREO I/O BUFFER                      
          GOTO1 AGOSUB                                                          
D32BK113X EQU   *                                                               
*                                                                               
          CLI   TDRK2BTP,XFF       DID WE REACH "LAST" BOOKTYPE?                
          BE    D32BKX              YEP, NO MORE BOOKTYPES TO DOWNLOAD          
*                                                                               
          MVC   STPRVOVL+(SPOVBTYP-SPVOVALD)(L'SPOVBTYP),TDRK2BTP               
*                                                                               
*  CALL ROUTINE TO TRANSLATE BOOKTYPE FROM INTERNAL TO DISPLAY                  
*                                                                               
         CLI   DBSRC,C'U'                                                       
         BE    D32BK125                                                         
         XC    DMCB2,DMCB2                                                      
         MVI   DMCB2,12                                                         
         MVC   DMCB2+1(1),TDRK2BTP                                              
         GOTO1 ATRANSBT                                                         
         MVC   STPRVOVL+(SPOVBTYP-SPVOVALD)(2),=C'??'                           
         CLI   DMCB2+4,X'FF'                                                    
         BE    *+10                                                             
         MVC   STPRVOVL+(SPOVBTYP-SPVOVALD)(2),DMCB2+4                          
*                                                                               
         MVI   TWOCHRBT,C'N'                                                    
         CLI   DMCB2+5,0           1 CHARACTER OR 2 CHARACTER BOOKTYPE?         
         BE    D32BK129                                                         
         CLI   DMCB2+5,X'40'       1 CHARACTER OR 2 CHARACTER BOOKTYPE?         
         BE    D32BK129                                                         
         MVC   STPRVOVL+(SPOVBTYP-SPVOVALD)(2),DMCB2+4                          
         MVI   TWOCHRBT,C'Y'                                                    
         B     D32BK129                                                         
*                                                                               
*                                                                               
D32BK125  CLI   DBMED,C'U'         COUNTY COVERAGE HAS 2 BYTES BOOKTYPS         
          BNE   *+10               FOR THE STATE ALPHA CODE                     
          MVC   STPRVOVL+(SPOVBTYP-SPVOVALD)(2),TDRK2BTP                        
D32BK129  EQU   *                                                               
                                                                                
*                                                                               
D32BK139  DS    0H                                                              
          L     R2,ASTIOBUF                                                     
          ZICM  R0,IODATALN,(3)                                                 
          BZ    *+10                                                            
          AR    R2,R0                                                           
          B     D32BK139M                                                       
*                                                                               
          CLI   DBMED,C'C'                                                      
          BNE   D32BK139B                                                       
          CLI   DBSRC,C'A'                                                      
          BNE   D32BK139B                                                       
          L     RF,ADEMFIL                                                      
          CLC   =C'WTP',8(RF)                                                   
          BNE   D32BK139B                                                       
          CLI   TDRK2BTP,C'W'                                                   
          BE    D32BK139C                                                       
*                                                                               
D32BK139B DS    0H                                                              
*                                                                               
          MVC   0(L'TDRK2BTP,R2),TDRK2BTP  ALWAYS 1ST ITEM IN GRP#2             
          CLI   DBMED,C'U'                 COUNTY COVERAGE                      
          BNE   *+10                                                            
          MVC   0(2,R2),TDRK2BTP           ALWAYS 1ST ITEM IN GRP#2             
*                                                                               
          CLI   TWOCHRBT,C'Y'                                                   
          BNE   *+10                                                            
          MVC   0(2,R2),DMCB2+4                                                 
*                                                                               
D32BK139C DS    0H                                                              
*                                                                               
* NEW CODE FOR CSI  WE HAD TO PUT BOOKTYPE C'W' TO DIFFERENTIATE                
* MONTHLY AND WEEKLY BOOKS FOR CSI ALSO DUE TO THE DISPLAYING                   
* OF AVERAGED WEEKly BOOKS WE HAD TO PROCESS MONTHLY BOOKS FROM                 
* THE WEEKLY BOOKS- NOW TURN OF THE DOWNLOAD OF THE BOOKTYPE                    
* C'W' FOR CSI                                                                  
*                                                                               
          CLI   DBMED,C'C'                                                      
          BNE   D32BK139G                                                       
          CLI   DBSRC,C'A'                                                      
          BE    D32BK139G                                                       
          MVI   0(R2),0                  CSI-NO BOOKTYPE                        
          B     D32BK139M                                                       
D32BK139G DS    0H                                                              
*                                                                               
          OC    0(L'TDRK2BTP,R2),0(R2)     IF NO BOOKTYPE,                      
          BZ    D32BK139M                  DON'T BUMP OUTPUT POINTER            
          AHI   R2,L'TDRK2BTP                                                   
          CLI   DBMED,C'U'                 COUNTY COVERAGE HAS 2 BYTES          
          BNE   *+8                        FOR THE BOOKTYPE BECAUSE             
          AHI   R2,1                       ITS A STATE CODE                     
          CLI   TWOCHRBT,C'Y'                                                   
          BNE   *+8                                                             
          AHI   R2,1                                                            
D32BK139M EQU   *                                                               
                                                                                
*                                                                               
          DS    0H                                                              
*         CLC   =X'0000',DUB       MAKE SURE THE BOOKS ARE VALID FOR            
*         BE    D32BKX             DOWNLOAD                                     
*         CLI   DUB,0                                                           
*         BE    D32BKX                                                          
*         CLI   DUB+1,0                                                         
*         BE    D32BKX                                                          
          MVC   DUB(2),TDRK2BK     BOOK                                         
          XC    DUB(2),=X'FFFF'                                                 
                                                                                
          CLI   DBMED,C'U'                                                      
          BE    D32BK141M                                                       
          CLI   TDRK2BTP,C'W'                                                   
          BNE   *+8                                                             
          MVI   DUB+2,C'W'                                                      
D32BK141M EQU   *                                                               
          GOTO1 AD32TBK             FORMAT IT "DEM32" STYLE                     
                                                                                
          LA    R0,1(R2)                                                        
          S     R0,ASTIOBUF                                                     
          AH    R0,HALF             R0 = NEW L(DATA IN I/O BUFFER)              
          CHI   R0,255              CAN THIS BOOK FIT INTO BUFFER?              
          BNH   D32BK145X            NO, DOWNLOAD STUFF IN BUFFER               
                                                                                
          MVC   FALDMPC,=Y(FMDBOOKGP2A)                                         
          L     R0,ASTIOBUF                                                     
          LR    R1,R2                                                           
          S     R1,ASTIOBUF                                                     
          BAS   RE,D32BKAD                                                      
          MVI   GOSUBN,CIOB#        CLEAR STEREO I/O BUFFER                     
          GOTO1 AGOSUB                                                          
          B     D32BK139                                                        
D32BK145X EQU   *                                                               
*                                                                               
          MVC   0(1,R2),MYD32DLM    INSERT DELIMITER INTO BUFFER                
          AHI   R2,1                                                            
*                                                                               
          LH    R1,HALF                                                         
          BCTR  R1,0                                                            
          EXMVC R1,0(R2),WORK       MOVE FORMATTED BOOK TO BUFFER               
          LA    R2,1(R2,R1)                                                     
                                                                                
*                                                                               
          DS    0H                                                              
          S     R2,ASTIOBUF                                                     
          STH   R2,IODATALN        R2 = L(DATA IN STEREO I/O BUFFER)            
*                                                                               
D32BK159  EQU   *                                                               
          B     D32BKX                                                          
                                                                                
                                                                                
*                                                                               
D32BKX    DS    0H                                                              
          B     XIT_STE                                                         
          DROP  R5                                                              
*                                                                               
D32BKAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
          XIT1                                                                  
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--TBK#)'         
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* Translates book into a printable format                                       
* At entry,                                                                     
*   DUB+0(2) = book                                                             
* At exit,                                                                      
*   WORK     = formatted book                                                   
*   HALF     = length of formatted book                                         
*   WORK+(LENGTH STORED IN HALF) = WEEKLY OR MONTHLY (W/M)                      
                                                                                
TRSLTBK  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   BYTE,C'M'          MONTHLY IS DEFAULT                            
         CLI   DBMED,C'W'                                                       
         BE    TBK060                                                           
         CLI   DBMED,C'O'                                                       
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
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   STARTDAY,1                                                       
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
         MVI   BYTE,C'W'                                                        
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
         LA    R1,WORK                                                          
         AR    R1,R4                                                            
         MVC   0(1,R1),BYTE      MONTHLY/WEEKLY INDICATOR                       
*                                                                               
         J     EXIT                                                             
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--CIOB#)+        
               '                                                                
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
         TITLE 'DEDEM06 - $DEM LIST BOOKS FOR A STATION (SUBR01--LTORG +        
               && CONSTANTS)'                                                   
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== EQUATES ==============================*         
BTCUTOFF EQU   X'5A07'             KILL BOOKTYPE PRIOR TO JUL90                 
                                                                                
NSCUTOFF EQU   X'5E03'             NO NETWK/SYND INFO PRIOR TO MAR94            
OVCUTOFF EQU   X'6832'             WEEK OF DEC0604                              
                                                                                
*                                                                               
LOCWORKL EQU   DM06WRKL                                                         
***********************************************************************         
         EJECT                                                                  
*                                                                               
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(GOSUB-DEM06),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM06),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(DLDATTAB-DEM06),AL2(ADLDTABS-DEMTMPD)                        
         DC    AL2(BTPTBNHT-DEM06),AL2(ABTPTBNH-DEMTMPD)                        
         DC    AL2(STACODE-DEM06),AL2(ASTATTAB-DEMTMPD)                         
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
*                                                                               
         EJECT                                                                  
*                                  TABLE OF NHT BOOKTYPES (CATEGORIES)          
BTPTBNHT DS    0CL(1)                                                           
         DC     X'00'               STANDARD                                    
         DC     C'5'                HISPANIC                                    
         DC     C'6'                ENGLISH                                     
         DC     C'7'                BILINGUAL                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*======================== DOWNLOAD DATA TABLES =======================*         
                                                                                
DLDATTAB    DS  0D                                                              
                                                                                
*                                                                               
** RECORD TYPE = TDRTBKQ **                                                     
*                                                                               
DLDT1A        DS    0X                                                          
              DC     AL2(DLDT1AX-DLDT1A)                                        
              DC     AL1(DLDT1AG-DLDT1A)                                        
              DC     AL1(TDRTBKQ)                                               
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2A3A    DS      0X                                                        
              DC       AL2(DLDT1A2A3AX-DLDT1A2A3A)                              
              DC       AL1(DLDT1A2A3AG-DLDT1A2A3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2A3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2A3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2A3A4CX-DLDT1A2A3A4C)                         
              DC        AL1(DLDT1A2A3A4CG-DLDT1A2A3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2A3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2A3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2A3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2A3A4AX-DLDT1A2A3A4A)                         
              DC        AL1(DLDT1A2A3A4AG-DLDT1A2A3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2A3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2B3A    DS      0X                                                        
              DC       AL2(DLDT1A2B3AX-DLDT1A2B3A)                              
              DC       AL1(DLDT1A2B3AG-DLDT1A2B3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2B3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2B3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2B3A4CX-DLDT1A2B3A4C)                         
              DC        AL1(DLDT1A2B3A4CG-DLDT1A2B3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2B3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2B3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2B3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2B3A4AX-DLDT1A2B3A4A)                         
              DC        AL1(DLDT1A2B3A4AG-DLDT1A2B3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2B3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
              DC        AL2(0)                                                  
DLDT1A2B3A4AX EQU      *                                                        
                                                                                
DLDT1A2B3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2B3AX   EQU     *                                                         
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2C3A    DS      0X                                                        
              DC       AL2(DLDT1A2C3AX-DLDT1A2C3A)                              
              DC       AL1(DLDT1A2C3AG-DLDT1A2C3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2C3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2C3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2C3A4CX-DLDT1A2C3A4C)                         
              DC        AL1(DLDT1A2C3A4CG-DLDT1A2C3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2C3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2C3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2C3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2C3A4AX-DLDT1A2C3A4A)                         
              DC        AL1(DLDT1A2C3A4AG-DLDT1A2C3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2C3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2D3A    DS      0X                                                        
              DC       AL2(DLDT1A2D3AX-DLDT1A2D3A)                              
              DC       AL1(DLDT1A2D3AG-DLDT1A2D3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2D3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2D3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2D3A4CX-DLDT1A2D3A4C)                         
              DC        AL1(DLDT1A2D3A4CG-DLDT1A2D3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2D3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2D3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2D3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2D3A4AX-DLDT1A2D3A4A)                         
              DC        AL1(DLDT1A2D3A4AG-DLDT1A2D3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2D3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2E3A    DS      0X                                                        
              DC       AL2(DLDT1A2E3AX-DLDT1A2E3A)                              
              DC       AL1(DLDT1A2E3AG-DLDT1A2E3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2E3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2E3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2E3A4CX-DLDT1A2E3A4C)                         
              DC        AL1(DLDT1A2E3A4CG-DLDT1A2E3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2E3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2E3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2E3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2E3A4AX-DLDT1A2E3A4A)                         
              DC        AL1(DLDT1A2E3A4AG-DLDT1A2E3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2E3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2F3A    DS      0X                                                        
              DC       AL2(DLDT1A2F3AX-DLDT1A2F3A)                              
              DC       AL1(DLDT1A2F3AG-DLDT1A2F3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2F3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2F3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2F3A4CX-DLDT1A2F3A4C)                         
              DC        AL1(DLDT1A2F3A4CG-DLDT1A2F3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2F3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2F3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2F3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2F3A4AX-DLDT1A2F3A4A)                         
              DC        AL1(DLDT1A2F3A4AG-DLDT1A2F3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2F3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2G3A    DS      0X                                                        
              DC       AL2(DLDT1A2G3AX-DLDT1A2G3A)                              
              DC       AL1(DLDT1A2G3AG-DLDT1A2G3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2G3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2G3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2G3A4CX-DLDT1A2G3A4C)                         
              DC        AL1(DLDT1A2G3A4CG-DLDT1A2G3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2G3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2G3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2G3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2G3A4AX-DLDT1A2G3A4A)                         
              DC        AL1(DLDT1A2G3A4AG-DLDT1A2G3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2G3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2H3A    DS      0X                                                        
              DC       AL2(DLDT1A2H3AX-DLDT1A2H3A)                              
              DC       AL1(DLDT1A2H3AG-DLDT1A2H3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2H3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2H3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2H3A4CX-DLDT1A2H3A4C)                         
              DC        AL1(DLDT1A2H3A4CG-DLDT1A2H3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2H3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2H3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2H3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2H3A4AX-DLDT1A2H3A4A)                         
              DC        AL1(DLDT1A2H3A4AG-DLDT1A2H3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2H3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2J3A    DS      0X                                                        
              DC       AL2(DLDT1A2J3AX-DLDT1A2J3A)                              
              DC       AL1(DLDT1A2J3AG-DLDT1A2J3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2J3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2J3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2J3A4CX-DLDT1A2J3A4C)                         
              DC        AL1(DLDT1A2J3A4CG-DLDT1A2J3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2J3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2J3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2J3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2J3A4AX-DLDT1A2J3A4A)                         
              DC        AL1(DLDT1A2J3A4AG-DLDT1A2J3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2J3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2K3A    DS      0X                                                        
              DC       AL2(DLDT1A2K3AX-DLDT1A2K3A)                              
              DC       AL1(DLDT1A2K3AG-DLDT1A2K3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2K3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2K3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2K3A4CX-DLDT1A2K3A4C)                         
              DC        AL1(DLDT1A2K3A4CG-DLDT1A2K3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2K3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2K3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2K3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2K3A4AX-DLDT1A2K3A4A)                         
              DC        AL1(DLDT1A2K3A4AG-DLDT1A2K3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2K3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2L3A    DS      0X                                                        
              DC       AL2(DLDT1A2L3AX-DLDT1A2L3A)                              
              DC       AL1(DLDT1A2L3AG-DLDT1A2L3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2L3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2L3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2L3A4CX-DLDT1A2L3A4C)                         
              DC        AL1(DLDT1A2L3A4CG-DLDT1A2L3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2L3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2L3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2L3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2L3A4AX-DLDT1A2L3A4A)                         
              DC        AL1(DLDT1A2L3A4AG-DLDT1A2L3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2L3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2M3A    DS      0X                                                        
              DC       AL2(DLDT1A2M3AX-DLDT1A2M3A)                              
              DC       AL1(DLDT1A2M3AG-DLDT1A2M3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2M3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2M3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2M3A4CX-DLDT1A2M3A4C)                         
              DC        AL1(DLDT1A2M3A4CG-DLDT1A2M3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2M3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2M3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2M3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2M3A4AX-DLDT1A2M3A4A)                         
              DC        AL1(DLDT1A2M3A4AG-DLDT1A2M3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2M3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2N3A    DS      0X                                                        
              DC       AL2(DLDT1A2N3AX-DLDT1A2N3A)                              
              DC       AL1(DLDT1A2N3AG-DLDT1A2N3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2N3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2N3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2N3A4CX-DLDT1A2N3A4C)                         
              DC        AL1(DLDT1A2N3A4CG-DLDT1A2N3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2N3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2N3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2N3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2N3A4AX-DLDT1A2N3A4A)                         
              DC        AL1(DLDT1A2N3A4AG-DLDT1A2N3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2N3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
*                                                                               
*** FIL/SRC/MED = TP/NET/CABLE ***                                              
*                                                                               
DLDT1A2O      DS     0X                                                         
              DC      AL2(DLDT1A2OX-DLDT1A2O)                                   
              DC      AL1(DLDT1A2OG-DLDT1A2O)                                   
              DC      C'TP CN'              TP/NET/CABLE                        
DLDT1A2OG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2O3A    DS      0X                                                        
              DC       AL2(DLDT1A2O3AX-DLDT1A2O3A)                              
              DC       AL1(DLDT1A2O3AG-DLDT1A2O3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2O3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2O3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2O3A4CX-DLDT1A2O3A4C)                         
              DC        AL1(DLDT1A2O3A4CG-DLDT1A2O3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2O3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2O3A4CX EQU      *                                                        
                                                                                
DLDT1A2O3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2O3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2O3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2OX     EQU    *                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*** FIL/SRC/MED = RTP/RAR/RADIO ***                                             
*                                                                               
DLDT1A2P      DS     0X                                                         
              DC      AL2(DLDT1A2PX-DLDT1A2P)                                   
              DC      AL1(DLDT1A2PG-DLDT1A2P)                                   
              DC      C'TP RR'             RTP/RAR/RADIO                        
DLDT1A2PG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2P3A    DS      0X                                                        
              DC       AL2(DLDT1A2P3AX-DLDT1A2P3A)                              
              DC       AL1(DLDT1A2P3AG-DLDT1A2P3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2P3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2P3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2P3A4CX-DLDT1A2P3A4C)                         
              DC        AL1(DLDT1A2P3A4CG-DLDT1A2P3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2P3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2P3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2P3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2P3A4AX-DLDT1A2P3A4A)                         
              DC        AL1(DLDT1A2P3A4AG-DLDT1A2P3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2P3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
              DC        AL2(0)                                                  
DLDT1A2P3A4AX EQU      *                                                        
                                                                                
DLDT1A2P3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2P3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2P3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2PX     EQU    *                                                          
*                                                                               
*  COUNTY COVERAGE DOWNLOAD TABLE                                               
*** FIL/SRC/MED = CTP/NSI  COUNTY COVERAGE                                      
*                                                                               
DLDT1A2S      DS     0X                                                         
              DC      AL2(DLDT1A2SX-DLDT1A2S)                                   
              DC      AL1(DLDT1A2SG-DLDT1A2S)                                   
*****         DC      C'TP NU'              COUNTY COVERAGE                     
              DC      C'CTPNU'              COUNTY COVERAGE                     
DLDT1A2SG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2S3A    DS      0X                                                        
              DC       AL2(DLDT1A2S3AX-DLDT1A2S3A)                              
              DC       AL1(DLDT1A2S3AG-DLDT1A2S3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2S3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2S3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2S3A4CX-DLDT1A2S3A4C)                         
              DC        AL1(DLDT1A2S3A4CG-DLDT1A2S3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2S3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2S3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2S3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2S3A4AX-DLDT1A2S3A4A)                         
              DC        AL1(DLDT1A2S3A4AG-DLDT1A2S3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2S3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
              DC        AL2(0)                                                  
DLDT1A2S3A4AX EQU      *                                                        
                                                                                
DLDT1A2S3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2S3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2S3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2SX     EQU    *                                                          
*                                                                               
*                                                                               
*                                                                               
*  OVERNIGHTS TIME PERIOD TABLE                                                 
*** FIL/SRC/MED = CTP/NSI  COUNTY COVERAGE                                      
*                                                                               
DLDT1A2T      DS     0X                                                         
              DC      AL2(DLDT1A2TX-DLDT1A2T)                                   
              DC      AL1(DLDT1A2TG-DLDT1A2T)                                   
              DC      C'TP NO'              OVERNIGHTS                          
DLDT1A2TG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2T3A    DS      0X                                                        
              DC       AL2(DLDT1A2T3AX-DLDT1A2T3A)                              
              DC       AL1(DLDT1A2T3AG-DLDT1A2T3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2T3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2T3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2T3A4CX-DLDT1A2T3A4C)                         
              DC        AL1(DLDT1A2T3A4CG-DLDT1A2T3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2T3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2T3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2T3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2T3A4AX-DLDT1A2T3A4A)                         
              DC        AL1(DLDT1A2T3A4AG-DLDT1A2T3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2T3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
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
****************************************************************                
*                                                                               
*  FUSION TIME PERIOD TABLE                                                     
*** FIL/SRC/MED = CTP/NSI  COUNTY COVERAGE                                      
*                                                                               
DLDT1A2U      DS     0X                                                         
              DC      AL2(DLDT1A2UX-DLDT1A2U)                                   
              DC      AL1(DLDT1A2UG-DLDT1A2U)                                   
              DC      C'TP FT'              FUSION                              
DLDT1A2UG     EQU    *                                                          
                                                                                
*                                                                               
**** ELEMENT MAP CODE = FMHBOOK ****                                            
*                                                                               
DLDT1A2U3A    DS      0X                                                        
              DC       AL2(DLDT1A2U3AX-DLDT1A2U3A)                              
              DC       AL1(DLDT1A2U3AG-DLDT1A2U3A)                              
              DC       AL2(FMHBOOK)         BOOKS                               
DLDT1A2U3AG   EQU     *                                                         
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER3 *****                                          
*                                                                               
DLDT1A2U3A4C  DS       0X                                                       
              DC        AL2(DLDT1A2U3A4CX-DLDT1A2U3A4C)                         
              DC        AL1(DLDT1A2U3A4CG-DLDT1A2U3A4C)                         
              DC        AL4(XTRCVER3)                                           
DLDT1A2U3A4CG EQU      *                                                        
              DC        AL2(FMDBOOKGP2A)    GROUP #2 DATA (W/ DEM32 BK)         
              DC        AL2(FMDBOOKDLMT)    DELIMITERS                          
              DC        AL2(0)                                                  
DLDT1A2U3A4CX EQU      *                                                        
                                                                                
*                                                                               
***** EXTRACT VERSION = XTRCVER1 *****                                          
*                                                                               
DLDT1A2U3A4A  DS       0X                                                       
              DC        AL2(DLDT1A2U3A4AX-DLDT1A2U3A4A)                         
              DC        AL1(DLDT1A2T3A4AG-DLDT1A2T3A4A)                         
              DC        AL4(XTRCVER1)                                           
DLDT1A2U3A4AG EQU      *                                                        
              DC        AL2(FMDFMBK)        BOOK                                
              DC        AL2(FMDBKTC)        BOOK TYPE                           
              DC        AL2(0)                                                  
DLDT1A2U3A4AX EQU      *                                                        
                                                                                
DLDT1A2U3A4   DS       0X                                                       
              DC        AL2(0)                                                  
DLDT1A2U3AX   EQU     *                                                         
         EJECT                                                                  
*                                                                               
DLDT1A2U3     DS      0X                                                        
              DC       AL2(0)                                                   
DLDT1A2UX     EQU    *                                                          
****************************************************************                
         EJECT                                                                  
DLDT1A2       DS     0X                                                         
              DC      AL2(0)                                                    
DLDT1AX       EQU   *                                                           
         EJECT                                                                  
DLDT1         DS    0X                                                          
              DC     AL2(0)                                                     
DLDATTABX     EQU  *                                                            
***********************************************************************         
*                                                                               
       ++INCLUDE DESTACODE                                                      
*                                                                               
         EJECT                                                                  
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
         ORG                                                                    
         EJECT                                                                  
DM06WRKD DSECT                                                                  
DM06W_RD DS    A                   DEM03'S RD                                   
DM06WRKX EQU   *                                                                
DM06WRKL EQU   DM06WRKX-DM06WRKD                                                
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
RELO06   DS    F                                                                
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
ALOCWRK  DS    A                   A(LOC WRK AREA)-USED B/W SUBRTN POOL         
TMPRTYP  DS    XL(L'TDRTYPE)       TEMP STORAGE FOR TSAR DEM RECD TYP           
GOSUBN   DS    XL1                 SUB-ROUTINE NUMBER                           
ADLDNTRY DS    A                                                                
ASUBRTN  DS    A                                                                
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUBR01 POOL INTERFACE)                     
ADLDTABS DS    A                                                                
ABTPTBNH DS    A                   A(BTPTPNHT)                                  
ASTATTAB DS    A                   A(STACODE) TABLE                             
*                                                                               
TEMPBRO  DS    CL12                                                             
*                                                                               
STARTDAY DS    XL1                 NSIWEEK START DAY (1-7)                      
TMPBOOK  DS    XL(L'TDRBOOK)        TEMP STORAGE FOR BOOK VALUE                 
TMPBTYP  DS    XL(L'TDRBTYP)         "      "     "  BOOKTYPE                   
SPILLMKT DS    XL(L'STASPILL)      SPILL MARKET                                 
MYD32DLM DS    CL(L'SPOVDLMT)      DELIMITERS IN DEM32 DOWNLOAD                 
DOWEEK   DS    CL2                 2 BYTE BBM FLAG                              
SVPREWBK DS    CL2                 SAVE BOOK                                    
SVLASTM  DS    CL2                 SAVE BOOK                                    
DEMHKFLG DS    X                   FLAG FOR DEMHOOK                             
DMHKFLGY DC    C'Y'                                                             
DMHKFLGN DC    C'N'                                                             
TWOCHRBT DS    X                   2 CHAR BOOKTYPE FLAG                         
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINBOOK  DS    XL2                 BOOK VALUE                                   
BINBTYP  DS    XL1                 BOOKTYPE                                     
BINRECL  EQU   *-BINRECD                                                        
                                                                                
                                                                                
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  KEY OF TSAR DEMO RECORD                      
TDRTYPE  DS    XL1                  RECORD TYPE                                 
TDRTMSGQ EQU   X'10'                 (DUMMY) MESSAGE RECORD                     
TDRTBKQ  EQU   X'20'                 BOOK RECORD                                
TDRBOOK  DS    XL2                  BOOK VALUE  (REVERSE CHRONOLOGICAL)         
TDRBTYP  DS    XL1                  BOOKTYPE                                    
         DS    XL1                  SPARE OR EXTRA BYTE FOR STATE CODE          
TDRKEYL  EQU   *-TSDEMRCD          KEY LENGTH                                   
                                                                                
         ORG   TDRTYPE+L'TDRTYPE                                                
TDRK2BTP DS    XL1                  BOOKTYPE                                    
         DS    XL1                  SPARE OR EXTRA BYTE FOR STATE CODE          
TDRK2BK  DS    XL2                  BOOK VALUE  (REVERSE CHRONOLOGICAL)         
TDRKEY2L EQU   *-TSDEMRCD          KEY LENGTH                                   
                                                                                
         ORG                                                                    
TDRDUMMY DS    XL1                  DUMMY DATA FIELD                            
TDRRECL  EQU   *-TSDEMRCD          REC LENGTH                                   
                                                                                
         DS    0XL((TDRKEYL-TDRKEY2L)+1)                                        
         DS    0XL((TDRKEY2L-TDRKEYL)+1)                                        
*                                                                               
*                                                                               
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL2                 L(ENTRY)                                     
DLDTDSP  DS    XL1                 DISPLACEMENT TO DATA                         
DLDTFIXL EQU   *-DLDTABD                                                        
*                                                                               
DLDTDATA DS    0X                                                               
                                                                                
         ORG   DLDTDATA                                                         
DLDTRTYP DS     XL(L'TDRTYPE)       TSAR DEMO RECORD TYPE                       
                                                                                
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
                                                                                
                                                                                
* DSECT TO COVER PREVIOUS VALUES OUTPUTTED TO STEREO                            
*                                                                               
SPVOVALD DSECT                                                                  
SPOVSETE DS    CL1                 SET ELEMENT YET?                             
SPOVDLMT DS    CL1                 DELIMITERS                                   
SPOVBTYP DS    XL(L'TDRK2BTP)      BOOK TYPE                                    
SPVOVALQ EQU   *-SPVOVALD                                                       
         DS    0CL(L'STPRVOVL-SPVOVALQ+1)                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069DEDEM06   06/15/11'                                      
         END                                                                    
