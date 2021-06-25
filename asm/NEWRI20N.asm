*          DATA SET NEWRI20N   AT LEVEL 174 AS OF 03/31/08                      
*PHASE T32020A                                                                  
*INCLUDE NETSPB                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE NEPACC                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE SPGETBFR                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE NETACCN           **   FOR 3 CHAR PROD                                 
*INCLUDE NETNET                                                                 
         TITLE 'T32020 - NETWORK WRITER CONTROL'                                
*****************************************************************               
* 7/16/07 THIS IS NEWRI2G - NBVTYPS HAS 2 CHAR VTYP CODE -> NENTVLDEMO          
* 7/16/07 THIS IS NEWRI2G WITH TESTING BLDPRD -                                 
* 7/16/07 THIS IS NEWRI20N WITH PLGOAL CODING -                                 
* 4/30/07 STABRD TURNED OFF AGAIN !!!                                           
* 4/30/07 STABRD TUIRNED ON - BUG IN GETPRD3 FIXED LEVEL 156                    
* 2/26/07   PXZ   STABRD FOR MANUAL BILLS NEEDS WORK - MADE LIVE WITH-          
*                 OUT STABRD -                                                  
* 12/28/06   PXZ   STABRD FOR MANUAL BILLS ADDED  + TMTLTBL READ                
*                  HAD TO CREATE RDB1XPROF BECAUSE OF ADDRESSABILITY            
*                                                                               
* 6/2/03     PXZ   NEWRI20 NO LONGER SETS NBINDS2 WITH NBBILLRD                 
*                  THIS IS IN NEWRIGEN IF BILLING KEYWORD REQUESTED             
* 3/13/03    PXZ   LEVEL 93  DEFAULT TO NO ACTUAL DEMOS                         
*                  NBACTOPT SET IN NEWRIGEN IF ACTUAL DEMOS NEEDED              
*****************************************************************               
T32020   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32020**                                                       
         USING T32020,RB,R5,R8                                                  
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         LA    R8,2048(R5)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         L     R1,ANETWS1          PASS CLIENT RECORD IN W/S AREA 1             
         ST    R1,NBACLI                                                        
         L     R7,ANETWS2          2500 BYTES IN ANETWS2,3,4 NOW FREE           
         A     R7,=F'500'                                                       
         USING WRITERD,R7                                                       
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         LA    R1,XDDEMBLK                                                      
         ST    R1,NDADMBLK                                                      
         L     R1,=A(RAWDATA)          (EXTENSION FOR RAW DEMOS)                
         ST    R1,XDARAWEX                                                      
         LA    R1,DBLOCK                                                        
         ST    R1,NDADBLCK                                                      
         LA    R1,DATEAREA                                                      
         ST    R1,NDADATES                                                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE1A                                                             
         BRAS  RE,INITOFLN         INIT OFF LINE                                
         EJECT                                                                  
                                                                                
* CHECK  SECURITY                                                               
AE1A     CLC   =C'INVENT',SPLNAM  SPECIAL FOR MEDIAVEST                         
         BNE   NOTMDV                                                           
         CLC   NBSELAGY,=C'DU'         ONLY MEDIAVEST=DU                        
         BNE   NOTMDV                                                           
         OI    NBUNTSW,X'20'           SKIP OFFICE CHECK                        
         CLI   OFFLINE,C'Y'            AND IF OFFLINE                           
         BE    AE1                     SKIP SECURITY                            
         CLI   TWAOFFC,C'*'        DDS                                          
         BE    AE1                 SKIP CHECK                                   
         B     AE1B                    ELSE TURN ON SECURITY                    
NOTMDV   EQU   *                                                                
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    AE1                 SKIP CHECK                                   
         CLI   TWAOFFC,C'*'        DDS                                          
         BE    AE1                 SKIP CHECK                                   
                                                                                
* SPECIAL CROSS AGY READ SEC FOR @OJ (OM AND JW PARTICULAR CASE)                
         CLC   =C'@OJ',SPLCLI     IF OJ                                         
         BE    AE1B               FORCE SECURITY ON                             
                                                                                
         TM    GENSTAT4,NODELLST   IS SECURITY ON (SET IN NEWRI00)              
         BNO   AE1                                                              
         TM    GENSTAT5,NOCHGLST                                                
         BNO   AE1                                                              
         MVC   NDAUTH,TWAAUTH      SET AUTHORIZATION                            
AE1B     MVI   NDSECFLG,C'Y'       SET FLAG                                     
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION-DELETE                                
         BE    SECERR                                                           
         CLI   ACTNUM,ACTADD       ACTION=ADD                                   
         BE    SECERR                                                           
         CLI   ACTNUM,ACTCHA       ACTION=CHANGE                                
         BNE   AE1                                                              
SECERR   LA    R2,SPLCLIH                                                       
         MVC   HALF,=H'673'        SECURITY LOCKOUT                             
         B     ERR2                                                             
                                                                                
AE1      CLI   MODE,PRINTREP                                                    
         BE    REPS                                                             
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,DISPREC                                                     
         BE    DISP                                                             
         CLI   MODE,VALREC                                                      
         BE    AE2                                                              
         CLI   MODE,ERRHOOK                                                     
         BNE   *+8                                                              
         BRAS  RE,MQRPTERR                                                      
         J     XIT                                                              
*                                                                               
SOONERR  LA    R2,CONWHENH                                                      
         MVI   ERROR,INVPRINT                                                   
         B     EDERR                                                            
                                                                                
* VALIDATE KEY                                                                  
*                                                                               
VKEY     CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    VKEY0                                                            
         CLI   ACTNUM,ACTADD       TEST ACTION=ADD                              
         BE    VKEY0                                                            
         CLI   ACTNUM,ACTDIS       ACTION=DISPLAY                               
         BNE   VKEYX                                                            
*                                                                               
VKEY0    BAS   RE,RDPROF           CHK WR PROFILE                               
         CLI   WORK+18,C'Y'        TEST FILTER REQUIRED                         
         BNE   VKEYX                                                            
*                                                                               
VKEY01   L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   5(R2),4             TEST AT LEAST 4 FILTER CHARACTERS            
         BL    FILTERR                                                          
         ZIC   RE,5(R2)                                                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
VKEY2    CLI   0(R1),C'*'                                                       
         BE    FILTERR                                                          
         CLI   0(R1),C'-'                                                       
         BE    FILTERR                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VKEY2                                                         
VKEY3    MVC   FILTSV,8(R2)        SAVE VALID FILTER                            
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE                                                                  
RDPROF   NTR1                      YES-READ AGENCY'S WR PROFILE                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0WR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DISP     BAS   RE,RDPROF           READ AGENCY'S WR PROFILE                     
         CLI   WORK+18,C'Y'        TEST FILTER REQUIRED                         
         BNE   DISPX                                                            
         CLI   TWAOFFC,C'*'        IF DDS                                       
         BE    DISPX                                                            
         LA    R2,SPLFLTH          YES-CHECK IT                                 
         CLC   FILTSV,SPLFLT       DOES IT MATCH                                
         BE    DISPX               YES                                          
         MVC   SPLFLT,FILTSV       NO-RETURN REQUESTED FILTER                   
         OI    SPLFLTH+6,X'80'                                                  
         LA    R1,SPLCLIH            -CLEAR ALL UPROTECTED FIELDS               
         SR    R0,R0                                                            
         B     DISP4                                                            
*                                                                               
DISP2    ICM   R0,1,0(R1)                                                       
         BZ    DISPX                 - AND EXIT                                 
         AR    R1,R0                                                            
*                                                                               
DISP4    TM    1(R1),X'20'                                                      
         BO    DISP2                                                            
         ZIC   RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         BM    DISP2                                                            
         TM    1(R1),X'02'                                                      
         BZ    *+12                                                             
         SH    RE,=H'8'                                                         
         BM    DISP2                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         B     DISP2                                                            
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*              INITIALIZE                                                       
         SPACE 3                                                                
AE2      MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBACTOPT,0          DEFAULT TO NO ACTUAL DEMOS                   
         MVI   LSTRD,0                                                          
         MVI   BHRD,0                                                           
         XC    BILLFLT,BILLFLT                                                  
         MVI   NDANYREC,C'N'                                                    
         CLI   SPLRECPH+5,0                                                     
         BE    *+8                                                              
         MVI   NDANYREC,C'Y'                                                    
                                                                                
********************************************                                    
* - CROSS AGENCY (AGENCY LIST) READ CHECK                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    OJXX                                                             
         LA    R2,SPLFLTH          POINT ERROR MSG TO FILTER FIELD              
         CLC   =C'@OJX',SPLCLI     IF SPECIAL OM/JW CROSS AGY REPORT?           
         BNE   OJXX                                                             
         CLI   TWAOFFC,C'*'        ..UNLESS DDS TERMINAL                        
         BE    OJXX                                                             
         CLC   =C'CMBO',SPLFLT     MUST HAVE PASSWORD                           
         BNE   FILTERR                                                          
OJXX     EQU   *                                                                
***********************************************                                 
                                                                                
                                                                                
* - CROSS AGENCY (AGENCY LIST) READ CHECK                                       
         CLI   SPLCLI,C'@'         ..IF AGY LIST READ                           
         BNE   AE2A                                                             
         CLI   OFFLINE,C'Y'        ..AND ONLINE                                 
         BE    SKIPTWA                                                          
         CLI   TWAOFFC,C'*'        ..UNLESS DDS TERMINAL                        
         BE    SKIPTWA                                                          
         CLC   =C'TH',NBSELAGY     ..OR ZENITH (TH) AGENCY                      
         BE    SKIPTWA                                                          
         CLI   TWAWHEN,2           ..SOON IS VERBOTEN                           
         BE    SOONERR                                                          
SKIPTWA  MVC   LSTCLT,SPLCLI       .SAVE REQUEST @NNN                           
         OI    NDCROSS,X'01'       .SET GENERAL CROSS AGY READ                  
*                                                                               
         CLC   TWAORIG,=X'115D'    ,, ZEGM RESTRICTED                           
         BNE   *+14                                                             
         CLC   LSTCLT,=C'@GI2'      ,,TO GI2 ONLY                               
         BNE   AUTHERR                                                          
*                                                                               
         CLC   LSTCLT,=C'@JOM'      IF JWNYRE/OMNYRE                            
         BE    GI2RES               ALLOW ALL KEYWORDS FOR NOW                  
         CLC   =C'@OJ',LSTCLT       IF JWNYRE/OMNYRE                            
         BE    GI2RES               ALLOW ALL KEYWORDS FOR NOW                  
         CLC   =C'@OMS',LSTCLT      IF OMCH/MSCH                                
         BE    GI2RES               ALLOW ALL KEYWORDS FOR NOW                  
*                                                                               
         CLC   LSTCLT,=C'@GI2'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@TMS'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@LEX'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@RES'                                                  
         BNE   NOTGI2                                                           
GI2RES   OI    NDCROSS,X'10'       X'10'=@GI2/@RES/LEX/TMS                      
         B     ACS03                                                            
*                                                                               
NOTGI2   OI    NDCROSS,X'02'       .SET CROSS AGY READ KEYWORD CHECK            
         CLC   LSTCLT,=C'@YAL'          @YAL=NW=X'02'                           
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@ATT'          @ATT=AT=X'02'                           
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@BCS'          @BCS=AT=X'02'                           
         BE    ACS03                                                            
         CLC   =C'@FC',LSTCLT           @FC=FCNY=X'02'                          
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@CHR'          CHR=X'04'                               
         BNE   ACS03                                                            
         NI    NDCROSS,X'FF'-X'02'                                              
         OI    NDCROSS,X'04'       .SET CROSS AGY READ KEYWORD CHECK            
         B     ACS03                                                            
ACS03    CLI   OFFLINE,C'Y'        .AND IF ON-LINE                              
         BE    AE2A                                                             
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         L     R2,=A(AGYTABL)      .CHECK AUTHORIZED ACCESS                     
         AR    R2,R4                                                            
ACS05    CLC   NBSELAGY,0(R2)                                                   
         BNE   ACS10                                                            
         CLC   SPLCLI+1(3),4(R2)                                                
         BE    ACS20                                                            
ACS10    LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BE    AUTHERR             .AUTHORIZATION ERROR                         
         B     ACS05                                                            
ACS20    MVC   NBSELCLI,=C'ALL'    .OK/ FUDGE ON-LINE CLI VALIDATION            
         B     AE4D                                                             
         SPACE                                                                  
*                                                                               
AE2A     CLI   OFFLINE,C'Y'                                                     
         BNE   AE3                                                              
*                                                                               
         OI    GENSTAT7,GES7ERHK   SET FOR ERROR HOOK                           
         CLI   SPLCLI,C'@'         READ AGY LIST                                
         BNE   AE2B                                                             
         BAS   RE,LSTRDS                                                        
         B     AE3                                                              
AE2B     CLC   SPLCLI(3),=C'***'   ALLOW FOR DDS REQUEST                        
         BNE   AE3                                                              
         XC    NBSELAGY,NBSELAGY                                                
         XC    NBEFFAGY,NBEFFAGY                                                
         MVC   SPLCLI(3),=C'ALL'                                                
                                                                                
*                                                                               
AE3      DS    0H                                                               
         CLC   =C'BH',SPLFLAV       CHK BILL HEADER READ                        
         BNE   AE3D                                                             
         MVI   BHRD,1                                                           
         CLI   SPLFLAV+2,C'O'                                                   
         BNE   *+8                                                              
         MVI   BHRD,2              ONLY BILL HEADER READ                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE3D                                                             
         L     R3,=A(MYIO)         CLEAR BLOCK FOR BILLS                        
***      A     R3,=F'4000'                                                      
         A     R3,=F'6000'         THOSE F...N CLEARED STATUS RECS              
         USING BHBLOCK,R3                                                       
         LR    RE,R3                                                            
         LA    RF,BHBLENE                                                       
         XCEF                                                                   
         MVC   BHBLOCK,=C'**BBLK**'      BILL HEADER BLOCKS IN DUMP             
         MVC   BHDATA,=C'**BKEY**'                                              
         MVC   BHDOLS,=C'**BDOL**'                                              
                                                                                
AE3D     CLC   CONREC(2),=CL2'PW'   CHECK FOR PUP WRITER                        
***      BNE   AE4                                                              
****     OI    NDCOLIND,X'20'       SET PUP INDICATOR                           
****     MVI   FTERMFLG,0          (REQUIRED FIELDS)                            
****     MVI   SPLFLAVH+5,1                                                     
****     MVI   SPLPROH+5,3                                                      
****     MVI   SPLESTH+5,3                                                      
         SPACE 1                                                                
*              EDIT CLI/PRD/EST/NET/DPT/PACKAGE                                 
*                                                                               
AE4      DS    0H                  CLIENT VALIDATION                            
         EJECT                                                                  
                                                                                
*        ARE WE POINTING TO CONTROL FILE WHEN PF12                              
*        PRESSED IN REPORT MODE WHEN LISTING STORED REQ RECS?                   
         LA    R2,SPLCLIH      POSITION CURSOR FOR ERROR                        
         CLC   =C'CTF',SYSDIR                                                   
         BE    EDERR                                                            
*                                                                               
         LA    R2,SPLCLIH                                                       
         NETGO NVCLIALL,DMCB,0                                                  
*                                                                               
**       CLC   =C'ALL',NBSELCLI                                                 
**       BNE   *+8                                                              
**       MVI   REQSML,C'L'        LARGE JOB                                     
*                                                                               
         CLI   9(R2),C'='         CLIENT GROUP?                                 
         BE    AE4B                                                             
                                                                                
* - CHECK SPECIAL CLIENT GRP ACCESS LIMIT                                       
***      GOTO1 =A(OVERFLW),DMCB,(RC),9,RR=YES                                   
***      BE    AE4B                                                             
***      MVC   HALF,=H'746'        CLIENT GROUP LIMIT ACCESS                    
***      B     ERR2                                                             
                                                                                
AE4B     CLI   LSTRD,1             IF 1ST TIME XAGY READ                        
         BNE   *+10                                                             
         MVC   SPLCLI(4),LSTCLT    RESET @NNN TO SCREEN FIELD                   
*                                                                               
         MVI   NBDODEMS,C'N'       PRESET FOR NO DEMOS                          
         CLI   BHRD,0              IF BILLHEAD READ                             
         BE    AE4D                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE4D                                                             
         MVC   BHAGYMD,NBACTAM     SET AGY/MD OF CLIENT REC                     
         CLC   =C'ALL',NBSELCLI                                                 
         BE    AE4D                                                             
         MVC   BHSELCLI,NBEFFCLI   SET CLIENT                                   
         SPACE 1                                                                
*                                                                               
AE4D     DS    0H                                                               
*                                                                               
WR016    DS    0H                                                               
         TM    NDCOLIND,X'20'      TEST FOR PUP REQUEST                         
         BNO   WR017                                                            
****     MVC   SPLPRO(3),=CL3'ALL'                                              
****     MVC   SPLEST(3),=CL3'ALL'                                              
****     MVI   SPLPROH+5,3                                                      
****     MVI   SPLESTH+5,3                                                      
         SPACE 1                                                                
*                                  PRODUCT & PRODUCT GROUP                      
WR017    LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,0                                                  
         CLC   =C'PEN',SPLCLI                                                   
         BNE   WR017A                                                           
         CLC   =C'BD',NBSELAGY                                                  
         BNE   WR017A                                                           
* - GET FULL AGENCY ID                                                          
         GOTOR GETAGYID                                                         
******   GOTO1 =A(OVERFLW),DMCB,(RC),6,RR=YES                                   
         BNE   EDERR                                                            
*                                                                               
WR017A   CLI   BHRD,0              CHK BILL HEADER READ                         
         BE    WR017D                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR017D                                                           
         CLC   =C'ALL',NBSELPRD                                                 
         BE    WR017D                                                           
         CLC   =C'POL',NBSELPRD                                                 
         BE    WR017D                                                           
         MVC   BHSELPRD,NBSELPRD                                                
         SPACE 1                                                                
*                                  ESTIMATE VALIDATION                          
WR017D   LA    R2,SPLESTH                                                       
         OI    NBINDS6,NBI6XDEM                                                 
WR017DD  NETGO NVESTRNG,DMCB,0,XDDEMBLK                                         
*                                                                               
WR017DDD CLI   BHRD,0              CHK BILL HEADER READ                         
         BE    WR017E                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR017E                                                           
         MVC   BHSELEST,NBSELEST                                                
         MVC   BHSELESE,NBSELESE                                                
         SPACE 1                                                                
WR017E   MVI   FTERMFLG,1          (OPTIONAL FIELDS)                            
         SPACE 1                                                                
*                                  NETWORK VALIDATION                           
         DS    0H                                                               
         XC    BINMKT,BINMKT                                                    
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,BINMKT                                             
         CLI   BHRD,0              CHK BILL HEADER READ                         
         BNE   WR15                YES                                          
         CLC   =C'PROGDOWN',CONREC       IF PROG DOWN REQUEST                   
         BNE   WR20                                                             
         MVI   ERROR,INVALID                                                    
         OC    NBSELNET,NBSELNET       NET MUST BE SPECIFIED                    
         BZ    EDERR                                                            
         B     WR20                                                             
WR15     CLI   OFFLINE,C'Y'                                                     
         BNE   WR20                                                             
         CLC   =C'ALL,',SPLNET     MEDIA FILTER                                 
         BNE   *+10                                                             
         MVC   BHSELMED,SPLNET+4                                                
         CLC   =C'ALL',NBSELNET                                                 
         BE    *+10                                                             
         MVC   BHSELNET,NBSELNET                                                
         SPACE 1                                                                
*                                  DAYPART VALIDATION                           
WR20     LA    R2,SPLDPTH                                                       
         NETGO NVDPTALL,DMCB,0                                                  
         OI    SPLDPTH+6,X'80'                                                  
         CLI   BHRD,0                                                           
         BE    WR30                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR30                                                             
         MVC   BHSELDPT,NBSELDP                                                 
         SPACE 1                                                                
*                                  PACKAGE VALIDATION                           
WR30     LA    R2,SPLPAKH                                                       
         TM    NDCOLIND,X'20'                                                   
         BNO   VPAK20                                                           
****     XC    SPLPRO,SPLPRO                                                    
****     XC    SPLEST,SPLEST                                                    
****     MVI   SPLPROH+5,0                                                      
****     MVI   SPLESTH+5,0                                                      
****     GOTO1 NDVALPLN            FOR PUP REQUEST VALIDATE PLAN                
*-IF NO INPUT IN DATE FIELDS DERIVE THE DATES FROM THE                          
*-PLAN RECORD.NDVALPLN STORES THE DATES IN WORK+20.                             
***8     OC    WORK+20(3),WORK+20  WAS DATES GENERATED                          
***      BZ    VPAK30                                                           
***      CLI   SPLRSTRH+5,0        WAS DATE INPUTTED                            
***      BNE   VPAK15                                                           
***      MVI   SPLRSTRH+5,7                                                     
***      OI    SPLRSTRH+6,X'80'    TRANSMIT THE FIELD                           
***      MVC   SPLRSTR(7),WORK+20                                               
VPAK15   CLI   SPLRENDH+5,0        WAS END DATE INPUTTED                        
***      BNE   VPAK30                                                           
***      MVI   SPLRENDH+5,8                                                     
***      OI    SPLRENDH+6,X'80'    TRANSMIT THE FIELD                           
***      MVC   SPLREND(8),WORK+27                                               
***      B     VPAK30                                                           
         SPACE 2                                                                
VPAK20   NETGO NVPAKLOK,DMCB,0                                                  
         SPACE 1                                                                
VPAK30   LA    R2,SPLFLAVH         FLAVOR                                       
         GOTO1 ANY                                                              
         MVC   NDFLAVOR,WORK                                                    
         CLC   =C'MXD',WORK                                                     
         BNE   *+8                                                              
         OI    NBSPLOPT,X'01'      SET MXD FLAVOR                               
         CLC   =C'BH',NDFLAVOR     IF BH READ                                   
         BNE   *+10                                                             
         MVC   NDFLAVOR,=C'E '     MAKE IT E FLAVOR                             
         CLI   NDFLAVOR+1,C'G'     IF GOALS ONLY REQUESTED                      
         BNE   VPK32                                                            
         MVI   NDFLAVOR+1,X'40'    GET IT OUT OF NDFLAVOR                       
         MVI   SPLFLAV+2,C'G'   SET IT IN SPLFLAV+2                             
         MVI   SPLFLAV+1,X'40'                                                  
         MVI   WORK+1,X'40'                                                     
VPK32    LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
         SPACE 1                                                                
         LA    R2,SPLOPTH          OPTIONS                                      
         GOTO1 NDVALOPT                                                         
         SPACE 1                                                                
         LA    R2,SPLOTHH          OTHERS                                       
         GOTO1 NDVALOTH                                                         
                                                                                
*                                                                               
         CLI   NDPQIX,C'Y'         PQIX OPTION?                                 
         BNE   NOPQIX                                                           
         TM    NDDOWNL,GLDLACTV    DOWNLOADING?                                 
         BZ    NOPQIX                                                           
*******  NI    NDDOWNL,X'FF'-GLDLNOHD    TURN OFF NO HEAD                       
*******  OI    NDDOWNL,GLDLHEAD    TURN ON DOWNHEAD                             
         MVI   NDFLAV3,C'N'        SET NO TARGETS ON DOWNHEAD                   
NOPQIX   DS    0H                                                               
                                                                                
         CLC   =C'TRANSMIT',CONREC       IF TRANSMIT REPORT                     
         BNE   UPLOAD                                                           
         OI    NDDOWNL,X'80'             MUST DOWNLOAD                          
         OI    GENSTAT2,NOREQDET         ALWAYS SET NOREQDET                    
         OI    REQRTYP,REQTDOWN                                                 
         OI    NDDOWNL,GLDLACTV+GLDLALPH+GLDLNOHD+GLDLCUT+GLDLNOTR              
         B     NOTRNSMT                                                         
*                                                                               
UPLOAD   CLC   =C'GOALDO',CONREC         GOAL DOWN REPORT?                      
         BE    UPLOAD10                                                         
         CLC   =C'PROGDOWN',CONREC       ,,IF PROG DOWN REPORT                  
         BNE   NOTRNSMT                                                         
         OI    NDDOWNL,GLDLNOTR           ,,DON'T TRUNCATE                      
UPLOAD10 OI    NDDOWNL,X'80'             MUST DOWNLOAD                          
         OI    REQRTYP,REQTDOWN                                                 
         OI    NDDOWNL,GLDLACTV+GLDLALPH+GLDLNOHD+GLDLCUT                       
                                                                                
*                                                                               
NOTRNSMT TM    NDDOWNL,X'80'          IF WE ARE DOWNLOADING                     
         BNO   VREC2                                                            
                                                                                
         OC    ARFPBLK,ARFPBLK        IF RFP                                    
         BZ    *+10                                                             
         MVC   TWAOUT,=CL8'DOWN'                                                
                                                                                
         LA    RE,RELO                                                          
         S     RE,RELO                                                          
         L     RF,=A(DOWNOPT)                                                   
         AR    RF,RE               RELOCATE ADDRESS                             
         MVI   0(RF),C'Y'                                                       
****     MVI   DOWNOPT,C'Y'                                                     
         CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
         BH    VREC2                                                            
         MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
         OI    CONOUTH+6,X'80'                                                  
         MVI   CONOUTH+4,4                                                      
         MVC   TWAOUT,CONOUT                                                    
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
* START-END DATE VALIDATION                                                     
VREC2    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         OC    ARFPBLK,ARFPBLK         IF RFP                                   
         BZ    VREC2C                                                           
         CLI   OFFLINE,C'Y'            AND ON-LINE                              
         BE    *+10                                                             
         MVC   DATEAREA(3),=C'RFP'     SET FLAG IN DATELIST                     
*                                    (NEWRIGEN WILL SKIP DATE VAL)              
                                                                                
* ALLOW DATE OR SYMBOLIC NAME FOR RFP                                           
         LA    R2,SPLRSTRH                                                      
         GOTO1 DATVAL,DMCB,SPLRSTR,WORK                                         
         OC    DMCB(4),DMCB        IS IT VALID DATE                             
         BZ    VREC2A                                                           
         NETGO NVSTRDAT,DMCB       YES/HAVE SYSTEM DO IT'S THING                
         B     VREC2B                                                           
VREC2A   BAS   RE,VALRFP           NO/TRY SYMBOLIC NAME                         
         CLC   =Y(NE#RFP1D),9(R2)                                               
         BNE   EDERR                                                            
                                                                                
*                                                                               
VREC2B   LA    R2,SPLRENDH                                                      
         GOTO1 DATVAL,DMCB,SPLREND,WORK  IS IT VALID DATE                       
         OC    DMCB(4),DMCB                                                     
         BZ    VREC2BB                                                          
         NETGO NVENDDAT,DMCB             YES/HAVE SYSTEM DO IT'S THING          
         B     VREC2D                                                           
VREC2BB  BAS   RE,VALRFP                 NO/TRY RFP                             
         CLC   =Y(NE#RFP2D),9(R2)                                               
         BE    VREC2D                                                           
         BNE   EDERR                                                            
                                                                                
* NOT RFP REQUEST                                                               
VREC2C   LA    R2,SPLRSTRH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
**********************************************************                      
         CLC   =C'@OJ',SPLCLI     IF OJ (OM AND JW)                             
         BNE   VREC2CC                                                          
         CLC   NBSELSTR,=C'970901'  MUST BE 9/1/97 OR LATER                     
         BNL   VREC2CC                                                          
         CLC   NBSELSTR(2),=C'25'      21ST CENTURY?                            
         BL    VREC2CC                                                          
         MVC   HALF,=H'707'                                                     
         B     ERR2                                                             
***************************************************************                 
VREC2CC  LA    R2,SPLRENDH         AND END                                      
         NETGO NVENDDAT,DMCB                                                    
         BRAS  RE,CHKACMN                                                       
         BAS   RE,BDLIST           BUILD DATE LISTS                             
         CLI   BHRD,0              CHK BILLHEADER READ                          
         BE    VREC2D                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VREC2D                                                           
         MVC   BHSELSTR,NBSELSTR                                                
         MVC   BHSELEND,NBSELEND                                                
         B     VREC2D                                                           
*                                                                               
* - RFP SYMBOLIC NAME VALIDATION                                                
*  R2 POINTS TO FIELD HEADER                                                    
*                                                                               
VALRFP   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    EDERR                                                            
*****    TM    4(R2),X'20'         ALREADY VALIDATED                            
*****    BO    VALRFPX                                                          
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,=X'40404040404040404040'                                
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    EDERR                                                            
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),8             SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),8            PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
VALRFPX  XIT1                                                                   
         DROP  R3                                                               
                                                                                
         EJECT                                                                  
         SPACE 1                                                                
VREC2D   LA    R2,SPLFILTH         ACCOUNTING FILTERS                           
         NETGO NVFILT,DMCB                                                      
         SPACE 1                                                                
         LA    R2,SPLALLUH         ALL UNITS?                                   
         CLI   SPLFLAV,C'E'         ..IN E FLAVOR                               
         BNE   ALLU5                                                            
         CLC   SPLFLAV+1(2),=2X'40'                                             
         BH    ALLU5                                                            
         CLI   SPLALLU,X'40'         ..SET TO Y IF BLANK                        
         BH    ALLU5                                                            
         MVI   SPLALLU,C'Y'                                                     
         MVI   SPLALLUH+5,1                                                     
         OI    SPLALLUH+6,X'80'                                                 
ALLU5    NETGO NVGETFLD,DMCB                                                    
         SPACE 1                                                                
         XC    NDPERCNT,NDPERCNT   OPTIONAL PERCENTAGE OVERRIDE                 
         LA    R2,SPLPCTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED4                                                              
         MVC   NDPERCNT,=C'COST'   WE'LL PASS THROUGHT THE WORD COST            
         CLC   FLD(4),=C'COST'     IF IT IS INPUT                               
         BE    ED2                                                              
         LR    R3,R1               (R1 IS LENGTH OF FIELD)                      
         GOTO1 CASHVAL,DMCB,(4,FLD),(R3)                                        
         CLI   DMCB,X'FF'          CHECK FOR ERROR                              
         BE    EDERR                                                            
         MVC   NDPERCNT,DMCB+4     SAVE VALUE IN NDPERCNT                       
         B     ED4                                                              
         SPACE 1                                                                
ED2      MVC   HALF,=H'644'        COST NOT ALLOWED CLI=ALL                     
         CLC   SPLCLI(3),=C'ALL'                                                
         BNE   *+12                                                             
         CLI   TWAWHEN,2           IF SOON                                      
         BE    SOONERR                                                          
         MVC   HALF,=H'645'        COST NOT ALLOWED WITH PROD=POL               
         CLC   SPLPRO(3),=C'POL'                                                
         BE    ERR2                                                             
         SPACE 1                                                                
ED4      LA    R2,SPLDEMH          DEMOS                                        
         MVI   ERROR,INVALID                                                    
         TM    NBINDS6,NBI6XDEM                                                 
         BO    ED4X                                                             
*******  MVI   NDNDEMOS,14         MAX 14 ****OPPPS? WAS 12                     
*******  NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*******  B     ED4XX                                                            
ED4X     MVI   XDNDEMOS,14         MAX 14 ****OPPPS? WAS 12                     
         NETGO NVDEM,DMCB,DBLOCK,XDDEMBLK                                       
ED4XX    CLC   LSTCLT,=C'@CHR'     ..IF CHR CROSS-AGY READ                      
         BNE   ED4D                                                             
         CLI   SPLDEMH+5,0         ..DEMOS MUST BE INPUT                        
         BE    EDERR                                                            
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         L     R3,=A(MYIO)                                                      
         AR    R3,R4                                                            
         GOTO1 SCANNER,DMCB,SPLDEMH,(4,0(R3)),0                                 
         ZIC   R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BZ    EDERR                                                            
         C     R4,=F'3'            MAX OF 3 DEMOS                               
         BH    EDERR                                                            
ED4B     CLC   =C'M2554',12(R3)    ONLY VALID CHR CROSS-AGY DEMOS               
         BE    ED4C                                                             
         CLC   =C'A2554',12(R3)                                                 
         BE    ED4C                                                             
         CLC   =C'HOMES',12(R3)                                                 
         BNE   EDERR                                                            
ED4C     LA    R3,32(R3)                                                        
         BCT   R4,ED4B                                                          
         XC    0(100,R3),0(R3)     CLEAR SCAN BLOCK                             
         SPACE 1                                                                
ED4D     LA    R2,SPLTITLH         TITLE                                        
         GOTO1 NDVALTIT                                                         
*              NOW HANDLE THE DRONE BIT                                         
         SPACE 3                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    ED5                                                              
         LA    R1,SVDRON           ONLINE BUFF                                  
         ST    R1,DRSTBUF                                                       
         A     R1,=F'1024'                                                      
         ST    R1,DRENDBUF                                                      
         B     ED6                                                              
ED5      L     R1,NDADPG           DRONE NEEDS A DPG BUFFER                     
         ST    R1,DRSTBUF                                                       
         A     R1,=F'24000'                                                     
         BCTR  R1,0                                                             
         ST    R1,DRENDBUF                                                      
         SPACE 1                                                                
ED6      MVI   NDRPTYPE,C'D'       DO THE DETAILS FIRST                         
         BAS   RE,DODRONE                                                       
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         GOTO1 =A(OVERFLW),DMCB,(RC),2,RR=R4    HICOM4                          
         MVI   NDRPTYPE,C'R'       THEN, IF REQUESTED, RECAPS                   
         CLI   NDANYREC,C'Y'    DOING RECAPS                                    
         BE    ED20             YES                                             
         OC    NDRPT2ID,NDRPT2ID CONINUE WRITER                                 
         BZ    EDEND                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   EDEND                                                            
*                                                                               
         MVC   DRSTBUF,DRCURBUF      RESET DRONE BUFFER                         
         BRAS  RE,RP2GET                                                        
         B     EDEND                                                            
*                                                                               
****     CLI   NDANYREC,C'Y'                                                    
****     BNE   EDEND                                                            
ED20     CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DRSTBUF,DRCURBUF    FOR SECOND REPORT, RESET START               
*                                  TO CURRENT DRONE POINTER                     
         XC    NDPRGFLT,NDPRGFLT   THIS CALL WILL REBUILD THE LIST              
         BAS   RE,DODRONE                                                       
         CLI   NDRCPOPT,C'Y'       IF GRAND RECAP WAS REQUESTED...              
         BNE   EDEND                                                            
*                                  GENERATE A GRAND RECAP                       
         XC    NDPRGFLT,NDPRGFLT   THIS CALL WILL REBUILD THE LIST              
         MVC   DRSTBUF,DRCURBUF                                                 
         BAS   RE,GRRECAP                                                       
         B     EDEND                                                            
RELO     DC    A(*)                                                             
         SPACE 3                                                                
GRRECAP  NTR1                                                                   
         GOTO1 NDINIDRO                                                         
         LA    R2,2                                                             
         BAS   RE,PUTLIT                                                        
         B     DODRONE4                                                         
DODRONE  NTR1                                                                   
         MVI   NDPUPREQ,0                                                       
         GOTO1 NDINIDRO                                                         
         LA    R2,1                                                             
         BAS   RE,PUTLIT                                                        
         LA    R2,SPLLEFTH         LEFT HEADERS                                 
         GOTO1 NDVALLFT                                                         
         LA    R2,SPLRGHTH         RIGHT HEADERS                                
         GOTO1 NDVALRGT                                                         
         CLI   NDANYREC,C'Y'       IF THERE IS ALSO A RECAP                     
         BNE   DODRONE2                                                         
         CLI   OFFLINE,C'Y'           AND WE ARE OFF LINE                       
         BNE   DODRONE2                                                         
         XC    WORK,WORK           NEED TO POP IN RECORD NUMBER                 
         MVI   WORK+5,6                                                         
         MVC   WORK+8(6),=C'RECORD'                                             
         LA    R1,WORK             THIS IS SO THAT THE DETAILS WILL             
         ST    R1,DRACCFLD         INTERLEAVE WITH THE RECAPS                   
         OI    DRFLAGS,DREXPDIC                                                 
         MVI   DRACTION,DRROW                                                   
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         NI    DRFLAGO,X'7F'                                                    
         MVI   DRACTION,DRGENROW                                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         SPACE 1                                                                
DODRONE2 CLI   NDRPTYPE,C'D'       FOR DETAILS,                                 
         BNE   DODRONE4                                                         
         LA    R2,SPLMIDH          MIDLINE                                      
         GOTO1 NDVALMID                                                         
         SPACE 1                                                                
DODRONE4 LA    R2,SPLDETSH         DETAILS                                      
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R2,SPLRECPH         OR RECAPS                                    
         GOTO1 ANY                 (MUST BE AT LEAST 1)                         
         GOTO1 NDVALROW            COLUMNS                                      
         LA    R2,SPLCOLSH                                                      
         GOTO1 NDVALCOL                                                         
**       TM    NDANYDEM,RAW+EQU+NN+NQ    SPECIAL DEMOS?                         
**       BZ    *+8                                                              
**       MVI   REQSML,C'L'               SET FOR LARGE SOON                     
         B     XIT                                                              
         EJECT                                                                  
*              PUT A LITERAL INTO DRONE                                         
         SPACE 3                                                                
*              INPUT               R2=CHARACTER TO BE INSERTED                  
         SPACE 1                                                                
PUTLIT   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     R1,DRCURBUF                                                      
         MVC   0(2,R1),=X'2002'           IN                                    
         LA    R1,2(R1)                                                         
         MVC   0(5,R1),=X'2205E70001'     T=X                                   
         LA    R1,5(R1)                                                         
         MVC   0(3,R1),=X'230301'         L=1                                   
         LA    R1,3(R1)                                                         
         MVC   0(4,R1),=X'87040200'       LIT=X'??'                             
         STC   R2,3(R1)                                                         
         LA    R1,4(R1)                                                         
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL THE REPORTS                                              
         SPACE 3                                                                
REPS     DS    0H                                                               
***      CLC   =C'@OJX',SPLCLI     IF SPECIAL OM/JW CROSS AGY REPORT?           
***      BNE   REPS10                                                           
***      CLC   =C'CMBO',SPLFLT     MUST HAVE PASSWORD                           
***      BNE   SECERR                                                           
***      XC    SPLFLT,SPLFLT       CLEAR PASSWORD SO IT DOESN'T                 
***      OI    SPLFLTH+6,X'80'     APPEAR ON REPORTS                            
REPS10   L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
***      LA    R2,HOOK                                                          
***      ST    R2,HEADHOOK                                                      
         DROP  R3                                                               
         TM    NDRDBCEL,X'21'      CASH RECEIPT DATA OR BILL HEADER?            
         BNZ   RP3A                                                             
*                                                                               
         CLC   =C'BH',SPLFLAV       CHK BILL HEADER READ                        
         BNE   RP3D                                                             
*                                                                               
RP3A     DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZE DDCASHIER CONTROL BLOCK                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHINIT  DS    0H                                                               
*                                                                               
         L     R6,NDACSHRC         POINT TO CASHIER CONTROL BLOCK               
         USING CSHIERD,R6          ESTABLISH AREA                               
         XC    CSHIERD(CSHIERL),CSHIERD INIT CONTROL BLOCK                      
*                                                                               
         MVI   CSHACT,CSHINIQ      SET TO INITIALIZE                            
*                                                                               
         MVC   CSHAGYCH,AGENCY     SET AGENCY ALPHA                             
         MVI   CSHSYS,CSHNETQ      SET TO PRINT SYSTEM                          
*                                                                               
         MVC   CSHBLLL,=Y(256)     MAX BILL HEADER RECORD LENGTH                
         MVI   CSHBLLKL,L'BKEY     BILL RECORD KEY LENGTH                       
*                                                                               
         MVC   CSHMAX,=F'2000'     MAX NUMBER OF BILL RECORDS                   
*                                                                               
         OI    CSHCTL,CSHCBLLQ     INDICATE BILL DATA WANTED                    
*                                                                               
         TM    NDRDBCEL,X'20'      IF CASH DATA NEEDED                          
         BNO   *+8                                                              
         OI    CSHCTL,CSHCCSHQ        INDICATE CASH DATA WANTED                 
*                                                                               
         L     RF,=A(MYIO)                                                      
         ST    RF,CSHBLLA          SET A(BILLREC)                               
*                                                                               
         MVC   CSHDMGRA,DATAMGR    DATAMGR ADDRESS                              
         MVC   CSHGTPRA,GETPROF    GETPROF ADDRESS                              
         MVC   CSHDATCA,DATCON     DATCON  ADDRESS                              
*                                                                               
         GOTO1 NBCALLOV,DMCB,0,X'D9000A7D'  DDTSAROFF                           
         MVC   CSHTSARA,DMCB       PASS V(TSAROFF)                              
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RF,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RF,CSHUTLA          PASS V(UTL)                                  
*                                                                               
         MVC   CSHCLTA,NBACLI      PASS A(CLTREC)                               
*                                                                               
         GOTO1 NBCALLOV,DMCB,0,X'D9000AA5'  DDCASHIER                           
         L     RF,DMCB                                                          
*                                                                               
         GOTO1 (RF),DMCB,CSHIERD   INIT DDCASHIER                               
         CLI   CSHERR,0            NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CSHINITX DS    0H                                                               
*                                                                               
RP3D     DS    0H                                                               
*                                                                               
         GOTO1 NDINIHED            INITIALIZE PRINT RELATED FIELDS              
         GOTO1 NDINIDRV            INITIALIZE DRIVER                            
         L     R6,NDGLOBAL                                                      
         USING GLOBALD,R6                                                       
*****                                                                           
         MVI   GLDETHED,C'Y'       PXZ                                          
*******                                                                         
         L     R2,=A(HOOK)                                                      
         ST    R2,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         SPACE 1                                                                
         BAS   RE,INITNBL          INITIALIZE NETBLOCK                          
         BAS   RE,INITDB                      DBLOCK                            
         MVC   GLRNKMAX,NDRNKMAX    SET RANK MAX                                
                                                                                
* SET UP DSECT FOR NEGENUBILL READER                                            
* GET 10000 BYTES FOR VIRTUAL EXPANDED UNIT REC                                 
         TM    NDCOLIND,X'20'      PUP READ?                                    
         BO    GTUNIT0             YES                                          
***CODE BELOW MOVED TO INITOFLN ***                                             
*                                  NO-GET STORAGE FOR VIRTUAL UNIT              
         L     R1,=F'16000'     NBAIO=10,DPTSV=6                                
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,NBAIO            USE COVAIL ADDR FOR NBAIO                    
         A     RE,=F'10000'        BUMP AND SET FOR DPT AREA                    
         ST    RE,NBADPTSV                                                      
******   A     RE,=F'6000'         NBWBFLT MUST BE LOADED                       
******   ST    RE,NBWBFLT          EARLIER-NEEDED IN VALCLI IN NETIO            
*                                                                               
***      XC    NBA56K,NBA56K                                                    
***      CLI   TWAWHEN,2           IF SOON                                      
***      BE    ENDFTRD             NO FULL TRACK READ                           
***      L     R1,=F'57000'     FOR FULL TRACK READ                             
***      ST    R1,DMCB+4                                                        
***      ST    R1,DMCB+8                                                        
***      GOTO1 =V(COVAIL),DMCB,C'GET'                                           
***      ICM   RE,15,4(R1)                                                      
***      BZ    *+8                                                              
***      ST    RE,NBA56K           FOR FULL TRACK READ                          
ENDFTRD  DS    0H                                                               
*                                                                               
* DO WE HAVE TO READ NET WORK BILLING RECORDS ?                                 
         CLC   NBSELAGY,=C'DR'     IF SMGTEST                                   
         BE    ENDPRF              SKIP PROF READ                               
         CLC   NBSELAGY,=C'*B'     IF DDSB                                      
         BE    ENDPRF              SKIP PROF READ                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB1S'                                                 
         NI    WORK,X'BF'           LOWER CASE                                  
         MVC   WORK+4(2),NBSELAGY                                               
         GOTO1 NBGTPROF,DMCB,(X'C0',WORK),WORK+20,NBDM                          
         CLI   WORK+23,C'Y'                ONLY WRITING NEW BILLING ?           
         BE    *+8                         YES-LET NEWRIGEN HANDLE FLAG         
         NI    NBINDS2,X'FF'-NBBILLRD      NO-TURN OFF FLAG                     
ENDPRF   DS    0H                                                               
*                                                                               
         TM    NBINDS2,NBBILLRD   READING NEW BILLING RECORDS?                  
         BNO   ENDNBRD            NO                                            
         L     R1,=A(NEWBLRD)     NETBILLRD DSECT                               
         USING NBLBILLD,R1                                                      
         ST    R1,NBABILRD                                                      
         XC    0(NBLLENQ,R1),0(R1)                                              
         MVC   NBLUNAIO,NBAIO     NBAIO                                         
         OI    NBLFUNC,NBLSEED    SEED UNIT REC WITH ALL BILL ELEMS             
         OI    NBLFUNC2,NBLWBFLT  RETURN X'10' WITH FLTID                       
ENDNBRD  DS    0H                                                               
*                                                                               
         EJECT                                                                  
*              CONTROL READING OF UNITS                                         
                                                                                
****************************************************************                
GETUNIT  CLC   =C'PROGDO',CONREC     PROGRAM  DOWN REQUEST?                     
         BNE   PROGUPX                                                          
         OI    NBSELPRN,X'02'   SET FIRST FLAG                                  
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         MVI   NBSELPRN,0       CLEAR FLAG                                      
         XC    FILENAME,FILENAME                                                
         NETGO NVSETSPT,DMCB         SET FOR SPOT READ                          
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NPGKEY,R2                                                        
         MVC   0(2,R2),=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,BINMKT                                                   
         CLI   NBSELPRG,X'40'        PROGRAM FILTER?                            
         BNH   *+10                                                             
         MVC   NPGKPROG,NBSELPRG       YES                                      
PROGUP14 GOTO1 HIGH                                                             
         B     PROGUP17                                                         
PROGUP15 GOTO1 SEQ                                                              
*                                                                               
PROGUP17 CLC   KEY(3),KEYSAVE          AGY/MED?                                 
         BNE   PROGUPXX                                                         
         CLC   NPGKNET,BINMKT          STATION FILTER                           
         BNE   PROGUP15                                                         
         CLI   NBSELPRG,X'40'          PROGRAM FILTER?                          
         BNH   PROGUP20                                                         
         CLC   NPGKPROG,NBSELPRG                                                
         BNE   PROGUP15                                                         
PROGUP20 DS    0H                       END DATE FILTER?                        
         CLC   NPGKEND,NBCMPEND                                                 
         BH    PROGUP15                                                         
         CLC   NPGKEND,NBCMPSTR                                                 
         BL    PROGUP15                                                         
PROGUP30 MVC   NBKEY,KEY          PASS KEY IN NETBLOCK TO DRIVER                
*                                                                               
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         GOTO1 HIGH                    RESET KEY                                
         B     PROGUP15                                                         
PROGUPXX DS    0H                                                               
         J     GL12                GO TO COMMON GLOUTPUT                        
****     MVI   GLMODE,GLOUTPUT                                                  
****     GOTO1 NDDRIVER,DMCB,(R6)                                               
****     B     XIT                                                              
PROGUPX  EQU   *                                                                
*********************************************************                       
         EJECT                                                                  
*                                                                               
*                                                                               
         TM    NDRDBCEL,X'10'        READING EST FOR AUTH$ ?                    
         BNO   GT00                                                             
         NI    NDRDBCEL,X'FF'-X'10'            YES/CLEAR FLAG                   
         BAS   R4,SVNTBLK                      SAVE NETBLOCK                    
         GOTO1 =A(OVERFLW),DMCB,(RC),0         READ EST HEADERS                 
         BAS   R4,RSTNTBLK                     RESTORE NETBLOCK                 
         B     GT00                                                             
                                                                                
*                                                                               
SVNTBLK  L     RF,=A(NETBLKSV)    ... SAVE CURRENT NETBLOCK                     
         LA    RE,NETBLOCK                                                      
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE  ((RF),(R1)),(RE)                                                 
         BR    R4                                                               
                                                                                
RSTNTBLK LA    RF,NETBLOCK                     RESTORE NETBLOCK                 
         L     RE,=A(NETBLKSV)                                                  
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NBFUNCT,NBFRDHI                 READ HI TO RESTORE SEQ           
         BR    R4                                                               
                                                                                
************************************************************                    
* CHECK IF INVOICE READ                                                         
GT00     TM    NBVARIND,X'20'        INVOICE RECORDS READ?                      
         BZ    GT001                                                            
         BAS   R4,SVNTBLK             SAVE NETBLOCK                             
         BAS   RE,INVRDRTN                                                      
         BAS   R4,RSTNTBLK            RESTORE NETBLOCK                          
         NI    NBVARIND,X'FF'-X'20'   TURN OFF INVOICE FLAG                     
                                                                                
                                                                                
*************************************************************                   
* CHECK IF READING BILLING HEADER RECORDS                                       
GT001    CLI   BHRD,0             ...CHK BILL HEADER READ                       
         BE    GTU00                                                            
         TM    NDRDBCEL,X'20'      SKIP IF CASH DATA NEEDED                     
         BO    *+8                                                              
         NI    NDRDBCEL,X'FF'-X'01' . TURN OFF UNTBILELEM                       
*****    MVI   NDRDBCEL,0         ... TURN OFF UNTBILELEM / CHQ READ            
         BAS   R4,SVNTBLK          YES/SAVE NETBLOCK                            
         L     R3,=A(MYIO)                                                      
         GOTO1 ABHRD,DMCB,(RC),(R5),(R3),0,NDACSHRC READ BILL RECORDS           
         L     R1,=A(HOOK)          RESET HOOK SINCE ABHRD CHANGES IT           
         ST    R1,GLAHOOK                                                       
         OI    NDCOLIND,X'40'              SURPRESS TOTSTACK                    
         CLI   BHRD,1                  .ALSO READ UNITS                         
         BE    GT002                                                            
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,0                                                       
         B     UNITEND                 .NO                                      
GT002    BAS   RE,SETUNRD              .SET TO READ UNITS                       
*                                    ...AND NOW GO READ UNITS                   
                                                                                
                                                                                
*******************************************************************             
* CHECK FOR PRIOR AND AFTER OPTION SET                                          
GTU00    CLI   NDPRIOR,0           CHECK FOR PRIOR OPTION                       
         BE    RANGCK20                                                         
         XC    NBCMPSTR,NBCMPSTR                                                
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,NDPRIOR)                             
RANGCK20 TM    NDAFTER,0           CHECK FOR AFTER OPTION                       
         BZ    RANGCK30                                                         
         MVC   NBCMPEND(2),=XL2'FFFF'                                           
         GOTO1 DATCON,DMCB,(0,NBSELEND),(2,NDAFTER)                             
*                                                                               
RANGCK30 MVC   NBREROPT,NDREROPT   PASS RERATE OPTION                           
         MVC   NBAFFOPT,NDAFFOPT        AND AFFID OPTION                        
         CLI   NDRDBCEL,0            ...IF READING BILL/CHQ ELEMS               
         BE    *+8                                                              
         OI    NBSPLOPT,X'C0'           ...SPLIT UNITS                          
         TM    NBACCFLT,X'04'                                                   
         BZ    *+8                                                              
         MVI   NBRESUME,NBPROCPK   START AT PKG                                 
*                                                                               
****->                        NBSPLOPT=X'20' FINDS BILLED PRODS THAT            
****->                        ARE NOT SCHEDULED PROD - IN CASES WHERE           
****->                        PROD WAS CHANGED AFTER BILLING                    
****->                        BUT WITH NEW BLDPRD KEYWORD                       
****->                        TURNING THIS ON DOUBLES $$$ FOR BILLED            
****->                        PRODS THAT ARE NOT SCHEDULED IN                   
****->                        BILLED PRODUCT MODE (BLDPRD KEYWORD)              
********** THE NEW WRITER NOW HANDLES BOTH *********************                
********** KEPT THIS OLD CODE FOR HISTORICAL PURPOSES***********                
*******  TM    NBINDS3,NBI3BPRD      BILLED PROD MODE?                          
*******  BNO   *+8                   NO                                         
*******  NI    NBSPLOPT,X'FF'-X'20'  YES/TURN OFF (IN NETVALUE)                 
*                                                                               
         CLC   =C'POL',NBSELPRD         IF POL,NEVER BLDPRD                     
         BNE   *+8                                                              
         NI    NBINDS3,X'FF'-NBI3BPRD   SINCE RLP MIGHT HAVE IT ON              
*                                                                               
         TM    NBINDS3,NBI3CMRT        COMMERCIAL ROTATION MODE?                
         BNO   *+8                                                              
         MVI   NBSEQ,C'Q'              USE X'84' KEY IN NETIO READ              
*                                      READING UNITS IN PROGRAM ORDER           
*                                                                               
         CLI   NBDODEMS,C'N'            ARE DEMOS REQUIRED                      
         BNE   GTUNIT0                                                          
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,0                                                       
                                                                                
GTUNIT0  TM    NDCOLIND,X'20'      PUP REPORT ?                                 
         BO    GETUNIT1            YES                                          
         OI    NBVARIND,X'40'      NO-NAD DEMOS USE NDDEMOS                     
         B     GETUNIT1                                                         
*                                                                               
GETUNIT1 DS    0H                                                               
         TM    NDINDS1,X'02'       ACCMON KEYWORD?                              
         BNO   NOTACMON                                                         
         MVC   NBCMPSTR,=2X'00'                                                 
         MVC   NBCMPEND,=2X'FF'                                                 
NOTACMON EQU   *                                                                
         TM    NBINDS2,NBNOEACT    DON'T DO EST AS ACT DEMOS                    
         BNO   *+10                                                             
         MVC   NBUSER1+6(3),=C'NNN'                                             
**                                                                              
         OI    NBINDS5,NBI5XRD     SPECIAL READ IN BILLRDR                      
*                                                                               
                                                                                
         TM    NDANYDEM,NN+NQ      IF ASKING FOR THIS                           
         BNZ   SKPTHIS             SKIP NEW CODE FOR NOW                        
         TM    NDANYDEM,RAW+EQU    IF ASKING FOR RAW/EQU                        
         BZ    SKPTHIS                                                          
         MVI   NBN0B2,0            LET'S TRY FOR SOME EFFICIENCY                
         MVI   NBN2B1,0                                                         
SKPTHIS  EQU   *                                                                
*                                                                               
         CLI   NDVTYPTB,0          MULTIPLE VTYPE DEMO COLUMNS?                 
         BE    ENDVTYP             NO                                           
         OI    NBVTYPS3,X'01'      SET VTYP OVERRIDE FLAG                       
**************************************************************                  
         MVC   NBVTYPS(2),NDVTYPTB PASS 2 BYTE CODE                             
         B     VTYP12A                                                          
**************************************************************                  
***      LA    R1,NDVTYPTB         REQUESTED VTYPES                             
***      LA    RE,VTYPEQUS                                                      
***      LA    RF,NBVTYPS          PROGRAM AVERAGE FLAG                         
***      LA    R4,6                                                             
VTYP10   CLC   0(2,R1),0(RE)                                                    
***      BE    VTYP12                                                           
***      LA    RE,3(RE)                                                         
***      CLI   0(RE),0                                                          
***      BNE   *+6                                                              
***      DC    H'0'                                                             
***      BCT   R4,VTYP10                                                        
***      LA    R4,6                                                             
***      LA    RE,VTYPEQU2                                                      
***      LA    RF,NBVTYPS2         COMMERCIAL AVERAGE FLAG                      
***      B     VTYP10                                                           
                                                                                
*SET NBVTYPS                                                                    
**VTYPEQUS DC    C'PL',X'00',C'PS',X'01',C'P7',X'07'                            
**         DC    C'P1',X'F1',C'P2',X'02',C'P3',X'03'                            
**VTYPEQU2 DC    C'CL',X'FF',C'CS',X'01',C'C7',X'07'                            
**         DC    C'C1',X'F1',C'C2',X'02',C'C3',X'03'                            
**         DC    X'00'                                                          
*                                                                               
**VTYP12   OC    0(1,RF),2(RE)                                                  
VTYP12A  MVI   VTYPCNT,1           SET TO 1ST PASS                              
ENDVTYP  EQU   *                                                                
*                                                                               
*                                                                               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         SPACE 1                                                                
GETUNIT2 CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    NDCOLIND,X'20'      PUP REPORT                                   
         BO    UNITEND                                                          
                                                                                
         CLI   NBMODE,NBPROCPK     FOR PACKAGE                                  
         BNE   GETUNIT3                                                         
         BAS   RE,FUDGUNT          FUDGE UNIT DATA IN NETBLOCK                  
         TM    NBACCFLT,X'08'      IF DAPA REQUEST                              
         BO    GETUNIT1            DON'T SEND PKG TO DRIVER                     
         BAS   RE,DOIT                                                          
         B     GETUNIT1                                                         
*                                                                               
GETUNIT3 CLI   NBMODE,NBPROCUN                                                  
         BE    GOTUNIT                                                          
         CLI   NBMODE,NBVALCLI                                                  
         BE    FRSTCLI                                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    UNITEND                                                          
         B     GETUNIT1                                                         
         SPACE 1                                                                
FRSTCLI  XC    CIDTBL,CIDTBL                                                    
         CLI   SPLFLAV,C'E'                                                     
         BNE   GETUNIT1                                                         
         MVI   NBUSER+13,C'N'      RESET PROFILE FOR E FLAVOR                   
         B     GETUNIT1                                                         
         SPACE 1                                                                
*                                                                               
GOTUNIT  CLI   SPLFLAV+2,C'G'      GOALS ONLY                                   
         BE    UNITEND                                                          
*                                                                               
         TM    NDINDS1,ND1STAB  READ STATION BUCKETS FOR MANUAL BILLS?          
         BNO   NOSTAB                                                           
         BAS   R4,SVNTBLK                                                       
         BRAS  RE,STABRD                                                        
         BAS   R4,RSTNTBLK                                                      
NOSTAB   DS    0H                                                               
*                                                                               
         BRAS  RE,TMTLTBL          FILL TABLE                                   
*                                                                               
         BAS   RE,FILTUNIT         DO WE NEED THIS ONE?                         
         BNE   GETUNIT1                                                         
***************************************************************                 
         TM    NBINDS3,NBI3CMRT        COMMERCIAL ROTATION MODE?                
         BNO   NOTCOMDE                                                         
         BRAS  RE,DOCOMRTN             CHK PROGRAM REC FOR CMRTN MODE           
         NI    NBINDS3,X'FF'-NBI3CMPR  TURN OFF PROG FLAG                       
         OC    NBAFFTIM,NBAFFTIM       AFFID TIME AVAILABLE?                    
         BNZ   ROT01                                                            
         L     R1,=A(PRGHOURS)         NO AFFID TIME                            
         MVC   NBBILTGR,0(R1)          PASS HOURS FOR FUDGE                     
         MVI   NBBILTNT,X'FF'          PASS UNMATCHED FLAG                      
         B     NOTCOMDE                                                         
*                                                                               
ROT01    ZICM  R1,NBAFFTIM,2                                                    
         CHI   R1,600                  LESS THAN 600                            
         BNL   *+8                                                              
         AHI   R1,2400                                                          
         STCM  R1,3,HALF                                                        
         L     R1,=A(PRGHOURS)                                                  
         CLC   HALF,0(R1)              LESS THAN START TIME                     
         BL    OUTROTN                 OUT OF ROTATION                          
         LA    R1,2(R1)                POINT TO END 1/2 HOUR                    
         B     *+8                                                              
ROT10    LA    R1,4(R1)                BUMP TO NXT END 1/2 HR                   
         CLI   0(R1),X'FF'                                                      
         BE    OUTROTN                                                          
         CLC   HALF,0(R1)              LESS/= END TIME                          
         BH    ROT10                                                            
         S     R1,=F'2'                BUMP BACK TO START                       
         MVC   NBBILTGR,0(R1)                                                   
         B     NOTCOMDE                                                         
*                                                                               
OUTROTN  SR    R0,R0                OUT OF ROTATION                             
         ZICM  R1,HALF,2            ADJUSTED AFFID TIME                         
         D     R0,=F'100'                                                       
         C     R0,=F'30'               IF REMAINDER 30<                         
         BH    OUTROT10                                                         
         MHI   R1,100                                                           
         STCM  R1,3,NBBILTGR           USE HOUR FOR START                       
         AHI   R1,30                   BUMP TO NXT 1/2 HOUR                     
         STCM  R1,3,NBBILTGR+2         USE NXT FOR END                          
         MVI   NBBILTNT,C'*'           OUTOFROT FLAG                            
         B     NOTCOMDE                                                         
OUTROT10 MHI   R1,100                  REMAINDER >30                            
         AHI   R1,30                   BUMP TO NXT HOUR                         
         STCM  R1,3,NBBILTGR                                                    
         AHI   R1,70                                                            
         STCM  R1,3,NBBILTGR+2                                                  
         MVI   NBBILTNT,C'*'                                                    
         B     NOTCOMDE                                                         
                                                                                
**************************************************************                  
NOTCOMDE EQU   *                                                                
*********************************************************PXZ                    
         BRAS  RE,RDB1XPRF                                                      
*********************************************************PXZ                    
         GOTO1 =A(OVERFLW),DMCB,(RC),11,RR=YES   GET PROD COST                  
*                                                                               
* - IF WILA AGY SUMMARY REQUEST, BUILD DATELIST ON FLY                          
         CLI   NDPRIOR,0                                                        
         BNE   DATFLY                                                           
         CLI   NDAFTER,0                                                        
         BE    GOTUNT0                                                          
DATFLY   TM    NBSUBMSK,NBSBMNET   ..IF NETWORK CHANGE                          
         BNO   GOTUNT0                                                          
         XC    KEY,KEY             ..GET USER PROFILE INTO NBUSER               
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
*        MVI   KEY+6,C'N'                                                       
         MVC   KEY+6(1),NBPOSTYP    SPECIFIC PROFILE FOR N,C                    
         MVC   KEY+7(3),NBCLICOD                                                
         GOTO1 NBGTPROF,DMCB,KEY,NBUSER,NBDM                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         BAS   RE,BDXTRA           BUILD DATE LIST ON FLY                       
         SPACE                                                                  
GOTUNT0  DS    0H                                                               
         GOTO1 =A(OVERFLO),DMCB,(RC)      GET DEMOS INTO EXTENSION              
                                                                                
         BAS   RE,DOIT                                                          
         B     GETUNIT1                                                         
         EJECT                                                                  
*              ROUTINE TO EXPLODE FOR SPLIT BILLING                             
         SPACE 3                                                                
DOIT     NTR1                                                                   
         CLI   NBMODE,NBPROCPK                                                  
         BE    DOIT00                                                           
         CLI   NDSPLOPT,0                                                       
         BNE   DOIT2                                                            
                                                                                
* GET CURRENT 3 CHARACTER PROD CODE->NDCURPRD                                   
* IF NOT SPLITTING PRODUCTS, USE NBPRD (1ST PRODUCT) AS DEFAULT                 
         CLI   NBMODE,NBPROCUN     ONLY IF PROCESSING UNIT                      
         BNE   CURPRDX                                                          
         XC    NDCURPRD,NDCURPRD   CLEAR IT                                     
         B     CURPRDUN            SHOULD NOT BE USED ANYMORE                   
***      MVC   BYTE,NBPRD          SET 1ST PROD AS DEFAULT                      
***      TM    NBSPLOPT,X'C0'      IS SPLIT OPTION ON?                          
***      BZ    CURPRD5              NO                                          
***      CLI   NBPRDNO,0           MULTI PRODS?                                 
***      BNE   CURPRD              YES                                          
***      CLI   NBSPLPRN,0          SPLITTING PRODS?                             
***      BE    CURPRD5             NO                                           
***      MVC   BYTE,NBSPLPRN       YES                                          
***      B     CURPRD5                                                          
                                                                                
* HANDLE MULTI PRODS HERE                                                       
CURPRD   ZIC   R1,NBSPLCNT         CURRENT PROD POSITION                        
***      LA    RE,NBPRDLST         LIST OF MULTI PRODS                          
***      BCTR  R1,0                NBSPLCNT=1 IF 1ST PROD                       
***      AR    RE,R1                                                            
***      MVC   BYTE,0(RE)          SET 1 BYTE PROD CODE TO BYTE                 
***      MVI   HALF,0                                                           
CURPRD5  L     RE,NBACLI                                                        
***      USING CKEY,RE                                                          
***      LA    RE,CLIST                                                         
***      LA    RF,220                                                           
CURPRD10 CLC   BYTE,3(RE)                                                       
***      BE    CURPRD15                                                         
***      LA    RE,4(RE)                                                         
***      BCT   RF,CURPRD10                                                      
***      CLI   HALF,2                                                           
***      BE    CURPRDUN                                                         
***      LA    RF,35                                                            
***      LA    RE,CLIST2                                                        
***      B     CURPRD10                                                         
***CURPRDUN MVC   NDCURPRD,=C'***'    UNALLOCATED                               
CURPRDUN MVC   NDCURPRD,=C'PXZ'       MY INITITALS TO IDENTIFY                  
***      B     *+10                                                             
***CURPRD15 MVC   NDCURPRD,0(RE)   SET CURRENT 3 CHAR PROD CODE                 
CURPRDX  EQU   *                                                                
*                                                                               
*                                                                               
         TM    NBPPDOPT,NBPPDONQ   SKIP IF NOT PAYPEND OPT                      
         BNO   PPDOPTX                                                          
*                                                                               
         NI    NBPPDOPT,NBPPDONQ   CLEAR SWITCHES                               
         OI    NBPPDOPT,NBPPDCKQ      SET TO CHECK FOR CLEARED BUY              
*                                     BUT NOT PAID                              
         BAS   RE,CHECKRD          CHECK FOR CLEARED /NOT PAID                  
*                                                                               
         NI    NBPPDOPT,X'0FF'-NBPPDCKQ TURN OFF INDICATOR                      
*                                                                               
         TM    NBPPDOPT,NBPPDINQ      KEEP BUY IF TO BE INCLUDED                
         BO    PPDOPTX                                                          
*                                                                               
         NI    NBPPDOPT,NBPPDONQ   CLEAR SWITCHES                               
         MVI   NBRDBELS,0          CLEAR INDICATOR                              
*                                                                               
         B     DOITXIT                                                          
*                                                                               
PPDOPTX  DS    0H                                                               
*                                                                               
         BAS   RE,UNITBEL          CHECK IF SENDING UNIT BILL ELEMS             
         BAS   RE,CHECKRD          CHECK IF SENDING CHECK NUMBERS               
*                                                                               
         CLI   NBRDBELS,0        DID WE PASS MULTIPLE RECS TO DRIVER            
         BE    DOIT00                                                           
         MVI   NBRDBELS,0        YES RESET FLAG                                 
         B     DOITXIT           EXIT                                           
DOIT00   MVC   AIO2,AIO                                                         
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
*                                                                               
         TM    NBINDS2,NB$TYPSP    USING $TYPE KEYWORD?                         
         BNO   *+12                                                             
         BAS   RE,SEND$TYP         SEND MULTIPLE $TYPS                          
         B     DOIT01                                                           
         TM    NBINDS3,NBI3BPRD    BILLING PRODUCT MODE?                        
         BNO   *+8                                                              
         BAS   RE,SEND$TYP                                                      
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
DOIT01   MVC   AIO,AIO2                                                         
         B     DOITXIT                                                          
         SPACE 1                                                                
DOIT2    L     R4,=A(SPLBILL)                                                   
         ST    R4,NDASPLBL                                                      
         USING SPLTBLKD,R4                                                      
         LA    R3,SPLPLIST                                                      
         CLI   0(R3),0             DO WE HAVE PRODUCTS YET                      
         BNE   DOIT4                                                            
         SPACE 1                                                                
DOIT3    LA    R1,DOITHOOK                                                      
         ST    R1,SPLAHOOK                                                      
         LA    R1,NETBLOCK                                                      
         ST    R1,SPLANETB                                                      
         MVC   SPLPRD,NBSELPRD                                                  
         CLC   SPLPRD,=C'ALL'                                                   
         BNE   *+10                                                             
         MVC   SPLPRD,=C'POL'                                                   
         CLI   NDSPLPRD,0          OPTION TO NOMINATE SPLIT PRODUCT             
         BE    *+10                                                             
         MVC   SPLPRD,NDSPLPRD                                                  
         MVI   SPLPRDO,C'Y'        NEED THE PRODUCT LIST                        
         GOTO1 =V(NETSPB),DMCB,(R4)                                             
         LA    R3,SPLPLIST                                                      
         SPACE 1                                                                
DOIT4    CLI   SPLIO,0             IS THE RECORD STILL THERE?                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),0             ANY MORE PRODUCTS?                           
         BE    DOITXIT                                                          
         MVC   SPLIPRD,0(R3)       PICK ONE PRODUCT FROM LIST                   
         MVC   NDCURPRD,0(R3)      AND FILL IN NDCURPRD                         
*                                                                               
         TM    NBPPDOPT,NBPPDONQ   SKIP IF NOT PAYPEND OPT                      
         BNO   DITOPTX                                                          
*                                                                               
         NI    NBPPDOPT,X'0FF'-NBPPDONQ  CLEAR SWITCHES                         
         OI    NBPPDOPT,NBPPDCKQ      SET TO CHECK FOR CLEARED BUY              
*                                     BUT NOT PAID                              
         BAS   RE,CHECKRD          CHECK CLEARED/NOT PAID                       
*                                                                               
         TM    NBPPDOPT,NBPPDINQ      KEEP BUY IF TO BE INCLUDED                
         BO    DITOPTX                                                          
*                                                                               
         NI    NBPPDOPT,X'0FF'-NBPPDONQ  CLEAR SWITCHES                         
         MVI   NBRDBELS,0          CLEAR INDICATOR                              
         B     DOITXIT                                                          
*                                                                               
DITOPTX  DS    0H                                                               
*                                                                               
         BAS   RE,UNITBEL          CHECK IF SENDING UNIT BILL ELEMS             
         BAS   RE,CHECKRD          CHECK IF SENDING CHECK NUMBERS               
*                                                                               
         CLI   NBRDBELS,0        DID WE PASS MULTIPLE RECS TO DRIVER            
         BE    DOIT7                                                            
         MVI   NBRDBELS,0        YES RESET FLAG                                 
         B     DOITXIT           EXIT                                           
*                                                                               
DOIT7    MVC   AIO2,AIO                                                         
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         MVC   AIO,AIO2                                                         
         LA    R3,3(R3)                                                         
         B     DOIT4                                                            
         SPACE 1                                                                
DOITXIT  DS    0H                                                               
         B     XIT                                                              
         SPACE 1                                                                
DOITHOOK BR    RE                                                               
*                                                                               
                                                                                
         EJECT                                                                  
* SEND ALL SPECIAL CHARGES 1ST TIME + ACT,ASGND,INTEG FOR PROD                  
* ON FURTHER CALLS JUST SEND ACTUAL,ASSIGNED AND INTEG DATA                     
SEND$TYP NTR1                                                                   
         TM    NBINDS3,NBI3BPRD    BILLING PRODUCT MODE?                        
         BO    SENDBP00            YES/GO THERE                                 
*                                  NO-SPECIAL $TYPE ROUTINE                     
         CLC   $TYPKYSV,NBKEY      HAVE WE SENT SPECIAL CHARGES?                
         BE    SENDTP20            YES                                          
         MVC   $TYPKYSV,NBKEY      NO/SAVE UNIT KEY AS INDICATOR                
         L     R4,NBAIO                                                         
         MVI   ELCODE,3            SPECIAL CHARGES ELEMENT                      
         USING NUSPRD,R4                                                        
         BAS   RE,GETEL                                                         
         BNE   SENDTP20                                                         
SENDTP03 LA    R2,SPCHRLST                                                      
*                                                                               
SENDTP05 CLC   NUSPRTYP,0(R2)                                                   
         BE    SENDTP10                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   SENDTP05                                                         
         DC    H'0'                 NEW SPECIAL CHARGE NOT IN LIST?             
SENDTP10 MVC   NB$AMT,NUSPRAMT                                                  
         MVC   NB$SPC,1(R2)                                                     
         BAS   R3,DRIVEIT                                                       
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           ANY MORE SPECIAL CHARGES?                    
         BE    SENDTP03                                                         
*                                                                               
SENDTP20 DS    0H                                                               
         OC    NBACTUAL,NBACTUAL                                                
         BNZ   SENDTP25                                                         
         TM    NBUNITST,X'20'      ACTUAL COST INPUT?                           
         BZ    SENDTP30            NO                                           
SENDTP25 MVC   NB$AMT,NBACTUAL                                                  
         MVC   NB$SPC,=C'ACT'      YES                                          
         BAS   R3,DRIVEIT                                                       
SENDTP30 DS    0H                                                               
         OC    NBINTEG,NBINTEG                                                  
         BZ    SENDTP35                                                         
         MVC   NB$SPC,=C'INT'                                                   
         MVC   NB$AMT,NBINTEG                                                   
         BAS   R3,DRIVEIT                                                       
SENDTP35 OC    NBASSIGN,NBASSIGN                                                
         BZ    SENDTPX                                                          
         MVC   NB$AMT,NBASSIGN                                                  
         MVC   NB$SPC,=C'ASN'                                                   
         BAS   R3,DRIVEIT                                                       
SENDTPX  B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* HANDLE BILLING PRODUCT ON SPECIAL CHARGES HERE                                
* IF BILLING PRDUCT DOES NOT MATCH ANY SCHEDULED PRODUCT                        
* PASS IT TO DRIVER HERE                                                        
SENDBP00 EQU   *                                                                
         CLC   $TYPKYSV,NBKEY      HAVE WE SENT SPECIAL CHARGES?                
         BE    SENDBPX             YES-EXIT                                     
*****************************************************************               
INTPRD   DS    0H                  DO WE HAVE AN INTEGRATION PROD?              
         L     RE,NBAIO                                                         
         LA    RE,27(RE)                                                        
         CLI   0(RE),X'01'         SHOULD BE HERE                               
         BE    *+10                                                             
         DC    H'0'                                                             
INTP10   CLI   0(RE),X'60'                                                      
         BH    ENDINTP                                                          
         BE    INTP12                                                           
INTP11   ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     INTP10                                                           
INTP12   CLI   2(RE),C'N'                                                       
         BNE   INTP11                                                           
         MVC   INTPRD3,3(RE)      SAVE 3 CHAR INT PROD                          
ENDINTP  DS    0H                                                               
****************************************************************                
         XC    SENTPLST,SENTPLST   CLEAR LIST                                   
         MVC   $TYPKYSV,NBKEY      NO/SAVE UNIT KEY AS INDICATOR                
         L     R4,NBAIO                                                         
         MVI   ELCODE,3            SPECIAL CHARGES ELEMENT                      
         USING NUSPRD,R4                                                        
         BAS   RE,GETEL                                                         
         BNE   SENDBPX                                                          
         B     SENDBP11                                                         
*                                                                               
SENDBP10 MVI   ELCODE,3            SPECIAL CHARGES ELEMENT                      
         BAS   RE,NEXTEL                                                        
         BNE   SENDBPX                                                          
SENDBP11 CLI   NUSPRLEN,NUSPRLN1   OLD STYLE ELEMENT?                           
         BE    SENDBP10            YES-SKIP/NO BILLING PRODUCT                  
*                                                                               
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4   NEW 3 CHAR PROD LEN ?                        
         BNE   SENDBP13            NO- USE 1 CHAR BILL PROD                     
**                                                                              
         CLI   NUSPRBPC,0          IS THERE BILLED PROD?                        
         BE    SENDBP10                                                         
         CLC   NUSPRBPC,NBPR1CL3   YES- NEW 3 CHAR PROD LEN                     
         BE    SENDBP10            SKIP                                         
         CLC   NUSPRBPC,NBPR2CL3                                                
         BE    SENDBP10            SKIP                                         
         CLC   NUSPRBPC,INTPRD3                                                 
         BE    SENDBP10            SKIP                                         
         B     SENDBP14                                                         
                                                                                
*                                  1 CHAR BILL PROD                             
SENDBP13 CLI   NUSPRBPR,0          BILLING PRODUCT NOT SET?                     
         BE    SENDBP10            YES/SKIP                                     
         TM    NBINDS6,NBI6X1OV    NBPRD A FUDGE?                               
         BO    *+14                YES                                          
         CLC   NUSPRBPR,NBPRD      NO - MATCH?                                  
         BE    SENDBP10            SKIP                                         
         TM    NBINDS6,NBI6X2OV    NBPRD2 A FUDGE ?                             
         BO    *+14                                                             
         CLC   NUSPRBPR,NBPRD2     MATCH?                                       
         BE    SENDBP10            SKIP                                         
*                                                                               
         MVC   WORK(1),NUSPRBPR    PASS 1 CHAR CODE                             
         BRAS  RE,GETPRD3                                                       
         CLC   WORK(3),INTPRD3     CHECK AGIANST INT PROD                       
         BE    SENDBP10            SKIP                                         
         B     SENDBP14                                                         
*                                                                               
*                                                                               
SENDBP14 CLI   NBPRDNO,0           MULTI PRODS?                                 
         BE    SENDBP20            NO                                           
*                                                                               
* - SEE IF PRODUCT IN PRODLIST OF MULT PROD UNIT                                
         ZIC   R1,NBPRDNO          NUMBER OF PRDS IN LIST                       
*                                                                               
         XC    WORK,WORK                                                        
         CLI   NUSPRLEN,NUSPRLN4                                                
         BNE   *+10                                                             
         MVC   WORK(3),NUSPRBPC                                                 
         CLI   WORK,0              DO WE HAVE 3 CHAR CODE ?                     
         BNE   SENDP14A            YES - USE THAT                               
         MVC   WORK(1),NUSPRBPR    PASS 1 CHAR CODE                             
         BRAS  RE,GETPRD3                                                       
         CLI   WORK,C'*'                                                        
         BNE   *+6                                                              
         DC    H'0'                ???                                          
         B     SENDP14A                                                         
                                                                                
*****************************************************                           
*****************************************************                           
*                                                                               
SENDP14A DS    0H                                                               
         ICM   RE,15,NBADPLST      YES- MULTI PROD 3 CHAR LIST                  
         BNZ   *+6                                                              
         DC    H'0'                ???                                          
SENDP14B CLC   WORK(3),0(RE)                                                    
         BE    SENDBP10            IT'S A MATCH-SKIP                            
         LA    RE,3(RE)            BUMP PROD LIST                               
         BCT   R1,SENDP14B                                                      
         B     SENDBP20                                                         
                                                                                
*********************************  DEAL WITH 1 BYTE PROD CODES                  
**SENDP14D LA    R2,NBPRDLST                                                    
**SENDBP15 CLC   NUSPRBPR,0(R2)      MULTI PROD MATCH?                          
**         BE    SENDBP10            IT'S A MATCH - SKIP                        
**         LA    R2,1(R2)                                                       
**         BCT   R1,SENDBP15                                                    
********************************                                                
                                                                                
                                                                                
                                                                                
* -> BILLING PRODUCT ON SPECIAL CHARGE ELEMENT DOES NOT MATCH                   
* -> ANY SCHEDULED PRODUCT - DEAL WITH IT HERE                                  
                                                                                
SENDBP20 LA    R1,SENTPLST         ALREADY PASSED THIS PROD?                    
                                                                                
SENDBP21 CLI   1(R1),0                                                          
         BE    SENDBP22             NO                                          
*                                                                               
         CLI   NUSPRBPR,0          IF 1 CHAR                                    
         BE    *+18                NO-USE 3 CHAR                                
         CLC   NUSPRBPR,0(R1)      YES-USE THAT                                 
         BE    SENDBP10            GET NEXT ELEM                                
         BNE   SENDB21A                                                         
         CLC   NUSPRBPC,1(R1)       3 CHAR                                      
         BE    SENDBP10             YES/GET NEXT ELEM                           
SENDB21A LA    R1,4(R1)                                                         
         B     SENDBP21                                                         
SENDBP22 MVC   PRODSAVE,NBSPLPRN   SAVE CURRENT PRODUCT                         
         MVC   PRODSAV3,NBSPLPR3   SAVE CURRENT PRODUCT                         
         MVC   NBSPLPRN,NUSPRBPR   SET BILLING PRODUCT                          
         CLI   NUSPRLEN,NUSPRLN4   3 CHAR PROD?                                 
         BNE   *+14                                                             
         MVC   NBSPLPR3,NUSPRBPC   YES - USE THAT                               
         B     SENDBP23                                                         
*                                                                               
         MVC   WORK(1),NUSPRBPR                                                 
         BRAS  RE,GETPRD3                                                       
         MVC   NBSPLPR3,WORK                                                    
*                                                                               
SENDBP23 MVI   NBRDBELS,2          ACCGEN ENTRIES RESTRICTED                    
         BAS   R3,DRIVEIT                                                       
         MVI   NBRDBELS,0          CLEAR ACCGEN RESTIRCTIONS                    
         MVC   NBSPLPRN,PRODSAVE   RESET PRODUCT                                
         MVC   WORK(3),NBSPLPR3     = NUSPRBPR                                  
         MVC   NBSPLPR3,PRODSAV3   RESET PRODUCT                                
*                                                                               
         LA    R1,SENTPLST         ADD PROD TO TABLE                            
         LA    RE,10                                                            
SENDBP25 CLI   1(R1),0                                                          
         BE    SENDBP30                                                         
         LA    R1,4(R1)                                                         
         BCT   RE,SENDBP25                                                      
         DC    H'0'                    INCREASE TABLE                           
SENDBP30 MVC   0(1,R1),NUSPRBPR        SET SENT PROD                            
         MVC   1(3,R1),WORK             SET SENT 3 CHAR PROD                    
         B     SENDBP10            ANY MORE ELEMENTS                            
SENDBPX  DS    0H                                                               
SENDBPXS B     XIT                                                              
*                                                                               
INTPRD3  DS    CL3                 LOCAL                                        
SENTPLST DS    CL40                                                             
         DC    X'00'                                                            
         DROP  R4                                                               
*                                                                               
* CHECK PROD ON BILL ELEMENT AGAINST MULTI PRODS                                
DPLST    NTR1                                                                   
         ZIC   R2,NBPRDNO                                                       
         LTR   R2,R2                                                            
         BZ    DPLST10                                                          
         ICM   R1,15,NBADPLST                                                   
DPLST5   CLC   0(3,R1),WORK                                                     
         BE    DPLSTX              MATCH CC=                                    
         LA    R1,3(R1)                                                         
         BCT   R2,DPLST5                                                        
DPLST10  LTR   R1,R1               NO MATCH CC NOT =                            
DPLSTX   B     XIT                                                              
                                                                                
                                                                                
DRIVEIT  DS    0H                                                               
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         BR    R3                                                               
*                                                                               
SPCHRLST DS    0H                                                               
         DC    C'UCUT'                                                          
         DC    C'BBLA'                                                          
         DC    C'SCPY'                                                          
         DC    C'AADM'                                                          
         DC    C'ESEC'                                                          
         DC    C'XTAX'                                                          
         DC    C'LLAT'                                                          
         DC    C'OOTH'                                                          
         DC    C'DDLV'                                                          
         DC    C'QERN'                                                          
         DC    C'IINT'                                                          
         DC    X'FF'                                                            
***************************************                                         
*                                                                               
UNITBEL  NTR1                                                                   
*                                                                               
***********************************************************                     
         CLI   NBPRDNO,0                                                        
         BE    NOCOPY                                                           
         LA    RE,PRDUPS                                                        
         CLI   NBSPLTYP,C'F'       FIRST PASS                                   
         BNE   NOCOP5                                                           
         XC    PRDUPS,PRDUPS                                                    
         MVC   0(3,RE),NBSPLPR3                                                 
         B     NOCOPY                                                           
*                                                                               
NOCOP5   ZIC   R1,NBPRDNO                                                       
NOCOP6   CLC   0(3,RE),NBSPLPR3                                                 
         BNE   NOCOP7              IF WE'VE DONE THAT ONE                       
         MVI   PRDUPS+18,X'FF'      PASS FLAG TO CHECK READ                     
         B     XIT                 WE'VE DONE THAT ONE                          
NOCOP7   LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BE    NOCOP8                                                           
         BCT   R1,NOCOP6                                                        
NOCOP8   MVC   0(3,RE),NBSPLPR3                                                 
*                                                                               
NOCOPY   DS    0H                                                               
***********************************************************                     
         L     R3,=A(MYIO)                                                      
         GOTO1 ABHRD,DMCB,(RC),(R5),(R3),1,NDACSHRC                             
         L     R1,=A(HOOK)          RESET HOOK SINCE ABHRD CHANGES IT           
         ST    R1,GLAHOOK                                                       
         B     XIT                                                              
*                                                                               
PRDUPS   DS    CL19              6X3 +1 FOR FLAG                                
*                                                                               
CHECKRD  NTR1                                                                   
         CLI   PRDUPS+18,X'FF'     IF WE'VE DONE THAT PROD                      
         BE    CHKRDX              SKIP IT                                      
         GOTO1 =A(OVERFLW),DMCB,(RC),5         UNIT CHECK NUMBER READ           
CHKRDX   MVI   PRDUPS+18,0                                                      
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL READING OF GOALS                                         
         SPACE 3                                                                
UNITEND  DS    0H                                                               
         TM    NDCOLIND,X'20'      PUP READ?                                    
         BO    UNITEND0            YES                                          
         ICM   R3,15,NBAIO         NO/TEST I/O AREA                             
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         L     R2,=F'16000'                                                     
         GOTO1 =V(COVAIL),DMCB,=C'FREE',(R3),(R2)                               
         MVC   NBAIO,AIO                                                        
*                                                                               
         L     R3,NBWBFLT                                                       
         LTR   R3,R3                                                            
         BZ    NOWBFLT                                                          
         L     R2,=F'34000'                                                     
         GOTO1 =V(COVAIL),DMCB,=C'FREE',(R3),(R2)                               
         XC    NBWBFLT,NBWBFLT                                                  
NOWBFLT  DS    0H                                                               
*                                                                               
         ICM   R3,15,NDAPKGTB         RETURN PKGTABLE STORAGE                   
         BZ    UNITEND0                                                         
         LA    R1,NDPKGTN             NUMBER OF RECORDS                         
         LA    R4,NDPKGTLE            LENGTH OF RECORD                          
         MR    R0,R4               GETLENGTH OF TABLE                           
         LR    R2,R1                                                            
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R3),(R2)                                
UNITEND0 EQU   *                                                                
                                                                                
         OC    BILLFLT,BILLFLT     SAVED BILL FILT IN BHRD                      
         BZ    *+10                                                             
         MVC   NBBILSTR(4),BILLFLT    YES/RESET                                 
         TM    NDCOLIND,X'80'                                                   
         BNO   PUPREAD                                                          
         SPACE 1                                                                
         L     R4,=A(NETGOALD)     OTHERS WILL NEED A(GOAL BLOCK)               
         ST    R4,NDAGBLOK                                                      
         USING NETGOALD,R4                                                      
         XC    NGBLOCK,NGBLOCK     INITIALIZE NETGOAL BLOCK                     
         OC    NDGOALDT,NDGOALDT                                                
         BZ    *+22                                                             
         XC    NDGOALDT(4),NBCMPSTR     SWAP THE FIELDS                         
         XC    NBCMPSTR(4),NDGOALDT     MOVE GOAL DATES INTO                    
         XC    NDGOALDT(4),NBCMPSTR     NETVALUE                                
         LA    R1,NETBLOCK         PASS A(NETBLOCK)                             
         ST    R1,NGANTBLK                                                      
         LA    R1,GGHOOK                A(MY HOOK ROUTINE)                      
         ST    R1,NGAHOOK                                                       
         L     R1,=A(PRODLIST)          A(PRODUCT LIST)                         
         ST    R1,NGAPLIST                                                      
         MVI   NGMAXPRD,250                                                     
         MVC   NGSELDP,NBSELDP     OPTIONAL FILTERS                             
         MVC   NGSELSL,NBSELLEN                                                 
*                                                                               
         CLI   NBSELMFL,0          MEDIA FILTERING ?                            
         BE    NOMEDFLT                                                         
         B     NOMEDFLT            FOR NOW!!!!!!!!!!!!!!!!!!!!                  
         CLI   NBSELMFL,C'N'       NETWORK=777                                  
         BNE   *+14                                                             
         MVC   NGSELMKT,=H'777'                                                 
         B     NOMEDFLT                                                         
         CLI   NBSELMFL,C'C'       CABLE=775                                    
         BNE   *+14                                                             
         MVC   NGSELMKT,=H'775'                                                 
         B     NOMEDFLT                                                         
         CLI   NBSELMFL,C'S'       SYNDICATION=774                              
         BNE   *+14                                                             
         MVC   NGSELMKT,=H'774'                                                 
         B     NOMEDFLT                                                         
NOMEDFLT DS    0H                                                               
*                                                                               
         CLI   NBN2B16,C'P'        PACKAGE LEVEL GOALS                          
         BE    *+12                                                             
         CLI   NBN2B16,C'B'        PACKAGE AND NETWORK LEVEL GOALS              
         BNE   *+10                                                             
         MVC   NGSELPKG,NBSELPAK                                                
         CLI   NBN2B16,C'Y'        NETWORK LEVEL GOALS                          
         BE    *+12                                                             
         CLI   NBN2B16,C'B'        PACKAGE AND NETWORK LEVEL GOALS              
         BNE   *+10                                                             
         MVC   NGSELMKT,BINMKT                                                  
         MVI   NGEXBOPT,C'Y'       EXPAND INTO NETBLOCK                         
*                                                                               
         OI    NGSELOPT,NGSELBOT   PASS BOTH PLAN AND REGULAR GOALS             
         SPACE 1                                                                
*                                  GET ADDRESS OF NETGOAL                       
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,NGBLOCK                                                
*                                                                               
         TM    NGOALIND,NGOALPLN   PLAN GOAL RECORD ?                           
         BNO   *+8                                                              
         OI    NBINDS7,NBI7PGL     YES-NETBLOCK FLAG                            
*                                                                               
         TM    NBSELPRN,1              IS THIS UPLD REPORT?                     
         BNO   NOTUPLD                                                          
         MVI   NBSELPRN,0              YES/CLEAR FLAG                           
         MVI   GLMODE,GLINPUT          AND AAND                                 
         MVI   NBMODE,NBPROCGL                                                  
         GOTO1 NDDRIVER,DMCB,(R6)                                               
NOTUPLD  EQU   *                                                                
*                                                                               
         OC    NDGOALDT,NDGOALDT                                                
         BZ    *+22                                                             
         XC    NDGOALDT(4),NBCMPSTR     SWAP THE FIELDS                         
         XC    NBCMPSTR(4),NDGOALDT     RESTORE NETVALUE WITH                   
         XC    NDGOALDT(4),NBCMPSTR     UNIT DATES.                             
         B     PUPREAD                                                          
         SPACE 1                                                                
GGHOOK   MVC   AIO2,AIO                                                         
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
*                                                                               
         NI    NBINDS7,X'FF'-NBI7PGL CLEAR FLAG                                 
         TM    NGOALIND,NGOALPLN   PLAN GOAL RECORD ?                           
         BNO   *+8                                                              
         OI    NBINDS7,NBI7PGL     YES-NETBLOCK FLAG                            
*                                                                               
         L     R3,NBAIO                                                         
         USING GOALREC,R3                                                       
         TM    GKEYAGY,X'20'       GOAL HISTORY REC ?                           
         BO    GGHOOK3                                                          
         TM    NDLOCAL,NDGHIST       NO/READ ONLY GOAL HISTORY RECS ?           
         BO    XIT                       YES SO SKIP THIS ONE                   
         B     GGHOOK4                                                          
*                                                                               
GGHOOK3  TM    NDCOLIND,X'01'      YES/ GOAL HIST KEYWORD?                      
         BNO   XIT                      NO                                      
         BAS   RE,DOGHIST               YES/DO GOAL HISTORY RECS                
         B     GGHOOK5                                                          
GGHOOK4  CLC   =C'GOALDOWN',CONREC     GOALDOWN PROGRAM                         
         BNE   GGHOOK4G              NO                                         
*                                                                               
         CLI   NBSELPRN,0            IN MEDIA RES?                              
         BNE   GGHOOK4B              YES                                        
         MVI   NBSELPRN,1            NO                                         
         OI    NBSELPRN,X'02'        FIRST TIME                                 
         L     R1,NBAIO                    HAS GOAL REC CHANGED?                
         MVC   PREVKEY,0(R1)               SET KEY                              
         B     GGHOOK4G                                                         
GGHOOK4B L     R1,NBAIO                    HAS GOAL REC CHANGED?                
         CLC   0(13,R1),PREVKEY             HAS GOAL REC CHANGED?               
         BE    GGHOOK4G                NO                                       
         MVC   PREVKEY,0(R1)                                                    
         OI    NBSELPRN,X'04'          SET REC FLAG                             
*                                                                               
GGHOOK4G MVI   GLMODE,GLINPUT                                                   
         MVI   NBMODE,NBPROCGL                                                  
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         TM    NBSELPRN,1           GOAL DOWN PROGRAM?                          
         BNO   GGHOOK5                                                          
*                                                                               
         TM    NBSELPRN,X'02'          FIRST TIME?                              
         BNO   GGHOOK4I                                                         
         NI    NBSELPRN,X'FF'-X'02'    CLEAR FIRST FLAG                         
         OI    NBSELPRN,X'04'          SET REC FLAG                             
         B     GGHOOK4G                AND PASS REC TO DRIVER                   
GGHOOK4I TM    NBSELPRN,X'04'          DID WE JUST PASS REC?                    
         BNO   GGHOOK5                 NO                                       
         NI    NBSELPRN,X'FF'-X'04'    TURN OF REC FLAG                         
         OI    NBSELPRN,X'08'          SET DETAIL FLAG                          
         B     GGHOOK4G                AND LET DRIVER DO DETAILS                
GGHOOK5  MVI   NGOALIOS,2          RESTORE READ SEQUENCE                        
         B     XIT                                                              
PREVKEY  DS    CL13                                                             
                                                                                
*******************************************************************             
* READ GOAL HIST REC AND PASS ADDR OF COMMENT IN NBPAKCPM TO DRIVER             
DOGHIST  NTR1                   GET GOAL HISTORY INFO                           
***      XC    NGOALACT(64),NGOALACT  CLEAR PREVIOUS GOAL INFO                  
*** HAD TO REMOVE ABOVE SINCE CLEARING ALL WAS SCREWING UP PRODS                
         XC    NGOALG2(12),NGOALG2   TARG1/$/TARG2                              
         LA    R3,24(R3)                                                        
DOG3     CLI   0(R3),X'60'         GOAL HIST ELEM                               
         BH    XIT                 THATS ALL                                    
         BE    DOG5                GOT IT                                       
DOG4     ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    XIT                 THATS ALL                                    
         AR    R3,R1                                                            
         B     DOG3                                                             
DOG5     ST    R3,NDAGHIST         PASS ADR OF COMMENT                          
         MVI   GLMODE,GLINPUT                                                   
         MVI   NBMODE,NBPROCGL                                                  
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         XC    NDAGHIST,NDAGHIST                                                
         B     DOG4                ANY MORE COMMENTS?                           
**********************************************************************          
         DROP  R4                                                               
         DROP  R3                                                               
         SPACE 1                                                                
PUPREAD  DS    0H                                                               
         TM    NDCOLIND,X'20'                                                   
         BNO   GOALEND                                                          
         SPACE 1                                                                
***      L     R4,=A(NETPUPD)      OTHERS WILL NEED A(GOAL BLOCK)               
***      ST    R4,NDAGBLOK                                                      
***      USING NETPUPD,R4                                                       
***      XC    NPBLOCK,NPBLOCK     INITIALIZE NETGOAL BLOCK                     
***      L     RF,=A(PLANIO)       ADDRESS OF PLAN RECORD                       
***      ST    RF,NPPLANAD                                                      
***      MVC   NPPROGAD,NBAIO      ADDRESS OF PROGRAM RECORD                    
***      LA    R1,NETBLOCK         PASS A(NETBLOCK)                             
***      ST    R1,NPANTBLK                                                      
***      LA    R1,PPHOOK                A(MY HOOK ROUTINE)                      
***      ST    R1,NPAHOOK                                                       
**8      MVC   NPSELAGY,NBSELAGY   AGENCY                                       
***      CLC   NBSELCLI,=C'ALL'                                                 
***      BE    *+10                                                             
***      MVC   NPSELCLI,NBACTCLI   CLIENT                                       
***      MVC   NPSELNET,NBSELNET   NETWORK                                      
***      MVC   NPSELMED,NBSELMFL   MEDIA FILTER                                 
***      MVC   NPSELDP,NBSELDP     DAYPART                                      
***      MVC   NPSELPLN,NDPLNCDE   PLAN                                         
***      MVC   NPSELPFT,NDPLNFLT   PLAN FILTER                                  
***      MVC   NPSELUFT,NDPROGFT   PROGRAM FILTER                               
***      MVC   NPSELLEN,NBSELLEN   LENGTH                                       
***      GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,NPSELSDT)                            
***      GOTO1 DATCON,DMCB,(0,NBSELEND),(2,NPSELEDT)                            
***      OI    NPPUPARM,NPEXBOPT   EXPAND INTO NETBLOCK                         
***      CLI   SPLDEMH+5,0         CHECK FOR DEMO INPUT                         
***      BZ    *+8                                                              
***      OI    NPPUPARM,NPOVDEMO   DONT CREATE DEMO LIST                        
***      CLI   NDPLNHED,C'Y'       CHECK PLAN IN HEADLINE                       
***      BNE   *+8                                                              
***      OI    NPPUPARM,NPHPLAN    CHANGE DEMO LIST ON PLAN READ                
***      TM    NDCOLIND,X'10'      BUDGET IN DETAIL                             
***      BZ    *+8                                                              
***      OI    NPPUPARM,NPBUDGT    SET BUDGET IN DETAIL                         
***      SPACE 1                                                                
***      OI    NBINDS,X'40'        SET INCREASE PRECISSION                      
*                                  GET ADDRESS OF NETPUP                        
**       GOTO1 NBCALLOV,DMCB,0,X'D9000A55'                                      
***      L     RF,DMCB                                                          
***      GOTO1 (RF),DMCB,NPBLOCK                                                
*                                                                               
         B     GOALEND                                                          
****     DROP  R4                                                               
         SPACE 1                                                                
PPHOOK   MVC   AIO2,AIO                                                         
***      L     R1,=A(MYIO)                                                      
***      ST    R1,AIO                                                           
***      MVI   GLMODE,GLINPUT                                                   
***      MVI   NBMODE,NBPROCPP                                                  
***      GOTO1 NDDRIVER,DMCB,(R6)                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
GOALEND  DS    0H                                                               
         CLI   LSTRD,0            ..ARE WE DOING LISTS                          
         BE    GL12                                                             
         BAS   RE,LSTRDS          ..YES                                         
         CLI   LSTRD,0            ..ANY MORE TO READ                            
         BNE   GETUNIT1                                                         
                                                                                
*                                                                               
GL12     DS    0H                                                               
*                                                                               
         TM    NDINDS1,ND1WB       SEND OUTPUT DATA TO FILE?                    
         BZ    GL12D                NO                                          
*                                                                               
** MQFILENM IS ORG'D OVER MAAORLK (ONLY USED DURING INPUT)                      
         MVC   MQFILENM,=CL35' '               SET BLANKS                       
         MVC   MQFILENM(14),=C'SFTPDISK.PROD.'                                  
         BRAS  RE,TESTRUN                                                       
         BNE   *+10                                                             
         MVC   MQFILENM+9(4),=C'TEST'                                           
         LA    R2,MQFILENM+14                                                   
*                                                                               
         USING FILNAMD,R2                                                       
         MVC   0(FILNAMDQ,R2),=C'NET.M2.MMMDD.THHMMSS'                          
******   MVC   FNAGY,NBSELAGY                                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(4,FNMMMDD)                               
         THMS                                                                   
         ST    R1,FULL                                                          
         EDIT  (P4,FULL),(6,FNHHMMSS),FILL=0                                    
         LA    RE,MQFILENM                                                      
         ST    RE,GLAOUTP          TELL DRIVER FILE NAME                        
         DROP  R2                                                               
*                                                                               
FILNAMD  DSECT                                                                  
FNSYS    DS    CL3                 NET                                          
         DS    C                   .                                            
FNAGY    DS    CL2                 M2                                           
         DS    C                   .                                            
FNMMMDD  DS    CL5                 MMMDD                                        
         DS    C                   .                                            
         DS    C                   T                                            
FNHHMMSS DS    CL6                 HHMMSS                                       
FILNAMDQ EQU   *-FILNAMD                                                        
*                                                                               
T32020   CSECT                                                                  
*                                                                               
GL12D    MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         TM    NDINDS1,ND1WB       DID WE SEND OUTPUT TO FILE?                  
         BZ    GL12T                NO                                          
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
MQ       USING MQMSGD,ELEM                                                      
*                                                                               
         BRAS  RE,MQOPEN                                                        
         BRAS  RE,MQCLOSE                                                       
*                                                                               
T32020   CSECT                                                                  
*                                                                               
GL12T    TM    NBINDS3,NBI3CMRT        COMMERCIAL ROTATION?                     
         BZ    GL12X                                                            
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         GOTO1 SPOOL,DMCB,(R3)                                                  
         L     R1,GLAP1                                                         
         MVC   1(35,R1),=C'* THIS HALF HOUR IS OUT OF ROTATION'                 
         GOTO1 SPOOL,DMCB,(R3)                                                  
         GOTO1 SPOOL,DMCB,(R3)                                                  
         DROP  R3                                                               
GL12X    EQU   *                                                                
*                                                                               
         OC    NDCMDEF2,NDCMDEF2   CHECK BOTTOM COMMENTS                        
         BZ    GL14                                                             
         GOTO1 =A(OVERFLW),DMCB,(RC),8,RR=YES        GET COMMENTS               
*                                                                               
GL14     ICM   R3,15,NDAPQIND      TEST PQINDEXTBL                              
         LTR   R3,R3                                                            
         BZ    GLX                                                              
         AHI   R3,-4               POINT TO BEGINNING OF TABLE                  
         L     R2,0(R3)                                                         
         GOTO1 =V(COVAIL),DMCB,=C'FREE',(R3),(R2)                               
GLX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
LSTRDS   NTR1                                                                   
         GOTO1 =A(OVERFLW),DMCB,(RC),3          CROSS AGY READ                  
         B     XIT                                                              
         EJECT                                                                  
*              FILTER UNITS                                                     
         SPACE 3                                                                
FILTUNIT NTR1                                                                   
*                                                                               
         ICM   R2,15,NBINVFLT                                                   
         BZ    INVFLTX                                                          
         OC    0(12,R2),0(R2)      INVOICE FILTERS?                             
         BZ    INVFLTX                                                          
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   REJECT                                                           
         USING NUBILD,R4                                                        
INVFLT5  GOTO1 DATCON,DMCB,(2,NUBILDAT),DUB                                     
         MVC   DUB(2),DUB+2                   MOVE MM                           
         MVC   DUB+2(4),NUBILNUM              MOVE NUMBER                       
*                                                                               
         CLI   0(R2),C'*'             START INV NUMBER                          
         BE    *+14                                                             
         CLC   DUB(6),0(R2)                                                     
         BL    INVSEQ                 NO-GET NXT INVOICE                        
*                                                                               
         CLI   6(R2),C'*'             END INV NUBER                             
         BE    INVFLTX                ONE INVOICE MATCHES                       
         CLC   DUB(6),6(R2)                                                     
         BNH   INVFLTX                ONE INVOICE MATCHES                       
         B     INVSEQ                 NO MATCH - ANY OTHER INVOICES             
*                                                                               
INVSEQ   BAS   RE,NEXTEL                                                        
         BNE   REJECT                                                           
         B     INVFLT5                                                          
         DROP  R4                                                               
INVFLTX  EQU   *                                                                
         TM    NBSBKEND,X'10'      ZERO $ UNITS ONLY                            
         BNO   ZERO5                                                            
         TM    NBUNITST,X'42'      IF PREEMPT OR MISSED                         
         BNZ   ZERO5               IT'S A ZERO $ UNIT                           
         OC    NBACTUAL,NBACTUAL                                                
         BNZ   REJECT                                                           
ZERO5    TM    NBSBKEND,X'40'       SKIP 0 $ UNITS                              
         BNO   ZEROX                                                            
         OC    NBACTUAL,NBACTUAL                                                
         BZ    REJECT                                                           
         TM    NBUNITST,X'42'      IF PREEMPT OR MISSED                         
         BNZ   REJECT              IT'S A ZERO $ UNIT                           
                                                                                
ZEROX    EQU   *                                                                
         TM    NBINDS,X'03'        DEMO OVERRIDE FILTERS                        
         BZ    DEMFILTX            NO                                           
* FILTER ON EST DEMO OVERRIDES                                                  
         TM    NBINDS,X'02'        FILTER ON EST DEMO OVERRIDES?                
         BZ    DEMFILT5            NO                                           
         L     R4,NBAIO            YES                                          
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMFILT4                                                         
         USING NUOVD,R4                                                         
DEMFILT2 TM    NUOVFLG,X'80'       IS IT A NAD DEMO?                            
         BZ    DEMFILTX            NO-LEGITIMATE OVERRIDE                       
         BAS   RE,NEXTEL           YES - ANY MORE ELEMS?                        
         BNE   *+8                                                              
         B     DEMFILT2                                                         
DEMFILT4 TM    NBINDS,X'01'        OR FILTER ON ACT DEMO OVERRIDE?              
         BZ    REJECT              NO - REJECT                                  
*                                                                               
DEMFILT5 EQU   *                   FILTER ON ACT DEMO OVERRIDE                  
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'DE'                                                     
         BAS   RE,GETEL                                                         
         BNE   REJECT              NO MATCH - REJECT                            
DEMFILT7 TM    NUOVFLG,X'80'       IS IT NAD DEMO?                              
         BZ    DEMFILTX            NO                                           
         BAS   RE,NEXTEL           YES - ANY MORE ELEMS                         
         BNE   REJECT              NO - REJECT                                  
         B     DEMFILT7                                                         
DEMFILTX EQU   *                                                                
*************************************************************                   
         OC    NDCMLFLT,NDCMLFLT  COMMERCIAL FILTER                             
         BZ    FILTUNT0                                                         
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   REJECT                                                           
         USING NUCMLEL,R4                                                       
         CLC   NDCMLFLT,NUCML1     COMMERCIALS MATCH ?                          
         BE    FILTUNT0                                                         
         CLC   NDCMLFLT,NUCML2     COMMERCIALS MATCH ?                          
         BE    FILTUNT0                                                         
                                                                                
         MVI   ELCODE,X'23'        NO - TRY FEED COMMERCIAL                     
FLTCOM8  BAS   RE,NEXTEL                                                        
         BNE   REJECT                                                           
         USING NUFDCEL,R4                                                       
         CLC   NUFDCML1,NDCMLFLT                                                
         BE    FILTUNT0                                                         
FLTCOM11 CLC   NUFDCML2,NDCMLFLT                                                
         BNE   FLTCOM8                                                          
         DROP  R4                                                               
***************************************************************                 
                                                                                
                                                                                
FILTUNT0 CLI   NDPOSTYP,0          POSTING TYPE FILTER                          
         BE    *+14                                                             
         CLC   NDPOSTYP,NBPOSTYP                                                
         BNE   REJECT                                                           
         OC    NDCMLCLS,NDCMLCLS   COMMERCIAL CLASS FILTER                      
         BZ    *+12                                                             
         BAS   RE,FLTCCLS                                                       
         BNE   REJECT                                                           
         CLC   =C'UNALL',SPLPRO                                                 
         BE    *+12                                                             
         CLI   SPLFILTH+5,0                                                     
         BE    FILTUNT2                                                         
         MVC   SVNBACT,NBACTUAL    SAVE VALUES                                  
         MVC   SVNBASS,NBASSIGN                                                 
         MVC   SVNBINT,NBINTEG                                                  
         NETGO NVACNEW,DMCB        AND TEST ACCOUNTING FILTERS                  
         BNE   REJECT                                                           
         MVC   NBACTUAL,SVNBACT    RESTORE THOSE VALUES                         
         MVC   NBASSIGN,SVNBASS                                                 
         MVC   NBINTEG,SVNBINT                                                  
         SPACE 1                                                                
FILTUNT2 CLI   SPLFLAV+1,C'P'                                                   
         BE    FLAVP                                                            
         CLI   SPLFLAV+1,C'M'                                                   
         BE    FLAVM                                                            
         CLI   SPLFLAV+1,C'F'                                                   
         BE    FLAVF                                                            
         CLI   SPLFLAV+1,C'X'                                                   
         BE    FLAVX                                                            
         CLI   SPLFLAV,C'E'                                                     
         BE    OK                                                               
******** TM    NBUNITST,X'02'      DEFAULT IGNORES MISSED                       
******** BO    REJECT              (WHY? - REMOVED 1/27/89)                     
         B     OK                                                               
         SPACE 1                                                                
FLAVF    TM    NBUNITST,X'04'      F NEEDS PFBS                                 
         BNO   REJECT                                                           
         TM    NBUNITST,X'03'        IGNORES MISSED AND MAKEGOODS               
         BNZ   REJECT                                                           
         B     OK                                                               
         SPACE 1                                                                
FLAVM    TM    NBUNITST,X'01'      M NEEDS ONLY MAKEGOODS                       
         BNO   REJECT                                                           
         B     OK                                                               
         SPACE 1                                                                
FLAVP    DS    0H                                                               
         TM    NBUNITST,X'40'      P NEEDS ONLY PRE-EMPTS                       
         BZ    REJECT                                                           
         TM    NBUNITST,X'02'        THAT HAVE NOT BEEN MADE GOOD               
         BO    REJECT                                                           
         B     OK                                                               
         SPACE 1                                                                
FLAVX    TM    NBUNITST,X'02'      IGNORE MISSED UNITS                          
         BO    REJECT                                                           
         TM    NBUNITST,X'04'      IGNORE PFBS THAT ARENT MAKEGOODS             
         BNO   OK                                                               
         TM    NBUNITST,X'01'      IS IT MAKEGOOD?                              
         BNO   REJECT              YES-KEEP / ELSE-REJECT                       
         SPACE 1                                                                
OK       SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
REJECT   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
EDEND    CLI   OFFLINE,C'Y'                                                     
         BNE   *+8                                                              
         BRAS   RE,INITFAX                                                      
         GOTO1 NDWRPDRO                                                         
*                                                                               
         CLI   OFFLINE,C'Y'        FILTER REQUIRED                              
         BE    EDENDX                                                           
         LA    R2,SPLFLTH          POINT ERROR MSG TO FILTER FIELD              
         CLC   =C'@OJX',SPLCLI     IF SPECIAL OM/JW CROSS AGY REPORT?           
         BNE   EDENDX                                                           
         CLC   =C'CMBO',SPLFLT     MUST HAVE PASSWORD                           
         BNE   FILTERR                                                          
         XC    SPLFLT,SPLFLT       OK,CLEAR IT                                  
         MVI   SPLFLTH+5,0         SO IT DOESN'T SHOW ON REPORTS                
EDENDX   LA    R2,SPLCLIH                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    EDENDXX                                                          
* REQSML CHECKS                                                                 
         TM    NDANYDEM,RAW+EQU+NN+NQ    ..SPECIAL DEMOS?                       
         BNZ   REQSET                                                           
         CLC   =C'ALL',NBSELCLI          ..ALL CLIENTS?                         
         BE    REQSET                                                           
         TM    NDINDS1,X'01'             ..MORE THAN 350 STR-END REQ?           
         BNO   *+8                                                              
REQSET   MVI   REQSML,C'L'               ..SET LARGE JOB                        
*                                                                               
         TM    NBSBKEND,NBEXPDEM+NBEXPDM6 BOTH TRUNCATE+EXPAND?                 
         BNO   EDENDXX                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'* CANNOT MIX EXPDEM AND EXPDEM6 OPTIONS'          
         GOTO1 ERREX2                                                           
EDENDXX  B     XMOD                                                             
         EJECT                                                                  
* - COMMERCIAL CLASS FILTER                                                     
FLTCCLS  GOTO1 =A(OVERFLW),DMCB,(RC),1                                          
         B     XIT                                                              
*                                                                               
FILTERR  MVC   HALF,=H'641'        4 CHARACTER FILTER REQUIRED                  
         B     ERR2                                                             
NOFLTERR MVC   HALF,=H'642'        REPORT REQUIRES FILTER                       
         B     ERR2                                                             
AUTHERR  LA    R2,SPLCLIH                                                       
         MVC   HALF,=H'643'        UNAUTHORIZED REQUEST                         
         B     ERR2                                                             
* - 2 BYTE ERROR MESSAGES                                                       
ERR2     LA    R3,WORK                                                          
         USING GBLOCK,R3                                                        
         XC    GBLOCK,GBLOCK                                                    
         MVC   GERROR,HALF                                                      
         GOTO1 NDERR                                                            
         DC    H'0'                                                             
         DROP  R3                                                               
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE TO BUILD DATE LISTS                                      
         SPACE 3                                                                
BDLIST   NTR1                                                                   
         XC    MYHALF,MYHALF                                                    
         BAS   RE,INITNBL          INITIALIZE NETBLOCK                          
         MVC   NBPEROVR,NDPEROPT   PASS PERIOD OVERRIDE                         
         MVI   NBRESUME,NBPROCPK   RESUME READING PACKAGES                      
         MVI   NREPTYP,C'A'        SET REPORT TYPE AS ACCOUNTING                
         LA    R3,NBUSER+2                                                      
         CLI   SPLFLAV,C'E'            FOR ESTIMATE FLAVORS                     
         BE    PROCDATE                                                         
         MVI   NREPTYP,C'M'            OR MEDIA                                 
         LA    R3,NBUSER+3                                                      
         SPACE 1                                                                
PROCDATE NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   PROCDATE                                                         
         SPACE 1                                                                
         CLI   BHRD,0              ...BILL HEADER READ                          
         BE    BDL5                ...NO                                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   BDL5                                                             
         L     R4,=A(MYIO)                                                      
***      A     R4,=F'4000'                                                      
         A     R4,=F'6000'                                                      
         USING BHBLOCK,R4                                                       
         GOTO1 DATCON,DMCB,(2,NBCMPSTR),(3,BHSELSTB)                            
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,NBCMPEND),(3,BHSELENB)                            
         SPACE 1                                                                
         DROP  R4                                                               
BDL5     OC    NDGOALDT,NDGOALDT         IF GOAL START DATE                     
         BZ    BDL10                     LOWER THAN REQUEST START               
         CLC   NDGOALDT(2),NBCMPSTR      USE GOAL START FOR DATE LIST           
         BNL   BDL10                                                            
         MVC   MYHALF,NBCMPSTR       SAVE NBCMPSTR IN MYHALF                    
         MVC   NBCMPSTR,NDGOALDT                                                
         B     BDL10                                                            
         SPACE 1                                                                
* - WILAN AGY SUMMARY COMES HERE TO BUILD DATES ON FLY FOR EACH UNIT            
BDXTRA   NTR1                                                                   
         MVI   NREPTYP,C'A'        SET REPORT TYPE AS ACCOUNTING                
         LA    R3,NBUSER+2                                                      
         CLI   SPLFLAV,C'E'            FOR ESTIMATE FLAVORS                     
         BE    BDL10                                                            
         MVI   NREPTYP,C'M'            OR MEDIA                                 
         LA    R3,NBUSER+3                                                      
         SPACE                                                                  
BDL10    LA    R4,DATEAREA                                                      
         USING DATELSTD,R4                                                      
         XC    PERTYPE,PERTYPE     BUILD DATE LISTS                             
         TM    NDINDS1,X'02'       DOING 36 ACCMON MONTHS                       
         BO    BDL15               SKIP WEEKS                                   
         LA    R1,105              105 WEEKS                                    
         ST    R1,NWEEKS                                                        
         MVI   PERTYPE,C'W'                                                     
         NETGO NVWKLST,DMCB,NWEEKS,WEEKLIST,PERTYPE                             
*                                                                               
BDL15    LA    R1,25               25 MONTHS                                    
         LA    R2,MNTHLIST                                                      
         TM    NDINDS1,X'02'       ACC MON KEYWROD?                             
         BNO   BDL20                                                            
         MVI   OPTION2,1           SET ACCMON KEYWORD FLAG                      
*                                                                               
BDL17    LA    R1,36                                                            
         CLI   OPTION2,2                                                        
         BE    BDL18                                                            
         LA    R2,ACCDTLSC+4       CALENDAR                                     
         MVI   NBPEROVR,C'C'                                                    
         MVC   ACCDTLSC(4),=X'00000001'                                         
         B     BDL20                                                            
BDL18    LA    R2,ACCDTLSB+4       BROADCAST                                    
         MVI   NBPEROVR,C'B'                                                    
         MVC   ACCDTLSB(4),=X'00000001'                                         
*                                                                               
BDL20    ST    R1,NMONTHS                                                       
         MVI   PERTYPE,C'M'                                                     
         NETGO NVWKLST,DMCB,NMONTHS,0(R2),PERTYPE                               
         TM    NDINDS1,X'02'                                                    
         BNO   ENDMNLST                                                         
*                                                                               
         LA    R1,36                                                            
MNLOOP   CLC   0(4,R2),=4X'00'                                                  
         BE    SETENDT                                                          
         LA    R2,4(R2)                                                         
         BCT   R1,MNLOOP                                                        
SETENDT  MVC   0(4,R2),=4X'FF'                                                  
         CLI   OPTION2,2           HAVE WE DONE BOTH LISTS?                     
         BE    ENDMNLST            YES                                          
         MVI   OPTION2,2           NO-SET FLAG AND DO                           
         B     BDL17                                                            
ENDMNLST EQU   *                                                                
*                                                                               
******************************************************                          
**                                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   SKIPMOB                                                          
         CLC   =C'DU',NBSELAGY                                                  
         BNE   SKIPMOB                                                          
         CLC   =C'MMM',NBSELCLI                                                 
         BE    DOMOB                                                            
         CLC   =C'MM3',NBSELCLI                                                 
         BE    DOMOB                                                            
         CLC   =C'MMH',NBSELCLI                                                 
         BE    DOMOB                                                            
         B     SKIPMOB                                                          
DOMOB    DS    0H                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R3,=A(MYIO)                                                      
         XC    0(30,R3),0(R3)                                                   
         GOTO1 GETPROF,DMCB,(0,WORK),(R3),DATAMGR                               
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         GOTO1 GETPROF,DMCB,(0,WORK),WORK,DATAMGR                               
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
         MVC   WORK+2(1),0(R3)                                                  
         MVC   WORK+6(3),1(R3)                                                  
*                                                                               
         XC    MNTHLIST,MNTHLIST                                                
         SR    R0,R0                                                            
         IC    R0,=C'M'                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(MOBILE),DMCB,(25,NBSELSTR),((R0),MNTHLIST),,WORK              
SKIPMOB  EQU   *                                                                
**                                                                              
******************************************************                          
*                                                                               
         SPACE 1                                                                
         CLI   NBPEROVR,0          ANY OVERRIDE?                                
         BE    QRT10                                                            
         CLI   NBPEROVR,C'B'                                                    
         BE    QLIST                                                            
         B     QRT15                                                            
QRT10    CLI   0(R3),C'B'          IF WE ARE NOT DOING BROADCAST                
         BE    QLIST                                                            
QRT15    LA    R1,8                8 QUARTERS                                   
         ST    R1,NQUARTS                                                       
         MVI   PERTYPE,C'Q'                                                     
         NETGO NVWKLST,DMCB,NQUARTS,QURTLIST,PERTYPE                            
         B     DLIST                                                            
         SPACE 1                                                                
QLIST    LA    R2,MNTHLIST                                                      
         LA    R3,QURTLIST                                                      
         LA    R0,25                                                            
         LA    R6,8                                                             
         MVC   0(2,R3),0(R2)       MOVE IN FIRST START DATE                     
         SPACE 1                                                                
QLIST2   MVC   2(2,R3),2(R2)       MOVE IN END OF CURRENT MONTH                 
         LA    R2,4(R2)            BUMP TO NEXT MONTH                           
         BCT   R0,*+8                                                           
         B     DLIST                                                            
         CLI   0(R2),0                                                          
         BE    DLIST                                                            
         GOTO1 NDGETBM             GET BROACAST MONTH MUMBER                    
         ZIC   RF,WORK+4           (RETURNED BY GETBM IN WORK+4)                
         SR    RE,RE                                                            
         D     RE,=F'3'            IF MONTH IS 1 4 7 10                         
         CH    RE,=H'1'               REMAINDER WILL BE 1                       
         BNE   QLIST2                                                           
         CH    R6,=H'8'            IF NOT FIRST PASS IGNORE SEP. CHECK          
         BNE   QLIST4                                                           
         CLI   WORK+4,10           BUT IF THIS IS OCTOBER                       
         BNE   QLIST4                                                           
         CLC   NBSELSTR+2(2),=C'09'    AND REQ. STARTED IN SEPTEMBER            
         BE    QLIST2              DON'T BREAK                                  
         SPACE 1                                                                
QLIST4   LA    R3,4(R3)               AND WE HAVE A NEW QUARTER                 
         BCT   R6,*+8                                                           
         B     DLIST                                                            
         MVC   0(2,R3),0(R2)          MOVE IN START                             
         B     QLIST2                                                           
         SPACE 1                                                                
DLIST    MVC   WORK(6),NBSELSTR                                                 
         LA    R3,DAYSLIST         LIST UP TO 14 DAYS                           
         LA    R0,14                                                            
         SPACE 1                                                                
DLIST2   GOTO1 DATCON,DMCB,(0,WORK),(2,0(R3))                                   
         MVC   2(2,R3),0(R3)       (END=START)                                  
         CLC   WORK(6),NBSELEND                                                 
         BE    YRLIST                                                           
         LA    R3,4(R3)                                                         
         BCT   R0,*+8                                                           
         B     YRLIST                                                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         B     DLIST2                                                           
*                                                                               
YRLIST   DS    0H                                                               
         MVC   YEARLIST(2),MNTHLIST                                             
         LA    R3,MNTHLIST                                                      
         LA    R3,46(R3)                                                        
         MVC   YEARLIST+2(2),0(R3)                                              
         LA    R3,2(R3)                                                         
         MVC   YEARLIST+4(2),0(R3)                                              
         LA    R3,46(R3)                                                        
         MVC   YEARLIST+6(2),0(R3)                                              
         B     BDEX                                                             
*                                                                               
BDEX     OC    MYHALF,MYHALF       IF WE SAVED NBCMPSTR                         
         BZ    *+10                                                             
         MVC   NBCMPSTR,MYHALF     RESTORE IT                                   
         L     R6,NDGLOBAL                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGAINST A LIST                               
         SPACE 2                                                                
LISTVAL  NTR1                                                                   
         SPACE 3                                                                
LISTVAL2 CLC   0(2,R3),WORK                                                     
         BE    XIT                                                              
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   LISTVAL2                                                         
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 2                                                                
FLAVLIST DS    0C                                                               
         DC    C'E '               ESTIMATE                                     
         DC    C'EE'                     ESTIMATED SCHEDULE                     
         DC    C'EM'                     MISSED/MAKEGOODS                       
         DC    C'EF'                     FROZEN                                 
         DC    C'EP'                     PRE-EMPTS                              
         DC    C'EX'                     EXCLUDE MISSED/MAKEGOODS               
         DC    C'V '               EVALUATION                                   
         DC    C'V1'                                                            
         DC    C'V2'                                                            
         DC    C'V3'                                                            
         DC    C'P '               POST  REGULAR                                
         DC    C'P2'                     DOUBLE COLUMNS                         
         DC    C'PF'                     FROZEN                                 
         DC    C'PM'                     MISSED/MAKEGOODS                       
         DC    C'PP'                     PRE-EMPTS                              
         DC    C'PX'                     EXCLUDE MISSED/MAKEGOODS               
         DC    C'M '               MEDIA REGULAR                                
         DC    C'MF'                     FROZEN                                 
         DC    C'MM'                     MISSED/MAKEGOODS                       
         DC    C'MP'                     PRE-EMPTS                              
         DC    C'MX'                     EXCLUDE MISSED/MAKEGOODS               
         DC    C'MD'                     EXCLUDE ADU                            
         DC    C'BH'                     BILLING HEADER READ                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              INITIALIZE ODDMENTS FOR NETBLOCK                                 
         SPACE 3                                                                
INITNBL  NTR1                                                                   
         GOTO1 =A(OVERFLW),DMCB,(RC),4,RR=YES                                   
         B     XIT                                                              
         EJECT                                                                  
*              INTIALIZE DBLOCK                                                 
         SPACE 3                                                                
INITDB   NTR1                                                                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
* - CALLED AFTER READING BILLING RECS IN BH/O FLAVOR                            
* - RE-VALIDATE SCREEN ENTRIES TO SET UP FOR READINMG UNITS                     
SETUNRD  NTR1                                                                   
         L     R3,=A(MYIO)             CLEAR BILL HEADER BLOCK                  
***      A     R3,=F'4000'                                                      
         A     R3,=F'6000'                                                      
         USING BHBLOCK,R3                                                       
         LR    RE,R3                                                            
         LA    RF,BHBLENE                                                       
         XCEF                                                                   
         MVC   BHBLOCK,=C'**BBLK**'      BILL HEADER BLOCKS IN DUMP             
         MVC   BHDATA,=C'**BKEY**'                                              
         MVC   BHDOLS,=C'**BDOL**'                                              
         ZAP   BHBGRS,=P'0'                                                     
         ZAP   BHBNET,=P'0'                                                     
         ZAP   BHBACT,=P'0'                                                     
         MVC   BHSELCLI,=X'FFFF'      SET UNIT READ FLAG IN BLOCK               
         MVI   BHRD,0                 CLEAR BILL READ INDICATOR                 
         TM    NDRDBCEL,X'20'      SKIP IF NOT CASHFLOW                         
         BNO   *+8                                                              
         OI    NDRDBCEL,X'01'         SET TO READ UNIT BILL ELEMENTS            
         LA    RF,NETBLOCK            RESTORE NETBLOCK                          
         L     RE,=A(NETBLKSV)                                                  
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE  ((RF),(R1)),(RE)                                                 
         OI    NBSPLOPT,X'C0'      ALWAYS SPLIT UNITS IN BILL FLAVOR            
         MVI   NBFUNCT,NBFRDHI     READ HI TO RESTORE SEQ                       
* - IF FILTERING ON BILLING/ CLEAR FILTER FOR UNITS                             
* - AND RETURN AT REQLST FOR HEADLINE PRINT                                     
         OC    NBBILSTR,NBBILSTR   FILTERING ON BILLING                         
         BZ    XIT                                                              
         MVC   BILLFLT,NBBILSTR                                                 
         XC    NBBILSTR(4),NBBILSTR                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************                      
* READ INVOICE RECORDS                                                          
INVRDRTN NTR1                                                                   
         L     R3,=A(MYIO)                                                      
         GOTO1 AINVRD,DMCB,(RC),(R5),(R3),0     READ INVOICE RECORDS            
         L     R1,=A(HOOK)          RESET HOOK SINCE INVRD CHANGES IT           
         ST    R1,GLAHOOK                                                       
         XIT1                                                                   
                                                                                
                                                                                
                                                                                
                                                                                
* - WHEN READING PACKAGE RECORDS THESE ROUTINES FILL IN NETBLOCK                
*   WITH DATA EXPECTED BY NEWRIDRIVE ROUTINES                                   
                                                                                
FUDGUNT  NTR1                                                                   
*                                                                               
         TM    NBACCFLT,X'04'               FILL PACKAGE TABLE?                 
         BNO   FILPKGXX                                                         
         OC    NDAPKGTB,NDAPKGTB           BEEN THERE,DONE THAT?                
         BNZ   FILPKGX             YES                                          
         LA    R1,NDPKGTN          TOTAL NUMBEROF RECS                          
         LA    R4,NDPKGTLE         LENGTH OF RECORD                             
         MR    R0,R4                                                            
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,NDAPKGTB         PACKAGE TABLE                                
FILPKGX  EQU   *                                                                
         L     R4,NBAIO                                                         
         CLI   0(R4),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPKEY,R4                                                         
         L     R2,NDAPKGTB         ADDRESS OF TABLE                             
         USING NPKTBLD,R2                                                       
         LA    R3,NDPKGTN          MAX RECS IN TABLE                            
FILPKG03 CLI   0(R2),0                                                          
         BE    FILPKG10                                                         
         LA    R2,NDPKGTLE(R2)     LENGTH OF EACH ENTRY                         
         BCT   R3,FILPKG03                                                      
         DC    H'0'                MUST EXPAND TABLE                            
FILPKG10 MVC   0(9,R2),NPKAM       KEY- A/M,CLI,NET.EST,K                       
         MVC   NPKTNAME,NPAKNAME  NAME                                          
         MVC   NPKTCPM,NPAKGCPM   GUARCPM                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   FILPKGXX                                                         
         USING NPK2D,R4                                                         
         MVC   NPKTDEMO,NPK2PDEM      PKG DEMO                                  
FILPKGXX EQU   *                                                                
         DROP  R4                                                               
*                                                                               
         MVC   NBPACK,NBACTPAK     FUDGE UNIT PKG NUMB                          
*                                                                               
* - READ STATION RECORD FOR POSTING TYPE                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NBACTNET                                             
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,NBCLICOD                                                 
         BAS   R2,FUDHI            READ HI                                      
         CLC   KEY(15),0(RA)                                                    
         BE    FUD20                                                            
         MVC   STAKCLT,=C'000'     READ AGAIN CLEARING CLIENT                   
***      BAS   RE,FUDHI            COOL !!!!!                                   
         BAS   R2,FUDHI            COOL !!!!!                                   
         CLC   KEY(15),0(RA)                                                    
         BE    FUD20                                                            
         DC    H'0'                                                             
*                                                                               
FUDHI    L     RA,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'STATION',KEY,(RA),0                 
         BR    R2                                                               
*                                                                               
FUD20    LR    R4,RA                                                            
         MVC   NBPOSTYP,SPTYPE                POSTING TYPE                      
         MVC   KEY,NBKEY                   RESET NETIO SEQ                      
         GOTO1 HIGH                                                             
         DROP  R4                                                               
*                                                                               
FUDX     XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
ANETGOAL DS    F                   A(NETGOAL)                                   
         EJECT                                                                  
                                                                                
         LTORG                                                                  
GETBFRW  DS    CL500                   SAVE AREA FOR SFM BILL READ              
SPGKYSV  DS    CL12                                                             
         EJECT                                                                  
                                                                                
**************************************************************                  
MQ       USING MQMSGD,ELEM                                                      
MQCLOSE  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         MVC   MQ.MQHID,=CL6'SFTP2'                                             
         MVC   MQ.MQQUAL,=CL16'MEDIACOM-MEDIA'                                  
         MVC   MQ.MQDATA1(26),SPLFLTH-41                                        
         OC    MQ.MQDATA1(26),=CL27' '                                          
         MVC   MQ.MQFILE(L'MQFILENM-14),MQFILENM+14                             
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    MQCX                                                             
         DCHO                                                                   
MQCX     XIT1                                                                   
         DROP  MQ                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQQUAL   DS    CL16                QUALIFIER                                    
*MQCOUNT  DS    CL8                 RECORD COUNT                                
MQDATA1  DS    CL32                                                             
MQDATA2  DS    CL32                                                             
MQFILE   DS    CL64                DSN                                          
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
                                                                                
*******************************************************************             
                                                                                
         EJECT                                                                  
*          DATA SET SPWRI01    AT LEVEL 078 AS OF 11/28/07                      
*=================================================================*             
* MQRPTERR - ERROR HANDLER WHEN WRITING TO FILE                   *             
*                                                                 *             
* THIS CODE WILL INTERCEPT ERROR EXITS AND TEST WHETHER OUTPUT IS *             
* BEING WRITTEN TO A FILE.  IF NOT, IT WILL GO TO ERREX.  ELSE IT *             
* WILL SEND INFORMATION TO MQ ABOUT WHERE THE REPORT CAN BE FOUND *             
* AND THEN CONTINUE ON TO ERREX.                                  *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
T32020   CSECT                                                                  
*                                                                               
MQE      USING MQERRD,ELEM                                                      
*                                                                               
MQRPTERR NTR1  BASE=*,LABEL=*                                                   
         TM    NDINDS1,ND1WB       SEND OUTPUT DATA TO FILE?                    
         BZ    MQRPX                NO - JUST GO ON TO ERREX                    
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQEERRLQ-1),ELEM                                          
         MVC   MQE.MQEHID,=CL6'MEDERR'                                          
         MVC   MQE.MQEQUAL,=CL16'MEDIACOM'                                      
***      MVC   MQE.MQEID,WRIDESC                                                
         MVC   MQE.MQEID,SPLFLTH-41     THIS FIELD HAS NO LABEL                 
*                                       POINTS TO CL26 (DESCRIPTION?)           
         OC    MQE.MQEID,=CL27' '                                               
*                                                                               
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-TWATASK(RF)  POINT OT MASTC                          
         USING MASTD,RF                                                         
         MVC   MQE.MQECOD(3),MCREMPQK+2                                         
         MVI   MQE.MQECOD+3,C','                                                
         XR    RE,RE                                                            
         ICM   RE,3,MCREMPQK+5                                                  
         EDIT  (RE),(4,MQE.MQECOD+4),ALIGN=LEFT                                 
         DROP  RF                                                               
         MVC   MQE.MQEMSG(L'CONHEAD),CONHEAD                                    
         DROP  MQE                                                              
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQEERRLQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
*                                                                               
MQRPX    J     XIT                                                              
         LTORG                                                                  
*                                                                               
MQERRD   DSECT                                                                  
MQEHID   DS    CL6                 HUB RECORD ID                                
MQEQUAL  DS    CL16                QUALIFIER                                    
MQEID    DS    CL27                REQUEST ID                                   
MQECOD   DS    CL8                 ERROR CODE (OPT)                             
MQEMSG   DS    CL128               ERROR MESSAGE (OPT)                          
MQEERRLQ EQU   *-MQERRD                                                         
*                                                                               
T32020   CSECT                                                                  
*                                                                               
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,TWADCONS                                                   
         BNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,TMQRPT-TWADCOND(RF)                                        
         BNZ   *+6                                                              
         DCHO                                                                   
         ST    RF,AMQRPT                                                        
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         BRAS  RE,TESTRUN          IS THIS A TEST RUN?                          
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                                                         
         JE    XIT                                                              
         DCHO                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TESTRUN  DS    0H                                                               
         ICM   RF,15,TWAMASTC                                                   
         JNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,MCSSB-MASTD(RF)                                            
         JNZ   *+6                                                              
         DCHO                                                                   
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   NOTEST                                                           
         CLI   SSODSPAC-SSOOFF(RF),C'A'  IS THIS A TEST REQUEST?                
         JE    NOTEST                     NO                                    
         CLI   SSODSPAC-SSOOFF(RF),C' '  IS THIS A TEST REQUEST?                
         JNH   NOTEST                     NO                                    
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NOTEST   CR    RE,RB                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
T32020   CSECT                                                                  
*                                                                               
* READ B1X PROFILE TO SEE IF WE NEED BILL MAINT FORMULA                         
RDB1XPRF NTR1  BASE=*,LABEL=*                                                   
         CLC   NDPERCNT,=C'COST'                                                
         BNE   B1XEND                                                           
         OC    B1X,B1X                 HABE WE DONE THIS?                       
         BZ    B1X01                                                            
         CLC   B1XSV(2),NBSELAGY                                                
         BNE   B1X01                                                            
         CLC   B1XSV+2(1),NBSTATYP                                              
         BNE   B1X01                                                            
         CLC   B1XSV+3(3),NBCLICOD                                              
         BE    B1X10                                                            
*                                                                               
B1X01    XC    B1X,B1X                                                          
         MVC   B1X(4),=C'SB1X'                                                  
         NI    B1X,X'BF'          LOWER CASE                                    
         MVC   B1X+4(2),NBSELAGY     AGENCY                                     
         MVC   B1X+6(1),NBSTATYP        MEDIA                                   
         MVC   B1X+7(3),NBCLICOD       CLIENT                                   
         MVC   B1XSV,B1X+4                                                      
         GOTO1 NBGTPROF,DMCB,B1X,B1X+20,NBDM                                    
B1X10    CLI   B1X+31,C'Y'            SFM BILL FORMULA                          
         BNE   B1XEND                                                           
****->   CLI   B1X+32,C'N'             EFFECTIVE DATES?                         
****->   BNE   B1XEND                                                           
B1X20    DS    0H                      GET SFM BILL FORMULA                     
*                                                                               
         L     R4,=A(GETBFRW)      SPGETBFR WORK AREA                           
         USING SPGBFRD,R4                                                       
*                                                                               
         MVC   SPGBAM,NBACTAM                                                   
         MVC   SPGBCLT,NBACTCLI                                                 
*                                                                               
         CLI   NBSPLPR3,0          IF WE HAVE 3 CHAR CLI                        
         BNE   PRD15A              USE THAT                                     
*                                                                               
         L     RE,NBACLI                                                        
         USING CKEY,RE                                                          
         LA    RE,CLIST                                                         
         LA    RF,220                                                           
         MVI   BYTE,0                                                           
PRD10    CLC   NBSPLPRN,3(RE)                                                   
         BE    PRD15                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,PRD10                                                         
         CLI   BYTE,2              2ND TIME AROUND                              
         BE    B1XEND                                                           
         MVI   BYTE,2                                                           
         LA    RF,35                                                            
         LA    RE,CLIST2                                                        
         B     PRD10                                                            
*                                                                               
PRD15    MVC   WORK(3),0(RE)   SET CURRENT 3 CHAR PROD CODE                     
         B     PRDX                                                             
*                                                                               
PRD15A   MVC   WORK(3),NBSPLPR3  SET CURRENT 3 CHAR PROD CODE                   
*                                                                               
PRDX     EQU   *                                                                
         DROP  RE                                                               
*                                                                               
         MVC   SPGBPRD,WORK            3 CHAR PROD CODE                         
         MVC   SPGBEST,NBACTEST                                                 
         XC    SPGBMGR,SPGBMGR                                                  
         MVC   SPGBMGR(1),NBSTATYP        YES/USE UNIT'S MEDIA TYPE             
*                                                                               
         CLI   B1X+31,C'Y'         EFFECTIVE DATES?                             
         BNE   BC2A8                                                            
         LA    R2,NBACTDAT                                                      
         GOTO1 NDGETBM             RETURNS  M IN WORK+4/Y IN WORK+5             
         MVC   SPGBMOS+1(1),WORK+4                                              
         MVC   SPGBMOS(1),WORK+5                                                
         ZIC   R1,SPGBMOS           NBGETBM RETURNS 02/03 ETC IN YYMM           
         CHI   R1,27                SPGETBFR NEEDS 102 ETC IN BINARY            
         BH    BC2A8                                                            
         LA    R1,100(R1)                                                       
         STC   R1,SPGBMOS                                                       
*                                                                               
BC2A8    L     RE,=A(SPGKYSV)         DO WE HAVE BILL FORMULA?                  
         CLC   SPGBAM(12),0(RE)       DO WE HAVE BILL FORMULA?                  
         BE    BC2D                   YES                                       
         MVC   0(L'SPGKYSV,RE),SPGBAM         NO/SAVE KEY                       
         MVC   SPGBACOM,NBACOM        ADDRESS OF COMFACS                        
         MVC   SPGBLODR,NBLOADER      ADDRESS OF LOADER                         
         GOTO1 =V(SPGETBFR),DMCB,SPGBFRD                                        
         XC    WORK(5),WORK                                                     
BC2D     MVC   WORK(5),SPGBFORM                                                 
         MVI   B1XFLAG,C'Y'                                                     
         DROP  R4                                                               
B1XEND   EQU   *                                                                
         XIT1                                                                   
*                                                                               
B1X      DS    CL50                                                             
B1XSV    DS    CL6                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE NENETPUPD                                                      
         EJECT                                                                  
*                                                                               
* AUTHORIZED AGENCY TABLE - AGY / REQUEST ID / ADDRESS OF LIST                  
         DS    0F                                                               
AGYTABL  DC    CL4'AT',CL4'ATT',A(ATTLIST)                                      
         DC    CL4'AT',CL4'BCS',A(BCSLIST)                                      
         DC    CL4'AT',CL4'YAL',A(YALLIST)                                      
         DC    CL4'NW',CL4'YAL',A(YALLIST)                                      
         DC    CL4'BJ',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'BD',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'CE',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'FC',CL4'FCB',A(FCBLIST)                                      
         DC    CL4'FC',CL4'FC1',A(FC1LIST)                                      
         DC    CL4'FC',CL4'FC2',A(FC2LIST)                                      
*                                                                               
         DC    CL4'TH',CL4'RES',A(RESLIST)                                      
         DC    CL4'TH',CL4'GI2',A(GI2LIST)                                      
         DC    CL4'TH',CL4'LEX',A(LEXLIST)                                      
         DC    CL4'TH',CL4'TMS',A(TMSLIST)                                      
         DC    CL4'JW',CL4'JOM',A(JOMLST)                                       
         DC    CL4'OM',CL4'JOM',A(JOMLST)                                       
         DC    CL4'JW',CL4'OJC',A(OJCLST)                                       
         DC    CL4'OM',CL4'OJC',A(OJCLST)                                       
         DC    CL4'JW',CL4'OJX',A(OJCLST)                                       
         DC    CL4'OM',CL4'OJX',A(OJCLST)                                       
         DC    CL4'OM',CL4'OMS',A(OMSLST)                                       
         DC    CL4'H7',CL4'OMS',A(OMSLST)                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*                                                                               
         DS    0H                                                               
       ++INCLUDE NETAGYLST                                                      
*                                                                               
NETBLKSV DS    CL2000                                                           
         EJECT                                                                  
* READ SPGENTAL RECORDS AND FILL TIME+TALENT DOLLAR TABLE                       
TMTLTBL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,NDTIMTAL                                                      
         USING TIMTALD,R2                                                       
         CLC   NBACTCLI,0(R2)      ALREADY FILLED                               
         BE    TMTLX                                                            
*                                                                               
         L     RE,NDTIMTAL           CLEAR TABLE                                
         L     RF,=F'2814'                                                      
         XCEF                                                                   
         LA    R5,200              COUNTER MAX RECS IN TABLE                    
         MVC   0(2,R2),NBACTCLI    SET CLIENT FOR DUMPS ETC                     
         LA    R2,TTLENE(R2)           BUMP TO NEXT ENTRY                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TALREC,R4                                                        
         MVC   TALKTYP,=X'0D27'                                                 
         MVC   TALKAGMD,NBACTAM                                                 
         MVC   TALKCLT,NBACTCLI                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     TMTL05                                                           
*                                                                               
TMTL03   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
TMTL05   CLC   KEY(5),KEYSAVE      STILL SAME CLIENT?                           
         BNE   TMTLX               NO- THAT'S ALL                               
         L     R3,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,(R3),MYDMW           
         LA    R3,24(R3)           JUMP TO 1ST ELEM                             
         USING TALEL05,R3                                                       
TMTL10   CLI   0(R3),X'05'         TALENT FACTOR                                
         BE    TMTL20                                                           
         ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R1                                                            
         B     TMTL10                                                           
TMTL20   MVC   TTNET,NTALKNET      NETWORK                                      
         MVC   TTDATE,NTALKDT      DATE                                         
         MVC   TTMULT,TAL05MUL     MULTIPLIER                                   
         MVC   TTDIV,TAL05DIV      DIVSOR                                       
         LA    R2,TTLENE(R2)                                                    
         BCT   R5,TMTL03                                                        
         DC    H'0'                INCREASE TABLE                               
TMTLX    XIT1                                                                   
         DROP  R2,R3,R4                                                         
*                                                                               
         DS    0D                                                               
MYDMW    DS    CL96                                                             
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* STATION BUCKETS READ AT NBVALCLI - THIS IS TO FACILITATE CLIENT               
* FILTERING WITH CLIENT LIST ETC                                                
STABRD   NTR1  BASE=*,LABEL=*                                                   
         CLC   STABCLSV,NBACTCLI   DID WE DO THIS ALREADY ?                     
         BE    STABRDXX            YES/EXIT                                     
         MVC   STABCLSV,NBACTCLI   SAVE CLIENT                                  
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STABUCK,R2                                                       
         MVC   STABPCOD,=X'0E81'                                                
         MVC   STABPAM,NBACTAM       AGENCY/MEDIA                               
***      CLC   =C'ALL',NBSELCLI                                                 
***      BE    *+10                                                             
         MVC   STABPCLT,NBACTCLI     CLIENT                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     STABRD5                                                          
STABSEQ  DS    0H                                                               
         LA    R2,KEY                                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
STABRD5  CLC   KEY(3),KEYSAVE  AGY/MED                                          
         BNE   STABRDX                                                          
***      CLC   =C'ALL',NBSELCLI                                                 
***      BE    STABRD7                                                          
         CLC   KEY(5),KEYSAVE      AGY/MED/CLI                                  
         BNE   STABRDX                                                          
STABRD7  CLI   NBSELEST,0      EST FILTER?                                      
         BE    STABRD20        NO                                               
         CLI   NBSELESE,0      EST RANGE ?                                      
         BNE   STABRD10                                                         
*                                                                               
         CLC   NBSELEST,STABPEST    MATCH EST FILTER ?                          
         BNE   STABSEQ                                                          
         B     STABRD20                                                         
*                                                                               
STABRD10 CLC   STABPEST,NBSELEST   EST RANGE                                    
         BL    STABSEQ                                                          
         CLC   STABPEST,NBSELESE                                                
         BH    STABSEQ                                                          
STABRD20 DS    0H                                                               
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    STABRD30                                                         
         CLC   NBSELPRD,=C'POL'                                                 
         BE    STABRD30                                                         
         CLC   NBSELPRD,STABPPRD                                                
         BNE   STABSEQ                                                          
STABRD30 DS    0H                                                               
         MVC   NBACTEST,STABPEST    SET ESTIMATE TO NETBLOCK                    
         MVC   FULL,STABPPRD        SAVE PRODUCT !!!! IN FULL                   
         MVC   FILENAME,=C'SPTFILE '                                            
         DROP  R2                                                               
         L     R2,=A(STABRDIO)                                                  
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,(R2),(0,DMWORK)          
*                                                                               
         LR    R3,R2                                                            
         USING STABELEM,R3                                                      
         LA    R3,24(R3)                                                        
         CLI   0(R3),X'0E'                                                      
         BE    DATCONVT                                                         
STABNXTE ZIC   R1,1(R3)            GET NEXT STAB ELEMENT                        
         LTR   R1,R1                                                            
         BZ    STABSEQ                                                          
         AR    R3,R1                                                            
         CLI   0(R3),X'0E'                                                      
         BH    STABSEQ                                                          
         BL    STABNXTE                                                         
*                                                                               
*                             CONVERT BINARY(YYMM) TO COMPRESSED                
* NOTE THAT STAB RECS HAS STABPER=YYMM, STABBDT=2 BYTE COMPRESSED               
*                                                                               
DATCONVT CLI   NBSELMFL,0                                                       
         BE    *+14                                                             
         CLC   NBSELMFL,STABSTYP                                                
         BNE   STABNXTE                                                         
*                                                                               
         MVI   WORK+2,X'01'                                                     
         MVC   WORK(2),STABPER                                                  
         GOTO1 DATCON,DMCB,(3,WORK),(2,WORK+50)    WORK+50=BILLPERIOD           
*                                                                               
         CLC   WORK+50(2),NBCMPSTR         CHK DATE RANGE                       
         BL    STABNXTE                                                         
         CLC   WORK+50(2),NBCMPEND                                              
         BH    STABNXTE                                                         
*                                                                               
*******  FILL NETBLOCK ****************************************                 
         MVI   WORK+2,X'0A'        **  USE NBACTDAT  **                         
         MVC   WORK(2),STABPER     ** USED BY MONTH KEYWORD **                  
         GOTO1 DATCON,DMCB,(3,WORK),(2,NBACTDAT)                                
*                                                                               
         MVC   NBSTATYP,STABSTYP   STATION TYPE                                 
*                                                                               
         MVC   NBSPLPR3,FULL       PROD SAVED IN FULL ABOVE                     
*                                                                               
*                                                                               
         MVC   NDMANG,STABGRS      NOTE USING NEDRVBLKD                         
         MVC   NDMANN,STABNET                                                   
*                                                                               
         MVC   STABKYSV,KEY                    SAVE KEY                         
         OI    NDINDS1,ND1NOAC     NO ACCGEN DOLLARS                            
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)    SEND TO DRIVER                             
         MVC   KEY,STABKYSV                   RESET KEY                         
         MVC   FILENAME,=C'SPTDIR  '          RESET FILENAME                    
         GOTO1 HIGH                           RESET SEQ IN CASE CHAGED          
*                                             IN NEWRIDRIVE                     
         B     STABNXTE                                                         
*                                                                               
STABRDX  XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         XC    NDMANN,NDMANN                                                    
         XC    NDMANG,NDMANG                                                    
         XC    NBACTDAT,NBACTDAT                                                
         NI    NDINDS1,X'FF'-ND1NOAC     RESET ACCGEN DOLLARS                   
STABRDXX XIT1                                                                   
         LTORG                                                                  
STABCLSV DS    CL2                                                              
STABKYSV DS    CL13                                                             
                                                                                
         EJECT                                                                  
INITOFLN NTR1  BASE=*,LABEL=*                                                   
         L     RE,=A(XPRDMSK)      NEEDED IF REQUEST PRD GROUPS                 
         ST    RE,NBADRPRG                                                      
         L     RE,=A(PRODTBL)      PRODTBL FOR NETVALUE/NEPRDACC                
         ST    RE,NBAPROD                                                       
         L     RE,=A(PRODTBL2)     PRODTBL(CL18)= BILLED PROD NOT ON            
         STCM  RE,15,NBSPLBPN                     UNIT                          
*                                                                               
         L     RE,=A(MULTPLST)     3 CHAR PROD LIST = NBPRDLST                  
*                                  6 PRODS X 3 CHAR = 18                        
         STCM  RE,15,NBADPLST                                                   
         L     R1,=A(STWRDLST)     STEWARD LIST                                 
         ST    R1,NBASTWRD                                                      
         L     R1,TWADCONS             SET THE ADDRESS OF LOADER                
         USING TWADCOND,R1                                                      
         L     R1,TLOADER                                                       
         ST    R1,NBLOADER                                                      
         DROP  R1                                                               
         GOTO1 =A(OVERFLW),DMCB,(RC),10,RR=YES      LOAD PROGRAMS               
         OC    NBANBUFF,NBANBUFF     AND THESE/NO ROOM IN OVERFLW               
         BNZ   AAAA                  LOAD JUST ONCE                             
         GOTO1 CALLOV,DMCB,X'B4000000',0,0          LOAD T320B4                 
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NDARDAOR,0(R1)                                                   
         GOTO1 CALLOV,DMCB,X'B3000000',0,0          LOAD T320B3                 
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             GET START OF PHASE                           
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,NBANBUFF         NETLIST                                      
         OI    NBINDS7,NBI7NTIO    XTENDED STATION BUFFER                       
         LA    R1,4008(R1)                                                      
         LA    R1,2000(R1)                                                      
         ST    R1,NBCNVNTI         STALIST  7                                   
*        LA    R1,2008(R1)                                                      
         LA    R1,3008(R1)                                                      
         ST    R1,NBAPLIST         GRPLIST                                      
         LA    R1,2568(R1)                                                      
         ST    R1,NDAUCOM          UCOM TABLE                                   
         LA    R1,508(R1)                                                       
         ST    R1,NDANINV          INVOICE TABLE                                
         LA    R1,108(R1)                                                       
         ST    R1,NDTIMTAL                                                      
*                               VIRTUAL UNIT AREA                               
         L     R1,=F'34000'     ,WBFLT=34                                       
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,NBWBFLT          34,000 FOR FLIGHTS                           
*                                                                               
AAAA     L     R1,=A(UDEFLST)                                                   
         ST    R1,NDUDEFD          PASS USER DEF DATA AREA OFFLINE              
         L     R1,=A(GLRGXTN)                                                   
         ST    R1,NDAGLARG         PASS GLARGS EXTENSION OFFLINE                
         L     R1,=A(INVFLT)                                                    
         STCM  R1,15,NBINVFLT      PASS INVOICE FILTERS OFFLINE                 
         L     R1,=A(CSHIERC)                                                   
         ST    R1,NDACSHRC         PASS DDCASHIER DATA AREA                     
         L     R1,=A(STAGRPTB)                                                  
         STCM  R1,15,NBNETGRP      PASS STATION GROUP LIST OFFLINE              
         L     R1,=A(PRGBUFF)                                                   
         STCM  R1,15,NBAPBUFF      PASS PRG BUFF                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
         PRINT GEN                                                              
HOOK     NTR1  BASE=*,LABEL=*                                                   
         PRINT NOGEN                                                            
                                                                                
*                                                                               
         CLI   GLHOOK,GLFIRST                                                   
         BNE   HOOK1                                                            
         ICM   RF,15,NDAPQIND                                                   
         BZ    HOOK1                                                            
         USING PQINDEX,RF                                                       
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,GLARGS                                                      
         BZ    HOOK1               SKIP LEVEL 0,1 BREAKS                        
         CHI   R1,1                (SINCE NET USES LEVEL1 BREAK                 
         BE    HOOK1                AS INDICATOR FOR DETAILRECS)                
         AHI   R1,-2                                                            
         MHI   R1,PQINDXEQ                                                      
         AR    RF,R1                                                            
                                                                                
         OI    PQSTATUS,PQCHG      SET CHANGE                                   
         L     RE,GLADTENT                                                      
         MVC   PQAOUT,DROAPOS-DROD(RE)                                          
         B     HOOKX                                                            
         DROP  RF                                                               
*                                                                               
                                                                                
HOOK1    CLI   GLHOOK,GLHEAD                                                    
         BNE   HOOK2                                                            
         GOTO1 NDGENHED                                                         
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK2    CLI   GLHOOK,GLPRINT                                                   
         BNE   HOOKX                                                            
HOOK2A   DS    0H                                                               
                                                                                
         TM    NDLININD,X'80'                                                   
         BNO   *+16                                                             
         MVI   GLHOOK,GLDONT                                                    
         MVI   NDLININD,0          RESET LINE INDICATORS                        
         B     HOOKX                                                            
*                                                                               
         CLI   FIRSTLIN,C'Y'                                                    
         BNE   PRINT19                                                          
*                                                                               
         CLC   =C'TRANSMIT',CONREC                                              
         BNE   GENEDX                                                           
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
*******  CLI   OFFLINE,C'Y'                                                     
*******  BNE   GENEDX                                                           
*******  L     R7,ATWA                                                          
*******  USING CONHEADH-64,R7                                                   
         LA    R2,KEY              READ DESTINATION ID RECORD                   
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         L     R1,TWAMASTC                                                      
         MVC   CTIKNUM,MCDESTID-MASTD(R1)                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'CTIKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         SR    R0,R0               FIND DESCRIPTION ELEMENT                     
         LA    R1,CTIDATA                                                       
*                                                                               
GENED2   DS    0H                                                               
****     BNE   *+6                                                              
*****    DC    H'0'                                                             
         CLI   0(R1),CTDSCELQ                                                   
         BNE   GENED4                                                           
         CLI   1(R1),12                                                         
         BNE   GENED4                                                           
         MVC   WORK(10),CTDSC-CTDSCD(R1)  EXTRACT DESTINATION ID NAME           
         B     GENED6                                                           
*                                                                               
GENED4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GENED2                                                           
*                                                                               
GENED6   MVI   P,X'40'             *HDR* CARD                                   
         MVC   P+1(L'P-1),P                                                     
         LA    R1,P                                                             
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
         MVC   15(10,R1),WORK      DESTINATION                                  
         MVI   34(R1),C'W'         132 CHARS WIDE                               
***->    MVI   36(R1),C'S'         STRIP DOUBLEQUOTES                           
         MVC   38(10,R1),WORK      FORMATTED DESTINATION NAME                   
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVI   P,C' '              ++DDS TRN CARD                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P(14),=CL14'++DDS NEW  TRN'                                      
         MVC   P+9(2),NBSELAGY                                                  
         LA    R1,P+15                                                          
         USING SPEDICTD,R1                                                      
         MVI   SPWRTYPE,SPWRDATQ                                                
         MVC   SPWRNAME,NBSELAGY                                                
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS CARD  FOR BDE                          
         MVC   P(14),=CL14'++DDS      SUB'                                      
         MVC   P+15(34),SPLTITL                                                 
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS FTP CARD                               
         MVC   P(14),=CL14'++DDS      FTP'                                      
*                                                                               
         MVC   P+15(8),SPLNAM      PROVIDE FORMAT NAME                          
*                                                                               
         CLC   SPLNAM,SPACES       IF NO FORMAT NAME                            
         BH    *+16                                                             
         MVC   P+15(3),REMUSER        REPLACE WITH REQUESTOR INITIALS           
         MVC   P+18(5),SPACES         CLEAR END OF FIELD                        
*                                                                               
         OC    P+15(8),SPACES      MAKE UPPERCASE                               
*                                                                               
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
******   LA    R1,RELO2                                                         
******   S     R1,RELO2                                                         
         L     RF,=A(TRTTABLE)                                                  
******   AR    RF,R1               RELOACTE ADDRESS                             
*                                                                               
         TRT   P+15(8),0(RF)       MAKE SURE ALL CHARS ARE ALLOWED              
         BZ    *+10                OKAY                                         
         MVC   P+15(8),=CL8'BADID'                                              
*                                                                               
         CLI   P+15,C'0'           FIRST CHAR CAN'T BE NUMERIC                  
         BL    *+8                                                              
         MVI   P+15,C'Z'           REPLACE WITH Z                               
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
GENEDX   EQU  *                                                                 
*                                                                               
* IF PQIX=Y, PRINT INDEX HEADER                                                 
         ICM   R3,15,NDAPQIND                                                   
         BZ    PRINTX              NO                                           
****     MVC   WORK(200),0(R3)                                                  
****     DC    H'0'                                                             
         USING PQINDEX,R3                                                       
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         LA    R0,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
*                                                                               
         MVC   P(06),=C'<DECL>'    START OF HEADING DECLARATIONS                
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(09,R2),=C'<REQNAME '                                           
         AHI   R2,9                                                             
         ZIC   RE,SPLNAMH+5                                                     
         LTR   RE,RE                                                            
         BZ    PRINT10                                                          
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPLNAM                                                   
         LA    R2,1(RE,R2)                                                      
PRINT10  MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IH '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT11  OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT12              NO                                          
         CLI   PQPOSO,C'H'         IS THIS A HEADLINE?                          
         BNE   PRINT12              NO - CLOSE & LOOK FOR MIDS & ROWS           
         BRAS  RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT11                                                       
*                                                                               
PRINT12  DS    0H                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IM '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT13  CLI   PQPOSO,C'C'         COLUMN DATA?                                 
         BE    PRINT14              YES - CLOSE MIDS & GET COLS                 
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT14              NO                                          
         BRAS  RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT13                                                       
*                                                                               
PRINT14  DS    0H                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IC '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT15  OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT16              NO                                          
         BRAS  RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT15                                                       
*                                                                               
PRINT16  MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         CLI   DOWNOPT,0           DOWNLOAD?                                    
         BE    PRINT17              NO                                          
         MVC   P(10),=C'<FMT DATA>'                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         B     PRINT18                                                          
*                                                                               
PRINT17  MVC   P(04),=C'<HL '      NUMBER OF HEADLINES                          
         EDIT  NDLASTHD,(2,P+4),FILL=0                                          
         MVI   P+6,C'>'                                                         
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
PRINT18  MVC   P(07),=C'</DECL>'   END OF HEADING DECLARATIONS                  
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
PRINT19  CLI   DOWNOPT,0           IF WE ARE DOWNLOADING, NO DATA LINES         
         BNE   PRINTX                                                           
         ICM   R3,15,NDAPQIND                                                   
         BZ    PRINTX                                                           
         USING PQINDEX,R3                                                       
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         LA    R2,P                                                             
*                                                                               
         MVI   HALF,0                                                           
         LA    R1,1                                                             
         LA    R0,MAXHEADS         INDEX LINES ONLY FOR HEAD CHANGES            
         LA    R2,P                                                             
*                                                                               
PRINT20  CLI   PQPOSO,C'H'         INDEX LINES ONLY FOR HEAD CHANGES            
         BNE   PRINT30                                                          
         TM    PQSTATUS,PQCHG      HAS THIS KEY CHANGED?                        
         BNZ   PRINT40              YES - PRINT IT                              
*                                                                               
PRINT30  AHI   R1,1                                                             
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT20                                                       
         B     PRINT60                                                          
*                                                                               
PRINT40  NI    PQSTATUS,X'FF'-PQCHG       RESET CHANGE FLAG                     
         CLI   HALF,0              ANY ENTRIES YET?                             
         BNE   PRINT50              YES                                         
         MVI   HALF,1              SET HAVE ONE NOW                             
         MVC   0(06,R2),=C'<DATA '                                              
         AHI   R2,6                                                             
         LR    RE,R0                                                            
         EDIT  (R1),(2,0(R2)),FILL=0                                            
         LR    R0,RE                                                            
         MVI   2(R2),C'='                                                       
         AHI   R2,3                                                             
*                                                                               
PRINT50  DS    0H                                                               
                                                                                
**       LR    R0,R2                                                            
**       L     R2,PQAOUT                                                        
**       DC    H'0'                                                             
**       LR    R2,R0                                                            
                                                                                
         L     RE,PQAOUT           GET A(OUTPUT SORT AREA)                      
         AHI   RE,L'LABLAREA+1     RE=CODE PORTION                              
         LA    RF,L'CODEAREA                                                    
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE DATA TO PRINT LINE                      
*                                                                               
         LA    R2,1(RF,R2)         GET LAST USED PRINT POSN                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'         SEMI-COLON                                   
         AHI   R2,2                                                             
         B     PRINT30                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
PRINT60  CLI   HALF,1                                                           
         BNE   PRINTX                                                           
         MVI   0(R2),C'>'                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
PRINTX   DS    0H                                                               
         MVI   FIRSTLIN,C'N'                                                    
         B     HOOKX                                                            
***RELO2    DC    A(*)                                                          
                                                                                
FIRSTLIN DC    C'Y'                                                             
DOWNOPT  DS    CL1                                                              
LABLAREA DS    CL15             ** MUST MATCH NEWRIDRIVE STORAGE                
         DS    CL1              **                                              
CODEAREA DS    CL10             **                                              
*                                                                               
*                                                                               
HOOKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                 CHECK IF ACCMON OTION -                       
*                                 NEED FLAG FOR BUILDING DATES                  
CHKACMN  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,RELO2                                                         
         S     R4,RELO2                                                         
         L     R3,=A(MYIO)                                                      
         AR    R3,R4                                                            
         GOTO1 SCANNER,DMCB,SPLOPTH,(7,0(R3)),0                                 
         ZIC   R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BZ    ENDSCAN                                                          
SCN3     CLC   =C'ACCMON',12(R3)                                                
         BNE   SCN5                                                             
         OI    NDINDS1,X'02'        ACCMON KEYWORD                              
         B     ENDSCAN                                                          
SCN5     LA    R3,32(R3)                                                        
         BCT   R4,SCN3                                                          
ENDSCAN  XIT1                                                                   
*                                                                               
RELO2    DC    A(*)                                                             
         LTORG                                                                  
         EJECT                                                                  
*              INITIALIZE FAX DATA                                              
         SPACE 1                                                                
INITFAX  NTR1  BASE=*,LABEL=*                                                   
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         L     R1,TFAXINFO                                                      
         USING FAXINFOD,R1                                                      
         MVC   FXITNCLI,SPLCLI                                                  
         MVC   FXITNEST,SPLEST                                                  
         CLI   FXITNEST+2,C','                                                  
         BNE   *+8                                                              
         MVI   FXITNEST+2,X'40'                                                 
         MVC   FXITNNET,SPLNET                                                  
         MVC   FXITNPAK,SPLPAK                                                  
         MVC   FXITNPRG,=C'WR'                                                  
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FORMAT INDEX DATA                                                             
*   INPUT    R2 = A(FORMAT AREA)                                                
*            R3 = A(PQINDEX ENTRY)                                              
*            R4 = A(SPOOL)                                                      
*   RETURN   FORMATTED DATA, FOLLOWED BY A SEMI-COLON                           
*            R2 = A(NEXT BLANK SPACE)                                           
*                                                                               
IXFMT    NTR1  BASE=*,LABEL=*                                                   
         USING PQINDEX,R3                                                       
         USING SPOOLD,R4                                                        
         MVC   0(8,R2),PQKEYWRD                                                 
         AHI   R2,8                                                             
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         CLC   PQHEAD1(96),SPACES                                               
         BNH   IX40                                                             
         CLC   =C'<IC ',P                                                       
         BE    IX40                                                             
         LA    R1,PQHEAD1                                                       
         LA    RE,96                                                            
         MVC   1(2,R2),=C'="'                                                   
         AHI   R2,3                                                             
         LA    R0,4                MAX HEADERS                                  
*                                                                               
IX10     MVC   0(24,R2),0(R1)      MOVE OUT HEADER                              
         AHI   R2,24               END OF HEADER                                
         CLI   0(R2),C' '          BACK UP TO LAST CHAR                         
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C'"'          CLOSE QUOTE                                  
         AHI   R2,1                                                             
         BCT   R0,*+8                                                           
         B     IX40                                                             
*                                                                               
         AHI   R1,24               NEXT HEADER                                  
         AHI   RE,-24              L'REMAINING HEADERS                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES      ANY LEFT?                                    
         BNH   IX40                                                             
         MVC   1(2,R2),=C',"'                                                   
         AHI   R2,3                                                             
         B     IX10                                                             
*                                                                               
IX40     MVI   1(R2),X'5E'         SEMICOLON                                    
         AHI   R2,2                                                             
IXX      XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* IF RUNNING IN COMMERCIAL ROTATION MODE                                        
* PASS PROGRAM RECORDS TO DRIVER HERE                                           
* WE ARE USING X'84' KEY IN THIS MODE WITH PROGRAM HIGH                         
*                                                                               
DOCOMRTN NTR1  BASE=*,LABEL=*                                                   
         CLC   PROGSV,NBACTPRG         HAS PROGRAM CHANGED                      
         BE    DOCOMX                  NO/EXIT                                  
         MVC   PROGSV,NBACTPRG         SET NEW PROGRAM                          
         XC    PRGHOURS,PRGHOURS       CLEAR TABLE                              
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NPGKEY,R2                                                        
         MVC   0(2,R2),=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,BINMKT                                                   
         MVC   NPGKPROG,NBACTPRG                                                
         MVC   NPGKEND,NBACTDAT                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                      MUST BE THERE                          
         DROP  R2                                                               
         L     R3,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,(R3),MYDM            
         USING NPGEL92,R3                                                       
         LA    R3,24(R3)                                                        
DOCOM20  CLI   0(R3),X'92'                                                      
         BE    DOCOM25                                                          
         ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                    MUST BE THERE                            
         AR    R3,R1                                                            
         B     DOCOM20                                                          
*                                                                               
* DIVIDE MILITARY START BY 100 AND CHECK REMAINDER FOR HOW                      
* TO CALCULATE NEXT FIRST 1/2 HOUR                                              
*                                                                               
DOCOM25  DS    0H                  BREAK OUT IN 1/2 HOUR INTERVALS              
         MVC   PRGSTART,NPGTIME     SAVE START MILITARY TIME                    
         MVC   PRGEND,NPGTIME+2    SAVE END MILITARY TIME                       
*  ADD 2400 IF LESS THAN 0600                                                   
         SR    R1,R1                                                            
         ICM   R1,3,PRGSTART                                                    
         C     R1,=F'600'                                                       
         BNL   DOCOM26                                                          
         AHI   R1,2400                                                          
         STCM  R1,3,PRGSTART                                                    
DOCOM26  ICM   R1,3,PRGEND                                                      
         C     R1,=F'600'                                                       
         BNL   DOCOM26B                                                         
         AHI   R1,2400                                                          
         STCM  R1,3,PRGEND                                                      
*                                                                               
DOCOM26B SR    R1,R1                  GET NXT 1/2 HOUR                          
         ICM   R1,3,PRGSTART                                                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         C     R0,=F'0'                IF 0 REMAINDER                           
         BNE   DOCOM27                 STARTS ON HOUR                           
         LA    RE,30                   BUMP BY 1/2 HOUR                         
         B     DOCOM30                                                          
DOCOM27  C     R0,=F'30'               IF 30 REMAINDER                          
         BNE   DOCOM28                 STARTS ON 1/2 HOUR                       
         LA    RE,70                   BUMP TO NEXT HOUR                        
         B     DOCOM30                                                          
DOCOM28  C     R0,=F'30'               IF LESS THAN 30                          
         BNL   DOCOM29                                                          
         LA    RE,30                   PUT 30 INTO RE                           
         SR    RE,R0                   SUBTRACT START MINUTES                   
         B     DOCOM30                                                          
DOCOM29  LA    RE,100                   GREATER THAN 30                         
         SR    RE,R0                   SUBTRACT MINUTES FROM 100                
         B     DOCOM30                                                          
* AT THIS POINT RE CONTAINS MINUTES TO ADD TO START TIME                        
* TO GET THE NEXT HALF HOUR                                                     
DOCOM30  SR    R1,R1                                                            
         ICM   R1,3,PRGSTART        START TIME                                  
         AR    R1,RE               +  MINUTES TO NEXT HOUR OR 1/2 HR            
         STCM  R1,3,PRGHLFHR        SET IN NEXT PROGRAM 1/2 HOUR                
DOCOM40  LA    R1,PRGHOURS         TABLE OF PROGRAM BY 1/2 HOUR                 
DOCOM41  OC    0(4,R1),0(R1)                                                    
         BE    DOCOM45                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   DOCOM41                                                          
         DC    H'0'                                                             
DOCOM45  MVC   0(4,R1),PRGSTART       SET START/NXT HALF HOUR TO TABLE          
         MVC   NBBILTGR,PRGSTART      USE $ FIELDS TO PASS DATA                 
         OI    NBINDS3,NBI3CMPR    PASSING PROGRAM DATA                         
*                                                                               
         MVI   GLMODE,GLINPUT       PASS TO DRIVER                              
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         CLC   PRGHLFHR,PRGEND         HAVE WE REACHED END?                     
         BE    DOCOMX                  YES                                      
         BH    DOCOMX                  YES                                      
*                                      NO-BUMP TO NXT 1/2/HOUR                  
         MVC   PRGSTART,PRGHLFHR       SET NXT AS START                         
         ZICM  R1,PRGHLFHR,2         GET NEXT HALF HOUR                         
         SR    R0,R0                                                            
         D     R0,=F'100'              DIVIDE MILITARY TIME BY 100              
         LTR   R0,R0                   IF NO REMAINDER                          
         BNE   DOCOM50                                                          
         ICM   R1,3,PRGHLFHR           BUMP BY 30 TO NXT 1/2 HOUR               
         LA    R1,30(R1)                                                        
         STCM  R1,3,PRGHLFHR                                                    
         B     DOCOM55                                                          
DOCOM50  ZICM  R1,PRGHLFHR,2         IF REMAINDER                               
         LA    R1,70(R1)               BUMP TO NXT MILITARY HOUR                
         STCM  R1,3,PRGHLFHR                                                    
*                                                                               
DOCOM55  CLC   PRGHLFHR,PRGEND         COMPARE NXT TO END                       
         BNH   DOCOM40                 ADD TO TABLE                             
         MVC   PRGHLFHR,PRGEND         SET END AS NXT                           
         B     DOCOM40                                                          
DOCOMX   XIT1                                                                   
         EJECT                                                                  
* FOR COMMERCIAL ROTATION MODE                                                  
PRGSTART DS    CL2                     PROGRAM START MILITARY TIME              
PRGHLFHR DS    CL2                     CALCULATED NXT 1/2 HOUR                  
PRGEND   DS    CL2                     PROGRAM END MILITART TIME                
PROGSV   DS    CL6                     PROGRAM SAVE                             
*                                                                               
PRGHOURS DS    CL192                  START-END TIME BY 1/2 HOUR                
         DC    4X'FF'                                                           
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
*                                                                               
* - DO NOT USE R5 - DRIVER CALLS A HOOK IN 1ST NMOD AND THIS                    
*                   IS DEPENDENT ON R5                                          
*                                                                               
OVERFLO  NMOD1 0,*NE20OF*,R8                                                    
         L     RC,0(R1)                                                         
         TM    NBINDS6,NBI6XDEM     EXPANDED DEMOS?                             
         BO    DODEMEXP            YES                                          
         B     DODEMEXT                                                         
EXXT     XIT1                                                                   
*                                                                               
******************************************************************              
*                                                                               
* GET DEMOS INTO DEMO EXTENSION AREA                                            
*                                                                               
* IF NOT DOING RAW ETC  RAW=EQU=NBDEMOS                                         
* CURRENTLY IF YOU ASK FOR RAW OR EQU YOU GET BOTH                              
* AND IF YOU ASK FOR NN OR NQ YOU GET BOTH                                      
* I.E. YOU CAN NOT GET PROFILE DEMO VALUE IF YOU ASK                            
* FOR ONE OF THE RAW OR EQU FLAVORS - WE COULD DO THIS                          
* BY FORCING THE REUESTOR TO ASK FOR 'P' FOR PROFILE -                          
*                                                                               
*                                                                               
*******************************************************************             
DODEMEXT DS    0H                                                               
         L     R3,NDARAWEX         COPY DEMOS TO BLOCK                          
         USING RAWDATA,R3                                                       
         MVI   NDDEMSV,0                                                        
                                                                                
         TM    NBINDS,X'40'            YES -GREATER IMP                         
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
*                                                                               
* SET UP VANILLA FLAVOR *          ALWAYS SET UP VANILLA FLAVOR                 
         MVC   RWESTHOM,NBESTHOM   RAW=EQU=NBDEMO VALUES                        
         MVC   RWACTHOM,NBACTHOM                                                
         MVC   RQESTHOM,NBESTHOM                                                
         MVC   RQACTHOM,NBACTHOM                                                
         MVC   RWESTDEM,NDESTDEM                                                
         MVC   RWACTDEM,NDACTDEM                                                
         MVC   RQESTDEM,NDESTDEM                                                
         MVC   RQACTDEM,NDACTDEM                                                
*                                                                               
         TM    NDANYDEM,NN+NQ      IF THIS                                      
         BNZ   SKPTHS              SKIP EFFICIENCY FOR NOW                      
         TM    NDANYDEM,RAW+EQU    IF RAW/EQU - HAVE ALREADY DONE RAW           
         BNZ   DODEM10                          GO GET EQU                      
SKPTHS   EQU   *                                                                
*                                                                               
         TM    NDANYDEM,X'20'      ARE WE DOING RAW ETC?                        
         BO    CHKFLAV             YES                                          
         B     DODEMXIT            NO AND THAT'S ALL                            
*                                                                               
CHKFLAV  TM    NDANYDEM,RAW+EQU+NN+NQ    SPECIFIC FLAVORS REQUESTED?            
         BNZ   DOFLAVS                   YES                                    
                                                                                
*                                        NO                                     
* NDANYDEM SET TO X'20' BUT SPECIFIC FALVORS NOT REQUESTED                      
* PASS RAW AND EQUIVALENCED                                                     
         MVC   NDDEMSV,NDANYDEM    SAVE NDANYDEM                                
         OI    NDANYDEM,RAW+EQU    SET FLAVORS                                  
                                                                                
* SAVE  VANILLA FLAVOR VALUES                                                   
DOFLAVS  MVC   NBESTHSV,NBESTHOM                                                
         MVC   NBACTHSV,NBACTHOM                                                
         MVC   NDESTDSV,NDESTDEM                                                
         MVC   NDACTDSV,NDACTDEM                                                
         MVC   NBN0B2SV,NBN0B2                                                  
         MVC   NBN2B1SV,NBN2B1                                                  
         MVC   NBDRRSV,NBDEMRAW                                                 
*                                                                               
***      TM    NDANYDEM,RAW        IS IT ADJ + NOT EQUIV ?                      
***      BNO   DODEM05                 NO                                       
         TM    NDANYDEM,RAW+EQU    IF THEY WANT ONE THEY GET BOTH               
         BZ    DODEM05                 NO                                       
         MVI   NBN0B2,0                                                         
         MVI   NBN2B1,0                                                         
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBINDS,X'40'            YES -GREATER IMP                         
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         MVC   RWESTHOM,NBESTHOM                                                
         MVC   RWACTHOM,NBACTHOM                                                
         MVC   RWESTDEM,NDESTDEM                                                
         MVC   RWACTDEM,NDACTDEM                                                
*                                                                               
*DODEM05  TM    NDANYDEM,NN         NOT ADJUSTED + NOT EQUIVALENCED             
**       BNO   DODEM10                                                          
DODEM05  TM    NDANYDEM,NN+NQ      IT'S ALWAYS BOTH                             
         BZ    DODEM10                                                          
         MVI   NBN0B2,0               SET FOR NOT EQUIV                         
         MVI   NBN2B1,0                                                         
         MVI   NBDEMRAW,C'Y'          SET NOT ADJ DEMO FLAG                     
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         MVC   NNESTHOM,NBESTHOM                                                
         MVC   NNACTHOM,NBACTHOM                                                
         MVC   NNESTDEM,NDESTDEM                                                
         MVC   NNACTDEM,NDACTDEM                                                
         MVC   NBDEMRAW,NBDRRSV                                                 
*                                                                               
DODEM10  TM    NDANYDEM,EQU+RAW    ALWAYS GET BOTH                              
         BZ    DODEM20                                                          
         MVC   NBDEMRAW,NBDRRSV       ADJUST DEMOS BY DEM/PKG GUAR              
         MVC   NBN0B2(1),NDQBASE      SET FOR EQUIV. BASE                       
         MVC   NBN2B1(1),NDQBASE                                                
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         MVC   RQESTHOM,NBESTHOM                                                
         MVC   RQACTHOM,NBACTHOM                                                
         MVC   RQESTDEM,NDESTDEM                                                
****     GOTO1 =V(PRNTBL),DMCB,=C'EQ2',NDESTDEM,C'DUMP',10,=C'1D'               
         MVC   RQACTDEM,NDACTDEM                                                
         TM    NDANYDEM,NN+NQ      IF ONY ASKING RAW/EQU                        
         BZ    DODEMXIT            GET OUT WITHOUT RESETTING                    
*                                                                               
*DODEM20  TM    NDANYDEM,NQ         NOT ADJ + EQUIVALENCED                      
*         BNO   DODEMXX                                                         
DODEM20  TM    NDANYDEM,NQ+NN      BOTH                                         
         BZ    DODEMXX                                                          
         MVI   NBDEMRAW,C'Y'       DON'T ADJUST DEMOS BY DEM/PKG GUAR           
         MVC   NBN0B2(1),NDQBASE      SET FOR EQUIV. BASE                       
         MVC   NBN2B1(1),NDQBASE                                                
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         MVC   NQESTHOM,NBESTHOM                                                
         MVC   NQACTHOM,NBACTHOM                                                
         MVC   NQESTDEM,NDESTDEM                                                
         MVC   NQACTDEM,NDACTDEM                                                
         MVC   NBDEMRAW,NBDRRSV       SET NOT ADJ DEMO FLAG                     
*                                                                               
DODEMXX  DS    0H                                                               
         CLI   NDDEMSV,0            IF USING NDDEMSV                            
         BE    *+14                                                             
         MVC   NDANYDEM,NDDEMSV     RESET NDANYDEM                              
         MVI   NDDEMSV,0                                                        
                                                                                
         MVC   NBESTHOM,NBESTHSV     RESET PLAIN DEMO VALUES                    
         MVC   NBACTHOM,NBACTHSV                                                
         MVC   NDESTDEM,NDESTDSV                                                
         MVC   NDACTDEM,NDACTDSV                                                
         MVC   NBN0B2(1),NBN0B2SV                                               
         MVC   NBN2B1(1),NBN2B1SV                                               
         MVC   NBDEMRAW,NBDRRSV       SET NOT ADJ DEMO FLAG                     
*                                                                               
DODEMXIT B     EXXT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                  EXPANDED DEMOS                               
DODEMEXP DS    0H                                                               
         L     R3,XDARAWEX         COPY DEMOS TO BLOCK                          
         USING RAWDATA,R3                                                       
DODEM02X MVI   NDDEMSV,0                                                        
                                                                                
         TM    NBINDS,X'40'            YES -GREATER IMP                         
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
*                                                                               
* SET UP VANILLA FLAVOR *          ALWAYS SET UP VANILLA FLAVOR                 
         MVC   RWESTHOM,NBESTHOM   RAW=EQU=NBDEMO VALUES                        
         MVC   RWACTHOM,NBACTHOM                                                
         MVC   RQESTHOM,NBESTHOM                                                
         MVC   RQACTHOM,NBACTHOM                                                
         MVC   RWESTDEM,XDESTDEM                                                
         MVC   RWESTDM2,XDESTDM2                                                
         MVC   RWACTDEM,XDACTDEM                                                
         MVC   RWACTDM2,XDACTDM2                                                
         MVC   RQESTDEM,XDESTDEM                                                
         MVC   RQESTDM2,XDESTDM2                                                
         MVC   RQACTDEM,XDACTDEM                                                
         MVC   RQACTDM2,XDACTDM2                                                
*                                                                               
         TM    NDANYDEM,NN+NQ      IF THIS                                      
         BNZ   SKPTHSX             SKIP EFFICIENCY FOR NOW                      
         TM    NDANYDEM,RAW+EQU    IF RAW/EQU - HAVE ALREADY DONE RAW           
         BNZ   DODEM10X                         GO GET EQU                      
SKPTHSX  EQU   *                                                                
*                                                                               
         TM    NDANYDEM,X'20'      ARE WE DOING RAW ETC?                        
         BO    CHKFLV              YES                                          
         B     DODEMXXT            NO AND THAT'S ALL                            
*                                                                               
CHKFLV   TM    NDANYDEM,RAW+EQU+NN+NQ    SPECIFIC FLAVORS REQUESTED?            
         BNZ   DOFLAVSX                  YES                                    
                                                                                
*                                        NO                                     
* NDANYDEM SET TO X'20' BUT SPECIFIC FALVORS NOT REQUESTED                      
* PASS RAW AND EQUIVALENCED                                                     
         MVC   NDDEMSV,NDANYDEM    SAVE NDANYDEM                                
         OI    NDANYDEM,RAW+EQU    SET FLAVORS                                  
                                                                                
* SAVE  VANILLA FLAVOR VALUES                                                   
DOFLAVSX MVC   NBESTHSV,NBESTHOM                                                
         MVC   NBACTHSV,NBACTHOM                                                
         MVC   NDESTDSV,XDESTDEM                                                
         MVC   NDESTDS2,XDESTDM2                                                
         MVC   NDACTDSV,XDACTDEM                                                
         MVC   NDACTDS2,XDACTDM2                                                
         MVC   NBN0B2SV,NBN0B2                                                  
         MVC   NBN2B1SV,NBN2B1                                                  
         MVC   NBDRRSV,NBDEMRAW                                                 
*                                                                               
         TM    NDANYDEM,RAW+EQU    IF THEY WANT ONE THEY GET BOTH               
         BZ    DODEM05X                NO                                       
         MVI   NBN0B2,0                                                         
         MVI   NBN2B1,0                                                         
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBINDS,X'40'            YES -GREATER IMP                         
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         MVC   RWESTHOM,NBESTHOM                                                
         MVC   RWACTHOM,NBACTHOM                                                
         MVC   RWESTDEM,XDESTDEM                                                
         MVC   RWESTDM2,XDESTDM2                                                
         MVC   RWACTDEM,XDACTDEM                                                
         MVC   RWACTDM2,XDACTDM2                                                
*                                                                               
DODEM05X TM    NDANYDEM,NN+NQ      IT'S ALWAYS BOTH                             
         BZ    DODEM10X                                                         
         MVI   NBN0B2,0               SET FOR NOT EQUIV                         
         MVI   NBN2B1,0                                                         
         MVI   NBDEMRAW,C'Y'          SET NOT ADJ DEMO FLAG                     
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         MVC   NNESTHOM,NBESTHOM                                                
         MVC   NNACTHOM,NBACTHOM                                                
         MVC   NNESTDEM,XDESTDEM                                                
         MVC   NNESTDM2,XDESTDM2                                                
         MVC   NNACTDEM,XDACTDEM                                                
         MVC   NNACTDM2,XDACTDM2                                                
         MVC   NBDEMRAW,NBDRRSV                                                 
*                                                                               
DODEM10X TM    NDANYDEM,EQU+RAW    ALWAYS GET BOTH                              
         BZ    DODEM20X                                                         
         MVC   NBDEMRAW,NBDRRSV       ADJUST DEMOS BY DEM/PKG GUAR              
         MVC   NBN0B2(1),NDQBASE      SET FOR EQUIV. BASE                       
         MVC   NBN2B1(1),NDQBASE                                                
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         MVC   RQESTHOM,NBESTHOM                                                
         MVC   RQACTHOM,NBACTHOM                                                
         MVC   RQESTDEM,XDESTDEM                                                
         MVC   RQESTDM2,XDESTDM2                                                
         MVC   RQACTDEM,XDACTDEM                                                
         MVC   RQACTDM2,XDACTDM2                                                
         TM    NDANYDEM,NN+NQ      IF ONY ASKING RAW/EQU                        
         BZ    DODEMXXT            GET OUT WITHOUT RESETTING                    
*                                                                               
DODEM20X TM    NDANYDEM,NQ+NN      BOTH                                         
         BZ    DODEMXXX                                                         
         MVI   NBDEMRAW,C'Y'       DON'T ADJUST DEMOS BY DEM/PKG GUAR           
         MVC   NBN0B2(1),NDQBASE      SET FOR EQUIV. BASE                       
         MVC   NBN2B1(1),NDQBASE                                                
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         TM    NBSBKEND,NBEXPDM6       EXPAND DEMOS?                            
         BZ    *+8                                                              
         BAS   RE,IMPEXPDM                                                      
         CLI   NBPREOPT,C'Y'       IF CABLE PRECISION                           
         BNE   *+8                                                              
         BAS   RE,CBLPREC          X 10 FOR NET/SYND GRPS                       
         TM    NBINDS,X'40'        GREATER IMP PRECISION SET                    
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         MVC   NQESTHOM,NBESTHOM                                                
         MVC   NQACTHOM,NBACTHOM                                                
         MVC   NQESTDEM,XDESTDEM                                                
         MVC   NQESTDM2,XDESTDM2                                                
         MVC   NQACTDEM,XDACTDEM                                                
         MVC   NQACTDM2,XDACTDM2                                                
         MVC   NBDEMRAW,NBDRRSV       SET NOT ADJ DEMO FLAG                     
*                                                                               
DODEMXXX DS    0H                                                               
         CLI   NDDEMSV,0            IF USING NDDEMSV                            
         BE    *+14                                                             
         MVC   NDANYDEM,NDDEMSV     RESET NDANYDEM                              
         MVI   NDDEMSV,0                                                        
                                                                                
         MVC   NBESTHOM,NBESTHSV     RESET PLAIN DEMO VALUES                    
         MVC   NBACTHOM,NBACTHSV                                                
         MVC   XDESTDEM,NDESTDSV                                                
         MVC   XDESTDM2,NDESTDS2                                                
         MVC   XDACTDEM,NDACTDSV                                                
         MVC   XDACTDM2,NDACTDS2                                                
         MVC   NBN0B2(1),NBN0B2SV                                               
         MVC   NBN2B1(1),NBN2B1SV                                               
         MVC   NBDEMRAW,NBDRRSV       SET NOT ADJ DEMO FLAG                     
*                                                                               
**DODEMXXT B     EXXT                                                           
DODEMXXT DS    0H                                                               
**********************************************************                      
         CLI   NDVTYPTB,0          MULTIPLE VTYPES?                             
         BE    VTYPX               NO                                           
*                                                                               
***      NI    NBUNTSW,X'FF'-X'13'   CLEAR ALL POSSIBLE BITS- THIS              
         XC    NBVTYPS,NBVTYPS       CLEAR ALL POSSIBLE BITS                    
         XC    NBVTYPS2,NBVTYPS2     CLEAR ALL POSSIBLE BITS                    
         GOTO1 NBHELLO,DMCB,(C'D',UNTFIL),(X'71',NBAIO),0,0                     
*                                    DELETE NETVAL ELEM TO FORCE                
*                                    RELOOK OF DEMOS IN CALLS                   
         CLI   VTYPCNT,1                                                        
         BNE   VTYP2                                                            
         MVI   VTYPCNT,2           SET 2ND POSITION                             
         MVC   HALF,NDVTYPTB+2                                                  
         CLI   HALF,0              ONLY ONE TYPE OF VT= REQUESTED               
         BE    VTYPX                                                            
         LA    R3,VTYPXT1          2ND DEMO TABLE AREA                          
         B     VTYP90                                                           
VTYP2    CLI   VTYPCNT,2                                                        
         BNE   VTYP3                                                            
         MVI   VTYPCNT,3           SET 3D POSITION                              
         MVC   HALF,NDVTYPTB+4                                                  
         CLI   HALF,0              ONLY TWO TYPES OF VT= REQUESTED              
         BE    VTYPX                                                            
         LA    R1,DEMTBLNE         LENGTH OF EACH DEMO AREA                     
         AR    R3,R1               BUMP DEMO TBL TO LAST AREA                   
         B     VTYP90                                                           
VTYP3    MVI   VTYPCNT,0           CLEAR POSTITION COUNTER                      
*                                  RESET AT GETUNIT1 NETIO CALL                 
         B     VTYPX               EXIT                                         
*******************************************************************             
*THIS IS NEW CODE TO PASS 2 CHRACTER VTYP CODE TO NENTVLDEMO*******             
*******************************************************************             
VTYP90   MVC   NBVTYPS(2),HALF     TABLE OF VTYPES +BITS                        
         B     VTYP99                                                           
********************************************************************            
*                                                                               
**VTYP90   LA    R1,VTYPEQUS         TABLE OF VTYPES +BITS                      
**         LA    RE,NBVTYPS          PROGRAM AVERAGE                            
**         LA    RF,6                                                           
**VTYP91   CLC   HALF,0(R1)          VTYPES MATCH ?                             
**         BE    VTYP95                                                         
**         LA    R1,3(R1)            BUMP TO NXT TYPE                           
**         CLI   0(R1),0             EOF?                                       
**         BNE   *+6                                                            
**         DC    H'0'                                                           
**       BCT   RF,VTYP91                                                        
**       LA    R1,VTYPEQU2                                                      
**       LA    RE,NBVTYPS2         COMMERCIAL AVERAGE                           
**       LA    RF,6                RESET BCT COUNTER                            
**       B     VTYP91                                                           
*                                                                               
**VTYP95   OC    0(1,RE),2(R1)       SET VTYPE BITS FOR NENTVLDEMO              
**         B     VTYP99                                                         
*                                                                               
VTYP99   MVC   NBN0B2(1),NBN0B2SV                                               
         MVC   NBN2B1(1),NBN2B1SV                                               
         MVC   NBDEMRAW,NBDRRSV       SET NOT ADJ DEMO FLAG                     
         GOTO1 NBNETVAL,DMCB,NETBLOCK  CALL NETVALUE                            
         B     DODEM02X                AND SAVE DATA TO TABLE                   
*                                                                               
UNTFIL   DC    CL8'UNTFIL'                                                      
***************************************************                             
VTYPX    B     EXXT                                                             
         DROP  R3                                                               
NBESTHSV DS    CL(L'NBESTHOM)                                                   
NBACTHSV DS    CL(L'NBACTHOM)                                                   
NDESTDSV DS    CL(L'NDESTDEM)                                                   
NDESTDS2 DS    CL(L'NDESTDEM)                                                   
NDACTDSV DS    CL(L'NDACTDEM)                                                   
NDACTDS2 DS    CL(L'NDACTDEM)                                                   
NBN2B1SV DS    CL1                                                              
NBN0B2SV DS    CL1                                                              
NBDRRSV  DS    CL1                                                              
NDDEMSV  DS    CL1                                                              
         EJECT                                                                  
*                                                                               
IMPDIV10 NTR1                           GREATER PRECISION IS 7 DIGITS           
*                                       NORMAL COMES IN AS 5 DIGITS             
*                                       DROP LAST 2 ZEROS HERE                  
*                                       DO FINAL ROUND IN NEWRIDRIVE            
*                                                                               
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BE    EXXT                 LEAVE AS IS                                 
         CLI   NDPREOPT,C'Y'       IF NOT CABLE BUT PRE=CAB SET                 
         BE    EXXT                 LEAVE AS IS (SO USER CAN MIX                
*                                  CABLE,NETWORK IN PRIMP MODE)                 
*                                                                               
         LA    R2,NBESTHOM+4       DO HOMES FIRST                               
         BAS   RE,DIV100                                                        
*******  LA    R2,NDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,XDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,4(R2)           POINT TO IMPS                                 
*******  LA    R3,25                                                            
         ZIC   R3,XDNDEMOS                                                      
IMPD10   BAS   RE,DIV100                                                        
         LA    R2,8(R2)                                                         
         BCT   R3,IMPD10                                                        
*                                                                               
         TM    NDCOLIND,X'20'      IF NOT PUP                                   
         BNO   EXXT                GET OUT HERE                                 
*                                                                               
*--RWESTHOM AND RWESTDEM ARE ROUNDED HERE FOR PUP                               
*                                                                               
***      LA    R2,RWESTHOM+4       DO HOMES FIRST                               
***      BAS   RE,DIV100                                                        
***      LA    R2,RWESTDEM        ESTIMATE IMPRESSIONS                          
***      LA    R2,4(R2)           POINT TO IMPS                                 
***      LA    R3,20                                                            
***IMPD40   BAS   RE,DIV100                                                     
***      LA    R2,8(R2)                                                         
***      BCT   R3,IMPD40                                                        
         B     EXXT                                                             
*                                                                               
DIV100   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)                                                      
         LTR   R1,R1                                                            
         BZ    DIV10X                                                           
         D     R0,=F'100'                                                       
         ST    R1,0(R2)                                                         
DIV10X   BR    RE                                                               
*                                                                               
                                                                                
IMPEXPDM NTR1                           EXPAND ESTIMATED DEMOS                  
*                                       FOR EXPDEM6 OPTION                      
*                                                                               
         LA    R2,NBESTHOM+4       DO HOMES FIRST                               
         BAS   RE,MULT00                                                        
******** LA    R2,NDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,XDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,4(R2)           POINT TO IMPS                                 
******** LA    R3,25                                                            
         ZIC   R3,XDNDEMOS                                                      
IMPEX10  BAS   RE,MULT00                                                        
         LA    R2,8(R2)                                                         
         BCT   R3,IMPEX10                                                       
         B     EXXT                                                             
*                                                                               
MULT00   DS    0H                                                               
         L     RF,=F'1000'                                                      
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BNE   *+8                 MAKE IT 100                                  
         L     RF,=F'100'                                                       
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)                                                      
         LTR   R1,R1                                                            
         BZ    MULT0X                                                           
         MR    R0,RF                                                            
         ST    R1,0(R2)                                                         
MULT0X   BR    RE                                                               
                                                                                
*                                                                               
************************************************************8                   
* FOR NETWORK AND SYNDICATION WHEN NBPREOPT=Y THE DEMO SYSTEM                   
* STILL RETURNS DEMOS TO 1 DECIMAL.  HERE MULTIPLY X 10                         
* SO NET AND SYND HAVE 2 DEC LIKE CABLE.                                        
*  NOTE NOTE SKIP USER DEMOS SKIP USER DEMOS                                    
CBLPREC  NTR1                                                                   
         CLI   NBPOSTYP,C'N'       AND NET WORK OR SYND                         
         BE    *+12                                                             
         CLI   NBPOSTYP,C'S'                                                    
         BNE   CBLPX                                                            
         MVI   BYTE,0                                                           
******** LA    R2,NDACTDEM         MULT GRP X 10 SO WE CAN REPORT               
******** LA    R4,NDESTDEM         NET/SYND GRPS TO 2 DECIMALS                  
******** LA    RF,NDDEMOS                                                       
         LA    R2,XDACTDEM         MULT GRP X 10 SO WE CAN REPORT               
         LA    R4,XDESTDEM         NET/SYND GRPS TO 2 DECIMALS                  
         LA    RF,XDDEMOS                                                       
*******  LA    R1,20                                                            
         ZICM  R1,XDNDEMOS,(1)                                                  
         BZ    CBLPX                                                            
FUDG30   CLI   1(RF),X'21'         USER DEMO?                                   
         BE    FUDG40              SKIP X10                                     
         ICM   R3,3,2(R2)                                                       
         MH    R3,=H'10'                                                        
         STCM  R3,3,2(R2)                                                       
FUDG40   LA    R2,8(R2)                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,FUDG30                                                        
         CLI   BYTE,1              HAVE WE BEEN HERE?                           
         BE    FUDG50                                                           
         MVI   BYTE,1              SET FLAG                                     
         LR    R2,R4                                                            
         LA    R1,20                                                            
         LA    RF,NDDEMOS                                                       
         B     FUDG30                                                           
*                                                                               
FUDG50   ICM   R3,3,NBESTHOM+2                                                  
         MH    R3,=H'10'                                                        
         STCM  R3,3,NBESTHOM+2                                                  
         ICM   R3,3,NBACTHOM+2                                                  
         MH    R3,=H'10'                                                        
         STCM  R3,3,NBACTHOM+2                                                  
CBLPX    B     EXXT                                                             
*                                                                               
RAW      EQU   X'01'                                                            
EQU      EQU   X'02'                                                            
NN       EQU   X'04'                                                            
NQ       EQU   X'08'                                                            
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
*                                                                               
* - DO NOT USE R5 - DRIVER CALLS A HOOK IN 1ST NMOD AND THIS                    
*                   IS DEPENDENT ON R5                                          
*                                                                               
OVERFLW  NMOD1 0,*NE20OV*,R8                                                    
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         SLL   RF,2                                                             
         B     BRANCHTB(RF)                                                     
EXIT     XIT1                                                                   
*                                                                               
BRANCHTB DS    0H                                                               
         B     AUTHRD           0=ESTIMATE READ FOR AUTH $                      
         B     FLTCCML          1=FILTER COMMERCIAL CLASS                       
         B     HICOM4           2=COMMENTS                                      
         B     XAGYRD           3=CROSS AGY READ                                
         B     XINITB           4=INIT NET BLOCK                                
         B     GETCHKS          5=GET CHECKING INFO                             
         DC    AL4(0)             REPLACED                                      
*****    B     GETAGYID         6=GET FULL AGY ID                               
******   B     GENEDICT         7=GENERATE EDICT                                
         DC    AL4(0)             REPLACED                                      
         B     COMMNTS          8=COMMENTS                                      
         B     CLTLIMIT         9=CLIENT LIMIT ACCESS                           
         B     INITIAL          10=INITIALIZE                                   
         B     XGTPRCT          11=PROD COST                                    
*                                                                               
                                                                                
*                                                                               
GETEL2   AH    (R4),DATADISP                                                    
*                                                                               
FIRST2   CLI   0((R4)),0                                                        
         BNE   *+10                                                             
         CLI   0((R4)),1                                                        
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0((R4))                                                   
         BCR   8,RE                                                             
NEXT2    SR    RF,RF                                                            
         IC    RF,1((R4))                                                       
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1((R4)),1                                                        
         BR    RE                                                               
         AR    (R4),RF                                                          
         B     FIRST2                                                           
                                                                                
* - WORK AREAS                                                                  
         DS    0D                                                               
MYDM     DS    CL96                                                             
ABELEM   DS    F                                                                
SVKEY    DS    CL13                                                             
         EJECT                                                                  
*******************************************************************             
* - READ ESTIMATE HEADERS FOR AUTHORIZATION $                                   
AUTHRD   DS    0H                                                               
         B     AUTHXXX                 NO AUTH$ FOR NOW                         
******                                                                          
         L     R1,ANETWS1          SAVE KEY OF CLIENT REC                       
         MVC   CLTKEYSV,0(R1)                                                   
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM    AGY/MED                                      
         LA    R1,1                                                             
         CLC   =C'ALL',NBSELCLI                                                 
         BE    AUT05                                                            
         MVC   KEY+2(2),NBACTCLI   CLIENT                                       
         LA    R1,2(R1)                                                         
                                                                                
         CLC   =C'ALL',NBSELPRD                                                 
         BE    AUT05                                                            
         CLC   =C'POL',NBSELPRD                                                 
         BE    AUT05                                                            
         MVC   KEY+4(3),NBSELPRD   PRODUCT                                      
         LA    R1,3(R1)                                                         
                                                                                
         CLC   =C'ALL',NBSELEST                                                 
         BE    AUT05                                                            
         MVC   KEY+7(1),NBACTEST     ESTIMATE                                   
         LA    R1,1(R1)                                                         
                                                                                
                                                                                
AUT05    STC   R1,EXCLC                                                         
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     AUT10                                                            
                                                                                
AUTSEQ   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
                                                                                
AUT10    ZIC   R1,EXCLC                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   AUTX                                                             
                                                                                
         CLI   KEY+4,0             CLT KEY?                                     
         BE    AUTSEQ                                                           
         CLI   KEY+7,0             PROD KEY?                                    
         BE    AUTSEQ                                                           
         CLI   KEY+8,0             EST KEY?                                     
         BNE   AUTSEQ                                                           
                                                                                
         BAS   RE,FILTERS                                                       
         BNE   AUTSEQ                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING EKEY,R4                                                          
*****    LA    R2,EAUTHN           AUTHORIZATION $                              
         LA    R2,EAUTH            NEW PL6 AUTHORIZATION $                      
* SET UP NXTDAT TO CONTROL AUTH$ BUCKETS AS WE LOOP THROUGH EAUTHN              
* START WITH YEAR OF EST START DATE BUT USE JAN1 OF THAT YEAR                   
* REGARDLESS OF ACTUAL START MONTH OF EST / AUTH$ BUCKETS IN EST REC            
* BEGIN AT JAN REGARDLESS OF ACTUAL START/END OF ESTIMATE                       
                                                                                
         MVC   NXTDAT(2),ESTART     START WITH EST YEAR                         
         MVC   NXTDAT+2(4),=C'0101' BUT WITH JAN REGARDLESS OF EST DATE         
*                                 FOR THIS IS HOW EST REC CARRIES AUTH$         
         B     AUT20                                                            
AUT15    GOTO1 ADDAY,DMCB,(C'M',DATWRK),NXTDAT,1     GET NEXT MONTH             
         CLC   NXTDAT(4),EEND                       MORE MONTHS?                
***->    BH    AUTX                                                             
         BH    AUTSEQ                                                           
AUT20    GOTO1 DATCON,DMCB,NXTDAT,(2,NBACTDAT)  PASS DATE IN NBACTDAT!          
         MVC   DATWRK,NXTDAT                    SAVE IT TO BUMP                 
         MVC   NBAUTHD,0(R2)                        PASS AUTHOR $               
         OC    NBAUTHD,NBAUTHD     NO NEED TO SEND                              
         BZ    *+8                 IF 0 $                                       
         BAS   RE,SENDIT                                                        
         LA    R2,4(R2)                             BUMP AUTH BUCKET            
         B     AUT15                                                            
         DROP  R4                                                               
AUTX     DS    0H                                                               
         L     R1,ANETWS1          GET ADDRESS OF CLIENT RECORD                 
         CLC   CLTKEYSV,0(R1)      COMPARE TO SAVEE CLIENT KEY                  
         BE    AUTHXX              SAME CLIENT/NO NEED TO DO ANYTHING           
         MVC   FULL,AIO            CLIENT CHANGED/RESTORE CLIENT RECORD         
         MVC   AIO,ANETWS1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                    SHOULD NEVER GET HERE                    
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                  CLIENT REC RESTORED IN ANETWS1           
*                                                                               
AUTHXX   XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
AUTHXXX  B     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
* FITERS FOR ESTIMATE HEADER READ FOR AUTH$                                     
* SETS CC NOT EAUAL TO REJECT                                                   
FILTERS  NTR1                                                                   
         CLI   EXCLC,7             IS ENTIRE KEY SPECIFIC?                      
         BE    FLTSX                                                            
                                                                                
*                                  YES                                          
         BAS   RE,CHKCLT           GET NEW CLIENT REC IF NECESSARY              
         MVC   FULL(3),KEY+4       GETPRD1 USES FULL                            
         BAS   RE,GETPRD1          RETURNS 1 BYTE PRODUCT IN BYTE               
         ZIC   R1,BYTE                                                          
         DC    H'0'                                                             
**       LA    R2,NBPRDMSK         TEST AGAINST MASK                            
         BAS   RE,TESTMASK                                                      
         BE    FLTSNO              REJECT PRODUCT                               
                                                                                
FLTS10   LA    R2,NBESTMSK                                                      
         ZIC   R1,KEY+7            TEST EST FOR POSSIBLE FILTER                 
         BAS   RE,TESTMASK                                                      
         BE    FLTSNO              REJECT EST                                   
         SR    RE,RE               EST OK                                       
                                                                                
FLTSNO   LTR   RE,RE                                                            
*                                                                               
FLTSX    B     EXIT                                                             
                                                                                
                                                                                
* R2 -> MASK,  R1 -> SPECIFIC BIT TO TEST                                       
TESTMASK NTR1                                                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         BE    TESTNO                                                           
         B     TESTX                                                            
*                                                                               
TESTNO   SR    R1,R1                                                            
*                                                                               
TESTX    LTR   R1,R1                                                            
         B     EXIT                                                             
                                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
CLTKEYSV DS    CL13                                                             
         EJECT                                                                  
* IN AUTH$ READ, CHECK IF CLIENT CHANGES/NEED NEW CLIENT REC                    
CHKCLT   NTR1                                                                   
         L     R1,ANETWS1          CLIENT REC IN ANETWS1                        
         CLC   KEY+2(2),2(R1)      NEED NEW CLIENT REC?                         
         BE    CHKCLTX             NO                                           
                                                                                
*                                  YES                                          
         MVC   FULL,AIO            SAVE AIO OF EST HEADER READ                  
         MVC   AIO,ANETWS1         READ CLIENT REC INTO ANETWS1                 
         MVC   KEY2,KEY            SAVE CURRENT ESTIMATE KEY                    
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),KEY2         SET A/M                                      
         MVC   KEY+2(2),KEY2+2     SET NEW CLIENT                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,FULL            RESET SAVED I/O AREA                         
         MVC   KEY,KEY2            RESET SAVED KEY                              
         MVC   FILENAME,=C'SPTDIR  '  RESET SEQ POINTER                         
         GOTO1 HIGH                                                             
                                                                                
CHKCLTX  B     EXIT                                                             
*                                                                               
DATWRK   DS    CL6                                                              
NXTDAT   DS    CL6                                                              
EXCLC    DS    CL1                                                              
                                                                                
*                                                                               
SENDIT   NTR1                                                                   
* -  NETBLOCK FUDGE                                                             
*                                                                               
         L     R1,ANETWS1          CLIENT REC IN ANETWS1                        
         CLC   2(2,R1),NBKEY+2              CLIENT CODE CHANGED?                
         BE    SND10                                                            
         USING CLTHDR,R1                                                        
         MVC   BYTE,CPROF+6                                                     
         DROP  R1                                                               
         GOTO1 NBCLUNPK,DMCB,(BYTE,KEY+2),NBCLICOD                              
SND10    MVC   NBACTEST,KEY+7                EST                                
         MVC   FULL(3),KEY+4                                                    
***      BAS   RE,GETPRD1                                                       
***      MVC   NBSPLPRN,BYTE              PRODUCT                               
         MVC   NBSPLPR3,FULL       PASS 3 CHAR CODE                             
         MVC   NBACTAM(3),KEY+1           AM/CLI                                
         MVI   NBPOSTYP,C'N'                                                    
         MVI   NBSTATYP,C'N'                                                    
         XC    NBACTPRG,NBACTPRG  ?WILL THIS WORK FOR PROGBOTH?                 
         XC    NBPROGNM,NBPROGNM                                                
*                                                                               
         MVI   NBRDBELS,2        SET FLAG TO TURN OFF ACCGEN                    
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
         NETGO NVSETSPT,DMCB                                                    
         B     EXIT                                                             
*                                                                               
                                                                                
         EJECT                                                                  
*                                                                               
*  - COMMERCIAL CLASS FILTERING                                                 
*                                                                               
FLTCCML  DS    0H                                                               
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'21'        X'21' COMMERCIAL ELEMENT                     
         BAS   RE,GETEL2                                                        
         USING NUCMLEID,R4                                                      
         BNE   FCNO                                                             
         TM    NUCMLFLG,X'C0'                                                   
         BO    FCNO                                                             
         LA    RE,TARGIDS          CLEAR COMMERCIAL ID TABLE                    
         LA    RF,TARGLENE                                                      
         XCEF                                                                   
         LA    R3,2                                                             
         LA    R1,TARGIDS                                                       
         MVC   0(8,R1),NUCML1                                                   
         MVC   8(8,R1),NUCML2                                                   
         LA    R1,16(R1)                                                        
         MVI   ELCODE,X'23'        NOW TRY X'23' ELEMENT                        
         USING NUFDCEID,R4                                                      
FCC05    BAS   RE,NEXT2                                                         
         BNE   FCC10                                                            
         MVC   0(8,R1),NUFDCML1                                                 
         MVC   8(8,R1),NUFDCML2                                                 
         LA    R3,2(R3)                                                         
         LA    R1,16(R1)                                                        
         C     R3,=F'42'           MAX=20 FEEDS + X'21' ELEM                    
         BNH   FCC05                                                            
         DS    H'0'                XPAND TARGIDS                                
*                                                                               
FCC10    LA    R2,TARGIDS            COMMERCIAL ID TABLE                        
         MVC   TARGID,0(R2)                                                     
FCC12    LA    R3,CIDTBL             COMMERCIAL ID/CLASS TABLE                  
         LA    RF,20                 MAX 20 ENTRIES                             
FCC20    CLI   0(R3),0                                                          
         BE    FCC30                                                            
         CLC   TARGID,0(R3)        IS ID/CLASS ALREADY IN TABLE                 
         BE    FCC99               YES/SO OK                                    
         LA    R3,12(R3)                                                        
         BCT   RF,FCC20                                                         
         XC    CIDTBL,CIDTBL     OVER 20 ENTRIES/START FROM SCRATCH             
         LA    R3,CIDTBL                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
FCC30    DS    0H                  NO/READ COMMERCIAL RECORD                    
         MVC   AIO2,AIO                                                         
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   0(8,R3),TARGID                                                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),NBACTAM   (AGENCY/MEDIA/CLIENT)                        
         MVC   CMLKCML,TARGID                                                   
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'TRFDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCNO                                                             
         MVC   FILENAME,=C'TRFFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING CMLDTAEL,R4                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BNE   FCNO                                                             
* - CHECK AGAINST FILTER                                                        
         LA    RE,NDCMLCLS         COMMERCIAL CLASS FILTER                      
         LA    RF,CMLCLASS                                                      
         LA    R1,4                                                             
FCC50    CLI   0(RE),C'*'          WILD CARD OK                                 
         BE    FCC60                                                            
         CLI   0(RE),X'40'         BLANK OK                                     
         BNH   FCC60                                                            
         CLC   0(1,RE),0(RF)                                                    
         BNE   FCNO                                                             
FCC60    LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,FCC50                                                         
         MVC   8(4,R3),CMLCLASS           SET IN CIDTBL                         
*                                                                               
FCC99    LA    R2,8(R2)            GET NEXT ID ENTRY                            
         CLI   0(R2),0                                                          
         BE    FCYES                                                            
         MVC   TARGID,0(R2)                                                     
         B     FCC12                                                            
*                                                                               
FCYES    DS    0H                                                               
         BAS   R4,FCRESET          RESET FILES/SEQ                              
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     FCCX                                                             
FCNO     DS    0H                                                               
         BAS   R4,FCRESET          RESET FILES/SEQ                              
         XC    0(8,R3),0(R3)       CLEAR REJECTED CML ID FROM TABLE             
         LA    R1,1                                                             
         LTR   R1,R1                                                            
FCCX     B     EXIT                                                             
*                                                                               
FCRESET  DS    0H                                                               
         MVC   AIO,AIO2                 RESET I/O,FILES,RD SEQ                  
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         BR    R4                                                               
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
HICOM4   DS    0H            IF HIGHCOM IN HEADER, SET NDHIGHCM BYTE            
         LA    R2,SPLLEFTH                                                      
         LA    R4,8               4=MAX NUMB OF FIELDS IN EACH HEADER           
         MVI   NDHIGHCM,0                                                       
HIC10    CLI   5(R2),0                                                          
         BE    HIC12                                                            
         CLC   =C'HIGHCOM4',8(R2)                                               
         BE    HIC20                                                            
HIC12    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCTR  R4,0                                                             
         C     R4,=F'0'                                                         
         BNH   HICX                                                             
         C     R4,=F'4'            IF HICOM NOT IN LEFT HEADER                  
         BNE   HIC10                                                            
         LA    R2,SPLRGHTH         TRY RIGHT HEADER                             
         B     HIC10                                                            
*                                                                               
HIC20    DS    0H                                                               
**       LA    R2,SPLLEFTH         YES/HICOM-SET APPROPRIATE BITS               
**       LA    R4,8                4=MAX NUMB FIELDS IN EACH HEADER             
**HIC30    CLI   5(R2),0                                                        
**         BE    HIC40                                                          
         OC    NBSELCLI,NBSELCLI                                                
         BZ    HIC32                                                            
         CLC   =C'ALL',NBSELCLI                                                 
         BE    HIC32                                                            
         OI    NDHIGHCM,X'01'                                                   
HIC32    OC    NBSELPRD,NBSELPRD                                                
         BZ    HIC33                                                            
         CLC   =C'ALL',NBSELPRD                                                 
         BE    HIC33                                                            
         CLC   =C'POL',NBSELPRD                                                 
         BE    HIC33                                                            
         OI    NDHIGHCM,X'02'                                                   
HIC33    OC    NBSELEST,NBSELEST                                                
         BZ    HIC34                                                            
         OI    NDHIGHCM,X'04'                                                   
HIC34    OC    NBSELNET,NBSELNET                                                
         BZ    HIC36                                                            
         CLC   =C'ALL',NBSELNET                                                 
         BE    HIC36                                                            
         OI    NDHIGHCM,X'08'                                                   
HIC36    OC    NBSELDP,NBSELDP                                                  
         BZ    HIC38                                                            
         OI    NDHIGHCM,X'10'                                                   
HIC38    OC    NBSELPAK,NBSELPAK                                                
         BZ    HIC40                                                            
         OI    NDHIGHCM,X'20'                                                   
HIC40    DS    0H                                                               
*                                                                               
HICX     B     EXIT                                                             
*                                                                               
SCANBLK  DS    CL420                                                            
                                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*  - CROSS AGENCY READ                                                          
*                                                                               
XAGYRD   DS    0H                                                               
         CLI   LSTRD,0             FIRST TIME IN                                
         BNE   LRD25                                                            
         L     R2,=A(AGYTABL)          ..YES/CHECK AUTHORIZED ACCESS            
LRD10    CLC   NBSELAGY,0(R2)                                                   
         BNE   LRD12                                                            
         CLC   SPLCLI+1(3),4(R2)                                                
         BE    LRD20                                                            
LRD12    LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   LRD10                                                            
         DC    H'0'           BAD BUG/SHOULD HAVE BEEN CAUGHT ON-LINE           
LRD20    MVC   LSTRQAGY,0(R2)       .. SAVE REQUESTING AGENCY                   
         L     R2,8(R2)             .. AND GET ADDRESS OF LIST                  
         ST    R2,AAGYCLT                                                       
         MVI   LSTRD,1                                                          
         XC    PREVLST,PREVLST                                                  
         MVC   CURRLST,0(R2)                                                    
         B     LRD40                                                            
*                                                                               
LRD25    L     R2,AAGYCLT          AGY/CLIENT/PRODUCT/EST LIST                  
         LA    R1,AELENE           LENGTH OF EACH ENTRY                         
         ST    R1,FULL                                                          
         ZIC   R1,LSTRD            BUMP TO NEXT AGY/CLT                         
         M     R0,FULL                                                          
         AR    R2,R1                                                            
         CLI   0(R2),X'FF'         END OF LINE                                  
         BNE   LRD27                                                            
* - IF NO MORE ENTRIES OPEN FILES FOR REQUESTOR AGENCY                          
* - ELSE APPLICATION DIES WITH INVALID AGENCY ERROR IN MULTIPLE                 
* - OVERNIGHT REQUESTS READING ACROSS AGENCIES                                  
         MVI   LSTRD,X'FF'         SET FLAG                                     
         LA    R2,LSTRQAGY         AND POINT TO REQUESTOR AGENCY                
         B     LRD42               GO OPEN FILE                                 
LRD27    ZIC   R1,LSTRD            ADD TO READ POINTER                          
         LA    R1,1(R1)                                                         
         STC   R1,LSTRD                                                         
         MVC   PREVLST,CURRLST                                                  
         MVC   CURRLST,0(R2)                                                    
*                                                                               
LRD40    CLC   PREVLST(2),CURRLST     IS IT SAME AGENCY                         
         BE    LRD70                                                            
*  AGY DIFFERENT - READ ACCESS REC TO FIND SE NUMBER - PASS IN BYTE             
LRD42    LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH,0(R2)     SET AGENCY ALPHA                              
         MVC   AIO2,AIO            SAVE AIO                                     
         L     R4,=A(MYIO)         AND USE MY OEN                               
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                                                             
         MVI   ELCODE,X'21'                                                     
         MVC   HALF,DATADISP       SAVE DATADISP                                
         MVC   DATADISP,=H'28'                                                  
         USING CTSYSD,R4                                                        
         BAS   RE,GETEL2                                                        
         BE    *+10                                                             
         DC    H'0'                                                             
LRD50    BAS   RE,NEXT2                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R4),3             CHK FOR NET SYSTEM                           
         BNE   LRD50                                                            
         MVC   BYTE,CTSYSSE                                                     
         MVC   DATADISP,HALF       RESET DATADISP                               
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         MVC   AIO,AIO2            RESET AIO                                    
* - GET UTL                                                                     
         L     R3,ATWA                                                          
         L     R3,TWAMASTC-TWATASK(R3)      POINT TO PASTC                      
         L     R3,MCUTL-MCBLOCK(R3)         POINT TO UTL                        
         ST    R3,NBUTL                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   4(1,R3),BYTE        BYTE HAS SE NUMBER OF NEW AGY                
         BE    LRD60               SAME FILE                                    
* - CLOSE OLD FILE                                                              
*        GOTO1 NBDM,DMCB,=C'CLOSE',=CL8'SPOT',FILLIST,NBAIO                     
* - OPEN NEW ONE                                                                
         L     R3,NBUTL                                                         
         MVC   4(1,R3),BYTE                                                     
         L     R5,=A(FILLIST)                                                   
         GOTO1 NBDM,DMCB,=C'OPEN',=CL8'SPOT',(R5),NBAIO                         
LRD60    CLI   LSTRD,X'FF'                   IF END/ EXIT HERE                  
         BE    LRDEND                                                           
*                                                                               
LRD70    DS    0H             NEW FILE OPEN/SET UP TO VALIDATE REQ              
         USING AGYENTD,R2                                                       
         XC    TWAACCS,TWAACCS     CLEAR CLIENT OFFICE RESTRICTIONS             
         MVI   NBQINIT,0                                                        
         MVI   NBSELAM,0                                                        
         MVC   NBSELAGY,AEAGY          ...SET AGENCY                            
         XC    SPLCLI,SPLCLI                                                    
         MVC   SPLCLI(3),AECLT         ...AND CLIENT                            
         MVI   SPLCLIH+5,3                                                      
         CLI   AECLT+2,X'40'                                                    
         BH    *+8                                                              
         MVI   SPLCLIH+5,2          ...AND LENGTH                               
LRD80    CLI   LSTRD,1             IF FIRST TIME                                
         BE    LRDX                GET OUT HERE                                 
         DROP  R2                                                               
*                                                                               
         XC    NBUSER,NBUSER       CLIENT PROFILES AGAIN                        
         XC    NBEFFCLI,NBEFFCLI                                                
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLIALL,DMCB,0                                                  
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,0     PROD                                         
         LA    R2,SPLESTH          EST                                          
         TM    NBINDS6,NBI6XDEM    EXPANDED DEMOS ?                             
         BO    LRD85                                                            
*******  NETGO NVESTRNG,DMCB,0,NDDEMBLK   NO                                    
*******  B     LRD90                                                            
LRD85    NETGO NVESTRNG,DMCB,0,XDDEMBLK   YES                                   
LRD90    LA    R2,SPLNETH          NET                                          
         NETGO NVNETALL,DMCB,0                                                  
         LA    R2,SPLDPTH          DPT                                          
         NETGO NVDPTALL,DMCB,0                                                  
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,0                                                  
         LA    R2,SPLRSTRH         START                                        
         NETGO NVSTRDAT,DMCB,0                                                  
         LA    R2,SPLRENDH         END                                          
         NETGO NVENDDAT,DMCB,0                                                  
         LA    R2,SPLDEMH          DEMOS                                        
*******  MVI   NDDEMOS,14                                                       
*******  TM    NBINDS6,NBI6XDEM    EXPANDED DEMO TABLE?                         
*******  BO    LRD95                                                            
*******  NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK   NO                                  
*******  B     LRDX                                                             
         MVI   XDDEMOS,14                                                       
LRD95    NETGO NVDEM,DMCB,DBLOCK,XDDEMBLK   YES                                 
         B     LRDX                            GO GET UNITS                     
*                                                                               
LRDEND   MVI   LSTRD,0                                                          
*                                                                               
LRDX     B     EXIT                                                             
         EJECT                                                                  
* READ UNIT PAYING ELEMENT AND GET CORRESPONDING CLEARANCE STATUS REC           
* THEN GO TO DRIVER WITH CHECK NUMBER                                           
* ROUTINE THUS CALLS DRIVER MULTIPLE TIMES FOR EACH UNIT                        
GETCHKS  DS    0H                                                               
         TM    NDRDBCEL,X'02'      ARE WE LOOKING FOR CHECK NUMBERS             
         BNO   EXIT                                                             
*                                                                               
         MVI   GCPYBLSW,0          INIT PAYABLES SWITCH                         
*                                                                               
*  - MAKE SURE PROD WE ARE DEALING WITH (NBSPLPRN) MATCHES ONE                  
*  - OF UNIT PRODS (IN CASE THEY BILL THEN CHANGE PROD)                         
         ZIC   RE,NBPRDNO          MULT PROD ELEM                               
         LTR   RE,RE                                                            
         BZ    GC04                                                             
**       LA    RF,NBPRDLST                                                      
**GC03     CLC   NBSPLPRN,0(RF)                                                 
**         BE    GC05                                                           
**         LA    RF,1(RF)                                                       
**         BCT   RE,GC03                                                        
         ICM   RF,15,NBADPLST      YES- MULTI PROD 3 CHAR LIST                  
         BNZ   *+6                                                              
         DC    H'0'                ???                                          
GC03     CLC   NBSPLPR3,0(RF)                                                   
         BE    GC05                                                             
         LA    RF,3(RF)                                                         
         BCT   RE,GC03                                                          
         B     EXIT                NO MATCH EXIT                                
*                                                                               
***GC04     CLC   NBSPLPRN,NBPRD      NORMAL 2 PROD UNIT                        
****     BE    GC05                                                             
****     CLC   NBSPLPRN,NBPRD2                                                  
****     BNE   EXIT                                                             
GC04     CLC   NBSPLPR3,NBPR1CL3                                                
         BE    GC05                                                             
         CLC   NBSPLPR3,NBPR2CL3                                                
         BNE   EXIT                                                             
*                                                                               
*          DATA SET NEWRI20    AT LEVEL 118 AS OF 01/26/06                      
GC05     DS    0H                                                               
*                                                                               
         MVC   AIO2,AIO            SET UP I/O AREA                              
         L     R3,=A(MYIO)                                                      
         ST    R3,AIO                                                           
***      A     R3,=F'4000'                                                      
         A     R3,=F'6000'                                                      
         ST    R3,NDCIDTBL       PASS ADDR TO DRIVER                            
         USING BHBLOCK,R3                                                       
         BAS   R2,CLRBLK           CLEAR BLOCK                                  
*                                                                               
         L     R4,NBAIO            FIND PAY ELEMENTS                            
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL2                                                        
         BE    GC05A               WE HAVE A PAY ELEMENT                        
*                                                                               
         BRAS  RE,CHKPYBL          CHECK FOR PAYABLE AMOUNTS                    
         BNE   GCX                 NO PYBLE TO WORRY ABOUT - EXIT               
*                                                                               
         MVI   GCPYBLSW,C'Y'       PAYABLES DONE                                
*                                                                               
         B     GC40F               CONTINUE                                     
*                                                                               
GC05A    DS    0H                                                               
*                                                                               
         ST    R4,ABELEM           YES-SAVE ADDR OF CURRENT PAY ELEM            
         USING NUPAYD,R4                                                        
********************************************************                        
         CLC   NBPRD,NBPRD2        ARE THESE DUP PIGGYS?                        
         BE    *+12                                                             
         MVI   DUPFLAG,0           JUST TO BE SAFE                              
         B     GC06                                                             
         CLI   DUPFLAG,1           DID WE DO 1ST CALL?                          
         BE    *+12                                                             
         MVI   DUPFLAG,1           NO SET IT                                    
         B     GC06                                                             
         MVI   DUPFLAG,2           SET TO DO SECOND                             
GC06     EQU   *                                                                
****************************************************                            
         B     GC15                                                             
                                                                                
*                                                                               
GC10     DS    0H                                                               
*                                                                               
         BAS   R2,CLRBLK                                                        
*                                                                               
         MVI   ELCODE,X'12'                                                     
         L     R4,ABELEM                                                        
         BAS   RE,NEXT2                                                         
         BE    GC10A                                                            
*                                                                               
         BRAS  RE,CHKPYBL          CHECK FOR PAYABLE AMOUNTS                    
         BNE   GCX                 NO PAYABLE AMNTS - EXIT                      
*                                                                               
         MVI   GCPYBLSW,C'Y'       PAYABLES DONE                                
*                                                                               
         B     GC40F               CONTINUE                                     
*                                                                               
GC10A    DS    0H                                                               
*                                                                               
         ST    R4,ABELEM                                                        
*                                                                               
GC15     DS    0H                                                               
*                                                                               
         TM    NBPPDOPT,NBPPDONQ   SKIP IF NOT PAYPENDING OPTION                
         BNO   GC15PPDX                                                         
*                                                                               
         TM    NBPPDOPT,NBPPDCKQ   SKIP IF NOT CLEARED/NOT PAID PASS            
         BNO   GC15PPDX                                                         
*                                                                               
         OC    NUPAYDAT,NUPAYDAT   SKIP IF ELEMENT NOT CLEARED                  
         BZ    GC10                                                             
*                                                                               
GC15PPDX DS    0H                                                               
*                                                                               
* - READ STATION RECORD FOR MARKET NUMBER                                       
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    RA,KEY                                                           
         USING STAREC,RA                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NBACTNET                                             
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,NBCLICOD                                                 
         CLC   STAKEYSV,KEY                  IS IT SAME STATION                 
         BE    GC17                                                             
         BAS   R2,GETHI            READ HI                                      
         CLC   KEY(15),0(RA)                                                    
         BE    GC16                                                             
         LA    RA,KEY              IF NO MATCH                                  
         MVC   STAKCLT,=C'000'     READ AGAIN CLEARING CLIENT                   
         BAS   RE,GETHI                                                         
         CLC   KEY(15),0(RA)                                                    
         BE    GC16                                                             
         DC    H'0'                                                             
*                                                                               
GETHI    L     RA,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'STATION',KEY,(RA),0                 
         BR    R2                                                               
         EJECT                                                                  
*                                                                               
GC16     MVC   MKTSV,SMKT                     EXTRACT MARKET NUMBER             
         MVC   STAKEYSV,KEY                   SAVE STATION KEY                  
                                                                                
* - READ CLEARANCE STATUS RECORD                                                
GC17     MVC   FULL,MKTSV                                                       
         LA    RA,KEY                                                           
         USING CLRSTATD,RA                                                      
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,NBACTAM    A-M                                          
         MVC   CLSKCLT,NBACTCLI    CLT                                          
         MVC   DUB(4),NBACTNET                                                  
         MVI   DUB+4,C'N'                                                       
* - PACK MKT/STATION  (FULL = MKT NUMBER SET ABOVE)                             
         GOTO1 MSPACK,DMCB,FULL,DUB,CLSKMKT                                     
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GC10                                                             
         MVC   SVKEY,KEY                                                        
*                                                                               
GETCH050 MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GETCH080                                                         
         CLC   CLSKDATE,NUPAYDAT   CHECK CLEARENCE DATE                         
         BH    GETCH080                                                         
         BNE   *+14                                                             
         CLC   CLSKSEQ,NUPAYSEQ    CHECK SEQUENCE NUMBER                        
         BH    GETCH080                                                         
         MVC   SVKEY,KEY                                                        
         B     GETCH050                                                         
*                                                                               
GETCH080 L     RA,=A(MYIO)                                                      
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,(RA),MYDM            
*                                                                               
         EJECT                                                                  
* - HAVE CLEARANCE RECORD - SEE IF IT MATCHES UNIT PAY ELEMENT                  
         LA    RA,CLSTEL01                                                      
         USING CLSTEL01,RA                                                      
         B     GETCH110                                                         
*                                                                               
GETCH100 ZIC   RE,CLSTEL01+1                                                    
         AR    RA,RE                                                            
GETCH110 CLI   CLSTEL01,3                                                       
         BE    GETCH100                                                         
         CLI   CLSTEL01,5                                                       
         BE    GETCH100                                                         
         CLI   CLSTEL01,1                                                       
*****    BNE   GC10                                                             
                                                                                
         BE    GETCH120            HAVE CLEARANCE STATUS RECORD                 
*                                  NOT CLEARED YET                              
         BRAS  RE,GTWRPRF         READ WR PROFILE                               
*                                                                               
         TM    NBINDS4,NBI4DISB   SKIP IF NO DISBURSE DATE NEEDED               
         BNO   GC10                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,CHQDATE) TODAY FOR CHQDATE                  
*                                                                               
         MVC   CHQBNKDT,CHQDATE    SAME FOR BANK DATE                           
         MVC   CHQCLRDT,CHQDATE    SAME FOR CLEARANCE DATE                      
         MVI   CHQBKDFT,C'*'      FLAG AS DEFAULT DATE                          
*                                                                               
         CLI   NBRDBELS,0         HAVE WE ALREADY GONE TO DRIVER ONCE           
         BNE   *+8                                                              
         MVI   NBRDBELS,1          NO/SET FIRST TIME                            
*                                                                               
         B     GC41                GO FILL IN MONEY FIELDS                      
*                                                                               
GETCH120 DS    0H                                                               
*                                                                               
* - IS IT RIGHT CHECK ELEMENT                                                   
*                                                                               
         L     R4,ABELEM           GET ADDRESS OF CURRENT PAY ELEM              
         CLC   CLSTCLRD,NUPAYDAT   DATE                                         
         BNE   GETCH100                                                         
         CLC   CLSTCLSQ,NUPAYSEQ   SEQUENCE NUMBER                              
         BNE   GETCH100                                                         
                                                                                
* - MAKE SURE WE ARE DEALING WITH CORRECT PRODUCT                               
         CLI   CLSTPRD,0           ARE THERE PRODS ON CLEARANCE REC             
         BE    GC25                                                             
         CLC   NBSPLPRN,CLSTPRD    YES/MATCH PRODUCTS                           
         BE    GC25                                                             
         CLC   NBSPLPRN,CLSTPRD2                                                
         BNE   GC10                                                             
GC25     DS    0H                                                               
         CLI   NBRDBELS,0         HAVE WE ALREADY GONE TO DRIVER ONCE           
         BNE   *+8                                                              
         MVI   NBRDBELS,1          NO/SET FIRST TIME                            
*                                                                               
* - FILL IN CHQ BLOCK / TEST FOR CHQ NUMBER AND DATE                            
*                                                                               
GC30     DS    0H                  CHECK IF UNIT BELONGS IN REPORT              
*                                                                               
         TM    NBPPDOPT,NBPPDONQ   SKIP TEST IF NOT PAYPEND OPTION              
         BNO   GC38                                                             
*                                                                               
         TM    NBPPDOPT,NBPPDCKQ   SKIP TEST IF NOT CHECKING BUY                
         BNO   GC38                                                             
*                                                                               
         TM    NBPPDOPT,NBPPDINQ   KEEP IF UNIT ALREADY IN REPORT               
         BO    GC35                                                             
*                                                                               
         CLC   =C'VOID',CLSTCHK    KEEP IF CHECK VOIDED                         
         BE    *+12                                                             
         CLI   CLSTCHK,C' '        DROP UNIT IF CHECK PRESENT                   
         BH    GC35                                                             
*                                                                               
         OI    NBPPDOPT,NBPPDINQ   KEEP UNIT IN REPORT                          
*                                                                               
GC35     DS    0H                                                               
*                                                                               
         B     GC10                GET NEXT PAY ELEMENT                         
*                                                                               
GC38     DS    0H                                                               
*                                                                               
         MVC   CHQNUM,CLSTCHK      CHECK NUMBER                                 
*                                                                               
         CLI   CHQNUM,X'40'        IF BLANK DON'T SEND                          
         BNH   GC39                                                             
*                                                                               
         CLC   =C'VOID',CHQNUM     IF VOID CHECK                                
         BE    *+12                                                             
         TM    NBPPDOPT,NBPPDONQ   IF PAYPEND OPT                               
         BO    GC10                   DROP ANYTHING WITH CHECK NUM              
*                                                                               
         CLC   =C'VOID',CHQNUM     IF VOID CHECK                                
         BNE   GC38A                                                            
         TM    NBPPDOPT,NBPPDONQ   AND IF PAYPEND OPT                           
         BNO   GC38A                                                            
*                                                                               
         MVC   CHQNUM,=CL10' '        BLANK CHECK NUMBER                        
*                                                                               
         B     GC39                                                             
*                                                                               
GC38A    DS    0H                                                               
*                                                                               
         MVI   CHQNUM+6,X'40'                                                   
*                                                                               
         TM    CLSTSTAT,X'80'      RECONCILED?                                  
         BNO   *+8                                                              
         MVI   CHQNUM+6,C'*'                                                    
*                                                                               
         MVC   CHQDATE,CLSTCHDT    CHECK DATE                                   
*                                                                               
         CLI   CHQDATE,0           IF NO DATE DON'T SEND                        
         BE    GC39                                                             
*                                                                               
         OC    CHQNUM,=CL10' '                                                  
*                                                                               
         B     GC40                                                             
*                                                                               
GC39     DS    0H                                                               
*                                                                               
         TM    NBPPDOPT,NBPPDONQ+NBPPDINQ IF NO CHECK OR DATE AND               
         BO    GC41                   IN REPORT, PRINT MONEY                    
*                                                                               
         BRAS  RE,GTWRPRF         READ WR PROFILE                               
*                                                                               
         TM    NBINDS4,NBI4DISB   SKIP IF NO DISBURSE DATE NEEDED               
         BNO   GC39A                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,CHQDATE) TODAY FOR CHQDATE                  
*                                                                               
         MVC   CHQBNKDT,CHQDATE    SAME FOR BANK DATE                           
         MVI   CHQBKDFT,C'*'      FLAG AS DEFAULT DATE                          
*                                                                               
         MVC   CHQCLRDT,CLSTCLRD   SET      CLEARANCE DATE                      
         MVC   CHQPRD,CLSTPRD      PRODUCT                                      
         MVC   CHQPRD2,CLSTPRD2    PRODUCT PARTNER                              
*                                                                               
         CLI   NBRDBELS,0         HAVE WE ALREADY GONE TO DRIVER ONCE           
         BNE   *+8                                                              
         MVI   NBRDBELS,1          NO/SET FIRST TIME                            
*                                                                               
         B     GC41                                                             
*                                                                               
GC39A    DS    0H                                                               
*                                                                               
         B     GC10                ELSE DROP ELEMENT                            
*                                                                               
GC40     DS    0H                                                               
*                                                                               
         MVC   CHQCLRDT,CLSTCLRD   CHECK CLEARANCE DATE                         
         MVC   CHQPRD,CLSTPRD      PRODUCT                                      
         MVC   CHQPRD2,CLSTPRD2    PRODUCT PARTNER                              
*                                                                               
         CLI   CLSTEL01+1,CLSTBKDT-CLSTEL01 SKIP IF NO BNK DEP DT               
         BNH   GC401                                                            
*                                                                               
         MVC   CHQBNKDT,CLSTBKDT   CHECK BANK CLEARANCE DATE                    
         MVI   CHQBKDFT,C' '       NOT DEFAULT DATE                             
*                                                                               
         OC    CHQBNKDT,CHQBNKDT  IF CHECK NOT DEPOSITED                        
         BNZ   GC41                  USE LAST DAY OF LAST MONTH                 
*                                                                               
GC401    DS    0H                 USE LAST OF PREVIOUS MONTH                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),WORK    TODAY AS YYMMDD                        
         LHI   RF,-1                                                            
         ST    RF,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'80',WORK+6)   BACKUP A MONTH           
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,CHQBNKDT)                              
         MVI   CHQBKDFT,C'*'      FLAG AS DEFAULT DATE                          
*                                                                               
GC41     DS    0H                                                               
*                                                                               
         MVC   CHQTYPE,NUPAYTYP    SET IN TYPE FROM PAY ELEM                    
*                                                                               
         CLI   NBPRDNO,0           ..IF MULT PROD ELEM                          
         BNE   GC40B               ..GO TO MULT PROD ROUTINE                    
*                                                                               
         CLI   NBPRD2,0            ..ELSE IF NO PIGGY PROD                      
         BNE   GC40A                                                            
         MVC   CHQGRS,NUPAYGRS     ..GROSS DOLLARS FROM PAY ELEM                
         MVC   CHQNET,NUPAYNET     ..NET DOLLARS FROM PAY ELEM                  
         B     GC40F                                                            
                                                                                
* PIGGY PRODS/ SPLIT PAY           .. IF PIGGYS/ SPLIT PAID                     
GC40A    MVC   FULL,NUPAYGRS                                                    
         BAS   RE,SPLITEM                                                       
         MVC   CHQGRS,DUB                                                       
         MVC   FULL,NUPAYNET                                                    
         BAS   RE,SPLITEM                                                       
         MVC   CHQNET,DUB                                                       
         B     GC40F                                                            
                                                                                
* - MULTIPLE PROD ELEMENT                                                       
GC40B    L     R2,NBAIO            GET ADDRESS OF ELEMENT                       
***      LA    R2,27(R2)                                                        
***GC40C    ZIC   R1,1(R2)                                                      
***      LTR   R1,R1                                                            
***      BNZ   *+6                                                              
***      DC    H'0'                                                             
***      AR    R2,R1                                                            
***      CLI   0(R2),X'14'                                                      
***      BNE   GC40C                                                            
***      ST    R2,DMCB             PASS ADDRESS OF ELEM                         
**       ZIC   R1,NBSPLPRN         SPECIFIC PRODUCT                             
         MVC   DMCB,NBAPROD                                                     
         ZIC   R1,NBSPLCNT         PROD POSITION                                
         STC   R1,DMCB                                                          
         GOTO1 =V(NEPACC),DMCB,,NUPAYGRS,(1,DUB),NETBLOCK                       
         MVC   CHQGRS,DUB                                                       
***      ST    R2,DMCB             ADDRESS OF PRODELEM                          
         MVC   DMCB,NBAPROD                                                     
**       ZIC   R1,NBSPLPRN         SPECIFIC PRODUCT                             
         ZIC   R1,NBSPLCNT         PROD POSITION                                
         STC   R1,DMCB                                                          
         GOTO1 =V(NEPACC),DMCB,,NUPAYNET,(1,DUB),NETBLOCK                       
         MVC   CHQNET,DUB                                                       
         B     GC40F                                                            
                                                                                
         EJECT                                                                  
*                                                                               
* - FULL HAS DOLLARS TO SPLIT                                                   
SPLITEM  NTR1                                                                   
* IF NO PRODUCT ON CLEARANCE RECORD                                             
* SKIP CHECK                                                                    
         XC    DUB,DUB                                                          
         CLI   CHQPRD,0                                                         
         BE    SPLIT0                                                           
         CLC   NBSPLPRN,CHQPRD     UNIT PRD MUST = ONE OF CHQ PRDS              
         BE    SPLIT0                                                           
         CLC   NBSPLPRN,CHQPRD2                                                 
         BNE   SPLIT10             ELSE SKIP IT                                 
SPLIT0   L     RF,FULL                                                          
         SR    RE,RE                                                            
         ICM   RE,3,NBP1SHR                                                     
         MR    RE,RE                                                            
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DUB              ASSUME FIRST PIGGY                           
*                                                                               
         CLC   NBPRD,NBPRD2        DUPLICATE PROD?                              
         BNE   SPLIT02                                                          
         CLI   DUPFLAG,2           ARE WE ON 2ND PROD?                          
         BE    SPLIT05             YES                                          
SPLIT02  CLC   NBSPLPRN,NBPRD                                                   
         BE    SPLIT10             FIRST PIGGY                                  
SPLIT05  L     RE,FULL             NO 2ND PIGGY/ GET TOTAL                      
         SR    RE,RF                             SUBTRACT 1ST                   
         ST    RE,DUB                                                           
SPLIT10  XIT1                                                                   
*                                                                               
DUPFLAG  DS    CL1                                                              
         EJECT                                                                  
* - NEED LAST DAY OF MOS OF UNIT DATE AND SAVE IN WORK                          
GC40F    GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,DUB)                                 
         CLI   NDPEROPT,C'B'                    BROADCAST MONTHS                
         BE    GC44                                                             
         MVC   WORK(6),DUB                      NO / USE CALENDAR               
         MVC   WORK+4(2),=C'30'                                                 
         CLC   WORK+2(2),=C'09'       SEP                                       
         BE    GC45                                                             
         CLC   WORK+2(2),=C'04'       APRIL                                     
         BE    GC45                                                             
         CLC   WORK+2(2),=C'06'       JUNE                                      
         BE    GC45                                                             
         CLC   WORK+2(2),=C'11'       NOV                                       
         BE    GC45                                                             
         CLC   WORK+2(2),=C'02'       IS IT  FEB                                
         BE    GC43                                                             
         MVC   WORK+4(2),=C'31'       NO/MAKE IT 31 FOR OTHER MONTHS            
         B     GC45                                                             
* FEBRUARY - DIVIDE YEAR BY 4 AND IF NO REMAINDER ITS 29                        
GC43     MVC   WORK+4(2),=C'28'                                                 
* Y2K DATES WON'T PACK                                                          
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(X'20',WORK+20)                         
         MVC   HALF,WORK+20                                                     
*                                                                               
         PACK  FULL,HALF                                                        
         DP    FULL,=P'4'                                                       
         CP    FULL+3(1),=P'0'                                                  
         BNE   GC45                                                             
         MVC   WORK+4(2),=C'29'                                                 
         B     GC45                                                             
                                                                                
* - BROADCAST                                                                   
GC44     L     R2,GETDAY                                                        
         L     R4,ADDAY                                * R4 USED HERE *         
* - RETURNS START-END OF BROADCAST IN WORK                                      
         GOTO1 =V(GETBROAD),DMCB,(1,DUB),WORK,(R2),(R4)                         
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(6),WORK+6      MOVE END MOS TO WORK                         
                                                                                
* - GET NUMBER OF DAYS BETWEEN LAST DAY OF MOS AND CHQDATE                      
GC45     DS    0H                                                               
         ZAP   CHQDALI,=P'0'       INIT GROSS DAILY BALANCE                     
         ZAP   CHQDALIN,=P'0'      INIT NET   DAILY BALANCE                     
*                                                                               
         OC    CHQDATE,CHQDATE     SKIP IF CHECK NOT WRITTEN                    
         BZ    GC53                                                             
*                                                                               
         BRAS  RE,GTWRPRF         READ WR PROFILE                               
*                                                                               
         LA    RF,CHQDATE         DEFAULT TO CHECK DATE                         
*                                                                               
         L     R1,=A(WRPROF)      POINT TO WR PROFILE                           
*                                                                               
         CLI   11(R1),C'Y'        IF USING BANK CLEAR DATE                      
         BNE   *+8                                                              
         LA    RF,CHQBNKDT           RESET POINTER                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(RF)),(0,WORK+6)                                 
*                                                                               
****     CLC   NBACTDAT,CHQDATE       IF UNIT  LOWER THAN CHKDAT                
         CLC   WORK(6),WORK+6        IF MOS  LOWER THAN CHKDAT                  
         BNH   GC50                                                             
*                                                                               
         XC    WORK(6),WORK+6      SWITCH THEM                                  
         XC    WORK+6(6),WORK                                                   
         XC    WORK(6),WORK+6                                                   
*                                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
*                                                                               
         MVC   HALF,8(R1)                                                       
         LH    R2,HALF                                                          
         BCTR  R2,0         PERVERT RETURNS DAYS INCLUSIVE                      
         LNR   R2,R2               MAKE IT NEGATIVE                             
         ST    R2,CHQDAYD                                                       
*                                                                               
         B     GC52                                                             
*                                                                               
* - CHECKDATE AFTER UNIT SO NUMBER OF DAYS IS POSITIVE                          
*                                                                               
GC50     GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
*                                                                               
         MVC   HALF,8(R1)                                                       
         LH    R2,HALF                                                          
         BCTR  R2,0           PERVERT RETURNS DAYS INCLUSIVE-DROP 1             
         ST    R2,CHQDAYD                                                       
*                                                                               
GC52     CVD   R2,DUB                                                           
         MVC   FULL,DUB+4                                                       
         ICM   R1,15,CHQGRS           GET CHECK AMOUNT                          
         CVD   R1,DUB                                                           
         ZAP   PAKWORK,DUB                                                      
         MP    PAKWORK,FULL                                                     
         MVC   CHQDALI,PAKWORK+4   GROSS DAILY BALANCE                          
         ICM   R1,15,CHQNET           NET CHECK AMOUNT                          
         CVD   R1,DUB                                                           
         ZAP   PAKWORK,DUB                                                      
         MP    PAKWORK,FULL                                                     
         MVC   CHQDALIN,PAKWORK+4   NET DAILY BALANCE                           
*                                                                               
GC53     DS    0H                                                               
*                                                                               
** PASS TO DRIVER HERE                                                          
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 NDDRIVER,DMCB,(R6)                                               
*                                                                               
         CLI   GCPYBLSW,C'Y'       DONE IF PAYABLES DONE                        
         BE    GCX                                                              
*                                                                               
         XC    CHQNET,CHQNET                                                    
         MVI   NBRDBELS,2        SET FLAG TO TURN OFF ACCGEN IF                 
         B     GC10              CURRENT UNIT GOES TO DRIVER AGAIN              
*                                                                               
GCX      DS    0H                                                               
         BAS   R2,CLRBLK           CLEAR BILL/CHQ BLOCK                         
         L     R2,=A(PLANIO)       CLEAR PASSED CHQ NUM/DATE AREA               
         XC    0(250,R2),0(R2)                                                  
         MVC   AIO,AIO2           RESET UNT FILE/RD SEQ/AIO AREA                
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         MVC   KEY,NBKEY           ..REREAD UNIT KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
GCXX     B     EXIT                                                             
*                                                                               
GCPYBLSW DS    XL1                 C'Y' - PAYABLES DONE                         
*                                                                               
         DS    XL1                 SPARE                                        
*                                                                               
CLRBLK   LR    RE,R3               CLEAR BLOCK                                  
         LA    RF,BHBLENE                                                       
         XCEF                                                                   
         MVC   BHBLOCK,=C'**BBLK**'      BILL HEADER BLOCKS IN DUMP             
         MVC   BHDATA,=C'**BKEY**'                                              
         MVC   BHDOLS,=C'**BDOL**'                                              
         BR    R2                                                               
         EJECT                                                                  
*                                                                               
* - THIS ROUTINE SPLITS CHECK DOLLAR AMOUNTS IF NBSPLPRN NOT= 0                 
DOLSPLT  NTR1                                                                   
         ICM   RF,15,CLSTGRS       FIRST COMPUTE FIRST SPLIT                    
         MVC   HALF,NBP1SHR        GET % SHARE                                  
         OC    NBP1SHR,NBP1SHR     IF NO SHARE SET                              
         BNZ   *+10                                                             
         MVC   HALF,=X'1388'       ASSUME 50%                                   
         LH    RE,HALF                                                          
         MR    RE,RE                                                            
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,15,WORK          SET 1ST                                      
         ICM   RE,15,CLSTGRS                                                    
         SR    RE,RF                                                            
         STCM  RE,15,WORK+4        SET 2ND BY SUBTRACTION                       
* - GET NET AMOUNT                                                              
         ICM   RF,15,CLSTNET       FIRST COMPUTE FIRST SPLIT                    
         LH    RE,HALF                                                          
         MR    RE,RE                                                            
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,15,WORK+8        SET 1ST                                      
         ICM   RE,15,CLSTNET                                                    
         SR    RE,RF                                                            
         STCM  RE,15,WORK+12       SET 2ND BY SUBTRACTION                       
         B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
XINITB   DS    0H                                                               
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         CLI   NBN0B2,0            UPDATE BASE OPTION                           
         BE    *+10                                                             
         MVC   NBN0B2(1),NDQBASE   SET FOR EQUIV. BASE                          
         CLI   NBN2B1,0                                                         
         BE    *+10                                                             
         MVC   NBN2B1(1),NDQBASE                                                
         CLI   NDPREOPT,C'Y'       CHECK FOR CABLE PRECISSION                   
         BNE   *+12                                                             
         MVI   NBPREOPT,C'Y'                                                    
         MVI   NBHUNOPT,C'Y'       WE CAN DEAL IN HUNDREDS                      
*                                                                               
         MVI   NBDATA,C'B'         UNITS AND PACKAGES                           
         TM    NBACCFLT,X'04'      DO I NEED PKGS                               
         BO    *+8                                                              
         MVI   NBDATA,C'U'         NO/UNITS ONLY                                
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         OI    NBINDS,X'80'        EQUIVALENCE OVERRIDES                        
         SPACE 1                                                                
         CLC   NDPERCNT(4),=C'COST'   IF COST ADJ BY BRAND NEEDED,              
         BNE   INIT1                                                            
         OI    NBSPLOPT,X'40'         GET SPLIT EVEN FOR POL                    
****     BAS   RE,SETCPOOL            AND SET UP A COST POOL                    
****                                  GET COST PER UNIT IN OVERFLOW             
         SPACE 1                                                                
INIT1    CLI   NDFLAVOR,C'E'       IF ESTIMATE FLAVOR                           
         BNE   INIT2                                                            
         CLI   SPLALLU,C'Y'        ALLUNS=Y                                     
         BNE   INIT2                                                            
         OI    NBSPLOPT,X'20'      ASK FOR BILLED SPLITS                        
         CLC   =C'POL',NBSELPRD    IF NOT PRD=POL                               
         BE    INIT2                                                            
         CLC   =C'ALL',NBSELPRD    DON'T TURN ON FOR SPECIFIC PROD              
         BNE   INIT2                                                            
         OI    NBINDS3,NBI3BPRD    TURN ON BPRD                                 
*                                                                               
INIT2    CLI   NDTHOOPT,C'Y'       OPTION TO STAY IN THOUSANDS                  
         BE    *+8                                                              
         MVI   NBHUNOPT,C'Y'       WE CAN DEAL IN HUNDREDS                      
         MVI   NBRESUME,NBPROCPK   RESUME READING PACKAGES                      
         SPACE 1                                                                
         CLI   NDFLAVOR,C'E'       SELECT FLAVOR DEPENDENT ROUTINES             
         BE    INITE                                                            
         CLC   NDFLAVOR,=C'V2'                                                  
         BE    INITV2                                                           
         CLC   NDFLAVOR,=C'V3'                                                  
         BE    INITV3                                                           
         CLI   NDFLAVOR,C'V'                                                    
         BE    INITV                                                            
         CLI   NDFLAVOR,C'M'                                                    
         BE    INITM                                                            
         SPACE 1                                                                
*                                  FLAVOR P                                     
INITP    MVI   NBSELUOP,C'A'       USES ACTUAL SCHEDULE                         
*******  MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR MGS                        
*                                  IF MISSED UNIT WAS NOT PFB                   
         CLI   NDPFBOPT,C'Y'       PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
         CLI   NDFLAVOR+1,C'M'                                                  
         BNE   *+8                 IF FLAVOR=PM                                 
         MVI   NBESTOPT,C'Y'       DONT GET DEMOS FOR MISSED,PFBS               
         B     EXIT                                                             
         SPACE 1                                                                
*                                  V (AND V1) FLAVOR                            
INITV    MVI   NBSELUOP,C'E'       USES ESTIMATED SCHEDULE                      
         MVI   NBESTOPT,C'Y'                                                    
         B     EXIT                                                             
         SPACE 1                                                                
*                                  V2 FLAVOR                                    
INITV2   MVI   NBSELUOP,C'A'       USES ACTUAL SCHEDULE                         
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR MGS                        
*                                  IF MISSED UNIT WAS NOT PFB                   
         CLI   NDPFBOPT,C'Y'       PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
         B     EXIT                                                             
         SPACE 1                                                                
INITV3   MVI   NBSELUOP,C'A'      USES ACTUAL SCHEDULE                          
         MVI   NBESTOPT,C'Y'                                                    
*******  MVI   NBACTOPT,C'Y'                                                    
         B     EXIT                                                             
         SPACE 1                                                                
*                                  FLAVOR M                                     
INITM    MVI   NBSELUOP,C'A'       USES ACTUAL SCHEDULE                         
*******  MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR MGS                        
*                                  IF MISSED UNIT WAS NOT PFB                   
         CLI   NDPFBOPT,C'Y'       PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
         CLI   NDFLAVOR+1,C'M'                                                  
         BNE   *+8                 IF FLAVOR=MM                                 
         MVI   NBESTOPT,C'Y'       DONT GET DEMOS FOR MISSED,PFBS               
         B     EXIT                                                             
         SPACE 1                                                                
*                                  E FLAVORS                                    
INITE    MVI   NBUSER+13,C'N'      OVERRIDE PROF. DONT FILT PRE-EMPTS           
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE UNLESS ALLU=Y            
         CLI   SPLALLU,C'Y'                                                     
         BNE   *+8                                                              
         MVI   NBSELUOP,0                                                       
         CLI   NDFLAVOR+1,C'P'     OR FLAVOR IS EP                              
         BNE   *+8                                                              
         MVI   NBSELUOP,0                                                       
         CLI   NDFLAVOR+1,C'E'     EE FLAVOR = ESTIMATED SCHEDULE               
         BNE   *+8                                                              
         MVI   NBSELUOP,C'E'                                                    
         SPACE 1                                                                
         CLI   NDANYDEM,0          MAY NEED DEMO LOOKUP FOR ESTIMATES           
         BE    EXIT                                                             
         MVI   NBESTOPT,C'A'                                                    
         CLI   NDPFBOPT,C'Y'       PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
***      TM    NDANYDEM,X'40'      MAYBE NEED ACTUAL                            
***      BNO   *+8                                                              
***      MVI   NBACTOPT,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
         DROP  RA                                                               
         EJECT                                                                  
*              ROUTINE TO GET COST FOR UNIT'S PRODUCT/S                         
         SPACE 3                                                                
XGTPRCT  DS    0H                                                               
         CLC   NDPERCNT,=C'COST'                                                
         BNE   EXIT                                                             
         L     R2,NDACPOOL                                                      
         CLI   B1XFLAG,C'Y'            B1X READ AND BFRML IN WORK?              
***      BE    GETPC3                  YES                                      
         BNE   ENDBX1              NO                                           
         XC    PREVCST,PREVCST     YES/CLEAR SAVE FIELD                         
         B     GETPC3                  SET NDACPOOL                             
                                                                                
ENDBX1   EQU   *                                                                
*                                                                               
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         CLI   NBSPLPR3,0          SET THIS HERE !                              
         BNE   *+10                                                             
         MVC   NBSPLPR3,=C'POL'                                                 
         MVC   WORK(2),NBACTCLI    CHECK TO SEE IF ALREADY READ                 
         MVC   WORK+2(3),NBSPLPR3                                               
*****    MVC   WORK+2(1),NBSPLPRN                                               
         CLI   NBSPLPR3,0          IF NO PROD ALLOCATED                         
*                                  I.E. NBSPLPRN = X'FF' IN 'OLD' CODE          
         BNE   *+10                                                             
         MVC   WORK+2(3),=C'POL'                                                
         MVC   WORK+5(1),NBACTEST                                               
         CLC   WORK(6),PREVCST                                                  
         BE    EXIT                ALREADY HAVE IT                              
         MVC   PREVCST,WORK        NO,SAVE IT                                   
         NETGO NVSETSPT,DMCB                                                    
         USING ESTHDR,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY               READ FOR ESTIMATE HEADER FIRST             
         MVC   KEY+1(3),NBACTAM                                                 
*****    MVC   WORK(1),NBSPLPRN                                                 
*****    BRAS  RE,GETPRD3                                                       
*****    MVC   EKEYPRD,WORK                                                     
         MVC   EKEYPRD,NBSPLPR3                                                 
         CLI   NBSPLPR3,0                                                       
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRYPRD                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,=A(MYIO)                                                      
         OC    EBILLBAS(5),EBILLBAS   IS IT EST LEVEL BILL FORMULA?             
         BZ    TRYPRD                                                           
         MVC   WORK(5),EBILLBAS          YES/PASS IN WORK                       
         B     GETPC3                                                           
                                                                                
TRYPRD   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PRDHDR,R4                                                        
         MVC   KEY+1(3),NBACTAM                                                 
*****    MVC   PKEYPRD,WORK        WORK HAS 3 CHAR PRD CODE                     
         MVC   PKEYPRD,NBSPLPR3    PASS ALPHA PROD                              
         CLI   NBSPLPR3,0          UNALLOCATED                                  
         BNE   *+10                                                             
         MVC   PKEYPRD,=C'POL'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRYAAA                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,=A(MYIO)                                                      
         OC    PBILLBAS(5),PBILLBAS  PROD LEVEL BILL FORMULA?                   
         BZ    TRYAAA                                                           
         MVC   WORK(5),PBILLBAS                                                 
         B     GETPC3                                                           
*                                                                               
TRYAAA   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         MVC   PKEYPRD,=C'AAA'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         XC    WORK(5),WORK        CLEAR WORK                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPC3                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,=A(MYIO)                                                      
         OC    PBILLBAS(5),PBILLBAS  PROD LEVEL BILL FORMULA?                   
         BZ    GETPC3                                                           
         MVC   WORK(5),PBILLBAS                                                 
*                                                                               
GETPC3   MVI   B1XFLAG,0           CLEAR FLAG                                   
***      ZIC   R3,NBSPLPRN         PICK UP PRODUCT CODE                         
***      BCTR  R3,0                                                             
***      SLL   R3,2                                                             
***      AR    R3,R2               DISPLACE INTO COST POOL                      
         LA    R1,500                                                           
         L     R2,NDACPOOL                                                      
         B     GETPC3D         **NOTE THIS BRANCH -                             
***                            **  NEED TO TABLE BY                             
***                                PROD AND EST                                 
GETPC3A  CLI   0(R2),0             EMPTY SPOT                                   
         BE    GETPC3D                                                          
         CLC   NBSPLPR3,0(R2)                                                   
         BE    GTPXIT              ALREADY IN TABLE                             
         LA    R2,7(R2)            BUMP ALPHA PROD+BILLFRML                     
         BCT   R1,GETPC3A                                                       
         DC    H'0'                SHOULD NOT GET HEREE                         
*                                                                               
GETPC3D  MVC   0(3,R2),NBSPLPR3    PRODUCT                                      
         MVC   DUB(4),WORK+1       (SIGNED) COMMISSION RATE                     
         L     R1,DUB                                                           
         TM    WORK,X'01'           COMMISSION BASED ON NET?                    
         BNO   GTPC4                                                            
         M     R0,=F'85'                                                        
         D     R0,=F'100'                                                       
         SPACE 1                                                                
GTPC4    L     R0,=F'1000000'                                                   
         TM    WORK,X'10'      COST BASED ON NET?                               
         BNO   *+8                                                              
         L     R0,=F'850000'                                                    
         AR    R1,R0                                                            
***      ST    R1,0(R3)                                                         
         STCM  R1,15,3(R2)                                                      
         SPACE 1                                                                
GTPXIT   XC    FILENAME,FILENAME   RESET TO UNT FILE                            
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   AIO,NBAIO                                                        
         B     EXIT                                                             
         DROP  R4                                                               
PREVCST  DS    CL6                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP COST POOL FOR PRODUCTS                         
         SPACE 3                                                                
SETCPOOL NTR1                                                                   
**       CLC   NDPERCNT,=C'COST'                                                
*        BNE   EXIT                                                             
*        NETGO NVSETSPT,DMCB                                                    
*        USING PRDHDR,R4                                                        
*        LA    R4,KEY                                                           
*        L     R2,NDACPOOL                                                      
*        XC    KEY,KEY                                                          
*        MVC   KEY+1(3),NBACTAM                                                 
*        SPACE 1                                                                
*SCP2     LA    R4,KEY                                                          
*        AI    PKEYPRD+2,1         SKIP TO NEXT PRODUCT                         
*        MVC   FILENAME,=C'SPTDIR  '                                            
*        GOTO1 HIGH                                                             
*        CLC   KEY(4),KEYSAVE                                                   
*        BNE   SCPXIT                                                           
*        MVC   FILENAME,=C'SPTFIL  '                                            
*        GOTO1 GETREC                                                           
*        LA    R4,IO                                                            
*        ZIC   R3,PCODE+1          PICK UP PRODUCT CODE                         
*        BCTR  R3,0                                                             
*        SLL   R3,2                                                             
*        AR    R3,R2               DISPLACE INTO COST POOL                      
*        MVC   DUB(4),PBILLCOM     (SIGNED) COMMISSION RATE                     
*        L     R1,DUB                                                           
*        TM    PBILLBAS,X'01'      COMMISSION BASED ON NET?                     
*        BNO   SCP4                                                             
*        M     R0,=F'85'                                                        
*        D     R0,=F'100'                                                       
*        SPACE 1                                                                
*SCP4     L     R0,=F'1000000'                                                  
*        TM    PBILLBAS,X'10'      COST BASED ON NET?                           
*        BNO   *+8                                                              
*        L     R0,=F'850000'                                                    
*        AR    R1,R0                                                            
*        ST    R1,0(R3)                                                         
*        B     SCP2                                                             
         SPACE 1                                                                
*SCPXIT   XC    FILENAME,FILENAME   RESET TO UNT FILE                           
*         NETGO NVSETUNT,DMCB                                                   
*         B     EXIT                                                            
******   DROP  R4                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*--GET BOTTOM OF PAGE COMMENTS                                                  
COMMNTS  DS    0H                                                               
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         XC    HEADHOOK,HEADHOOK                                                
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXREQ,C'C'                                                      
         GOTO1 SPOOL,DMCB,(R3)                                                  
         MVI   BOXYORN,C'N'                                                     
         GOTO1 SPOOL,DMCB,(R3)                                                  
         DROP  R4                                                               
*                                                                               
         L     R4,AIO              SET UP FOR NETCOM                            
         USING NCOMBLKD,R4                                                      
         XC    NCBAIO(50),NCBAIO                                                
         MVI   NCBBOT,C'Y'         BOTTOM OF PAGE COMMENTS                      
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         LA    RF,50(R4)                                                        
         ST    RF,NCBAIO                                                        
         LA    R1,COMHOOK                                                       
         ST    R1,NCBAHOOK                                                      
         LA    R1,NETBLOCK                                                      
         ST    R1,NCBNETB                                                       
         SPACE 1                                                                
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(3),NDCMDEF2   COMMENT ID                                   
         CLI   NDCMDEF2+2,C'A'     IF NO SIGNIFICANT 3RD CHARACTER,             
         BNL   *+8                                                              
         MVI   NCBID+2,C'1'        ASK FOR TOP COMMENTS                         
GETCMM10 MVI   NCBCLT,C'-'                                                      
         OC    NBSELCLI,NBSELCLI                                                
         BZ    GETCMM20                                                         
         CLC   NBSELCLI,=CL3'ALL'                                               
         BE    GETCMM20                                                         
         MVC   NCBCLT,NBACTCLI                                                  
*                                                                               
GETCMM20 MVI   NCBALPRD,C'-'                                                    
         OC    NBSELPRD,NBSELPRD                                                
         BZ    GETCMM30                                                         
         CLC   NBSELPRD,=CL3'ALL'                                               
         BE    GETCMM30                                                         
         CLC   NBSELPRD,=CL3'POL'                                               
         BE    GETCMM30                                                         
         MVC   NCBPRD,NBPRD                                                     
         MVI   NCBALPRD,0                                                       
*                                                                               
GETCMM30 MVI   NCBALEST,C'-'                                                    
         OC    NBSELEST,NBSELEST                                                
         BZ    GETCMM40                                                         
         MVC   NCBEST,NBSELEST     CURRENT ESTIMATE                             
         MVI   NCBALEST,0                                                       
*                                                                               
GETCMM40 MVI   NCBNTWK,C'-'                                                     
         OC    NBSELNET,NBSELNET                                                
         BZ    *+10                                                             
         MVC   NCBNTWK,NBSELNET                                                 
*                                                                               
         MVI   NCBDPT,C'-'                                                      
         OC    NBSELDP,NBSELDP                                                  
         BZ    *+10                                                             
         MVC   NCBDPT,NBSELDP                                                   
*                                                                               
         MVI   NCBPKG,C'-'                                                      
         OC    NBSELPAK,NBSELPAK                                                
         BZ    *+10                                                             
         MVC   NCBPKG,NBSELPAK                                                  
*                                                                               
         GOTO1 =V(NETCOM),DMCB,NCOMBLKD                                         
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 3                                                                
COMHOOK  NTR1                                                                   
         SPACE 1                                                                
         L     R4,AIO2                                                          
         LA    R4,50(R4)           POINT TO RECORD                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
         SPACE 1                                                                
COMHOOK2 BAS   RE,NEXT2                                                         
         BNE   COMHOOK4                                                         
         USING NCOMELEM,R4                                                      
         L     R2,GLAP1                                                         
         ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     COMHK3                                                           
*        MVC   P+20(0),NCOMETXT                                                 
         MVC   20(0,R2),NCOMETXT                                                
COMHK3   GOTO1 SPOOL,DMCB,(R3)                                                  
         B     COMHOOK2                                                         
COMHOOK4 DS    0H                                                               
*        GOTO1 SPOOL,DMCB,(R3)                                                  
         B     EXIT                                                             
         DROP  R4,R3                                                            
*                                                                               
         EJECT                                                                  
                                                                                
* CHK SPECIAL CLIENT GRP ACCESS LIMIT                                           
********************************************************                        
CLTLIMIT L     RA,ATWA                                                          
**       CLI   6(RA),C'*'                                                       
**       BNE   CLTOK                                                            
**       CLI   7(RA),C'A'          CHECK IF ALPHA                               
**       BL    CLTOK                                                            
**       CLI   7(RA),C'Z'                                                       
**       BH    CLTOK                                                            
**       CLI   8(RA),C'0'          CHECK IF NUMERIC                             
**       BL    CLTOK                                                            
**       CLI   8(RA),C'9'                                                       
**       BH    CLTOK                                                            
**       BAS   RE,VALGROUP            VALIDATE GROUP NUMBER                     
         B     EXIT                                                             
                                                                                
CLTOK    SR    RA,RA                                                            
**       LTR   RA,RA                                                            
**       B     EXIT                                                             
                                                                                
                                                                                
VALGROUP NTR1                                                                   
**       MVC   WORK(20),KEY        SAVE KEY                                     
**       LA    R4,KEY                                                           
**       USING GRPRECD,R4          CLIENT STATION PASSIVE POINTER               
*                                                                               
**       XC    KEY,KEY                                                          
**       MVI   GRPPTYP,GRPPTYPQ       RECORD TYPE                               
**       MVI   GRPPSTYP,GRPPCTYQ      CLIENT GROUP                              
**       MVC   GRPPAGMD(1),NBACTAM    AGENCY/MEDIA                              
**       MVC   GRPPVAL(3),8(R2)           CLIENT                                
**       OC    GRPPVAL,=X'404040404040'   BLANK PADDED                          
**       MVC   GRPPID(1),7(RA)            GROUP ID                              
*                                                                               
**       MVC   KEYSAVE(13),KEY                                                  
**       GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
**       CLC   KEY(10),KEYSAVE                                                  
**       BNE   VALGRPX                                                          
*                                                                               
**       MVC   FULL,=X'404040404040'                                            
**       MVC   FULL(2),8(RA)       GROUP CODE                                   
**       OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
**       PACK  DUB,FULL                                                         
**       L     R0,DUB+4                                                         
**       SRL   R0,4                GET RID OF SIGN NIBLE                        
**       STCM  R0,3,HALF           LEFT-JUSTIFIED, PWOS                         
*                                                                               
**       CLC   HALF,GRPPCODE       GROUP CODE MATCH?                            
*                                  CC= OK   CC NOT= ERROR                       
**       MVC   KEY(20),WORK        RESTORE KEY                                  
VALGRPX  XIT1                                                                   
*******************************************************************             
         EJECT                                                                  
* OFFLINE GETS A NUMBER OF ADDRESSES                                            
INITIAL  DS    0H                                                               
         L     R7,ANETWS2          ESTABLISH ADDR TO WORKING STORAGE            
         A     R7,=F'500'                                                       
         USING WRITERD,R7                                                       
                                                                                
         LA    R1,CIDTBL           COMMERCIAL TABLE                             
         ST    R1,NDCIDTBL                                                      
         LA    R1,BILLDFLT         ..BILL DATE START/END FILTERS                
         ST    R1,NBABDFLT         ..FOR COLUMN FILTERING                       
                                                                                
*                                                                               
* PQINDX IS HANDLED HERE AT INITIALIZATION SINCE PROGRAM NEEDS                  
* INDX STORAGE AT ROW AND COLUMN VALIDATION -                                   
* PQINDX                                                                        
         L     R1,NDAPQIND                                                      
         LTR   R1,R1                                                            
         BNZ   PQINDXX             GET STORAGE ONLY ONCE                        
*                                                                               
         L     R2,ATWA                                                          
         USING T320FFD,R2                                                       
*                                                                               
         L     R3,=A(MYIO)                                                      
         GOTO1 SCANNER,DMCB,SPLOPTH,(7,0(R3)),0                                 
         ZIC   R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BZ    PQINDXX                                                          
PQI5     CLC   =C'DOWNHEADX',12(R3)                                             
         BNE   PQI7                                                             
         MVI   NDFLAV3,C'N'        NO TARGETS ON DOWNHEAD                       
         B     PQI9                                                             
PQI7     CLC   =C'PQIX',12(R3)                                                  
         BE    PQINDX             YES                                           
PQI9     LA    R3,32(R3)                                                        
         BCT   R4,PQI5                                                          
         B     PQINDXX                                                          
         DROP  R2                                                               
*******************************                                                 
* THIS CODE WILL BE ACTIVATED WHEN PQIX IS OUT OF TESTING STAGE                 
         L     R1,ATWA                                                          
         USING CONHEADH-64,R1                                                   
         L     RF,TWAMASTC         ALLOCATE PQINDEX TABLE                       
         USING MASTD,RF                                                         
         TM    MCOPT1,MCQ1PQIX     PQIX?                                        
         BZ    PQINDXX                                                          
         OC    MCREMOTE,MCREMOTE   TEST DIRECT                                  
         BNZ   *+14                                                             
         OC    MCREMPQK,MCREMPQK   TEST SOON                                    
         BZ    PQINDXX                                                          
         DROP  R1,RF                                                            
*                                                                               
PQINDX   LA    R3,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
         LA    RE,PQINDXEQ                                                      
         MR    R2,RE               MAX RECS * L'ENTRY                           
         AHI   R3,4                EXTRA 4 BYTES FOR TABLE LENGTH               
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         AHI   R3,-4                                                            
         AHI   RE,4                                                             
         ST    RE,NDAPQIND                                                      
         XCEF  (RE),(R3)                                                        
*                                                                               
PQINDXX  EQU   *                                                                
*                                                                               
*****************************************************************               
         L     R1,NDADPG          LOAD MODULES ONLY ONCE                        
         LTR   R1,R1              ELSE NEW CLEARED STORAGE                      
         BNZ   INITXX             LOADS OVER SAVED STORAGE                      
                                                                                
* - T32082 READ BILLING HEADERS AND UNIT BILLING ELEMENTS                       
         GOTO1 CALLOV,DMCB,X'82000000',0,0            LOAD T32082               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABHRD,0(R1)                                                      
* - T32094 READ HISTORY RECORDS                                                 
**       GOTO1 CALLOV,DMCB,X'94000000',0,0            LOAD T32094               
**       CLI   4(R1),0                                                          
**       BE    *+6                                                              
**       DC    H'0'                                                             
**       MVC   AHSTRD,0(R1)                                                     
* - T32084 READ INVOICE HEADERS                                                 
         GOTO1 CALLOV,DMCB,X'84000000',0,0            LOAD T320842              
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AINVRD,0(R1)                                                     
*                                                                               
         GOTO1 CALLOV,DMCB,X'B2000000',0,0     LOAD T320B2 FOR DPG              
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB                                                          
         LA    R1,8(R1)                                                         
         ST    R1,NDADPG                                                        
*                                                                               
INITXX   B     EXIT                                                             
*                                                                               
*********************************************************************           
                                                                                
         DROP  R7                                                               
                                                                                
                                                                                
**AMYDPG   DC    F'0'                                                           
                                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*B1X      DS    CL50                                                            
*GETBFRW  DS    CL500                   SAVE AREA FOR SFM BILL READ             
*                                                                               
         EJECT                                                                  
*                                                                               
GETPRD1  NTR1  BASE=*,LABEL=*  USES FULL AND RETURNS                            
         L     R1,ANETWS1      1 BYTE PROD IN BYTE                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
         MVI   HALF,0                                                           
GTP5     CLC   FULL(3),0(R1)                                                    
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         CLI   HALF,2              2ND CLSIT DONE?                              
         BE    GTP7                                                             
         MVI   HALF,2                                                           
         LA    R2,35                                                            
         LA    R1,CLIST2                                                        
         B     GTP5                                                             
GTP7     MVI   BYTE,0                                                           
         B     *+10                                                             
GOTPRD   MVC   BYTE,3(R1)                                                       
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
FILLIST  DS    0F                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    CL8'NPAVDIR'                                                     
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'NUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL8'URECV'                                                       
         DC    CL8'NNTIDIR'                                                     
         DC    CL8'NL=NTIFL'                                                    
         DC    C'X'                                                             
*                                                                               
*        TRTTABLE FOR VALID DATASET NAME CHARACTERS                             
*                                                                               
TRTTABLE DC    256AL1(*-TRTTABLE)                                               
         ORG   TRTTABLE            ALPHANUMERIC ALLOWED                         
         DC    X'FF'                                                            
         ORG   TRTTABLE+C' '       ALPHANUMERIC ALLOWED                         
         DC    X'00'                                                            
         ORG   TRTTABLE+C'A'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'J'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'S'       ALPHANUMERIC ALLOWED                         
         DC    8X'00'                                                           
         ORG   TRTTABLE+C'0'       ALPHANUMERIC ALLOWED                         
         DC    10X'00'                                                          
         ORG                                                                    
         TITLE 'T32020 - NETWORK WRITER CONTROL - CHKPYBLE'                     
***********************************************************************         
*        ROUTINE TO CHECK IF THERE IS ANY PAYABLE AMOUNT              *         
*        LEFT IN THE UNIT. IF THERE IS IT FILLS IN CHQ FIELDS         *         
*        WITH DEFAULT DATE. ONLY DONE IF WR PROFILE+12 SET            *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
CHKPYBL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BHBLOCK,R3          ESTABLISH DATA RETURN AREA                   
*                                                                               
         TM    NBINDS4,NBI4DISB   SKIP IF NO DISBURSE DATE NEEDED               
         BNO   CHKPYBNO                                                         
*                                                                               
*        GET PAYABLE GROSS AND NET                                              
*                                                                               
         XC    DMCB(16),DMCB                                                    
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,155               TIME,INTEG,SPECIAL - GROSS                
         GOTO1 =V(NETACC),DMCB,,NETBLOCK                                        
*                                                                               
         MVC   DUB,WORK+1                                                       
         CVB   R1,DUB                                                           
         ST    R1,CHQGRS           SAVE PAYABLE GROSS                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,156               TIME,INTEG,SPECIAL - NET                  
         GOTO1 =V(NETACC),DMCB,,NETBLOCK                                        
         MVC   DUB,WORK+1                                                       
         CVB   R2,DUB                                                           
         ST    R2,CHQNET           SAVE PAYABLE NET                             
*                                                                               
         LTR   R2,R2               SKIP IF NOT NET PAYABLE                      
         BZ    CHKPYBNO                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,CHQDATE) TODAY FOR CHQDATE                  
*                                                                               
         MVC   CHQBNKDT,CHQDATE    SAME FOR BANK DATE                           
         MVC   CHQCLRDT,CHQDATE    SAME FOR CLEARANCE DATE                      
         MVI   CHQBKDFT,C'*'      FLAG AS DEFAULT DATE                          
*                                                                               
         CLI   NBRDBELS,0         HAVE WE ALREADY GONE TO DRIVER ONCE           
         BNE   *+8                                                              
         MVI   NBRDBELS,1          NO/SET FIRST TIME                            
*                                                                               
         MVI   CHQBKDFT,C'*'       SET AS DEFAULT DATE                          
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
         B     CHKPYBLX                                                         
*                                                                               
CHKPYBNO DS    0H                                                               
*                                                                               
         LTR   RB,RB               FORCE NE CC                                  
         B     CHKPYBLX                                                         
*                                                                               
CHKPYBLX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         TITLE 'T32020 - NETWORK WRITER CONTROL'                                
***********************************************************************         
*                                                                     *         
*        READ WR PROFILE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTWRPRF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    GTWRKY,GTWRKY       INIT PROFILE KEY                             
         MVC   GTWRSYS(2),=C'S0'   SYSTEM                                       
         MVC   GTWRPID,=C'WR'      PROFILE ID                                   
         MVC   GTWRAGY,AGENCY      AGENCY                                       
         MVI   GTWRMED,C'N'        MEDIA                                        
         MVC   GTWRCLT,NBCLICOD    CLIENT                                       
*                                                                               
         CLC   GTWRKY,GTWRKYS     SKIP IF KEY HASN'T CHANGED                    
         BE    GTWRPRF1                                                         
*                                                                               
         GOTO1 GETPROF,DMCB,GTWRKY,WRPROF,DATAMGR   GET WR PROFILE              
*                                                                               
GTWRPRF1 DS    0H                                                               
*                                                                               
GTWRPRFX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        KEY FOR WR PROFILE                                                     
*                                                                               
GTWRKY   DS    XL16                PROFILE KEY                                  
         ORG   GTWRKY                                                           
GTWRSYS  DS    CL1                 SYSTEM                                       
         DS    XL1                 NOT USED                                     
GTWRPID  DS    CL2                 PROFILE ID                                   
GTWRAGY  DS    CL2                 AGENCY                                       
GTWRMED  DS    CL1                 MEDIA                                        
GTWRCLT  DS    CL3                 CLIENT                                       
         ORG                                                                    
*                                                                               
GTWRKYS  DS    XL(L'GTWRKY)        PROFILE KEY SAVEAREA                         
*                                                                               
WRPROF   DS    CL16               WR PROFILE                                    
*                                                                               
         EJECT                                                                  
*                                                                               
GETPRD3  NTR1  BASE=*,LABEL=*  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
         MVI   HALF,0                                                           
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         CLI   HALF,2              CLIST2 DONE?                                 
         BE    GP7                                                              
         L     R1,ANETWS1                                                       
         LA    R1,CLIST2                                                        
         LA    R2,35                                                            
         MVI   HALF,2                                                           
         B     GP5                                                              
GP7      MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        - GET FULL AGENCY ID                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETAGYID NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         LA    R1,GAGYRELO                                                      
         S     R1,GAGYRELO                                                      
         L     R4,=A(MYIO)         YES-READ CONTROL ID REC FOR AGY ID           
         AR    R4,R1                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,T320FFD+10                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,CTIDATA                                                       
WRTRYAGN CLI   0(RE),X'02'                                                      
         BE    WRGOTIT                                                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   WRTRYAGN                                                         
         DC    H'0'                                                             
WRGOTIT  MVC   WORK(7),2(RE)       SET AGY ID IN WORK                           
         DROP  R4                                                               
         LA    R4,PENTABLE                                                      
WRGOT5   CLC   WORK(5),0(R4)       IS IT RESTRICTED AGY ID                      
         BE    WRGOT10             YES/CHECK IT                                 
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BE    WRGYES              EOF/OK                                       
         B     WRGOT5                                                           
*                                                                               
WRGOT10  CLC   7(4,R4),SPLPRO      CHECK PRODUCT                                
         BE    WRGYES                                                           
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BE    WRERR                                                            
         CLC   WORK(5),0(R4)       STILL SAME AGY ID                            
         BE    WRGYES              YES                                          
WRERR    MVI   ERROR,INVALID       NO MATCH                                     
         LTR   RE,RE                                                            
                                                                                
*                                                                               
WRGYES   XIT1                      ASSUME COND CODE =                           
*                                                                               
GAGYRELO DC    A(*)                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RA                                                               
*                                                                               
PENTABLE DS    0H                                                               
         DC    CL7'BDPEA',C'V=3**'                                              
         DC    CL7'BDPEB',C'V=1**'                                              
         DC    CL7'BDPEB',C'V=2**'                                              
         DC    CL7'BDPEC',C'V=4**'                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*=================================================================*             
* RP2GET - READ IN REPORT2/CONTINUE REPORT                        *             
*=================================================================*             
         SPACE 1                                                                
         USING WRITERD,R7                                                       
         USING CONHEADH-64,RA                                                   
         USING NETSYSD,R9                                                       
         USING GEND,RC                                                          
RP2GET   NTR1  BASE=*,LABEL=*                                                   
         USING CT01RECD,R4                                                      
         XC    KEY,KEY             ESTABLISH PROGRAM RECORD KEY                 
         LA    R4,KEY                                                           
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,NBSELAGY   SET AGENCY ID                                
         MVI   CT01SYS,3           SET FOR SPOT SYSTEM                          
         MVI   CT01PRG,X'20'       SET FOR NET WRITER                           
         MVI   CT01PHAS,X'20'      SET FOR T32020                               
         MVC   CT01NAME,NDRPT2ID   SET SECOND REPORT ID                         
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         ICM   R0,8,USEIO         SAVE USEIO                                    
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   FILENAME,=CL8'CTFILE'  SET FILE NAME                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                READ IN PROGRAM RECORD                       
         CLC   KEY(L'CT01KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                SHOULD ALREADY HAVE BEEN VALIDATED           
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         STC   R0,USEIO         AND USEIO                                       
*                                                                               
         L     R0,ATIA             SAVE OFF SCREEN                              
         LHI   R1,SPLWORK-T320FFD                                               
         LR    RF,R1                                                            
         LR    RE,RA                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVDISP,DATADISP                                                  
         MVC   DATADISP,=H'28'                                                  
         IC    R0,MODE                                                          
         MVI   MODE,DISPREC                                                     
***      GOTO1 GENPROG,DMCB,(RC)                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A37'           LOAD T00A37                  
         L     RF,DMCB             GET START OF PHASE                           
         GOTO1 (RF),DMCB,(RC)                                                   
**                                                                              
         STC   R0,MODE                                                          
         MVC   DATADISP,SVDISP                                                  
*                                                                               
         L     R3,ERREX            FORCE ERROR EXIT TO THIS ROUTINE             
         LA    R1,RP2DIE                                                        
         ST    R1,ERREX                                                         
*                                                                               
         LA    R2,SPLCOLSH                                                      
         GOTO1 NDVALCOL                                                         
         ST    R3,ERREX            RESTORE VALUABLE ADDRESS                     
         MVC   AIO,AIO1                                                         
         L     RE,ATIA             AND SCREEN                                   
         LHI   R1,SPLWORK-T320FFD                                               
         LR    RF,R1                                                            
         LR    R0,RA                                                            
         MVCL  R0,RE                                                            
         XIT1                                                                   
*                                                                               
RP2DIE   DCHO                      TAKING NO PRISONERS ON 2ND REPORT            
*                                                                               
         DROP                                                                   
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DC    5000X'00'                                                        
         DC    2000X'00'                                                        
         DC    CL8'PRODLIST'                                                    
PRODLIST DC    2000X'00'                                                        
         DC    CL8'*SPLBILL'                                                    
SPLBILL  DC    1000X'00'           SPLIT BILLING BLOCK                          
*                                                                               
PLANIO   DS    CL2000              I/O AREA FOR PLAN RECORD                     
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13 MNTHS X 6                         
*                                  MOS(2) + START(2) + END(2)                   
UDEFLST  DS    CL250               UDEF PROD/EST DATA                           
*                                  MOS(2) + START(2) + END(2)                   
         DS    0D                                                               
         DC    CL8'*GLRXTN**'                                                   
*GLRGXTN  DS    CL510               GLARG EXTENSION FOR 16 COLUMNS              
GLRGXTN  DS    CL1020               GLARG EXTENSION FOR 16 COLUMNS              
*          6(LENGTH OF EACH EXT)X 16 COLS X 5 FILTS PER COL                     
INVFLT   DS    CL12                START/END INVOICE NUMBERS                    
*                                                                               
         DS    0D                                                               
         DC    CL8'CASHIERC'                                                    
CSHIERC  DS    XL(CSHIERL)         DDCASHIER CONTROL BLOCK                      
*                                                                               
STAGRPTB DS    CL2001              STATION GROUP TABLE 4X500                    
*                                  1ST BYTE '-' = NEGATIVE FILTERING            
*                                                                               
NEWBLRD  DS    CL200                                                            
STWRDLST DS    CL256                                                            
*PRGBUFF  DS    CL550                                                           
PRGBUFF  DS    CL800               400 X 2                                      
XPRDMSK  DS    CL1600              400 X (1 BYTE BINARY + 3 CHAR CODE)          
         DS    0F                                                               
PRODTBL  DS    CL100               FOR USE BY NETEVALUE/NEPRDACC                
         DS    0F                                                               
PRODTBL2 DS    CL18                FOR USE BY NETEVALUE                         
         DS    0F                                                               
MULTPLST DS    CL18                                                             
*                                                                               
STABRDIO DS    CL4000                                                           
STABIOSV DS    CL4                                                              
       ++INCLUDE NENETDEMD       MULTIPLE VTYPE AREAS                           
         EJECT                                                                  
*              STORAGE FOR WRITER                                               
         SPACE 3                                                                
WRITERD  DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
         ORG   NDDEMBLK                                                         
       ++INCLUDE NETDEMON                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
VTYPCNT  DS    CL1                 TRACK POSITION OF VTYPEQUS                   
*                                                                               
PERTYPE  DS    CL3                                                              
NWEEKS   DS    F                                                                
NMONTHS  DS    F                                                                
NQUARTS  DS    F                                                                
                                                                                
* - DATEAREA COVERED BY NEDATELSTD DSECT                                        
DATEAREA DS    CL(DATLSTLN)                                                     
*                                                                               
SVNBACT  DS    F                                                                
SVNBASS  DS    F                                                                
SVNBINT  DS    F                                                                
ABHRD    DS    F                   ADDRESS OF T32082 MODULE                     
AINVRD   DS    F                   ADDRESS OF T32084 MODULE                     
AHSTRD   DS    F                   ADDRESS OF T32084 MODULE                     
MYHALF   DS    CL2                                                              
*                                                                               
AAGYCLT  DS    F                  .CROSS AGENCY READ SAVE AREA                  
CURRLST  DS    CL5                                                              
PREVLST  DS    CL5                                                              
LSTRD    DS    CL1                .CLIENT/AGY READ COUNT                        
LSTRQAGY DS    CL2                .REQUEST AGENCY IN CROSS-AGY READ             
LSTCLT   DS    CL4                .SAVE @NNN                                    
*                                                                               
STAKEYSV DS    CL15                STATION KEY SAVE FOR CHQ READ                
MKTSV    DS    CL4                 SAVE MKT NUMBER FOR CHQ READ                 
BILLFLT  DS    CL4                 SAVE BILL FILTER DATES IN BHRD               
*                                                                               
BHRD     DS    CL1                 BILL HEADER READ                             
*BHDTYPF  DS    CL1                 BILL HEADER READ DATE TYPE FILTER           
*BHMANFLG DS    CL1                 BILL HEADER MANUAL BILL ONLY                
*BHBFLG   DS    CL2                 BILL HEADER B1,B2 ETC FLAG                  
*CLTSAV   DS    CL3                 3 BYTE CLIENT CODE IN BH READ               
PRODSAVE DS    CL1                 USED IN BILLING PRODUCT MODE                 
PRODSAV3 DS    CL3                 USED IN BILLING PRODUCT MODE                 
B1XFLAG  DS    CL1                 B1X PROFILE USED?                            
KEY2     DS    CL25                                                             
COMPLEN  DS    CL1                 COMPARE LENGTH OF KEY                        
PAKWORK  DS    PL12                                                             
BINMKT   DS    XL2                 2 BYTE MARKET NUMBER                         
CIDTBL   DS    CL240               (CML ID CL8 + CML CLASS CL4 ) X 20           
BILLDFLT DS    CL40                BILLING DATE COLUMN FILTER TABLE             
*                                  CL4(START/END) X 10 MAX COLUMN               
TARGID   DS    CL8                                                              
TARGIDS  DS    CL336               TABLE OF COML IDS FROM UNIT                  
TARGLENE EQU   *-TARGIDS                                                        
*                                                                               
$TYPKYSV DS    CL20                FOR $TYPE KEYWORD MUYLTIPLE PASS             
SVDISP   DS    CL2                                                              
SVDRON   DS    CL1024              SAVE AREA FOR DRONE COMPUTATION              
*                                                                               
AMQRPT   DS    F                                                                
MQFILENM DS    CL35                                                             
*                                                                               
WRTSVLEN EQU   *-WRITERD                                                        
*                                                                               
*                                                                               
         SPACE                                                                  
AGYENTD  DSECT          AGENCY ENTRY LIST DSECT                                 
AEAGY    DS    CL2                 AGENCY                                       
AECLT    DS    CL3                 CLIENT                                       
         DS    CL15                SPARE                                        
AELENE   EQU   *-AEAGY                                                          
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
*        INCLUDE NEGENINCLS                                                     
*        INCLUDE NEDATELSTD                                                     
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE SPGENPRD                                                       
*        INCLUDE NECOMBLOK                                                      
*        INCLUDE NEGENCOM                                                       
       ++INCLUDE NEGENINCLN                                                     
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE NEGENCOM                                                       
         EJECT                                                                  
       ++INCLUDE SPLTBLKD                                                       
         EJECT                                                                  
*              BASE SCREEN DSECT                                                
         SPACE 3                                                                
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
*              OVERLAY SCREEN                                                   
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
FILTSV   DS    CL4                 SAVE WR FILTER                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDFAXINFOD                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NETPQIND                                                       
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE SPGENTAL                                                       
       ++INCLUDE TIMTALD                                                        
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE BHBLOCKBOB                                                     
       ++INCLUDE DDCASHIERD                                                     
       ++INCLUDE NEGBLOCKD                                                      
       ++INCLUDE NEPKGTBL                                                       
       ++INCLUDE SPGETBFRD                                                      
       ++INCLUDE NETXBLKD                                                       
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE NEDDEQUS                                                       
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
       ++INCLUDE DRINTRECD2                                                     
*                                                                               
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'174NEWRI20N  03/31/08'                                      
         END                                                                    
